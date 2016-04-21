(**
   CouchDB API
*)


open Lwt
open Couchdb_types
open Couchdb_config

open Cohttp


type status = 
| OK
| Created
| Accepted
| Not_Modified
| Bad_Request
| Not_Found
| Resource_Not_Allowed
| Conflict
| Precondition_Failed
| Internal_Server_Error
    
let status_of_code = function
  | 200 -> OK
  | 201 -> Created
  | 202 -> Accepted
  | 304 -> Not_Modified
  | 400 -> Bad_Request
  | 404 -> Not_Found
  | 405 -> Resource_Not_Allowed
  | 409 -> Conflict
  | 412 -> Precondition_Failed
  | 500 -> Internal_Server_Error
  | code -> (failwith ("Undefined status code " ^ (string_of_int code)) )

    
type error_msg = {
  error: string;
  reason: string;
} deriving (Yojson)

type ok_msg = {
  ok: bool;
  id: string;
  rev: string;
} deriving (Yojson)

type response_body = 
| Success of Yojson.Safe.json
| Fail of error_msg
| Ok of ok_msg

type response = status  * response_body



let attachments_decode j:Yojson.Safe.json =
  let open Yojson.Safe in
  let list_of_object : json -> json  = function
    | `Assoc files -> 
      `List (
        BatList.fold_left (fun a (key, v) ->
          let filename = `String key in
          let v = 
            if Json_util.json_mem_assoc "data" v then v 
            else
              Json_util.json_insert_assoc "data" v (`Null)
            in
          (Json_util.json_insert_assoc "filename" v filename) :: a
        )  [] files )
    | a -> a in
  let rec replace_attachments = function 
    | `Assoc li ->
      `Assoc (
        List.map (fun (name, v) ->
          match name with
          | "_attachments" ->
            (name, list_of_object v)
          | _ ->
            (name, replace_attachments v)
        ) li)
    | `List li ->
      `List ( List.map replace_attachments  li)
      
    | n -> n in

  replace_attachments j


let json_to_response json =
  match json with
  | `Assoc a ->
    (
      match (BatList.mem_assoc "ok" a, BatList.mem_assoc "error" a) with
      | true, false ->
        Ok (Yojson_ok_msg.from_json json)
      | false, true ->
        Fail (Yojson_error_msg.from_json json)
      | _ -> Success (attachments_decode json)
    )
  | e -> Success json
    

let convert_response (resp, body) =
  let code = resp |> Response.status |> Code.code_of_status in
  if !debug then (logout (Printf.sprintf "Code: %d\n" code));
  Cohttp_lwt_body.to_string body 
  >>= fun strbody ->
  if !debug then logout ("Response: " ^ strbody);
  let json = Yojson.Safe.from_string strbody in
  let vcode = status_of_code code in
  let response = json_to_response json in
  return (vcode, response)

let uri db =
  "http://" ^ db.hostname ^ ":" ^ (string_of_int db.port) ^ "/" ^ db.name ^ "/"
    
let remove_special_field j =
  match j with
  | `Assoc a ->
    `Assoc (
      BatList.remove_assoc "_id"
        (BatList.remove_assoc "_rev" a)
    )
  | n -> n 
    
    
let get db key =
  let uri = Uri.of_string ((uri db) ^ key)  in
  if !debug then logout ("GET " ^ (Uri.to_string uri));
  Cohttp_lwt_unix.Client.get uri 
  >>=  convert_response 

let get_rev db key rev =
  let uri = Uri.of_string ((uri db) ^ key ^ "?rev=" ^ rev)  in
  if !debug then logout ("GET " ^ (Uri.to_string uri));
  Cohttp_lwt_unix.Client.get uri 
  >>=  convert_response 

let get_revs_info db key =
  let uri = Uri.of_string ((uri db) ^ key ^ "?revs_info=true")  in
  if !debug then logout ("GET " ^ (Uri.to_string uri));
  Cohttp_lwt_unix.Client.get uri 
  >>=  convert_response 


let get_view db design_name vname ~query () : (status * response_body) Lwt.t =
  let view_uri =
    (Uri.make
       ~scheme:"http"
       ~host:db.hostname
       ~port:db.port
       ~path:("/" ^ db.name ^ "/_design/" ^ design_name ^ "/_view/" ^ vname) 
       ~query ()) in

  if !debug then logout ("GET VIEW " ^ (Uri.path_and_query view_uri));
  Cohttp_lwt_unix.Client.get view_uri 
  >>=  (fun (res:Cohttp_lwt.Response.t * Cohttp_lwt_body.t) ->
    convert_response res)

let gets_view db design_name vname ~keys_list () : (status * response_body) Lwt.t =
  let view_uri =
    (Uri.make
       ~scheme:"http"
       ~host:db.hostname
       ~port:db.port
       ~path:("/" ^ db.name ^ "/_design/" ^ design_name ^ "/_view/" ^ vname) 
        ()) in
  let jstr = Yojson.Safe.to_string keys_list in
  let body = Cohttp_lwt_body.of_string jstr in
  let headers = 
    Cohttp.Header.init_with "Content-Type" "application/json" in
  if !debug then logout ("GETS VIEW " ^ (Uri.path_and_query view_uri));
  if !debug then logout ("GETS BODY " ^ (jstr)); 
  Cohttp_lwt_unix.Client.post ~body ~headers view_uri 
  >>=  (fun (res:Cohttp_lwt.Response.t * Cohttp_lwt_body.t) ->
    convert_response res)

let gets db key_list =
  let json = `Assoc [("keys", `List (
    List.map (fun k -> `String k) key_list
  ))] in
  let jstr = Yojson.Safe.to_string json in
  let body = Cohttp_lwt_body.of_string jstr in
  let post_uri = (uri db) ^ "_all_docs?include_docs=true" in
  let headers = 
    Cohttp.Header.init_with "Content-Type" "application/json" in
  if !debug then logout ("GET " ^ ( post_uri));
  Cohttp_lwt_unix.Client.post ~body ~headers
    (Uri.of_string (post_uri)) 
  >>= convert_response

let attachments_encode j:Yojson.Safe.json =
  let open Yojson.Safe in
  let object_of_list : json -> json  = function
    | `List files -> (* one file *)
      `Assoc (
        BatList.fold_left (fun a file ->
          let filename = 
            try
              Json_util.string_of_json
                (Json_util.json_assoc "filename" file)
            with | _ -> failwith 
              ("attachement filename not found: " ^ (Yojson.Safe.to_string j))in
          let data = Json_util.json_assoc "data" file in
          let file' = Json_util.json_replace_assoc "data" file (
            let string_data = 
              try
                Json_util.string_of_json data 
              with _ -> "" in
            `String (Netencoding.Base64.encode string_data)) in
          (filename, 
           Json_util.json_remove_assoc "filename" file') :: a
        )  [] files )
    | a -> a in
  match j with
  | `Assoc li ->
    `Assoc (
      List.map (fun (name, v) ->
        match name with
        | "_attachments" ->
          (name, object_of_list v)
        | _ -> (name, v)
      ) li)
  | n -> n 

    
let post db j =
  let j' = attachments_encode j in
  let jstr = Yojson.Safe.to_string (remove_special_field j') in
  if !debug then logout jstr;
  let body = Cohttp_lwt_body.of_string jstr in
  let headers = 
    Cohttp.Header.init_with "Content-Type" "application/json" in
  Cohttp_lwt_unix.Client.post ~body ~headers
    (Uri.of_string (uri db)) 
  >>= convert_response

      
let put db id j =
  let j' = attachments_encode j in
  let jstr = Yojson.Safe.to_string (j') in
  let body = Cohttp_lwt_body.of_string jstr in
  if !debug then logout jstr;
  let headers = 
    Cohttp.Header.init_with "Content-Type" "application/json" in
  Cohttp_lwt_unix.Client.put ~body ~headers (Uri.of_string ((uri db) ^ id) ) 
  >>=  convert_response

let put_new db id j =
  put db id (remove_special_field j)

let delete db key rev =
  let uri = ((uri db) ^ key ^ "?rev=" ^ rev ) in
  Cohttp_lwt_unix.Client.delete
    (Uri.of_string uri) 
  >>= convert_response

let bulk_post db j =
  let jstr = Yojson.Safe.to_string j in
  if !debug then logout jstr;
  let body = Cohttp_lwt_body.of_string jstr in
  let headers = 
    Cohttp.Header.init_with "Content-Type" "application/json" 
  in
  Cohttp_lwt_unix.Client.post ~body ~headers
    (Uri.of_string ((uri db) ^ "_bulk_docs") ) 
  >>= fun (res,body) ->
  Cohttp_lwt_body.to_string body 
  >|= fun strbody ->
    if !debug then logout strbody;
    ()

let put_view db id j =
  let jstr = Yojson.Safe.to_string (j) in
  let body = Cohttp_lwt_body.of_string jstr in
  let uri = Uri.of_string ((uri db) ^ id ) in
  let headers = 
    Cohttp.Header.init_with "Content-Type" "application/json" in
  Cohttp_lwt_unix.Client.put ~body ~headers uri 
  >>=  convert_response
  

let temp_view db j =
  let jstr = Yojson.Safe.to_string j in
  if !debug then logout jstr;
  let body = Cohttp_lwt_body.of_string jstr in
  let headers = 
    Header.init_with "Content-Type" "application/json" 
  in
  Cohttp_lwt_unix.Client.post ~body ~headers
    (Uri.of_string ((uri db) ^ "_temp_view") ) 
  >>= fun (resp,body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  logout (Printf.sprintf "Response code: %d\n" code);
  List.iter 
    (fun s -> logout (Printf.sprintf "Headers: %s" s)) 
    (resp |> Response.headers |> Header.to_lines);

  convert_response (resp,body)

let get_attach db key filename =
  let uri = Uri.of_string ((uri db) ^ key ^ "/" ^filename)  in
  if !debug then logout ("GET " ^ (Uri.to_string uri));
  Cohttp_lwt_unix.Client.get uri 
  >|= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  let content_type = resp |> Response.headers |> Header.get_media_type in
  Cohttp_lwt_body.to_string body >>= fun body_str ->
  match code with
  | 200 -> Lwt.return  (content_type, body_str)
  | _ -> failwith ("attach file not found: " ^  key ^ "/" ^ filename)

      
let put_attach db id rev filename content_type data =
  let body = Cohttp_lwt_body.of_string data in
  let uri = Uri.of_string ((uri db) ^ id ^ "/" ^filename ^"?rev=" ^ rev) in
  let headers = 
    Cohttp.Header.init_with "Content-Type" content_type in
  Cohttp_lwt_unix.Client.put ~body ~headers uri 
  >>=  convert_response


let delete_attach db id rev filename  =
  let uri = Uri.of_string ((uri db) ^ id ^ "/" ^filename ^"?rev=" ^ rev) in
  Cohttp_lwt_unix.Client.delete  uri 
  >>=  convert_response
   


