(**
   View API for CouchDB 
*)

open Lwt
open Couchdb_types
module Api = Couchdb_api

(**

Creating View

   {[ module SentView = Couchdb_view.View (
     struct
       type key = Couchdb.doc_id deriving (Yojson)  type of key to be emit
       type value = t deriving (Yojson)             type of value to be emit
       let db = Config.db
       let design_name = "messagebox"  <- name of design document
       let name = "sent"
       let map =                       <- Javascript for map
         "function(doc) { if (doc.form=='message-sent') { emit(doc.recipient, doc);} }" 
       let reduce = None               <- Javascript for reduce
       let default_key = ""
       let default_value = default_sent
     end)]}

Define the design document

  {[ module TestDesign = Couchdb_view.Design (
    struct
      let db = db
      let name = "messagebox"
      let views = [
        (module SentView : Couchdb_view.View);  <- Registration the views
      ]
    end)]]}


*)



module type ViewType = sig
  type key
  type value
  val db : db
  val design_name : string
  val name: string (* view name *)
  val map: string
  val reduce: string option
  module Yojson_key : Deriving_Yojson.Yojson with type a = key
  module Yojson_value : Deriving_Yojson.Yojson with type a = value
  val default_key: key 
  val default_value: value
end


module type View = sig
  type row (* Type of result row *)
  val name: string
  val map: string
  val reduce: string option

  val to_json: Yojson.Safe.json
end

module View (VT: ViewType) =
struct

  type row = {
      id: string;
      key: VT.key;
      value: VT.value;
  } deriving (Yojson)

  type result_body = 
    {   
      total_rows: int option;
      offset: int option;
      rows: row list;
    } 

  let default_row = {
    id = "";
    key = VT.default_key;
    value = VT.default_value;
  }

  let name = VT.name
  let map = VT.map
  let reduce = VT.reduce

  let to_json = 
    match VT.reduce with 
    | Some re -> `Assoc [
      ("map", `String VT.map);
      ("reduce", `String re); ]
    | None -> `Assoc [
      ("map", `String VT.map) ]

  let result_of_json json rows = Json_util.{
          total_rows = 
            (try 
              Some (int_of_json (json_assoc "total_rows" json)) 
            with 
            | _ -> None);
          offset = 
            (try 
               Some (int_of_json (json_assoc "offset" json)) 
             with 
             | _ -> None);
          rows;
        } 
  let uri_query_of params =
    List.fold_left 
        (fun q (k, p)->
          match p with
          | Some v -> (k, [v]) :: q 
          | None -> q )
        [] params 
  let rows_of_json json =
            try 
              Yojson_row.from_json (*~o:default_row*) json
            with
              | e ->
                let open BatInnerIO in
                write_string stderr "Couchdb json parse error:";
                BatPrintexc.print stderr e;
                write_string stderr (" at " ^ (Yojson.Safe.to_string json));
                write_string stderr "\n";
                Yojson_row.from_json ~o:default_row json

  let query ?(key=None) ?(startkey=None) ?(descending=false) ?(limit=None) () :  result_body Lwt.t = 
    let params = [
      "key" , BatOption.map VT.Yojson_key.to_string key;
      "startkey" ,startkey;
      "limit" ,limit;
    ] in
    
    let params = 
      if descending then ("descending" , Some "true") :: params else params in
        
    let query = uri_query_of params in
    
    let open Json_util in
    Couchdb_api.get_view VT.db VT.design_name VT.name ~query () 
    >>= fun (code,body) ->
      match body with
      | Api.Success json -> 
        let rows = json_list (json_assoc "rows" json) in
        let row_result = List.map rows_of_json rows in
        let result = result_of_json json row_result in
        return (result)
      | Api.Fail err -> failwith (err.Api.error ^ ": " ^ err.Api.reason)
      | Api.Ok err -> failwith "Fatal: when get request retuen ok response error"

  let query_bulk ?(keys=[]) () :  result_body Lwt.t =

  let assoc =
    List.map (fun j ->
      `Assoc [("key", j)]
    ) (List.map VT.Yojson_key.to_json keys) in
  
    let keys_list =
      `List assoc in
    
    let open Json_util in
    Couchdb_api.gets_view VT.db VT.design_name VT.name ~keys_list () 
    >>= fun (code,body) ->
      match body with
      | Api.Success json -> 
        let rows = json_list (json_assoc "rows" json) in
        let row_result = List.map rows_of_json rows in
        let result = result_of_json json row_result in
        return (result)
      | Api.Fail err -> failwith (err.Api.error ^ ": " ^ err.Api.reason)
      | Api.Ok err -> failwith "Fatal: when get request retuen ok response error"


  let get_row_values res = 
    List.map (fun r -> r.value) res.rows

  let get_row_keys res = 
    List.map (fun r -> r.key) res.rows

  let get_row_id res = 
    List.map (fun r -> r.id) res.rows

    
end

(** Design Document *)
module type DesignType = sig
  val db : db
  val name : string (* documment name *)
  val views : (module View) list 
end

module Design  (DT: DesignType) =
struct
  let db = DT.db
  let name = DT.name
  let view_find name =
    List.find 
      ( fun v ->
        let module V = (val v : View) in
        V.name = name
      )
      DT.views

  let view_map f =
    Lwt_list.map_s
      ( fun v -> f v )
      DT.views

  let view_iter f =
    Lwt_list.iter_s
      ( fun v -> f v )
      DT.views

  (** Test of viwes *)
  let test () =
    let open Yojson.Basic.Util in
    let open Json_util in
    view_iter ( fun v ->
      let module V = (val v : View) in
      print_endline V.name;
      Api.temp_view db (V.to_json)
      >>= (function
      | (code, Api.Fail err ) -> failwith err.Api.error
      | (code, Api.Ok ok ) -> failwith "Fatal: The view returns ok format."
      | (code, Api.Success json ) -> (
        let list_rows = json_list (json_assoc "rows" json) in
        List.iter 
          (fun a -> print_endline ( Yojson.Safe.to_string a))
          list_rows;     
        return_unit)
      )
    )

  let load db id =
    Api.get db ("_design/" ^ DT.name )
    >>= fun (code,strbody) ->
    return (
      match strbody with
      | Api.Success json -> json
      | Api.Fail err -> failwith (err.Api.error ^ ": " ^ err.Api.reason ^ " " ^ id)
      | Api.Ok err -> failwith "Fatal: when get request retuen ok response error"
    )
  
  let store () =
    let db = DT.db in
    let views =  
      `Assoc (List.map ( fun v ->
        let module V = (val v : View) in
        (V.name, V.to_json )
      ) DT.views) in
    let id = ("_design/" ^ DT.name) in
    catch (fun () -> 
        (** Store if updated viwes *)
        load db DT.name
        >>= function view_json ->
          let org_views = Json_util.json_assoc "views" view_json in

          if org_views <> views then
            begin
              let rev = Json_util.json_assoc "_rev" view_json in
              let json =
                `Assoc [
                  "_id", `String id;
                  "_rev", rev;
                  "language", `String "javascript";
                  "views",  views;
                ] in
              Api.put_view db id json 
              >>= function
              | (code, Api.Success json ) -> failwith "Fatal: when put_view request retuen success response error"
              | (code, Api.Fail err ) -> failwith (err.Api.error ^ ": " ^ err.Api.reason ^ " at " ^(Yojson.Safe.to_string json))
              | (code, Api.Ok ok ) -> return_unit
            end
          else
            return_unit
      )
      (fun exn -> 
        let json =
          `Assoc [
            "_id", `String id;
            "language", `String "javascript";
            "views",  views;
          ] in
        Api.put_view db id json 
        >>= function
        | (code, Api.Success json ) -> failwith "Fatal: when put_view request retuen success response error"
        | (code, Api.Fail err ) -> failwith (err.Api.error ^ ": " ^ err.Api.reason ^ " at " ^ (Yojson.Safe.to_string json) )
        | (code, Api.Ok ok ) -> return_unit )


end
