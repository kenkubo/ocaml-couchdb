(**
   Document API for CouchDB 
*)



open Lwt
open Couchdb_types
open Couchdb_config
open Json_util

module Api = Couchdb_api

type attach_file = {
  filename: string;
  content_type: string;
  data: string option;
  revpos: int option;
  digest: string option;
  length: int option;
  stub: bool option
} deriving (Yojson)

type rev = {
  rev: string;
  status: string;
} deriving (Yojson)

type revs_info = {
  _revs_info: rev list
} deriving (Yojson)

let default_attach_file =
  {
    filename = "";
    content_type = "";
    data = None;
    revpos = None;
    digest = None;
    length = None;
    stub = None;
  }

let attach_file ~filename ~content_type ~data =
  {
    default_attach_file with
      filename;
      content_type;
      data = Some data
  }

(** Type of document *)
module type Doctype = sig
  type doc_t  (** document type *)
  val db : db (** database *)
  val default_value : doc_t (** Default value of document *)
  module Yojson_doc_t : Deriving_Yojson.Yojson with type a = doc_t
end

module Make (DT: Doctype) = 
struct
  type docs = {
    all_or_nothing: bool;
    docs: DT.doc_t list;
  }  deriving (Yojson)

  type all_docs_value = {
    rev: string;
  } deriving (Yojson)

  type result = {
    id: string;
    key: string;
    value: all_docs_value;
    doc: DT.doc_t;
  } deriving (Yojson)

  type result_body = {   
    total_rows: int option;
    offset: int option;
    rows: result list;
  } 
  
  let get key = 
    Api.get DT.db key 
    >>= fun (code,strbody) ->
    return (
      match strbody with
      | Api.Success json -> 
        DT.Yojson_doc_t.from_json ~o:(DT.default_value) json

      | Api.Fail err ->
        logout ("Failed at document get error:" ^ err.Api.error ^ " reason:" ^ err.Api.reason ^ "db:" ^ DT.db.name ^  " id:" ^ key); 
        failwith (err.Api.error ^ ": " ^ err.Api.reason)
      | Api.Ok err -> failwith "Fatal: when get request retuen ok response error"
    )

  let get_rev key rev = 
    Api.get_rev DT.db key rev
    >>= fun (code,strbody) ->
    return (
      match strbody with
      | Api.Success json -> 
        DT.Yojson_doc_t.from_json ~o:(DT.default_value) json

      | Api.Fail err ->
        logout ("Failed at document get error:" ^ err.Api.error ^ " reason:" ^ err.Api.reason ^ "db:" ^ DT.db.name ^  " id:" ^ key); 
        failwith (err.Api.error ^ ": " ^ err.Api.reason)
      | Api.Ok err -> failwith "Fatal: when get request retuen ok response error"
    )
    
  let revs_info key : revs_info Lwt.t =
    Api.get_revs_info DT.db key
    >>= fun (code,strbody) ->
    return (
      match strbody with
      | Api.Success json -> 
        Yojson_revs_info.from_json json
      | Api.Fail err ->
        logout ("Failed at document revisions get error:" ^ err.Api.error ^ " reason:" ^ err.Api.reason ^ "db:" ^ DT.db.name ^  " id:" ^ key); 
        failwith (err.Api.error ^ ": " ^ err.Api.reason)
      | Api.Ok err -> failwith "Fatal: when get revs_info request retuen ok response error"
    )

  let get_attach id filename = 
    Api.get_attach DT.db id filename
    >>= fun strbody ->
    strbody

  let put_attach id rev attach_file = 
    Api.put_attach DT.db id rev 
      attach_file.filename attach_file.content_type 
      (BatOption.default "" attach_file.data)
    >>= function
    | (code, Api.Success json ) -> failwith "Fatal: when put request retuen success response error"
    | (code, Api.Fail err ) ->
      logout ("Failed at put_attach error:" ^ err.Api.error ^ " reason:" ^ err.Api.reason ^ "db:" ^ DT.db.name ^  " id:" ^ id);
      failwith (err.Api.error ^ ": " ^ err.Api.reason)
    | (code, Api.Ok ok ) -> return ok


  let delete_attach id rev filename = 
    Api.delete_attach DT.db id rev filename
    >>= function
    | (code, Api.Success json ) -> failwith "Fatal: when put request retuen success response error"
    | (code, Api.Fail err ) ->
      logout ("Failed at delete_attach error:" ^ err.Api.error ^ " reason:" ^ err.Api.reason ^ "db:" ^ DT.db.name ^ " id:" ^ id);
      failwith (err.Api.error ^ ": " ^ err.Api.reason)
    | (code, Api.Ok ok ) -> return ok

  let gets key_list : result_body Lwt.t = 
    Api.gets DT.db key_list 
    >>= fun (code,strbody) ->
      match strbody with
      | Api.Success json -> (
        let rows = json_list (json_assoc "rows" json) in
        let row_result = List.map (fun a ->
          let id = json_assoc "id" a in
          let key =  json_assoc "key" a in
          let value = json_assoc "value" a in
          let doc = json_assoc "doc" a in
          let r = DT.Yojson_doc_t.from_json ~o:(DT.default_value) doc in
          {
            id = Json_util.string_of_json id;
            key = Json_util.string_of_json key;
            value = Yojson_all_docs_value.from_json value;
            doc = r ;
          }
        ) rows in
        let result = {
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
          rows = row_result;
        } in
        return result)
      | Api.Fail err ->
        logout ("Failed at document gets error:" ^ err.Api.error ^ " reason:" ^ err.Api.reason );
        failwith (err.Api.error ^ ": " ^ err.Api.reason)
      | Api.Ok err -> failwith "Fatal: when get request retuen ok response error"

  let post v =
    Api.post DT.db (DT.Yojson_doc_t.to_json v)
    >>= function
    | (code, Api.Success json ) -> failwith "Fatal: when post request retuen success response error"
    | (code, Api.Fail err ) ->
      logout ("Failed at document post error:" ^ err.Api.error ^ " reason:" ^ err.Api.reason);
      failwith (err.Api.error ^ ": " ^ err.Api.reason)
    | (code, Api.Ok ok ) -> return ok

  let put id v =
    Api.put DT.db id (DT.Yojson_doc_t.to_json v)
    >>= function
    | (code, Api.Success json ) -> failwith "Fatal: when put request retuen success response error"
    | (code, Api.Fail err ) ->
      logout ("Failed at document put error:" ^ err.Api.error ^ " reason:" ^ err.Api.reason ^ "db:" ^ DT.db.name ^  " id:" ^ id);
      failwith (err.Api.error ^ ": " ^ err.Api.reason)
    | (code, Api.Ok ok ) -> return ok

  let put_new id v =
    Api.put_new DT.db id (DT.Yojson_doc_t.to_json v)
    >>= function
    | (code, Api.Success json ) -> failwith "Fatal: when put_new request retuen success response error"
    | (code, Api.Fail err ) ->
      logout ("Failed at document put_new error:" ^ err.Api.error ^ " reason:" ^ err.Api.reason ^ "db:" ^ DT.db.name ^  " id:" ^ id);
      failwith (err.Api.error ^ ": " ^ err.Api.reason)
    | (code, Api.Ok ok ) -> return ok

  let delete key rev =
    Api.delete DT.db key rev
    >>= function
    | (code, Api.Success json ) -> failwith "Fatal: when delete request retuen success response error"
    | (code, Api.Fail err ) ->
      logout ("Failed at document delete error:" ^ err.Api.error ^ " reason:" ^ err.Api.reason ^ "db:" ^ DT.db.name ^  " id:" ^ key);
      failwith (err.Api.error ^ ": " ^ err.Api.reason)
    | (code, Api.Ok ok ) -> return ok

  let posts (vl:DT.doc_t list) =
    Api.bulk_post DT.db (Yojson_docs.to_json {docs=vl; all_or_nothing=true})

end

