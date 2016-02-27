open Lwt
open Couchdb_types

type doc_id = string deriving (Yojson)

module Api = Couchdb_api
module View = Couchdb_view
module Doc = Couchdb_doc


