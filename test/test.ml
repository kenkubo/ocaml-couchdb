open OUnit
open Couchdb
open Lwt


type t = {
  _id : Couchdb.doc_id;
  _rev : string;
  _attachments: Doc.attach_file list;
  so: string option;
} deriving (Yojson)

let default = {
  _id="";
  _rev = "";
  _attachments = [];
  so=None;
}

let file1 =
  Doc.attach_file
    ~filename: "test.txt"
    ~content_type: "text/plain"
    ~data: "Hello files"
  

let file2 =
  Doc.attach_file
    ~filename: "test2.txt"
    ~content_type: "text/plain"
    ~data: "This is raw text"
  

let file3 =
  Doc.attach_file
    ~filename: "putfile.txt"
    ~content_type: "text/plain"
    ~data: "Testing put file\n"
  


let db = Couchdb_types.{
  hostname = "localhost";
  port = 5984;
  name = "test_suite_db2";
}

module Doc = Doc.Make (struct
  type doc_t = t deriving (Yojson)
  let db = db
  let default_value = default
end)

module View = Couchdb_view.View (
  struct
    type key = Couchdb.doc_id deriving (Yojson)
    type value = t deriving (Yojson)
    let db = db
    let design_name = "test_view"
    let name = "attach"
    let map = 
      "function(doc) { if (doc._attachments) { emit(doc._id, doc);} }" 
    let reduce = None
    let default_key = ""
    let default_value = default
  end)

module Design = Couchdb_view.Design (
  struct
    let db = db
    let name = "test_view"
    let views = [
      (module View : Couchdb_view.View);
    ]
  end)

let current_id = ref ""
let current_rev= ref ""

let set_up () = ()
let tear_down () = () 



let attach_post = 
  let run () = 
    Lwt_main.run (
    Doc.post { 
      default with
        _attachments = [ file1; file2 ]
    } >|= fun res ->
      current_id := res.Api.id;
      current_rev:= res.Api.rev)
  in
  "attach file post" >::(bracket set_up ( run) tear_down)

let document_get =
  let run () = 
    Lwt_main.run (
    Doc.get !current_id >|= fun doc ->
      (* print_endline (Yojson_t.to_string doc); *)
      List.iter (fun att ->
        assert_equal att.Couchdb_doc.content_type file1.Couchdb_doc.content_type
      ) doc._attachments
    )
  in
  "get document with file" >::(bracket set_up ( run) tear_down)

let document_put = 
  let run () = 
    Lwt_main.run (
      Doc.get !current_id >>= 
      Doc.put !current_id >>= fun _ ->
      Doc.get !current_id >|= fun doc ->
      List.iter (fun att ->
        assert_equal att.Couchdb_doc.content_type file1.Couchdb_doc.content_type
      ) doc._attachments;
      current_rev:= doc._rev)
  in
  "put document" >::(bracket set_up ( run) tear_down)


let view_get =
  let run () = 
    Lwt_main.run (
    View.query () >|= 
    View.get_row_values >|= fun docs ->
      List.iter (fun doc ->
       (* print_endline (Yojson_t.to_string doc);  *)
        List.iter (fun att ->
        assert (att.Couchdb_doc.content_type = file1.Couchdb_doc.content_type)
        )
        doc._attachments
      ) docs
    )
  in
  "get view with file" >::(bracket set_up ( run) tear_down)


let attach_get = 
  let run () = 
    Lwt_main.run (
    Doc.get_attach !current_id "test.txt" >|= fun (media,res) ->
      assert_equal media (Some file1.Couchdb_doc.content_type); 
      assert_equal (Some res) file1.Couchdb_doc.data ) 
  in
  "attach file post" >::(bracket set_up ( run) tear_down)

let attach_put =
  let run () = 
    Lwt_main.run (
    Doc.put_attach !current_id !current_rev file3
    >>= fun res ->
      current_id := res.Api.id;
      current_rev:= res.Api.rev;
      Doc.get !current_id >>= fun doc ->
      List.iter (fun att ->
        assert_equal att.Couchdb_doc.content_type file1.Couchdb_doc.content_type
      ) doc._attachments;
      assert_equal 
        (List.length doc._attachments) 3;
    return ())
  in
  "attach file put" >::(bracket set_up ( run) tear_down)

let attach_delete =
  let run () = 
    Lwt_main.run (
    Doc.delete_attach !current_id !current_rev "putfile.txt"
    >>= fun res ->
      Doc.get !current_id >>= fun doc ->
      List.iter (fun att ->
        assert_equal att.Couchdb_doc.content_type file1.Couchdb_doc.content_type
      ) doc._attachments;
      assert_equal 
        (List.length doc._attachments) 2;
      return ())
  in
  "attach file delete" >::(bracket set_up ( run) tear_down)

let _ = 
  Lwt_main.run (
    Design.store()
  );
  Api.debug := false;

  run_test_tt_main (
    "Couchdb Test" >:::
      [
        attach_post;
        document_get;
        document_put;
        attach_get;
        view_get;
        attach_put;
        attach_delete;
      ])

  
