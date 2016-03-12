open OUnit
open Couchdb
open Lwt



module M  = Test_type.Make (struct let n="" end)
module M2  = Test_type.Make (struct let n="abc" end)



type t = {
  _id : Couchdb.doc_id;
  _rev : string;
  tags : M2.t;
  str: int list;
} deriving (Yojson)

type record = {
  id: string;
  value : t;
} deriving (Yojson)


let default_rec = {
  _id="";
  _rev = "";
  str=[];
  tags = [];
}

let default = {
  id ="ididid";
  value = default_rec;
}


let () =
  let json_str = "{
  \"id\" : \"0000\",
  \"value\": {
    \"_id\":\"idid0000\",
    str:[],
    \"_rev\":\"あいうおrev0000\"
  }
}" in
  let json = Yojson.Safe.from_string json_str in
  print_endline ((Yojson.Safe.to_string json));
  let v = Yojson_record.from_json ~o:default json in
  let str = Yojson_record.to_string v in
  print_endline str;
  
  let v = Yojson_record.from_string ~o:default json_str in
  let str = Yojson_record.to_string v in
  print_endline str
