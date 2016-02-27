let json_mem_assoc k (json:Yojson.Safe.json) =
  match json with
  | `Assoc a ->  
      BatList.mem_assoc k a
  | _ -> false 
  

let json_assoc k (json:Yojson.Safe.json) =
  match json with
  | `Assoc a ->  
    (try
      List.assoc k a
    with
    | _ -> 
    failwith ("assoc '"^k^"' not found in "
              ^ (Yojson.Safe.to_string json)))
  | j -> 
    failwith ("assoc '"^k^"' not found in "
              ^ (Yojson.Safe.to_string json))

let json_remove_assoc k (json:Yojson.Safe.json) =
  match json with
  | `Assoc a ->  
    (try
      `Assoc (BatList.remove_assoc k a)
    with
    | _ -> 
    failwith ("assoc '"^k^"' not found in "
              ^ (Yojson.Safe.to_string json)))
  | j -> 
    failwith ("assoc '"^k^"' not found in "
              ^ (Yojson.Safe.to_string json))

let json_replace_assoc k (json:Yojson.Safe.json) value =
  match json with
  | `Assoc a -> 
    `Assoc (List.map (fun (n, v) ->
      if n=k then (n, value) else (n, v)
    ) a)
  | j -> 
    failwith ("assoc '"^k^"' not found in "
              ^ (Yojson.Safe.to_string json))

let json_insert_assoc k (json:Yojson.Safe.json) value =
  match json with
  | `Assoc a -> 
    `Assoc ((k, value) :: a)
  | j -> j
    
let json_list = function
  | `List a -> a
  | j -> 
    failwith ("not list json" 
              ^ (Yojson.Safe.to_string j)) 

let string_of_json = function
  | `String a -> a
  | j -> 
    failwith ("not string json: " 
              ^ (Yojson.Safe.to_string j))
let int_of_json = function
  | `Int a -> a
  | j -> 
    failwith ("not int json: " 
              ^ (Yojson.Safe.to_string j))
