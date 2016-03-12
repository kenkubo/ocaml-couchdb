module Make (M:sig val n:string end) =
struct
type key = string deriving (Yojson)
type tag = key * (string option) deriving (Yojson)
type t = tag list deriving (Yojson)
end
