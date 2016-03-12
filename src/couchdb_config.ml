
let debug = ref false
let logger = ref print_endline
let logout s =
  !logger ("[couchdb] " ^ s )
