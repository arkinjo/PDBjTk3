open Postgresql
open ExtLib

let command (conn: connection) ?params query =
  ignore(conn#exec ~expect:[Command_ok;] ?params query)

let select (conn: connection) ?params query =
  conn#exec ~expect:[Tuples_ok;] ?params query
 
let transaction conn thunk =
  try
    command conn "BEGIN";
    let result = thunk () in
    command conn "COMMIT";
    result
  with exc -> 
    command conn "ROLLBACK";
    command conn "COMMIT"; 
    raise exc


let sep_array = Str.regexp "[{},() ]+"
let any_list_of_string conv s =
  List.map conv (Str.split sep_array s)

let any_array_of_string conv s =
  Array.of_list (List.map conv (Str.split sep_array s))

let string_list_of_string = any_list_of_string (fun s -> s)

let float_array_of_string = any_array_of_string float_of_string
let int_array_of_string = any_array_of_string int_of_string
let string_array_of_string = any_array_of_string (fun s -> s)


let string_of_string_list arr = "{" ^ String.concat "," arr ^ "}"
let string_of_string_array arr = 
  string_of_string_list (Array.to_list arr)
let string_of_float_array arr = 
  string_of_string_list (Array.to_list (Array.map string_of_float arr))
let string_of_int_array arr = 
  string_of_string_list (Array.to_list (Array.map string_of_int arr))

let result_of_field (res: result) =
  let ht = Hashtbl.create 10 in
  Array.iteri (fun i fname -> Hashtbl.add ht fname i) res#get_fnames;
  fun tuple fn -> 
    let ind = Hashtbl.find ht fn in
    tuple.(ind)

(** format Unix.tm into TIMESTAMP string. *)
let string_of_tm tm =
  Printf.sprintf "%4d-%2.2d-%2.2d %2.2d:%2.2d:%2.2d" 
    (tm.Unix.tm_year + 1900) tm.Unix.tm_mon tm.Unix.tm_mday 
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
