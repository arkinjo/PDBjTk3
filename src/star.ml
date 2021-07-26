open Star_syntax
open Format
open ExtList
open ExtString

exception Star_error of string
exception Star_parse_error of (string -> unit)

let get_lexbuf = function
  | `Channel ic -> Lexing.from_channel ic 
  | `String s -> Lexing.from_string s
  | _ -> raise (Star_error "get_lexbuf")

let report_parse_error lexbuf =
  let cl,cls,lp,ls = Star_lexer.pos lexbuf in
  fun s ->
    Printf.fprintf stderr "Parse Error in file: %s line: %d column: %d\n" s cl (lp - cls);
    flush stderr

let parse_lexbuf lexbuf = 
  Star_lexer.init lexbuf;
  try
    let star_file = Star_parser.star_file Star_lexer.token lexbuf in
    star_file
  with 
    Parsing.Parse_error -> 
      raise (Star_parse_error (report_parse_error lexbuf))
  | exc -> raise exc

let parse ic =
  let lexbuf = Lexing.from_channel ic in
  parse_lexbuf lexbuf

let parse_file file =
  try
    Ecomm.read_gzipped_maybe parse file
  with 
  | Star_parse_error f ->
      f file;
      raise Parsing.Parse_error

let parse_string str =
  let lexbuf = Lexing.from_string str in
  parse_lexbuf lexbuf


(*******************)

let unquote_data_value = function
  | Non_quoted_text_string s -> s
  | Single_quoted_text_string s -> String.lchop (String.rchop s)
  | Double_quoted_text_string s -> String.lchop (String.rchop s)
  | Semi_colon_bounded_text_string s -> String.lchop (String.rchop s)
  | Frame_code s -> "$" ^ s

let print_data_value ?(name="") = function
  | Non_quoted_text_string s ->printf "%s: %s\n" name s
  | Single_quoted_text_string s -> printf "%s: %s\n" name s
  | Double_quoted_text_string s -> printf "%s: %s\n" name s
  | Semi_colon_bounded_text_string s -> printf "%s: %s\n" name s
  | Frame_code s -> printf "%s: $%s\n" name s

let rec print_data_loop_definition = function
    [] -> ()
  | field :: rest -> 
      print_data_loop_field field; print_data_loop_definition rest
and print_data_loop_field = function
    Data_name (Underscore dn) -> printf "%s:\n" dn
(*  | Nested_loop nl -> print_data_loop_definition nl*)

let print_data_loop (defs,values) =
  printf "Loop_\n";
  print_data_loop_definition defs;
  List.iter (function 
      Data_value dv -> print_data_value dv; print_endline "//"
    | Stop_ -> printf "Stop_\n") values

let rec print_data data = 
  open_box 2;
  begin
    match data with
    | Item (Underscore name,value) -> print_data_value ~name value
    | Data_loop dl -> print_data_loop dl
  end;
  close_box ()
  
let rec print_datablock (Data_ head, body) =
  Format.open_box 2;
  printf "head: %s\n" head;
  let rec loop = function
    | [] -> ()
    | Data data :: rest -> printf "Data\n"; 
	print_data data;
	loop rest
    | Save_frame (Save_ head, dl) :: rest -> printf "Save_frame: %s\n" head;
	List.iter print_data dl;
	loop rest
  in
  loop body;
  Format.close_box ()

let print_star starf = 
  let rec loop = function
      [] -> ()
    | Data_block db :: rest ->  
	printf "data_block\n"; 
	print_datablock db;
	loop rest
    | Global_block gb :: rest -> printf "global_block\n"; loop rest
  in
  Format.open_box 2;
  loop starf;
  Format.close_box ()

let loop_structure df =
  let rec loop lst = function
    | [] -> List.map List.rev (List.rev lst)
    | (Data_name (Underscore dn))::rest -> 
	let ns = List.hd lst in
	loop ((dn::ns)::List.tl lst) rest
(*    | (Nested_loop dl) :: rest -> loop ([]::lst) dl*)
  in loop [[]] df

let split_loop_values dvalues =
  let rec loop lst = function
      [] -> List.map List.rev (List.rev lst)
    | (Data_value dv) :: rest -> 
	let h = List.hd lst in
	loop ((dv::h)::List.tl lst) rest
    | Stop_ :: rest -> 
	loop ([]::lst) rest
  in
  loop [[]] dvalues

let loop_combine df dv =
  let ls = loop_structure df in
  let vs = split_loop_values dv in
  let rec proc accum ls vals = 
    match ls with
      [] -> List.rev accum
    | h::[] -> 
	let nh = List.length h in
	let nv = List.length vals in
	let nseg = nv / nh in
	let mh = List.concat (List.make nseg h) in
	if List.length mh <> List.length vals then
	  begin
	    prerr_endline "ERROR (probably unmatched quotes):";
	    prerr_endline "header...";
	    List.iter (fun hh -> prerr_endline ("->" ^ hh)) h;
	    prerr_endline "value...";
	    List.iter (fun hh -> 
	      prerr_endline ("->" ^(unquote_data_value hh))) vals;
	    failwith (sprintf ".. loop_combine (1) %d %d" 
			(List.length mh) (List.length vals))
	  end
	else
	  proc ((List.combine mh vals) :: accum) [] []
    | h::rest ->
	let nh = List.length h in
	let nv,vals = List.split_nth nh vals in
	proc ((List.combine h nv) :: accum) rest vals
  in
  List.map (proc [] ls) vs 
    
let data_find_item data key =
  match data with
  | Item (Underscore dn,dv) when dn = key -> [dv]
  | Data_loop (df,dv) -> 
      let cv = List.map List.concat (loop_combine df dv) in
      let cv = List.concat cv in
      List.filter_map (fun (k,v) ->
	if k = key then Some v else None) cv
  | Item (_,_) -> []

let data_find_item_list data_list key =
  let d = List.fold_left (fun d data ->
    (data_find_item data key)::d) [] data_list in
  List.concat (List.rev d)

let get_loop_of_category star category =
  let db = 
    match List.hd star with 
      Data_block (Data_ dh,db) -> db
    | Global_block _ -> raise Not_found in
  let sf = List.filter_map 
      (function 
	| Data (Data_loop (ldef,lval)) -> 
	    if List.exists 
		(function 
		  | Data_name (Underscore s) 
		    when fst(String.split s ".") = category
		    -> true
		  | _ -> false)
		ldef
	    then Some(ldef,lval)
	    else None
	| _ -> None)
      db in
  let ldef,lval = List.hd sf in
  let df = List.filter_map (function Data_name (Underscore s) -> Some s) 
      ldef in
  let dvs = List.filter_map (function
    | Data_value d -> Some (unquote_data_value d)
    | _ -> None) lval in
  let n = List.length df in
  let rec chunk ls = function
    | [] -> List.rev ls
    | l -> chunk ((List.take n l)::ls) (List.drop n l)
  in
  df,(chunk [] dvs)

let get_items_of_category star category =
  let db = 
    match List.hd star with 
      Data_block (Data_ dh,db) -> db
    | Global_block _ -> raise Not_found in
  List.filter_map
    (function 
      | Data (Item ((Underscore name),value)) -> 
	  if fst(String.split name ".") = category
	  then Some (name,(unquote_data_value value))
	  else None
      | _ -> None)
    db

let get_name_values_of_category star category =
  match get_items_of_category star category with
  | [] -> get_loop_of_category star category 
  | l ->
      let names,values = List.split l in
      names,[values]


