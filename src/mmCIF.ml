open Star_syntax
open Printf
open ExtLib
open PDBjBasis

module Ht = Hashtbl

exception Bad_mmCIF_file

type category = {
    name: string; (* category name *)
    items: string list; (* category items *)
    data: string list list; (* data items *)
  }

let conv_category_list cif =
  let proc cat =
    let alst = 
      List.map (fun data -> List.combine cat.items data) cat.data in
    cat.name, alst in
  List.map proc cif

let datablock = function
  | Data_block (Data_ dh, db) -> dh,db
  | _ -> 
      fprintf stderr "Error(mmCIF): Cannot find datablock."; 
      flush stderr;
      raise Bad_mmCIF_file

let chunk_list names l =
  let n = List.length names in
  let rec loop acc = function
    | [] -> List.rev acc
    | l -> 
	let hs = List.take n l in
	if List.length hs < n
	then (fprintf stderr "Error(mmCIF): Bad loop_ (%s)\n"
		(String.concat "," names);
	      flush stderr;
	      raise Bad_mmCIF_file);
	let rest = List.drop n l in
	loop (hs::acc) rest
  in
  loop [] l

(* process loop_ section *)
let proc_loop (ldef,lvals) = 
  (* first, process definitions *)
  let l = List.filter_map (function
(*    | Nested_loop _ -> None*)
    | Data_name (Underscore dn) -> Some dn) ldef in
  (* next, deal with data values *)
  let vs = List.hd (Star.split_loop_values lvals) in
  let vs = chunk_list l vs in
  let data = List.map (List.map Star.unquote_data_value) vs in
  let cat = 
    let h = List.hd l in
    fst(String.split h ".") in
  {name = cat;
   items = List.map PDBx.chop_category l;
   data = data}

  
(* split into 1-data category and loops *)
let split_datablock db = 
  let l = List.filter_map
      (function 
	| Save_frame _ -> None
	| Data data -> Some data)
      db in
  let ht = Ht.create 10 (* for saving 1-data categories *) in
  (* filtering loop_ categories *)
  let l = List.filter_map (fun data ->
    match data with
    | Item ((Underscore dn), dv) -> (* 1-data category *)
	let cat,item = String.split dn "." in
	let dval = Star.unquote_data_value dv in
	let t = Ht.find_default ht cat {name = cat; items=[]; data=[]} in
	begin
	  match t.data with
	  | [] -> Ht.replace ht cat {t with items= [item];data = [[dval]]}
	  | h::_ -> 
	      Ht.replace ht cat {t with items= item :: t.items;
				 data = [dval::h]}
	end;
	None
    | Data_loop dl -> Some dl)
      l in
  (* 1-data categories are saved in reverse order; so revert them. *)
  Ht.iter (fun k t -> 
    Ht.replace ht k {t with 
		     items = List.rev t.items; 
		     data = [List.rev (List.hd t.data)]}) ht;
  let cats = List.map proc_loop l in
  Ht.fold (fun _ v l -> v :: l) ht cats

let parse ic =
  match Star.parse ic with
  | b::_ -> 
      let dbName,db = datablock b in
      Datablock.of_mmCIF (dbName,(conv_category_list (split_datablock db)))
  | _ -> raise Bad_mmCIF_file

let parse_file file = 
  Ecomm.read_gzipped_maybe parse file


let get_category cif category =
  List.find (fun c -> c.name = category) cif

let exists_category cif category =
  List.exists (fun c -> c.name = category) cif

let dump cif =
  List.iter
    (fun c ->
      printf "%s (%s)\n" c.name (String.concat "," c.items);
      List.iter (fun s -> printf "%s\n" (String.concat "\t" s)) c.data;
      printf "//\n")
    cif

let escape_cif_string (cat,item) s =
  match PDBjDict.type_of_item (cat,item) with
  | "int" -> s
  | "float" -> s
  | "idname" -> s
  | "line" | "uline" | "ec-type" ->
      if String.contains s '\n'
      then "\n;" ^ s ^ ";\n"
      else if String.contains s ' ' then  "'" ^ s ^ "'" else s
  | "text" -> 
      if String.contains s '\n' 
      then "\n;" ^ s ^ ";\n"
      else if String.contains s ' ' 
      then "'" ^ s ^ "'"
      else s
  | _ -> s

let sorted_atom_site_keys =
  ["group_PDB";
   "id";
   "type_symbol";
   "label_atom_id";
   "label_alt_id";
   "label_comp_id"; 
   "label_asym_id";
   "label_entity_id";
   "label_seq_id";
   "pdbx_PDB_ins_code";
   "Cartn_x";
   "Cartn_y";
   "Cartn_z"; 
   "occupancy"; 
   "B_iso_or_equiv"; 
   "Cartn_x_esd"; 
   "Cartn_y_esd"; 
   "Cartn_z_esd"; 
   "occupancy_esd"; 
   "B_iso_or_equiv_esd"; 
   "pdbx_formal_charge";
   "auth_seq_id"; 
   "auth_comp_id"; 
   "auth_asym_id"; 
   "auth_atom_id"; 
   "pdbx_PDB_model_num";
 ]

let atom_site_append_keys keys =
  let ap = List.fold_left
      (fun al k -> if List.mem k sorted_atom_site_keys then al else k::al)
      [] keys in
  sorted_atom_site_keys @ (List.rev ap)
			    
let make_get_items cname litems =
  let ht = Ht.create 10 in
  List.iter (fun l -> List.iter (fun (k,_) -> Ht.replace ht k ()) l) litems;
  let keys = Ht.fold (fun k _ keys -> k::keys) ht [] in
  let keys =
    if cname = "atom_site"
    then atom_site_append_keys keys
    else List.sort ~cmp:String.compare keys in
  keys,
  (fun items ->
    List.map 
      (fun key -> 
	if List.mem_assoc key items 
	then escape_cif_string (cname,key) (List.assoc key items)
	else if PDBjDict.item_is_mandatory (cname,key) 
	then "."
	else "?")
      keys)


let print oc datablock =
  let dbName, tables = Datablock.to_mmCIF datablock in
  fprintf oc "data_%s\n#\n" dbName;
  List.iter 
    (fun (cname,table) ->
      match table with
      | [] -> ()
      | h::[] ->
	  List.iter 
	    (fun (k,v) -> 
	      fprintf oc "_%s.%s  %s\n" 
		cname k (escape_cif_string (cname,k) v)) 
	    h;
	  fprintf oc "#\n"
      | lst -> 
	  fprintf oc "loop_\n";
	  let keys,get_items = make_get_items cname lst in
	  List.iter (fun k -> fprintf oc "_%s.%s\n" cname k) keys;
	  List.iter (fun l -> 
	    let line = String.concat " " (get_items l) in
	    fprintf oc "%s\n" line)
	    lst;
	  fprintf oc "#\n")
    tables;
  ()
