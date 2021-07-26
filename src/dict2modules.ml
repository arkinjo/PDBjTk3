open Printf
open ExtLib
module Ht = Hashtbl

let conv_type  = function
  | "int" -> "int"
  | "float" -> "float"
  | _ -> "string"

let type_convert_to = function
  | "int" -> "PDBjDict.some_int"
  | "float" -> "PDBjDict.some_float"
  | _ -> "Some"

let type_convert_from = function
  | "int" -> "string_of_int"
  | "float" -> "string_of_float"
  | _ -> ""
  
let reserved_words = 
  [ "struct"; "type"; "begin"; "end"; "for"; "do"; "method"; "class"; "val"; "function" ]

let conv_iname_pdbml =
  let re = [
    (Str.regexp "\\[\\|\\]","");
    (Str.regexp_string "/","_over_");
    (Str.regexp_string "%","");
    (Str.regexp "^\\([0-9].*\\)$","_\\1");
  ] in
  function s -> 
    List.fold_left (fun s (re,p) -> Str.global_replace re p s) s re

let conv_iname =
  let minus = Str.regexp_string "-" in
  function s ->
    let t = String.uncapitalize (conv_iname_pdbml s) in
    let t = Str.global_replace minus "_" t in
    if List.mem t reserved_words then t ^ "_" else t

let conv_catname t =
  if List.mem t reserved_words then t ^ "_" else t
    
let dump_pre ?(oc=stdout) dictinfo =
  fprintf oc "(*** This file is machine-generated. Edit at your own risk. ***)\n\n";
  fprintf oc "(*\n   Based on PDBx/mmCIF dictionary:\n";

  List.iter (fun (n,v) ->fprintf oc "     %s: %s\n" n v) dictinfo;
  fprintf oc " *)\n\n\n";

  fprintf oc "open ExtLib\n";
  fprintf oc "\n\n";
  ()

let dump_pkey_list ?(oc=stdout) dict = 
  fprintf oc "  let item_is_pkey = \n";
  fprintf oc "    let ht = Hashtbl.create 100 in\n";
  fprintf oc "    List.iter (fun k -> Hashtbl.add ht k ()) [\n";
  List.iter 
    (fun table ->
      fprintf oc "    ";
      List.iter 
	(fun pkey -> fprintf oc "(\"%s\",\"%s\"); " 
	    table.PDBx.table_name pkey)
	table.PDBx.pkey;
      fprintf oc "\n")
    dict.PDBx.tables;
  fprintf oc "    ];\n";
  fprintf oc "    fun s -> Hashtbl.mem ht s\n\n";
  ()


let dump_links ?(oc=stdout) dict =
  fprintf oc "  let category_links = [\n";
  List.iter
    (fun link ->
      Format.open_box 2;
      fprintf oc "    ((\"%s\",\"%s\",\"%s\"),["
	link.PDBx.t_src link.PDBx.t_dst link.PDBx.cross_id;
      List.iter
	(fun (s,t) ->
	  fprintf oc "(\"%s\",\"%s\"); " s t)
	link.PDBx.item_pairs;
      fprintf oc "]);\n")
    dict.PDBx.cross_refs;
  fprintf oc "  ]\n\n";
  ()

let dump_mmcif_to_pdbml_item ?(oc=stdout) dict = 
  fprintf oc "  let mmcif_to_pdbml_item,pdbml_to_mmcif_item = \n";
  fprintf oc "    let ht = Hashtbl.create 100 in\n";
  fprintf oc "    List.iter (fun (c,p) -> Hashtbl.add ht c p) [\n";
  List.iter 
    (fun table ->
      fprintf oc "    ";
      List.iter 
	(fun item -> fprintf oc "(\"%s\",\"%s\"); " 
	    item.PDBx.i_name (conv_iname_pdbml item.PDBx.i_name))
	table.PDBx.items;
      fprintf oc "\n")
    dict.PDBx.tables;
  fprintf oc "    ];\n";
  fprintf oc "    let ht2 = Hashtbl.create (Hashtbl.length ht) in\n";
  fprintf oc "    Hashtbl.iter (fun k v -> Hashtbl.add ht2 v k) ht;\n";
  fprintf oc
    "    (fun s -> Hashtbl.find ht s),(fun s -> Hashtbl.find ht2 s)\n\n";
  ()

let mmcif2mod oc table =
    match table.PDBx.items with
    | [] -> failwith ("mmcif2mod: no items in category: " 
		      ^ table.PDBx.table_name)
    | item::[] -> 
	let iname = conv_iname item.PDBx.i_name in
	fprintf oc "  let of_mmCIF items =\n";
	fprintf oc "    match items with\n";
	fprintf oc "    | [] -> default\n";
	fprintf oc "    | (_,v)::_ -> { %s = (%s v) }\n\n"
	  iname (type_convert_to item.PDBx.i_type);
	fprintf oc "  let to_mmCIF a =\n";
	fprintf oc "    match a.%s with\n" iname;
	fprintf oc "    | None -> []\n";
	fprintf oc "    | Some v -> [ (\"%s\",(%s v)) ]\n\n" 
	  item.PDBx.i_name (type_convert_from item.PDBx.i_type)
    | items ->
	fprintf oc "  let of_mmCIF items =\n";
	fprintf oc "    let conv b (k,v) =\n";
	fprintf oc "      match k with\n";
	List.iter
	  (fun item ->
	    let tconv = type_convert_to item.PDBx.i_type in
	    let item = item.PDBx.i_name in
	    fprintf oc "      | \"%s\" -> { b with %s = (%s v) }\n"
	      item (conv_iname item) tconv)
	  items;
	fprintf oc "      | _ -> b in\n";
	fprintf oc "    List.fold_left conv default items\n\n";
	fprintf oc "  let to_mmCIF a =\n";
	fprintf oc "    List.filter_map (fun x -> x) [\n";
	List.iter 
	  (fun item -> 
	    let tconv = type_convert_from item.PDBx.i_type in
	    let item = item.PDBx.i_name in
	    fprintf oc "      Option.map (fun v -> (\"%s\", %s v)) a.%s;\n" 
	      item tconv (conv_iname item))
	  items;
	fprintf oc "    ]\n\n";
	()

let pdbml2mod oc table =
  fprintf oc "  let of_PDBML items =\n";
  fprintf oc "    let conv b (k,v) =\n";
  fprintf oc "      match k with\n";
  List.iter
    (fun item ->
      let tconv = type_convert_to item.PDBx.i_type in
      let item = item.PDBx.i_name in
      fprintf oc "      | \"%s\" -> { b with %s = (%s v) }\n"
	(conv_iname_pdbml item) (conv_iname item) tconv)
    table.PDBx.items;
  fprintf oc "      | _ -> b in\n";
  fprintf oc "    List.fold_left conv default items\n\n";
  fprintf oc "  let to_PDBML a =\n";
  fprintf oc "    List.filter_map (fun x -> x) [\n";
  List.iter 
    (fun item -> 
      let tconv = type_convert_from item.PDBx.i_type in
      let item = item.PDBx.i_name in
      fprintf oc "      Option.map (fun v -> (\"%s\", %s v)) a.%s;\n" 
	(conv_iname_pdbml item) tconv (conv_iname item))
    table.PDBx.items;
  fprintf oc "    ]\n\n";
  ()

let table2mod ?(oc=stdout) table =
  fprintf oc "module %s = struct\n" (String.capitalize table.PDBx.table_name);

  fprintf oc "  type t = {\n";
  List.iter 
    (fun item ->
      fprintf oc "    %s: %s option;\n" 
	(conv_iname item.PDBx.i_name) (conv_type item.PDBx.i_type))
    table.PDBx.items;
  fprintf oc "  }\n\n";

  fprintf oc "  let default = {\n";
  List.iter 
    (fun item -> fprintf oc "    %s = None;\n" (conv_iname item.PDBx.i_name))
    table.PDBx.items;
  fprintf oc "  }\n\n";
  mmcif2mod oc table;
(*  pdbml2mod oc table;*)
  fprintf oc "end\n\n"; 
  ()

let datablock ?(oc=stdout) dict =
  fprintf oc "module Datablock = struct\n";
  fprintf oc "  type t = {\n";
  fprintf oc "    datablockName: string;\n";
  List.iter 
    (fun table ->
      fprintf oc "    %s: %s.t list;\n" 
	(conv_catname table.PDBx.table_name) (String.capitalize table.PDBx.table_name))
    dict.PDBx.tables;
  fprintf oc "  }\n\n";

  fprintf oc "  let default = {\n";
  fprintf oc "    datablockName = \"?\";\n";
  List.iter 
    (fun table ->
      fprintf oc "    %s = [];\n" (conv_catname table.PDBx.table_name))
    dict.PDBx.tables;
  fprintf oc "  }\n\n";

  let make_of_x of_func =
    fprintf oc "  let %s (dbName,lst) =\n" of_func;
      fprintf oc "    List.fold_left (fun d (cat,lst) ->\n";
    fprintf oc "      match cat with\n";
    List.iter 
      (fun table -> 
	let cat = table.PDBx.table_name in
	fprintf oc 
	  "      | \"%s\" -> { d with %s = List.map %s.%s lst }\n" 
	  cat (conv_catname cat) (String.capitalize cat) of_func)
      dict.PDBx.tables;
    fprintf oc "      | _ -> d)\n";
    fprintf oc "    { default with datablockName = dbName } lst\n\n";
  in
  make_of_x "of_mmCIF";
(*  make_of_x "of_PDBML";*)

  let make_to_x to_func =
    fprintf oc "  let %s d =\n" to_func;
    fprintf oc "    let lst = List.filter (fun (_,v) -> v <> []) [\n";
    List.iter 
      (fun table -> 
	let cat = table.PDBx.table_name in
	fprintf oc "      \"%s\",(List.map %s.%s d.%s);\n"
	  cat (String.capitalize cat) to_func (conv_catname cat)
      )
      dict.PDBx.tables;
    fprintf oc "    ] in d.datablockName,lst\n\n";
  in
  make_to_x "to_mmCIF";
(*  make_to_x "to_PDBML";*)

  fprintf oc "end\n\n";
  ()

let dump_item_type ?(oc=stdout) dict = 
  fprintf oc "  let type_of_item =\n";
  fprintf oc "    let ht = Hashtbl.create 100 in\n";
  fprintf oc "    List.iter (fun (k,v) -> Hashtbl.add ht k v) [\n";
  List.iter 
    (fun table ->
      fprintf oc "    ";
      List.iter 
	(fun item -> fprintf oc "((\"%s\",\"%s\"),\"%s\"); " 
	    table.PDBx.table_name item.PDBx.i_name item.PDBx.i_type)
	table.PDBx.items;
      fprintf oc "\n";)
    dict.PDBx.tables;
  fprintf oc "    ];\n";
  fprintf oc "    fun (cat,item) -> Hashtbl.find ht (cat,item)\n\n";
  ()

let dump_mandatory_item ?(oc=stdout) dict = 
  fprintf oc "  let item_is_mandatory =\n";
  fprintf oc "    let ht = Hashtbl.create 100 in\n";
  fprintf oc "    List.iter (fun k -> Hashtbl.add ht k ()) [\n";
  List.iter 
    (fun table ->
      fprintf oc "    ";
      List.iter 
	(fun item -> 
	  if item.PDBx.i_mandatory
	  then
	    fprintf oc "(\"%s\",\"%s\"); " 
	      table.PDBx.table_name item.PDBx.i_name)
	table.PDBx.items;
      fprintf oc "\n";)
    dict.PDBx.tables;
  fprintf oc "    ];\n";
  fprintf oc "    fun (cat,item) -> Hashtbl.mem ht (cat,item)\n\n";
  ()


let dump_helper ?(oc=stdout) dict_file dict dictinfo =
  let dict_file = Filename.basename dict_file in
  let dbase = Filename.chop_suffix dict_file ".dic" in
  let _,xsd_file = String.replace ~str:dbase ~sub:"mmcif_pdbx_" ~by:"pdbx-" in
  fprintf oc "module PDBjDict = struct\n";
  fprintf oc "  let dictionary_file = \"%s\"\n" dict_file; 
  fprintf oc "  let xsd_file = \"%s.xsd\"\n" xsd_file; 
  fprintf oc "  let dictionary_title = \"%s\"\n" (List.assoc "dictionary.title" dictinfo); 
  fprintf oc "  let dictionary_datablock_id = \"%s\"\n" (List.assoc "dictionary.datablock_id" dictinfo); 
  fprintf oc "  let dictionary_version = \"%s\"\n" (List.assoc "dictionary.version" dictinfo); 
  fprintf oc "\n";
  fprintf oc "  let some_float s = if s = \"?\" || s = \".\" then None else Some (float_of_string s)\n";
  fprintf oc "  let some_int s = if s = \"?\" || s = \".\" then None else if String.starts_with s \"+\" then Some (int_of_string (String.lchop s)) else Some (int_of_string s)\n";

  dump_item_type dict;
  dump_pkey_list dict;
  dump_mandatory_item dict;
  dump_links dict;
  dump_mmcif_to_pdbml_item dict;
  fprintf oc "end\n\n";
  ()


let jv_cat = 
  { PDBx.table_name = "jV_command";
    group = [];
    items = [
    {PDBx.i_name = "name"; i_type = "text"; i_mandatory=true; i_enum=[]};
    {PDBx.i_name = "command"; i_type = "text"; i_mandatory=false; i_enum=[]};
    {PDBx.i_name = "document"; i_type = "text"; i_mandatory=false; i_enum=[]}];
    pkey = ["name"]}

let main3 () =
  let dict_file = Sys.argv.(1) in
  let dict = PDBx.read_dictionary dict_file in
  let dictinfo = Star.get_items_of_category dict.PDBx.star "dictionary" in

  let dict = {dict with PDBx.tables = jv_cat::dict.PDBx.tables} in

  dump_pre dictinfo;
  dump_helper dict_file dict dictinfo;

  List.iter table2mod dict.PDBx.tables;
  datablock dict;
  ()

let _ = main3 ()

