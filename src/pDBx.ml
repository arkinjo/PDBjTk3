open Star_syntax
open Printf
open ExtLib
module Ht = Hashtbl

let warn s = 
  prerr_endline ("Warning(PDBx): " ^ s);
  flush stderr

type dtype = {
    d_code: string;
    d_primitive: string;
    d_construct: string;
    d_re: Pcre.regexp;
    d_detail: string;
  }

type item = {
    i_name: string;
    i_type: string;
    i_mandatory: bool;
    i_enum: string list;
  }
      
type table = {
    table_name: string;
    group: string list;
    items: item list;
    pkey: string list;
  }

type cross_ref = {
    t_src: string;
    t_dst: string;
    cross_id: string;
    item_pairs: (string * string) list;
  }

type dict = {
    dtypes: dtype list; (* definition of data types *)
    tables: table list; (* definition of tables *)
    cross_refs: cross_ref list; (* cross-reference between categories *)
    star: Star_syntax.star_file;
  }


let category_of_heading str =
  let str = try fst(String.split str ".") with Invalid_string -> str in
  let _,str = String.replace ~str ~sub:"_" ~by:"" in
  str

let chop_category str = snd (String.split str ".")

let select_save_frame star =
  let db = 
    match List.hd star with 
      Data_block (Data_ dh,db) -> db
    | Global_block _ -> raise Not_found in
  let sf = List.filter_map
      (function 
	| Data d -> None
	| Save_frame ((Save_ sh, data) as s) -> Some s)
      db in
  let categs = ref [] in
  let ht = Ht.create 1 in
  List.iter 
    (function (Save_ sh , data ) ->
      if String.starts_with sh "_" then
	let c = category_of_heading sh in
	try
	  Ht.replace ht c (data :: Ht.find ht c)
	with Not_found -> Ht.add ht c [data]
      else
	categs := data :: !categs)
    sf;
  (List.rev !categs),ht

(* Topologically ordering the categories based on cross-refs *)
let order_cross_refs cross_refs =
  let module V = 
    struct
      type t = string (* table name *)
      let hash = Ht.hash
      let equal = (( = ) : string -> string -> bool)
      let compare = String.compare
    end 
  in
  let module G = Graph.Imperative.Digraph.ConcreteBidirectional(V) in
  let module T = Graph.Topological.Make(G) in
  let g = G.create () in
  List.iter (fun r -> G.add_edge g r.t_src r.t_dst) cross_refs;
  let ocats = T.fold (fun v l -> v::l) g [] in
  let cinds = Ht.create 10 in
  List.iteri (fun i c -> Ht.add cinds c i) ocats;
  let cmp r s = 
    let i1 = Ht.find cinds r.t_src in
    let j1 = Ht.find cinds r.t_dst in
    let i2 = Ht.find cinds s.t_src in
    let j2 = Ht.find cinds s.t_dst in
    if i1 < i2 then -1
    else if i1 > i2 then 1
    else if j1 < j2 then -1
    else if j1 > j2 then 1
    else 0 in
  List.sort ~cmp cross_refs

let select_links star =
  let items,data = 
    Star.get_loop_of_category star "pdbx_item_linked_group_list" in
  let ht = Ht.create 10 in
  List.iter 
    (fun l ->
      let a = Array.of_list l in
      let t_src = a.(0) and t_dst = a.(4) in
      let cross_id = a.(1) in
      let i1 = chop_category a.(2) in
      let i2 = chop_category a.(3) in
      let k = (t_src,t_dst,cross_id) in
      let n = Ht.find_default ht k [] in
      Ht.replace ht k ((i1,i2) :: n)) 
    data;
  let ls = Ht.fold 
      (fun (t_src,t_dst,cross_id) l lst -> 
	let c = {t_src = t_src; t_dst = t_dst; cross_id = cross_id; 
		 item_pairs = List.rev l} in
	c::lst)
      ht [] in
  order_cross_refs ls
    
let select_type ht =
  let httype = Ht.create 1 in
  Ht.iter 
    (fun category_id items ->
      List.iter
	(fun item ->
	  let ifind = Star.data_find_item_list item in 
	  let name = ifind "item.name" in
	  let name = chop_category (Star.unquote_data_value (List.hd name)) in
	  let item_type = ifind "item_type.code" in
	  let item_type = 
	    (try 
	      Some (Star.unquote_data_value (List.hd item_type))
	    with _ -> None) in
	  let item_null = Star.unquote_data_value 
	      (List.hd (ifind "item.mandatory_code")) in
	  Ht.add httype (category_id,name) (item_type,item_null))
	items) 
    ht;
  httype

let make_table ht =
  let types = select_type ht in
  fun dl ->
    let find = Star.data_find_item_list dl in
    let cids = find "category.id" in
    let category_id = Star.unquote_data_value (List.hd cids) in
    let group = List.map Star.unquote_data_value (find "category_group.id") in
    let pkey = List.map chop_category 
	(List.map Star.unquote_data_value (find "category_key.name")) in
    let items = List.rev (try Ht.find ht category_id with Not_found -> []) in
    let htdup = Ht.create 10 in 
    let data = List.map 
	(fun item ->
	  let ifind = Star.data_find_item_list item in 
	  let name = ifind "item.name" in
	  let full_name = Star.unquote_data_value (List.hd name) in
	  if Ht.mem htdup full_name
	  then
	    warn (sprintf "Redundant definitions: %s" full_name);
	  Ht.add htdup full_name ();
	  let cat2,name = String.split full_name "." in
	  let cat2 = String.lchop cat2 in
	  if cat2 <> category_id then
	    warn (sprintf "Category name mismatch: <<%s>> in save frame, but <<%s>> in item.name. Using the latter."
		    category_id cat2);
	  let item_type,item_null = Ht.find types (category_id,name) in
	  let item_type = 
	    match item_type with
	    | Some a -> a
	    | None -> "?" in
	  let mandatory = (item_null = "yes") in
	  let enums = List.map 
	      Star.unquote_data_value (ifind "item_enumeration.value") in
	  {i_name=name; i_type=item_type; i_mandatory=mandatory; i_enum=enums})
	items in
    {table_name=category_id; group=group; items=data; pkey=pkey}

let select_dtypes star =
  let items,data = Star.get_loop_of_category star "item_type_list" in
  List.map 
    (fun l ->
      let a = Array.of_list l in
      let flags = if a.(1) = "uchar" then [`CASELESS] else [] in
      let re = Pcre.regexp ~flags ("^" ^ a.(2) ^ "$") in
      {d_code = a.(0); d_primitive = a.(1); d_construct = a.(2); 
       d_re = re; d_detail = a.(3)})
    data

let get_table_def dict tname =
  List.find (fun t -> t.table_name = tname) dict.tables

let get_item table iname = 
  try
    List.find (fun i -> i.i_name = iname) table.items
  with Not_found ->
    warn (sprintf "Item not found: %s.%s" table.table_name iname);
    raise Not_found

let get_dtype dict dcode =
  List.find (fun d -> d.d_code = dcode) dict.dtypes

let fix_type_by_link tables refs =
  let ftable = Ht.create 10 in
  List.iter (fun t -> Ht.add ftable t.table_name t) tables;
  let check r =
    let src = Ht.find ftable r.t_src in
    let dst = Ht.find ftable r.t_dst in
    let nidst = Ht.create 10 in
    let nisrc = Ht.create 10 in
    List.iter 
      (fun (is,id) ->
	try
	  let isrc = get_item src is in
	  let idst = get_item dst id in
	  if isrc.i_type = idst.i_type then ()
	  else if idst.i_type = "?" then ()
	  else if isrc.i_type = "?" then 
	    ((*warn (sprintf "Coalesce undefined type: %s.%s <- %s.%s : %s"
		     src.table_name is dst.table_name id idst.i_type);*)
	     Ht.add nisrc is idst.i_type)
	  else 
	    (warn (sprintf "Coalesce inconsistent type: %s.%s: %s <- %s.%s: %s"
		     src.table_name is isrc.i_type
		     dst.table_name id idst.i_type);
	     Ht.add nisrc is idst.i_type)
	with _ -> ())
      r.item_pairs;
    let nitems = List.map 
	(fun i ->
	  if Ht.mem nidst i.i_name 
	  then {i with i_type = Ht.find nidst i.i_name}
	  else i)
	dst.items in
    Ht.replace ftable dst.table_name {dst with items = nitems};
    let nitems = List.map
	(fun i ->
	  if Ht.mem nisrc i.i_name 
	  then {i with i_type = Ht.find nisrc i.i_name}
	  else i)
	src.items in
    Ht.replace ftable src.table_name {src with items = nitems}
  in
  List.iter check refs;
  List.map (fun t -> Ht.find ftable t.table_name) tables

let force_untyped tables =
  let proc t =
    let items = List.map 
	(fun i ->
	  if i.i_type = "?" 
	  then 
	    (warn (sprintf "Force undefined type: %s.%s <- text."
		     t.table_name i.i_name);
	     {i with i_type = "text"})
	  else i)
	t.items in
    {t with items = items} in
  List.map proc tables

let read_dictionary file =
  let star = Star.parse_file file in
  let categs,ht = select_save_frame star in
  let cross_refs = select_links star in
  let dtypes = select_dtypes star in
  let tables = List.map (make_table ht) categs in
  let tables = fix_type_by_link tables cross_refs in
  let tables = force_untyped tables in
  {dtypes=dtypes; tables=tables; cross_refs=cross_refs; star=star}


let dump dict =
  List.iter 
    (fun t ->
      printf "Table: %s\n" t.table_name;
      printf "-- groups: %s\n" (String.concat " " t.group);
      List.iter 
	(fun i ->
	  printf "  %s %s %b (%s)\n" 
	    i.i_name i.i_type i.i_mandatory
	    (String.concat " , " i.i_enum))
	t.items;
      printf "P-Key: (%s)\n" (String.concat "," t.pkey))
    dict.tables
