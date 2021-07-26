open PDBjTk
open Printf
open Util

let debug = false

let nfetch = 2000
let default_max_score = 1000.0

let trefsets_table = "TRefsets"

type opt = {
    search: itype list;
    sub: string;
    dcut: float;
    min_score: float;
    max_score: float;
    nlimit: int;
    random: bool;
    pdbset: string list; (* limiting search to particular PDB entries *)
    if_set: string list; (* list of (if_id) limiting search to particular interfaces *)
  }

let opt_default = 
  {search = []; 
   pdbset = [];
   if_set = [];
   sub=""; dcut=1.5; min_score=40.0; max_score= default_max_score;
   nlimit = max_int; random=false}

type result = {
    itype: itype;
    if_id: string;
    rs_id: string;
    qrs_id: string;
    score: float;
    cnt: int;
    frame: Refset.frame_t;
  }

type presult = {
    itype_p: itype;
    if_id_p: string;
    lgi: result list;
  }

let result_default =
  {itype=NONPOLYMER; if_id=""; rs_id=""; qrs_id=""; score=0.0; cnt=0; 
   frame=Refset.default_frame;}


let cmp_result a b =
  if a.score > b.score then -1
  else if a.score < b.score then 1
  else if a.cnt > b.cnt then -1
  else if a.cnt < b.cnt then 1
  else if a.if_id < b.if_id then -1
  else if a.if_id > b.if_id then 1
  else if a.rs_id < a.rs_id then -1
  else if a.rs_id > a.rs_id then 1
  else if a.qrs_id < a.qrs_id then -1
  else if a.qrs_id > a.qrs_id then 1
  else 0

let sort_results ?(random=false) results = 
  let cmp = 
    if random then fun _ _ -> Random.int 10000 - Random.int 10000
    else cmp_result in
  List.sort ~cmp results

  

(** parameters for "delta" *)

let get_params conn itype =
  let q = "SELECT * FROM Refsets_param WHERE type = $1" in
  let res = Sql.select conn ~params:[|itype|] q in
  let ht = Ht.create 50 in
  Array.iteri (fun i f ->
    let fname = res#fname i in
    Ht.add ht fname f) (res#get_tuple 0);
  ht
    
let make_between t1 t2 params col =
  try
    let d = Ht.find params (String.lowercase col) in
    sprintf "%s.%s BETWEEN %s.%s - %s AND %s.%s + %s"
      t1 col t2 col d t2 col d
  with Not_found -> fprintf stderr "make_between: %s not found\n" col;
    raise Not_found

let make_ft_delta params =
  let l = List.map (make_between "r" "s" params) Refset.ft_names in
  String.concat "\n AND " l

let limit_pdb conn = function
  | [] -> "" 
  | pdbset -> 
      let lst = List.map (fun s -> "'" ^ String.uppercase s ^ "'") pdbset in
      let lst = String.concat "," lst in
      " AND i.pdbid = ANY (ARRAY[" ^ lst ^ "])"

let limit_obj conn = function
  | [] -> ""
  | obj_set ->
    let lst = List.map (fun i -> sprintf " r.if_id = '%s' " i) obj_set in
    " AND (" ^ (String.concat " OR " lst) ^ ")"

let make_query conn opt itype =
  let sitype = string_of_itype itype in
  let params = get_params conn sitype in
  let tabs = sprintf "FROM Refaco_%s%s r, %s s" 
      sitype opt.sub trefsets_table in
  let pdbset = limit_pdb conn opt.pdbset in
  let objset = limit_obj conn opt.if_set in
  let latt = 
    let h = "array_count_intersect(r.lattice, s.lattice)" in
    let m = "LEAST(r.natoms, s.natoms)" in
    if opt.max_score < default_max_score
    then
      sprintf "%s BETWEEN %f * %s AND %f * %s\n"  
	h (opt.min_score /. 100.0) m (opt.max_score /. 100.0) m
    else
      sprintf "%s > %f * %s\n" h (opt.min_score /. 100.0) m
  in
  let head = 
    "SELECT r.type, r.if_id, r.rs_id, s.rs_id AS qrs_id, r.frame\n"
    ^ ", array_count_intersect(r.lattice, s.lattice) AS cnt "
    ^ ", LEAST(r.natoms,s.natoms) AS smin\n"
    ^ tabs ^ "\nWHERE"
  in
  let cond = String.concat "\n AND " 
      (if opt.random
      then []
      else [make_ft_delta params; latt])
      ^ pdbset ^ objset
  in
  let tail = 
    if opt.random 
    then "ORDER BY RANDOM () LIMIT 10000" 
    else ""
  in
  String.concat "\n" [head; cond; tail]

let process_sql_res conn opt res ls =
  let n = res#ntuples in
  let tuples = res#get_all in
  let nls = ref ls in
  for i = 0 to n - 1 do
    let t = tuples.(i) in
    let titype = itype_of_string t.(0) in
    let if_id = t.(1) and rs_id = t.(2) 
    and qrs_id = t.(3) in
    let frame = Refset.decode_frame t.(4) in
    let cnt = int_of_string t.(5) in
    let smin = float_of_string t.(6) in
    let score = float (100 * cnt) /. smin in
    let r = {itype=titype; if_id=if_id; rs_id=rs_id; qrs_id=qrs_id; 
	     score=score; cnt=cnt; frame=frame; } in
    nls := r :: !nls
  done;
  !nls

let process_sql_cursor conn opt query =
  Sql.command conn ("DECLARE gicursor NO SCROLL CURSOR FOR " ^ query);
  let proc = process_sql_res conn opt in
  let fetch = sprintf "FETCH %d FROM gicursor" nfetch in
  let rec loop l =
    let res = Sql.select conn fetch in
    if res#ntuples = 0 then l else loop (proc res l)
  in
  let results = loop [] in
  Sql.command conn "CLOSE gicursor";
  results

let create_refsets_table conn ?(temp=true) table =
  let q = 
    "CREATE" 
    ^ (if temp then " TEMPORARY " else " ") 
    ^ (sprintf "TABLE %s (LIKE Refaco)"
	 table) in
  Sql.command conn q

let drop_refsets_table conn table =
  let q = "DROP TABLE IF EXISTS " ^ table in
  Sql.command conn q

let fill_refsets conn ?(temp=true) ?(table=trefsets_table) opt atoms refsets =
  let params, q = 
    Refset.make_sql_insert conn table ~dcut:opt.dcut "query" atoms refsets in
  Sql.transaction conn (fun () ->
    if temp then 
      Sql.command conn ("TRUNCATE " ^ table)
    else
      (drop_refsets_table conn table;
       create_refsets_table conn ~temp table);
    Sql.command conn ~params q)

let init conn = create_refsets_table conn trefsets_table
let finish conn = drop_refsets_table conn trefsets_table

let search conn ?(clog=stderr) ?(opt=opt_default) atoms = function 
  | [] -> []
  | refsets ->
     fill_refsets conn opt atoms refsets;
     let results = List.fold_left 
	             (fun rl itype ->
	               let query = make_query conn opt itype in
	               let rt = Sql.transaction
                                  conn 
		                  (fun () -> process_sql_cursor conn opt query) in
	               List.rev_append rt rl)
	             [] opt.search in
     List.take opt.nlimit (sort_results ~random:opt.random results)

(* assuming all results are from the same template *)
let cluster_frames frefsets lgi =
  let dcut = 1.5 in
  let qframe qrs_id = (frefsets qrs_id).Refset.frame in
  let lgi = sort_results lgi in
  let rec loop ngi = function
    | [] -> List.rev ngi
    | h::rest ->
      let oq0 = quad1 (qframe h.qrs_id) in
      let ot0 = quad1 h.frame in
      let lgi = List.fold_left 
	  (fun lgi t ->
	    let qp = Refset.transform_point (qframe t.qrs_id) oq0 in
	    let tp = Refset.transform_point t.frame ot0 in
	    let d0 = Vec3.distance2 qp tp in
	    if d0 < dcut then lgi else t::lgi)
	  [] rest in
      loop (h::ngi) (List.rev lgi)
  in loop [] lgi

let partition_results refsets gires = 
  let ht = Ht.create 1000 in
  let frefsets = Refset.make_frefsets refsets in
  List.iter (fun gi -> 
    let key = gi.if_id, gi.itype in
    let l = Ht.find_default ht key [] in
    Ht.replace ht key (gi::l)) gires;
  let l = Ht.fold (fun (i,t) l ls -> 
    let l = cluster_frames frefsets l in
    {itype_p=t; if_id_p=i; lgi=l}::ls) ht [] in
  List.sort ~cmp:(fun a b -> List.length b.lgi - List.length a.lgi) l


let pseudo_partition_results refsets gires = 
  let gires = partition_results refsets gires in
  List.fold_left (fun l pgi -> 
    List.fold_left (fun l gi -> {pgi with lgi=[gi]}::l) l pgi.lgi) [] gires
