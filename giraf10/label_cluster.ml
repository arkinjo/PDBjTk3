open Util
open ExtLib
open Printf

let stop_word = 
  let l = [
    "the"; "a"; "an"; "of"; "in"; "for"; "at"; "to"; "from"; "by"; "study";
    "as"; "with"; "and";
    "study"; "studies"; "studied"; "remark"; "described"; "contains";
    "complex"; "complexed"; "component"; "3d"; "structure"; "structures";
    "resolution"; "spectroscopy"; "crystallography"; "crystallographic"; 
    "crystallized"; "crystal"; "angstroms"; "angstrom"
  ]
  in
  let ht = Ht.create 1 in
  List.iter (fun a -> Ht.add ht a ()) l;
  fun w -> Ht.mem ht w

let is_number = 
  let ren = Str.regexp "^[0-9]+$" in
  fun w -> Str.string_match ren w 0

let re_space = Str.regexp "[ \t\n\r]+"
let seg_words s = 
  let cs = Str.regexp "[*{}().,:\\/\"]\\|\\[\\|\\]\\|-" in
  let l = Str.split re_space (Str.global_replace cs " " (String.lowercase s)) in
  List.filter_map (fun w -> 
    if String.length w < 2 then None
    else if stop_word w then None
    else if is_number w then None
    else Some w (*(Paice.stem w)*)) l

let get_minfo htcnt = 
  let tot = Ht.fold (fun _ c tot -> c +. tot) htcnt 0.0 in
  let htc = Ht.create 1 and htw = Ht.create 1 in
  Ht.iter (fun ((cid,w) as k) c -> 
    let p = c /. tot in
    let pc = Ht.find_default htc cid 0.0 in
    Ht.replace htc cid (pc +. p);
    let pw = Ht.find_default htw w 0.0 in
    Ht.replace htw w (pw +. p);
    Ht.replace htcnt k p) htcnt;
  let htmi = Ht.create 1 in
  Ht.iter (fun (cid,w) p -> 
    let pc = Ht.find htc cid and pw = Ht.find htw w in
    let mi = p *. log (p /. (pc *. pw)) in
    let l = Ht.find_default htmi cid [] in
    Ht.replace htmi cid ((w,mi)::l)) htcnt;
  Ht.iter (fun cid l ->
    let l = List.sort ~cmp:(fun (w1,mi1) (w2,mi2) ->
      if mi1 > mi2 then -1
      else if mi1 < mi2 then 1
      else String.compare w1 w2) l in
    Ht.replace htmi cid l) htmi;
  htmi

let get_seq_based_clusters conn tbl_name =
  let res = Sql.select conn 
      (sprintf
	 "SELECT c.sclust,c.cclust,a.description, 
	 a.title || ' ' || a.descriptor || ' ' ||
	 a.l_description || ' ' || a.comp_id 
	 FROM %s c JOIN asym_objects_summary a
	 ON c.pdbcode = a.pdbcode AND c.label_asym_id = a.label_asym_id" 
      tbl_name) in
  res
let get_obj_based_clusters conn tbl_name =
  let res = Sql.select conn 
    ("SELECT clust_id, s.description, s.comp_id || ' ' || s.l_description"
     ^ " FROM clusters_" ^ tbl_name ^ " c"
     ^ " JOIN interfaces_summary_mat s ON s.if_id = c.if_id") 
  in
  res

let filter_annot conn tbl_name =
  let res = 
    match tbl_name with
    | "nonpolymer" 
    | "ppi" 
    | "dnarna" -> get_obj_based_clusters conn tbl_name
    | _ -> get_seq_based_clusters conn tbl_name
  in
  let ht = Ht.create 100 in
  let add_words cid line =
    let ws = seg_words line in
    List.iter (fun w -> 
      let c = Ht.find_default ht (cid,w) 0.0 in
      Ht.replace ht (cid,w) (c +. 1.0)) ws
  in
  Array.iter 
    (fun t ->
      let cid = t.(0) in
(* adding "description" 10 times *)
      for i = 1 to 10 do add_words cid t.(1) done;
      add_words cid t.(2)) 
    res#get_all;
  ht

let clust_data conn tbl_name = 
  let tbl_name = 
    match tbl_name with
      "nonpolymer" | "ppi" | "dnarna" -> "clusters_" ^ tbl_name
    | a -> a
  in
  let res = Sql.select conn 
      (sprintf "SELECT clust_id,count(*) FROM %s GROUP BY clust_id" tbl_name) 
  in
  let ht = Ht.create 1 in
  Array.iter (fun t -> Ht.add ht t.(0) t.(1)) res#get_all;
  ht

let main () =
  let dbname = Sys.argv.(1) in
  let tbl_name = Sys.argv.(2) in
  let nsel = int_of_string Sys.argv.(3) in
  let conn = new PG.connection ~dbname () in
  let htcnt = filter_annot conn tbl_name in
  let htdat = clust_data conn tbl_name in
  let htmi = get_minfo htcnt in
  Ht.iter (fun cid l ->
    let ws = List.take nsel l in
    let cnt = Ht.find htdat cid in
    printf "%s\t%s\t%s\t" cid tbl_name cnt;
    let labels = 
      "{" 
      ^ String.concat "," 
	  (List.map fst ws)
      ^ "}" in
    printf "%s\n" labels
	  ) htmi;
  conn#finish

let _ = 
  try
    main () 
  with
    PG.Error e -> fprintf stderr "%s\n" (PG.string_of_error e);
      exit 1
  | _ -> fprintf stderr "Usage: label_cluster dbname cluster_table n_words\n";
      exit 2
  
    
