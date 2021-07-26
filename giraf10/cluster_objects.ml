open Util
open Printf
open ClusterBase
open ExtList

type opt = {
    conninfo: string;
    itype: string;
    scut: float;
    query: string;
    decomp: bool;
    expand: bool;
    cmethod: string;
  }

let sql_example = 
  "\"SELECT if_id1, if_id2, irscore"
  ^ " FROM all_vs_all_nonpolymer"
  ^ " WHERE irscore >= 15"
  ^ " ORDER BY irscore DESC, nali DESC"

let get_opts () =
  let opt = ref {conninfo=""; itype="\\N"; scut = 10.0; query="";
		 decomp=false; expand=false; cmethod = "1"} in
  let specs = [
    ("-conninfo", Arg.String (fun s -> opt := {!opt with conninfo = s}),
     "conninfo");
    ("-itype", Arg.String (fun s -> opt := {!opt with itype = s}),
     " itype (e.g., nonpolymer, ppi, dnarna, ...)");
    ("-scut", Arg.Float (fun s -> opt := {!opt with scut = s}),
     " cutoff score");
    ("-decomp", Arg.Bool (fun b -> opt := {!opt with decomp = b}),
     " just decompose the similarity network (default: false)");
    ("-expand", Arg.Bool (fun b -> opt := {!opt with expand = b}),
     " expand complete-linkage clusters (default: false)");
    ("-method", Arg.String (fun s -> opt := {!opt with cmethod = s}),
     " 1 or 2 [1] (with or without Graph structure)")
  ] in
  let usage = "cluster_objects [options] query"
    ^ "\nExample query:\n" ^ sql_example in
  Arg.parse specs (fun s -> opt := {!opt with query = s}) usage;
  if !opt.conninfo = "" || !opt.itype = "" || !opt.query = "" then
    (Arg.usage specs usage; exit 1);
  !opt

module VO = struct
  type t = string (* if_id *)
  let equal (a: string) b = a = b
  let hash = Hashtbl.hash
  let compare = String.compare
end

module Cl = Cluster(VO)

module G = Cl.G

let make_sim_net opt =
  let conn = new PG.connection ~conninfo:opt.conninfo () in
  Sql.command conn "BEGIN";
  Sql.command conn ("DECLARE myc NO SCROLL CURSOR FOR " ^ opt.query);
  let graph = G.create ~size:1000000 () in
  let rec loop time0 =
    let res = Sql.select conn "FETCH 100000 FROM myc" in
    let time1 = Unix.gettimeofday () in
    let n = res#ntuples in
    fprintf stderr "# %d tuples (time = %f)\n" n (time1 -. time0);
    flush stderr;
    if n = 0 then ()
    else 
      (for i = 0 to n - 1 do
        let t = res#get_tuple i in
        let v1 = t.(0) in
        let v2 = t.(1) in
        if v1 = v2 then ()
        else if G.mem_edge graph v1 v2
	then ()
        else 
          let score = float_of_string t.(2) in
	  G.add_edge_e graph (v1, score, v2);
      done;
       loop time1)
  in
  let time0 = Unix.gettimeofday () in
  loop time0;
  let time1 = Unix.gettimeofday () in
  fprintf stderr "# Created Graph (time = %f)\n" (time1 -. time0);
  Sql.command conn "CLOSE myc";
  Sql.command conn "COMMIT";
  conn#finish;
  graph

let make_ht_smat opt =
  let conn = new PG.connection ~conninfo:opt.conninfo () in
  Sql.command conn "BEGIN";
  Sql.command conn ("DECLARE myc NO SCROLL CURSOR FOR " ^ opt.query);
  let ht_smat = Ht.create 100000 in
  let rec loop time0 =
    let res = Sql.select conn "FETCH 100000 FROM myc" in
    let time1 = Unix.gettimeofday () in
    let n = res#ntuples in
    fprintf stderr "# %d tuples (time = %f)\n" n (time1 -. time0);
    flush stderr;
    if n = 0 then ()
    else 
      (for i = 0 to n - 1 do
        let t = res#get_tuple i in
        let (v1,v2) as p = if t.(0) < t.(1) then t.(0),t.(1) else t.(1),t.(0) in
        if v1 = v2 then ()
        else if Ht.mem ht_smat p
	then ()
        else 
          let score = float_of_string t.(2) in
	  Ht.add ht_smat p score;
      done;
       loop time1)
  in
  let time0 = Unix.gettimeofday () in
  loop time0;
  let time1 = Unix.gettimeofday () in
  fprintf stderr "# Created Graph (time = %f)\n" (time1 -. time0);
  Sql.command conn "CLOSE myc";
  Sql.command conn "COMMIT";
  conn#finish;
  ht_smat

let decompose ht_smat =
  let nn = Ht.create 100000 in
  Ht.iter 
    (fun (v1,v2) s -> 
      let l = Ht.find_default nn v1 [] in
      Ht.replace nn v1 (v2::l); 
      let l = Ht.find_default nn v2 [] in
      Ht.replace nn v2 (v1::l))
    ht_smat;
  let mark = Ht.create 10000 in
  let rec visit v l =
    if Ht.mem mark v then []
    else
      let compo =
	Ht.add mark v ();
	List.fold_left 
	  (fun compo v' -> List.rev_append (visit v' (Ht.find nn v')) compo)
	  [v]
	  l 
      in
      compo
  in
  let compos = Ht.fold 
      (fun v l compos ->
	match visit v l with
	| [] -> compos
	| c -> c::compos)
      nn [] in
  let compos = List.sort 
      ~cmp:(fun a b -> List.length b - List.length a) compos in
  let mkgraph compo = 
    let graph = G.create ~size:(List.length compo) () in
    List.iter 
      (fun v1 -> 
	let l = Ht.find nn v1 in
	List.iter 
	  (fun v2 ->
	    if v1 < v2 && not(G.mem_edge graph v1 v2)
	    then
	      let s = Ht.find ht_smat (v1,v2) in
	      G.add_edge_e graph (v1,s,v2)
	  )
	  l)
      compo;
    graph in
  mkgraph,compos

	
let print_members itype g isl icl cset =
  let elms = List.sort ~cmp:(fun a b ->
    List.length (G.succ g b) - List.length (G.succ g a)) 
      (Cl.S.elements cset) in
  List.iter 
    (fun a -> 
      let if_id = G.V.label a in
      let nn = List.length (G.succ g a) in
      printf "%d:%d\t%s\t%d\t%s\n" isl icl if_id nn itype)
    elms

let print_members_ng itype icl cset =
  Cl.S.iter 
    (fun if_id -> 
      printf "%6.6d\t%s\t%d\t%s\n" icl if_id 0 itype)
    cset

let main opt =
  let graph = make_sim_net opt in
  flush stderr;
  let comps = Cl.decomp graph in
  let oc = if opt.decomp then stdout else stderr in
  List.iteri 
    (fun i comp ->
      fprintf oc "comp: %5d -- %d\n" (i+1) (List.length comp); flush stderr;
      if opt.decomp then ()
      else
	let l = 
	  let l = Cl.do_cluster graph comp in
	  if opt.expand then List.map (Cl.expand graph) l else l in
	List.iteri 
	  (fun j c ->
	    fprintf stderr "  cluster %5d %5d -- %d\n" i j (Cl.S.cardinal c);
	    print_members opt.itype graph i j c
	  )
	  l)
    comps;
  ()

let main2 opt =
  let ht_smat = make_ht_smat opt in
  flush stderr;
  let l = Cl.do_cluster_ng ht_smat in	
  List.iteri 
    (fun j c ->
      fprintf stderr "  cluster %5d %5d -- %d\n" 0 j (Cl.S.cardinal c);
      print_members_ng opt.itype j c)
    l;
  ()

let main3 opt =
  let printtime message time0 =
    let time1 = Unix.gettimeofday () in
    fprintf stderr "# %s (Elapsed time %f sec.)\n" message (time1 -. time0);
    flush stderr;
    time1
  in
  let time0 = Unix.gettimeofday () in
  let time1 = printtime "Stating" time0  in
  let oc = if opt.decomp then stdout else stderr in
  let ht_smat = make_ht_smat opt in
  let time1 = printtime "Data loaded" time1 in
  let mkgraph,compos = decompose ht_smat in
  let time1 = printtime "Network decomposed" time1 in
  fprintf stderr "# %5d connected components\n" (List.length compos); 
  flush stderr;
  List.iteri 
    (fun i comp ->
      let graph = mkgraph comp in
      fprintf oc "comp: %5d -- %d\n" (i+1) (List.length comp); flush oc;
      if opt.decomp then ()
      else
	let l = 
	  let l = Cl.do_cluster graph comp in
	  if opt.expand 
	  then List.map (Cl.expand graph) l 
	  else l in
	List.iteri 
	  (fun j c ->
	    fprintf stderr "  cluster %5d %5d -- %d\n" i j (Cl.S.cardinal c);
	    print_members opt.itype graph i j c)
	  l)
    compos;
  let _ = printtime "Done." time1 in
  let _ = printtime "Total time." time0 in
  ()

let test opt =
  let ht_smat = make_ht_smat opt in
  flush stderr;
  let _,compos = decompose ht_smat in
  printf "%d components\n" (List.length compos);
  List.iteri (fun i c -> printf "%5d %d\n" i (List.length c)) compos;
  ()

let _ = 
  let opt = get_opts () in
  fprintf stderr "Query: %s\n" opt.query; 
  if opt.cmethod = "1" then main opt
  else if opt.cmethod = "2" then  main2 opt
  else if opt.cmethod = "3" then  main3 opt
  else test opt

