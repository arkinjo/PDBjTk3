open Util
open Printf
open ClusterBase
open ExtList

type opt = {
    conninfo: string;
    scut: float;
    pid: float;
    decomp: bool;
    expand: bool;
  }

let get_opts () =
  let opt = ref {conninfo=""; scut=0.0; pid=0.0; decomp=false; 
		 expand=true} in
  let specs = [
    ("-conninfo", Arg.String (fun s -> opt := {!opt with conninfo = s}),
     "conninfo");
    ("-cutoff", Arg.Float (fun f -> opt := {!opt with scut = f}),
     " cut-off score");
    ("-pid", Arg.Float (fun f -> opt := {!opt with pid = f}),
     " cut-off percent identity");
    ("-decomp", Arg.Bool (fun b -> opt := {!opt with decomp = b}),
     " just decompose the similarity network (default: false)");
    ("-expand", Arg.Bool (fun b -> opt := {!opt with expand = b}),
     " expand complete-linkage clusters (default: true)");
  ] in
  let usage = "cluster_blast [options]" in
  Arg.parse specs (fun _ -> ()) usage;
  if !opt.conninfo = "" then
    (Arg.usage specs usage; exit 1);
  !opt

module VO = struct
  type t = string * string (* pdbcode, entity_id *)
  let equal ((a1,b1):t) (a2,b2) = a1 = a2 && b1 = b2
  let hash = Hashtbl.hash
  let compare ((a1,b1):t) (a2,b2) = 
    if a1 < a2 then -1
    else if a1 > a2 then 1
    else if b1 < b2 then -1
    else if b1 > b2 then 1
    else 0
end

module Cl = Cluster(VO)

module G = Cl.G

let make_ttab conn =
  let q = "CREATE TEMP TABLE _tcomp(pdbcode INTEGER, label_asym_id TEXT, PRIMARY KEY(pdbcode, label_asym_id))" in
  Sql.command conn q

let get_asyms conn =
  let q = "SELECT pdbcode, label_asym_id FROM objects" in
  let res = Sql.select conn q in
  Array.fold_left (fun l a -> (a.(0),a.(1)) :: l) [] res#get_all

let get_comps opt conn asyms =
  let pid = string_of_float opt.pid in
  let ht = Ht.create 1000 in
  let getone ((pdbcode,aid) as h) =
    if Ht.mem ht h then []
    else
      let q = "SELECT pdbcode, label_asym_id FROM blast_sim_comp($1,$2,$3)" in
      let params = [| pdbcode; aid; pid|] in
      let res = Sql.select conn ~params q in
      Array.fold_left (fun l t -> 
	let k = (t.(0),t.(1)) in
	Ht.add ht k ();
	k :: l) [] res#get_all
  in
  let rec loop comps = function
    | [] -> List.sort ~cmp:(fun a b -> List.length b - List.length a) comps
    | h::rest ->
	match getone h with
	| [] | [_] -> loop comps rest
	| c -> 
	    fprintf stderr "get_comps: %5d (%s,%s)\n" 
	      (List.length c) (fst h) (snd h);
	    flush stderr;
	    loop (c::comps) rest
  in
  loop [] asyms

let make_sim_net opt conn comp =
  Sql.command conn "TRUNCATE _tcomp";
  Sql.command conn "BEGIN";
  List.iter (fun (pdbcode,aid) ->
    Sql.command conn ~params:[|pdbcode; aid|] 
      "INSERT INTO _tcomp VALUES($1,$2)") comp;
  Sql.command conn "COMMIT";
  (* blast_pair_sum_asym IS symmetric *)
  let q = sprintf
      "SELECT pdbcode1,label_asym_id1,pdbcode2,label_asym_id2,score FROM blast_pair_sum_asym p JOIN  _tcomp t ON t.pdbcode = p.pdbcode1 AND t.label_asym_id = p.label_asym_id1 " in
  let q = 
    if opt.scut > 0.0 && opt.pid > 0.0 
    then sprintf "%s WHERE score >= %f AND pidentity >= %f" q opt.scut opt.pid
    else if opt.scut > 0.0 
    then sprintf "%s WHERE score >= %f" q opt.scut
    else if opt.pid > 0.0 
    then sprintf "%s WHERE pidentity >= %f" q opt.pid 
    else q in
  Sql.command conn "BEGIN";
  Sql.command conn ("DECLARE myc NO SCROLL CURSOR FOR " ^ q);
  let graph = G.create ~size:(List.length comp) () in
  let ht = Ht.create 100 in
  let getv l =
    if Ht.mem ht l then Ht.find ht l 
    else
      let v = G.V.create l in
      Ht.add ht l v;
      v
  in
  let rec loop () =
    let res = Sql.select conn "FETCH 100000 FROM myc" in
    let n = res#ntuples in
    if n = 0 then ()
    else 
      (for i = 0 to n - 1 do
        let t = res#get_tuple i in
        let t1 = t.(0),t.(1) in
        let t2 = t.(2),t.(3) in
	let v1 = getv t1 and v2 = getv t2 in
        if G.V.equal v1 v2 then ()
        else
          let score = float_of_string t.(4) in
	  let ne = G.E.create v1 score v2 in
          if G.mem_edge graph v1 v2 then
            let e = G.find_edge graph v1 v2 in
	    let sp = G.E.label e in
            if score > sp then
              (G.remove_edge graph v1 v2;
               G.add_edge_e graph ne)
            else ()
          else
            G.add_edge_e graph ne;
      done;
       loop ())
  in
  loop ();
  Sql.command conn "CLOSE myc";
  Sql.command conn "COMMIT";
  graph

let print_members g isl icl cset =
  let elms = List.sort ~cmp:(fun a b ->
    List.length (G.succ g b) - List.length (G.succ g a)) 
      (Cl.S.elements cset) in
  List.iter 
    (fun a -> 
      let pdbcode,obj_id = G.V.label a in
      let nn = List.length (G.succ g a) in
      printf "%d\t%d\t%s\t%s\t%d\n" isl icl pdbcode obj_id nn)
    elms

let main () =
  let opt = get_opts () in
  let conn = new PG.connection ~conninfo:opt.conninfo () in
  make_ttab conn;
  let asyms = get_asyms conn in
(*  let asyms = [("3280789","A-1")] in*)
  let comps = get_comps opt conn asyms in
  List.iteri (fun i comp ->
    fprintf stderr "comp: %5d -- %d\n" i (List.length comp);
    let graph = make_sim_net opt conn comp in
    let cs = G.fold_vertex (fun v l -> v::l) graph [] in
    if opt.decomp then ()
    else
      let l = 
	let l = Cl.do_cluster graph cs in
	if opt.expand then
	  List.map (Cl.expand graph) l
	else l in
      List.iteri (fun j c ->
	fprintf stderr "  cluster %5d %5d -- %d\n" i j (Cl.S.cardinal c);
	print_members graph i j c
		 ) l) 
    comps;
  conn#finish;
  ()

let _ = 
  try 
    main ()
  with PG.Error e -> 
    prerr_endline (PG.string_of_error e);
    exit 1
