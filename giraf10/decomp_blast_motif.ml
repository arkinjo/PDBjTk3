open Util
open Printf
open Graph
open ExtList


module G = Imperative.Graph.AbstractLabeled (String) (ClusterBase.EBase)

module C = Components.Make (G)

let make_sim_net () =
  let graph = G.create () in
  let ht = Ht.create 100 in
  let getv l =
    if Ht.mem ht l then Ht.find ht l 
    else
      let v = G.V.create l in
      Ht.add ht l v;
      v
  in
  let lines = Std.input_lines stdin in
  let rec loop () =
    match Enum.get lines with
    | None -> ()
    | Some l ->
	let t = String.nsplit l "\t" in
	let t = Array.of_list t in
	let t1 = t.(0) in
	let t2 = t.(2) in
	let score = float_of_string t.(3) in
	let v1 = getv t1 and v2 = getv t2 in
	if G.V.equal v1 v2 then ()
	else
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
	  loop ()
  in
  loop ();
  graph

type node_stat = 
    { nb: int; eb: int;
      nm: int; em: int;
      nn: int; en: int;
      np: int; ep: int;
      na: int; ea: int
    }

let anacomp graph comp =
  let n = List.length comp in
  let ns = List.fold_left 
      (fun ns v ->
	let l = G.V.label v in
	let k = List.length (G.succ graph v) in
	match l.[0] with
	| 'B' -> {ns with nb = ns.nb + 1; eb = ns.eb + k}
	| 'M' -> {ns with nm = ns.nm + 1; em = ns.em + k}
	| 'N' -> {ns with nn = ns.nn + 1; en = ns.en + k}
	| 'P' -> {ns with np = ns.np + 1; ep = ns.ep + k}
	| 'A' -> {ns with na = ns.na + 1; ea = ns.ea + k}
	| _ -> ns)
      {nb = 0; eb = 0; 
       nm = 0; em = 0;
       nn = 0; en = 0;
       np = 0; ep = 0;
       na = 0; ea = 0}
      comp
  in
  fprintf stderr "stat: %d (b) %d %d (m) %d %d (n) %d %d (p) %d %d (a) %d %d\n" 
    n ns.nb ns.eb ns.nm (ns.nm - ns.nb) ns.nn ns.en ns.np ns.ep ns.na ns.ea
  
let main () =
  let icomp = int_of_string Sys.argv.(1) in
  let graph = make_sim_net () in
  let comps = C.scc_list graph in
  let comps = List.sort ~cmp:(fun a b -> List.length b - List.length a) comps in
(*
  List.iteri (fun i comp ->
    fprintf stderr "comp: %5d -- %d\n" i (List.length comp);
    anacomp graph comp) comps;*)
  let comp = List.nth comps icomp in
  let ht = Ht.create (List.length comp) in
  List.iter (fun c -> Ht.add ht c ()) comp;
  G.iter_edges_e (fun e ->
    let v1 = G.E.src e in
    let v2 = G.E.dst e in
    let l1 = G.V.label v1 in
    let l2 = G.V.label v2 in
    let l1,l2,v1,v2 = if l1 < l2 then l1,l2,v1,v2 else l2,l1,v2,v1 in
    if Ht.mem ht v1 then
      printf "%s\t(bm)\t%s\t%.0f\n" l1 l2 (G.E.label e)) graph;
  ()

let _ = 
  try main () 
  with _ -> 
    prerr_endline "Usage: decomp_blast_motif [icomp] < data";
    exit 1
