open Printf
open Graph
open ExtLib

module Ht = Hashtbl

module V = struct
  type t = string
  let compare = String.compare
  let equal (a: string) (b: string) = a = b
  let hash = Hashtbl.hash
end

module E = struct
  type t = string
  let default = "?"
  let compare = String.compare
end

module G = Imperative.Graph.ConcreteLabeled (V) (E)

module C = Components.Make (G)

let make_net () =
  let graph = G.create () in
  let ht = Ht.create 100 in
  let lines = Std.input_lines stdin in
  let rec loop () =
    match Enum.get lines with
    | None -> ()
    | Some l ->
	let t = String.nsplit l "\t" in
	let t = Array.of_list t in
	Ht.add ht (t.(0),t.(2)) ();
        G.add_edge_e graph (t.(0),t.(1),t.(2));
	loop ()
  in
  loop ();
  graph,ht

let anacomp comp =
  let ht = Ht.create 5 in
  let cs = ['K'; 'S'; 'M'; 'N'; 'P'; 'A'; 'B'] in
  List.iter (fun c -> Ht.add ht c 0) cs;
  List.iter (fun c ->
    let n = Ht.find ht c.[0] in
    Ht.replace ht c.[0] (n+1)) comp;
  List.map (fun c -> (c,Ht.find ht c)) cs

let main () =
  let node = ref "?" in
  let icomp = ref (-1) in
  let stat = ref false in
  let clust = ref false in
  let specs = [("-v", Arg.Set_string node, "select component containing the node");
	       ("-icomp",Arg.Set_int icomp, "select i-th component");
	       ("-stat", Arg.Set stat, "count statistics");
	       ("-cluster", Arg.Set clust, "list clusters")] in
  Arg.parse specs (fun s -> icomp := int_of_string s) "usage: decomp_graph [options] < dat.tsv";
  let graph,norder = make_net () in
  let comps = C.scc_list graph in
  let comps = List.sort
      ~cmp:(fun a b -> List.length b - List.length a) comps in
  if !stat then
    List.iteri 
      (fun i l -> 
	let cnt = anacomp l in
	printf "component.. %5d %5d " i (List.length l);
	List.iter 
	  (fun (k,v) -> if v > 0 then printf " (%c %5d) " k v) cnt;
	printf "\n")
      comps
  else if !clust then
    List.iteri 
      (fun i l -> 
	List.iter 
	  (fun c -> printf "%d\t%s\n" i c) l)
      comps
  else
    let comp = 
      if !node = "?" 
      then 
	List.nth comps !icomp
      else
	let n,fcomp = C.scc graph in
	let icomp = fcomp !node in
	List.find (fun comp -> icomp = fcomp (List.hd comp)) comps
    in
    let ht = Ht.create (List.length comp) in
    List.iter (fun c -> Ht.add ht c ()) comp;
    G.iter_edges_e 
      (fun (v1,e,v2) ->
	if Ht.mem ht v1 then
	  if Ht.mem norder (v1,v2) then
	    printf "%s\t%s\t%s\n" v1 e v2
	  else
	    printf "%s\t%s\t%s\n" v2 e v1)
      graph;
    ()

let _ = 
  try main () 
  with exc -> 
    prerr_endline (Printexc.to_string exc);
    prerr_endline "Usage: decomp_graph [icomp] < data";
    exit 1
