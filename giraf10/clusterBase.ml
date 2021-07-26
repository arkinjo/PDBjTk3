open Printf
open Graph
open ExtList
open PDBjTk

module Ht = ExtHashtbl.Hashtbl

(** base undirected graph (similarity network) *)
module VBase = struct
  type t = string
  let compare = String.compare
  let hash = Ht.hash
  let equal = ((=) : string -> string -> bool)
  let default = ""
  let to_string s = s
end

module EBase = struct
  type t = float
  let default = 0.0
  let compare (a: float) (b: float) = 
    if a < b then -1
    else if a > b then 1
    else 0
end

module type ClusterSig = sig
  module V: Sig.COMPARABLE
  module G: Sig.I with type V.t = V.t and type V.label = V.t
                    and type E.t = V.t * EBase.t * V.t 
                    and type E.label = EBase.t
  module S: Set.S with type elt = G.vertex

  val decomp: G.t -> G.vertex list list
  val do_cluster: G.t -> G.vertex list -> S.t list
  val do_cluster_ng: (S.elt * S.elt, float) Ht.t -> S.t list
  val expand: ?k:int -> G.t -> S.t -> S.t
end

module Cluster (V: Sig.COMPARABLE) : (ClusterSig with type V.t = V.t) = struct
  (*
    exposed functions are:
    1. decomp g
    2. do_cluster g comp
    3. expand ?(k=0) g c
   *)
  module V = V
  module G = Imperative.Graph.ConcreteLabeled (V) (EBase)
  module C = Components.Make (G)
  module S = Set.Make(G.V)
  module S2 = Set.Make(S)

  let decomp g =
    let comps = C.scc_list g in
    List.sort ~cmp:(fun c d -> List.length d - List.length c) comps


(** for complete-linkage clustering *)
  module P = struct
    type t = (int * int) * float
    let compare ((c1,d1),v1) ((c2,d2),v2) = compare (v1: float) (v2: float)
  end

  module Int = struct
    type t = int
    let compare a b = a - b
  end

  module IntPair = struct
    type t = int * int
    let equal (a1,b1) (a2,b2) = 
      a1 = (a2: int) && b1 = (b2: int)
    let hash (a,b) = 1023 * a + b
  end

  module Hs = Hashtbl.Make(IntPair)
  module IS = Set.Make(Int)

  module Heap = Fheap.SkewBinomialHeap(P)

  let prepare ht_smat =
    let ht_v = Ht.create 1000 in
    Ht.iter
      (fun (v1,v2) _ -> Ht.replace ht_v v1 0; Ht.replace ht_v v2 0) 
      ht_smat;
    let len = Ht.length ht_v in
    let i = ref 0 in
    Ht.iter (fun k _ -> Ht.replace ht_v k !i; incr i) ht_v;
    let nl = Array.make (2*len) IS.empty in
    let smat = Hs.create len in
    Ht.iter 
      (fun (v1,v2) s ->
	let i = Ht.find ht_v v1 in
	let j = Ht.find ht_v v2 in
	let i,j = if i < j then i,j else j,i in
	if not(Hs.mem smat (i,j))
	then
	  (Hs.add smat (i,j) s;
	   nl.(i) <- IS.add j nl.(i);
	   nl.(j) <- IS.add i nl.(j)))
      ht_smat;
    let clust = Array.make (2*len) S.empty in
    Ht.iter (fun v i -> clust.(i) <- S.singleton v) ht_v;
    let heap = Hs.fold (fun p v h -> Heap.add (p,v) h) smat Heap.empty in
    smat,nl,heap,clust

  let make_smat g comp =
    let len = List.length comp in
    let clust = Array.of_list 
	(List.map (fun v -> S.singleton v) comp) in
    let clust = Array.append clust (Array.make len S.empty) in
    let smat = Hs.create len in
    let nl = Array.make (2*len) IS.empty in
    let ht = Ht.create len in
    List.iteri (fun i v -> Ht.add ht (G.V.label v) i) comp;
    List.iteri
      (fun i v ->
	let es = G.succ_e g v in
	List.iter 
	  (fun e ->
	    let w = G.V.label (G.E.dst e) in
	    let s = G.E.label e in
	    if Ht.mem ht w then
	      let j = Ht.find ht w in
	      if i < j && not(Hs.mem smat (i,j))
	      then
		(Hs.add smat (i,j) s;
		 nl.(i) <- IS.add j nl.(i);
		 nl.(j) <- IS.add i nl.(j)))
	  es)
      comp;
    let heap = Hs.fold (fun p v h -> Heap.add (p,v) h) smat Heap.empty in
    smat,nl,heap,clust

  let get_score smat i j =
    if Hs.mem smat (i,j) then Some(Hs.find smat (i,j))
    else if Hs.mem smat (j,i) then Some(Hs.find smat (j,i))
    else None

  let update smat nl heap inew i j =
    Hs.remove smat (i,j); Hs.remove smat (j,i);
    let l1 = nl.(i) and l2 = nl.(j) in
    if l1 = IS.empty || l2 = IS.empty then 
      failwith "Cluster.update (empty)";
    let s = IS.inter l1 l2 in
    let tmat = Hs.create (IS.cardinal s) in
    IS.iter 
      (fun d ->
	match (get_score smat d i),(get_score smat d j) with
	| None,_ -> ()
	| _,None -> ()
	| (Some s1), (Some s2) ->
	    nl.(d) <- IS.add inew nl.(d);
	    Hs.add tmat (d,inew) (min s1 s2))
      s;
    IS.iter
      (fun d -> 
	Hs.remove smat (d,i); Hs.remove smat (i,d); 
	nl.(d) <- IS.remove i nl.(d))
      l1;
    IS.iter
      (fun d -> 
	Hs.remove smat (d,j); Hs.remove smat (j,d);
	nl.(d) <- IS.remove j nl.(d))
      l2;
    nl.(i) <- IS.empty;
    nl.(j) <- IS.empty;
    nl.(inew) <- s;
    Hs.iter (Hs.add smat) tmat;
    let nheap = Hs.fold (fun p v h -> Heap.add (p,v) h) tmat heap in
    nheap

  let find_pair smat heap =
    let rec loop hp =
      if Heap.is_empty hp then None
      else
	let p,_ = Heap.maximum hp in
	if Hs.mem smat p then Some (p, Heap.remove hp)
	else loop (Heap.remove hp)
    in
    match loop heap with
    | None -> raise Not_found
    | Some p -> p

  let rec cluster_core counter smat nl heap clust =
    if Hs.length smat = 0 then 
      Array.fold_left (fun cset c -> 
	if c = S.empty then cset else S2.add c cset) S2.empty clust
    else
      let (i,j),heap = find_pair smat heap in
      let c1 = clust.(i) and c2 = clust.(j) in
      let cnew = S.union c1 c2 in
      let inew = counter () in
      clust.(i) <- S.empty;
      clust.(j) <- S.empty;
      clust.(inew) <- cnew;
      let heap = update smat nl heap inew i j in
      cluster_core counter smat nl heap clust

  let gen_counter n =
    let cnt = ref (n-1) in
    fun () -> incr cnt; !cnt

  let do_cluster g comp =
    let counter = gen_counter (List.length comp) in
    let smat,nl,heap,clust = make_smat g comp in
    let nv = float (List.length comp) in
    let ne = float (Hs.length smat) in
    let rho = if ne = 0.0 then 0.0 else 2.0 *. ne /. (nv *. (nv -. 1.0)) in
    fprintf stderr "Cluster.do_cluster: %5.0f %8.0f %f\n" nv ne rho; 
    flush stderr;
    if rho = 1.0 then
      [List.fold_left (fun c v -> S.add v c) S.empty comp]
    else
      let cset = cluster_core counter smat nl heap clust in
      List.sort ~cmp:(fun c d -> S.cardinal d - S.cardinal c) 
	(S2.elements cset) 

  let do_cluster_ng ht_smat =
    let smat,nl,heap,clust = prepare ht_smat in
    let counter = gen_counter ((Array.length clust)/2) in
    let cset = cluster_core counter smat nl heap clust in
    List.sort ~cmp:(fun c d -> S.cardinal d - S.cardinal c) 
      (S2.elements cset) 
	
(****************************************************************************)  

(* expanded complete-linkage clustering *)

  let make_subgraph g c =
    let v = S.choose c in
    let nvc = List.fold_left (fun nvc w -> S.add w nvc) 
	S.empty (G.succ g v) in
    let nvc = S.fold (fun v nvc -> 
      let nv = List.fold_left (fun nvc w -> S.add w nvc) 
	  S.empty (G.succ g v) in
      S.inter nv nvc) c nvc in
    let comp = S.elements nvc in
    comp,(S.cardinal nvc)

  let kselect k cset =
    let l = List.sort ~cmp:(fun c d -> S.cardinal d - S.cardinal c) cset in
    if k > 0 then List.take k l else l
      
  let check_cond nv c cmax = nv + S.cardinal c < S.cardinal cmax
      
  let rec expand_core k g c cmax =
    let bigger c d = if S.cardinal c > S.cardinal d then c else d in
    let comp,nv = make_subgraph g c in
    if nv = 0 || check_cond nv c cmax then bigger c cmax
    else
      match do_cluster g comp with
      | [] -> bigger c cmax
      | clusters ->
	  let clst = kselect k clusters in
	  let cmax = List.fold_left
	      (fun cmax c' ->
		let c'' = expand_core k g (S.union c c') cmax in
		bigger c'' cmax)
	      cmax clst in
	  cmax

  let expand ?(k=0) g c = expand_core k g c c 
end

module TT = Cluster(VBase)

