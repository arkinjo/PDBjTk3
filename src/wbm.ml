(**
   Weighted bipartite matching by Hungarian algorithm
   
   @author Akira Kinjo
 *)

(* $Id$ *)

(* OCamlGraph *)

open Graph
module type WEIGHT = sig
  type label
  type t
  val weight : label -> t
  val zero : t
  val infinity : t
  val add : t -> t -> t
  val sub : t -> t -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
end

module Hungarian = struct
  module WBM_Common (G: Sig.G) (W: WEIGHT with type label = G.E.label) = struct
    module H = Hashtbl.Make(G.V)

(* ordered type for heaps *)
    module EltSlack = struct
      type t = W.t * G.V.t * G.V.t
      let compare (w1,x1,y1) (w2,x2,y2) =
	let cw = W.compare w2 w1 in
	if cw <> 0 then cw else 
	let cw = G.V.compare x1 x2 in
	if cw <> 0 then cw
	else G.V.compare y1 y2
    end
    module EltLab = struct
      type t = W.t * G.V.t
      let compare (w1,v1) (w2,v2) =
	let cw = W.compare w2 w1 in
	if cw <> 0 then cw else G.V.compare v1 v2
    end

(* imperative binary heaps *)
    module PQSlack = Heap.Imperative(EltSlack)
    module PQLab = Heap.Imperative(EltLab)

    exception No_value
    let opt_get = function
      | None -> raise No_value
      | Some x -> x

(* initializing labels on X- and Y-nodes. *)
    let init_labels graph xv yv = 
      let ht = H.create 17 in
      List.iter (fun v -> 
	let es = G.succ_e graph v in
	match es with
	| [] -> H.add ht v W.zero
	| _ ->
	    let mw = List.fold_left (fun v e -> max v (W.weight (G.E.label e)))
		(W.weight (G.E.label (List.hd es))) es in
	    H.add ht v mw) xv;
      List.iter (fun v -> H.add ht v W.zero) yv;
      ht

    let incr_label labels offset v = 
      let l = H.find labels v in
      H.replace labels v (W.add l offset) 

    let decr_label labels offset v = 
      let l = H.find labels v in
      H.replace labels v (W.sub l offset)

    exception Found of H.key
    let htv_choose ht = 
      try
	H.iter (fun k _ -> raise (Found k)) ht;
	raise Not_found
      with Found k -> k

(* update "ns" and "htback" based on "x":
   N(S) = { y in Y | x in S, x-y in E: pi(x-y) = 0 }.
   See Gupta & Ying, p. 11.
   Here, ns = N(S) \ T, a set from which next y in loop will be chosen.
 *)
    let grow_ns_naive graph labels x ns htback =
      let lx = H.find labels x in
      let ys = G.succ graph x in 
      List.iter (fun y ->
	let ly = H.find labels y in
	let w = W.weight (G.E.label (G.find_edge graph x y)) in
	let pi = W.sub (W.add lx ly) w in
	if W.equal pi W.zero then 
	  (* don't overwrite the existing htback entry 
	     (which is already in tset), otherwise path 
	     to origin may be lost. *)
	  if not (H.mem htback y) then
	    (H.add ns y (); H.add htback y x)) ys

(* update "ns" and "htback" based on "x" *)
    let grow_ns graph labels offset x ns htback =
      let lx = H.find labels x in
      let ys = G.succ graph x in 
      List.iter (fun y ->
	let ly = H.find labels y in
	let w = W.weight (G.E.label (G.find_edge graph x y)) in
	let pi = W.sub (W.add lx ly) w in
	if W.equal pi offset then 
	  if not (H.mem htback y) then
	    (H.add ns y (); H.add htback y x)) ys
	
(* adding a new element into heap, imperative version *)
    let add2heaps graph labels pqlabx pqslack tset x =
      let lx = H.find labels x in
      PQLab.add pqlabx (lx,x);
      let ys = G.succ_e graph x in
      List.iter (fun e ->
	let y = G.E.dst e in
	if not (H.mem tset y) then
	  let ly = H.find labels y in
	  let w = W.weight (G.E.label e) in
	  let pi = W.sub (W.add lx ly) w in
	  PQSlack.add pqslack (pi,x,y)) ys

(* finding alternating and augmenting path by backtracking. *)
    let rec find_path_loop_y path graph gmatch htback xi y = 
      let x = H.find htback y in
      let e = G.find_edge graph x y in
      let npath = e :: path in
      if x = xi then npath 
      else find_path_loop_x npath graph gmatch htback xi x
    and find_path_loop_x path graph gmatch htback xi x =
      let e = List.hd (G.succ_e gmatch x) in
      let y = G.E.dst e in
      find_path_loop_y (e :: path) graph gmatch htback xi y

    let find_augpath = find_path_loop_y []
    let find_altpath = find_path_loop_x [] 

    let min_delta1 labels sset = 
      let x,d = H.fold (fun v _ (x,d) ->
	let w = H.find labels v in
	if W.compare w d < 0 then ((Some v),w) else (x,d)) 
	  sset (None,W.infinity) in
      d,(opt_get x)

    let min_delta2 graph labels sset tset = 
      let x,y,d = 
	H.fold (fun vx _ (x,y,d) -> 
	  let lx = H.find labels vx in
	  let es = G.succ_e graph vx in
	  let y',d' = List.fold_left (fun (y',d') e ->
	    let y = G.E.dst e in
	    if H.mem tset y then (y',d')
	    else
	      let ly = H.find labels y in
	      let w = W.weight (G.E.label e) in
	      let pi = W.sub (W.add lx ly) w in
	      if W.compare pi d' < 0 then (Some y),pi else (y',d')) 
	      (y,d) es in
	  if W.compare d' d < 0 then (Some vx),y',d'
	  else x,y,d) sset (None, None, W.infinity) in
      d,x,y

    let total_weight gmatch = 
      G.fold_edges_e (fun e w -> W.add w (W.weight (G.E.label e))) 
	gmatch W.zero 

(* functor for using functional heap structures. *)
    module FunFheap_Common (FunFheap: functor (X: Fheap.Ordered) -> 
      Fheap.FunctionalSig with type elt = X.t) = struct 

(*
  Currently available functional heap structures are:
  Fheap.LeftistHeap 
  Fheap.BinomialHeap 
  Fheap.PairingHeap 
  Fheap.SkewBinomialHeap 
 *)
	module PQSlackF = Fheap.ExplicitMin (EltSlack) (FunFheap(EltSlack))
	module PQLabF = Fheap.ExplicitMin (EltLab) (FunFheap(EltLab))

            (* adding a new element into heap, functional version *)
	let add2heapsF graph labels pqlabx pqslack tset x =
	  let lx = H.find labels x in
	  let pqlabx = PQLabF.add (lx,x) pqlabx in
	  let ys = G.succ_e graph x in
	  let pqslack = List.fold_left (fun pq e ->
	    let y = G.E.dst e in
	    if H.mem tset y then pq
	    else
	      let ly = H.find labels y in
	      let w = W.weight (G.E.label e) in
	      let pi = W.sub (W.add lx ly) w in
	      PQSlackF.add (pi,x,y) pq) pqslack ys in
	  pqlabx,pqslack
      end
  end

(** Imperative implementation. *)
  module I (G: Sig.I) (W: WEIGHT with type label = G.E.label) = struct

    include WBM_Common (G) (W)

(* symmetric difference for imperative graph. *)
    let symm_diff gmatch path =
      List.iter (fun e -> 
	if G.mem_edge_e gmatch e then
	  G.remove_edge_e gmatch e
	else
	  G.add_edge_e gmatch e) path

(* a naive implementation: O(V^2 * E) worst-case. *)
    let maximum_matching_naive graph xv yv =
      (* swap xv & yv if necessary for efficiency *)
      let xv,yv = if List.length xv <= List.length yv  then xv,yv else yv,xv in
      let labels = init_labels graph xv yv in
      let gmatch = G.create () in
      List.iter (fun xi -> (* iterate over each node in xv *)
	if not (G.mem_vertex gmatch xi) then 
	  let htback = H.create 17 in (* cache for backtracking *)
	  let sset = H.create 1 and tset = H.create 1 and ns = H.create 1 in
	  let rec loop () =
	    if H.length ns > 0 then
	      let y = htv_choose ns in  
	      if not (G.mem_vertex gmatch y) then
		let apath = find_augpath graph gmatch htback xi y in
		symm_diff gmatch apath
	      else (* y is not free. *)
		let z = List.hd (G.succ gmatch y) in
		H.add sset z (); H.add tset y (); H.remove ns y;
		grow_ns_naive graph labels z ns htback;
		loop ()
	    else (* ns = tset *)
	      let delta1,x = min_delta1 labels sset in
	      let delta2,x',y' = min_delta2 graph labels sset tset in 
	      let delta = 
		if W.compare delta1 delta2 <= 0 then delta1 else delta2 in
	      H.iter (fun v _ -> decr_label labels delta v) sset;
	      H.iter (fun v _ -> incr_label labels delta v) tset;
	      if W.equal delta delta1 then
		if G.V.equal xi x then ()
		else begin
		  (* updating htback for backtracking *)
		  grow_ns_naive graph labels x ns htback;
		  let apath = find_altpath graph gmatch htback xi x in
		  symm_diff gmatch apath
		end
	      else (* delta = delta2 *)
		let x',y' = opt_get x', opt_get y' in
		grow_ns_naive graph labels x' ns htback;
		if not (G.mem_vertex gmatch y') then 
		  let apath = find_augpath graph gmatch htback xi y' in
		  symm_diff gmatch apath
		else 
		  let z' = List.hd (G.succ gmatch y') in
		  H.add sset z' (); H.add tset y' (); H.remove ns y';
		  grow_ns_naive graph labels z' ns htback;
		  loop ()
	  in
	  H.add sset xi ();
	  grow_ns_naive graph labels xi ns htback;
	  loop ()) xv;
      gmatch,(total_weight gmatch)

(* implementation using imperative heaps *)
    let maximum_matching_ih graph xv yv =
      let gmatch = G.create () in
      let xv,yv = if List.length xv <= List.length yv  then xv,yv else yv,xv in
      let labels = init_labels graph xv yv in
      List.iter (fun xi ->
	if not (G.mem_vertex gmatch xi) then
	  let htback = H.create 17 in
	  let sset = H.create 1 and tset = H.create 1 and ns = H.create 1 in
	  let pqslack = PQSlack.create 55 and pqlabx = PQLab.create 55 in
	  let rec loop offset =
	    if H.length ns > 0 then
	      let y = htv_choose ns in
	      if not (G.mem_vertex gmatch y) then (* y is free *)
		let apath = find_augpath graph gmatch htback xi y in
		H.iter (fun v _ -> decr_label labels offset v) sset; 
		H.iter (fun v _ -> incr_label labels offset v) tset;
		symm_diff gmatch apath
	      else (* y is not free. *)
		let z = List.hd (G.succ gmatch y) in
		H.add sset z (); H.add tset y (); H.remove ns y;
		incr_label labels offset z; decr_label labels offset y;
		add2heaps graph labels pqlabx pqslack tset z;
		grow_ns graph labels offset z ns htback;
		loop offset 
	    else (* ns = tset *)
	      let delta1,x = PQLab.maximum pqlabx in
	      let delta2,x',y' = 
		let rec skipping () =
		  let d,x,y = PQSlack.maximum pqslack in
		  if H.mem sset x && H.mem tset y then
		    (PQSlack.remove pqslack; skipping ())
		  else
		    d,Some x, Some y
		in
		try skipping () with Fheap.EmptyHeap -> W.infinity,None,None
	      in
	      let delta = 
		if W.compare delta1 delta2 <= 0 then delta1 else delta2 in
	      let offset = delta in
	      if W.equal delta delta1 then 
		if G.V.equal xi x then ()
		else begin
		  grow_ns graph labels offset x ns htback;
		  let apath = find_altpath graph gmatch htback xi x in
		  H.iter (fun v _ -> decr_label labels offset v) sset; 
		  H.iter (fun v _ -> incr_label labels offset v) tset;
		  symm_diff gmatch apath
		end
	      else 
		(* delta = delta2, there exist x', y' s.t. pi(x'y') = delta *)
		let x',y' = opt_get x', opt_get y' in
		grow_ns graph labels offset x' ns htback;
		if not (G.mem_vertex gmatch y') then (* y' is free *)
		  let apath = find_augpath graph gmatch htback xi y' in
		  H.iter (fun v _ -> decr_label labels offset v) sset; 
		  H.iter (fun v _ -> incr_label labels offset v) tset;
		  symm_diff gmatch apath
		else (* y' is not free *)
		  let z' = List.hd (G.succ gmatch y') in
		  H.add sset z' (); H.add tset y' (); H.remove ns y';
		  incr_label labels offset z'; decr_label labels offset y';
		  add2heaps graph labels pqlabx pqslack tset z';
		  grow_ns graph labels offset z' ns htback;
		  loop offset in
	  H.add sset xi ();
	  let offset = W.zero in
	  add2heaps graph labels pqlabx pqslack tset xi;
	  grow_ns graph labels offset xi ns htback;
	  loop offset) xv;
      gmatch,(total_weight gmatch)

(* functorized version using functional heaps *)
    module MM_FunFheap (FunFheap: functor (X: Fheap.Ordered) -> Fheap.FunctionalSig with type elt = X.t) = struct
      
      include FunFheap_Common (FunFheap)

      let maximum_matching graph xv yv =
	let gmatch = G.create () in
	let xv,yv = 
	  if List.length xv <= List.length yv  then xv,yv else yv,xv in
	let labels = init_labels graph xv yv in
	List.iter (fun xi ->
	  if not (G.mem_vertex gmatch xi) then
	    let htback = H.create 17 in
	    let sset = H.create 1 and tset = H.create 1 and ns = H.create 1 in
	    let rec loop offset pqlabx pqslack =
	      if H.length ns > 0 then
		let y = htv_choose ns in
		if not (G.mem_vertex gmatch y) then (* y is free *)
		  let apath = find_augpath graph gmatch htback xi y in
		  H.iter (fun v _ -> decr_label labels offset v) sset; 
		  H.iter (fun v _ -> incr_label labels offset v) tset;
		  symm_diff gmatch apath
		else (* y is not free. *)
		  let z = List.hd (G.succ gmatch y) in
		  H.add sset z (); H.add tset y (); H.remove ns y;
		  incr_label labels offset z; decr_label labels offset y;
		  let pqlabx,pqslack = 
		    add2heapsF graph labels pqlabx pqslack tset z in
		  grow_ns graph labels offset z ns htback;
		  loop offset pqlabx pqslack 
	      else (* ns = tset *)
		let delta1,x = PQLabF.maximum pqlabx in
		let delta2,x',y',pqslack = 
		  let rec skipping pqslack =
		    let d,x,y = PQSlackF.maximum pqslack in
		    if H.mem sset x && H.mem tset y then
		      skipping (PQSlackF.remove pqslack)
		    else
		      d,Some x, Some y,pqslack
		  in
		  try 
		    skipping pqslack 
		  with Fheap.EmptyHeap -> W.infinity,None,None,PQSlackF.empty
		in
		let delta = 
		  if W.compare delta1 delta2 <= 0 then delta1 else delta2 in
		let offset = delta in
		if W.equal delta delta1 then 
		  if G.V.equal xi x then ()
		  else begin
		    grow_ns graph labels offset x ns htback;
		    let apath = find_altpath graph gmatch htback xi x in
		    H.iter (fun v _ -> decr_label labels offset v) sset; 
		    H.iter (fun v _ -> incr_label labels offset v) tset;
		    symm_diff gmatch apath
		  end
		else 
		  (* delta = delta2, there exist x', y' s.t. pi(x'y') = delta *)
		  let x',y' = opt_get x', opt_get y' in
		  grow_ns graph labels offset x' ns htback;
		  if not (G.mem_vertex gmatch y') then (* y' is free *)
		    let apath = find_augpath graph gmatch htback xi y' in
		    H.iter (fun v _ -> decr_label labels offset v) sset; 
		    H.iter (fun v _ -> incr_label labels offset v) tset;
		    symm_diff gmatch apath
		  else (* y' is not free *)
		    let z' = List.hd (G.succ gmatch y') in
		    H.add sset z' (); H.add tset y' (); H.remove ns y';
		    incr_label labels offset z'; decr_label labels offset y';
		    let pqlabx,pqslack = 
		      add2heapsF graph labels pqlabx pqslack tset z' in
		    grow_ns graph labels offset z' ns htback;
		    loop offset pqlabx pqslack in
	    H.add sset xi ();
	    let offset = W.zero in
	    let pqlabx,pqslack = 
              add2heapsF graph labels PQLabF.empty PQSlackF.empty tset xi in
	    grow_ns graph labels offset xi ns htback;
	    loop offset pqlabx pqslack) xv;
	gmatch,(total_weight gmatch)
    end

(*
    let maximum_matching_fun = 
      let module M = MM_FunFheap (Heap.Functional) in M.maximum_matching
*)
    let maximum_matching_leftist = 
      let module M = MM_FunFheap (Fheap.LeftistHeap) in M.maximum_matching

    let maximum_matching_binomial = 
      let module M = MM_FunFheap (Fheap.BinomialHeap) in M.maximum_matching

    let maximum_matching_splay = 
      let module M = MM_FunFheap (Fheap.SplayHeap) in M.maximum_matching

    let maximum_matching_pairing = 
      let module M = MM_FunFheap (Fheap.PairingHeap) in M.maximum_matching

    let maximum_matching_lbh = 
      let module M = MM_FunFheap (Fheap.LazyBinomialHeap) in M.maximum_matching

    let maximum_matching_lph = 
      let module M = MM_FunFheap (Fheap.LazyPairingHeap) in M.maximum_matching

    let maximum_matching_scbh = 
      let module M = MM_FunFheap (Fheap.ScheduledBinomialHeap) in M.maximum_matching

    let maximum_matching_sbh = 
      let module M = MM_FunFheap (Fheap.SkewBinomialHeap) in M.maximum_matching

    let maximum_matching = maximum_matching_scbh 
  end

(** Persistent implementation. *)
  module P (G : Sig.P) (W : WEIGHT with type label = G.E.label) = struct

    include WBM_Common (G) (W)

(* symmetric difference for persistent graph. *)
    let symm_diff gmatch path =
      List.fold_left (fun gmatch e -> 
	if G.mem_edge_e gmatch e then
	  G.remove_edge_e gmatch e
	else
	  G.add_edge_e gmatch e) gmatch path

(* a naive implementation: O(V^2 * E) worst-case. *)
    let maximum_matching_naive graph xv yv =
      let xv,yv = if List.length xv <= List.length yv  then xv,yv else yv,xv in
      let labels = init_labels graph xv yv in
      let gmatch = List.fold_left (fun gmatch xi ->
	if G.mem_vertex gmatch xi then gmatch
	else
	  let htback = H.create 17 in
	  let sset = H.create 1 and tset = H.create 1 and ns = H.create 1 in
	  let rec loop () =
	    if H.length ns > 0 then
	      let y = htv_choose ns in
	      if not (G.mem_vertex gmatch y) then
		let apath = find_augpath graph gmatch htback xi y in
		symm_diff gmatch apath
	      else (* y is not free. *)
		let z = List.hd (G.succ gmatch y) in
		H.add sset z ();
		H.add tset y ();
		H.remove ns y;
		grow_ns_naive graph labels z ns htback;
		loop ()
	    else (* ns = tset *)
	      let delta1,x = min_delta1 labels sset in
	      let delta2,x',y' = min_delta2 graph labels sset tset in 
	      let delta = 
		if W.compare delta1 delta2 <= 0 then delta1 else delta2 in
	      H.iter (fun v _ -> decr_label labels delta v) sset;
	      H.iter (fun v _ -> incr_label labels delta v) tset;
	      if W.equal delta delta1 then
		if G.V.equal xi x then gmatch
		else begin
		  grow_ns_naive graph labels x ns htback;
		  let apath = find_altpath graph gmatch htback xi x in
		  symm_diff gmatch apath
		end
	      else (* delta = delta2 *)
		let x',y' = opt_get x', opt_get y' in
		grow_ns_naive graph labels x' ns htback;
		if not (G.mem_vertex gmatch y') then 
		  let apath = find_augpath graph gmatch htback xi y' in
		  symm_diff gmatch apath
		else 
		  let z' = List.hd (G.succ gmatch y') in
		  H.add sset z' ();
		  H.add tset y' ();
		  H.remove ns y';
		  grow_ns_naive graph labels z' ns htback;
		  loop ()
	  in
	  H.add sset xi ();
	  grow_ns_naive graph labels xi ns htback;
	  loop ()) G.empty xv in
      gmatch,(total_weight gmatch)

(* implementation using imperative heaps *)
    let maximum_matching_ih graph xv yv =
      let xv,yv = if List.length xv <= List.length yv  then xv,yv else yv,xv in
      let labels = init_labels graph xv yv in
      let gmatch = List.fold_left (fun gmatch xi ->
	if G.mem_vertex gmatch xi then gmatch
	else
	  let htback = H.create 17 in
	  let sset = H.create 1 and tset = H.create 1 and ns = H.create 1 in
	  let pqslack = PQSlack.create 55 and pqlabx = PQLab.create 55 in
	  let rec loop offset =
	    if H.length ns > 0 then
	      let y = htv_choose ns in
	      if not (G.mem_vertex gmatch y) then (* y is free *)
		let apath = find_augpath graph gmatch htback xi y in
		H.iter (fun v _ -> decr_label labels offset v) sset; 
		H.iter (fun v _ -> incr_label labels offset v) tset;
		symm_diff gmatch apath
	      else (* y is not free. *)
		let z = List.hd (G.succ gmatch y) in
		H.add sset z (); H.add tset y (); H.remove ns y;
		incr_label labels offset z; decr_label labels offset y;
		add2heaps graph labels pqlabx pqslack tset z;
		grow_ns graph labels offset z ns htback;
		loop offset 
	    else (* ns = tset *)
	      let delta1,x = PQLab.maximum pqlabx in
	      let delta2,x',y' = 
		let rec skipping () =
		  let d,x,y = PQSlack.maximum pqslack in
		  if H.mem sset x && H.mem tset y then
		    (PQSlack.remove pqslack; skipping ())
		  else
		    d,Some x, Some y
		in
		try skipping () with Fheap.EmptyHeap -> W.infinity,None,None
	      in
	      let delta = 
		if W.compare delta1 delta2 <= 0 then delta1 else delta2 in
	      let offset = delta in
	      if W.equal delta delta1 then 
		if G.V.equal xi x then gmatch
		else begin
		  grow_ns graph labels offset x ns htback;
		  let apath = find_altpath graph gmatch htback xi x in
		  H.iter (fun v _ -> decr_label labels offset v) sset; 
		  H.iter (fun v _ -> incr_label labels offset v) tset;
		  symm_diff gmatch apath
		end
	      else 
		(* delta = delta2, there exist x', y' s.t. pi(x'y') = delta *)
		let x',y' = opt_get x', opt_get y' in
		grow_ns graph labels offset x' ns htback;
		if not (G.mem_vertex gmatch y') then (* y' is free *)
		  let apath = find_augpath graph gmatch htback xi y' in
		  H.iter (fun v _ -> decr_label labels offset v) sset; 
		  H.iter (fun v _ -> incr_label labels offset v) tset;
		  symm_diff gmatch apath
		else (* y' is not free *)
		  let z' = List.hd (G.succ gmatch y') in
		  H.add sset z' (); H.add tset y' (); H.remove ns y';
		  incr_label labels offset z'; decr_label labels offset y';
		  add2heaps graph labels pqlabx pqslack tset z';
		  grow_ns graph labels offset z' ns htback;
		  loop offset in
	  H.add sset xi ();
	  let offset = W.zero in
	  add2heaps graph labels pqlabx pqslack tset xi;
	  grow_ns graph labels offset xi ns htback;
	  loop offset) G.empty xv in
      gmatch,(total_weight gmatch)

(* functorized version using functional heaps *)
    module MM_FunFheap (FunFheap: functor (X: Fheap.Ordered) -> Fheap.FunctionalSig with type elt = X.t) = struct 

      include FunFheap_Common(FunFheap)

      let maximum_matching graph xv yv =
	let xv,yv = 
	  if List.length xv <= List.length yv  then xv,yv else yv,xv in
	let labels = init_labels graph xv yv in
	let gmatch = List.fold_left (fun gmatch xi ->
	  if G.mem_vertex gmatch xi then gmatch
	  else
	    let htback = H.create 17 in
	    let sset = H.create 1 and tset = H.create 1 and ns = H.create 1 in
	    let rec loop offset pqlabx pqslack =
	      if H.length ns > 0 then
		let y = htv_choose ns in
		if not (G.mem_vertex gmatch y) then (* y is free *)
		  let apath = find_augpath graph gmatch htback xi y in
		  H.iter (fun v _ -> decr_label labels offset v) sset; 
		  H.iter (fun v _ -> incr_label labels offset v) tset;
		  symm_diff gmatch apath
		else (* y is not free. *)
		  let z = List.hd (G.succ gmatch y) in
		  H.add sset z (); H.add tset y (); H.remove ns y;
		  incr_label labels offset z; decr_label labels offset y;
		  let pqlabx,pqslack = 
		    add2heapsF graph labels pqlabx pqslack tset z in
		  grow_ns graph labels offset z ns htback;
		  loop offset pqlabx pqslack 
	      else (* ns = tset *)
		let delta1,x = PQLabF.maximum pqlabx in
		let delta2,x',y',pqslack = 
		  let rec skipping pqslack =
		    let d,x,y = PQSlackF.maximum pqslack in
		    if H.mem sset x && H.mem tset y then
		      skipping (PQSlackF.remove pqslack)
		    else
		      d,Some x, Some y,pqslack
		  in
		  try 
		    skipping pqslack 
		  with Fheap.EmptyHeap -> W.infinity,None,None,PQSlackF.empty
		in
		let delta = 
		  if W.compare delta1 delta2 <= 0 then delta1 else delta2 in
		let offset = delta in
		if W.equal delta delta1 then 
		  if G.V.equal xi x then gmatch
		  else begin
		    grow_ns graph labels offset x ns htback;
		    let apath = find_altpath graph gmatch htback xi x in
		    H.iter (fun v _ -> decr_label labels offset v) sset; 
		    H.iter (fun v _ -> incr_label labels offset v) tset;
		    symm_diff gmatch apath
		  end
		else 
		  (* delta = delta2, there exist x', y' s.t. pi(x'y') = delta *)
		  let x',y' = opt_get x', opt_get y' in
		  grow_ns graph labels offset x' ns htback;
		  if not (G.mem_vertex gmatch y') then (* y' is free *)
		    let apath = find_augpath graph gmatch htback xi y' in
		    H.iter (fun v _ -> decr_label labels offset v) sset; 
		    H.iter (fun v _ -> incr_label labels offset v) tset;
		    symm_diff gmatch apath
		  else (* y' is not free *)
		    let z' = List.hd (G.succ gmatch y') in
		    H.add sset z' (); H.add tset y' (); H.remove ns y';
		    incr_label labels offset z'; decr_label labels offset y';
		    let pqlabx,pqslack =
		      add2heapsF graph labels pqlabx pqslack tset z' in
		    grow_ns graph labels offset z' ns htback;
		    loop offset pqlabx pqslack in
	    H.add sset xi ();
	    let offset = W.zero in
	    let pqlabx,pqslack = 
	      add2heapsF graph labels PQLabF.empty PQSlackF.empty tset xi in
	    grow_ns graph labels offset xi ns htback;
	    loop offset pqlabx pqslack) G.empty xv in
	gmatch,(total_weight gmatch)
    end
(*
    let maximum_matching_fun = 
      let module M = MM_FunFheap (Heap.Functional) in M.maximum_matching
*)
    let maximum_matching_leftist = 
      let module M = MM_FunFheap (Fheap.LeftistHeap) in M.maximum_matching

    let maximum_matching_binomial = 
      let module M = MM_FunFheap (Fheap.BinomialHeap) in M.maximum_matching

    let maximum_matching_splay = 
      let module M = MM_FunFheap (Fheap.SplayHeap) in M.maximum_matching

    let maximum_matching_pairing = 
      let module M = MM_FunFheap (Fheap.PairingHeap) in M.maximum_matching

    let maximum_matching_lbh = 
      let module M = MM_FunFheap (Fheap.LazyBinomialHeap) in M.maximum_matching

    let maximum_matching_lph = 
      let module M = MM_FunFheap (Fheap.LazyPairingHeap) in M.maximum_matching

    let maximum_matching_scbh = 
      let module M = MM_FunFheap (Fheap.ScheduledBinomialHeap) in M.maximum_matching

    let maximum_matching_sbh = 
      let module M = MM_FunFheap (Fheap.SkewBinomialHeap) in M.maximum_matching

    let maximum_matching = maximum_matching_scbh 
  end
end
