(*s Functional implementation *)

(* 
   A wrapper functor to keep the [maximum] operation O(1). 
   Useful, e.g., for SkewBinomialHeap whose [maximum] takes O(log(n)) time.
   From Okasaki's Exercise 3.7.  
*)

module type Ordered = sig
  type t
  val compare : t -> t -> int
end

exception EmptyHeap

module type FunctionalSig =
  sig
    type elt
    type t
    val empty : t
    val is_empty : t -> bool
    val add : elt -> t -> t
    val maximum : t -> elt
    val remove : t -> t
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  end

module ExplicitMin (X: Ordered) (H: FunctionalSig with type elt = X.t) = struct
  type elt = X.t
  type t = E | NE of elt * H.t

  let empty = E
  let is_empty h = h = E

  let add x = function
    | E -> NE (x, H.add x H.empty)
    | NE (e,h) -> 
	if X.compare x e >= 0 then NE(x, H.add x h)
	else NE(e, H.add x h)

  let maximum = function
    | E -> raise EmptyHeap
    | NE(e,h) -> e

  let remove = function
    | E -> raise EmptyHeap
    | NE(e,h) -> 
	let nh = H.remove h in
	if nh = H.empty then E
	else NE(H.maximum nh, nh)

  let iter f = function
    | E -> ()
    | NE(_,h) -> H.iter f h

  let fold f h x0 = 
    match h with
    | E -> x0
    | NE(_,h) -> H.fold f h x0
end 

(* 
   Pairing Heap adopted from Okasaki's Purely Functional Data Structures to
   Ocamlgraph's Heap signature. 
   Here and below, the codes are based on Markus Mottl's translation.
   @author Akira Kinjo
 *)
module PairingHeap (X: Ordered) =struct
  type elt = X.t
  type t = E | T of elt * t list

  let empty = E
  let is_empty h = h = E

  let merge hp1 hp2 = 
    match hp1,hp2 with
    | h,E -> h
    | E,h -> h
    | (T (x,hs1) as h1), (T (y,hs2) as h2) ->
	if X.compare x y >= 0 
	then T (x, (h2::hs1))
	else T (y, (h1::hs2))

  let add x h = merge (T(x,[])) h

  let rec merge_pairs = function
    | [] -> E
    | [h] -> h
    | h1::h2::hs -> merge (merge h1 h2) (merge_pairs hs)

  let maximum = function
    | E -> raise EmptyHeap
    | T(x,hs) -> x

  let remove = function
    | E -> raise EmptyHeap
    | T(x,hs) -> merge_pairs hs

  let rec iter f h =
    if is_empty h then ()
    else
      let x = maximum h in 
      f x; iter f (remove h)

  let rec fold f h x0 = 
    if is_empty h then x0
    else
      let x = maximum h in
      fold f (remove h) (f x x0)
end

(* 
   Skew Binomial Heap adopted from Okasaki's Purely Functional Data Structures 
   to Ocamlgraph's Heap signature. 
   @author Akira Kinjo
 *)
module SkewBinomialHeap (X : Ordered) = struct

  type elt = X.t
  type tree = Node of int * elt * elt list * tree list
  type t = tree list

  let empty = []
  let is_empty ts = ts = []

  let rank (Node (r, _, _, _)) = r
  let root (Node (_, x, _, _)) = x

  let link (Node (r, x1, xs1, c1) as t1) (Node (_, x2, xs2, c2) as t2) =
    if X.compare x1 x2 >= 0 then Node (r + 1, x1, xs1, t2 :: c1)
    else Node (r + 1, x2, xs2, t1 :: c2)

  let skew_link x t1 t2 =
    let Node (r, y, ys, c) = link t1 t2 in
    if X.compare x y >= 0 then Node (r, x, y :: ys, c)
    else Node (r, y, x :: ys, c)

  let rec ins_tree t = function
    | [] -> [t]
    | t' :: ts ->
        if rank t < rank t' then t :: t' :: ts
        else ins_tree (link t t') ts

  let rec merge_trees ts1 ts2 = 
    match ts1, ts2 with
    | _, [] -> ts1
    | [], _ -> ts2
    | t1 :: ts1', t2 :: ts2' ->
	if rank t1 < rank t2 then t1 :: merge_trees ts1' ts2
	else if rank t2 < rank t1 then t2 :: merge_trees ts1 ts2'
	else ins_tree (link t1 t2) (merge_trees ts1' ts2')

  let normalize = function
    | [] -> []
    | t :: ts -> ins_tree t ts

  let add x = function
    | t1 :: t2 :: rest as ts ->
        if rank t1 = rank t2 then skew_link x t1 t2 :: rest
        else Node (0, x, [], []) :: ts
    | ts -> Node (0, x, [], []) :: ts

  let merge ts1 ts2 = merge_trees (normalize ts1) (normalize ts2)

  let rec remove_max_tree = function
    | [] -> raise EmptyHeap
    | [t] -> t, []
    | t :: ts ->
        let t', ts' = remove_max_tree ts in
        if X.compare (root t) (root t') >= 0 then t, ts 
	else t', t :: ts'

  let maximum ts = root (fst (remove_max_tree ts))

  let remove ts =
    let Node (_, x, xs, ts1), ts2 = remove_max_tree ts in
    let rec insert_all ts = function
      | [] -> ts
      | x :: xs' -> insert_all (add x ts) xs' in
    insert_all (merge (List.rev ts1) ts2) xs

  let rec iter f h =
    if is_empty h then ()
    else
      let x = maximum h in 
      f x; iter f (remove h)

  let rec fold f h x0 = 
    if is_empty h then x0
    else
      let x = maximum h in
      fold f (remove h) (f x x0)
end

(* 
   Leftist Heap adopted from Okasaki's Purely Functional Data Structures 
   to Ocamlgraph's Heap signature. 
   @author Akira Kinjo
 *)
module LeftistHeap (X : Ordered) = struct
  type elt = X.t
  type t = E | T of int * elt * t * t

  let rank = function E -> 0 | T (r,_,_,_) -> r

  let makeT x a b =
    if rank a >= rank b then T (rank b + 1, x, a, b)
    else T (rank a + 1, x, b, a)

  let empty = E
  let is_empty h = h = E

  let rec merge h1 h2 = 
    match h1, h2 with
    | _, E -> h1
    | E, _ -> h2
    | T (_, x, a1, b1), T (_, y, a2, b2) ->
	if X.compare x y >= 0 then makeT x a1 (merge b1 h2)
	else makeT y a2 (merge h1 b2)

  let add x h = merge (T (1, x, E, E)) h
  let maximum = function E -> raise EmptyHeap | T (_, x, _, _) -> x
  let remove = function E -> raise EmptyHeap | T (_, x, a, b) -> merge a b

  let rec iter f h =
    if is_empty h then ()
    else
      let x = maximum h in 
      f x; iter f (remove h)

  let rec fold f h x0 = 
    if is_empty h then x0
    else
      let x = maximum h in
      fold f (remove h) (f x x0)
end

module BinomialHeap (X : Ordered) = struct
  type elt = X.t
  type tree = Node of int * elt * tree list
  type t = tree list
  
  let empty = []
  let is_empty ts = ts = []

  let rank (Node (r, _, _)) = r
  let root (Node (_, x, _)) = x

  let link (Node (r, x1, c1) as t1) (Node (_, x2, c2) as t2) =
    if X.compare x1 x2 >= 0 then Node (r + 1, x1, t2 :: c1)
    else Node (r + 1, x2, t1 :: c2)

  let rec ins_tree t = function
    | [] -> [t]
    | t' :: ts' as ts ->
	if rank t < rank t' then t :: ts
	else ins_tree (link t t') ts'

  let add x ts = ins_tree (Node (0, x, [])) ts

  let rec merge ts1 ts2 = 
    match ts1, ts2 with
    | _, [] -> ts1
    | [], _ -> ts2
    | t1 :: ts1', t2 :: ts2' ->
	if rank t1 < rank t2 then t1 :: merge ts1' ts2
	else if rank t2 < rank t1 then t2 :: merge ts1 ts2'
	else ins_tree (link t1 t2) (merge ts1' ts2')

  let rec remove_min_tree = function
    | [] -> raise EmptyHeap
    | [t] -> t, []
    | t :: ts ->
	let t', ts' = remove_min_tree ts in
	if X.compare (root t) (root t') >= 0 then (t, ts)
	else (t', t :: ts')

  let maximum ts = root (fst (remove_min_tree ts))

  let remove ts =
    let Node (_, x, ts1), ts2 = remove_min_tree ts in
    merge (List.rev ts1) ts2

  let rec iter f h =
    if is_empty h then ()
    else
      let x = maximum h in 
      f x; iter f (remove h)

  let rec fold f h x0 = 
    if is_empty h then x0
    else
      let x = maximum h in
      fold f (remove h) (f x x0)
end

module SplayHeap (X : Ordered) = struct
  type elt = X.t
  type t = E | T of t * elt * t

  let empty = E
  let is_empty h = h = E

  let rec partition pivot = function
    | E -> E, E
    | T (a, x, b) as t ->
        if X.compare x pivot >= 0 then
          match b with
          | E -> t, E
          | T (b1, y, b2) ->
              if X.compare y pivot >= 0 then
                let small, big = partition pivot b2 in
                T (T (a, x, b1), y, small), big
              else
                let small, big = partition pivot b1 in
                T (a, x, small), T (big, y, b2)
        else
          match a with
          | E -> E, t
          | T (a1, y, a2) ->
              if X.compare y pivot >= 0 then
                let small, big = partition pivot a2 in
                T (a1, y, small), T (big, x, b)
              else
                let small, big = partition pivot a1 in
                small, T (big, y, T (a2, x, b))

  let add x t = let a, b = partition x t in T (a, x, b)

  let rec merge s t = 
    match s, t with
    | E, _ -> t
    | T (a, x, b), _ ->
        let ta, tb = partition x t in
        T (merge ta a, x, merge tb b)

  let rec maximum = function
    | E -> raise EmptyHeap
    | T (E, x, _) -> x
    | T (a, x, _) -> maximum a

  let rec remove = function
    | E -> raise EmptyHeap
    | T (E, _, b) -> b
    | T (T (E, _, b), y, c) -> T (b, y, c)
    | T (T (a, x, b), y, c) -> T (remove a, x, T (b, y, c))

  let rec iter f h =
    if is_empty h then ()
    else
      let x = maximum h in 
      f x; iter f (remove h)

  let rec fold f h x0 = 
    if is_empty h then x0
    else
      let x = maximum h in
      fold f (remove h) (f x x0)
end

let (!$) = Lazy.force

module LazyBinomialHeap (X : Ordered) = struct
  type elt = X.t
  type tree = Node of int * elt * tree list
  type t = tree list Lazy.t

  let empty = lazy []
  let is_empty ts = !$ts = []

  let rank (Node (r, _, _)) = r
  let root (Node (_, x, _)) = x

  let link (Node (r, x1, c1) as t1) (Node (_, x2, c2) as t2) =
    if X.compare x1 x2 >= 0 then Node (r + 1, x1, t2 :: c1)
    else Node (r + 1, x2, t1 :: c2)

  let rec ins_tree t ts = match t, ts with
    | _, [] -> [t]
    | t, t' :: ts' ->
        if rank t < rank t' then t :: ts
        else ins_tree (link t t') ts'

  let rec mrg ts1 ts2 = match ts1, ts2 with
    | _, [] -> ts1
    | [], _ -> ts2
    | t1 :: ts1', t2 :: ts2' ->
        if rank t1 < rank t2 then t1 :: mrg ts1' ts2
        else if rank t2 < rank t1 then t2 :: mrg ts1 ts2'
        else ins_tree (link t1 t2) (mrg ts1' ts2')

  (* fun lazy *)
  let add x ts = lazy (ins_tree (Node (0, x, [])) !$ts)

  (* fun lazy *)
  let merge ts1 ts2 = lazy (mrg !$ts1 !$ts2)

  let rec remove_min_tree = function
    | [] -> raise EmptyHeap
    | [t] -> t, []
    | t :: ts ->
        let t', ts' = remove_min_tree ts in
        if X.compare (root t) (root t') >= 0 then t, ts
        else t', t :: ts'

  let maximum ts = let t, _ = remove_min_tree !$ts in root t

  (* fun lazy *)
  let remove ts =
    let Node (_, _, ts1), ts2 = remove_min_tree !$ts in
    lazy (mrg (List.rev ts1) ts2)

  let rec iter f h =
    if is_empty h then ()
    else
      let x = maximum h in 
      f x; iter f (remove h)

  let rec fold f h x0 = 
    if is_empty h then x0
    else
      let x = maximum h in
      fold f (remove h) (f x x0)
end

module LazyPairingHeap (X : Ordered) = struct
  type elt = X.t

  type t = E | T of elt * t * t Lazy.t

  let empty = E
  let is_empty h = h = E

  let rec merge a b = 
    match a, b with
    | _, E -> a
    | E, _ -> b
    | T (x, _, _), T (y, _, _) -> if X.compare x y >= 0 then link a b else link b a
  and link h a = 
    match h with
    | T (x, E, m) -> T (x, a, m)
    | T (x, b, m) -> T (x, E, lazy (merge (merge a b) !$m))
    | _ -> failwith "link"

  let add x a = merge (T (x, E, lazy E)) a

  let maximum = function E -> raise EmptyHeap | T (x, _, _) -> x
  let remove = function E -> raise EmptyHeap | T (_, a, b) -> merge a !$b

  let rec iter f h =
    if is_empty h then ()
    else
      let x = maximum h in 
      f x; iter f (remove h)

  let rec fold f h x0 = 
    if is_empty h then x0
    else
      let x = maximum h in
      fold f (remove h) (f x x0)
end

module type STREAM = sig
  type 'a stream = Nil | Cons of 'a * 'a stream Lazy.t

  val (++) : 'a stream -> 'a stream -> 'a stream  (* stream append *)
  val take : int -> 'a stream -> 'a stream
  val drop : int -> 'a stream -> 'a stream
  val reverse : 'a stream -> 'a stream
  val list_to_stream : 'a list -> 'a stream
end

module StreamF : STREAM = struct
  type 'a stream = Nil | Cons of 'a * 'a stream Lazy.t

  (* function lazy *)
  let rec (++) s1 s2 = match s1 with
    | Nil -> s2
    | Cons (hd, tl) -> Cons (hd, lazy (!$tl ++ s2))

  (* function lazy *)
  let rec take n s = match n, s with
    | 0, _ -> Nil
    | _, Nil -> Nil
    | _, Cons (hd, tl) -> Cons (hd, lazy (take (n - 1) !$tl))

  (* function lazy *)
  let drop n s =
    let rec drop' n s = match n, s with
      | 0, _ -> s
      | _, Nil -> Nil
      | _, Cons (_, tl) -> drop' (n - 1) !$tl in
    drop' n s

  (* function lazy *)
  let reverse s =
    let rec reverse' acc = function
      | Nil -> acc
      | Cons (hd, tl) -> reverse' (Cons (hd, lazy acc)) !$tl in
    reverse' Nil s

  let rec list_to_stream = function
    | [] -> Nil
    | x :: xs -> Cons (x, lazy (list_to_stream xs))
end


module ScheduledBinomialHeap (X : Ordered) = struct
  open StreamF
  type elt = X.t
  type tree = Node of elt * tree list
  type digit = Zero | One of tree
  type schedule = digit stream list
  type t = digit stream * schedule

  let empty = Nil, []
  let is_empty (ds, _) = ds = Nil

  let link (Node (x1, c1) as t1) (Node (x2, c2) as t2) =
    if X.compare x1 x2 >= 0 then Node (x1, t2 :: c1)
    else Node (x2, t1 :: c2)

  let rec ins_tree t = function
    | Nil -> Cons (One t, lazy Nil)
    | Cons (Zero, ds) -> Cons (One t, ds)
    | Cons (One t', ds) -> Cons (Zero, lazy (ins_tree (link t t') !$ds))

  let rec mrg a b = match a, b with
    | ds1, Nil -> ds1
    | Nil, ds2 -> ds2
    | Cons (Zero, ds1), Cons (d, ds2) -> Cons (d, lazy (mrg !$ds1 !$ds2))
    | Cons (d, ds1), Cons (Zero, ds2) -> Cons (d, lazy (mrg !$ds1 !$ds2))
    | Cons (One t1, ds1), Cons (One t2, ds2) ->
        Cons (Zero, lazy (ins_tree (link t1 t2) (mrg !$ds1 !$ds2)))

  let rec normalize ds = match ds with
    | Nil -> ds
    | Cons (_, ds') -> ignore(normalize (!$ds')); ds

  let exec = function
    | [] -> []
    | Cons (Zero, job) :: sched -> !$job :: sched
    | _ :: sched -> sched

  let add x (ds, sched) =
    let ds' = ins_tree (Node (x, [])) ds in
    ds', exec (exec (ds' :: sched))

  let merge (ds1, _) (ds2, _) = normalize (mrg ds1 ds2), []

  let rec remove_min_tree = function
    | Nil -> raise EmptyHeap
    | Cons (hd, tl) ->
        match hd, !$tl with
        | One t, Nil -> t, Nil
        | Zero, ds ->
            let t', ds' = remove_min_tree ds in t', Cons (Zero, lazy ds')
        | One (Node (x, _) as t), ds ->
            let Node (x', _) as t', ds' = remove_min_tree ds in
            if X.compare x x' >= 0 then t, Cons (Zero, tl)
            else t', Cons (One t, lazy ds')

  let maximum (ds, _) = let Node (x, _), _ = remove_min_tree ds in x

  let remove (ds, _) =
    let Node (_, c), ds' = remove_min_tree ds in
    let ds'' =
      mrg (list_to_stream (List.map (fun e -> One e) (List.rev c))) ds' in
    normalize ds'', []

  let rec iter f h =
    if is_empty h then ()
    else
      let x = maximum h in 
      f x; iter f (remove h)

  let rec fold f h x0 = 
    if is_empty h then x0
    else
      let x = maximum h in
      fold f (remove h) (f x x0)
end

