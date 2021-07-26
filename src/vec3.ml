(** generic operations for 3D cartesian coordinates*)

type t = float * float * float
type qrot = float array

(** pi *)
let pi = acos (-1.0)

(** origin *)
let zero = (0.0,0.0,0.0);;

(** radian -> degree *)
let rad2deg ang = ang *. 180.0 /. pi

(** dot product *)
let dot (x1,y1,z1) (x2,y2,z2) = x1 *. x2 +. y1 *. y2 +. z1 *. z2

(** addition *)
let add (x1,y1,z1) (x2,y2,z2) = x1 +. x2, y1 +. y2, z1 +. z2

(** subtraction *)
let subtract (x1,y1,z1) (x2,y2,z2) = x1 -. x2, y1 -. y2, z1 -. z2

(** negation *)
let negate (x1,y1,z1) = ~-.x1 , ~-.y1, ~-. z1

(** scalar multiplication *)
let scale a (x1,y1,z1) = a *. x1 , a *. y1 , a *. z1

(** cross product *)
let cross (x1,y1,z1) (x2,y2,z2) = 
  let x = y1 *. z2 -. y2 *. z1 
  and y = z1 *. x2 -. z2 *. x1 
  and z = x1 *. y2 -. x2 *. y1 
  in x,y,z
    
(** magnitude of vector *)
let magnitude (x,y,z) = sqrt (dot (x,y,z) (x,y,z)) 

(** euclid distance *)
let distance (x1,y1,z1) (x2,y2,z2) = magnitude (x1 -. x2, y1 -. y2, z1 -. z2)

(** square of euclid distance *)
let distance2 (x1,y1,z1) (x2,y2,z2) = 
  let dx = x1 -. x2 and dy = y1 -. y2 and dz = z1 -. z2 in
  dx *. dx +. dy *. dy +. dz *. dz

let fabs = abs_float
let within_distance dcut ((x1,y1,z1) as co1) ((x2,y2,z2) as co2) = 
  fabs (x1 -. x2) < dcut && fabs (y1 -. y2) < dcut && fabs (z1 -. z2) < dcut
    && distance2 co1 co2 < dcut *. dcut

(** normalize a vector *)
let normalize (x,y,z) = 
  let mag = magnitude (x,y,z) in 
  if mag = 0.0 then zero 
  else (x /. mag),(y /. mag),(z /. mag)

(** angle between two vectors *)
let angle a b = 
  let a = normalize a and b = normalize b in
  acos (dot a b)

(** generic dihedral angle: 1--2--3--4 *)
let torsion (x1,y1,z1) (x2,y2,z2) (x3,y3,z3) (x4,y4,z4) = 
  let v21 = (x1 -. x2),(y1 -. y2),(z1 -. z2) in
  let v34 = (x4 -. x3),(y4 -. y3),(z4 -. z3) in
  let v23 = (x3 -. x2),(y3 -. y2),(z3 -. z2) in
  let c1 = cross v23 v21 and c2 = cross v23 v34 in
  let theta = angle c1 c2 and c3 = cross c1 c2 in
  let dir = dot c3 v23 in
  let theta = if dir < 0.0 then ~-. theta else theta in
  rad2deg theta 

(** generating equal-area points on the unit sphere. This is NOT suitable for ASA *)
let gen_point_sphere_ea ntheta nphi =
  let dz =  1.0 /. float ntheta in
  let dphi = 2.0 *. pi /. float nphi in
  let cod z phi = 
    let r = sqrt (1.0 -. z *. z) in
    let x = r *. cos phi and y = r *. sin phi in
    x,y,z in
  (* north pole *)
  let l = ref [] in
  for i = 0 to nphi - 1 do
    let phi = dphi *. (float i +. 0.5) in
    l := (cod (1.0 -. dz) phi) :: !l;
  done;
  for j = 1 to ntheta - 2 do
    let z = 1.0 -. dz *. float (2 * j + 1) in
    let phase = if j mod 2 = 0 then 0.5 else 0.0 in
    for i = 0 to nphi - 1 do
      let phi = dphi *. (float i +. phase) in
      l := (cod z phi) :: !l
    done;
  done;
  (* south pole *)
  let phase = if ntheta mod 2 = 0 then 0.0 else 0.5 in
  let z = dz -. 1.0 in
  for i = 0 to nphi - 1 do
    let phi = dphi *. (float i +. phase) in
    l := (cod z phi) :: !l;
  done;

  !l

(** generating random uniform points on the unit sphere *)
let gen_point_sphere_random ?(seed=135173) npoints = 
  let rng = Gsl.Rng.make Gsl.Rng.MT19937 in
  Gsl.Rng.set rng (Nativeint.of_int seed);
  let rec gen () =
    let t = 2.0 *. (Gsl.Rng.uniform_pos rng) -. 1.0 in
    let u = 2.0 *. (Gsl.Rng.uniform_pos rng) -. 1.0 in
    let s = t *. t +. u *. u in
    if s >= 1.0 then gen ()
    else
      let m = sqrt (1.0 -. s) in
      let x = 2.0 *. t *. m in
      let y = 2.0 *. u *. m in
      let z = 1.0 -. 2.0 *. s in
      x,y,z
  in
  let rec loop n l = 
    if n = 0 then l
    else loop (n - 1) ((n,gen ()) :: l)
  in loop npoints []

let tetra_vertices,tetra_faces = 
  let s = sqrt 3.0 in
  (List.map normalize [(s,s,s); (-.s,-.s,s); (-.s,s,-.s);(s,-.s,-.s)]),
  [(1,3,2); (1,2,4); (3,4,2); (4,3,1)]

let octa_vertices,octa_faces =
  [(0.0,0.0,1.0); (0.0,0.0,-1.0); 
   (1.0,0.0,0.0); (0.0,1.0,0.0); (-1.0,0.0,0.0); (0.0,-1.0,0.0)],
  [(1,3,4);(1,4,5);(1,5,6);(1,6,3);(2,4,3);(2,5,4);(2,6,5);(2,3,6)]

let icosa_vertices,icosa_faces =
  let t = (1.0 +. sqrt 5.0) /. 2.0 in
  let tau = t /. sqrt (1.0 +. t *. t) in
  let one = 1.0 /. sqrt (1.0 +. t *. t) in
  let vs = [(tau,one,0.0); 
	    (-.tau,one,0.0);
	    (-.tau,-.one,0.0);
	    (tau,-.one,0.0);
	    (one,0.0,tau);
	    (one,0.0,-.tau);
	    (-.one,0.0,-.tau);
	    (-.one,0.0,tau);
	    (0.0,tau,one);
	    (0.0,-.tau,one);
	    (0.0,-.tau,-.one);
	    (0.0,tau,-.one);] in
  let fs = [(5,9,8); (5,8,10); (6,7,12); (6,11,7); (1,5,4);
	    (1,4,6); (3,8,2); (3,2,7); (9,1,12); (9,12,2);
	    (10,11,4);(10,3,11); (9,5,1); (12,1,6);(5,10,4);
	    (6,4,11); (8,9,2);(7,2,12); (8,3,10);  (7,11,3);] in
  (List.map normalize vs),fs

(** CAUTION: triangles are NOT equilaberal, but are isosceles. *)
let pentakis_dodeca_vertices,pentakis_dodeca_faces =
  let module H = Hashtbl in
  let unique = let ind = ref 0 in fun () -> incr ind; !ind in 
  let vh = H.create 60 in
  List.iter (fun a -> H.add vh (unique ()) a) icosa_vertices;
  let eh = H.create 60 in
  let add_eh (k,i,j) =
    if H.mem eh (i,j) then
      H.replace eh (i,j) (k :: H.find eh (i,j))
    else if H.mem eh (j,i) then
      H.replace eh (j,i) (k :: H.find eh (j,i))
    else 
      H.add eh (i,j) [k]
  in
  let split3 (i0,i1,i2) =
    let v0 = H.find vh i0 in
    let v1 = H.find vh i1 in
    let v2 = H.find vh i2 in
    let c = normalize (add v0 (add v1 v2)) in
    let ck = unique () in
    H.add vh ck c;
    add_eh (ck,i0,i1); add_eh (ck,i1,i2); add_eh (ck,i2,i0)
  in
  List.iter split3 icosa_faces;
  let nts = H.fold (fun (i,j) l nts ->
    match l with
    | [] -> failwith "pentakis_dodeca(0)"
    | [_] -> failwith "pentakis_dodeca(1)"
    | [c1; c2] -> 
	(c1,c2,i)::(c2,c1,j):: nts
    | _ -> failwith "pentakis_dodeca(too many)") eh [] in
  let vs = H.fold (fun k v l -> (k,v) :: l) vh [] in
  let vs = List.sort (fun (k,_) (l,_) -> k - l) vs in
  let vs = List.map snd vs in
  (List.map normalize vs),nts

(** generating points on a unit sphere. *)
let gen_point_sphere_tri ivertices ifaces nlevel = 
  let module Q = Queue in
  let module H = Hashtbl in
  let unique = let ind = ref 0 in fun () -> incr ind; !ind in 
  let vertices = H.create 2000 in
  List.iter (fun a -> H.add vertices (unique()) a) ivertices;
  let triangles = Q.create () in
  List.iter (fun t -> Q.push (0,t) triangles) ifaces;
  let add_vertex a = let key = unique () in H.add vertices key a; key in
  let mid = H.create 2000 in (* tracing middle points of vertices *)
  let make_mid (i0,v0) (i1,v1) =
    let tup = if i0 < i1 then i0,i1 else i1,i0 in
    try H.find mid tup
    with Not_found -> 
      let nv01 = normalize (add v0 v1) in
      let nk01 = add_vertex nv01 in
      H.add mid tup (nv01,nk01);
      nv01,nk01
  in
  let push_new_triangle (n,(i0,i1,i2)) = 
    let v0 = H.find vertices i0 in
    let v1 = H.find vertices i1 in
    let v2 = H.find vertices i2 in
    let nv01,nk01 = make_mid (i0,v0) (i1,v1) in
    let nv12,nk12 = make_mid (i1,v1) (i2,v2) in
    let nv20,nk20 = make_mid (i2,v2) (i0,v0) in
    let ntri1 = (i0, nk01, nk20)
    and ntri2 = (i1, nk12, nk01)
    and ntri3 = (i2, nk20, nk12)
    and ntri4 = (nk01, nk12, nk20) in
    let n' = n + 1 in
    Q.push (n', ntri1) triangles;
    Q.push (n', ntri2) triangles;
    Q.push (n', ntri3) triangles;
    Q.push (n', ntri4) triangles;
  in
  let rec loop () = 
    let n,tri = Q.peek triangles in
    if n = nlevel then ()
    else loop (push_new_triangle (Q.pop triangles))
  in
  loop ();
  let l = H.fold (fun a b c -> (a,b) :: c) vertices [] in
  let vs = List.sort (fun (a1,_) (a2,_) -> a1 - a2) l in
  let ts = Q.fold (fun tl (_,l) -> l :: tl) [] triangles in
  vs,ts 

let gen_point_sphere_tri4 =
  gen_point_sphere_tri tetra_vertices tetra_faces

let gen_point_sphere_tri8 =
  gen_point_sphere_tri octa_vertices octa_faces

let gen_point_sphere_tri20 =
  gen_point_sphere_tri icosa_vertices icosa_faces

let gen_point_sphere_tri60 =
  gen_point_sphere_tri pentakis_dodeca_vertices pentakis_dodeca_faces

(** generating points on a unit sphere. *)
let gen_point_sphere ?(npoly=8) nlevel = 
  let vs =
    if npoly = 4 then
      fst(gen_point_sphere_tri4 nlevel)
    else if npoly = 20 then
      fst(gen_point_sphere_tri20 nlevel)
    else if npoly = 60 then
      fst(gen_point_sphere_tri60 nlevel)
    else if npoly < 0 then
      gen_point_sphere_random ~seed:(~- npoly) nlevel
    else
      fst(gen_point_sphere_tri8 nlevel)
  in
  List.map snd vs

let gen_point_sphere_tri_n ?(npoly=8) nlevel = 
  if npoly = 4 then
    gen_point_sphere_tri4 nlevel
  else if npoly = 8 then
    gen_point_sphere_tri8 nlevel
  else if npoly = 20 then
    gen_point_sphere_tri20 nlevel
  else if npoly = 60 then
    gen_point_sphere_tri60 nlevel
  else
    failwith 
      (Printf.sprintf
	 "gen_point_sphere_tri_n: npoly must be 4,8,20,60, but got %d" npoly)

let invert_mat amat = 
  let b = Gsl.Linalg.invert_LU ~protect:true (`AA amat) in
  Gsl.Vectmat.to_arrays b

let area_triangle p0 p1 p2 =
  let d1 = subtract p1 p0 and d2 = subtract p2 p0 in
  0.5 *. magnitude (cross d1 d2)

let mul_mat_vec mat (x,y,z) = 
  let x' = mat.(0).(0) *. x +. mat.(0).(1) *. y +. mat.(0).(2) *. z 
  and y' = mat.(1).(0) *. x +. mat.(1).(1) *. y +. mat.(1).(2) *. z 
  and z' = mat.(2).(0) *. x +. mat.(2).(1) *. y +. mat.(2).(2) *. z 
  in (x' , y' , z')
;;


(* for RMSD calculation *)
let qconj p = [|p.(0); -.p.(1); -.p.(2); -.p.(3)|]
(** Quaternion conjugate*)

let qmult p q =
  let p0 = p.(0) and q0 = q.(0) in
  let ps = p.(1), p.(2), p.(3) and qs = q.(1), q.(2), q.(3) in
  let r0 = p0*.q0 -. (dot ps qs) in
  let x,y,z = add (add (scale p0 qs) (scale q0 ps)) (cross ps qs) in
  [|r0; x; y; z |]
;;
(** Quaternion multiplication *)

let qrotate q (x,y,z) =
  let x' = [|0.0; x; y; z|] in
  let q' = qconj q in
  let r = qmult q (qmult x' q') in
  r.(1), r.(2), r.(3)
;;
(** Rotation of 3-vector by a quaternion.*)

let make_ak2 (x1,y1,z1) (x2,y2,z2) = 
  let ak =
    [|[| 0.0; -.x2; -.y2; -.z2 |];
      [| x2;   0.0; -.z1;   y1 |];
      [| y2;   z1;   0.0; -.x1 |];
      [| z2; -.y1;   x1;   0.0 |];|]
  in
  let ak2 = Array.make_matrix 4 4 0.0 in
  for i = 0 to 3 do
    for j = 0 to 3 do
      for k = 0 to 3 do
        ak2.(i).(j) <- ak2.(i).(j) +. ak.(k).(i)*.ak.(k).(j);
      done;
    done;
  done;
  ak2

let crms_mat amol bmol =
  let n = Array.length amol in
  let fn = 1.0 /. float n in
  let cma = scale fn (Array.fold_left add (0.0,0.0,0.0) amol)
  and cmb = scale fn (Array.fold_left add (0.0,0.0,0.0) bmol) in
  let amol = Array.map (fun a -> subtract a cma) amol
  and bmol = Array.map (fun a -> subtract a cmb) bmol in
  let mmat = Array.make_matrix 4 4 0.0 in
  for i = 0 to n - 1 do
    let ak2 = make_ak2 (add amol.(i) bmol.(i)) (subtract amol.(i) bmol.(i)) in
    for k = 0 to 3 do
      for l = 0 to 3 do
        mmat.(k).(l) <- mmat.(k).(l) +. ak2.(k).(l)
      done;
    done;
  done;
  for k = 0 to 3 do
    for l = 0 to 3 do
      mmat.(k).(l) <- mmat.(k).(l) *. fn
    done;
  done;
  let eigs,vecs = Gsl.Eigen.symmv (`AA mmat) in
  Gsl.Eigen.symmv_sort (eigs,vecs) Gsl.Eigen.VAL_ASC;
  let rms = sqrt (max 0.0 (Gsl.Vector.get eigs 0)) in
  let qrot = Array.init 4 (fun i -> Gsl.Matrix.get vecs i 0) in
  rms,qrot,cma,cmb
;;
(** least-square fit of two molecules *)

let crms amol bmol =
  let eig,_,_,_ = crms_mat amol bmol in
  eig
;;

(** make quarternion for rotation *)
let get_qrot (x,y,z) ang = 
  let ang = 0.5 *. ang in
  let s = sin ang in
  [|cos ang; s *. x; s *. y; s *. z|]
;;

(** make rotation matrix from direction vector and angle *)
let get_rot ((x,y,z) as co) = 
  let ang = asin (magnitude co) in
  let x,y,z = if ang = 0.0 then zero else normalize co in
  let sa = sin (ang /. 2.0) and q0 = cos (ang /. 2.0) in
  let q = Array.make 3 0.0 in
  q.(0) <- sa *. x; q.(1) <- sa *. y; q.(2) <- sa *. z; 
  let mag = q0 *. q0 -. (q.(0) *. q.(0) +. q.(1) *. q.(1) +. q.(2) *. q.(2)) in
  let rot = Array.make_matrix 3 3 0.0 in
  rot.(0).(0) <- mag; rot.(1).(1) <- mag; rot.(2).(2) <- mag;
  for i = 0 to 2 do
    for j = 0 to 2 do
      rot.(i).(j) <- rot.(i).(j) +. 2.0 *. q.(i) *. q.(j)
    done;
  done;
  rot.(1).(0) <- rot.(1).(0) +. 2.0 *. q0 *. q.(2);
  rot.(0).(1) <- rot.(0).(1) -. 2.0 *. q0 *. q.(2);

  rot.(2).(0) <- rot.(2).(0) -. 2.0 *. q0 *. q.(1);
  rot.(0).(2) <- rot.(0).(2) +. 2.0 *. q0 *. q.(1);

  rot.(2).(1) <- rot.(2).(1) +. 2.0 *. q0 *. q.(0);
  rot.(1).(2) <- rot.(1).(2) -. 2.0 *. q0 *. q.(0);

  rot
;;

let center_of_mass mol =
  let n = Array.length mol in
  let fn = float n in
  let cm = scale (1.0/.fn) (Array.fold_left add (0.0,0.0,0.0) mol) in
  cm

(** get moment of inertia *)
let moment mol =
  let n = Array.length mol in
  let fn = float n in
  let cm = center_of_mass mol in
  let m = Array.make_matrix 3 3 0.0 in
  for i = 0 to n - 1 do
    let x,y,z = subtract mol.(i) cm in
    m.(0).(0) <- m.(0).(0) +. y *. y +. z *. z;
    m.(0).(1) <- m.(0).(1) -. x *. y;
    m.(0).(2) <- m.(0).(2) -. x *. z;
    m.(1).(0) <- m.(1).(0) -. y *. x;
    m.(1).(1) <- m.(1).(1) +. z *. z +. x *. x;
    m.(1).(2) <- m.(1).(2) -. y *. z;
    m.(2).(0) <- m.(2).(0) -. z *. x;
    m.(2).(1) <- m.(2).(1) -. z *. y;
    m.(2).(2) <- m.(2).(2) +. x *. x +. y *. y;
  done;
  for i = 0 to 2 do
    for j = 0 to 2 do
      m.(i).(j) <- m.(i).(j) /. fn
    done;
  done;
  let eigs,vecs = Gsl.Eigen.symmv (`AA m) in
  Gsl.Eigen.symmv_sort (eigs,vecs) Gsl.Eigen.VAL_ASC;
  cm,eigs,vecs

(** do principal component analysis *)
let pca mol =
  let n = Array.length mol in
  let fn = float n in
  let cm = center_of_mass mol in
  let m = Array.make_matrix 3 3 0.0 in
  for i = 0 to n - 1 do
    let x,y,z = subtract mol.(i) cm in
    m.(0).(0) <- m.(0).(0) +. x *. x;
    m.(0).(1) <- m.(0).(1) +. x *. y;
    m.(0).(2) <- m.(0).(2) +. x *. z;
    m.(1).(0) <- m.(1).(0) +. y *. x;
    m.(1).(1) <- m.(1).(1) +. y *. y;
    m.(1).(2) <- m.(1).(2) +. y *. z;
    m.(2).(0) <- m.(2).(0) +. z *. x;
    m.(2).(1) <- m.(2).(1) +. z *. y;
    m.(2).(2) <- m.(2).(2) +. z *. z;
  done;
  for i = 0 to 2 do
    for j = 0 to 2 do
      m.(i).(j) <- m.(i).(j) /. fn
    done;
  done;
  let eigs,vecs = Gsl.Eigen.symmv (`AA m) in
  Gsl.Eigen.symmv_sort (eigs,vecs) Gsl.Eigen.VAL_DESC;
  cm,eigs,vecs

