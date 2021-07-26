module Ht = Hashtbl

let pi4 = Vec3.pi *. 4.0

let angle_sphere v0 v1 v2 =
  let d1 = Vec3.normalize (Vec3.cross v0 (Vec3.subtract v1 v0)) in
  let d2 = Vec3.normalize (Vec3.cross v0 (Vec3.subtract v2 v0)) in
  acos (Vec3.dot d1 d2)
  
let weight_vertices vs ts = 
  let ht = Ht.create 100 in
  List.iter (fun (i,v) -> Ht.add ht i v) vs;
  let center (i0,i1,i2) =
    let v0 = Ht.find ht i0 in
    let v1 = Ht.find ht i1 in
    let v2 = Ht.find ht i2 in
    let c = Vec3.normalize (Vec3.add v0 (Vec3.add v1 v2)) in
    let a0 = angle_sphere v0 v1 v2 in
    let a1 = angle_sphere v1 v2 v0 in
    let a2 = angle_sphere v2 v0 v1 in
    (* the area of a triangle on the unit sphere *)
    let a = a0 +. a1 +. a2 -. Vec3.pi in
    c,a
  in
  List.map center ts

(* 
   get ASA.
   [npoly] is the number of faces in the initial polygon.
   [npoly] is one of 4, 8, 20, 60.
   [level] is the depth of subdivision of polygon faces. 
*)
let get radius_of_atom ?(rad_probe=1.4) ?(npoly=60) ?(level=2) atoms =
  let vs,ts = Vec3.gen_point_sphere_tri_n ~npoly level in
  let vertices = weight_vertices vs ts in
  let atoms = List.map (fun a -> 
    {a with Atom.radius = rad_probe +. radius_of_atom a}) atoms in
  let max_radius = List.fold_left (fun r a -> max r a.Atom.radius) 0.0 atoms in
  let cell = IntCell.Atom.make ~size:(2.0 *. max_radius) atoms in
  let get_neighbors a =
    let nn = IntCell.Atom.neighbors cell a in
    let co = Atom.coord a in
    List.filter (fun a' ->
      let co' = Atom.coord a' in
      let d = Vec3.distance co co' in
      a'.Atom.type_symbol <> "H" && (not (Atom.equal a a'))
	&& d < a'.Atom.radius +. a.Atom.radius) nn in
  let rec overlap point = function
    | [] -> false
    | h::t -> 
	let co2 = Atom.coord h in
	if Vec3.distance point co2 < h.Atom.radius then true
	else overlap point t in
  let f_atom a =
    if a.Atom.type_symbol = "H" then a
    else
      let co = Atom.coord a in
      let rad = a.Atom.radius in
      let nn = get_neighbors a in
      let asa = List.fold_left 
	  (fun cnt (v,area) ->
	    let vco = Vec3.add co (Vec3.scale rad v) in
	    if overlap vco nn then cnt else cnt +. area)
	  0.0 vertices in
      {a with Atom.asa = asa *. rad *. rad}
  in
  List.map f_atom atoms
