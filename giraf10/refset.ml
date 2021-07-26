open Util
open ExtLib
open PDBjTk
open Printf
open Scanf
open PDBjBasis

type frame_t = Vec3.t * Vec3.t * Vec3.t * Vec3.t

(** refsets are tetrahedras *)
type refset  = {
    label_asym_id: string;
    rs_id: string;
    atoms: Atom.t array;
    frame: frame_t; 
    ft: float array;
  }

let print oc refset =
  fprintf oc "label_alym_id: %s\n" refset.label_asym_id;
  fprintf oc "rs_id: %s\n" refset.rs_id;
  fprintf oc "atoms: %d\n" (Array.length refset.atoms);
  Array.iteri 
    (fun i a ->
      fprintf oc "\t %2d atom_site_id: %s\n" i a.Atom.atom_site_id;
      fprintf oc "\t %2d label_asym_id: %s\n" i a.Atom.label_asym_id;
      fprintf oc "\t %2d label_seq_id: %d\n" i a.Atom.label_seq_id;
      fprintf oc "\t %2d label_comp_id: %s\n" i a.Atom.label_comp_id;
      fprintf oc "\t %2d label_atom_id: %s\n" i a.Atom.label_atom_id;
      fprintf oc "\t %2d xyz: %8.3f %8.3f %8.3f\n" i a.Atom.x a.Atom.y a.Atom.z;
    )
    refset.atoms;
  let o,x,y,z = refset.frame in
  let _x,_y,_z = o in
  List.iter (fun (l,(x,y,z)) -> 
    fprintf oc "\t %s: %8.3f %8.3f %8.3f\n" l x y z)
    [ ("O",o); ("X",x); ("Y",y); ("Z",z)];
  fprintf oc "features: ";
  Array.iter (fun f -> fprintf oc " %8.3f" f) refset.ft;
  fprintf oc "\n\n";
  flush oc

  
let nfeatures = 12 + 32 (* x,y,z of 4 atoms +  8 atom types in 4 directions *)
let nfeatures_fold = 24 + 20 (* x,y,z of 8 atoms + BLOSUM column *)
let ft_names = Array.to_list 
    (Array.init nfeatures (fun i -> sprintf "ft_%d" i))

let null_vec = Vec3.zero
let default_frame = (null_vec,null_vec,null_vec,null_vec)

let default = 
  {
   label_asym_id="";
    rs_id="0"; 
    atoms= [||]; 
    frame = default_frame;
    ft=[||];
 }

let dcut_env_max = 8.0
let dcut_env_min = 2.0
let rad_env = 0.5 *. (dcut_env_max -. dcut_env_min)

(* atomic environmnet of refsets. 
 Only atoms in the same label_asym_id as the refset are counted. *)
let set_refset_env cell refset =
  let env = Array.make_matrix 4 nstd_atypes 0.0 in
  let ori,v1,v2,v3 = refset.frame in
  let loc_atoms = IntCell.Atom.neighbors_coord cell ori in
  List.iter 
    (fun a ->
      let co' = Vec3.subtract (Atom.coord a) ori in
      let t = type_of_atom a in
      let d = Vec3.magnitude co' in
      if d <= dcut_env_max && d >= dcut_env_min && mem_std_atype t then
        let ind = get_atype_id t in
(*	let w = let s = (d -. dcut_env_min) /. rad_env in exp(-0.5 *. s *. s) in*)
	let w = 1.0 in
	let d1 = Vec3.dot co' v1 and d2 = Vec3.dot co' v2 in
	if d1 >= 0.0
	then env.(0).(ind) <- env.(0).(ind) +. w
	else env.(1).(ind) <- env.(1).(ind) +. w;

	if d2 >= 0.0
	then env.(2).(ind) <- env.(2).(ind) +. w
	else env.(3).(ind) <- env.(3).(ind) +. w
    )
    loc_atoms;
  let arr = Array.concat (Array.to_list env) in
  {refset with ft = Array.append refset.ft arr}

let get_bb atoms =
  let bb = Ht.create 100 in
  List.iter 
    (fun a -> 
      if a.Atom.group_PDB = "ATOM" &&
	(a.Atom.label_atom_id = "CA" 
       || a.Atom.label_atom_id = "N"
       || a.Atom.label_atom_id = "C") 
      then Ht.add bb 
	  (a.Atom.label_asym_id,a.Atom.label_seq_id,a.Atom.label_atom_id) a)
    atoms;
  bb

let get_calpha atoms =
  List.filter 
    (fun a -> a.Atom.label_atom_id = "CA" && a.Atom.group_PDB = "ATOM") 
    atoms

let get_frags atoms =
  let akey a = a.Atom.label_asym_id,a.Atom.label_seq_id in
  let calpha = get_calpha atoms in
  let res = 
    let ht = Ht.create 1 in 
    List.iter (fun a -> Ht.add ht (akey a) a) atoms;
    ht in
  let proc_res res a = 
    let ratoms = Ht.find_all res (akey a) in
    if List.exists (fun a -> a.Atom.label_atom_id = "N") ratoms
      && List.exists (fun a -> a.Atom.label_atom_id = "CA") ratoms
      && List.exists (fun a -> a.Atom.label_atom_id = "C") ratoms
    then Some ratoms
    else None in
  let is_segment a = 
	a.(0).Atom.label_asym_id = a.(1).Atom.label_asym_id &&
	a.(0).Atom.label_asym_id = a.(2).Atom.label_asym_id &&
	a.(0).Atom.label_asym_id = a.(3).Atom.label_asym_id &&
	a.(0).Atom.label_asym_id = a.(4).Atom.label_asym_id &&
	a.(0).Atom.label_seq_id = a.(1).Atom.label_seq_id - 1 &&
	a.(0).Atom.label_seq_id = a.(2).Atom.label_seq_id - 2 &&
	a.(0).Atom.label_seq_id = a.(3).Atom.label_seq_id - 3 &&
	a.(0).Atom.label_seq_id = a.(4).Atom.label_seq_id - 4
  in
  let rec loop frags = function
    | [] -> List.rev frags
    | l when List.length l < 5 -> List.rev frags
    | l -> 
	let f5 = Array.of_list (List.take 5 l) in
	let frags = 
	  if is_segment f5
	  then 
	    match proc_res res f5.(2)  with
	    | Some ratoms -> ((f5,ratoms)::frags) 
	    | None -> frags
	  else frags in
	loop frags (List.tl l)
  in
  loop [] calpha


let make_basis n ca c =
  let ori = Atom.coord ca in
  let vn = Vec3.normalize (Vec3.subtract (Atom.coord n) ori) in
  let vc = Vec3.normalize (Vec3.subtract (Atom.coord c) ori) in
  let xs = Vec3.normalize (Vec3.add vn vc) in
  let ys = Vec3.normalize (Vec3.cross vn vc) in
  let zs = Vec3.normalize (Vec3.cross xs ys) in
  xs,ys,zs


let find_pivot ratoms =
  let ca = List.find (fun a -> a.Atom.label_atom_id = "CA") ratoms in
  match ca.Atom.label_comp_id with
  | "GLY" -> Atom.coord ca
  | _ ->
      let n,co = List.fold_left 
	  (fun (n,co) a ->
	    if not(List.mem a.Atom.label_atom_id ["N"; "CA"; "C"; "O"; "OXT"])
	    then (n+1),(Vec3.add co (Atom.coord a))
	    else n,co)
	  (0,Vec3.zero) ratoms in
      if n = 0 then Atom.coord ca
      else Vec3.scale (1.0 /. float n) co

let transform_atom_array (ori,xs,ys,zs) a =
  let co = Vec3.subtract (Atom.coord a) ori in
  [|(Vec3.dot co xs); (Vec3.dot co ys); (Vec3.dot co zs)|]

let transform_atom_vec3 (ori,xs,ys,zs) a =
  let co = Vec3.subtract (Atom.coord a) ori in
  (Vec3.dot co xs), (Vec3.dot co ys), (Vec3.dot co zs)

let refset_of_frag cell label_asym_id (frag,ratoms) =
  let n = List.find (fun a -> a.Atom.label_atom_id = "N") ratoms in
  let ca = List.find (fun a -> a.Atom.label_atom_id = "CA") ratoms in
  let c = List.find (fun a -> a.Atom.label_atom_id = "C") ratoms in
  let ori = find_pivot ratoms in
  let xs,ys,zs = (make_basis n ca c) in
  let frame = ori,xs,ys,zs in
  let ft = 
    let arr = List.map (transform_atom_array frame) 
	[frag.(0); frag.(4); frag.(1); frag.(3)] in
    Array.concat arr in
  let rs = {label_asym_id=label_asym_id;
	    atoms=frag;
	    rs_id = (string_of_int frag.(2).Atom.label_seq_id);
	    frame=frame;
	    ft = ft} in
  set_refset_env cell rs

let refset_of_fold atoms =
  let lcalpha = get_calpha atoms in
  let calpha = Array.of_list lcalpha in 
  let n = Array.length calpha in
  let cnaa = Array.init n (fun i -> Array.make 20 0.0) in
  let cn = Array.make n 0.0 in
  for i = 0 to n - 3 do
    let a1 = calpha.(i) in
    let iaa = PDBjUtil.index_amino3 a1.Atom.label_comp_id in
    let co1 = Atom.coord a1 in
    for j = i + 2 to n - 1 do
      let a2 = calpha.(j) in
      if Vec3.distance co1 (Atom.coord a2) < 10.0 
      then
	let jaa = PDBjUtil.index_amino3 a2.Atom.label_comp_id in
	if iaa < 20 && jaa < 20 then
	  (cnaa.(i).(jaa) <- cnaa.(i).(jaa) +. 1.0;
	   cnaa.(j).(iaa) <- cnaa.(j).(iaa) +. 1.0);
	cn.(i) <- cn.(i) +. 1.0;
	cn.(j) <- cn.(j) +. 1.0;
    done;
  done;
  let ave = Array.fold_left (+.) 0.0 cn /. (float n) in
  let dev = Array.map (fun n -> let d = n -. ave in d*.d) cn in
  let sd = sqrt(Array.fold_left (+.) 0.0 dev /. (float n)) in
  let buried = Array.map (fun n -> ave <= n && n <= ave +. sd) cn in
  let get_env arr =
    let s = Array.make 20 0.0 in
    Array.iteri 
      (fun i n ->
	let bl = BLOSUM.blosum30.(i) in
	Array.iteri (fun i b -> s.(i) <- s.(i) +. n *. b) bl)
      arr;
    s in
  let bb = get_bb atoms in
  let lst = ref [] in
  for i = 4 to n - 5 do
    let a = calpha.(i) in
    let ori = Atom.coord a in
    if buried.(i)
    then
      if Ht.mem bb (a.Atom.label_asym_id,a.Atom.label_seq_id,"N")
	  && Ht.mem bb (a.Atom.label_asym_id,a.Atom.label_seq_id,"C")
      then
	let env = get_env cnaa.(i) in
	let n = Ht.find bb (a.Atom.label_asym_id,a.Atom.label_seq_id,"N") in
	let c = Ht.find bb (a.Atom.label_asym_id,a.Atom.label_seq_id,"C") in
	let xs,ys,zs = (make_basis n a c) in
	let frame = ori,xs,ys,zs in
	let co = Array.map (transform_atom_array frame) 
	    [| calpha.(i-4); calpha.(i+4); calpha.(i-3); calpha.(i+3);
	       calpha.(i-2); calpha.(i+2); calpha.(i-1); calpha.(i+1)|] in
	let ft = Array.concat (Array.to_list co) in
	let rs = {default with
		  atoms = Array.sub calpha (i-4) 9;
		  frame = frame;
		  ft = Array.append ft env} in
	lst := rs :: !lst
  done;
  let refsets = List.mapi (fun i r -> {r with rs_id = string_of_int i}) 
      (List.rev !lst) in
  lcalpha,refsets
  
let make_atoms (label_asym_id,atoms) = 
  let cell = IntCell.Atom.make ~size:dcut_env_max atoms in
  let frags = get_frags atoms in
  let rs = List.map (refset_of_frag cell label_asym_id) frags in
  rs

let make_proteins proteins =
  let l = List.fold_left 
      (fun rs al -> List.rev_append (make_atoms al) rs)
      [] proteins in
  l

let atoms_filter atoms refsets =
  let akey a = a.Atom.label_asym_id,a.Atom.label_seq_id in
  let ht = Ht.create 100 in
  List.iter (fun a -> Ht.replace ht (akey a) ()) atoms;
  List.filter (fun r -> Ht.mem ht (akey r.atoms.(2))) refsets

let make_exposed_subunit atoms =
  let a = List.hd atoms in
  let refsets = make_atoms (a.Atom.label_asym_id,atoms) in
  let atoms = Asa.get OONS.radius_of_atom atoms in
  let exposed = Ht.create 10 in
  List.iter (fun a -> 
    let key = (a.Atom.label_asym_id,a.Atom.label_seq_id) in
    let asa = a.Atom.asa +. Ht.find_default exposed key 0.0 in
    Ht.replace exposed key asa)
    atoms;
  let ave = (Ht.fold (fun _ a asa -> a +. asa) exposed 0.0) 
      /. float (Ht.length exposed) in
  Ht.iter (fun k v -> if v < ave then Ht.remove exposed k) exposed;
  let dcut = 2.0 and size = 10.0 in
  let eatoms = List.filter (fun a -> a.Atom.asa > 0.0) atoms in
  let cell = IntCell.Atom.make ~size eatoms in
  let atoms = List.filter 
      (fun a -> 
	match IntCell.Atom.neighbors cell a with
	| [] -> false
	| l ->
	    let co1 = Atom.coord a in
	    List.exists (fun b ->
	      let co2 = Atom.coord b in
	      Vec3.distance co1 co2 < dcut) l)
      atoms in
  let refsets = List.filter 
      (fun r ->
	Ht.mem exposed 
	  (r.atoms.(2).Atom.label_asym_id,r.atoms.(2).Atom.label_seq_id))
      refsets in
  atoms,refsets

let transform_point (ori,xs,ys,zs) co = 
  let co = Vec3.subtract co ori in
  let x = Vec3.dot co xs 
  and y = Vec3.dot co ys 
  and z = Vec3.dot co zs in
  x,y,z

let prep_atoms r atoms =
  List.map 
    (fun a -> 
      let tco = transform_point r.frame (Atom.coord a) in
      let t = type_of_atom a in
      let atype_id = get_atype_id t in
      atype_id,tco)
    atoms

let dgrid = 1.0

let make_bit_lattice ?(dcut=dgrid) if_id atoms refset = 
  let dwidth = int_of_float (floor (dcut /. dgrid)) in
  let dcut2 = dcut *. dcut in
  let aco = prep_atoms refset atoms in
  let ind_of_co x = int_of_float (floor x /. dgrid) in
  let ind i k l m  = 
    radix12_of_string (sprintf "%d.%d.%d.%d" i k l m) in
  let htcen = Ht.create 1 in
  let natoms = ref 0 in
  List.iter 
    (fun (aid,((x,y,z) as co)) ->
      if aid < nstd_atypes && Vec3.magnitude co < 15.0
      then
	let k = ind_of_co x in
	let l = ind_of_co y in
	let m = ind_of_co z in
	incr natoms;
	if if_id = "query" then
	  for a = -dwidth to dwidth do
	    for b = -dwidth to dwidth do
	      for c = -dwidth to dwidth do
		if float (a * a + b * b + c * c) <= dcut2 then
		  Ht.replace htcen (ind aid (k+a) (l+b) (m+c)) ()
	      done;
	    done;
	  done
	else
	  Ht.replace htcen (ind aid k l m) ()
      else ())
    aco;
  let lat = Ht.fold (fun k _ l -> k::l) htcen [] in
  (string_of_int !natoms),"{" ^ (String.join "," lat) ^ "}" 

let encode_frame frame = Frame_j.string_of_frame_t frame
let decode_frame str = Frame_j.frame_t_of_string str

(** for GI search *)
let make_hashtbl pdbid refsets =
  let ht = Ht.create (List.length refsets) in
  List.iter (fun r -> Ht.add ht r.rs_id r) refsets;
  ht

let sql_columns =
  let b = "type,if_id,rs_id,frame," in
  let env = String.concat "," ft_names in
  b ^ env ^ ",natoms,lattice"

let prep_sql_save_data (conn: PG.connection) ?(if_id="query") 
    ?dcut itype atoms refsets =
  let ncol = nfeatures + 4 + 2 in
  let one ind r =
    let env = Array.map string_of_float r.ft in
    let natoms,latt = make_bit_lattice ?dcut if_id atoms r in
    let frame = encode_frame r.frame in
    let param = Array.concat 
	[ [| itype; if_id; r.rs_id; frame |]; env; [|natoms; latt|] ] in
    let v = Array.init ncol 
	(fun i -> "$" ^ string_of_int (i + 1 + ncol * ind)) in
    let vs = "(" ^ (String.concat "," (Array.to_list v)) ^ ")" in
    param,vs in
  let params,vals = List.split (List.mapi one refsets) in
  let params = Array.concat params in
  let vals = String.concat "," vals in
  params,vals
  
let make_sql_insert (conn: PG.connection) table ?dcut itype atoms = function
  | [] -> [||],""
  | refsets ->
      let base = "INSERT INTO " ^ table ^ "(" ^ sql_columns ^ ") VALUES " in
      let params,vals = prep_sql_save_data conn ?dcut itype atoms refsets in
      let q = base ^ vals in
      params, q

let restore_from_result (result: PG.result) =
  let n = result#ntuples in
  let fnames = Array.mapi (fun i a -> (i,a)) result#get_fnames in
  let rec loop l i =
    if i = n then List.rev l
    else
      let t = result#get_tuple i in
      let init = {default with ft = Array.make nfeatures 0.0} in
      let refset = Array.fold_left 
          (fun rs (i,fn) ->
            match fn with
            | "rs_id" -> {rs with rs_id = t.(i)}
            | "frame" -> 
                let frame = decode_frame t.(i) in
                {rs with frame = frame}
            | ft when String.starts_with ft "ft_" -> 
		let j = int_of_string (snd(String.split ft "_")) in
		rs.ft.(j) <- float_of_string t.(i); rs
	    | _ -> rs)
          init fnames
      in
      loop (refset::l) (succ i)
  in
  loop [] 0

let restore_by_interface conn if_id =
  let res = Sql.select conn ~params:[|if_id|]
      "SELECT * FROM Refaco WHERE if_id = $1" in
  restore_from_result res

let make_frefsets refsets =
  let ht = Ht.create 10 in
  List.iter (fun r -> Ht.add ht r.rs_id r) refsets;
  fun rs_id -> Ht.find ht rs_id
