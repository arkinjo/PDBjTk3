open PDBjTk
open PDBjBasis
open Util
open Printf
open Scanf 

let _ = Printexc.record_backtrace true
let debug = true
let warn s = if debug then ( prerr_endline ("Interface: " ^ s); flush stderr) else ()

(** protein-ligand and protein-protein interfaces *)

(** minimum number of interface atoms for PLI *)
let min_atoms = 10
let min_atoms_ppi = 20
let max_ligand_atoms = 1000000

type interface = {
    pdbid: string;
    assembly_id: string;
    if_id: string; 
    itype: itype;
    receptor: string;
    ligand: string;
    atoms: Atom.t list; (* receptor atoms *)
    latoms: Atom.t list; (* ligand atoms *)
  }

let compare i1 i2 =
  let d = compare_itype i1.itype i2.itype in
  if d <> 0 then d
  else if i1.receptor < i2.receptor then -1
  else if i1.receptor > i2.receptor then 1
  else if i1.ligand < i2.ligand then -1
  else if i1.ligand > i2.ligand then 1
  else 
    let l1 = List.length i1.atoms and l2= List.length i2.atoms in
    if l1 < l2 then -1
    else if l1 > l2 then 1
    else 
      let l1 = List.length i1.latoms and l2= List.length i2.latoms in
      if l1 < l2 then -1
      else if l1 > l2 then 1
      else 0

(** augment interface atoms to include whole residue atoms. *)
let augment_interface receptor ifatoms =
  let ht = Ht.create 1 in
  List.iter 
    (fun a ->
      let key = a.Atom.label_asym_id,a.Atom.label_seq_id in
      if Ht.mem ht key then () else Ht.add ht key ())
    ifatoms;
  List.filter (fun a -> 
    Ht.mem ht (a.Atom.label_asym_id,a.Atom.label_seq_id)) receptor

(** find residues of receptor in contact with ligand *)
let contact ?(dcut=5.0) receptor ligand =
  let d2 = dcut *. dcut in
  let cell = IntCell.Atom.make ~size:(dcut +. 1.0) receptor in
  let ht = Ht.create 1 in
  let htch = Ht.create 1 in
  List.iter 
    (fun a ->
      let co = Atom.coord a in
      let nn = IntCell.Atom.neighbors_coord cell co in
      List.iter (fun b ->
	let co' = Atom.coord b in
	if not (Ht.mem ht b.Atom.atom_site_id) 
	    && Vec3.distance2 co co' < d2 then (
	  Ht.replace ht b.Atom.atom_site_id ();
	  Ht.replace htch b.Atom.label_asym_id ()))
	nn)
    ligand;
  let atoms = List.filter (fun a -> Ht.mem ht a.Atom.atom_site_id) receptor in
  let atoms = augment_interface receptor atoms in
  let ch = List.sort ~cmp:String.compare 
      (Ht.fold (fun k _ l -> k::l) htch []) in
  ch,atoms

(** protein-ligand interfaces *)
let find_interface ?dcut pdbid assembly_id itype (receptor,ligand) =
  List.flatten
    (List.filter_map 
       (fun (lch,latoms) ->
	 if List.length ligand > max_ligand_atoms then None
	 else
	   Some
	     (List.filter_map 
		(fun (rch,atoms) ->
		  match snd(contact ?dcut atoms latoms) with
		  | [] -> None
		  | la -> 
		      let natoms = List.length la in
		      if natoms < min_atoms
		    || (itype = PPI && natoms < min_atoms_ppi)
		      then None
		      else
			let if_id = 
			  String.concat ":" [pdbid; assembly_id; rch; lch] in
			Some {pdbid=pdbid; assembly_id=assembly_id;
			      if_id=if_id; itype=itype;
			      receptor=rch; ligand=lch;
			      atoms=la; latoms=latoms})
		receptor))
       ligand)

let find_fold ?dcut pdbid assembly_id itype (receptor,ligand) =
  List.map (fun (rch,ratoms) ->
    let lch,latoms = "?",[] in
    let if_id = String.concat ":" [pdbid; assembly_id; rch; lch] in
    {pdbid=pdbid; assembly_id=assembly_id;
     if_id=if_id; itype=itype; receptor=rch; ligand=lch;
     atoms=ratoms; latoms=latoms}) receptor

let find_all pdbid assembly_id lst = 
  List.fold_left 
    (fun ifs (itype,objs) -> 
      let find = if itype = FOLD then find_fold else find_interface in
      let interfaces = List.map (find pdbid assembly_id itype) objs in
      List.fold_left List.rev_append ifs interfaces)
    []
    lst

let string_of_atom_list atoms = Atom_j.string_of_atom_list atoms
let atom_list_of_string str = Atom_j.atom_list_of_string str

let make_sql_insert (conn: PG.connection) pdbid assembly_id interfaces =
  let exp = "($1,$2,$3,$4,$5,$6,$7,$8,$9)" in
  let qparams = List.rev_map
      (fun i ->
	let t = string_of_itype i.itype in
	let q = sprintf "INSERT INTO Interfaces_%s(if_id,pdbid,assembly_id,label_asym_id,ligand_asym_id,atoms,atom_site_ids,natoms,nlatoms) VALUES %s" t exp in
	let asites = string_of_atom_list i.atoms in
(*	let asites = conn#escape_bytea asites in*)
	let asite_ids = Sql.string_of_string_list 
	    (List.map (fun a -> a.Atom.atom_site_id) i.atoms) in
	q,[|i.if_id; pdbid;assembly_id;i.receptor;i.ligand;
	    asites; asite_ids; 
	    string_of_int (List.length i.atoms) ;
	    string_of_int (List.length i.latoms)|])
      interfaces in
  qparams

let load conn pdbid assembly_id interfaces =
  let l = make_sql_insert conn pdbid assembly_id interfaces in
  try
    List.iter (fun (q,params) -> Sql.command conn ~params q) l
  with
  | PG.Error e -> failwith ("Interface.load: " ^ (PG.string_of_error e))


let restore conn if_id =
  let res = Sql.select conn ~params:[|if_id|]
      "SELECT i.if_id, i.type, i.atoms, s.atom_record
      FROM Interfaces i JOIN Structs s ON s.pdbid = i.pdbid 
      WHERE i.if_id = $1" in
  if res#ntuples = 0 then
    raise Not_found
  else
    let t = res#get_tuple 0 in
    let if_id = t.(0) in
    let ids = String.nsplit if_id ":" in
    let pdbid = List.nth ids 0 in
    let assembly_id = List.nth ids 1 in
    let receptor = List.nth ids 2 in
    let ligand = List.nth ids 3 in
    let itype = itype_of_string t.(1) in
    let atoms = atom_list_of_string t.(2) in
    let dblk = 
      Prep_struct.gen_biounit1 (PDBML.parse_string t.(3)) assembly_id in
    let whole = PDBjUtil.atoms_of_datablock dblk in
    let latoms = List.filter 
	(fun a -> a.Atom.label_asym_id = ligand) 
	whole in
    let htch = Ht.create 1 in
    List.iter (fun a -> Ht.replace htch a.Atom.label_asym_id ()) atoms;
    let proteins = List.filter (fun a -> Ht.mem htch a.Atom.label_asym_id) 
	whole in
    {if_id = if_id;
     pdbid = pdbid;
     assembly_id = assembly_id;
     itype = itype; 
     receptor= receptor; 
     ligand = ligand;
     atoms = atoms; latoms = latoms},
    pdbid,proteins

let restore_simple conn if_id =
  let res = Sql.select conn ~params:[|if_id|]
      "SELECT i.if_id, i.atoms, i.type FROM Interfaces i WHERE i.if_id = $1" in
  if res#ntuples = 0 then
    raise Not_found
  else
    let t = res#get_tuple 0 in
    let if_id = t.(0) in
    let ids = String.nsplit if_id ":" in
    let pdbid = List.nth ids 0 in
    let assembly_id = List.nth ids 1 in
    let receptor = List.nth ids 2 in
    let ligand = List.nth ids 3 in
    let itype = itype_of_string t.(2) in
    let atoms = atom_list_of_string t.(1) in
    {if_id=if_id;
     pdbid=pdbid;
     assembly_id=assembly_id;
     itype = itype; 
     receptor= receptor; 
     ligand = ligand;
     atoms = atoms; latoms = []}

let prepare_ifs conn if_id =
  try
    let iquery = restore_simple conn if_id in
    let atoms = iquery.atoms in
    let refsets = Refset.restore_by_interface conn if_id in
    match atoms,refsets with
    | [],_ | _,[] -> None
    | ars -> Some ars
  with 
  | Not_found -> None
  | exc -> fprintf stderr "Interface.prepare_ifs: unknown error (%s)\n"
	(Printexc.to_string exc); raise exc

(** get summary of all interfaces *)
type summary = {
    s_pdbid: string;
    s_entity_id: string;
    s_assembly_id: string;
    s_if_id: string;
    s_type: string;
    s_title: string;
    s_descriptor: string;
    s_label_asym_id: string;
    s_auth_asym_id: string;
    s_description: string;
    s_l_entity_id: string;
    s_l_label_asym_id: string;
    s_l_auth_asym_id: string;
    s_l_description: string;
    s_comp_id: string;
  }

let default_summary = {
  s_pdbid = ""; s_entity_id=""; s_assembly_id= ""; s_if_id = "";
  s_type=""; s_title=""; s_descriptor = ""; s_label_asym_id = "";
  s_auth_asym_id = ""; s_description = ""; 
  s_l_entity_id = ""; s_l_label_asym_id = "";
  s_l_auth_asym_id = ""; s_l_description = ""; s_comp_id = "";}


let get_summary conn = function 
  | [] -> Ht.create 1
  | objset ->
      let objset = List.map (fun i -> 
	sprintf "if_id = '%s'" i) objset in
      let lpdb = "WHERE " ^ String.concat " OR " objset  in
      let q = "SELECT pdbid,entity_id,assembly_id,if_id,type,title,descriptor,label_asym_id,auth_asym_id,description,l_entity_id,l_label_asym_id,l_auth_asym_id,comp_id,l_description FROM Interfaces_summary " ^ lpdb in
      let (res: PG.result) = Sql.select conn q in
      let gr = Sql.result_of_field res in
      let ht = Ht.create 1 in
      Array.iter 
	(fun t ->
	  let r = gr t in
	  let s = {
	    s_pdbid = r "pdbid";
	    s_assembly_id = r "assembly_id";
	    s_entity_id = r "entity_id";
	    s_if_id = r "if_id";
	    s_type = r "type";
	    s_title = r "title";
	    s_descriptor = r "descriptor";
	    s_label_asym_id = r "label_asym_id";
	    s_auth_asym_id = r "auth_asym_id";
	    s_description = r "description";
	    s_l_entity_id = r "l_entity_id";
	    s_l_label_asym_id = r "l_label_asym_id";
	    s_l_auth_asym_id = r "l_auth_asym_id";
	    s_l_description = r "l_description";
	    s_comp_id = r "comp_id"} in
	  Ht.add ht s.s_if_id s)
	res#get_all;
      ht
      
let get_iatoms conn itype if_id = 
  let q = sprintf 
      "SELECT atoms FROM Interfaces_%s WHERE if_id = $1" itype in
  try 
    let res = Sql.select conn ~params:[|if_id|] q in
    let bdata = res#getvalue 0 0 in
    let atoms = atom_list_of_string bdata in
    let _ = List.hd atoms in
    atoms
  with 
  | exc ->
    warn (sprintf "Interface.get_iatoms: Exception %s" (Printexc.to_string exc));
    Printexc.print_backtrace stderr;
    exit 6
