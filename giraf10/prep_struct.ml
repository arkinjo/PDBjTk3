open PDBjTk
open PDBjBasis
open Util
open Printf

let simplify_datablock dblk =
  {Datablock.default with
   Datablock.entry = dblk.Datablock.entry;
   database_PDB_rev = dblk.Datablock.database_PDB_rev;
   entity = dblk.Datablock.entity;
   entity_poly = dblk.Datablock.entity_poly;
   exptl = dblk.Datablock.exptl;
   pdbx_entity_nonpoly = dblk.Datablock.pdbx_entity_nonpoly;
   pdbx_nmr_representative = dblk.Datablock.pdbx_nmr_representative;
   pdbx_struct_assembly = dblk.Datablock.pdbx_struct_assembly;
   pdbx_struct_assembly_gen = dblk.Datablock.pdbx_struct_assembly_gen;
   pdbx_struct_oper_list = dblk.Datablock.pdbx_struct_oper_list;
   struct_ = dblk.Datablock.struct_;
   struct_asym = dblk.Datablock.struct_asym;
   struct_biol = dblk.Datablock.struct_biol;
   struct_conf = dblk.Datablock.struct_conf;
   struct_conn = dblk.Datablock.struct_conn;
   struct_ref = dblk.Datablock.struct_ref;
   struct_sheet_range = dblk.Datablock.struct_sheet_range;
   atom_site = dblk.Datablock.atom_site;}

let clean dblk =
  let ht = Ht.create 1 in
  List.iter (fun a ->
    Ht.replace ht (Option.default 1 a.Atom_site.pdbx_PDB_model_num) ())
    dblk.Datablock.atom_site;
  let model_num = Ht.fold (fun k _ m -> min k m) ht max_int in
  let model_num = 
    match dblk.Datablock.pdbx_nmr_representative with
    | [] -> model_num
    | h::_ -> 
	try
	  let id = Option.default (string_of_int model_num) 
	      h.Pdbx_nmr_representative.conformer_id in
	  int_of_string id
	with _ -> model_num
  in
  let asites = List.filter (fun a ->
    Option.default 1 a.Atom_site.pdbx_PDB_model_num = model_num
      && a.Atom_site.type_symbol <> Some "H"
      && (match a.Atom_site.label_alt_id with 
      | None -> true | Some id -> id = "1" || id = "A" || id = ".")
      && not (a.Atom_site.group_PDB = Some "HETATM" && 
              (a.Atom_site.label_comp_id = Some "HOH" 
             || a.Atom_site.label_comp_id = Some "DOD" )))
      dblk.Datablock.atom_site in
  simplify_datablock {dblk with Datablock.atom_site = asites}

let remove_water dblk asyms =
  List.filter (fun (asym_id,_) ->
    not (PDBjUtil.asym_is_water dblk asym_id)) asyms

let remove_water_guess asyms =
  List.filter (fun (_,l) ->
    not (List.exists (fun a -> 
      a.Atom.group_PDB = "HETATM" 
	&& (a.Atom.auth_comp_id = "HOH"
	  || a.Atom.auth_comp_id = "DOD")) l)) asyms

let remove_water_atoms_guess atoms =
  List.filter (fun a -> 
    not (a.Atom.group_PDB = "HETATM" 
	   && (a.Atom.auth_comp_id = "HOH" || 
	   a.Atom.auth_comp_id = "DOD"))) atoms

let split_asyms dblk atoms =
  let asyms = remove_water dblk (PDBjUtil.split_atoms2asyms atoms) in
  asyms

let select_proteins_guess asyms =
  List.filter (fun (_,l) -> PDBjUtil.guess_is_protein l) asyms

let remove_hydrogens atoms =
  List.filter (fun a -> a.Atom.type_symbol <> "H") atoms

let unknown_residues_only atoms =
  let frac_limit = 0.7 in
  let n = List.length atoms in
  let nunk = List.fold_left (fun n a ->
    if a.Atom.label_comp_id = "UNK" then succ n else n) 0 atoms in
  let frac = float nunk /. float n in 
  if frac > frac_limit then
    (fprintf stderr "Prep_struct.unknown_residues_only = %f\n" frac;
     true)
  else false


let filter_virus units = 
  let nan = Str.regexp "[^0-9]+" in
  let num = Str.regexp "[0-9]+" in
  let icosa = "icosahedral asymmetric unit" in
  let helical = "helical asymmetric unit" in
  let point = "point asymmetric unit" in
  if List.exists (fun u -> u.Biounit.details = icosa) units
  then [List.find (fun u -> u.Biounit.details = icosa) units]
  else if List.exists (fun u -> u.Biounit.details = point) units
  then [List.find (fun u -> u.Biounit.details = point) units]
  else if List.exists 
      (fun u -> u.Biounit.details = helical) units
  then [List.find 
	  (fun u -> u.Biounit.details = helical) units]
  else if List.exists (fun u -> Str.string_match nan u.Biounit.id 0) units
  then [List.hd (List.filter (fun u -> Str.string_match num u.Biounit.id 0)
		   units)]
  else units
      
let gen_biounit dblk =
  let atoms = PDBjUtil.atoms_of_datablock dblk in
  match filter_virus (Biounit.get_units dblk) with
  | [] -> List.enum [("0", dblk)]
  | units ->
      let gen = Biounit.generate1 dblk atoms in
      let eunits = List.enum units in
      Enum.map 
	(fun u ->
	  let i,atoms = gen u in
	  let sasym = Biounit.get_struct_asym dblk atoms in
	  let conn = Biounit.get_struct_conn dblk atoms in
	  let dblk = 
	    {dblk with
	     Datablock.atom_site = List.map Atom.to_atom_site atoms;
	     struct_asym = sasym;
	     struct_conn = conn} in
	  let struct_conf = Biounit.get_struct_conf dblk in
	  let sheet_range = Biounit.get_struct_sheet_range dblk in
	  i,{dblk with 
	     Datablock.struct_conf = struct_conf;
	     struct_sheet_range = sheet_range})
	eunits

let gen_biounit1 dblk assembly_id =
  let atoms = PDBjUtil.atoms_of_datablock dblk in
  match filter_virus (Biounit.get_units dblk) with
  | [] -> dblk
  | units ->
      if List.exists (fun u -> u.Biounit.id = assembly_id) units 
      then
	let u = List.find (fun u -> u.Biounit.id = assembly_id) units in
	let i,atoms = Biounit.generate1 dblk atoms u in
	let sasym = Biounit.get_struct_asym dblk atoms in
	let conn = Biounit.get_struct_conn dblk atoms in
	let dblk = 
	  {dblk with
	   Datablock.atom_site = List.map Atom.to_atom_site atoms;
	   struct_asym = sasym;
	   struct_conn = conn} in
	let struct_conf = Biounit.get_struct_conf dblk in
	let sheet_range = Biounit.get_struct_sheet_range dblk in
	{dblk with 
	 Datablock.struct_conf = struct_conf;
	 struct_sheet_range = sheet_range}
      else dblk

    
