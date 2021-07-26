open ExtLib
open PDBjTk
open PDBjBasis
open Printf

module Ht = Hashtbl

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
  
let get dblk =
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
	  let conn = Biounit.get_struct_conn dblk in
	  let dblk = 
	    {dblk with
	     Datablock.atom_siteCategory = List.map Atom.to_atom_site atoms;
	     struct_asymCategory = sasym;
	     struct_connCategory = conn} in
	  let struct_conf = Biounit.get_struct_conf dblk in
	  let sheet_range = Biounit.get_struct_sheet_range dblk in
	  {dblk with 
	   Datablock.struct_confCategory = struct_conf;
	   struct_sheet_rangeCategory = sheet_range})
	eunits

    
