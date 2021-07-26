(** prepare input structures for GIRAF-Search and GIRAF-Dock *)
open Util
open Printf
open PDBjTk
open PDBjBasis

let read_file infile =
  match infile with
  | PDBF file -> PDBFormat.parse_file file
  | PDBML file -> PDBML.parse_file file
  | MMCIF file -> MmCIF.parse_file file
  | PDBID pdbid ->
      let file = Filename.concat (Sys.getenv "PDBMLDIR") (pdbid ^ ".xml.gz") in
      PDBML.parse_file file
  | Unknown file -> PDBjUtil.parse_file file

let of_asyms itypes dblk auth_asyms label_asyms =
  let atoms = Prep_struct.remove_hydrogens (PDBjUtil.atoms_of_datablock dblk) in
  let atoms = 
    match auth_asyms,label_asyms with
    | [],[] -> 
	atoms
    | l,[] -> 
	List.filter (fun a -> List.mem a.Atom.auth_asym_id l) atoms
    | _,l -> 
	List.filter (fun a -> List.mem a.Atom.label_asym_id l) atoms
  in
  let asyms = 
    Prep_struct.remove_water_guess (PDBjUtil.split_atoms2asyms atoms) in
  let proteins = Prep_struct.select_proteins_guess asyms in
  let lst = 
    if List.exists (fun s -> s <> FOLD) itypes then
      List.filter_map 
	(fun (asym,atoms) ->
	  let atoms,refsets = Refset.make_exposed_subunit atoms in
	  fprintf stderr "Prepin.of_asyms: atoms= %d , refsets = %d\n"
	    (List.length atoms) (List.length refsets);
	  flush stderr;
	  match atoms,refsets with
	  | [],_ -> 
	      fprintf stderr "Prepin.of_asyms: no atoms\n"; flush stderr;
	      None
	  | _,[] -> 
	      fprintf stderr "Prepin.of_asyms: no refsets\n"; flush stderr;
	      None
	  | a,r -> Some (asym,(a,r)))
	proteins
    else [] in
  let ars_fold = 
    if List.mem FOLD itypes 
    then
      List.filter_map 
	(fun (asym,atoms) ->
	  let atoms,refsets = Refset.refset_of_fold atoms in
	  fprintf stderr "Prepin.of_asyms: atoms= %d , refsets = %d\n"
	    (List.length atoms) (List.length refsets);
	  flush stderr;
	  match atoms,refsets with
	  | [],_ -> None
	  | _,[] -> None
	  | a,r -> Some (asym,(a,r)))
	proteins 
    else [] in
  List.append lst ars_fold
