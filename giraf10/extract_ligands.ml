open PDBjTk
open PDBjBasis
open Util
open Printf
open ExtLib

type lig = {
    pdbid: string;
    assembly_id: string;
    if_id: string;
    ligand_asym_id: string;
    nlatoms: int;
  }

let outdir = "ligands"

module CC = Chem_comp
module CCA = Chem_comp_atom

let get_coord dblk = 
  let cc = List.hd dblk.Datablock.chem_compCategory in
  match cc.CC.pdbx_ideal_coordinates_missing_flag
      , cc.CC.pdbx_model_coordinates_missing_flag with
  | (Some "N"),_ -> 
      fun cca a ->
	(match cca.CCA.pdbx_model_Cartn_x_ideal with
	| Some _ ->
	    Some {a with
		  Atom.x = Option.get cca.CCA.pdbx_model_Cartn_x_ideal;
		  y = Option.get cca.CCA.pdbx_model_Cartn_y_ideal;
		  z = Option.get cca.CCA.pdbx_model_Cartn_z_ideal}
	| None -> None)
  | _,(Some "N") -> 
      fun cca a ->
	(match cca.CCA.model_Cartn_x with
	| Some _ ->
	    Some {a with
		  Atom.x = Option.get cca.CCA.model_Cartn_x;
		  y = Option.get cca.CCA.model_Cartn_y;
		  z = Option.get cca.CCA.model_Cartn_z}
	| None -> None)
  | _,_ ->
      fun cca a ->
	(match cca.CCA.pdbx_model_Cartn_x_ideal with
	| Some _ ->
	    Some {a with
		  Atom.x = Option.get cca.CCA.pdbx_model_Cartn_x_ideal;
		  y = Option.get cca.CCA.pdbx_model_Cartn_y_ideal;
		  z = Option.get cca.CCA.pdbx_model_Cartn_z_ideal}
	| None -> None)

let atom_of_chem_comp_atoms dblk = 
  let get_co = get_coord dblk in
  let conv cca = 
    try
      match cca.CCA.type_symbol with
      | None -> None
      | Some "H" -> None
      | Some _ ->
	  let a = {Atom.default with 
		   Atom.label_asym_id = "A";
		   auth_asym_id = "A";
		   model_num = 1;
		   group_PDB = "HETATM";
		   label_seq_id = 1;
		   auth_seq_id = "1";
		   ins_code = ".";
		   label_alt_id = ".";
		   label_comp_id = Option.get cca.CCA.pdbx_component_comp_id;
		   auth_comp_id =  Option.get cca.CCA.pdbx_component_comp_id;
		   type_symbol =   Option.get cca.CCA.type_symbol;
		   label_atom_id = Option.get cca.CCA.pdbx_component_atom_id;
		   auth_atom_id =  Option.get cca.CCA.pdbx_component_atom_id;
		   occupancy = 1.0;
		   b_iso = 0.0;
		   atom_site_id = 
		   string_of_int (Option.get cca.CCA.pdbx_ordinal);} in
	  get_co cca a
    with exc -> 
      fprintf stderr "Error in comp %s  atom %s : %s\n"
	(Option.get cca.CCA.comp_id) (Option.get cca.CCA.atom_id)
	(Printexc.to_string exc);
      flush stderr;
      raise exc
  in
  List.filter_map conv dblk.Datablock.chem_comp_atomCategory

let dump_pdb dir atoms = 
  let a1 = List.hd atoms in
  let cc = a1.Atom.label_comp_id in
  let file = Filename.concat dir (cc ^ ".pdb") in
  let oc = open_out file in
  PDBFormat.write_atom_records oc atoms;
  close_out oc

let dump_extatom dir atoms =
  let a1 = List.hd atoms in
  let cc = a1.Atom.label_comp_id in
  let file = Filename.concat dir (cc ^ ".xml") in
  let dblk = {Datablock.default 
	     with Datablock.atom_siteCategory = 
	      List.map Atom.to_atom_site atoms} in
  let doc = PDBML.xml_of_datablock dblk in
  let oc = open_out file in
  fprintf oc "%s\n" (Xml.to_string doc);
  close_out oc

let main () =
  let ifile = ref "" in
  let min_nlatoms = ref 5 in
  let dir = ref outdir in
  let ofmt = ref "pdb" in
  let specs = [
    ("-min_atoms",Arg.Set_int min_nlatoms,"Minimum number of ligand atoms [5]");
    ("-dir", Arg.Set_string dir, "directory for output [./ligands]");
    ("-ofmt", Arg.Symbol (["pdb"; "extatom"],(fun o -> ofmt := o))
       , "output format")
  ] in
  let usage = sprintf "%s [option] dbname" Sys.executable_name in
  Arg.parse specs (fun s -> ifile := s) usage;
  let ifile = !ifile in
  let min_nlatoms = !min_nlatoms in
  let dir = !dir in
  if ifile = "" then
    begin
      Arg.usage specs usage;
      exit 1
    end;
  fprintf stderr "ifile = %s\n" ifile;
  fprintf stderr "min_nlatoms = %d\n" min_nlatoms;
  let dblk = PDBML.parse_file ifile in 
  let atoms = atom_of_chem_comp_atoms dblk in
  if !ofmt = "pdb" 
  then dump_pdb dir atoms
  else dump_extatom dir atoms;
  ()

let _ = main ()

  
