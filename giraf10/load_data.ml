open Printf
open PDBjTk
open PDBjBasis
open Util

let file_size_limit = 100_000_000

let get_pdbmldir = 
  let pdbmldir = ref "" in
  fun () -> 
    if !pdbmldir <> "" then !pdbmldir
    else
      let d = 
	try Sys.getenv "PDBMLDIR"
	with Not_found ->  Sys.getcwd () in
      pdbmldir := d;
      !pdbmldir

let read_pdbmldir pdbids = 
  let pdbmldir = get_pdbmldir () in
  let ht = Ht.create 1 in
  List.iter (fun id -> Ht.add ht (id ^ ".xml.gz") ()) pdbids;
  let files = Array.to_list (Sys.readdir pdbmldir) in
  List.filter_map (fun s -> 
    if Ht.mem ht s
    then Some (Filename.concat pdbmldir s)
    else None) files

let delete_obsolete conn =
  let pdbmldir = get_pdbmldir () in
  let files = Sys.readdir pdbmldir in
  let ht = Ht.create 10000 in
  Array.iter
    (fun s -> 
      let pdbid = Filename.chop_suffix s ".xml.gz" in
      let filename = Filename.concat pdbmldir s in
      let mtime = (Unix.stat filename).Unix.st_mtime in
      Ht.add ht pdbid mtime) 
    files;
  let res = Sql.select conn "SELECT LOWER(pdbid),mtime FROM Structs" in
  let pdbids = Array.to_list
      (Array.map (fun t -> t.(0), (float_of_string t.(1))) res#get_all) in
  List.iter 
    (fun (pdbid,mtime) -> 
      if not (Ht.mem ht pdbid) 
      then
	begin
	  fprintf stderr "Load_data: deleting obsolete entry: %s ..." pdbid;
	  flush stderr;
	  Sql.command conn ~params:[|pdbid|] 
	    "DELETE FROM Structs WHERE pdbid = UPPER($1)";
	  fprintf stderr " done.\n";
	  flush stderr
	end
      else if Ht.find ht pdbid > mtime 
      then
	begin
	  fprintf stderr "Load_data: deleting updated entry: %s ..." pdbid;
	  flush stderr;
	  Sql.command conn ~params:[|pdbid|] 
	    "DELETE FROM Structs WHERE pdbid = UPPER($1)";
	  fprintf stderr " done.\n";
	  flush stderr;
	end
      else Ht.remove ht pdbid)
    pdbids;
  ht

let check_loaded conn pdbid mtime =
  Sql.command conn ~params:[|pdbid; mtime|]
    "DELETE FROM Structs WHERE pdbid = $1 AND mtime < $2";
  let res = Sql.select conn ~params:[|pdbid|]
      "SELECT * FROM Structs WHERE pdbid = $1" in
  res#ntuples > 0

let load_core conn mtime itype dblk =
  let warn _ = () in
  warn "load_core: 1";
  let pdbid = PDBjUtil.pdbid dblk in
  warn "load_core: 2";
  if check_loaded conn pdbid mtime
  then (fprintf stderr "Warning: %s already loaded.\n" pdbid; flush stderr)
  else
    let proc_bu (assembly_id,dblk) =
      let atoms = PDBjUtil.atoms_of_datablock dblk in
      if Prep_struct.unknown_residues_only atoms 
      then (fprintf stderr "load_core: %s too many unknown residues\n" pdbid;
	    flush stderr)
      else
	let natoms = List.length atoms in 
	warn "load_core: 3";
	if natoms < 10 then
	  (fprintf stderr "load_core: %s too few atoms: %d\n" pdbid natoms; 
	   flush stderr)
	else
	  let asyms = Prep_struct.split_asyms dblk atoms in
	  warn "load_core: Prep_struct.split_asyms";
	  let protein = List.filter 
	      (fun (a,l) -> PDBjUtil.asym_is_protein dblk a) asyms in
	  let pdbid = PDBjUtil.pdbid dblk in
	  let pep,nuc,oth,nonpoly = Cmp_obj.identify_ligands dblk asyms in
	  warn "load_core: Cmp_obj.identify_ligands";
	  let ppis = Cmp_obj.identify_ppi dblk asyms in
	  warn "load_core: Cmp_obj.identify_ppi";
	  let refsets = Refset.make_proteins protein in
	  warn "load_core: Refset.make_proteins";
	  let lifs = List.filter (fun (i,c) -> List.mem i itype)
	      [(PPI,ppis); (PEPTIDE,pep); (DNARNA,nuc); 
	       (OPOLY,oth); (NONPOLYMER,nonpoly)] in
	  let interfaces = Interface.find_all pdbid assembly_id lifs in
	  warn "Annot.load_assembly";
	  Annot.load_assembly conn dblk pdbid assembly_id asyms;
	  warn "Interface.load";
	  Interface.load conn pdbid assembly_id interfaces;
	  warn "Patom.load";
	  Patom.load conn interfaces refsets;
	  (***** fold *****)
(*
	  warn "load_core: folds...";
	  let folds = Cmp_obj.identify_folds dblk asyms in
	  warn (sprintf "identify_folds: %d" (List.length folds));
	  let frefsets = List.map (fun (r,l) ->
	    let asym,atoms = List.hd r in
	    let atoms,rs = Refset.refset_of_fold atoms in
	    ([(asym,atoms)],l),rs) folds in
	  let fold,frefsets = List.split frefsets in
	  let frefsets = List.concat frefsets in
	  let folds = Interface.find_all pdbid assembly_id [(FOLD,fold)] in
	  Interface.load conn pdbid assembly_id folds;
	  Patom.load conn folds frefsets
*)
    in
    Sql.transaction conn 
      (fun () ->
	warn "Annot.load_base";
	Annot.load_base conn mtime dblk pdbid;
	Enum.iter proc_bu (Prep_struct.gen_biounit dblk)
      )

let load conn itype pdbid =
  let pdbmldir = get_pdbmldir () in
  let fname = Filename.concat pdbmldir (pdbid ^ ".xml.gz") in
  if Sys.file_exists fname then 
    let s = Unix.stat fname in
    if s.Unix.st_size > file_size_limit then 
      (fprintf stderr "Load_data.load (%s): too big:  %d bytes\n" 
	 pdbid s.Unix.st_size; flush stderr)
    else
      let mtime = string_of_float s.Unix.st_mtime in
      let dblk = PDBML.parse_file fname in
      let dblk = Prep_struct.clean dblk in
      Gc.compact();
      begin
	try 
	  fprintf stderr "Load_data.load: %s\n" pdbid; flush stderr;
	  load_core conn mtime itype dblk
	with 
	| PG.Error e -> 
	    fprintf stderr "Load_data.load (%s): %s\n"
	      pdbid (PG.string_of_error e);
	    flush stderr;
	| exc ->
	    fprintf stderr "Load_data.load (%s): failed %s\n" 
	      pdbid  (Printexc.to_string exc); flush stderr
      end;
  else
    begin
      Printf.fprintf stderr "Load_data.load:  %s does not exist.\n" pdbid;
      flush stderr
    end

