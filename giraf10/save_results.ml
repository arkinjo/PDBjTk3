(** 
    Saving search results on the client side.
    Alignments are saved on the server side.
    Summary and temperature-mapped query structure is saved here.
 *)

open PDBjTk
open PDBjBasis
open Util
open Printf
open Xml

let outdir_base () = 
  try Sys.getenv "GIRAF_SAVEDIR" 
  with Not_found -> Sys.getcwd ()

let mkoutdir ?(prefix="search") ?(base_dir=outdir_base()) query =
  let mk dir = 
    if Sys.file_exists dir && Sys.is_directory dir then ()
    else if Sys.file_exists dir && not (Sys.is_directory dir) then
      failwith (sprintf "Save_ali: %s is not a directory. aborting..." dir)
    else
      (Unix.mkdir dir 0o777;
       Unix.chmod dir 0o777)
  in
  mk base_dir;
  let dir = base_dir ^ "/dir.giraf_" ^ prefix ^ "." ^ query in
  mk dir;
  dir

let mk_infile_elem infile auth_asyms =
  let asyms = String.concat "," auth_asyms in
  let ft,file = 
    match infile with
    | PDBML f -> "pdbml",f
    | MMCIF f -> "mmcif",f
    | PDBF f -> "pdb",f
    | PDBID f -> "pdbid",f
    | Unknown f -> "unknown",f
  in
  Element("infile"
	    ,[("filetype",ft);
	      ("auth_asyms",if asyms = "" then "*" else asyms)]
	    ,[(PCData file)])

let map_frequency atoms orig_atoms results =
  let atoms = Array.of_list 
                (List.map (fun a -> {a with Atom.b_iso = 0.0}) atoms) in
  List.iter (fun r ->
    let s = r.Ir.score in
    List.iter (fun (i,_) ->
      let a = atoms.(i) in
      atoms.(i) <- {a with Atom.b_iso = a.Atom.b_iso +. s}) r.Ir.ali) results;
  let mf = Array.fold_left (fun mf a -> max mf a.Atom.b_iso) 0.1 atoms in
  Array.iteri (fun i a -> 
    atoms.(i) <- {a with Atom.b_iso = 100.0 *. a.Atom.b_iso /. mf})
    atoms;
  let ht = Ht.create 1 in
  Array.iter (fun a -> Ht.add ht a.Atom.atom_site_id a) atoms;
  List.map (fun a -> 
    let a = Ht.find_default ht a.Atom.atom_site_id a in
    {a with Atom.model_num=0}) orig_atoms

let save_query outdir query orig_atoms results  =
  let dblk = 
    {Datablock.default with
      Datablock.datablockName = "query";
      atom_site = List.map Atom.to_atom_site orig_atoms;
    } in
  let fbase = "query.cif" in
  let oc = open_out (Filename.concat outdir fbase) in
  MmCIF.print oc dblk;
  close_out oc


let chop_asym_id aid = try fst(String.split aid "-") with _ -> aid

let dump_minimum query_id results =
  List.iter 
    (fun (atoms,ir_result) ->
      let asym_id = (List.hd atoms).Atom.auth_asym_id in
      List.iter
	(fun r ->
	  printf "%s:%s,%s,%f,%f,%f,%f,%f,%f,%d,%d\n" 
	    query_id asym_id r.Ir.if_id r.Ir.score r.Ir.gi_score
		r.Ir.rms r.Ir.drms r.Ir.tani r.Ir.seqid
		(List.length r.Ir.ali) r.Ir.ngi)
	ir_result;
      flush stdout)
    results

let dump_alignment1 conn outdir atoms irank ir_result =
  let corr,twhole,iface = Ir.get_alignment conn atoms ir_result in
  let qasym = (List.hd atoms).Atom.label_asym_id in
  let dump_mmcif dblkName tatoms =
    let dblk =
      {Datablock.default with
        Datablock.datablockName = dblkName;
        atom_site = List.map Atom.to_atom_site tatoms} in
    let fbase = dblkName ^ ".cif" in
    let oc = open_out (Filename.concat outdir fbase) in
    MmCIF.print oc dblk;
    close_out oc
  in
  let tname = sprintf "whole_%s_%d" qasym (irank + 1) in
  dump_mmcif tname twhole;
  let tname = sprintf "if_%s_%d" qasym (irank + 1) in
  dump_mmcif tname (iface.Interface.atoms @ iface.Interface.latoms);
  ()
  
let summary_of_chain1 qasym_id summary blast_results ir_results = 
  List.mapi
    (fun irank r ->
      let annot = Ht.find_default summary r.Ir.if_id
	  Interface.default_summary in
      let pdbid = annot.Interface.s_pdbid in
      let asym_id = chop_asym_id annot.Interface.s_label_asym_id in
      let bl_seq_id,bl_eval = 
	Ht.find_default blast_results (qasym_id,pdbid,asym_id) (-1.0,10.0) in
      Element("interface",[("type",annot.Interface.s_type);
			   ("if_id",annot.Interface.s_if_id);
			   ("rank",string_of_int (succ irank))],
	      [Element("pdbid",[],[PCData annot.Interface.s_pdbid]);
	       Element("assembly_id",[],[PCData annot.Interface.s_assembly_id]);
	       Element("label_asym_id",[],[PCData annot.Interface.s_label_asym_id]);
	       Element("auth_asym_id",[],[PCData annot.Interface.s_auth_asym_id]);
	       Element("entity_id",[],[PCData annot.Interface.s_entity_id]);
	       Element("description",[],[PCData annot.Interface.s_description]);
	       Element("l_label_asym_id",[],[PCData annot.Interface.s_l_label_asym_id]);
	       Element("l_auth_asym_id",[],[PCData annot.Interface.s_l_auth_asym_id]);
	       Element("l_entity_id",[],[PCData annot.Interface.s_l_entity_id]);
	       Element("l_description",[],[PCData annot.Interface.s_l_description]);
	       Element("comp_id",[],[PCData annot.Interface.s_comp_id]);
	       Element("score",[],[PCData (string_of_float r.Ir.score)]);
	       Element("gi_score",[],[PCData (string_of_float r.Ir.gi_score)]);
	       Element("tanimoto_score",[],[PCData (string_of_float r.Ir.tani)]);
	       Element("nali",[],[PCData (string_of_int (List.length r.Ir.ali))]);
	       Element("nali_res",[],[PCData (string_of_int r.Ir.nali_res)]);
	       Element("blast_seq_id",[],[PCData (string_of_float bl_seq_id)]);
	       Element("blast_evalue",[],[PCData (string_of_float bl_eval)]);
	     ])
	)
    ir_results

let dump_results conninfo outdir query_id nshow orig_atoms 
    blast_results results =
  let outdir = mkoutdir ~base_dir:outdir query_id in
  fprintf stderr "outdir: %s\n" outdir;
  let conn = new Postgresql.connection ~conninfo () in
  let orig_atoms = List.fold_left
      (fun orig_atoms (atoms,ir_results) -> 
	map_frequency atoms orig_atoms ir_results) orig_atoms results in
  save_query outdir query_id orig_atoms results;
  let asym_hits =
    List.map
      (fun (atoms,ir_results) ->
	let asym_id = (List.hd atoms).Atom.auth_asym_id in
	let nhits = List.length ir_results in
	let ir_results = List.take nshow ir_results in
        (* *)
        List.iteri (dump_alignment1 conn outdir atoms) ir_results;
	let pdbset = List.map (fun r -> r.Ir.if_id) ir_results in
	let summary = Interface.get_summary conn pdbset in
	let hits = summary_of_chain1 asym_id summary blast_results ir_results in
	Element("chain",[("chain_id",asym_id);
			 ("nhits", string_of_int nhits)],
		hits))
    results in
  conn#finish;
  let doc = Element("giraf_search",[("query_id",query_id)],asym_hits) in
  print_endline (Xml.to_string_fmt doc);
  ()
    
