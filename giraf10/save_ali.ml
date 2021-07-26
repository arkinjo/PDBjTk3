open PDBjTk
open PDBjBasis
open Util
open Printf
open Xml

let outdir_base () = 
  try Sys.getenv "GIRAF_SAVEDIR" 
  with Not_found -> Sys.getcwd ()

let jv_select_atom a =
  sprintf "[%s]%s:%s.%s/%d" a.Atom.auth_comp_id a.Atom.auth_seq_id 
    a.Atom.auth_asym_id a.Atom.auth_atom_id a.Atom.model_num

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

let mk_jv_command name comm doc =
  {JV_command.name = Some name; command = Some comm; document = Some doc}

let mkoutdir ?(prefix="search") ?(base_dir=outdir_base()) query =
  let dir = base_dir ^ "/dir.giraf_" ^ prefix ^ "." ^ query in
  if Sys.file_exists dir && Sys.is_directory dir then ()
  else if Sys.file_exists dir && not (Sys.is_directory dir) then
    failwith (sprintf "Save_ali: %s is not a directory. aborting..." dir)
  else
    (Unix.mkdir dir 0o777;
     Unix.chmod dir 0o777);
  dir

let save_alignment1 conn atoms dir result = 
  let ind = result.Ir.rank in
  let corr,twhole,iface = Ir.get_alignment conn atoms result in
  let qa,ta = List.split corr in
  let qa = List.map (fun a -> {a with Atom.model_num = 0}) qa in
  let ta = List.map (fun a -> {a with Atom.model_num = ind}) ta in
  let twhole = List.map (fun a -> {a with Atom.model_num = ind}) twhole in
  let ratoms = List.map (fun a -> {a with Atom.model_num = ind})
      iface.Interface.atoms in
  let latoms = List.map (fun a -> {a with Atom.model_num = ind})
      iface.Interface.latoms in
  let combo = List.map Atom.to_atom_site (twhole @ latoms) in
  let pdbid = fst(String.split result.Ir.if_id ":") in
  let qaligned = List.map jv_select_atom qa in
  let taligned = List.map jv_select_atom ta in
  let alignment = List.combine qaligned taligned in
  let qselect = "select (" 
    ^ String.concat " | " qaligned  ^ ")" in
  let tselect = "select (" 
    ^ String.concat " | " taligned ^ ")" in
  let qselect_all = "select ("
      ^ String.concat " | " (List.map jv_select_atom atoms) ^ ")" in
  let lselect = "select (" 
    ^ String.concat " | " (List.map jv_select_atom latoms) ^ ")" in
  let tselect_all = "select (" 
    ^ String.concat " | " (List.map jv_select_atom ratoms) ^ ")" in
  let qcomm = mk_jv_command (sprintf "qselect%d" ind) qselect 
      (sprintf "select query atoms aligned to the template %d." ind) in
  let tcomm = mk_jv_command (sprintf "tselect%d" ind) tselect 
      (sprintf "select template %d atoms aligned to the query." ind) in
  let qcomm_all = mk_jv_command (sprintf "qselect_all%d" ind) qselect_all
      "select all query (interface) atoms" in
  let lcomm = mk_jv_command (sprintf "lselect%d" ind) lselect
      (sprintf "select ligand in template %d." ind) in
  let tcomm_all = mk_jv_command (sprintf "tselect_all%d" ind) tselect_all 
      "select all template atoms." in
  let jvcom = [lcomm; tcomm_all; qcomm_all; tcomm; qcomm] in
  let dblk = 
    {Datablock.default with Datablock.atom_siteCategory = combo;
     entryCategory = [{Entry.default with Entry.id = Some pdbid}];
     jV_commandCategory = jvcom} in
  let doc = PDBML.xml_of_datablock dblk in
  let q_auth_asym = (List.hd atoms).Atom.auth_asym_id in
  let fbase = sprintf "atoms-%s-%s-%d.xml" 
      q_auth_asym (string_of_itype result.Ir.itype) ind in
  let oc = open_out (Filename.concat dir fbase) in
  fprintf oc "%s\n" (Xml.to_string_fmt doc);
  close_out oc;
  jvcom,alignment

let summary_of_interface1 annot res =
  Element("interface",
	  [("if_id",res.Ir.if_id);
	   ("type", string_of_itype res.Ir.itype)],
	  [Element("l_entity_id",[],
		   [PCData annot.Interface.s_l_entity_id]);
	   Element("l_label_asym_id",[],
		   [PCData annot.Interface.s_l_label_asym_id]);
	   Element("l_auth_asym_id",[],
		   [PCData annot.Interface.s_l_auth_asym_id]);
	   Element("l_description",[],[PCData annot.Interface.s_l_description]);
	   Element("l_comp_id",[],[PCData annot.Interface.s_comp_id]);
	   Element("score",[],[PCData (string_of_float res.Ir.score)]);
	   Element("rms",[],[PCData (string_of_float res.Ir.rms)]);
	   Element("rank",[],[PCData (string_of_int res.Ir.rank)]);
	   Element("nali",[],[PCData (string_of_int (List.length res.Ir.ali))]);
	   Element("site_seqid",[],[PCData (sprintf "%.1f" res.Ir.seqid)]);])


(** save summary of a hit in a file *)
let summary_of_ir_result1 dir query_id atoms annot alignment jvcom r =
  let tcom = Option.get (List.nth jvcom 3).JV_command.command in
  let qcom = Option.get (List.nth jvcom 4).JV_command.command in
  let ali = Element("alignment",[],
		    List.mapi 
		      (fun i (q,t) -> 
			Element("ali_site",[("i", string_of_int i)],
				[Element("q",[],[PCData q]);
				 Element("t",[],[PCData t])]))
		      alignment) in
  let epdb = summary_of_interface1 annot r in
  let ejv = Element("jV_command",[],
	     [Element("qselect",[],[PCData qcom]);
	      Element("tselect",[],[PCData tcom])]) in
  let elems = [epdb; ejv; ali] in
  let pdbid,buid = 
    let pdbidb = annot.Interface.s_pdbid in
    if String.length pdbidb > 4 
    then (String.sub pdbidb 0 4),(String.slice ~first:4 pdbidb)
    else pdbidb,"" in
  let head = [Element("pdbid",[],[PCData pdbid]);
	      Element("biological_unit",[],
		      if buid = "" then [] else [PCData buid]);
	      Element("title",[],[PCData annot.Interface.s_title]);
	      Element("description",[],[PCData annot.Interface.s_description]);
	      Element("label_asym_id",[],
		      [PCData annot.Interface.s_label_asym_id]);
	      Element("auth_asym_id",[],
		      [PCData annot.Interface.s_auth_asym_id]);] in
  let q_auth_asym = (List.hd atoms).Atom.auth_asym_id in
  let doc = Element("giraf_align", 
		    [("query",query_id);("chain_id",q_auth_asym);
		     ("rank", (string_of_int r.Ir.rank))],
		    head @ elems) in
  let fbase = sprintf "sum-%s-%s-%d.xml" 
      q_auth_asym (string_of_itype r.Ir.itype) r.Ir.rank in
  let fout = Filename.concat dir fbase in
  let oc = open_out fout in
  fprintf oc "%s\n" (Xml.to_string_fmt doc);
  close_out oc


(** executed in server *)
let dump conn outdir query atoms nali results = 
  let pdbset = List.map (fun r -> r.Ir.if_id) results in
  let summary = Interface.get_summary conn pdbset in
  let ht = Ht.create 1 in
  let commands = ref [] in
  List.iter (fun res ->
    let key = res.Ir.if_id in
    if Ht.mem summary key then 
      let sum  = Ht.find summary key in
      Ht.add ht key sum;
      if res.Ir.rank <= nali then
	let com,ali = save_alignment1 conn atoms outdir res in
	summary_of_ir_result1 outdir query atoms sum ali com res;
	commands := List.rev_append com !commands
      else()
    else ())
    results;
  !commands,ht

