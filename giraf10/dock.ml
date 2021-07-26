open PDBjTk
open PDBjBasis
open Util
open Printf
open Xml

let xml_of_ir_result1 id r =
  let sof s = sprintf "%.3f" s in
  let soi = string_of_int in
  let qali,tali = List.split r.Ir.ali in
  Element("hit",[("id",soi (id+1));
		 ("if_id",r.Ir.if_id);],
	  [Element("score",[],[PCData (sof r.Ir.score)]);
	   Element("gi_score",[],[PCData (sof r.Ir.gi_score)]);
	   Element("nali",[],[PCData (soi (List.length r.Ir.ali))]);
	   Element("rms",[],[PCData (sof r.Ir.rms)]);
	   Element("qali",[],[PCData (String.concat "," (List.map soi qali))]);
	   Element("tali",[],[PCData (String.concat "," (List.map soi tali))]);]
	 )

let xml_of_ir_result2 id (r1,r2) =
  let x1 = 
    match xml_of_ir_result1 id r1 with
    | Element("hit",atts,elms) -> Element("hit1",atts,elms)
    | _ -> failwith "Dock.xml_of_ir_result1: cannot happen."
  in
  let x2 = 
    match xml_of_ir_result1 id r2 with
    | Element("hit",atts,elms) -> Element("hit2",atts,elms)
    | _ -> failwith "Dock.xml_of_ir_result1: cannot happen."
  in
  Element("hit", [("total_score", sprintf "%g" (r1.Ir.score +. r2.Ir.score))],
	  [x1; x2])

let summary_of_ir_result1 summary ind (r1,r2) =
  let annot = Ht.find summary r1.Ir.if_id in
  let hit = xml_of_ir_result2 ind (r1,r2) in
  Element("hit_summary",
	  [("id", string_of_int (ind+1))],
	  [Element("pdbid", [], 
		   [PCData (String.uppercase annot.Interface.s_pdbid)]);
	   Element("title",[], [PCData annot.Interface.s_title]);
	   Element("descriptor",[],[PCData annot.Interface.s_descriptor]);
	   Element("receptor",
		   [("label_asym_id",annot.Interface.s_label_asym_id);
		    ("auth_asym_id", annot.Interface.s_auth_asym_id)],
		   [PCData annot.Interface.s_description]);
	   Element("ligand",
		   [("label_asym_id",annot.Interface.s_l_label_asym_id);
		    ("auth_asym_id", annot.Interface.s_l_auth_asym_id)],
		   [PCData annot.Interface.s_l_description]);
	   hit])

let summary_of_results summary query_id results =
  let nhits = string_of_int (List.length results) in
  Element("giraf_dock_summary",
	  [("query",query_id);
	   ("nhits",nhits)],
	  List.mapi (summary_of_ir_result1 summary) results)

let mkoutdir query =
  let dir = "dir.giraf_dock." ^ query in
  if Sys.file_exists dir && Sys.is_directory dir then ()
  else if Sys.file_exists dir && not (Sys.is_directory dir) then
    failwith (sprintf "Dock: %s is not a directory. aborting..." dir)
  else
    Unix.mkdir dir 0o755;
  dir

let fit_to_original1 oatoms1 atoms1 atoms2 =
  let amol = List.map Atom.coord oatoms1 in
  let bmol = List.map Atom.coord atoms1 in
  let amol = Array.of_list amol and bmol = Array.of_list bmol in
  let _,qrot,cma,cmb = Vec3.crms_mat amol bmol in
  let move a = 
    let co = Atom.coord a in
    let x,y,z = Vec3.add cma (Vec3.qrotate qrot (Vec3.subtract co cmb)) in
    {a with Atom.x = x; y = y; z = z}
  in
  let atoms2 = List.map move atoms2 in
  atoms2

let save_model conn query atoms1 atoms2 ind (res1,res2) =
  let dir = mkoutdir query in
  let ali1,atoms1',iface1 = 
    Ir.get_alignment ~inv:true conn atoms1 res1 in
  let ali2,atoms2',iface2 = 
    Ir.get_alignment ~inv:true conn atoms2 res2 in
  let atoms2 = fit_to_original1 atoms1 atoms1' atoms2' in
  let combo = List.map Atom.to_atom_site (atoms1 @ atoms2) in
  let dblk = {Datablock.default with Datablock.atom_siteCategory = combo} in
  let doc = PDBML.xml_of_datablock dblk in
  let fbase = query ^ "_" ^ (string_of_int (succ ind)) ^ ".xml" in
  let oc = open_out (Filename.concat dir fbase) in
  fprintf oc "%s\n" (Xml.to_string_fmt doc);
  close_out oc

