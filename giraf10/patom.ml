(** making pre-processed atomic coordinates *)

open Printf
open PDBjTk
open Util
let warn _ = ()


type aco = ((string * int * int * int) * (float * float * float)) list
(*
  (a.Atom.label_asym_id),(a.Atom.label_seq_id),
  (PDBjUtil.index_amino3 a.Atom.label_comp_id),
  (type_of_atom a)
*)

let aco_of_atom a = 
  ((a.Atom.label_asym_id),
   (a.Atom.label_seq_id),
   (PDBjUtil.index_amino3 a.Atom.label_comp_id),
   (type_of_atom a)),
  (Atom.coord a)

let prep_atoms ?refset atoms =
  List.map 
    (fun a -> 
      let tco = 
	match refset with
	| None -> Atom.coord a
	| Some r -> Refset.transform_point r.Refset.frame (Atom.coord a) in
      let t = type_of_atom a in
      let atype_id = get_atype_id t in
      let aa = PDBjUtil.index_amino3 a.Atom.label_comp_id in
      let ch = a.Atom.label_asym_id in
      let sid = a.Atom.label_seq_id in
      (ch,sid,aa,atype_id),tco)
    atoms

let load (conn: PG.connection) interfaces refsets =
  let rec insloop table i n = function
    | [] -> ()
    | l ->
	let rs = List.take n l in
	let rest = List.drop n l in
	try
	  let itype = string_of_itype i.Interface.itype in
	  let params,vals = 
	    Refset.prep_sql_save_data conn 
	      ~if_id:i.Interface.if_id itype i.Interface.atoms rs in
	  let exp = 
	    sprintf "INSERT INTO %s(%s) VALUES %s" 
	      table Refset.sql_columns vals in
	  Sql.command conn ~params exp;
	  insloop table i n rest
	with
	| PG.Error e ->
	    fprintf stderr "PG.Error %s\n" (PG.string_of_error e);
	    raise (PG.Error e)
	| exc -> 
	    fprintf stderr "Patom.load: %s\n" (Printexc.to_string exc);
	    flush stderr;
	    insloop table i (max 1 (n/2)) l
  in
  List.iter 
    (fun i ->
      let itype = string_of_itype i.Interface.itype in
      let table = "refaco_"^itype in
      let rs = Refset.atoms_filter i.Interface.atoms refsets in
      warn (sprintf "Patom.load: %s - %s:%s rs=%d\n" itype 
	      i.Interface.receptor
	      i.Interface.ligand
	      (List.length rs));
      insloop table i 100 rs)
    interfaces
