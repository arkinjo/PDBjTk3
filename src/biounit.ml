(*
  generation of biological units based on 
  - pdbx_struct_assembly
  - pdbx_struct_assembly_gen
  - pdbx_struct_oper_list
*)

open ExtLib
open Printf
open PDBjBasis
module Ht = Hashtbl

type unit = {
    id: string;
    count: int;
    details: string;
    oligo_details: string;
  }

type gen = {
    assembly_id: string;
    asym_id: string list;
    oper: string list list;
  }

type oper = {
    oid: string;
    m11: float; m12: float; m13: float;
    m21: float; m22: float; m23: float;
    m31: float; m32: float; m33: float;
    v1: float; v2: float; v3: float;
    name: string;
    symm: string;
    otype: string;
  }

let epsilon = epsilon_float *. 2.0
let oper_is_identity op =
  op.otype = "identity operation" ||
  abs_float(op.m11 -. 1.0) < epsilon && 
  abs_float(op.m12) < epsilon && 
  abs_float(op.m13) < epsilon && 
  abs_float(op.m21) < epsilon && 
  abs_float(op.m22 -. 1.0) < epsilon && 
  abs_float(op.m23) < epsilon && 
  abs_float(op.m31) < epsilon && 
  abs_float(op.m32) < epsilon && 
  abs_float(op.m33 -. 1.0) < epsilon && 
  abs_float(op.v1) < epsilon &&
  abs_float(op.v2) < epsilon &&
  abs_float(op.v3) < epsilon

let get_units datablock =
  List.map (fun a -> 
    {id = Option.get a.Pdbx_struct_assembly.id;
     count = Option.default (-1) a.Pdbx_struct_assembly.oligomeric_count;
     details = Option.default "" a.Pdbx_struct_assembly.details;
     oligo_details = Option.default "" 
       a.Pdbx_struct_assembly.oligomeric_details;})
    datablock.Datablock.pdbx_struct_assembly

let handle_oper_list l =
  let pr = Str.regexp "[()]+" in
  let sp = Str.regexp "[,]" in
  let ch1 = List.filter (fun a -> a <> "") (Str.split pr l) in
  let ch = List.rev_map (fun s -> Str.split sp s) ch1 in
  let fun1 ch =
    let ch = List.map (fun s ->
      if String.exists s "-" 
      then 
	let b,e = String.split s "-" in
	let b = int_of_string b and e = int_of_string e in
	let arr = Array.init (e - b + 1) (fun i -> i + b) in
	List.map string_of_int (Array.to_list arr)
      else [s]) ch in
    List.concat ch
  in
  List.map fun1 ch
  
let get_gen datablock =
  List.map 
    (fun a -> 
      {assembly_id = Option.get a.Pdbx_struct_assembly_gen.assembly_id;
       asym_id = String.nsplit 
	 (Option.get a.Pdbx_struct_assembly_gen.asym_id_list) ",";
       oper = handle_oper_list 
	 (Option.get a.Pdbx_struct_assembly_gen.oper_expression)})
    datablock.Datablock.pdbx_struct_assembly_gen

let get_oper datablock =
  List.map 
    (fun a ->
      {oid = (try Option.get a.Pdbx_struct_oper_list.id
	  with Option.No_value -> failwith "Biounit.get_oper: no id.");
       m11 = Option.default 1.0 a.Pdbx_struct_oper_list.matrix11;
       m12 = Option.default 0.0 a.Pdbx_struct_oper_list.matrix12;
       m13 = Option.default 0.0 a.Pdbx_struct_oper_list.matrix13;
       m21 = Option.default 0.0 a.Pdbx_struct_oper_list.matrix21;
       m22 = Option.default 1.0 a.Pdbx_struct_oper_list.matrix22;
       m23 = Option.default 0.0 a.Pdbx_struct_oper_list.matrix23;
       m31 = Option.default 0.0 a.Pdbx_struct_oper_list.matrix31;
       m32 = Option.default 0.0 a.Pdbx_struct_oper_list.matrix32;
       m33 = Option.default 1.0  a.Pdbx_struct_oper_list.matrix33;
       v1 = Option.default 0.0 a.Pdbx_struct_oper_list.vector1;
       v2 = Option.default 0.0 a.Pdbx_struct_oper_list.vector2;
       v3 = Option.default 0.0 a.Pdbx_struct_oper_list.vector3;
       name = Option.default "" a.Pdbx_struct_oper_list.name;
       symm = Option.default "" a.Pdbx_struct_oper_list.symmetry_operation;
       otype = (try Option.get a.Pdbx_struct_oper_list.type_ 
       with Option.No_value -> failwith "Biounit.get_oper: no type.")})
    datablock.Datablock.pdbx_struct_oper_list

let apply_oper opers atoms oid =
  let op = List.find (fun o -> o.oid = oid) opers in
  List.map (fun a ->
    {a with 
     Atom.label_asym_id = a.Atom.label_asym_id ^ "-" ^ oid;
     auth_asym_id = a.Atom.auth_asym_id ^ "-" ^ oid;
     x = op.m11 *. a.Atom.x +. op.m12 *. a.Atom.y +. op.m13 *. a.Atom.z
       +. op.v1;
     y = op.m21 *. a.Atom.x +. op.m22 *. a.Atom.y +. op.m23 *. a.Atom.z
       +. op.v2;
     z = op.m31 *. a.Atom.x +. op.m32 *. a.Atom.y +. op.m33 *. a.Atom.z
       +. op.v3;})
    atoms

let apply_gen atoms opers gen =
  let atoms = 
    List.filter (fun a -> List.mem a.Atom.label_asym_id gen.asym_id)
      atoms in
  let apply1 atoms oper1 =
    List.concat (List.map (apply_oper opers atoms) oper1) in
  List.fold_left apply1 atoms gen.oper

let sort_atoms atoms =
  let ht = Ht.create 10000 in
  let ind = ref 0 in
  let l = ref [] in
  List.iter (fun a -> 
    let key = a.Atom.atom_site_id, a.Atom.label_asym_id in
    if Ht.mem ht key then ()
    else (incr ind; 
	  Ht.add ht key ();
	  l := {a with Atom.atom_site_id = string_of_int !ind}:: !l))
    atoms;
  List.rev !l
      
let gen_unit atoms gens opers u =
  let gens = List.filter (fun g -> g.assembly_id = u.id) gens in
  u.id, sort_atoms(List.concat (List.map (apply_gen atoms opers) gens))

let generate datablock atoms =
  let units = get_units datablock in
  let gens = get_gen datablock in
  let opers = get_oper datablock in
  List.map (gen_unit atoms gens opers) units
  
let generate1 datablock atoms =
  let gens = get_gen datablock in
  let opers = get_oper datablock in
  fun u -> gen_unit atoms gens opers u

let get_struct_assembly dblk uid =
  let assembly = List.filter (fun u -> u.Pdbx_struct_assembly.id = Some uid) dblk.Datablock.pdbx_struct_assembly in
  let gen = List.filter (fun u -> u.Pdbx_struct_assembly_gen.assembly_id = Some uid) dblk.Datablock.pdbx_struct_assembly_gen in
  {dblk with
   Datablock.pdbx_struct_assembly = assembly;
   pdbx_struct_assembly_gen = gen;}
   
    
(** assign entity_id to generated asyms *)
let get_struct_asym dblk atoms =
  let ht = Ht.create 1 in
  List.iter (fun a -> Ht.replace ht a.Atom.label_asym_id ()) atoms;
  let l = Ht.fold (fun k _ l -> k::l) ht [] in
  let l = List.sort ~cmp:String.compare l in
  let base a = if String.exists a "-" then fst(String.split a "-") else a in
  let sasyms = Ht.create 1 in
  List.iter (fun a -> Ht.replace sasyms (Option.get a.Struct_asym.id) a) 
    dblk.Datablock.struct_asym;
  List.map 
    (fun a -> 
      let s = Ht.find sasyms (base a) in
      {s with Struct_asym.id = Some a})
    l

let remove_absent_entities dblk atoms =
  let ht = Ht.create 1 in
  List.iter (fun a -> Ht.replace ht (Some a.Atom.label_entity_id) ()) atoms;
  let entity = List.filter
      (fun e -> Ht.exists ht e.Entity.id) dblk.Datablock.entity in
  let poly = List.filter
      (fun e -> Ht.exists ht e.Entity_poly.entity_id) dblk.Datablock.entity_poly in
  let polyseq = List.filter
      (fun e -> Ht.exists ht e.Entity_poly_seq.entity_id) dblk.Datablock.entity_poly_seq in
  let polyseqscheme = List.filter
      (fun e -> Ht.exists ht e.Pdbx_poly_seq_scheme.entity_id) dblk.Datablock.pdbx_poly_seq_scheme in
  let nonpoly = List.filter
      (fun e -> Ht.exists ht e.Pdbx_entity_nonpoly.entity_id) dblk.Datablock.pdbx_entity_nonpoly in
  let nonpolyscheme = List.filter
      (fun e -> Ht.exists ht e.Pdbx_nonpoly_scheme.entity_id) dblk.Datablock.pdbx_nonpoly_scheme in
  let srcgen = List.filter
      (fun e -> Ht.exists ht e.Entity_src_gen.entity_id) dblk.Datablock.entity_src_gen in
  let srcnat = List.filter
      (fun e -> Ht.exists ht e.Entity_src_nat.entity_id) dblk.Datablock.entity_src_nat in
  {dblk with Datablock.entity = entity; entity_poly = poly; 
   entity_poly_seq = polyseq;
   pdbx_poly_seq_scheme = polyseqscheme;
   pdbx_entity_nonpoly = nonpoly;
   pdbx_nonpoly_scheme = nonpolyscheme;
   entity_src_gen = srcgen;
   entity_src_nat = srcnat; }

(** for connections generated by symmetry operations *)  
let get_struct_conn dblk atoms =
  let conn = dblk.Datablock.struct_conn in
  let oper = get_oper dblk in
  let suff op s = s ^ "-" ^ op.oid in
  let ht = Ht.create 1 in
  List.iter (fun a -> Ht.replace ht (Some a.Atom.label_asym_id) ()) atoms;
  List.filter_map 
    (fun c ->
      try
	let symm1 = Option.get c.Struct_conn.ptnr1_symmetry in
	let symm2 = Option.get c.Struct_conn.ptnr2_symmetry in
	let op1 = List.find (fun o -> o.name = symm1) oper in
	let op2 = List.find (fun o -> o.name = symm2) oper in
	let la1 = Option.map (suff op1) c.Struct_conn.ptnr1_label_asym_id in
	let la2 = Option.map (suff op2) c.Struct_conn.ptnr2_label_asym_id in
	if (la1 = None || (Ht.mem ht la1)) && (la2 = None || (Ht.mem ht la2 ))
	then
	  let aa1 = Option.map (suff op1) c.Struct_conn.ptnr1_auth_asym_id in
	  let aa2 = Option.map (suff op2) c.Struct_conn.ptnr2_auth_asym_id in
	  
	  let id = 
	    Option.map (fun a -> suff op2 (suff op1 a)) c.Struct_conn.id in
	  Some {c with Struct_conn.id = id;
		ptnr1_auth_asym_id = aa1; ptnr2_auth_asym_id = aa2;
		ptnr1_label_asym_id = la1; ptnr2_label_asym_id = la2}
	else None
      with Not_found -> None)
      conn


let get_struct_conf dblk = 
  let conf = dblk.Datablock.struct_conf in
  let base a = if String.exists a "-" then fst(String.split a "-") else a in
  let sasyms = Ht.create 1 in
  List.iter 
    (fun a -> 
      let id = Option.get a.Struct_asym.id in
      Ht.add sasyms (base id) id) 
    dblk.Datablock.struct_asym;
  let add_suff a b = 
    if String.exists a "-" 
    then b ^ "-" ^ snd(String.split a "-") 
    else b in
  let rp c =
    let basym = Option.get c.Struct_conf.beg_label_asym_id in
    let easym = Option.get c.Struct_conf.end_label_asym_id in
    let id = Option.get c.Struct_conf.id in
    
    let basym2 = Option.get c.Struct_conf.beg_auth_asym_id in
    let easym2 = Option.get c.Struct_conf.end_auth_asym_id in
    
    let asyms = Ht.find_all sasyms basym in
    List.map (fun asym ->
      {c with Struct_conf.beg_label_asym_id = Some asym;
       end_label_asym_id = Some (add_suff asym easym);
       id = Some(add_suff asym id);
       beg_auth_asym_id = Some (add_suff asym basym2);
       end_auth_asym_id = Some (add_suff asym easym2);})
      asyms
  in
  let l = List.fold_left 
      (fun confs c -> List.rev_append (rp c) confs) [] conf in
  List.rev l

let get_struct_sheet_range dblk = 
  let conf = dblk.Datablock.struct_sheet_range in
  let base a = if String.exists a "-" then fst(String.split a "-") else a in
  let sasyms = Ht.create 1 in
  List.iter 
    (fun a -> 
      let id = Option.get a.Struct_asym.id in
      Ht.add sasyms (base id) id) 
    dblk.Datablock.struct_asym;
  let add_suff a b = 
    if String.exists a "-" 
    then b ^ "-" ^ snd(String.split a "-") 
    else b in
  let rp c =
    let basym = Option.get c.Struct_sheet_range.beg_label_asym_id in
    let easym = Option.get c.Struct_sheet_range.end_label_asym_id in
    let id = Option.get c.Struct_sheet_range.id in

    let basym2 = Option.get c.Struct_sheet_range.beg_auth_asym_id in
    let easym2 = Option.get c.Struct_sheet_range.end_auth_asym_id in
    
    let asyms = Ht.find_all sasyms basym in
    List.map
      (fun asym ->
	{c with Struct_sheet_range.beg_label_asym_id = Some asym;
	 id = Some(add_suff asym id);
	 end_label_asym_id = Some (add_suff asym easym);
	 beg_auth_asym_id = Some (add_suff asym basym2);
	 end_auth_asym_id = Some (add_suff asym easym2);})
      asyms
  in
  let l = List.fold_left 
      (fun confs c -> List.rev_append (rp c) confs) [] conf in
  List.rev l

  
