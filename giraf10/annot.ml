(** get annotations from PDBML *)
open PDBjTk
open PDBjBasis
open Util
open Printf

let save_atom_record = true

type entity = { 
    e_entity_id: string;
    e_type: string;
    e_description: string;
    e_comp_id: string;
    e_uniprot_acc: string;
  }

exception Big_file of string

let get_struct_ref dblk = 
  let ht = Ht.create 1 in
  List.iter (fun s ->
    match s.Struct_ref.db_name with
    | Some "UNP" -> 
	let entity_id = Option.default "0" s.Struct_ref.entity_id in
	(match s.Struct_ref.pdbx_db_accession with
	| Some acc -> Ht.add ht entity_id ("'" ^ acc ^ "'")
	| None -> ())
    | _ -> ()) dblk.Datablock.struct_ref;
  ht

let entity_summary dblk =
  List.filter_map (fun e ->
    let entity_id = Option.default "0" e.Entity.id in
    let unp_acc = get_struct_ref dblk in
    match e.Entity.type_ with
    | None -> None
    | Some t when t = "polymer" ->
	let p = 
	  try 
	    PDBjUtil.find_poly_of_entity dblk entity_id 
	  with Not_found -> Entity_poly.default 
	in
	Some {e_entity_id = entity_id;
	      e_type = t;
	      e_description = Option.default "(N/A)" e.Entity.pdbx_description;
	      e_comp_id = 
	      Option.default "unknown-polymer" p.Entity_poly.type_;
	      e_uniprot_acc = Ht.find_default unp_acc entity_id "NULL";}
    | Some t when t = "non-polymer" ->
	let p = 
	  try 
	    PDBjUtil.find_nonpoly_of_entity dblk entity_id 
	  with Not_found -> Pdbx_entity_nonpoly.default
	in
	Some {e_entity_id = entity_id;
	      e_type = t;
	      e_description = Option.default "(N/A)" e.Entity.pdbx_description;
	      e_comp_id = Option.default "unknown-non-polymer" 
		p.Pdbx_entity_nonpoly.comp_id;
	      e_uniprot_acc = "NULL"; }
    | Some _ -> None)
    dblk.Datablock.entity

let asym_entity_pair dblk asyms = 
  let pairs = List.map (fun (asym_id,l) -> 
    let a = List.hd l in
    asym_id,a.Atom.auth_asym_id,PDBjUtil.entity_id_of_asym_id dblk asym_id)
      asyms in
  pairs

    
let sql_insert_of_structs (conn: PG.connection) mtime dblk pdbid =
  let s = List.hd dblk.Datablock.struct_ in
  let release_date = PDBjUtil.last_release_date dblk in
  let title= (*conn#escape_string *) (Option.default "No title" s.Struct.title) in
  let descriptor = (*conn#escape_string*) (Option.default "No descriptor" 
					 s.Struct.pdbx_descriptor) in
  let h = "INSERT INTO Structs(pdbid,title,descriptor,atom_record,reldate,mtime) VALUES" in
  try
    let estr =  
      if save_atom_record then
	let pxml =  PDBML.xml_of_datablock dblk in
	let mstr =  Xml.to_string pxml in
	mstr
      else "" in
    let q =  h ^ "($1,$2,$3,$4,$5,$6)" in
    Sql.command conn
      ~params:[|pdbid; title; descriptor; estr; release_date; mtime|] q
  with
  | PG.Error e -> failwith ("sql_insert_of_structs: " ^ (PG.string_of_error e))
  | _ -> raise (Big_file pdbid)

let sql_insert_of_entities (conn: PG.connection) dblk pdbid =
  let entities = entity_summary dblk in
  let vals = List.map 
      (fun e ->
	sprintf "('%s',%s,'%s',E'%s','%s',%s)" 
	  pdbid e.e_entity_id e.e_type 
	  (conn#escape_string e.e_description)
	  e.e_comp_id e.e_uniprot_acc)
      entities in
  let vstr = String.concat "," vals in
  try
    Sql.command conn
      ("INSERT INTO Entities(pdbid,entity_id,type,description,comp_id,uniprot_acc) VALUES "
       ^ vstr)
  with
  | PG.Error e -> failwith ("sql_insert_of_entities: " ^ (PG.string_of_error e))
  | exc -> raise exc

let sql_insert_of_assembly conn dblk pdbid assembly_id =
  try 
    let ass = List.find (fun u -> 
      u.Pdbx_struct_assembly.id = Some assembly_id) 
	dblk.Datablock.pdbx_struct_assembly in
    let details = Option.default PG.null ass.Pdbx_struct_assembly.details in
    let method_details = Option.default PG.null ass.Pdbx_struct_assembly.method_details in
    let count = string_of_int (Option.default (-1) ass.Pdbx_struct_assembly.oligomeric_count) in
    let params = [|pdbid; assembly_id; details; method_details; count|] in
    Sql.command ~params conn 
      "INSERT INTO Assemblies(pdbid,assembly_id,details,method_details,oligomeric_count) VALUES ($1,$2,$3,$4,$5)"
  with Not_found ->
    Sql.command conn 
      (sprintf "INSERT INTO Assemblies(pdbid,assembly_id) VALUES ('%s','%s')" pdbid assembly_id)


let sql_insert_of_asyms conn dblk pdbid assembly_id asyms =
  let aes = asym_entity_pair dblk asyms in
  let vals = List.map (fun (l,a,e) ->
    sprintf "('%s','%s','%s','%s','%s')" pdbid assembly_id l a e) aes 
  in
  let vstr = String.concat "," vals in
  Sql.command conn
    ("INSERT INTO Asyms(pdbid,assembly_id,label_asym_id,auth_asym_id,entity_id) VALUES "
     ^ vstr)

let load_base conn mtime dblk pdbid =
  begin
    sql_insert_of_structs conn mtime dblk pdbid;
    sql_insert_of_entities conn dblk pdbid;
  end

let load_assembly conn dblk pdbid assembly_id asyms =
  begin
    sql_insert_of_assembly conn dblk pdbid assembly_id;
    sql_insert_of_asyms conn dblk pdbid assembly_id asyms
  end

