open PDBjBasis
open ExtLib

module Ht = Hashtbl

let std_amino3 = ["ALA"; "CYS"; "ASP"; "GLU"; "PHE"; 
		  "GLY"; "HIS"; "ILE"; "LYS"; "LEU";
		  "MET"; "ASN"; "PRO"; "GLN"; "ARG"; 
		  "SER"; "THR"; "VAL"; "TRP"; "TYR"]
let std_dna = ["DA"; "DT"; "DG"; "DC"]
let std_rna = ["A"; "T"; "G"; "C"]

exception UnknownFormat of string

let parse_file file =
  try MmCIF.parse_file file
  with _ -> 
    try PDBML.parse_file file
    with _ -> 
      try PDBFormat.parse_file file
      with _ -> raise (UnknownFormat file)

let atoms_of_datablock dblk =
  List.map Atom.of_atom_site dblk.Datablock.atom_site

let get_model atoms model_num =
  List.filter (fun a -> a.Atom.model_num = model_num) atoms

let get_asym atoms model_num label_asym_id =
  List.filter (fun a -> 
    a.Atom.model_num = model_num && a.Atom.label_asym_id = label_asym_id) 
    atoms

let get_auth_asym atoms model_num auth_asym_id =
  List.filter (fun a -> 
    a.Atom.model_num = model_num && a.Atom.auth_asym_id = auth_asym_id)
    atoms

let get_comp_of_atom atoms atom =
  List.filter (fun a -> 
    a.Atom.model_num = atom.Atom.model_num
      && a.Atom.label_asym_id = atom.Atom.label_asym_id
      && a.Atom.label_seq_id = atom.Atom.label_seq_id) atoms

let get_standardized_modres datablock atom =
  try
    if List.mem atom.Atom.label_comp_id std_amino3 then
      atom.Atom.label_comp_id
    else
      let conn = List.find 
	  (fun c -> 
	    match c.Struct_conn.conn_type_id with
	    | None -> false
	    | Some "modres" -> 
		atom.Atom.label_asym_id = 
		Option.get c.Struct_conn.ptnr1_label_asym_id 
		  && atom.Atom.label_seq_id =
		Option.get c.Struct_conn.ptnr1_label_seq_id 
		  && atom.Atom.label_comp_id =
		Option.get c.Struct_conn.ptnr1_label_comp_id
	    | Some _ -> false)
	  datablock.Datablock.struct_conn 
      in
      Option.get conn.Struct_conn.pdbx_ptnr1_standard_comp_id
  with _ -> atom.Atom.label_comp_id

let split_atoms2asyms atoms =
  let ht = Ht.create 1 in
  List.iter (fun a ->
    let asym = a.Atom.label_asym_id in
    let l = Ht.find_default ht asym [] in
    Ht.replace ht asym (a::l)) atoms;
  let latoms = Ht.fold (fun asym l nl -> (asym, List.rev l) :: nl) ht [] in
  List.sort ~cmp:(fun (a,_) (b,_) -> String.compare a b) latoms

let entity_id_of_asym_id dblk asym_id =
  let id = Some asym_id in
  let sa = List.find 
      (fun sa -> sa.Struct_asym.id = id)
      dblk.Datablock.struct_asym in
  Option.get sa.Struct_asym.entity_id

      
let asym_is_something something dblk asym_id =
  let entity_id = entity_id_of_asym_id dblk asym_id in
  let entity = List.find (fun e -> e.Entity.id = Some entity_id)
      dblk.Datablock.entity in
  entity.Entity.type_ = Some something

let asym_is_polymer = asym_is_something "polymer"
let asym_is_nonpolymer = asym_is_something "non-polymer"
let asym_is_water = asym_is_something "water"

let asym_is_polymer_something something dblk asym_id =
  let entity_id = entity_id_of_asym_id dblk asym_id in
  List.exists (fun e -> 
    e.Entity_poly.entity_id = Some entity_id
      && e.Entity_poly.type_ = Some something)
    dblk.Datablock.entity_poly

let asym_is_polypeptideL = asym_is_polymer_something "polypeptide(L)"
let asym_is_protein = asym_is_polypeptideL
let asym_is_polypeptideD = asym_is_polymer_something "polypeptide(D)"
let asym_is_DNA = asym_is_polymer_something "polydeoxyribonucleotide"
let asym_is_RNA = asym_is_polymer_something "polyribonucleotide"
let asym_is_DNA_RNA_hybrid = asym_is_polymer_something 
    "polydeoxyribonucleotide/polyribonucleotide hybrid"
let asym_is_nucleic_acids dblk asym_id = 
  asym_is_DNA dblk asym_id || asym_is_RNA dblk asym_id || asym_is_DNA_RNA_hybrid dblk asym_id

let asym_is_polysaccharideD = asym_is_polymer_something "polysaccharide(D)"

let guess_is_protein asym =
  let cnt = List.fold_left (fun cnt a ->
    if a.Atom.label_atom_id = "CA" 
	&& List.mem a.Atom.auth_comp_id std_amino3
    then succ cnt 
    else cnt) 0 asym in
  cnt >= 25

let find_poly_of_entity dblk entity_id =
  List.find (fun e -> e.Entity_poly.entity_id = Some entity_id)
    dblk.Datablock.entity_poly

let find_poly_of_asym dblk asym_id =
  let entity_id = entity_id_of_asym_id dblk asym_id in
  find_poly_of_entity dblk entity_id

let find_nonpoly_of_entity dblk entity_id =
  List.find (fun e -> e.Pdbx_entity_nonpoly.entity_id = Some entity_id)
    dblk.Datablock.pdbx_entity_nonpoly

let find_nonpoly_of_asym dblk asym_id =
  let entity_id = entity_id_of_asym_id dblk asym_id in
  find_nonpoly_of_entity dblk entity_id

let count_comps atoms = 
  let ht = Ht.create 1 in
  List.iter (fun a -> Ht.replace ht a.Atom.label_seq_id ()) atoms;
  Ht.length ht

let pdbid dblk =
  try
    Option.get (List.hd dblk.Datablock.entry).Entry.id
  with _ -> "????"

let last_release_date dblk =
  try
    Option.get (List.last dblk.Datablock.database_PDB_rev).Database_PDB_rev.date
  with _ -> "1900-01-01"

let aatbl = [
  (* standard amino acids *)
  ("ALA","A"); ("CYS","C"); ("ASP","D"); ("GLU","E"); ("PHE","F");
  ("GLY","G"); ("HIS","H"); ("ILE","I"); ("LYS","K"); ("LEU","L");
  ("MET","M"); ("ASN","N"); ("PRO","P"); ("GLN","Q"); ("ARG","R");
  ("SER","S"); ("THR","T"); ("VAL","V"); ("TRP","W"); ("TYR","Y");
  ("ASX","B"); ("GLX","Z"); 
  ("MSE","M");
]

let amino1_string = "ARNDCQEGHILKMFPSTWYVBZX"
let amino3to1 aa = try List.assoc aa aatbl with Not_found -> "X"

let index_amino1 a = try String.index amino1_string a with Not_found -> 22
let index_amino3 a = index_amino1 (amino3to1 a).[0]

let re_hydrogen = Str.regexp "[0-9]*[HD]"
let str_is_hydrogen str = Str.string_match re_hydrogen str 0
let atom_is_hydrogen a =
  a.Atom.type_symbol = "H"
  || a.Atom.type_symbol = "D"
  || (str_is_hydrogen a.Atom.label_atom_id)
  || (str_is_hydrogen a.Atom.auth_atom_id)
let remove_hydrogens atoms =
  List.filter (fun a -> not (atom_is_hydrogen a)) atoms

let get_calpha atoms = List.filter (fun a -> a.Atom.label_atom_id = "CA") atoms
