  (** Record for PDBML Atom *)
open PDBjBasis
(*
type t = {
    atom_site_id: string;
    model_num: int;
    group_PDB: string;
    label_asym_id: string;
    auth_asym_id: string;
    label_seq_id: int;
    auth_seq_id: string; 
    ins_code: string;
    label_alt_id: string;
    label_comp_id: string;
    auth_comp_id: string;
    type_symbol: string;
    label_atom_id: string;
    auth_atom_id: string;
    x: float; y: float; z: float;
    occupancy: float;
    pdbx_formal_charge: int;
    b_iso: float;
    aniso_B11: float; aniso_B12: float; aniso_B13: float;
    aniso_B22: float; aniso_B23: float;
    aniso_B33: float;
    asa: float;
    radius: float;
  }
*)

include Atom_t
(** The default atom record *)
let default = {model_num=0; label_entity_id="?";
	       label_asym_id="?"; auth_asym_id=".";
	       label_seq_id=0;label_comp_id="?"; auth_seq_id="0";
	       auth_comp_id="?"; 
	       atom_site_id="0";
	       group_PDB="ATOM";
	       type_symbol="?";
	       label_atom_id=".";
	       ins_code="?";
	       label_alt_id=".";
	       auth_atom_id="?";
	       x=nan;y=nan;z=nan;
	       occupancy=nan;
               pdbx_formal_charge=0;
	       b_iso=nan;
	       aniso_B11=nan; aniso_B12=nan; aniso_B13=nan;
	       aniso_B22=nan; aniso_B23=nan;
	       aniso_B33=nan;
	       asa=0.0;
	       radius=2.0;
	     }

    (** [coord atom] returns (x,y,z) coordinates. *)
let coord a = a.x,a.y,a.z
    (** Test the equality of two atoms by atom_site_id *)
let equal (a1 : t) (a2 : t) = a1.atom_site_id = a2.atom_site_id
let in_same_model a1 a2 = a1.model_num = a2.model_num
let in_same_asym a1 a2 = 
  in_same_model a1 a2 && a1.label_asym_id = a2.label_asym_id
    (** Test if two atoms are in the same comp [Comp.t]. *)
let in_same_comp a1 a2 = 
  in_same_asym a1 a2 && a1.label_seq_id = a2.label_seq_id 
let of_atom_site asite =
  let a = default in
  let a = Option.map_default (fun x -> {a with label_entity_id = x}) a
      asite.Atom_site.label_entity_id in
  let a = Option.map_default (fun x -> {a with atom_site_id = x}) a
      asite.Atom_site.id in
  let a = Option.map_default (fun x -> {a with model_num = x}) a 
      asite.Atom_site.pdbx_PDB_model_num in
  let a = Option.map_default (fun x -> {a with label_asym_id = x}) a 
      asite.Atom_site.label_asym_id in
  let a = Option.map_default (fun x -> {a with auth_asym_id = x}) a 
      asite.Atom_site.auth_asym_id in
  let a = Option.map_default (fun x -> {a with label_seq_id = x}) a 
      asite.Atom_site.label_seq_id in
  let a = Option.map_default (fun x -> {a with auth_seq_id = x}) a 
      asite.Atom_site.auth_seq_id in
  let a = Option.map_default (fun x -> {a with label_comp_id = x}) a 
      asite.Atom_site.label_comp_id in
  let a = Option.map_default (fun x -> {a with auth_comp_id = x}) a 
      asite.Atom_site.auth_comp_id in
  let a = Option.map_default (fun x -> {a with ins_code = x}) a 
      asite.Atom_site.pdbx_PDB_ins_code in
  let a = Option.map_default (fun x -> {a with group_PDB = x}) a 
      asite.Atom_site.group_PDB in
  let a = Option.map_default (fun x -> {a with type_symbol = x}) a 
      asite.Atom_site.type_symbol in
  let a = Option.map_default (fun x -> {a with label_atom_id = x}) a 
      asite.Atom_site.label_atom_id in
  let a = Option.map_default (fun x -> {a with auth_atom_id = x}) a 
      asite.Atom_site.auth_atom_id in
  let a = Option.map_default (fun x -> {a with label_alt_id = x}) a 
      asite.Atom_site.label_alt_id in
  let a = Option.map_default (fun x -> {a with x = x}) a 
      asite.Atom_site.cartn_x in
  let a = Option.map_default (fun x -> {a with y = x}) a 
      asite.Atom_site.cartn_y in
  let a = Option.map_default (fun x -> {a with z = x}) a 
      asite.Atom_site.cartn_z in
  let a = Option.map_default (fun x -> {a with occupancy = x}) a 
      asite.Atom_site.occupancy in
  let a = Option.map_default (fun x -> {a with pdbx_formal_charge = x}) a 
      asite.Atom_site.pdbx_formal_charge in
  let a = Option.map_default (fun x -> {a with b_iso = x}) a 
      asite.Atom_site.b_iso_or_equiv in
  let a = Option.map_default (fun x -> {a with aniso_B11 = x}) a
      asite.Atom_site.aniso_B11 in
  let a = Option.map_default (fun x -> {a with aniso_B12 = x}) a
      asite.Atom_site.aniso_B12 in
  let a = Option.map_default (fun x -> {a with aniso_B13 = x}) a
      asite.Atom_site.aniso_B13 in
  let a = Option.map_default (fun x -> {a with aniso_B22 = x}) a
      asite.Atom_site.aniso_B22 in
  let a = Option.map_default (fun x -> {a with aniso_B23 = x}) a
      asite.Atom_site.aniso_B23 in
  let a = Option.map_default (fun x -> {a with aniso_B33 = x}) a
      asite.Atom_site.aniso_B33 in
  a

let to_atom_site a =
  let nan_none x = 
    match classify_float x with
    | FP_nan -> None
    | FP_infinite -> None
    | _ -> Some x in
  {Atom_site.default with
   Atom_site.id = Some a.atom_site_id;
   pdbx_PDB_model_num = Some a.model_num;
   group_PDB = Some a.group_PDB;
   label_entity_id = Some a.label_entity_id;
   label_asym_id = Some a.label_asym_id;
   auth_asym_id = Some a.auth_asym_id;
   label_seq_id = Some a.label_seq_id;
   auth_seq_id = Some a.auth_seq_id;
   pdbx_PDB_ins_code = Some a.ins_code;
   label_alt_id = Some a.label_alt_id;
   label_comp_id = Some a.label_comp_id;
   auth_comp_id = Some a.auth_comp_id;
   type_symbol = Some a.type_symbol;
   label_atom_id = Some a.label_atom_id;
   auth_atom_id = Some a.auth_atom_id;
   cartn_x = Some a.x;
   cartn_y = Some a.y;
   cartn_z = Some a.z;
   occupancy = Some a.occupancy;
   pdbx_formal_charge = Some a.pdbx_formal_charge;
   b_iso_or_equiv = Some a.b_iso;
   aniso_B11 = nan_none a.aniso_B11; 
   aniso_B12 = nan_none a.aniso_B12; 
   aniso_B13 = nan_none a.aniso_B13;
   aniso_B22 = nan_none a.aniso_B22; 
   aniso_B23 = nan_none a.aniso_B23;
   aniso_B33 = nan_none a.aniso_B33;
 }

  
