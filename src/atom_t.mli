(* Auto-generated from "atom.atd" *)


type t = {
  atom_site_id: string;
  model_num: int;
  group_PDB: string;
  label_entity_id: string;
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
  x: float;
  y: float;
  z: float;
  occupancy: float;
  pdbx_formal_charge: int;
  b_iso: float;
  aniso_B11: float;
  aniso_B12: float;
  aniso_B13: float;
  aniso_B22: float;
  aniso_B23: float;
  aniso_B33: float;
  asa: float;
  radius: float
}

type atom_list = t list
