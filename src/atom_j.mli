(* Auto-generated from "atom.atd" *)


type t = Atom_t.t = {
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

type atom_list = Atom_t.atom_list

val write_t :
  Bi_outbuf.t -> t -> unit
  (** Output a JSON value of type {!t}. *)

val string_of_t :
  ?len:int -> t -> string
  (** Serialize a value of type {!t}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_t :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> t
  (** Input JSON data of type {!t}. *)

val t_of_string :
  string -> t
  (** Deserialize JSON data of type {!t}. *)

val write_atom_list :
  Bi_outbuf.t -> atom_list -> unit
  (** Output a JSON value of type {!atom_list}. *)

val string_of_atom_list :
  ?len:int -> atom_list -> string
  (** Serialize a value of type {!atom_list}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_atom_list :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> atom_list
  (** Input JSON data of type {!atom_list}. *)

val atom_list_of_string :
  string -> atom_list
  (** Deserialize JSON data of type {!atom_list}. *)

