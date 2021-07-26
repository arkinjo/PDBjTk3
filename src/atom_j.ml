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

let write_t : _ -> t -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"atom_site_id\":";
    (
      Yojson.Safe.write_string
    )
      ob x.atom_site_id;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"model_num\":";
    (
      Yojson.Safe.write_int
    )
      ob x.model_num;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"group_PDB\":";
    (
      Yojson.Safe.write_string
    )
      ob x.group_PDB;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"label_entity_id\":";
    (
      Yojson.Safe.write_string
    )
      ob x.label_entity_id;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"label_asym_id\":";
    (
      Yojson.Safe.write_string
    )
      ob x.label_asym_id;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"auth_asym_id\":";
    (
      Yojson.Safe.write_string
    )
      ob x.auth_asym_id;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"label_seq_id\":";
    (
      Yojson.Safe.write_int
    )
      ob x.label_seq_id;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"auth_seq_id\":";
    (
      Yojson.Safe.write_string
    )
      ob x.auth_seq_id;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"ins_code\":";
    (
      Yojson.Safe.write_string
    )
      ob x.ins_code;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"label_alt_id\":";
    (
      Yojson.Safe.write_string
    )
      ob x.label_alt_id;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"label_comp_id\":";
    (
      Yojson.Safe.write_string
    )
      ob x.label_comp_id;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"auth_comp_id\":";
    (
      Yojson.Safe.write_string
    )
      ob x.auth_comp_id;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"type_symbol\":";
    (
      Yojson.Safe.write_string
    )
      ob x.type_symbol;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"label_atom_id\":";
    (
      Yojson.Safe.write_string
    )
      ob x.label_atom_id;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"auth_atom_id\":";
    (
      Yojson.Safe.write_string
    )
      ob x.auth_atom_id;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"x\":";
    (
      Yojson.Safe.write_float
    )
      ob x.x;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"y\":";
    (
      Yojson.Safe.write_float
    )
      ob x.y;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"z\":";
    (
      Yojson.Safe.write_float
    )
      ob x.z;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"occupancy\":";
    (
      Yojson.Safe.write_float
    )
      ob x.occupancy;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"pdbx_formal_charge\":";
    (
      Yojson.Safe.write_int
    )
      ob x.pdbx_formal_charge;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"b_iso\":";
    (
      Yojson.Safe.write_float
    )
      ob x.b_iso;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"aniso_B11\":";
    (
      Yojson.Safe.write_float
    )
      ob x.aniso_B11;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"aniso_B12\":";
    (
      Yojson.Safe.write_float
    )
      ob x.aniso_B12;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"aniso_B13\":";
    (
      Yojson.Safe.write_float
    )
      ob x.aniso_B13;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"aniso_B22\":";
    (
      Yojson.Safe.write_float
    )
      ob x.aniso_B22;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"aniso_B23\":";
    (
      Yojson.Safe.write_float
    )
      ob x.aniso_B23;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"aniso_B33\":";
    (
      Yojson.Safe.write_float
    )
      ob x.aniso_B33;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"asa\":";
    (
      Yojson.Safe.write_float
    )
      ob x.asa;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"radius\":";
    (
      Yojson.Safe.write_float
    )
      ob x.radius;
    Bi_outbuf.add_char ob '}';
)
let string_of_t ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_t ob x;
  Bi_outbuf.contents ob
let read_t = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_atom_site_id = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_model_num = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_group_PDB = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_label_entity_id = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_label_asym_id = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_auth_asym_id = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_label_seq_id = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_auth_seq_id = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_ins_code = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_label_alt_id = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_label_comp_id = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_auth_comp_id = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_type_symbol = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_label_atom_id = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_auth_atom_id = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_x = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_y = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_z = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_occupancy = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_pdbx_formal_charge = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_b_iso = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_aniso_B11 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_aniso_B12 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_aniso_B13 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_aniso_B22 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_aniso_B23 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_aniso_B33 = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_asa = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_radius = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          match len with
            | 1 -> (
                match String.unsafe_get s pos with
                  | 'x' -> (
                      15
                    )
                  | 'y' -> (
                      16
                    )
                  | 'z' -> (
                      17
                    )
                  | _ -> (
                      -1
                    )
              )
            | 3 -> (
                if String.unsafe_get s pos = 'a' && String.unsafe_get s (pos+1) = 's' && String.unsafe_get s (pos+2) = 'a' then (
                  27
                )
                else (
                  -1
                )
              )
            | 5 -> (
                if String.unsafe_get s pos = 'b' && String.unsafe_get s (pos+1) = '_' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 'o' then (
                  20
                )
                else (
                  -1
                )
              )
            | 6 -> (
                if String.unsafe_get s pos = 'r' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'd' && String.unsafe_get s (pos+3) = 'i' && String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 's' then (
                  28
                )
                else (
                  -1
                )
              )
            | 8 -> (
                if String.unsafe_get s pos = 'i' && String.unsafe_get s (pos+1) = 'n' && String.unsafe_get s (pos+2) = 's' && String.unsafe_get s (pos+3) = '_' && String.unsafe_get s (pos+4) = 'c' && String.unsafe_get s (pos+5) = 'o' && String.unsafe_get s (pos+6) = 'd' && String.unsafe_get s (pos+7) = 'e' then (
                  8
                )
                else (
                  -1
                )
              )
            | 9 -> (
                match String.unsafe_get s pos with
                  | 'a' -> (
                      if String.unsafe_get s (pos+1) = 'n' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'B' then (
                        match String.unsafe_get s (pos+7) with
                          | '1' -> (
                              match String.unsafe_get s (pos+8) with
                                | '1' -> (
                                    21
                                  )
                                | '2' -> (
                                    22
                                  )
                                | '3' -> (
                                    23
                                  )
                                | _ -> (
                                    -1
                                  )
                            )
                          | '2' -> (
                              match String.unsafe_get s (pos+8) with
                                | '2' -> (
                                    24
                                  )
                                | '3' -> (
                                    25
                                  )
                                | _ -> (
                                    -1
                                  )
                            )
                          | '3' -> (
                              if String.unsafe_get s (pos+8) = '3' then (
                                26
                              )
                              else (
                                -1
                              )
                            )
                          | _ -> (
                              -1
                            )
                      )
                      else (
                        -1
                      )
                    )
                  | 'g' -> (
                      if String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'p' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'P' && String.unsafe_get s (pos+7) = 'D' && String.unsafe_get s (pos+8) = 'B' then (
                        2
                      )
                      else (
                        -1
                      )
                    )
                  | 'm' -> (
                      if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'd' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'n' && String.unsafe_get s (pos+7) = 'u' && String.unsafe_get s (pos+8) = 'm' then (
                        1
                      )
                      else (
                        -1
                      )
                    )
                  | 'o' -> (
                      if String.unsafe_get s (pos+1) = 'c' && String.unsafe_get s (pos+2) = 'c' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'p' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 'n' && String.unsafe_get s (pos+7) = 'c' && String.unsafe_get s (pos+8) = 'y' then (
                        18
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 11 -> (
                match String.unsafe_get s pos with
                  | 'a' -> (
                      if String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'h' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 's' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'q' && String.unsafe_get s (pos+8) = '_' && String.unsafe_get s (pos+9) = 'i' && String.unsafe_get s (pos+10) = 'd' then (
                        7
                      )
                      else (
                        -1
                      )
                    )
                  | 't' -> (
                      if String.unsafe_get s (pos+1) = 'y' && String.unsafe_get s (pos+2) = 'p' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 's' && String.unsafe_get s (pos+6) = 'y' && String.unsafe_get s (pos+7) = 'm' && String.unsafe_get s (pos+8) = 'b' && String.unsafe_get s (pos+9) = 'o' && String.unsafe_get s (pos+10) = 'l' then (
                        12
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 12 -> (
                match String.unsafe_get s pos with
                  | 'a' -> (
                      match String.unsafe_get s (pos+1) with
                        | 't' -> (
                            if String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 'm' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 's' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 't' && String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = '_' && String.unsafe_get s (pos+10) = 'i' && String.unsafe_get s (pos+11) = 'd' then (
                              0
                            )
                            else (
                              -1
                            )
                          )
                        | 'u' -> (
                            if String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'h' && String.unsafe_get s (pos+4) = '_' then (
                              match String.unsafe_get s (pos+5) with
                                | 'a' -> (
                                    match String.unsafe_get s (pos+6) with
                                      | 's' -> (
                                          if String.unsafe_get s (pos+7) = 'y' && String.unsafe_get s (pos+8) = 'm' && String.unsafe_get s (pos+9) = '_' && String.unsafe_get s (pos+10) = 'i' && String.unsafe_get s (pos+11) = 'd' then (
                                            5
                                          )
                                          else (
                                            -1
                                          )
                                        )
                                      | 't' -> (
                                          if String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'm' && String.unsafe_get s (pos+9) = '_' && String.unsafe_get s (pos+10) = 'i' && String.unsafe_get s (pos+11) = 'd' then (
                                            14
                                          )
                                          else (
                                            -1
                                          )
                                        )
                                      | _ -> (
                                          -1
                                        )
                                  )
                                | 'c' -> (
                                    if String.unsafe_get s (pos+6) = 'o' && String.unsafe_get s (pos+7) = 'm' && String.unsafe_get s (pos+8) = 'p' && String.unsafe_get s (pos+9) = '_' && String.unsafe_get s (pos+10) = 'i' && String.unsafe_get s (pos+11) = 'd' then (
                                      11
                                    )
                                    else (
                                      -1
                                    )
                                  )
                                | _ -> (
                                    -1
                                  )
                            )
                            else (
                              -1
                            )
                          )
                        | _ -> (
                            -1
                          )
                    )
                  | 'l' -> (
                      if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'b' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = '_' then (
                        match String.unsafe_get s (pos+6) with
                          | 'a' -> (
                              if String.unsafe_get s (pos+7) = 'l' && String.unsafe_get s (pos+8) = 't' && String.unsafe_get s (pos+9) = '_' && String.unsafe_get s (pos+10) = 'i' && String.unsafe_get s (pos+11) = 'd' then (
                                9
                              )
                              else (
                                -1
                              )
                            )
                          | 's' -> (
                              if String.unsafe_get s (pos+7) = 'e' && String.unsafe_get s (pos+8) = 'q' && String.unsafe_get s (pos+9) = '_' && String.unsafe_get s (pos+10) = 'i' && String.unsafe_get s (pos+11) = 'd' then (
                                6
                              )
                              else (
                                -1
                              )
                            )
                          | _ -> (
                              -1
                            )
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 13 -> (
                if String.unsafe_get s pos = 'l' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'b' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = '_' then (
                  match String.unsafe_get s (pos+6) with
                    | 'a' -> (
                        match String.unsafe_get s (pos+7) with
                          | 's' -> (
                              if String.unsafe_get s (pos+8) = 'y' && String.unsafe_get s (pos+9) = 'm' && String.unsafe_get s (pos+10) = '_' && String.unsafe_get s (pos+11) = 'i' && String.unsafe_get s (pos+12) = 'd' then (
                                4
                              )
                              else (
                                -1
                              )
                            )
                          | 't' -> (
                              if String.unsafe_get s (pos+8) = 'o' && String.unsafe_get s (pos+9) = 'm' && String.unsafe_get s (pos+10) = '_' && String.unsafe_get s (pos+11) = 'i' && String.unsafe_get s (pos+12) = 'd' then (
                                13
                              )
                              else (
                                -1
                              )
                            )
                          | _ -> (
                              -1
                            )
                      )
                    | 'c' -> (
                        if String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'm' && String.unsafe_get s (pos+9) = 'p' && String.unsafe_get s (pos+10) = '_' && String.unsafe_get s (pos+11) = 'i' && String.unsafe_get s (pos+12) = 'd' then (
                          10
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
                else (
                  -1
                )
              )
            | 15 -> (
                if String.unsafe_get s pos = 'l' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'b' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'n' && String.unsafe_get s (pos+8) = 't' && String.unsafe_get s (pos+9) = 'i' && String.unsafe_get s (pos+10) = 't' && String.unsafe_get s (pos+11) = 'y' && String.unsafe_get s (pos+12) = '_' && String.unsafe_get s (pos+13) = 'i' && String.unsafe_get s (pos+14) = 'd' then (
                  3
                )
                else (
                  -1
                )
              )
            | 18 -> (
                if String.unsafe_get s pos = 'p' && String.unsafe_get s (pos+1) = 'd' && String.unsafe_get s (pos+2) = 'b' && String.unsafe_get s (pos+3) = 'x' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'f' && String.unsafe_get s (pos+6) = 'o' && String.unsafe_get s (pos+7) = 'r' && String.unsafe_get s (pos+8) = 'm' && String.unsafe_get s (pos+9) = 'a' && String.unsafe_get s (pos+10) = 'l' && String.unsafe_get s (pos+11) = '_' && String.unsafe_get s (pos+12) = 'c' && String.unsafe_get s (pos+13) = 'h' && String.unsafe_get s (pos+14) = 'a' && String.unsafe_get s (pos+15) = 'r' && String.unsafe_get s (pos+16) = 'g' && String.unsafe_get s (pos+17) = 'e' then (
                  19
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Ag_oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_atom_site_id := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x1;
          | 1 ->
            field_model_num := (
              (
                Ag_oj_run.read_int
              ) p lb
            );
            bits0 := !bits0 lor 0x2;
          | 2 ->
            field_group_PDB := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x4;
          | 3 ->
            field_label_entity_id := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x8;
          | 4 ->
            field_label_asym_id := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x10;
          | 5 ->
            field_auth_asym_id := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x20;
          | 6 ->
            field_label_seq_id := (
              (
                Ag_oj_run.read_int
              ) p lb
            );
            bits0 := !bits0 lor 0x40;
          | 7 ->
            field_auth_seq_id := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x80;
          | 8 ->
            field_ins_code := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x100;
          | 9 ->
            field_label_alt_id := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x200;
          | 10 ->
            field_label_comp_id := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x400;
          | 11 ->
            field_auth_comp_id := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x800;
          | 12 ->
            field_type_symbol := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x1000;
          | 13 ->
            field_label_atom_id := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x2000;
          | 14 ->
            field_auth_atom_id := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x4000;
          | 15 ->
            field_x := (
              (
                Ag_oj_run.read_number
              ) p lb
            );
            bits0 := !bits0 lor 0x8000;
          | 16 ->
            field_y := (
              (
                Ag_oj_run.read_number
              ) p lb
            );
            bits0 := !bits0 lor 0x10000;
          | 17 ->
            field_z := (
              (
                Ag_oj_run.read_number
              ) p lb
            );
            bits0 := !bits0 lor 0x20000;
          | 18 ->
            field_occupancy := (
              (
                Ag_oj_run.read_number
              ) p lb
            );
            bits0 := !bits0 lor 0x40000;
          | 19 ->
            field_pdbx_formal_charge := (
              (
                Ag_oj_run.read_int
              ) p lb
            );
            bits0 := !bits0 lor 0x80000;
          | 20 ->
            field_b_iso := (
              (
                Ag_oj_run.read_number
              ) p lb
            );
            bits0 := !bits0 lor 0x100000;
          | 21 ->
            field_aniso_B11 := (
              (
                Ag_oj_run.read_number
              ) p lb
            );
            bits0 := !bits0 lor 0x200000;
          | 22 ->
            field_aniso_B12 := (
              (
                Ag_oj_run.read_number
              ) p lb
            );
            bits0 := !bits0 lor 0x400000;
          | 23 ->
            field_aniso_B13 := (
              (
                Ag_oj_run.read_number
              ) p lb
            );
            bits0 := !bits0 lor 0x800000;
          | 24 ->
            field_aniso_B22 := (
              (
                Ag_oj_run.read_number
              ) p lb
            );
            bits0 := !bits0 lor 0x1000000;
          | 25 ->
            field_aniso_B23 := (
              (
                Ag_oj_run.read_number
              ) p lb
            );
            bits0 := !bits0 lor 0x2000000;
          | 26 ->
            field_aniso_B33 := (
              (
                Ag_oj_run.read_number
              ) p lb
            );
            bits0 := !bits0 lor 0x4000000;
          | 27 ->
            field_asa := (
              (
                Ag_oj_run.read_number
              ) p lb
            );
            bits0 := !bits0 lor 0x8000000;
          | 28 ->
            field_radius := (
              (
                Ag_oj_run.read_number
              ) p lb
            );
            bits0 := !bits0 lor 0x10000000;
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            match len with
              | 1 -> (
                  match String.unsafe_get s pos with
                    | 'x' -> (
                        15
                      )
                    | 'y' -> (
                        16
                      )
                    | 'z' -> (
                        17
                      )
                    | _ -> (
                        -1
                      )
                )
              | 3 -> (
                  if String.unsafe_get s pos = 'a' && String.unsafe_get s (pos+1) = 's' && String.unsafe_get s (pos+2) = 'a' then (
                    27
                  )
                  else (
                    -1
                  )
                )
              | 5 -> (
                  if String.unsafe_get s pos = 'b' && String.unsafe_get s (pos+1) = '_' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 'o' then (
                    20
                  )
                  else (
                    -1
                  )
                )
              | 6 -> (
                  if String.unsafe_get s pos = 'r' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'd' && String.unsafe_get s (pos+3) = 'i' && String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 's' then (
                    28
                  )
                  else (
                    -1
                  )
                )
              | 8 -> (
                  if String.unsafe_get s pos = 'i' && String.unsafe_get s (pos+1) = 'n' && String.unsafe_get s (pos+2) = 's' && String.unsafe_get s (pos+3) = '_' && String.unsafe_get s (pos+4) = 'c' && String.unsafe_get s (pos+5) = 'o' && String.unsafe_get s (pos+6) = 'd' && String.unsafe_get s (pos+7) = 'e' then (
                    8
                  )
                  else (
                    -1
                  )
                )
              | 9 -> (
                  match String.unsafe_get s pos with
                    | 'a' -> (
                        if String.unsafe_get s (pos+1) = 'n' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'B' then (
                          match String.unsafe_get s (pos+7) with
                            | '1' -> (
                                match String.unsafe_get s (pos+8) with
                                  | '1' -> (
                                      21
                                    )
                                  | '2' -> (
                                      22
                                    )
                                  | '3' -> (
                                      23
                                    )
                                  | _ -> (
                                      -1
                                    )
                              )
                            | '2' -> (
                                match String.unsafe_get s (pos+8) with
                                  | '2' -> (
                                      24
                                    )
                                  | '3' -> (
                                      25
                                    )
                                  | _ -> (
                                      -1
                                    )
                              )
                            | '3' -> (
                                if String.unsafe_get s (pos+8) = '3' then (
                                  26
                                )
                                else (
                                  -1
                                )
                              )
                            | _ -> (
                                -1
                              )
                        )
                        else (
                          -1
                        )
                      )
                    | 'g' -> (
                        if String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'p' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'P' && String.unsafe_get s (pos+7) = 'D' && String.unsafe_get s (pos+8) = 'B' then (
                          2
                        )
                        else (
                          -1
                        )
                      )
                    | 'm' -> (
                        if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'd' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'n' && String.unsafe_get s (pos+7) = 'u' && String.unsafe_get s (pos+8) = 'm' then (
                          1
                        )
                        else (
                          -1
                        )
                      )
                    | 'o' -> (
                        if String.unsafe_get s (pos+1) = 'c' && String.unsafe_get s (pos+2) = 'c' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'p' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 'n' && String.unsafe_get s (pos+7) = 'c' && String.unsafe_get s (pos+8) = 'y' then (
                          18
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 11 -> (
                  match String.unsafe_get s pos with
                    | 'a' -> (
                        if String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'h' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 's' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'q' && String.unsafe_get s (pos+8) = '_' && String.unsafe_get s (pos+9) = 'i' && String.unsafe_get s (pos+10) = 'd' then (
                          7
                        )
                        else (
                          -1
                        )
                      )
                    | 't' -> (
                        if String.unsafe_get s (pos+1) = 'y' && String.unsafe_get s (pos+2) = 'p' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 's' && String.unsafe_get s (pos+6) = 'y' && String.unsafe_get s (pos+7) = 'm' && String.unsafe_get s (pos+8) = 'b' && String.unsafe_get s (pos+9) = 'o' && String.unsafe_get s (pos+10) = 'l' then (
                          12
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 12 -> (
                  match String.unsafe_get s pos with
                    | 'a' -> (
                        match String.unsafe_get s (pos+1) with
                          | 't' -> (
                              if String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 'm' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 's' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 't' && String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = '_' && String.unsafe_get s (pos+10) = 'i' && String.unsafe_get s (pos+11) = 'd' then (
                                0
                              )
                              else (
                                -1
                              )
                            )
                          | 'u' -> (
                              if String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'h' && String.unsafe_get s (pos+4) = '_' then (
                                match String.unsafe_get s (pos+5) with
                                  | 'a' -> (
                                      match String.unsafe_get s (pos+6) with
                                        | 's' -> (
                                            if String.unsafe_get s (pos+7) = 'y' && String.unsafe_get s (pos+8) = 'm' && String.unsafe_get s (pos+9) = '_' && String.unsafe_get s (pos+10) = 'i' && String.unsafe_get s (pos+11) = 'd' then (
                                              5
                                            )
                                            else (
                                              -1
                                            )
                                          )
                                        | 't' -> (
                                            if String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'm' && String.unsafe_get s (pos+9) = '_' && String.unsafe_get s (pos+10) = 'i' && String.unsafe_get s (pos+11) = 'd' then (
                                              14
                                            )
                                            else (
                                              -1
                                            )
                                          )
                                        | _ -> (
                                            -1
                                          )
                                    )
                                  | 'c' -> (
                                      if String.unsafe_get s (pos+6) = 'o' && String.unsafe_get s (pos+7) = 'm' && String.unsafe_get s (pos+8) = 'p' && String.unsafe_get s (pos+9) = '_' && String.unsafe_get s (pos+10) = 'i' && String.unsafe_get s (pos+11) = 'd' then (
                                        11
                                      )
                                      else (
                                        -1
                                      )
                                    )
                                  | _ -> (
                                      -1
                                    )
                              )
                              else (
                                -1
                              )
                            )
                          | _ -> (
                              -1
                            )
                      )
                    | 'l' -> (
                        if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'b' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = '_' then (
                          match String.unsafe_get s (pos+6) with
                            | 'a' -> (
                                if String.unsafe_get s (pos+7) = 'l' && String.unsafe_get s (pos+8) = 't' && String.unsafe_get s (pos+9) = '_' && String.unsafe_get s (pos+10) = 'i' && String.unsafe_get s (pos+11) = 'd' then (
                                  9
                                )
                                else (
                                  -1
                                )
                              )
                            | 's' -> (
                                if String.unsafe_get s (pos+7) = 'e' && String.unsafe_get s (pos+8) = 'q' && String.unsafe_get s (pos+9) = '_' && String.unsafe_get s (pos+10) = 'i' && String.unsafe_get s (pos+11) = 'd' then (
                                  6
                                )
                                else (
                                  -1
                                )
                              )
                            | _ -> (
                                -1
                              )
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 13 -> (
                  if String.unsafe_get s pos = 'l' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'b' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = '_' then (
                    match String.unsafe_get s (pos+6) with
                      | 'a' -> (
                          match String.unsafe_get s (pos+7) with
                            | 's' -> (
                                if String.unsafe_get s (pos+8) = 'y' && String.unsafe_get s (pos+9) = 'm' && String.unsafe_get s (pos+10) = '_' && String.unsafe_get s (pos+11) = 'i' && String.unsafe_get s (pos+12) = 'd' then (
                                  4
                                )
                                else (
                                  -1
                                )
                              )
                            | 't' -> (
                                if String.unsafe_get s (pos+8) = 'o' && String.unsafe_get s (pos+9) = 'm' && String.unsafe_get s (pos+10) = '_' && String.unsafe_get s (pos+11) = 'i' && String.unsafe_get s (pos+12) = 'd' then (
                                  13
                                )
                                else (
                                  -1
                                )
                              )
                            | _ -> (
                                -1
                              )
                        )
                      | 'c' -> (
                          if String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'm' && String.unsafe_get s (pos+9) = 'p' && String.unsafe_get s (pos+10) = '_' && String.unsafe_get s (pos+11) = 'i' && String.unsafe_get s (pos+12) = 'd' then (
                            10
                          )
                          else (
                            -1
                          )
                        )
                      | _ -> (
                          -1
                        )
                  )
                  else (
                    -1
                  )
                )
              | 15 -> (
                  if String.unsafe_get s pos = 'l' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'b' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'n' && String.unsafe_get s (pos+8) = 't' && String.unsafe_get s (pos+9) = 'i' && String.unsafe_get s (pos+10) = 't' && String.unsafe_get s (pos+11) = 'y' && String.unsafe_get s (pos+12) = '_' && String.unsafe_get s (pos+13) = 'i' && String.unsafe_get s (pos+14) = 'd' then (
                    3
                  )
                  else (
                    -1
                  )
                )
              | 18 -> (
                  if String.unsafe_get s pos = 'p' && String.unsafe_get s (pos+1) = 'd' && String.unsafe_get s (pos+2) = 'b' && String.unsafe_get s (pos+3) = 'x' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'f' && String.unsafe_get s (pos+6) = 'o' && String.unsafe_get s (pos+7) = 'r' && String.unsafe_get s (pos+8) = 'm' && String.unsafe_get s (pos+9) = 'a' && String.unsafe_get s (pos+10) = 'l' && String.unsafe_get s (pos+11) = '_' && String.unsafe_get s (pos+12) = 'c' && String.unsafe_get s (pos+13) = 'h' && String.unsafe_get s (pos+14) = 'a' && String.unsafe_get s (pos+15) = 'r' && String.unsafe_get s (pos+16) = 'g' && String.unsafe_get s (pos+17) = 'e' then (
                    19
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Ag_oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_atom_site_id := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x1;
            | 1 ->
              field_model_num := (
                (
                  Ag_oj_run.read_int
                ) p lb
              );
              bits0 := !bits0 lor 0x2;
            | 2 ->
              field_group_PDB := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x4;
            | 3 ->
              field_label_entity_id := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x8;
            | 4 ->
              field_label_asym_id := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x10;
            | 5 ->
              field_auth_asym_id := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x20;
            | 6 ->
              field_label_seq_id := (
                (
                  Ag_oj_run.read_int
                ) p lb
              );
              bits0 := !bits0 lor 0x40;
            | 7 ->
              field_auth_seq_id := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x80;
            | 8 ->
              field_ins_code := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x100;
            | 9 ->
              field_label_alt_id := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x200;
            | 10 ->
              field_label_comp_id := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x400;
            | 11 ->
              field_auth_comp_id := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x800;
            | 12 ->
              field_type_symbol := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x1000;
            | 13 ->
              field_label_atom_id := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x2000;
            | 14 ->
              field_auth_atom_id := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x4000;
            | 15 ->
              field_x := (
                (
                  Ag_oj_run.read_number
                ) p lb
              );
              bits0 := !bits0 lor 0x8000;
            | 16 ->
              field_y := (
                (
                  Ag_oj_run.read_number
                ) p lb
              );
              bits0 := !bits0 lor 0x10000;
            | 17 ->
              field_z := (
                (
                  Ag_oj_run.read_number
                ) p lb
              );
              bits0 := !bits0 lor 0x20000;
            | 18 ->
              field_occupancy := (
                (
                  Ag_oj_run.read_number
                ) p lb
              );
              bits0 := !bits0 lor 0x40000;
            | 19 ->
              field_pdbx_formal_charge := (
                (
                  Ag_oj_run.read_int
                ) p lb
              );
              bits0 := !bits0 lor 0x80000;
            | 20 ->
              field_b_iso := (
                (
                  Ag_oj_run.read_number
                ) p lb
              );
              bits0 := !bits0 lor 0x100000;
            | 21 ->
              field_aniso_B11 := (
                (
                  Ag_oj_run.read_number
                ) p lb
              );
              bits0 := !bits0 lor 0x200000;
            | 22 ->
              field_aniso_B12 := (
                (
                  Ag_oj_run.read_number
                ) p lb
              );
              bits0 := !bits0 lor 0x400000;
            | 23 ->
              field_aniso_B13 := (
                (
                  Ag_oj_run.read_number
                ) p lb
              );
              bits0 := !bits0 lor 0x800000;
            | 24 ->
              field_aniso_B22 := (
                (
                  Ag_oj_run.read_number
                ) p lb
              );
              bits0 := !bits0 lor 0x1000000;
            | 25 ->
              field_aniso_B23 := (
                (
                  Ag_oj_run.read_number
                ) p lb
              );
              bits0 := !bits0 lor 0x2000000;
            | 26 ->
              field_aniso_B33 := (
                (
                  Ag_oj_run.read_number
                ) p lb
              );
              bits0 := !bits0 lor 0x4000000;
            | 27 ->
              field_asa := (
                (
                  Ag_oj_run.read_number
                ) p lb
              );
              bits0 := !bits0 lor 0x8000000;
            | 28 ->
              field_radius := (
                (
                  Ag_oj_run.read_number
                ) p lb
              );
              bits0 := !bits0 lor 0x10000000;
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x1fffffff then Ag_oj_run.missing_fields p [| !bits0 |] [| "atom_site_id"; "model_num"; "group_PDB"; "label_entity_id"; "label_asym_id"; "auth_asym_id"; "label_seq_id"; "auth_seq_id"; "ins_code"; "label_alt_id"; "label_comp_id"; "auth_comp_id"; "type_symbol"; "label_atom_id"; "auth_atom_id"; "x"; "y"; "z"; "occupancy"; "pdbx_formal_charge"; "b_iso"; "aniso_B11"; "aniso_B12"; "aniso_B13"; "aniso_B22"; "aniso_B23"; "aniso_B33"; "asa"; "radius" |];
        (
          {
            atom_site_id = !field_atom_site_id;
            model_num = !field_model_num;
            group_PDB = !field_group_PDB;
            label_entity_id = !field_label_entity_id;
            label_asym_id = !field_label_asym_id;
            auth_asym_id = !field_auth_asym_id;
            label_seq_id = !field_label_seq_id;
            auth_seq_id = !field_auth_seq_id;
            ins_code = !field_ins_code;
            label_alt_id = !field_label_alt_id;
            label_comp_id = !field_label_comp_id;
            auth_comp_id = !field_auth_comp_id;
            type_symbol = !field_type_symbol;
            label_atom_id = !field_label_atom_id;
            auth_atom_id = !field_auth_atom_id;
            x = !field_x;
            y = !field_y;
            z = !field_z;
            occupancy = !field_occupancy;
            pdbx_formal_charge = !field_pdbx_formal_charge;
            b_iso = !field_b_iso;
            aniso_B11 = !field_aniso_B11;
            aniso_B12 = !field_aniso_B12;
            aniso_B13 = !field_aniso_B13;
            aniso_B22 = !field_aniso_B22;
            aniso_B23 = !field_aniso_B23;
            aniso_B33 = !field_aniso_B33;
            asa = !field_asa;
            radius = !field_radius;
          }
         : t)
      )
)
let t_of_string s =
  read_t (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__1 = (
  Ag_oj_run.write_list (
    write_t
  )
)
let string_of__1 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__1 ob x;
  Bi_outbuf.contents ob
let read__1 = (
  Ag_oj_run.read_list (
    read_t
  )
)
let _1_of_string s =
  read__1 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_atom_list = (
  write__1
)
let string_of_atom_list ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_atom_list ob x;
  Bi_outbuf.contents ob
let read_atom_list = (
  read__1
)
let atom_list_of_string s =
  read_atom_list (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
