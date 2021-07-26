(* Auto-generated from "frame.atd" *)


type vec3_t = Frame_t.vec3_t

type frame_t = Frame_t.frame_t


let vec3_t_tag = Bi_io.tuple_tag
let write_untagged_vec3_t = (
  fun ob x ->
    Bi_vint.write_uvint ob 3;
    (
      let x, _, _ = x in (
        Bi_io.write_float64
      ) ob x
    );
    (
      let _, x, _ = x in (
        Bi_io.write_float64
      ) ob x
    );
    (
      let _, _, x = x in (
        Bi_io.write_float64
      ) ob x
    );
)
let write_vec3_t ob x =
  Bi_io.write_tag ob Bi_io.tuple_tag;
  write_untagged_vec3_t ob x
let string_of_vec3_t ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_vec3_t ob x;
  Bi_outbuf.contents ob
let get_vec3_t_reader = (
  fun tag ->
    if tag <> 20 then Ag_ob_run.read_error () else
      fun ib ->
        let len = Bi_vint.read_uvint ib in
        if len < 3 then Ag_ob_run.missing_tuple_fields len [ 0; 1; 2 ];
        let x0 =
          (
            Ag_ob_run.read_float
          ) ib
        in
        let x1 =
          (
            Ag_ob_run.read_float
          ) ib
        in
        let x2 =
          (
            Ag_ob_run.read_float
          ) ib
        in
        for i = 3 to len - 1 do Bi_io.skip ib done;
        (x0, x1, x2)
)
let read_vec3_t = (
  fun ib ->
    if Bi_io.read_tag ib <> 20 then Ag_ob_run.read_error_at ib;
    let len = Bi_vint.read_uvint ib in
    if len < 3 then Ag_ob_run.missing_tuple_fields len [ 0; 1; 2 ];
    let x0 =
      (
        Ag_ob_run.read_float
      ) ib
    in
    let x1 =
      (
        Ag_ob_run.read_float
      ) ib
    in
    let x2 =
      (
        Ag_ob_run.read_float
      ) ib
    in
    for i = 3 to len - 1 do Bi_io.skip ib done;
    (x0, x1, x2)
)
let vec3_t_of_string ?pos s =
  read_vec3_t (Bi_inbuf.from_string ?pos s)
let frame_t_tag = Bi_io.tuple_tag
let write_untagged_frame_t = (
  fun ob x ->
    Bi_vint.write_uvint ob 4;
    (
      let x, _, _, _ = x in (
        write_vec3_t
      ) ob x
    );
    (
      let _, x, _, _ = x in (
        write_vec3_t
      ) ob x
    );
    (
      let _, _, x, _ = x in (
        write_vec3_t
      ) ob x
    );
    (
      let _, _, _, x = x in (
        write_vec3_t
      ) ob x
    );
)
let write_frame_t ob x =
  Bi_io.write_tag ob Bi_io.tuple_tag;
  write_untagged_frame_t ob x
let string_of_frame_t ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_frame_t ob x;
  Bi_outbuf.contents ob
let get_frame_t_reader = (
  fun tag ->
    if tag <> 20 then Ag_ob_run.read_error () else
      fun ib ->
        let len = Bi_vint.read_uvint ib in
        if len < 4 then Ag_ob_run.missing_tuple_fields len [ 0; 1; 2; 3 ];
        let x0 =
          (
            read_vec3_t
          ) ib
        in
        let x1 =
          (
            read_vec3_t
          ) ib
        in
        let x2 =
          (
            read_vec3_t
          ) ib
        in
        let x3 =
          (
            read_vec3_t
          ) ib
        in
        for i = 4 to len - 1 do Bi_io.skip ib done;
        (x0, x1, x2, x3)
)
let read_frame_t = (
  fun ib ->
    if Bi_io.read_tag ib <> 20 then Ag_ob_run.read_error_at ib;
    let len = Bi_vint.read_uvint ib in
    if len < 4 then Ag_ob_run.missing_tuple_fields len [ 0; 1; 2; 3 ];
    let x0 =
      (
        read_vec3_t
      ) ib
    in
    let x1 =
      (
        read_vec3_t
      ) ib
    in
    let x2 =
      (
        read_vec3_t
      ) ib
    in
    let x3 =
      (
        read_vec3_t
      ) ib
    in
    for i = 4 to len - 1 do Bi_io.skip ib done;
    (x0, x1, x2, x3)
)
let frame_t_of_string ?pos s =
  read_frame_t (Bi_inbuf.from_string ?pos s)
