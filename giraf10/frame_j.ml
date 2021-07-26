(* Auto-generated from "frame.atd" *)


type vec3_t = Frame_t.vec3_t

type frame_t = Frame_t.frame_t

let write_vec3_t = (
  fun ob x ->
    Bi_outbuf.add_char ob '(';
    (let x, _, _ = x in
    (
      Yojson.Safe.write_float
    ) ob x
    );
    Bi_outbuf.add_char ob ',';
    (let _, x, _ = x in
    (
      Yojson.Safe.write_float
    ) ob x
    );
    Bi_outbuf.add_char ob ',';
    (let _, _, x = x in
    (
      Yojson.Safe.write_float
    ) ob x
    );
    Bi_outbuf.add_char ob ')';
)
let string_of_vec3_t ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_vec3_t ob x;
  Bi_outbuf.contents ob
let read_vec3_t = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    let std_tuple = Yojson.Safe.start_any_tuple p lb in
    let len = ref 0 in
    let end_of_tuple = ref false in
    (try
      let x0 =
        let x =
          (
            Ag_oj_run.read_number
          ) p lb
        in
        incr len;
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
        x
      in
      let x1 =
        let x =
          (
            Ag_oj_run.read_number
          ) p lb
        in
        incr len;
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
        x
      in
      let x2 =
        let x =
          (
            Ag_oj_run.read_number
          ) p lb
        in
        incr len;
        (try
          Yojson.Safe.read_space p lb;
          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
        with Yojson.End_of_tuple -> end_of_tuple := true);
        x
      in
      if not !end_of_tuple then (
        try
          while true do
            Yojson.Safe.skip_json p lb;
            Yojson.Safe.read_space p lb;
            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
          done
        with Yojson.End_of_tuple -> ()
      );
      (x0, x1, x2)
    with Yojson.End_of_tuple ->
      Ag_oj_run.missing_tuple_fields p !len [ 0; 1; 2 ]);
)
let vec3_t_of_string s =
  read_vec3_t (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_frame_t = (
  fun ob x ->
    Bi_outbuf.add_char ob '(';
    (let x, _, _, _ = x in
    (
      write_vec3_t
    ) ob x
    );
    Bi_outbuf.add_char ob ',';
    (let _, x, _, _ = x in
    (
      write_vec3_t
    ) ob x
    );
    Bi_outbuf.add_char ob ',';
    (let _, _, x, _ = x in
    (
      write_vec3_t
    ) ob x
    );
    Bi_outbuf.add_char ob ',';
    (let _, _, _, x = x in
    (
      write_vec3_t
    ) ob x
    );
    Bi_outbuf.add_char ob ')';
)
let string_of_frame_t ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_frame_t ob x;
  Bi_outbuf.contents ob
let read_frame_t = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    let std_tuple = Yojson.Safe.start_any_tuple p lb in
    let len = ref 0 in
    let end_of_tuple = ref false in
    (try
      let x0 =
        let x =
          (
            read_vec3_t
          ) p lb
        in
        incr len;
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
        x
      in
      let x1 =
        let x =
          (
            read_vec3_t
          ) p lb
        in
        incr len;
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
        x
      in
      let x2 =
        let x =
          (
            read_vec3_t
          ) p lb
        in
        incr len;
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
        x
      in
      let x3 =
        let x =
          (
            read_vec3_t
          ) p lb
        in
        incr len;
        (try
          Yojson.Safe.read_space p lb;
          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
        with Yojson.End_of_tuple -> end_of_tuple := true);
        x
      in
      if not !end_of_tuple then (
        try
          while true do
            Yojson.Safe.skip_json p lb;
            Yojson.Safe.read_space p lb;
            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
          done
        with Yojson.End_of_tuple -> ()
      );
      (x0, x1, x2, x3)
    with Yojson.End_of_tuple ->
      Ag_oj_run.missing_tuple_fields p !len [ 0; 1; 2; 3 ]);
)
let frame_t_of_string s =
  read_frame_t (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
