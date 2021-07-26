(* Auto-generated from "frame.atd" *)


type vec3_t = Frame_t.vec3_t

type frame_t = Frame_t.frame_t

val write_vec3_t :
  Bi_outbuf.t -> vec3_t -> unit
  (** Output a JSON value of type {!vec3_t}. *)

val string_of_vec3_t :
  ?len:int -> vec3_t -> string
  (** Serialize a value of type {!vec3_t}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_vec3_t :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> vec3_t
  (** Input JSON data of type {!vec3_t}. *)

val vec3_t_of_string :
  string -> vec3_t
  (** Deserialize JSON data of type {!vec3_t}. *)

val write_frame_t :
  Bi_outbuf.t -> frame_t -> unit
  (** Output a JSON value of type {!frame_t}. *)

val string_of_frame_t :
  ?len:int -> frame_t -> string
  (** Serialize a value of type {!frame_t}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_frame_t :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> frame_t
  (** Input JSON data of type {!frame_t}. *)

val frame_t_of_string :
  string -> frame_t
  (** Deserialize JSON data of type {!frame_t}. *)

