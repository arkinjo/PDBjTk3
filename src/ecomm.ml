(** external commands *)
open Printf

(** gzip *)
let gzip = "gzip"

(** BLAST *)
(* new version *)
let blastp = "blastp"
let psiblast = "psiblast"
let rpsblast = "rpsblast"

(* legacy version *)
let blastpgp = "blastpgp"
let blastall = "blastall"

let processor command writer reader =
  let ic,oc = Unix.open_process command in
  writer oc;
  let result = reader ic in
  match Unix.close_process(ic,oc) with
  | Unix.WEXITED _ -> result
  | Unix.WSIGNALED i -> 
      fprintf stderr "Ecomm.processor: killed by signal %d\n" i;
      flush stderr;
      exit i
  | Unix.WSTOPPED i ->
      fprintf stderr "Ecomm.processor: stopped by signal %d\n" i;
      flush stderr;
      exit i

let open_gzipped_file file = 
  let comm = String.concat " " [gzip; "--decompress"; "--stdout"; file] in
  let ic,oc = Unix.open_process comm in
  ic,oc

let close_gzip ic oc = ignore(Unix.close_process (ic,oc))

(* this is faster than using Zlib (camlzip) library... *)
let read_gzipped reader file =
  let ic,oc = open_gzipped_file file in
  let result = reader ic in
  close_gzip ic oc;
  result

let read_gzipped_maybe reader file =
  if Filename.check_suffix file "gz" 
  then read_gzipped reader file
  else 
    let ic = open_in file in
    let doc = reader ic in
    close_in ic;
    doc
