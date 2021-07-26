open ExtLib
open Printf
module PG = Postgresql
module Ht = Hashtbl

type opt = 
    { itype: string;
      min_score: float;
      min_count: int;
      conninfo: string;
    }

let get_opts () =
  let opt = ref { itype = "small"; min_score = 10.0; min_count = 10; conninfo = "dbname=giraf9" } in
  let specs = [
    ("-conninfo", Arg.String (fun s -> opt := {!opt with conninfo = s}), " DB connection");
    ("-itype", Arg.String (fun s -> opt := {!opt with itype = s}), " interface type");
    ("-min_score", Arg.Float (fun s -> opt := {!opt with min_score = s}), " minimum score");
    ("-min_count", Arg.Int (fun s -> opt := {!opt with min_count = s}), " minimum count");
  ] in
  let usage = sprintf "%s [options]" Sys.argv.(0) in
  Arg.parse specs (fun _ -> ()) usage;
  !opt
   

let main0 opt = 
  let conn = new PG.connection ~conninfo:opt.conninfo () in
  let query = Gi.make_query conn Gi.opt_default
      (Util.itype_of_string opt.itype) in
  let f,nquery = String.replace ~str:query ~sub:"TRefsets" ~by:"_refsets_sample1" in
  printf "EXPLAIN ANALYZE\n";
  print_string nquery;
  print_endline ";";
  ()

let _ = 
  let opt = get_opts () in
  main0 opt 
