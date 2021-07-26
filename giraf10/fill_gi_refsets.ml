open Util
open PDBjTk
open PDBjBasis
open Printf

let main () = 
  let dcut = ref 1.5 in
  let dbname = ref "giraf9" in
  let if_id = ref "" in
  let specs = [
    ("-dcut",Arg.Set_float dcut, "cutoff distance (default: 1.5)");
    ("-dbname",Arg.Set_string dbname, "database (default: giraf9)")] in
  let usage = "fill_gi_refsets [options] if_id" in
  Arg.parse specs (fun s -> if_id := s) usage;
  if !if_id = "" then  (Arg.usage specs usage; exit 1) else ();
  let table = "_Refsets_sample1" in
  let dbname = !dbname in
  let conn = new Postgresql.connection ~dbname () in
  let giopt = {Gi.opt_default with Gi.dcut = !dcut} in
  let if_id = !if_id in
  begin
    match Interface.prepare_ifs conn if_id with
    | None -> 
	fprintf stderr "No refsets found for %s\n" if_id;
	flush stderr
    | Some (atoms,refsets) ->
	Gi.fill_refsets conn ~temp:false ~table giopt atoms refsets
  end;
  conn#finish

let _ = 
  try
    main ()
  with
  | Postgresql.Error err ->
      fprintf stderr "%s\n" (Postgresql.string_of_error err);
      flush stderr;
      exit 2
  | exc ->
      fprintf stderr "Error: %s\n" (Printexc.to_string exc);
      flush stderr;
      exit 3
