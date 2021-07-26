open Printf
open PDBjTk
open PDBjBasis
open Util

type opts = {
    conninfo: string;
    ncores: int;
    pdblist: string list;
    itypes: itype list;
  }

let get_opts () = 
  let conninfo = ref "" in
  let ncores = ref 1 in
  let flist = ref "" in
  let itypes = ref [NONPOLYMER; PPI; DNARNA] in
  let specs = [
    ("-conninfo",Arg.Set_string conninfo, "database connection info.");
    ("-ncores",Arg.Set_int ncores, "Number of CPU cores used.");
    ("-itype",
     Arg.Symbol 
       (["ppi"; "peptide"; "dnarna"; "opoly" ; "nonpolymer";
	 "ligands"; "sse"; "all"], 
	(function
	  | "ppi" -> itypes := PPI :: !itypes
	  | "peptide" -> itypes := PEPTIDE :: !itypes
	  | "dnarna" -> itypes := DNARNA :: !itypes
	  | "opoly" -> itypes := OPOLY :: !itypes
	  | "nonpolymer" -> itypes := NONPOLYMER :: !itypes
	  | "ligands" -> itypes := [ PEPTIDE; DNARNA; OPOLY; NONPOLYMER] @ !itypes
	  | "all" -> itypes := itype_all
	  | _ -> ())),
     " load ppi, polymer, nonpolymer, or all");
   ] in
  let usage = "giraf_load [options] list_of_pdbids" in
  Arg.parse specs (fun s -> flist := s) usage;
  let lst = 
    if !flist = "" then []
    else
      let ic = open_in !flist in
      let l = Std.input_list ic in
      close_in ic;
      List.map String.lowercase l
  in
  {conninfo= !conninfo; 
   ncores= !ncores; 
   pdblist= lst; itypes= remove_dup_itype !itypes;}


let main () =
  let t0 = Unix.gettimeofday () in
  let opt = get_opts () in
  let conn = new Postgresql.connection ~conninfo:opt.conninfo () in
  let ht_ok = Load_data.delete_obsolete conn in
  let pdblist = List.filter (Ht.mem ht_ok) opt.pdblist in
  begin
    match opt.ncores with
    | 1 ->  List.iter (Load_data.load conn opt.itypes) pdblist
    | ncores -> 
	let llpdbid = split_list_chunk 50 pdblist in
	Parmap.pariter ~ncores 
	  (fun pdbids -> 
	    let conn = new Postgresql.connection ~conninfo:opt.conninfo () in
	    List.iter (Load_data.load conn opt.itypes) pdbids;
	    conn#finish)
	  (Parmap.L llpdbid)
  end;
  conn#finish;
  let t1 = Unix.gettimeofday () in
  fprintf stderr "## Elapsed time = %f sec.\n" (t1 -. t0);
  flush stderr

let _ = 
  try
    main ()
  with
  | PG.Error e -> 
      fprintf stderr "PG.Error %s\n" (PG.string_of_error e);
      exit 1
  | exc -> raise exc
