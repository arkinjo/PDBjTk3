open Util
open Printf
open ExtLib
module PG = Postgresql
module Ht = Hashtbl

type opt = {
    conninfo: string;
    outdir: string;
    max_struct: int;
    giopt: Gi.opt;
    iropt: Ir.opt;
    itype: string;
    hosts: string list;
    port: int;
    sock_addrs: Unix.sockaddr list;
  }

let get_opts () = 
  let giopt = ref {Gi.opt_default with Gi.search = []} in
  let iropt = ref Ir.opt_default in
  let hosts = ref ["localhost"] in
  let itype = ref "" in
  let conninfo = ref "" in
  let outdir = ref "" in
  let max_struct = ref 10 in
  let port = ref 7789 in
  let specs = [
    ("-conninfo",Arg.String (fun s -> conninfo := s), "Database connection");
    ("-outdir",Arg.String (fun s -> outdir := s), "Output directory");
    ("-hosts", Arg.String (fun s -> hosts := String.nsplit s ","),
     "hosts of GIRAF servers (comma-separated)");
    ("-max_struct",
     Arg.Int (fun i -> max_struct := i),
     "  # of output structures.");

    ("-port",Arg.Set_int port," port for giraf_core_server");
    ("-nlimit_gi",
     Arg.Int (fun i -> giopt := {!giopt with Gi.nlimit = i}),
     " max # of hits for GI search.");
    ("-min_score_gi",
     Arg.Float (fun f -> giopt := {!giopt with Gi.min_score = f}),
     " minimum score for GI");
    ("-dcut_gi", 
     Arg.Float (fun f -> giopt := {!giopt with Gi.dcut = f}), 
     " cut-off distance for GI search (2.0A).");
    ("-nlimit_ir",
     Arg.Int (fun i -> iropt := {!iropt with Ir.nlimit = i}),
     " max # of hits for IR procedure.");
    ("-min_score_ir",
     Arg.Float (fun f -> iropt := {!iropt with Ir.min_score = f}),
     " minimum score for IR");
    ("-rigid",
     Arg.Bool (fun f -> iropt := {!iropt with Ir.rigid = f}), " true for rigid alignment, false for flexible alignemnt (default: true)");
    ("-niter",
     Arg.Int (fun f -> iropt := {!iropt with Ir.niter = f}), " number of iteration for refining alignment (5)");
    ("-dcut_ir", 
     Arg.Float (fun f -> iropt := {!iropt with Ir.dcut = f}), 
     " cut-off distance for refined alignment (2.0A).");
    ("-itype",
     Arg.Symbol 
       (["ppi"; "nonpolymer"; "dnarna"], 
	(function
	  | "ppi" -> 
	      giopt := {!giopt with Gi.search = [PPI]};
	      itype := "ppi";
	  | "dnarna" ->
	      giopt := {!giopt with Gi.search = [DNARNA]};
	      itype := "dnarna";
	  | "nonpolymer" ->
	      giopt := {!giopt with Gi.search = [NONPOLYMER]};
	      itype := "nonpolymer";
	  | _ -> ())),
     " interface type");
  ] in
  let usage = "fit_emotif [options]" in
  Arg.parse specs (fun _ -> ()) usage;
  {conninfo= !conninfo; 
   outdir= !outdir;
   max_struct = !max_struct;
   giopt= !giopt; iropt= !iropt; 
   itype = !itype;
   hosts= !hosts; port = !port;
   sock_addrs = List.map (fun h -> socket_of_host_port h !port) !hosts;}

let get_emotifs opt conn = 
  let q = "SELECT clust_id, if_id FROM clusters WHERE itype = $1 ORDER BY nedge" in
  let res = Sql.select conn ~params:[|opt.itype|] q in
  let ht = Ht.create 10000 in
  Array.iter (fun t -> 
    let l = Ht.find_default ht t.(0) [] in
    Ht.replace ht t.(0) (t.(1)::l))
    res#get_all;
  ht

let process_emotif opt conn pool clust_id if_list =
  match if_list with
  | [] -> ()
  | (h1::rest) as lst0 -> 
      let lst = List.take opt.max_struct lst0 in
      let giopt = {opt.giopt with Gi.if_set = lst} in
      fprintf stderr "process_emotif: %s %s\t%d\n" 
	clust_id h1 (List.length lst0);
      flush stderr;
      let outdir = Filename.concat opt.outdir clust_id in
      if Sys.file_exists outdir
      then ()
      else Unix.mkdir outdir 0o755;
      match Giraf_clients.prepare_align_ifs conn h1 with
      | None -> fprintf stderr "empty atoms: %s\n" h1; exit 1
      | Some(atoms,refsets,orig_atoms,pdbid) ->
	  let ir_result = Giir.collect pool giopt opt.iropt (atoms,refsets) in
	  let ir_result = List.mapi 
	      (fun i r -> {r with Ir.rank = i+1}) ir_result in
	  Save_results.dump_alignment pool outdir clust_id atoms ir_result

let _ = 
  try
    let t0 = Unix.gettimeofday () in
    let opt = get_opts () in
    let pool = open_connection_pool opt.sock_addrs in
    let conn = new PG.connection ~conninfo:opt.conninfo () in
    let emotifs = get_emotifs opt conn in
    Ht.iter (process_emotif opt conn pool) emotifs;
    let t1 = Unix.gettimeofday () in
    conn#finish;
    fprintf stderr "Elapsed time %8.3f sec.\n" (t1 -. t0); 
    flush stderr;
  with 
  | PG.Error e -> 
      fprintf stderr "PG.Error %s\n" (PG.string_of_error e);
      exit 1
  | e -> raise e
