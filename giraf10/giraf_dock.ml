open Util
open Printf
open PDBjTk
open PDBjBasis

type opt = {
    hosts: string list;
    port: int;
    sock_addrs: Unix.sockaddr list;
    infile1: pdbfile_type;
    auth_asyms1: string list;
    infile2: pdbfile_type;
    auth_asyms2: string list;
    giopt: Gi.opt;
    iropt: Ir.opt;
    query_name: string;
    nali: int;
  }

let get_opts () = 
  let hosts = ref ["localhost"] in
  let port = ref 7789 in
  let infile1 = ref (PDBF "") in
  let infile2 = ref (PDBF "") in
  let pdbset = ref [] in
  let giopt = ref {Gi.opt_default with Gi.search = [PPI]} in
  let iropt = ref Ir.opt_default in
  let auth_asyms1 = ref [] in
  let auth_asyms2 = ref [] in
  let qname = ref "query" in
  let nali = ref 100 in
  let specs = [
    ("-hosts", Arg.String (fun s -> hosts := String.nsplit s ","),
     "hosts of GIRAF servers (comma-separated)");
    ("-port", Arg.Set_int port, " port of server(s)");
    ("-query", Arg.Set_string qname, " name of the query");
    ("-nali", Arg.Set_int nali, " number of alignments saved.");
    ("-nlimit_gi",
     Arg.Int (fun i -> giopt := {!giopt with Gi.nlimit = i}),
     " max # of hits for GI search.");
    ("-min_score_gi",
     Arg.Float (fun f -> giopt := {!giopt with Gi.min_score = f}),
     " minimum score for GI");
    ("-nlimit_ir",
     Arg.Int (fun i -> iropt := {!iropt with Ir.nlimit = i}),
     " max # of hits for IR procedure.");
    ("-min_score_ir",
     Arg.Float (fun f -> iropt := {!iropt with Ir.min_score = f}),
     " minimum score for IR");
    ("-pdb1", Arg.String (fun s -> infile1 := Unknown s), "pdbfile1");
    ("-pdb2", Arg.String (fun s -> infile2 := Unknown s), "pdbfile2");
    ("-auth_asyms1",
     Arg.String (fun s ->
       if s = "*" then ()
       else
	 let l = try String.nsplit s "," with _ -> [s] in
	 auth_asyms1 := l),
     " comma-separated chain IDs (auth_asym_id).");
    ("-auth_asyms2",
     Arg.String (fun s ->
       if s = "*" then ()
       else
	 let l = try String.nsplit s "," with _ -> [s] in
	 auth_asyms2 := l),
     " comma-separated chain IDs (auth_asym_id).");
    ("-pdbset",
     Arg.String (fun s ->
       pdbset := if s = "-" then [] else String.nsplit s ","),
     "limit to particular PDB entries (comma-separated)");

  ] in
  let usage = "giraf_search [options]" in
  Arg.parse specs (fun _ -> ()) usage;
  {hosts= !hosts; port= !port; 
   sock_addrs = List.map (fun h -> socket_of_host_port h !port) !hosts;
   infile1= !infile1; infile2 = !infile2; 
   auth_asyms1= !auth_asyms1; auth_asyms2 = !auth_asyms2;
   giopt= {!giopt with Gi.pdbset = !pdbset}; iropt= !iropt; 
   query_name = !qname; nali = !nali}

let main () =
  let t0 = Unix.gettimeofday () in
  let opt = get_opts () in
  let dblk1 = Prepin.read_file opt.infile1 in
  let dblk2 = Prepin.read_file opt.infile2 in
  let ent1 = Prepin.of_asyms [] dblk1 opt.auth_asyms1 [] in
  let ent2 = Prepin.of_asyms [] dblk2 opt.auth_asyms2 [] in
  (match ent1,ent2 with
  | [], _ | _, [] -> 
      prerr_endline "no applicable coordinates."; flush stderr
  | (_,ars1)::_, (_,ars2)::_ ->
      let atoms1 = fst ars1 and atoms2 = fst ars2 in
      let pool = open_connection_pool opt.sock_addrs in
      let ir_res1 = Giir.collect pool opt.giopt opt.iropt ars1 in
      let ir_res2 = Giir.collect pool opt.giopt opt.iropt ars2 in
      let atoms1 = Save_results.map_frequency atoms1 atoms1 ir_res1 in
      let atoms2 = Save_results.map_frequency atoms2 atoms2 ir_res2 in
      let results = Giir.dock pool opt.iropt ir_res1 ir_res2 in
      Save_results.dump_dock_model 
	opt.nali opt.query_name pool atoms1 atoms2 results;
      shutdown_connection_pool pool
  );
  let t1 = Unix.gettimeofday () in
  fprintf stderr "Elapsed time %8.3f sec.\n" (t1 -. t0); 
  flush stderr;
  ()

let _ = 
  try
    main ()
  with 
  | PG.Error e -> 
      fprintf stderr "PG.Error %s\n" (PG.string_of_error e);
      exit 1
  | e -> raise e
