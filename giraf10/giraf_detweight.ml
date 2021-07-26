open Util
open Printf
open PDBjTk
open PDBjBasis

let giopt = {Gi.opt_default with 
	     Gi.random = false; 
	     max_score = 90.0;
	     search = [];
	   }

let iropt = {Ir.opt_default with Ir.min_score = 10.0;}

type opt = {
    reps: (string * string) list;
    hosts: string list;
    port: int;
    sock_addrs: Unix.sockaddr list;
    giopt: Gi.opt;
    iropt: Ir.opt;
  }

let get_opts () = 
  let hosts = ref ["localhost"] in
  let port = ref 7789 in
  let pdbset = ref "" in
  let giopt = ref giopt in
  let iropt = ref iropt in
  let specs = [
    ("-hosts",Arg.String (fun s -> hosts := String.nsplit s ","),
     "hosts of GIRAF servers (comma-separated)");
    ("-port", Arg.Set_int port, " port of server(s)");
    ("-pdbset", Arg.Set_string pdbset, "file containing a list of target PDB entries");
    ("-gi_sub",
     Arg.String (fun s -> giopt := {!giopt with Gi.sub = s}),
     " Sub-table ID for GI Search (0,1,..). Default '' (none).");
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
    ("-dcut_ir", 
     Arg.Float (fun f -> iropt := {!iropt with Ir.dcut = f}), 
     " cut-off distance for refined alignment (2.0A).");
    ("-search",
     Arg.Symbol 
       (["ppi"; "peptide"; "dnarna"; "opoly"; "polymer"; 
	 "nonpolymer"; "all"; "ligands"], 
	(function
	  | "ppi" -> 
	      giopt := {!giopt with Gi.search = PPI :: !giopt.Gi.search}
	  | "peptide" ->
	      giopt := {!giopt with Gi.search = PEPTIDE :: !giopt.Gi.search}
	  | "dnarna" ->
	      giopt := {!giopt with Gi.search = DNARNA :: !giopt.Gi.search}
	  | "opoly" ->
	      giopt := {!giopt with Gi.search = OPOLY :: !giopt.Gi.search}
	  | "polymer" ->
	      giopt := {!giopt with Gi.search = [PEPTIDE; DNARNA; OPOLY]}
	  | "nonpolymer" ->
	      giopt := {!giopt with Gi.search = NONPOLYMER :: !giopt.Gi.search}
	  | "all" ->
	      giopt := {!giopt with 
			Gi.search = [PPI; PEPTIDE; DNARNA; OPOLY; NONPOLYMER]}
	  | "ligands" ->
	      giopt := {!giopt with 
			Gi.search = [PEPTIDE; DNARNA; OPOLY; NONPOLYMER]}
	  | _ -> ())),
     " Protein-Protein, Protein-Ligand, or Both")
  ] in
  let usage = "giraf_detweight [options]" in
  let infile = ref "" in
  Arg.parse specs (fun s -> infile := s) usage;
  let lst = 
    if !infile = "" then []
    else
      let ic = open_in !infile in
      let l = Std.input_list ic in
      close_in ic;
      List.map (fun l -> 
	let m = String.nsplit l " " in
	(List.hd m),(List.nth m 1)) l
  in
  let plst = 
    if !pdbset = "" then [] 
    else
      let ic = open_in !infile in
      let l = Std.input_list ic in
      close_in ic;
      List.map String.uppercase l
  in
  {reps = lst; 
   port= !port; hosts= !hosts; 
   sock_addrs = List.map (fun h -> socket_of_host_port h !port) !hosts;
   giopt = {!giopt with Gi.pdbset = plst} ; iropt = !iropt;
 }

let print_prob nreps htsave = 
  let ntot = float_of_int nreps in
  Ht.iter (fun (if_id) v -> 
    let p = 1.0 -. v /. ntot in
    printf "%s\t%e\t%e\t%e\n" if_id p v ntot) htsave

let main () =
  let t0 = Unix.gettimeofday () in
  let opt = get_opts () in
  fprintf stderr "Number of representatives: %d\n" (List.length opt.reps);
  flush stderr;
  let htsave = Ht.create 1000 in
  let pool = open_connection_pool opt.sock_addrs in
  let pdbmldir = Load_data.get_pdbmldir () in
  let nreps = ref 0 in
  List.iteri 
    (fun i (pdbid,asym) ->
      fprintf stderr "%3d %s %s\n" (i+1) pdbid asym; flush stderr;
      let infile = Filename.concat pdbmldir (pdbid ^ ".xml.gz") in
      if Sys.file_exists infile then
	let dblk = PDBML.parse_file infile in
	match Prepin.of_asyms opt.giopt.Gi.search dblk [asym] [] with
	| [] -> ()
	| (_,ars)::_ ->
	    let t0 = Unix.gettimeofday () in
	    let ir_result = Giir.collect pool opt.giopt opt.iropt ars in
	    incr nreps;
	    let t1 = Unix.gettimeofday () in
	    fprintf stderr "GIIR done ( %5d %8.3f sec ).\n" 
	      (List.length ir_result) (t1 -. t0); 
	    flush stderr;
	    List.iter 
	      (fun r -> 
		let key = r.Ir.if_id in
		let n = Ht.find_default htsave key 0.0 in
		Ht.replace htsave key (n +. 1.0))
	      ir_result
      else 
	begin
	  fprintf stderr "PDBML file: %s does not exist!\n" infile;
	  flush stderr;
	end)
    opt.reps;
  print_prob !nreps htsave;
  let t1 = Unix.gettimeofday () in
  shutdown_connection_pool pool;
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
