open Util
open Printf
open ExtLib

type opt = {
    conninfo: string;
    ncores: int;
    giopt: Gi.opt;
    iropt: Ir.opt;
  }

let get_opts () = 
  let conninfo = ref "" in
  let ncores = ref 1 in
  let giopt = ref {Gi.opt_default with Gi.search = []} in
  let iropt = ref Ir.opt_default in
  let ifs_list = ref [] in
  let specs = [
    ("-conninfo",Arg.Set_string conninfo, "database connection info.");
    ("-ncores",Arg.Set_int ncores, "Number of CPU cores used.");
    ("-sub", Arg.String (fun s -> giopt := {!giopt with Gi.sub = s}),
     "subset of refaco table");
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
    ("-search",
     Arg.Symbol 
       (["ppi"; "peptide"; "dnarna"; "opoly"; "polymer"; 
	 "nonpolymer"; "all"; "small"; "fold"], 
	(function
	  | "fold" -> 
	      giopt := {!giopt with Gi.search = FOLD :: !giopt.Gi.search}
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
	  | "small" ->
	      giopt := {!giopt with Gi.search = [SMALL]}
	  | _ -> ())),
     " Protein-Protein, Protein-Ligand, or Both");
    ("-ifs_list", 
     Arg.String (fun s -> 
       let ic = open_in s in
       ifs_list := input_list ic;
       close_in ic),
     "A file containing a list of interface ID's");
  ] in
  let usage = "giraf_search [options] < list(if_id)" in
  Arg.parse specs (fun _ -> ()) usage;
  {conninfo= !conninfo;
   ncores= !ncores;
   giopt= {!giopt with Gi.if_set = !ifs_list}; 
   iropt= !iropt;}

let print_ir_results if_id ir_result =
  List.iteri
    (fun i r ->
      if if_id = r.Ir.if_id
      then ()
      else
        printf "%s,%s,%f,%f,%f,%f,%f,%f,%d,%d\n" 
        if_id r.Ir.if_id r.Ir.score r.Ir.gi_score
            r.Ir.rms r.Ir.drms r.Ir.tani r.Ir.seqid
            (List.length r.Ir.ali) r.Ir.ngi)
    ir_result;
  flush stdout

let run_giir conn giopt iropt if_id =
  match Interface.prepare_ifs conn if_id with
  | None -> ()
  | Some(atoms,refsets) -> 
      let res = Gi.search conn ~opt:giopt atoms refsets in
      let pres = Gi.partition_results refsets res in
      let res = Ir.refine conn ~opt:iropt atoms refsets pres in
      print_ir_results if_id res

let _ = 
  try
    let t0 = Unix.gettimeofday () in
    let opt = get_opts () in
    let if_list = input_list stdin in
    begin
      match opt.ncores with
      | 1 ->
	  let conn = new Postgresql.connection ~conninfo:opt.conninfo () in
	  Gi.init conn;
	  List.iter (run_giir conn opt.giopt opt.iropt) if_list;
	  Gi.finish conn
      | ncores -> 
	  let llif_ids = split_list_chunk 100 if_list in
	  Parmap.pariter ~ncores 
	    (fun lif_ids -> 
	      let conn = new Postgresql.connection ~conninfo:opt.conninfo () in
	      Gi.init conn;
	      List.iter (run_giir conn opt.giopt opt.iropt) lif_ids;
	      Gi.finish conn;
	      conn#finish)
	    (Parmap.L llif_ids)

    end;
    let t1 = Unix.gettimeofday () in
    fprintf stderr "Elapsed time %8.3f sec.\n" (t1 -. t0); 
    flush stderr
  with 
  | PG.Error e -> 
      fprintf stderr "PG.Error %s\n" (PG.string_of_error e);
      exit 1
  | e -> raise e
