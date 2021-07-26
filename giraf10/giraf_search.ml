open PDBjTk
open Util
open Xml

let random_flag = false

type opt = {
    conninfo: string;
    infile: pdbfile_type;
    auth_asyms: string list;
    label_asyms: string list;
    giopt: Gi.opt;
    iropt: Ir.opt;
    query_id: string;
    nali: int;
    nshow: int;
    fmt: string;
    outdir: string;
  }

let get_opts () = 
  let conninfo = ref "" in
  let infile = ref (PDBF "") in
  let giopt = ref {Gi.opt_default with Gi.random = random_flag} in
  let iropt = ref {Ir.opt_default with Ir.reweight = true} in
  let auth_asyms = ref [] in
  let label_asyms = ref [] in
  let pdbset = ref [] in
  let qname = ref "query" in
  let nali = ref 100 in
  let nshow = ref 1000 in
  let fmt = ref "full" in
  let outdir = ref (Sys.getcwd ()) in
  let specs = [
    ("-conninfo",Arg.Set_string conninfo, "database connection info.");
    ("-ncores",Arg.Int (fun n -> Parmap.set_default_ncores n), "Number of CPU cores used.");
    ("-query", Arg.Set_string qname, " name of the query");
    ("-nali", Arg.Set_int nali, " number of alignments saved.");
    ("-nshow", Arg.Set_int nshow, " number of results shown.");
    ("-pdb",Arg.String (fun s -> infile := PDBF s)," input PDB file.");
    ("-pdbml",Arg.String (fun s -> infile := PDBML s)," input PDBML file.");
    ("-mmcif",Arg.String (fun s -> infile := MMCIF s)," input mmCIF file.");
    ("-pdbid",Arg.String (fun s -> infile := PDBID s)," input PDB ID.");
    ("-random",Arg.Unit (fun () -> 
      giopt := {!giopt with Gi.random = true};
      iropt := {!iropt with Ir.random = true}),
     "get random hits");
    ("-nlimit_gi", 
     Arg.Int (fun i -> giopt := {!giopt with Gi.nlimit = i}),
     " max # of hits for GI search.");
    ("-min_score_gi",
     Arg.Float (fun f -> giopt := {!giopt with Gi.min_score = f}),
     " minimum score for GI");
    ("-dcut_gi", 
     Arg.Float (fun f -> giopt := {!giopt with Gi.dcut = f}), " cut-off distance for GI search (2.0A).");
    ("-nlimit_ir",
     Arg.Int (fun i -> iropt := {!iropt with Ir.nlimit = i}),
     " max # of hits for IR procedure.");
    ("-min_score_ir",
     Arg.Float (fun f -> iropt := {!iropt with Ir.min_score = f}),
     " minimum score for IR");
    ("-dcut_ir", 
     Arg.Float (fun f -> iropt := {!iropt with Ir.dcut = f}), " cut-off distance for refined alignment (2.0A).");
    ("-rigid",
     Arg.Bool (fun f -> iropt := {!iropt with Ir.rigid = f}), " true for rigid alignment, false for flexible alignemnt (default: true)");
    ("-niter",
     Arg.Int (fun f -> iropt := {!iropt with Ir.niter = f}), " number of iteration for refining alignment (5)");
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
    ("-auth_asyms",
     Arg.String (fun s ->
       if s = "-" then ()
       else
	 let l = try String.nsplit s "," with _ -> [s] in
	 auth_asyms := l),
     " comma-separated chain IDs (auth_asym_id).");
    ("-label_asyms",
     Arg.String (fun s ->
       if s = "-" then ()
       else
	 let l = try String.nsplit s "," with _ -> [s] in
	 label_asyms := l),
     " comma-separated chain IDs (label_asym_id).");
    ("-pdbset",
     Arg.String (fun s -> 
       pdbset := if s = "-" then [] else String.nsplit s ","),
     "limit to particular PDB entries (comma-separated)");
    ("-fmt", Arg.Symbol (["full";"minimum"], (fun s -> fmt := s)), 
     " output format");
    ("-outdir", Arg.Set_string outdir, " output directory");
  ] in
  let usage = "giraf_search [options]" in
  Arg.parse specs (fun s -> infile := Unknown s) usage;
  let giopt = {!giopt with Gi.search = remove_dup_itype !giopt.Gi.search;
	       pdbset = !pdbset} in
  {conninfo= !conninfo;
   infile= !infile;
   auth_asyms= !auth_asyms; label_asyms = !label_asyms;
   giopt= giopt; iropt= !iropt; 
   query_id = !qname; nali = !nali; nshow = !nshow; fmt = !fmt;
   outdir = !outdir;}


let run_blast dblk = 
  let ht = Ht.create 300 in
  if true then ht  else (* for testing, just skip blastp *)
  try
    let bres = Blast.datablock_run_blastp
	" -db pdb_label_asym -outfmt 6 -evalue 10 -max_target_seqs 100000" dblk in
    let bres = Blast.parse_m8 bres in

    List.iter (fun b ->
      let qchain = snd(String.split b.Blast.q_id "_") in
      let did = List.last (String.nsplit b.Blast.s_id "|") in
      let p,c = String.split did "_" in
      Ht.add ht (qchain,p,c) (b.Blast.identity,b.Blast.evalue)) bres;
    ht
  with _ -> ht

let process_chain  asym conninfo giopt iropt atoms refsets =
  let ncores = Parmap.get_default_ncores () in
  let llrefsets = split_list (ncores*2) refsets in
  let gi_res = 
    Parmap.parfold
      (fun refsets gi_res -> 
	let conn = new Postgresql.connection ~conninfo () in
	Gi.init conn;
	let res = Gi.search conn ~opt:giopt atoms refsets in
	Gi.finish conn;
	conn#finish;
	List.rev_append res gi_res)
      (Parmap.L llrefsets)
      []
      List.rev_append in
  let pres = Gi.partition_results refsets gi_res in
  let llpres = split_list (ncores*2) pres in
  let ir_res = Parmap.parfold
      (fun pres ir_res -> 
	let conn = new Postgresql.connection ~conninfo () in
	let res = Ir.refine conn ~opt:iropt atoms refsets pres in
	conn#finish;
	List.rev_append res ir_res)
      (Parmap.L llpres)
      []
      List.rev_append in
  Ir.sort_results ir_res

exception Giraf_search_error of string

let main () = 
  try
    let opt = get_opts () in
    let dblk = Prep_struct.clean (Prepin.read_file opt.infile) in
    let blast_results = run_blast dblk in
    let orig_atoms = 
      Prep_struct.remove_water_atoms_guess 
	(Prep_struct.remove_hydrogens (PDBjUtil.atoms_of_datablock dblk)) in
    match Prepin.of_asyms opt.giopt.Gi.search 
	dblk opt.auth_asyms opt.label_asyms with
    | [] -> ()
    | lst ->
	let results = List.map 
                        (fun (asym,(atoms,refsets)) ->
	                  let ir_res = 
		            process_chain 
		              asym opt.conninfo opt.giopt opt.iropt
                              atoms refsets in
                          (atoms,ir_res))
            lst in
	if opt.fmt = "minimum"
	then
	  Save_results.dump_minimum opt.query_id results
	else
	  Save_results.dump_results 
	    opt.conninfo opt.outdir opt.query_id opt.nshow
	    orig_atoms blast_results results
  with 
  | PDBjUtil.UnknownFormat(s) -> 
      raise (Giraf_search_error ("Unkwon Format: " ^ s))
  | Xml.Error err ->
      raise (Giraf_search_error ("XML Error: " ^ (Xml.error err)))
  | Postgresql.Error err ->
      raise (Giraf_search_error ("Database Error: " ^ (Postgresql.string_of_error err)))

let _ = 
  try main ()
  with Giraf_search_error s ->
    let doc = Element("giraf_search",[],[Element("error",[],[PCData s])]) in
    print_endline (Xml.to_string_fmt doc)
