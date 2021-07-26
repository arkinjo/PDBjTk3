open Util

type opt = {
    outdir: string;
    infile: pdbfile_type;
    auth_asym: string;
    giopt: Gi.opt;
    iropt: Ir.opt;
    rank: int;
    query_id: string;
  }

let get_opts () = 
  let infile = ref (Unknown "") in
  let giopt = ref Gi.opt_default in
  let iropt = ref {Ir.opt_default with Ir.min_score = 0.0} in
  let auth_asym = ref "A" in
  let t_if_id = ref "" in
  let rank = ref 1 in
  let qname = ref "query" in
  let outdir = ref "." in
  let specs = [
    ("-outdir", Arg.Set_string outdir, " output directory");
    ("-query", Arg.Set_string qname, " name of the query");
    ("-pdb",Arg.String (fun s -> infile := PDBF s)," input PDB file.");
    ("-pdbml",Arg.String (fun s -> infile := PDBML s)," input PDBML file.");
    ("-rigid",
     Arg.Bool (fun f -> iropt := {!iropt with Ir.rigid = f}), " true for rigid alignment, false for flexible alignemnt (default: true)");
    ("-niter",
     Arg.Int (fun f -> iropt := {!iropt with Ir.niter = f}), " number of iteration for refining alignment (5)");
    ("-auth_asym",
     Arg.String (fun s -> auth_asym := s),
     " comma-separated chain IDs (auth_asym_id).");
    ("-if_id",
     Arg.String (fun s -> t_if_id := s), "if_id (interface) of the template");
    ("-rank", Arg.Set_int rank, "rank of aligned template");
    ("-itype",
     Arg.Symbol 
       (["ppi"; "peptide"; "dnarna"; "opoly"; "nonpolymer"], 
	(fun itype -> 
	  giopt := {!giopt with Gi.search = [itype_of_string itype]})),
     " interface type of the template");
  ]
  in
  let usage = "giraf_align [options] [pdbfile])" in
  Arg.parse specs (fun s -> infile := Unknown s) usage;
  {infile= !infile;
   outdir = !outdir;
   query_id = !qname; auth_asym= !auth_asym; 
   giopt = {!giopt with Gi.if_set = [!t_if_id]};
   rank = !rank;
   iropt= !iropt; 
  }

let _ = 
  try
    let opt = get_opts () in
    Giraf_clients.align opt.sock_addrs opt.outdir opt.infile opt.auth_asym
      opt.giopt opt.iropt opt.query_id opt.rank;
  with 
  | PG.Error e -> 
      Printf.fprintf stderr "PG.Error %s\n" (PG.string_of_error e);
      exit 1
  | e -> raise e
