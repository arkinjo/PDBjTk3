open Util

type opt = {
    conninfo: string;
    giopt: Gi.opt;
    iropt: Ir.opt;
    sock_addrs: Unix.sockaddr list;
  }

let get_opts () = 
  let conninfo = ref "" in
  let giopt = ref Gi.opt_default in
  let iropt = ref {Ir.opt_default with Ir.min_score = 0.0} in
  let specs = [
    ("-conninfo",Arg.Set_string conninfo," Postgres conninfo");
    ("-rigid",
     Arg.Bool (fun f -> iropt := {!iropt with Ir.rigid = f}), " true for rigid alignment, false for flexible alignemnt (default: true)");
    ("-niter",
     Arg.Int (fun f -> iropt := {!iropt with Ir.niter = f}), " number of iteration for refining alignment (5)");
    ("-itype",
     Arg.Symbol 
       (["ppi"; "peptide"; "dnarna"; "opoly"; "nonpolymer"; "small"], 
        (fun itype -> 
          giopt := {!giopt with Gi.search = [itype_of_string itype]})),
     " interface type of the template");
  ] in
  let usage = "align_ifs [options] < (if_id1,if_id2)" in
  Arg.parse specs (fun _ -> ()) usage;
  {conninfo = !conninfo; giopt = !giopt; iropt = !iropt; 
   sock_addrs = [socket_of_host_port "localhost" default_port]}


let main () =
  let opt = get_opts () in
  Giraf_clients.align_ifs opt.conninfo opt.sock_addrs "." opt.giopt opt.iropt

let _ = 
  try
    main ()
  with 
  | PG.Error e -> 
      Printf.fprintf stderr "PG.Error %s\n" (PG.string_of_error e);
      exit 1
  | e -> raise e
