open ExtLib

let run blast options fasta =
  let comm = blast ^ " " ^ options in
  Ecomm.processor comm
    (fun oc -> output_string oc fasta; close_out oc)
    input_all

let run_blastpgp = run Ecomm.blastpgp
let run_blastp = run Ecomm.blastp
let run_psiblast = run Ecomm.psiblast

(*
let run_blastp = run (Ecomm.blastpgp ^ " -p blastp")
*)

type m8 = {
    q_id: string;
    s_id: string;
    identity: float;
    alen: int;
    mismatches: int;
    gaps: int;
    q_start: int;
    q_end: int;
    s_start: int;
    s_end: int;
    evalue: float;
    bit_score: float;
  }

let parse_m8 odata =
  List.filter_map 
    (fun l ->
      if String.starts_with l "#" then None
      else
	let chk = Array.of_list (String.nsplit l "\t") in
	if Array.length chk <> 12 then None
	else
	  Some { q_id = chk.(0);
		 s_id = chk.(1);
		 identity = float_of_string chk.(2);
		 alen = int_of_string chk.(3);
		 mismatches = int_of_string chk.(4);
		 gaps = int_of_string chk.(5);
		 q_start = int_of_string chk.(6);
		 q_end = int_of_string chk.(7);
		 s_start = int_of_string chk.(8);
		 s_end = int_of_string chk.(9);
		 evalue = float_of_string chk.(10);
		 bit_score = float_of_string chk.(11);})
    (String.nsplit odata "\n")

let fasta_of_datablock dblk =
  let atoms = PDBjUtil.get_calpha (PDBjUtil.atoms_of_datablock dblk) in
  let asyms = PDBjUtil.split_atoms2asyms atoms in
  let asyms = List.filter (fun (_,l) -> PDBjUtil.guess_is_protein l) asyms in
  let proc (label,atoms) =
    let seq = String.concat ""
	(List.map (fun a ->
	  PDBjUtil.amino3to1 a.Atom.label_comp_id) atoms) in
    Printf.sprintf ">query_%s\n%s" label seq
  in
  let seqs = List.map proc asyms in
  String.concat "\n" seqs

let datablock_run blast options dblk =
  let fasta = fasta_of_datablock dblk in
  run blast options fasta

let datablock_run_blastpgp = datablock_run Ecomm.blastpgp
let datablock_run_blastp = datablock_run Ecomm.blastp
