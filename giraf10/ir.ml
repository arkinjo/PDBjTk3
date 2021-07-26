(** iterative refinement of atomic alignments *)
open Util
open Printf
open PDBjTk
open PDBjBasis
open Graph

let debug = false
let warn s = if debug then ( prerr_endline ("Ir: " ^ s); flush stderr) else ()

let ir_alpha = 3.12
let ir_beta = 0.386
let get_pvalue (itype: itype) x = 
  let a,b = 
    match itype with
    | PPI -> 1.044833,2.104835
    | PEPTIDE -> 1.554593,1.650020
    | DNARNA -> 1.184648,1.671053
    | OPOLY -> 1.893212,1.290463
    | NONPOLYMER -> 1.664845,1.339933
    | SMALL -> 1.0,1.0
    | FOLD -> 1.0,1.0
  in
  Gsl.Cdf.gamma_Q ~a ~b ~x

type bplabel = Q | T

(* cut-off distance for atoms included in alignment process *)

type opt = {
    min_score: float; 
    min_score0: float;
    dcut: float;
    drad: float;
    nlimit: int;
    reweight: bool;
    random: bool;
    rigid: bool;
    redundant: bool;
    niter: int;
  }

let opt_default = 
  {min_score=10.0; min_score0=10.0; dcut= 2.5; drad=30.0; 
   nlimit=max_int; reweight=false; random=false;
   rigid=true; redundant=false; niter=15}

type result = {
    rank: int;
    itype: itype;
  if_id: string; 
    score: float;
    gi_score: float;
    ngi: int;
    ali: (int * int) list;
    rms: float;
    drms: float;
    tani: float;
    seqid: float;
    nali_res: int; (* number of aligned RESIDUES (instead of ATOMS) *)
    pvalue: float;
  }

let cmp_result a b =
  if a.score > b.score then -1
  else if a.score < b.score then 1
  else if a.gi_score > b.gi_score then -1
  else if a.gi_score < b.gi_score then 1
  else if a.rms < b.rms then -1
  else if a.rms > b.rms then 1
  else 0

let remove_redundancy results =
  let ht = Ht.create 100 in
  List.iter 
    (fun r ->
      let key = r.if_id,r.ali in
      Ht.replace ht key r)
    results;
  Ht.fold (fun _ r l -> r::l) ht []

let sort_results ?(random=false) ?(rerank=true) results = 
  let cmp = 
    if random then fun _ _ -> Random.int 10000 - Random.int 10000 
    else cmp_result in
  List.mapi (fun i r -> if rerank then {r with rank = i + 1} else r) 
    (List.sort ~cmp results)

module V = struct
  type t = bplabel * int
  let compare (s1,i1) (s2,i2) = 
    if s1 = s2 then i1 - i2
    else if s1 = Q then -1
    else 1
  let hash = Hashtbl.hash
  let equal (s1,i1) (s2,i2) = 
    s1 = (s2: bplabel) && i1 = (i2:int)
end

module E = struct
  type t = float
  let default = 0.0
  let compare = (compare : float -> float -> int)
end

module W = struct
  type label = float
  type t = float
  let weight l = (l : t)
  let zero = 0.0
  let infinity = infinity
  let add = ( +. )
  let sub = ( -. )
  let compare = (compare : float -> float -> int)
  let equal = (( = ) : float -> float -> bool)
end

module G = Imperative.Graph.ConcreteLabeled (V) (E)
module Wbm = Wbm.Hungarian.I (G) (W)
module CompoBPG = Components.Make(G)

let split_vertices qlab tlab graph =
  G.fold_vertex 
    (fun v (q,t) ->
      match fst v with 
      | lab when lab = qlab -> v::q , t 
      | lab when lab = tlab -> q, v::t 
      | _ -> q,t)
    graph ([],[])

module IAco = struct
  type t = int * ((string * int * int * int) * (float * float * float))
  let coord (_,(_,co)) = co
end

module IntCellIAco = IntCell.Make (IAco)

let weight_edge dcut qaco taco = 
  let (_,_,_,qa),qco = qaco and (_,_,_,ta),tco = taco in
  if qa <> ta then 0.0
  else 
    let d = Vec3.distance qco tco in
    if d >= dcut then 0.0
    else 1.0 -. d /. dcut


let make_graph_bs opt qaco qlab taco tlab =
  let qacoi = Array.mapi (fun i qaco0 -> (i,qaco0)) qaco in
  let nq = Array.length qaco in
  let cell = IntCellIAco.make ~size:opt.dcut (Array.to_list qacoi) in
  let g = G.create () in
  Array.iteri 
    (fun j taco0 -> 
      let tco = snd taco0 in
      let nn = IntCellIAco.neighbors_coord cell tco in
      List.iter 
	(fun (i,qaco0) ->
	  let w = weight_edge opt.dcut qaco0 taco0 in
	  if i >= nq then warn (sprintf "make_graph: i >= nq: %d %d" i nq);
	  if w > 0.0 then 
	    G.add_edge_e g ((qlab,i),w,(tlab,j)))
	nn)
    taco;
  g

let make_graph_fold opt qaco qlab taco tlab =
  let nq = Array.length qaco in
  let nt = Array.length taco in
  let ht = Ht.create nq in
  let htd = Ht.create nq in
  let len = 5 in
  let dcut = 5.0 in
  let hlen = len / 2 in
  let check_seg i j =
    let b = ref true in
    for k = -hlen to hlen do
      let qi,qaco = qaco.(i+k) in
      let ti,taco = taco.(j+k) in
      let c = Ht.find_default htd (qi,ti) (Vec3.distance qaco taco < dcut) in
      Ht.replace htd (qi,ti) c;
      b := !b && c
    done;
    if !b then
      for k = -hlen to hlen do
	let key = ((qlab,i+k),(tlab,j+k)) in
	let w = Ht.find_default ht key 0.0 in
	let w = w +. weight_edge dcut qaco.(i+k) taco.(j+k) in
	Ht.replace ht key w
      done;
  in
  let qacoi = Array.mapi (fun i qaco0 -> (i,qaco0)) qaco in
  let cell = IntCellIAco.make ~size:dcut (Array.to_list qacoi) in
  for j = hlen to nt - hlen - 1 do
    let tco = snd taco.(j) in
    let nn = IntCellIAco.neighbors_coord cell tco in
    List.iter 
      (fun (i,qaco0) ->
	if i >= hlen && i <= nq - hlen - 1 then
	  check_seg i j)
      nn
  done;
  let g = G.create () in
  let flen = 1.0 /. float len in
  Ht.iter (fun (q,t) w -> G.add_edge_e g (q,(w *. flen),t)) ht;
  g

let make_graph itype =
  if itype = FOLD 
  then make_graph_fold 
  else make_graph_bs

let get_drms amol bmol =
  let n = Array.length amol in
  let n1 = n - 1 and n2 = n - 2 in
  let dev = ref 0.0 in
  for i = 0 to n2 do
    let co1i = amol.(i) and co2i = bmol.(i) in
    for j = i + 1 to n1 do
      let co1j = amol.(j) and co2j = bmol.(j) in
      let d1 = Vec3.distance co1i co1j in
      let d2 = Vec3.distance co2i co2j in
      let d = abs_float (d1 -. d2) in
      dev := !dev +. d *. d;
    done;
  done;
  sqrt ( 2.0 *. !dev /. (float (n * n1)))

let fit_ali qaco taco ali =
  let amol,bmol = List.fold_left (fun (a,b) (i,j) ->
    (snd qaco.(i)) :: a , (snd taco.(j)) :: b) ([],[]) ali in
  let amol = Array.of_list amol and bmol = Array.of_list bmol in
  let drms = get_drms amol bmol in
  let rms,qrot,cma,cmb = Vec3.crms_mat amol bmol in
  rms,qrot,cma,cmb,drms

let get_seqid qatoms tatoms ali =
  let ht = Ht.create 1 in
  List.iter (fun (i,j) ->
    let (qch,qsid,qaa,_),_ = qatoms.(i) 
    and (tch,tsid,taa,_),_ = tatoms.(j) in
    Ht.replace ht (qch,qsid,tch,tsid) (qaa,taa)) ali;
  let nali = Ht.length ht in
  let nid = Ht.fold 
      (fun _ (c1,c2) nid -> if c1 = c2 then nid +. 1.0 else nid) ht 0.0 in
  nali,(if nali = 0 then 0.0 else (100.0 *. nid /. float_of_int nali))


let qsetup atoms cm =
  Array.map (fun (a,co) -> (a, Vec3.subtract co cm)) atoms

let tsetup atoms qrot cm =
  Array.map (fun (a,co) -> 
    (a, Vec3.qrotate qrot (Vec3.subtract co cm)))
    atoms

let prep_atoms ?refset atoms = Array.of_list (Patom.prep_atoms ?refset atoms)

let prep_aco refsets atoms tatoms gi =
  warn "prep_aco 1";
  let refset = refsets gi.Gi.qrs_id in
  warn "prep_aco 2";
  let qaco = prep_atoms ~refset atoms in
  warn "prep_aco 3";
  let refset = {Refset.default with Refset.frame = gi.Gi.frame} in
(*  Refset.print stderr refset;*)
  let taco = prep_atoms ~refset tatoms in
  warn "prep_aco 4";
  qaco,taco

let ali_of_gmatch qv gmatch =
  let ali = List.fold_left (fun ali qv ->
    if not (G.mem_vertex gmatch qv) || G.out_degree gmatch qv = 0 then ali
    else
      let tv = List.hd (G.succ gmatch qv) in
      (snd qv, snd tv) :: ali) [] qv 
  in
  List.sort ~cmp:(fun (i,_) (j,_) -> i - j) ali

let iterative_refine opt itype qaco taco =
  let qlen = Array.length qaco 
  and tlen = Array.length taco in
  let mkgraph = 
    if qlen >= tlen
    then fun qaco taco -> make_graph itype opt qaco Q taco T
    else fun qaco taco -> make_graph itype opt taco T qaco Q
  in
  let rec loop nstep nbad (bgmatch,bscore) (gmatch0,score0,ali0) qaco taco =
    if nstep = opt.niter || nbad > 2
    then bgmatch,bscore
    else
      let rms,qrot,cma,cmb,drms = fit_ali qaco taco ali0 in
      let qaco = qsetup qaco cma in
      let taco = tsetup taco qrot cmb in
      let g1 = mkgraph qaco taco in
      let qv,tv = split_vertices Q T g1 in
      let gmatch,score = Wbm.maximum_matching g1 qv tv in
      let ali = ali_of_gmatch qv gmatch in
      let nbad,best = 
	if score > bscore
	then nbad,(gmatch,score)
	else (nbad+1),(bgmatch,bscore) in
      loop (nstep + 1) nbad best (gmatch,score,ali) qaco taco
  in
  let g1 = mkgraph qaco taco in
  let qv,tv = split_vertices Q T g1 in
  let gmatch,score = Wbm.maximum_matching g1 qv tv in
  let ali = ali_of_gmatch qv gmatch in
  let gmatch,score = loop 0 0 (gmatch,score) (gmatch,score,ali) qaco taco in
  let nali = G.nb_edges gmatch in
  let ir_score = score *. float nali /. float (min qlen tlen) in
  gmatch,ir_score

let is_sound_conflict qaco taco gtot ecnf =
  let dtol = 5.0 in
  let rec loop = function
    | [] -> true
    | (v1,_,v2)::rest -> 
	let v1,v2 = if G.mem_vertex gtot v1 then v1,v2 else v2,v1 in
	let nvs = G.succ gtot v1 in
	let co1,acos2 = 
	  match v2 with
	  | T,i -> (snd taco.(i)),taco
	  | Q,i -> (snd qaco.(i)),qaco in
	let n = List.fold_left 
	    (fun n (_,i) ->
	      let co = snd acos2.(i) in
	      if Vec3.distance co1 co > dtol then succ n else n)
	    0
	    nvs in
	if n > 0 then false
	else loop rest
  in loop ecnf

let merge_graphs opt qaco taco lgraphs = 
  let flg_msg = 0 in
  let gtot = G.create () in
  let proc nm (g,score) =
    if score < opt.min_score0 
    then nm
    else
      let ncomm,eadd,ecnf = G.fold_edges_e
	  (fun ((v1,w,v2) as e) (n,add,rmv) -> 
	    if G.mem_edge gtot v1 v2
	    then (succ n,add,rmv)
	    else if not (G.mem_vertex gtot v1) && not (G.mem_vertex gtot v2)
	    then (n,e::add, rmv)
	    else (n,add, e::rmv))
	  g (0,[],[]) in
      let cn = List.length ecnf in
      let an = List.length eadd in
      if flg_msg > 1 then
	(fprintf stderr "Ir.merge_graphs: %3d common, %3d additional, %3d conflicting edges\n" ncomm an cn; flush stderr);
      if an > cn && (ncomm > 1 || is_sound_conflict qaco taco gtot ecnf)
      then (List.iter (G.add_edge_e gtot) eadd; 
	    List.iter (G.add_edge_e gtot) ecnf;
	    nm + 1)
      else nm 
  in
  let nm = List.fold_left proc 0 lgraphs in
  if flg_msg > 0 then
    (fprintf stderr "Ir.merge_graph (done): %3d edges , %2d graphs\n" 
       (G.nb_edges gtot) nm; flush stderr);
  gtot,nm

let flex_align opt itype atoms refsets tatoms pgi =
  let qaco = prep_atoms atoms in
  let taco = prep_atoms tatoms in
  let lgraphs = List.map
      (fun gi ->
	let qaco,taco = prep_aco refsets atoms tatoms gi in
	let gmatch,score = iterative_refine opt itype qaco taco in
	(gmatch,score))
      pgi.Gi.lgi in
  let lgraphs = List.sort 
      ~cmp:(fun (g1,s1) (g2,s2) ->
	if s1 > s2 then -1
	else if s1 < s2 then 1
	else G.nb_edges g2 - G.nb_edges g1) lgraphs in
  let gmatch_r,score_r = List.hd lgraphs in (* best rigid alignment *)
  let gtot,nm = merge_graphs opt qaco taco lgraphs in
  if nm <= 1 
  then 
    let qv,tv = split_vertices Q T gmatch_r in
    qv,tv,gmatch_r,score_r,nm
  else
    let qv,tv = split_vertices Q T gtot in
    let gmatch,score = Wbm.maximum_matching gtot qv tv in
    let qlen = List.length atoms and tlen = List.length tatoms in
    let nali = G.nb_edges gmatch in
    let score = score *. (float nali) /. float (min qlen tlen) in
    qv,tv,gmatch,score,nm

let rigid_align opt itype atoms refsets tatoms pgi =
  warn "rigid_align 1";
  let gbest,sbest = List.fold_left 
      (fun (gb,sb) gi ->
	warn "rigid_align 1.1";
	let qaco,taco = prep_aco refsets atoms tatoms gi in
	warn "rigid_align 1.2";
	let gmatch,score = iterative_refine opt itype qaco taco in
	warn "rigid_align 1.3";
	if score > sb then (gmatch,score) else (gb,sb)) 
      (G.create (),W.zero) pgi.Gi.lgi in
  let qv,tv = split_vertices Q T gbest in
  qv,tv,gbest,sbest,(List.length pgi.Gi.lgi)

let refine_core conn opt atoms refsets pgi =
  warn "refine_core 1";
  let itype = pgi.Gi.itype_p in
(* CHECK if tatoms is okey or corrupted!! *)
  let tatoms = Interface.get_iatoms 
      conn (string_of_itype itype) pgi.Gi.if_id_p in
  let get_match = if opt.rigid then rigid_align else flex_align in
  warn "refine_core 3";
  let qv,tv,gmatch,score,nmatch = 
    get_match opt itype atoms refsets tatoms pgi in
  warn "refine_core 3.5";
  let ali = ali_of_gmatch qv gmatch in
  warn "refine_core 4";
  let nali = List.length ali in
  let qaco = prep_atoms atoms in
  let taco = prep_atoms tatoms in
  warn "refine_core 5";
  let rms,qrot,cma,cmb,drms = fit_ali qaco taco ali in
  let nres,seqid = get_seqid qaco taco ali in
  let sgi = List.fold_left (fun s gi -> s +. gi.Gi.score) 0.0 pgi.Gi.lgi in
  let fngi = float(List.length pgi.Gi.lgi) in
  let gi_score = sgi /. fngi in
  let tani = 
    (float nali) /. (float (Array.length qaco + Array.length taco - nali)) in
  warn "refine_core 10";
  if score < opt.min_score then None
  else Some {rank=0; itype = pgi.Gi.itype_p; 
	     if_id = pgi.Gi.if_id_p; 
	     score = score; gi_score = gi_score; 
	     ngi = nmatch;
	     ali = ali; rms = rms; drms = drms;
	     tani = tani;
	     seqid=seqid; nali_res=nres;
	     pvalue = get_pvalue pgi.Gi.itype_p score}

let reweight conn = function 
  | [] -> []
  | results ->
      let q = "SELECT if_id,prob FROM Interfaces WHERE " in
      let const = List.map (fun r -> 
	sprintf "(if_id = '%s')" r.if_id) results in
      let const = String.concat " OR " const in
      let res = Sql.select conn (q ^ const) in
      let ht = Ht.create 10000 in
      Array.iter (fun t -> Ht.add ht t.(0) (float_of_string t.(1))) 
	res#get_all;
      List.map (fun r ->
	let p = Ht.find ht r.if_id in
	{r with score = r.score *. p; pvalue = r.pvalue *. p}) results

let refine conn ?(clog=stderr) ?(opt=opt_default) atoms refsets gi_results = 
  Printexc.record_backtrace true;
  try
    match gi_results with
    | [] -> warn (sprintf "Ir.refine enter! (0)"); []
    | gi_results ->
	warn (sprintf "Ir.refine enter! (1)");
	let frefsets = Refset.make_frefsets refsets in
	warn (sprintf "Ir.refine gi_results %d" (List.length gi_results));
	let res = List.filter_map 
	    (refine_core conn opt atoms frefsets) gi_results in
	warn (sprintf "Ir.refine results %d" (List.length res));
	let res = if opt.redundant then remove_redundancy res else res in
	let res = List.take opt.nlimit (sort_results res) in
	if opt.reweight then reweight conn res else res
  with exc ->
    warn (sprintf "Ir.refine: Exception %s" (Printexc.to_string exc));
    Printexc.print_backtrace stderr;
    exit 5

let align_atoms qatoms tatoms ali =
  let qaco = Array.map Patom.aco_of_atom qatoms in
  let taco = Array.map Patom.aco_of_atom tatoms in
  fit_ali qaco taco ali

let get_alignment ?(inv=false) conn atoms result =
  let atoms = Array.of_list atoms in
  let iface,pdbid,proteins = 
    Interface.restore conn result.if_id in
  let tatoms = Array.of_list iface.Interface.atoms in
  let _,qrot,cma,cmb,drms = align_atoms atoms tatoms result.ali in
  let qrot,cma,cmb = 
    if not inv then qrot,cma,cmb
    else (Vec3.qconj qrot),cmb,cma
  in
  let move a = 
    let co = Atom.coord a in
    let x,y,z = Vec3.add cma (Vec3.qrotate qrot (Vec3.subtract co cmb)) in
    {a with Atom.x = x; y = y; z = z}
  in
  let atoms,tatoms,twhole,latoms = 
    if not inv then
      let tatoms = Array.map move tatoms in
      let twhole = List.map move proteins in
      let latoms = List.map move iface.Interface.latoms in
      atoms,tatoms,twhole,latoms
    else
      let atoms = Array.map move atoms in
      atoms,tatoms,(Array.to_list atoms),iface.Interface.latoms
  in
  let corr = List.map (fun (i,j) -> atoms.(i),tatoms.(j)) result.ali in
  let iface = {iface with Interface.atoms = Array.to_list tatoms;
	       latoms = latoms} in
  corr,twhole,iface
