open Util
open Printf
open ExtLib
open Xml

module PG = Postgresql
module Ht = Hashtbl

type opt = {
    conninfo: string;
    outdir: string;
    itype: string;
  }

let get_opts () = 
  let itype = ref "" in
  let conninfo = ref "" in
  let outdir = ref "" in
  let specs = [
    ("-conninfo",Arg.String (fun s -> conninfo := s), "Database connection");
    ("-outdir",Arg.String (fun s -> outdir := s), "Output directory");
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

let process_interface conn if_id =
  let fields = ["pdbid"; "assembly_id"; "title"; "type"; "label_asym_id"; 
		"auth_asym_id"; "entity_id"; "destription";
		"l_label_asym_id"; "l_auth_asym_id"; "l_entity_id"; 
		"l_description";"comp_id"] in
  let q = 
    "SELECT "
    ^ (String.concat "," fields)
    ^ " FROM interfaces_summary_mat WHERE if_id = $1" in
  let res = Sql.select conn ~params:[|if_id|] q in
  let getf = Sql.result_of_field res (res#get_tuple 0) in
  Element("interface",[("if_id",if_id)],
	  (List.map (fun fn -> Element(fn,getf fn)) fields))


