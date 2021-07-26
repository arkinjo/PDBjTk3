open Util
open ExtLib
open Printf

let swap pdbcode1 eid1 pdbcode2 eid2 cs =
  let i1 = int_of_string pdbcode1 and i2 = int_of_string pdbcode2 in
  if i1 < i2 || i1 = i2 && eid1 <= eid2 
  then [pdbcode1; eid1; pdbcode2; eid2] @ cs
  else
    let d = Array.of_list cs in
    [pdbcode2; eid2; pdbcode1; eid1;
     d.(0); d.(1); d.(2); d.(3);
     d.(6); d.(7);
     d.(4); d.(5);
     d.(8); d.(9)]

let get_sig cnk = 
  Digest.string 
    ((List.nth cnk 0) ^ "," 
     ^ (List.nth cnk 1) ^ "," 
     ^ (List.nth cnk 2) ^ "," 
     ^ (List.nth cnk 3) ^ "," 
     ^ (List.nth cnk 5) ^ "," 
     ^ (List.nth cnk 6) ^ "," 
     ^ (List.nth cnk 7) ^ "," 
     ^ (List.nth cnk 8) ^ "," 
     ^ (List.nth cnk 9) ^ "," 
     ^ (List.nth cnk 10) ^ "," 
     ^ (List.nth cnk 11))

let pdbcodes_gen conn =
  let q = "SELECT DISTINCT SUBSTRING(s.pdbid FROM 1 FOR 4), a.entity_id, s.pdbcode FROM Structs s JOIN Asyms a ON a.pdbcode = s.pdbcode" in
  let res = Sql.select conn q in
  let ht = Ht.create 1000 in
  Array.iter (fun t -> Ht.add ht (t.(0),t.(1)) t.(2)) res#get_all;
  fun pdbid eid -> Ht.find_all ht (pdbid,eid)

let cross2 cache pdbcodes pdbid1 e1 pdbid2 e2 cs =
  let l1 = pdbcodes pdbid1 e1 in
  let l2 = pdbcodes pdbid2 e2 in
  let ps = List.flatten 
      (List.map (fun c1 -> List.map (fun c2 -> (c1,c2)) l2) l1) in
  List.iter 
    (fun (c1,c2) ->
      let cnk = swap c1 e1 c2 e2 cs in
      let line = String.concat "\t" cnk  in
      let d = get_sig cnk in
      if Ht.mem cache d then ()
      else (Ht.add cache d (); print_endline line))
    ps

let proc_line cache pdbcodes l =
  let cs = String.nsplit l "\t" in
  let e1 = List.hd cs and cs = List.tl cs in
  let e2 = List.hd cs and cs = List.tl cs in
  let pdbid1,eid1 = String.split e1 "_" in
  let pdbid1 = 
    let l = String.nsplit pdbid1 "|" in
    List.hd (List.rev l) in
  let pdbid2,eid2 = String.split e2 "_" in
  cross2 cache pdbcodes pdbid1 eid1 pdbid2 eid2 cs
  
let _ =  
  try
    let dbname = Sys.argv.(1) in
    let conn = new PG.connection ~dbname () in
    let pdbcodes = pdbcodes_gen conn in
    conn#finish;
    let cache = Ht.create 10000 in
    Enum.iter (proc_line cache pdbcodes) (input_lines stdin)
  with
  | PG.Error e -> 
      prerr_endline (PG.string_of_error e);
      exit 1
  | exc -> prerr_endline "Usage: load_blast dbname < blast_output";
      raise exc

  


  
  
