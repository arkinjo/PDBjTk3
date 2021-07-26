open Printf
open PDBjTk
open PDBjBasis
open Util

type ftype1 = (Vec3.t * (Vec3.t * Vec3.t * Vec3.t))
type ftype2 = (Vec3.t * Vec3.t * Vec3.t * Vec3.t)

let rewrite conn table  = 
  Sql.command conn 
    ("DECLARE rsc NO SCROLL CURSOR FOR SELECT frame FROM " ^ table);
  let update = sprintf "UPDATE %s SET frame = $1 WHERE CURRENT OF rsc" table in
  let proc res =
    let str = PG.unescape_bytea (res#getvalue 0 0) in
    let (o,(x,y,z)) = (Marshal.from_string str 0 : ftype1) in
    let frame = (o,x,y,z) in
    let nframe = conn#escape_bytea (Marshal.to_string frame []) in
    Sql.command conn ~params:[|nframe|] update
  in
  let rec loop () =
    let res = Sql.select conn "FETCH rsc" in
    if res#ntuples = 0
    then Sql.command conn "CLOSE rsc"
    else loop (proc res) in
  loop ()

let main () =
  try
    let conn = new PG.connection ~dbname:"giraf9" () in
    let table = Sys.argv.(1) in
    print_endline ("Processing " ^ table); flush stdout;
    Sql.transaction conn 
      (fun () -> 
	rewrite conn table;
	Sql.command conn ("VACUUM ANALYZE " ^ table))
  with
  | PG.Error e -> 
      fprintf stderr "PG.Error: %s\n" (PG.string_of_error e);
      exit 1


let main2 () = 
  let nexec = 
    if Array.length Sys.argv > 1 
    then int_of_string Sys.argv.(1)
    else 100000 in
  let p () = 
    (Random.float 10.0 -. 5.0),
    (Random.float 10.0 -. 5.0),
    (Random.float 10.0 -. 5.0) in
  let o = p () in
  let x = p () in
  let y = p () in
  let z = p () in
  let frame = (o,(x,y,z)) in
  let t0 = Sys.time () in
  for i = 1 to nexec do
    let s = (Marshal.to_string frame []) in  
    let _ = (Marshal.from_string s 0 : ftype1) in
    ()
  done;
  let t1 = Sys.time () in
  for i = 1 to nexec do
    let (o,(x,y,z)) = frame in
    let s = (Marshal.to_string (o,x,y,z) []) in  
    let (o,x,y,z) = (Marshal.from_string s 0 : ftype2) in
    let _ = (o,(x,y,z)) in
    ()
  done;
  let t2 = Sys.time () in
  Printf.printf "time = %f %f \n" (t1 -. t0) (t2 -. t1);
  ()

let _ = main ()
