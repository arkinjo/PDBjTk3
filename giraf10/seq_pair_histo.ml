open ExtLib
module Ht = Hashtbl

let _ = 
  let pos = Ht.create 10 in
  let lines = input_lines stdin in
  let add c pid =
    let p,t = Ht.find_default pos pid (0.0,0.0) in
    if c > 0 
    then Ht.replace pos pid ((p +. 1.0),(t +. 1.0))
    else Ht.replace pos pid (p,(t +. 1.0))
  in
  let f l = 
    let ch = Array.of_list (String.nsplit l "\t") in
    let pid = float_of_string ch.(4) in
    let c = int_of_string ch.(5) in
    let pid1 = 10 * int_of_float (pid /. 10.0) in
    let pid2 = 5 + 10 * int_of_float ((pid -. 5.0) /. 10.0) in
    add c pid1;
    add c pid2
  in
  Enum.iter f lines;
  let l = Ht.fold (fun k v l -> (k,v)::l) pos [] in
  let l = List.sort ~cmp:(fun (a,_) (b,_) -> a - b) l in
  List.iter (fun (pid,(p,t)) ->
    Printf.printf "%3d %8.3f %.0f %.0f\n" pid (100.0 *. p /. t) p t) l


