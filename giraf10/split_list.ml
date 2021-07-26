open Printf
open ExtLib

module Ht = Hashtbl

let split_list n lst =
  let ht = Ht.create 1 in
  List.iteri (fun i a -> 
    let k = i mod n in
    let l = Ht.find_default ht k [] in
    Ht.replace ht k (a::l)) lst;
  Ht.fold (fun _ l ll -> List.rev l :: ll) ht []

let format_int n i =
  if n < 10  then sprintf "%d" i
  else if n < 100 then sprintf "%2.2d" i
  else if n < 1000 then sprintf "%3.3d" i
  else sprintf "%4.4d" i

let main () =
  let n = int_of_string Sys.argv.(1) in
  let ic,file = 
    if Array.length Sys.argv > 2 
    then (open_in Sys.argv.(2)),Sys.argv.(2)
    else stdin,"tmplist"
  in
  let lst = Std.input_list ic in
  if ic <> stdin then close_in ic;
  let ll = split_list n lst in
  List.iteri (fun i l -> 
    let fout = file ^ "." ^ (format_int n i) in
    let oc = open_out fout in
    List.iter (fun s -> fprintf oc "%s\n" s) l;
    close_out oc) ll;
  ()

let _ = 
  try
    main ()
  with _ -> 
    fprintf stderr "Usage: split_list num [file]\n";
    exit 1
  
