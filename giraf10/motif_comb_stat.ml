#use "topfind";;
#require "extlib";;

open ExtLib

module Ht = Hashtbl

let read () = 
  let lines = input_lines stdin in
  let ht = Ht.create 100 in
  Enum.iter (fun l -> 
    let chk = Array.of_list (String.nsplit l "\t") in
    Ht.add ht (chk.(0),chk.(1)) (float_of_string chk.(2))) lines;
  ht

let get_prob hp = 
  let hs = Ht.create 10 in
  let ht = Ht.create 10 in
  let tot = Ht.fold (fun _ c tot -> c +. tot) hp 0.0 in
  Ht.iter (fun k c -> Ht.add ht k (c /. tot)) hp;
  Ht.iter (fun (m,n) c ->
    let p = Ht.find_default hs m 0.0 in 
    Ht.replace hs m (p +. c);
    let p = Ht.find_default hs n 0.0 in 
    Ht.replace hs n (p +. c);) ht;
  Ht.iter (fun ((m,n) as key) c ->
    let pm = Ht.find hs m in
    let pn = Ht.find hs n in 
    let d = 1.0 /. (pm *. pn) in
    Ht.replace ht key (c *. d)) ht;
  ht

let main () =
  let hp = read () in
  let ht = get_prob hp in
  Ht.iter (fun (m,n) c ->
    let p = Ht.find hp (m,n) in
    Printf.printf "%s\t%s\t%f\t%f\n" m n p (log10 c)) ht

let _ = main ()
    
