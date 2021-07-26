open Util
open PDBjTk
open PDBjBasis
open Printf
open ExtLib

open Server_comm.Protocol

let debug = false
let warn s =  
  if debug then (prerr_endline ("Giir:" ^ s); flush stderr) else ()

let collect pool giopt iropt (atoms,refsets) =
  let res = ref [] in
  let lock = Mutex.create () in
  let irun (ic,oc) =
    warn "irun before output_value";
    output_value oc (GIIR(giopt,iropt,atoms,refsets));
    flush oc;
    warn "irun after output_value";
    let l = (input_value ic : Ir.result list) in
    warn "irun after input_value";
    Mutex.lock lock;
    res := List.rev_append l !res;
    Mutex.unlock lock;
  in
  let th = List.map (Thread.create irun) pool in
  List.iter Thread.join th;
  let l = List.take iropt.Ir.nlimit 
      (Ir.sort_results ~random:iropt.Ir.random !res) in
  l

let dock pool iropt ir_res1 ir_res2 =
  let res = ref [] in
  let lock = Mutex.create () in
  let irun (ic,oc) =
    output_value oc (Dock(ir_res1,ir_res2));
    flush oc;
    let l = (input_value ic : (Ir.result * Ir.result) list) in
    Mutex.lock lock;
    res := List.rev_append l !res;
    Mutex.unlock lock
  in
  let th = List.map (Thread.create irun) pool in
  List.iter Thread.join th;
  let res = List.sort ~cmp:(fun (a1,b1) (a2,b2) ->
    let s1 = a1.Ir.score +. b1.Ir.score in
    let s2 = a2.Ir.score +. b2.Ir.score in
    if s1 > s2 then -1 else if s1 < s2 then 1 else 0) !res in
  List.take iropt.Ir.nlimit res

