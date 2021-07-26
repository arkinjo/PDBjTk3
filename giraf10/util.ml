open Num
open PDBjTk
open PDBjBasis
open Printf

let default_port = 7789

module String = ExtString.String
module List = ExtList.List
module Array = ExtArray.Array
module Ht = ExtHashtbl.Hashtbl
module PG = Postgresql
module Q = Queue
module SSet = Set.Make(String)

type itype = PPI | PEPTIDE | DNARNA | OPOLY | NONPOLYMER | SMALL | FOLD
let itype_all = [NONPOLYMER; PPI; DNARNA; PEPTIDE; OPOLY]

let compare_itype =
  let l = List.mapi (fun i a -> (a,i)) itype_all in
  fun i1 i2 -> (List.assoc i1 l) - (List.assoc i2 l)

let itype_of_string s = 
  match s with
  | "ppi" -> PPI
  | "peptide" -> PEPTIDE
  | "dnarna" -> DNARNA
  | "opoly" -> OPOLY
  | "nonpolymer" -> NONPOLYMER
  | "small" -> SMALL
  | "fold" -> FOLD
  | _ -> raise Not_found

let string_of_itype = function
  | PPI -> "ppi"
  | PEPTIDE -> "peptide"
  | DNARNA -> "dnarna"
  | OPOLY -> "opoly"
  | NONPOLYMER -> "nonpolymer"
  | SMALL -> "small"
  | FOLD -> "fold"

let remove_dup_itype l =
  let ht = Ht.create 1 in
  List.iter (fun a -> Ht.replace ht a ()) l;
  Ht.fold (fun k _ l -> k :: l) ht []

type pdbfile_type = 
  | PDBML of string 
  | MMCIF of string 
  | PDBF of string
  | PDBID of string
  | Unknown of string


let warn s = prerr_endline ("#warning: " ^ s); flush stderr

let radix_n_of_string base =
  let b = num_of_int (String.length base) in
  let zero = num_of_int 0 in
  let index a = num_of_int (String.index base a) in
  let conv str =
    List.fold_left (fun n a -> n */ b +/ index a) zero str in
  fun str ->
    match String.explode (String.lowercase str) with
    | [] -> "0"
    | h::t when h = '+' || h = '-' ->
	let n = conv t in
	string_of_num (if h = '-' then minus_num n else n)
    | str -> string_of_num (conv str)

let string_of_radix_n base =
  let b = num_of_int (String.length base) in
  let zero = num_of_int 0 in
  let conv str =
    let n = num_of_string str in
    let m = abs_num n in
    let rec loop res n =
      if n =/ zero then String.implode res
      else
	let r = int_of_num (mod_num n b) in
	loop ((base.[r])::res) (quo_num n b)
    in
    let res = loop [] m in
    if n </ zero then "-" ^ res else res
  in
  fun str -> conv str
    
let base36 = "0123456789abcdefghijklmnopqrstuvwxyz"
let radix36_of_string = radix_n_of_string base36
let string_of_radix36 = string_of_radix_n base36

let base38 = "0123456789abcdefghijklmnopqrstuvwxyz,."
let radix38_of_string = radix_n_of_string base38
let string_of_radix38 = string_of_radix_n base38

let base12 = "0123456789.-"
let radix12_of_string = radix_n_of_string base12
let string_of_radix12 = string_of_radix_n base12

let triple1 (x,_,_) = x
let triple2 (_,y,_) = y
let triple3 (_,_,z) = z

module LocTypes = struct
  let standard_types = ["N"; "C"; "O"; "S"]
  let type_of_atom atom = atom.Atom.type_symbol
end

module ATypes = LocTypes

let std_atypes = ["0N";"0A"; "0C"; "0O"] @ ATypes.standard_types

let nstd_atypes = List.length std_atypes

let mem_std_atype,get_atype_id =
  let ht = Ht.create 1 in
  List.iteri (fun i a -> Ht.add ht a i) std_atypes;
  (fun a -> Ht.mem ht a),
  (fun a -> 
    try Ht.find ht a with Not_found -> -1)

let type_of_atom atom = 
  match atom.Atom.label_atom_id with
  | "N" -> "0N"
  | "CA" -> "0A"
  | "C" -> "0C"
  | "O" -> "0O"
(*  | "CB" -> "0B"*)
  | _ ->  ATypes.type_of_atom atom

let radius_of_atom dblk =
  let ht = Ht.create 1 in
  fun a ->
    try
      Ht.find ht a.Atom.atom_site_id
    with Not_found ->
      let comp_id = PDBjUtil.get_standardized_modres dblk a in
      let r = OONS.radius_of_atom {a with Atom.label_comp_id = comp_id} in
      Ht.add ht a.Atom.atom_site_id r;
      r

module AcoPoint = struct
(* (label_asym_id,label_seq_id,index_amino3, aid),(x,y,z) *)
  type t = (string * int * int * int) * (float * float * float) 
  let coord (_,co) = co
end

module IntCellAco = IntCell.Make(AcoPoint)

let split_list n = function
  | [] -> List.make n []
  | lst ->
      let ht = Ht.create 1 in
      List.iteri (fun i a -> 
	let k = i mod n in
	let l = Ht.find_default ht k [] in
	Ht.replace ht k (a::l)) lst;
      let l = Ht.fold (fun _ l ll -> List.rev l :: ll) ht [] in
      let m = List.length l in
      if m < n then l @ List.make (n-m) [] else	l

let split_list_chunk n lst = 
  let rec loop acc = function
    | [] -> List.rev acc
    | l -> loop (List.take n l :: acc) (List.drop n l)
  in loop [] lst

let grow_buffer s =
  let s' = String.create (2 * String.length s) in
  String.blit s 0 s' 0 (String.length s);
  s'

let compress_string ?(level = 6) inbuf =
  let zs = Zlib.deflate_init level true in
  let rec compr inpos outbuf outpos =
    let inavail = String.length inbuf - inpos in
    let outavail = String.length outbuf - outpos in
    if outavail = 0
    then compr inpos (grow_buffer outbuf) outpos
    else begin
      let (finished, used_in, used_out) =
        Zlib.deflate zs inbuf inpos inavail outbuf outpos outavail
                   (if inavail = 0 then Zlib.Z_FINISH else Zlib.Z_NO_FLUSH) in
      if finished then 
        String.sub outbuf 0 (outpos + used_out)
      else
        compr (inpos + used_in) outbuf (outpos + used_out)
    end in
  let res = compr 0 (String.create (String.length inbuf)) 0 in
  Zlib.deflate_end zs;
  res

let uncompress_string inbuf =
  fprintf stderr "uncompress_string : %s\n" inbuf;
  let zs = Zlib.inflate_init true in
  fprintf stderr "uncompress_string :inflate_init \n" ;
  let rec uncompr inpos outbuf outpos =
    fprintf stderr "uncompress_string :uncompr inpos=%d \n" inpos;
    let inavail = String.length inbuf - inpos in
    let outavail = String.length outbuf - outpos in
    if outavail = 0
    then uncompr inpos (grow_buffer outbuf) outpos
    else begin
      let (finished, used_in, used_out) =
        Zlib.inflate zs inbuf inpos inavail outbuf outpos outavail
	  Zlib.Z_SYNC_FLUSH in
      if finished then 
        String.sub outbuf 0 (outpos + used_out)
      else
        uncompr (inpos + used_in) outbuf (outpos + used_out)
    end in
  let res = uncompr 0 (String.create (2 * String.length inbuf)) 0 in
  fprintf stderr "uncompress_string :uncompr \n" ;
  Zlib.inflate_end zs;
  fprintf stderr "uncompress_string : inflate_end \n" ;
  res

let socket_of_host_port hostname port = 
  let inet_addr = (Unix.gethostbyname hostname).Unix.h_addr_list.(0) in
  Unix.ADDR_INET(inet_addr,port)

let get_date_time () =
  let tm = Unix.localtime (Unix.time ()) in
  sprintf "%4d-%2.2d-%2.2d %2.2d:%2.2d:%2.2d" 
    (tm.Unix.tm_year + 1900) tm.Unix.tm_mon tm.Unix.tm_mday
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

let randomize_list l =
  let l = List.map (fun a -> (Random.int 100000),a) l in
  let l = List.sort ~cmp:(fun (a,_) (b,_) -> a - b) l in
  snd(List.split l)

let quad1 (a,_,_,_) = a
let quad2 (_,a,_,_) = a
let quad3 (_,_,a,_) = a
let quad4 (_,_,_,a) = a

