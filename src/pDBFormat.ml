(** parsing PDB format flat file *)

open ExtLib
open Printf
open PDBjBasis

exception No_atoms_found of string

type atom = {aind: int; aname: string; alt: string ;
	     rind: string; rins: string; rname: string;
             chid: string; x: float; y: float; z: float;
             occup: float; bfact: float; hetflg: bool;
	     type_symbol: string}
and term = {
  tind : int; 
  tname : string; 
  tch : string
}


exception Not_atom
exception Atom_not_found

type t = 
  Header of string*string
| Title of string
| Compnd of string
| Source of string
| Author of string
| Atom of atom
| Terminal of term
| Other of string

let make_chid, clear_chid =
  let ht_chid = Hashtbl.create 26 in
  let make_chid = 
    let base = Array.init 26 
        (fun i -> Char.chr (Char.code 'A' + i)) in
    let next c = 
      let ind = Char.code c + 1 - Char.code 'A' in
      base.(ind) 
    in
    fun chid -> 
      (match Hashtbl.find_all ht_chid chid with
      | [] -> Hashtbl.add ht_chid chid (List.rev (String.explode chid))
      | h::_ -> 
          (match List.hd h with
          | 'Z' -> Hashtbl.replace ht_chid chid ('A' :: h)
          | c -> 
              if List.length h > 1 then 
                Hashtbl.replace ht_chid chid (next c :: List.tl h)
              else Hashtbl.replace ht_chid chid ('A' :: h) ));
      String.implode (List.rev (Hashtbl.find ht_chid chid))
  in 
  let clear_chid () = Hashtbl.clear ht_chid in
  make_chid, clear_chid

let safe_number f anum = 
  let bnum =  Str.global_replace (Str.regexp " +")  "" anum in 
  f bnum

let safe_int = safe_number int_of_string
let safe_float = safe_number float_of_string

  
let read_header line = 
  let len = (String.length line) in
  if len > 66 then 
    let head = String.sub line 10 40 
    and pid = String.lowercase (String.sub line 62 4) in 
      Header (pid, head)
  else 
      Header ("0???",(String.make 40 '?'))

let read_record record line = 
  let len = min (String.length line -10) 60 in
  let com = String.sub line 10 len in
  match record with 
    "COMPND" -> Compnd com
  | "TITLE " -> Title  com
  | "SOURCE" -> Source com
  | "AUTHOR" -> Author com
  | _ -> Other line

let read_compnd = read_record "COMPND"
let read_title = read_record "TITLE "
let read_source = read_record "SOURCE"
let read_author = read_record "AUTHOR"

let read_atom line = 
  let llen = String.length line 
  in 
  let anum = safe_int (String.sub line 6 5) 
  and anam = String.sub line 12 4 
  and alt = String.sub line 16 1
  and rnam = String.sub line 17 4
  and chid = String.sub line 21 1
  and rnum = String.sub line 22 4
  and rins = String.sub line 26 1
  and xco = safe_float (String.sub line 30 8)
  and yco = safe_float (String.sub line 38 8)
  and zco = safe_float (String.sub line 46 8) in
  let occ = if llen >= 60 then safe_float (String.sub line 54 6) else 0.0
  and bfa = if llen >= 66 then safe_float (String.sub line 60 6) else 0.0 in
  let symb = if llen >= 78 then String.sub line 76 2 else "" in
  let flg = String.sub line 0 3 = "HET" in
    Atom {aind = anum; aname = anam; alt = alt ;
       rind = rnum; rins=rins; rname = rnam;
       chid = chid; x = xco; y = yco; z = zco;
       occup = occ; bfact = bfa; hetflg=flg; type_symbol=symb} 

let read_term line = 
  let len = String.length line in 
  let anum = if len > 60 then safe_int (String.sub line 6 5) else 0 
  and rnam = if len > 60 then String.sub line 17 3 else "UNK" 
  and chid = if len > 60 then String.sub line 21 1 else " " 
  in 
  Terminal {tind = anum; tname = rnam; tch = chid}

let read_raw ic = 
  let ls = ref [] in
  let rec loop () = 
    let line = input_line ic in
    if String.length line >= 6 then
      match (String.sub line 0 6) with
        "END   " | "ENDMDL" -> raise End_of_file
      | "HEADER" -> (ls := (read_header line)::!ls ; loop ())
      | "COMPND" -> (ls := (read_compnd line)::!ls ; loop ()) 
      | "TITLE " -> (ls := (read_title  line)::!ls ; loop ()) 
      | "SOURCE" -> (ls := (read_source line)::!ls ; loop ())
      | "AUTHOR" -> (ls := (read_author line)::!ls ; loop ())
      | "ATOM  " | "HETATM" -> (ls := (read_atom line)::!ls; loop ()) 
      | "TER   " -> (ls := (read_term line)::!ls ; loop ())
      |  _    -> (ls := (Other line)::!ls ; loop ())
    else loop ()
  in
  (try loop () with End_of_file -> close_in ic);
  List.rev !ls

let get_header apdb = 
  try
    match List.find (function Header _ -> true | _ -> false) apdb with
    | Header (i,h) -> i,h
    | _ -> failwith "get_header"
  with Not_found -> "NO TITLE","XXXX"

let get_pdbid apdb = fst (get_header apdb)
let get_title apdb = 
  try 
    match List.find (function Title _ -> true | _ -> false) apdb with
    | Title t -> t
    | _ -> failwith "get_title"
  with Not_found -> "NO TITLE"

let get_chain apdb chid = 
  let nch = if chid = "_" then " " else chid in
  let aux ent = 
    match ent with 
      Atom a -> if a.chid = nch then true else false
    | Terminal t -> if t.tch = nch then true else false
    | _ -> true
  in
  let rec loop ils ols =
    match ils with
      [] -> ols
    | h::t -> 
        match h with 
        Terminal _ -> h::ols
        | _ -> loop t (h::ols)
  in
  List.rev (loop (List.filter aux apdb) [])

(** remove all spaces from a string. *)
let space = Str.regexp "[ \t]+";;

let rmsp = Str.global_replace space "" ;;

let guess_type_symbol aname sym =
  if sym <> "" then sym
  else 
    match aname.[0] with
    | 'H' -> "H"
    | 'N' -> "N"
    | 'C' -> "C"
    | 'O' -> "O"
    | 'S' -> "S"
    | _ -> "X"

(** to transform this atom to PDBj.Atom.t *)
let toAtom_site label_asym_id prev_seq_id seq_id a =
  if !prev_seq_id <> a.rind ^ a.rins then
    (incr seq_id; prev_seq_id := a.rind ^ a.rins);
  {Atom_site.default with 
   Atom_site.id = Some (string_of_int a.aind); 
   pdbx_PDB_model_num = Some 1;
   label_asym_id = Some label_asym_id; 
   auth_asym_id = Some (if a.chid = " " then "A" else a.chid);
   label_seq_id = Some (!seq_id); 
   label_comp_id= Some (rmsp a.rname);
   auth_seq_id = Some (rmsp a.rind);
   auth_comp_id= Some (rmsp a.rname);
   group_PDB = Some (if a.hetflg then "HETATM" else "ATOM");
   type_symbol= Some (rmsp (guess_type_symbol a.aname a.type_symbol));
   label_atom_id= Some (rmsp a.aname);
   pdbx_PDB_ins_code = Some (if a.rins = " " then "?" else a.rins);
   label_alt_id= Some (if a.alt = " " then "." else a.alt);
   auth_atom_id= Some (rmsp a.aname);
   cartn_x= Some a.x; cartn_y= Some a.y; cartn_z= Some a.z; 
   occupancy= Some a.occup; b_iso_or_equiv= Some a.bfact}

let pdb2pdbjatoms apdb =
  let tatoms = List.filter (function 
    | Atom _ | Terminal _ -> true
    | _ -> false) apdb in
  let hets,llatoms = List.fold_left (fun (t,l) a -> 
    match a with
    | Atom a -> (a::t,l)
    | Terminal _ -> ([],(List.rev t)::l)
    | _ -> failwith "pdb2pdbjatoms failed.") ([],[]) tatoms in
  (* HETATM sections are not followed by "TER". *)
  let last,sep_hets = List.fold_left (fun (t,l) a ->
    match t with
    | [] -> (a::t,l)
    | h::_ when h.chid <> a.chid -> ([a],(List.rev t)::l)
    | _ -> (a::t,l)) ([],[]) hets in
  let llatoms = (List.rev llatoms) @ (last :: sep_hets) in
  let proc_lst l =
    if l = [] then []
    else
      let h = List.hd l in
      let h = if h.chid = " " then {h with chid = "A"} else h in
      let label_asym_id = make_chid h.chid in
      let prev_seq_id = ref "" in
      let seq_id = ref 0 in
      List.map (toAtom_site label_asym_id prev_seq_id seq_id) l
  in
  List.concat (List.map proc_lst llatoms)

let parse ?(file="-") ic =
  let apdb = read_raw ic in
  let pdbid,desc = get_header apdb in
  let title = get_title apdb in
  let str = {Struct.default with Struct.title = Some title; 
	     pdbx_descriptor= Some desc} in
  let entry = {Entry.default with Entry.id = Some pdbid} in
  let atoms = pdb2pdbjatoms apdb in
  if List.length atoms > 0 then
    {Datablock.default with Datablock.datablockName=pdbid;
     entry = [entry];
     struct_ = [str];
     atom_site = atoms}
  else
    raise (No_atoms_found file)

let parse_file file =
  Ecomm.read_gzipped_maybe (parse ~file) file

let write_atom_records oc atoms =
  let nodot s = if s = "." then " " else s in
  let maname r = 
    let n = String.length r in
    if n = 4 then r
    else " " ^ r in
  let pr a = 
    fprintf oc "%6s%5s %-4s%1s%-3s %1s%4s%1s   %8.3f%8.3f%8.3f%6.2f%6.2f          %2s\n"
      a.Atom.group_PDB
      a.Atom.atom_site_id
      (maname a.Atom.auth_atom_id)
      (nodot a.Atom.label_alt_id)
      a.Atom.auth_comp_id
      a.Atom.auth_asym_id
      a.Atom.auth_seq_id
      (nodot a.Atom.ins_code)
      a.Atom.x a.Atom.y a.Atom.z a.Atom.occupancy a.Atom.b_iso
      a.Atom.type_symbol in
  let cur_ch = ref "" in
  List.iter 
    (fun a ->
      if !cur_ch <> a.Atom.label_asym_id && !cur_ch <> "" 
      then fprintf oc "TER                \n"
      else cur_ch := a.Atom.label_asym_id;
      pr a)
    atoms;
  flush oc
