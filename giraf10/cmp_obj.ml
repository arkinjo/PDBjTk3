(* Identify possible ligands.
   Ligands can be
   (1) polypeptide chains
   (2) nucleic acids (DNA/RNA/hybrid)
   (3) sugars
   (4) non-polymers
 *)

open PDBjTk
open PDBjBasis
open Printf
open ExtLib
open Util

(* A "receptor" is a single polypeptide with more than 24 residues *)

let peptide_length = 25

let get_peptide_length atoms =
  let ht = Ht.create 1 in
  List.iter (fun a -> Ht.replace ht a.Atom.label_seq_id ()) atoms;
  Ht.length ht

let filter_peptide_by_size cmp pep = 
  List.filter (fun (_,l) ->
    let ht = Ht.create 1 in
    List.iter (fun a -> Ht.replace ht a.Atom.label_seq_id ()) 
      (snd (List.hd l));
    cmp (Ht.length ht)) pep

let remove_big_peptides pep = filter_peptide_by_size ((>) peptide_length) pep
let remove_small_peptides pep = filter_peptide_by_size ((<=) peptide_length) pep

let identify_entities dblk =
  let poly_type = List.map
      (fun e -> 
	(Option.get e.Entity_poly.entity_id),(Option.get e.Entity_poly.type_))
      dblk.Datablock.entity_poly in
  let nonpoly_type = List.map
      (fun e ->
	(Option.get e.Pdbx_entity_nonpoly.entity_id),
	(Option.get e.Pdbx_entity_nonpoly.comp_id))
      dblk.Datablock.pdbx_entity_nonpoly in
  let asyms = List.map 
      (fun a -> 
	(Option.get a.Struct_asym.entity_id),
	(Option.get a.Struct_asym.id))
      dblk.Datablock.struct_asym in
  let nonpoly,poly = List.partition 
      (fun (e,_) -> List.mem_assoc e nonpoly_type) 
      asyms in
  let polya = List.map 
      (fun (e,a) -> 
	let t = 
	  try List.assoc e poly_type 
	  with (* for incomplete annotation *) 
	    Not_found -> "unknown(polymer)"
	in
	a,t)
      poly in
  let nonpolya = List.map
      (fun (e,a) ->
	let t = 
	  try List.assoc e nonpoly_type 
	  with Not_found -> "unknown(nonpoly)" 
	in
	a,t)
      nonpoly in
  polya,nonpolya

(*
module VS = struct
  type t = string
  let compare = String.compare
  let hash = Hashtbl.hash
  let equal = (=)
end

module GS = Graph.Imperative.Graph.Concrete (VS)
module CS = Graph.Components.Make(GS)
*)

let nucleic_acids = 
  ["polyribonucleotide"; "polydeoxyribonucleotide";
   "polydeoxyribonucleotide/polyribonucleotide hybrid"]

(** split protein into receptor and ligand *)
let split_receptor_ligand dblk asyms (lig_label,latoms) =
  let receptor = List.filter 
      (fun (asym_id,l) ->
	PDBjUtil.asym_is_protein dblk asym_id
	  && get_peptide_length l >= peptide_length
	  && asym_id <> lig_label)
      asyms in
  let ligand = List.filter 
      (fun (i,l) -> i = lig_label) asyms in
  receptor,ligand

let classify_poly dblk poly =
  List.fold_left 
    (fun (pep,nuc,oth) ((r,l) as p) ->
      match l with
        [] -> (pep,nuc,oth)
      | (a,_)::_ ->
          if PDBjUtil.asym_is_protein dblk a then
            (p :: pep, nuc, oth)
          else if PDBjUtil.asym_is_nucleic_acids dblk a then
            (pep, p::nuc, oth)
          else
            (pep, nuc, p::oth))
    ([],[],[]) poly

let identify_ppi dblk asyms =
  let rec loop pairs = function
      [] -> pairs
    | h::t ->
	if PDBjUtil.asym_is_protein dblk (fst h) 
	  && get_peptide_length (snd h) >= peptide_length then
	  let l = List.fold_left 
	            (fun l a -> 
		      if fst h = fst a then l
		      else if PDBjUtil.asym_is_protein dblk (fst a) 
		              && get_peptide_length (snd a) >= peptide_length
                      then
		        ([h], [a])::l
		      else l) 
	      pairs asyms in
	  loop l t
	else loop pairs t
  in
  loop [] asyms

let identify_ligands dblk asyms =
  let poly,nonpoly = identify_entities dblk in
  let poly = List.map (split_receptor_ligand dblk asyms) poly in
  let nonpoly = List.map (split_receptor_ligand dblk asyms) nonpoly in
  let pep,nuc,oth = classify_poly dblk poly in
  let pep = remove_big_peptides pep in
  pep,nuc,oth,nonpoly

let identify_folds dblk asyms =
  List.filter_map (fun h -> 
    if PDBjUtil.asym_is_protein dblk (fst h)
    then Some ([h],[("?",([] : Atom.t list))])
    else None) asyms
