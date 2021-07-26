(*
 * This file is machine-generated.
 * Do not edit.
 * atomic parameters based on OONS.
 *)

let radius_of_atom atom = 
  match atom.Atom.label_comp_id, atom.Atom.label_atom_id with
  | "ALA","C" -> 1.55
  | "ALA","CA" -> 2.00
  | "ALA","CB" -> 2.00
  | "ALA","N" -> 1.55
  | "ALA","O" -> 1.40
  | "ALA","OT1" -> 1.40
  | "ALA","OT2" -> 1.40
  | "ALA","OXT" -> 1.40
  | "ARG","C" -> 1.55
  | "ARG","CA" -> 2.00
  | "ARG","CB" -> 2.00
  | "ARG","CD" -> 2.00
  | "ARG","CG" -> 2.00
  | "ARG","CZ" -> 1.75
  | "ARG","N" -> 1.55
  | "ARG","NE" -> 1.55
  | "ARG","NH1" -> 1.55
  | "ARG","NH2" -> 1.55
  | "ARG","O" -> 1.40
  | "ARG","OT1" -> 1.40
  | "ARG","OT2" -> 1.40
  | "ARG","OXT" -> 1.40
  | "ASN","C" -> 1.55
  | "ASN","CA" -> 2.00
  | "ASN","CB" -> 2.00
  | "ASN","CG" -> 1.55
  | "ASN","N" -> 1.55
  | "ASN","ND2" -> 1.55
  | "ASN","O" -> 1.40
  | "ASN","OD1" -> 1.40
  | "ASN","OT1" -> 1.40
  | "ASN","OT2" -> 1.40
  | "ASN","OXT" -> 1.40
  | "ASP","C" -> 1.55
  | "ASP","CA" -> 2.00
  | "ASP","CB" -> 2.00
  | "ASP","CG" -> 1.55
  | "ASP","N" -> 1.55
  | "ASP","O" -> 1.40
  | "ASP","OD1" -> 1.40
  | "ASP","OD2" -> 1.40
  | "ASP","OT1" -> 1.40
  | "ASP","OT2" -> 1.40
  | "ASP","OXT" -> 1.40
  | "CYS","C" -> 1.55
  | "CYS","CA" -> 2.00
  | "CYS","CB" -> 2.00
  | "CYS","N" -> 1.55
  | "CYS","O" -> 1.40
  | "CYS","OT1" -> 1.40
  | "CYS","OT2" -> 1.40
  | "CYS","OXT" -> 1.40
  | "CYS","SG" -> 2.00
  | "GLN","C" -> 1.55
  | "GLN","CA" -> 2.00
  | "GLN","CB" -> 2.00
  | "GLN","CD" -> 1.55
  | "GLN","CG" -> 2.00
  | "GLN","N" -> 1.55
  | "GLN","NE2" -> 1.55
  | "GLN","O" -> 1.40
  | "GLN","OE1" -> 1.40
  | "GLN","OT1" -> 1.40
  | "GLN","OT2" -> 1.40
  | "GLN","OXT" -> 1.40
  | "GLU","C" -> 1.55
  | "GLU","CA" -> 2.00
  | "GLU","CB" -> 2.00
  | "GLU","CD" -> 1.55
  | "GLU","CG" -> 2.00
  | "GLU","N" -> 1.55
  | "GLU","O" -> 1.40
  | "GLU","OE1" -> 1.40
  | "GLU","OE2" -> 1.40
  | "GLU","OT1" -> 1.40
  | "GLU","OT2" -> 1.40
  | "GLU","OXT" -> 1.40
  | "GLY","C" -> 1.55
  | "GLY","CA" -> 2.00
  | "GLY","N" -> 1.55
  | "GLY","O" -> 1.40
  | "GLY","OT1" -> 1.40
  | "GLY","OT2" -> 1.40
  | "GLY","OXT" -> 1.40
  | "HIS","C" -> 1.55
  | "HIS","CA" -> 2.00
  | "HIS","CB" -> 2.00
  | "HIS","CD2" -> 1.75
  | "HIS","CE1" -> 1.75
  | "HIS","CG" -> 1.75
  | "HIS","N" -> 1.55
  | "HIS","ND1" -> 1.55
  | "HIS","NE2" -> 1.55
  | "HIS","O" -> 1.40
  | "HIS","OT1" -> 1.40
  | "HIS","OT2" -> 1.40
  | "HIS","OXT" -> 1.40
  | "ILE","C" -> 1.55
  | "ILE","CA" -> 2.00
  | "ILE","CB" -> 2.00
  | "ILE","CD1" -> 2.00
  | "ILE","CG1" -> 2.00
  | "ILE","CG2" -> 2.00
  | "ILE","N" -> 1.55
  | "ILE","O" -> 1.40
  | "ILE","OT1" -> 1.40
  | "ILE","OT2" -> 1.40
  | "ILE","OXT" -> 1.40
  | "LEU","C" -> 1.55
  | "LEU","CA" -> 2.00
  | "LEU","CB" -> 2.00
  | "LEU","CD1" -> 2.00
  | "LEU","CD2" -> 2.00
  | "LEU","CG" -> 2.00
  | "LEU","N" -> 1.55
  | "LEU","O" -> 1.40
  | "LEU","OT1" -> 1.40
  | "LEU","OT2" -> 1.40
  | "LEU","OXT" -> 1.40
  | "LYS","C" -> 1.55
  | "LYS","CA" -> 2.00
  | "LYS","CB" -> 2.00
  | "LYS","CD" -> 2.00
  | "LYS","CE" -> 2.00
  | "LYS","CG" -> 2.00
  | "LYS","N" -> 1.55
  | "LYS","NZ" -> 1.55
  | "LYS","O" -> 1.40
  | "LYS","OT1" -> 1.40
  | "LYS","OT2" -> 1.40
  | "LYS","OXT" -> 1.40
  | "MET","C" -> 1.55
  | "MET","CA" -> 2.00
  | "MET","CB" -> 2.00
  | "MET","CE" -> 2.00
  | "MET","CG" -> 2.00
  | "MET","N" -> 1.55
  | "MET","O" -> 1.40
  | "MET","OT1" -> 1.40
  | "MET","OT2" -> 1.40
  | "MET","OXT" -> 1.40
  | "MET","SD" -> 2.00
  | "PHE","C" -> 1.55
  | "PHE","CA" -> 2.00
  | "PHE","CB" -> 2.00
  | "PHE","CD1" -> 1.75
  | "PHE","CD2" -> 1.75
  | "PHE","CE1" -> 1.75
  | "PHE","CE2" -> 1.75
  | "PHE","CG" -> 1.75
  | "PHE","CZ" -> 1.75
  | "PHE","N" -> 1.55
  | "PHE","O" -> 1.40
  | "PHE","OT1" -> 1.40
  | "PHE","OT2" -> 1.40
  | "PHE","OXT" -> 1.40
  | "PRO","C" -> 1.55
  | "PRO","CA" -> 2.00
  | "PRO","CB" -> 2.00
  | "PRO","CD" -> 2.00
  | "PRO","CG" -> 2.00
  | "PRO","N" -> 1.55
  | "PRO","O" -> 1.40
  | "PRO","OT1" -> 1.40
  | "PRO","OT2" -> 1.40
  | "PRO","OXT" -> 1.40
  | "SER","C" -> 1.55
  | "SER","CA" -> 2.00
  | "SER","CB" -> 2.00
  | "SER","N" -> 1.55
  | "SER","O" -> 1.40
  | "SER","OG" -> 1.40
  | "SER","OT1" -> 1.40
  | "SER","OT2" -> 1.40
  | "SER","OXT" -> 1.40
  | "THR","C" -> 1.55
  | "THR","CA" -> 2.00
  | "THR","CB" -> 2.00
  | "THR","CG2" -> 2.00
  | "THR","N" -> 1.55
  | "THR","O" -> 1.40
  | "THR","OG1" -> 1.40
  | "THR","OT1" -> 1.40
  | "THR","OT2" -> 1.40
  | "THR","OXT" -> 1.40
  | "TRP","C" -> 1.55
  | "TRP","CA" -> 2.00
  | "TRP","CB" -> 2.00
  | "TRP","CD1" -> 1.75
  | "TRP","CD2" -> 1.75
  | "TRP","CE2" -> 1.75
  | "TRP","CE3" -> 1.75
  | "TRP","CG" -> 1.75
  | "TRP","CH2" -> 1.75
  | "TRP","CZ2" -> 1.75
  | "TRP","CZ3" -> 1.75
  | "TRP","N" -> 1.55
  | "TRP","NE1" -> 1.55
  | "TRP","O" -> 1.40
  | "TRP","OT1" -> 1.40
  | "TRP","OT2" -> 1.40
  | "TRP","OXT" -> 1.40
  | "TYR","C" -> 1.55
  | "TYR","CA" -> 2.00
  | "TYR","CB" -> 2.00
  | "TYR","CD1" -> 1.75
  | "TYR","CD2" -> 1.75
  | "TYR","CE1" -> 1.75
  | "TYR","CE2" -> 1.75
  | "TYR","CG" -> 1.75
  | "TYR","CZ" -> 1.55
  | "TYR","N" -> 1.55
  | "TYR","O" -> 1.40
  | "TYR","OH" -> 1.40
  | "TYR","OT1" -> 1.40
  | "TYR","OT2" -> 1.40
  | "TYR","OXT" -> 1.40
  | "VAL","C" -> 1.55
  | "VAL","CA" -> 2.00
  | "VAL","CB" -> 2.00
  | "VAL","CG1" -> 2.00
  | "VAL","CG2" -> 2.00
  | "VAL","N" -> 1.55
  | "VAL","O" -> 1.40
  | "VAL","OT1" -> 1.40
  | "VAL","OT2" -> 1.40
  | "VAL","OXT" -> 1.40
  | _, _ -> 2.0

(* types are: 
  C1: aliphatic carbon
  C2: aromatic carbon
  O1: Hydroxyl oxygen
  N1: amide and amine
  C3: carbonyl/carboxyl carbon
  O2: carbonyl/carboxyl oxygen
  S1: sulfur and thiol
 *) 

let type_of_atom_option atom = 
  match atom.Atom.label_comp_id, atom.Atom.label_atom_id with
  | "ALA", "C" -> Some "C3"
  | "ALA", "CA" -> Some "C1"
  | "ALA", "CB" -> Some "C1"
  | "ALA", "N" -> Some "N1"
  | "ALA", "O" -> Some "O2"
  | "ALA", "OT1" -> Some "O2"
  | "ALA", "OT2" -> Some "O2"
  | "ALA", "OXT" -> Some "O2"
  | "ARG", "C" -> Some "C3"
  | "ARG", "CA" -> Some "C1"
  | "ARG", "CB" -> Some "C1"
  | "ARG", "CD" -> Some "C1"
  | "ARG", "CG" -> Some "C1"
  | "ARG", "CZ" -> Some "C2"
  | "ARG", "N" -> Some "N1"
  | "ARG", "NE" -> Some "N1"
  | "ARG", "NH1" -> Some "N1"
  | "ARG", "NH2" -> Some "N1"
  | "ARG", "O" -> Some "O2"
  | "ARG", "OT1" -> Some "O2"
  | "ARG", "OT2" -> Some "O2"
  | "ARG", "OXT" -> Some "O2"
  | "ASN", "C" -> Some "C3"
  | "ASN", "CA" -> Some "C1"
  | "ASN", "CB" -> Some "C1"
  | "ASN", "CG" -> Some "C3"
  | "ASN", "N" -> Some "N1"
  | "ASN", "ND2" -> Some "N1"
  | "ASN", "O" -> Some "O2"
  | "ASN", "OD1" -> Some "O2"
  | "ASN", "OT1" -> Some "O2"
  | "ASN", "OT2" -> Some "O2"
  | "ASN", "OXT" -> Some "O2"
  | "ASP", "C" -> Some "C3"
  | "ASP", "CA" -> Some "C1"
  | "ASP", "CB" -> Some "C1"
  | "ASP", "CG" -> Some "C3"
  | "ASP", "N" -> Some "N1"
  | "ASP", "O" -> Some "O2"
  | "ASP", "OD1" -> Some "O2"
  | "ASP", "OD2" -> Some "O2"
  | "ASP", "OT1" -> Some "O2"
  | "ASP", "OT2" -> Some "O2"
  | "ASP", "OXT" -> Some "O2"
  | "CYS", "C" -> Some "C3"
  | "CYS", "CA" -> Some "C1"
  | "CYS", "CB" -> Some "C1"
  | "CYS", "N" -> Some "N1"
  | "CYS", "O" -> Some "O2"
  | "CYS", "OT1" -> Some "O2"
  | "CYS", "OT2" -> Some "O2"
  | "CYS", "OXT" -> Some "O2"
  | "CYS", "SG" -> Some "S1"
  | "GLN", "C" -> Some "C3"
  | "GLN", "CA" -> Some "C1"
  | "GLN", "CB" -> Some "C1"
  | "GLN", "CD" -> Some "C3"
  | "GLN", "CG" -> Some "C1"
  | "GLN", "N" -> Some "N1"
  | "GLN", "NE2" -> Some "N1"
  | "GLN", "O" -> Some "O2"
  | "GLN", "OE1" -> Some "O2"
  | "GLN", "OT1" -> Some "O2"
  | "GLN", "OT2" -> Some "O2"
  | "GLN", "OXT" -> Some "O2"
  | "GLU", "C" -> Some "C3"
  | "GLU", "CA" -> Some "C1"
  | "GLU", "CB" -> Some "C1"
  | "GLU", "CD" -> Some "C3"
  | "GLU", "CG" -> Some "C1"
  | "GLU", "N" -> Some "N1"
  | "GLU", "O" -> Some "O2"
  | "GLU", "OE1" -> Some "O2"
  | "GLU", "OE2" -> Some "O2"
  | "GLU", "OT1" -> Some "O2"
  | "GLU", "OT2" -> Some "O2"
  | "GLU", "OXT" -> Some "O2"
  | "GLY", "C" -> Some "C3"
  | "GLY", "CA" -> Some "C1"
  | "GLY", "N" -> Some "N1"
  | "GLY", "O" -> Some "O2"
  | "GLY", "OT1" -> Some "O2"
  | "GLY", "OT2" -> Some "O2"
  | "GLY", "OXT" -> Some "O2"
  | "HIS", "C" -> Some "C3"
  | "HIS", "CA" -> Some "C1"
  | "HIS", "CB" -> Some "C1"
  | "HIS", "CD2" -> Some "C2"
  | "HIS", "CE1" -> Some "C2"
  | "HIS", "CG" -> Some "C2"
  | "HIS", "N" -> Some "N1"
  | "HIS", "ND1" -> Some "N1"
  | "HIS", "NE2" -> Some "N1"
  | "HIS", "O" -> Some "O2"
  | "HIS", "OT1" -> Some "O2"
  | "HIS", "OT2" -> Some "O2"
  | "HIS", "OXT" -> Some "O2"
  | "ILE", "C" -> Some "C3"
  | "ILE", "CA" -> Some "C1"
  | "ILE", "CB" -> Some "C1"
  | "ILE", "CD1" -> Some "C1"
  | "ILE", "CG1" -> Some "C1"
  | "ILE", "CG2" -> Some "C1"
  | "ILE", "N" -> Some "N1"
  | "ILE", "O" -> Some "O2"
  | "ILE", "OT1" -> Some "O2"
  | "ILE", "OT2" -> Some "O2"
  | "ILE", "OXT" -> Some "O2"
  | "LEU", "C" -> Some "C3"
  | "LEU", "CA" -> Some "C1"
  | "LEU", "CB" -> Some "C1"
  | "LEU", "CD1" -> Some "C1"
  | "LEU", "CD2" -> Some "C1"
  | "LEU", "CG" -> Some "C1"
  | "LEU", "N" -> Some "N1"
  | "LEU", "O" -> Some "O2"
  | "LEU", "OT1" -> Some "O2"
  | "LEU", "OT2" -> Some "O2"
  | "LEU", "OXT" -> Some "O2"
  | "LYS", "C" -> Some "C3"
  | "LYS", "CA" -> Some "C1"
  | "LYS", "CB" -> Some "C1"
  | "LYS", "CD" -> Some "C1"
  | "LYS", "CE" -> Some "C1"
  | "LYS", "CG" -> Some "C1"
  | "LYS", "N" -> Some "N1"
  | "LYS", "NZ" -> Some "N1"
  | "LYS", "O" -> Some "O2"
  | "LYS", "OT1" -> Some "O2"
  | "LYS", "OT2" -> Some "O2"
  | "LYS", "OXT" -> Some "O2"
  | "MET", "C" -> Some "C3"
  | "MET", "CA" -> Some "C1"
  | "MET", "CB" -> Some "C1"
  | "MET", "CE" -> Some "C1"
  | "MET", "CG" -> Some "C1"
  | "MET", "N" -> Some "N1"
  | "MET", "O" -> Some "O2"
  | "MET", "OT1" -> Some "O2"
  | "MET", "OT2" -> Some "O2"
  | "MET", "OXT" -> Some "O2"
  | "MET", "SD" -> Some "S1"
  | "PHE", "C" -> Some "C3"
  | "PHE", "CA" -> Some "C1"
  | "PHE", "CB" -> Some "C1"
  | "PHE", "CD1" -> Some "C2"
  | "PHE", "CD2" -> Some "C2"
  | "PHE", "CE1" -> Some "C2"
  | "PHE", "CE2" -> Some "C2"
  | "PHE", "CG" -> Some "C2"
  | "PHE", "CZ" -> Some "C2"
  | "PHE", "N" -> Some "N1"
  | "PHE", "O" -> Some "O2"
  | "PHE", "OT1" -> Some "O2"
  | "PHE", "OT2" -> Some "O2"
  | "PHE", "OXT" -> Some "O2"
  | "PRO", "C" -> Some "C3"
  | "PRO", "CA" -> Some "C1"
  | "PRO", "CB" -> Some "C1"
  | "PRO", "CD" -> Some "C1"
  | "PRO", "CG" -> Some "C1"
  | "PRO", "N" -> Some "N1"
  | "PRO", "O" -> Some "O2"
  | "PRO", "OT1" -> Some "O2"
  | "PRO", "OT2" -> Some "O2"
  | "PRO", "OXT" -> Some "O2"
  | "SER", "C" -> Some "C3"
  | "SER", "CA" -> Some "C1"
  | "SER", "CB" -> Some "C1"
  | "SER", "N" -> Some "N1"
  | "SER", "O" -> Some "O2"
  | "SER", "OG" -> Some "O1"
  | "SER", "OT1" -> Some "O2"
  | "SER", "OT2" -> Some "O2"
  | "SER", "OXT" -> Some "O2"
  | "THR", "C" -> Some "C3"
  | "THR", "CA" -> Some "C1"
  | "THR", "CB" -> Some "C1"
  | "THR", "CG2" -> Some "C1"
  | "THR", "N" -> Some "N1"
  | "THR", "O" -> Some "O2"
  | "THR", "OG1" -> Some "O1"
  | "THR", "OT1" -> Some "O2"
  | "THR", "OT2" -> Some "O2"
  | "THR", "OXT" -> Some "O2"
  | "TRP", "C" -> Some "C3"
  | "TRP", "CA" -> Some "C1"
  | "TRP", "CB" -> Some "C1"
  | "TRP", "CD1" -> Some "C2"
  | "TRP", "CD2" -> Some "C2"
  | "TRP", "CE2" -> Some "C2"
  | "TRP", "CE3" -> Some "C2"
  | "TRP", "CG" -> Some "C2"
  | "TRP", "CH2" -> Some "C2"
  | "TRP", "CZ2" -> Some "C2"
  | "TRP", "CZ3" -> Some "C2"
  | "TRP", "N" -> Some "N1"
  | "TRP", "NE1" -> Some "N1"
  | "TRP", "O" -> Some "O2"
  | "TRP", "OT1" -> Some "O2"
  | "TRP", "OT2" -> Some "O2"
  | "TRP", "OXT" -> Some "O2"
  | "TYR", "C" -> Some "C3"
  | "TYR", "CA" -> Some "C1"
  | "TYR", "CB" -> Some "C1"
  | "TYR", "CD1" -> Some "C2"
  | "TYR", "CD2" -> Some "C2"
  | "TYR", "CE1" -> Some "C2"
  | "TYR", "CE2" -> Some "C2"
  | "TYR", "CG" -> Some "C2"
  | "TYR", "CZ" -> Some "C3"
  | "TYR", "N" -> Some "N1"
  | "TYR", "O" -> Some "O2"
  | "TYR", "OH" -> Some "O1"
  | "TYR", "OT1" -> Some "O2"
  | "TYR", "OT2" -> Some "O2"
  | "TYR", "OXT" -> Some "O2"
  | "VAL", "C" -> Some "C3"
  | "VAL", "CA" -> Some "C1"
  | "VAL", "CB" -> Some "C1"
  | "VAL", "CG1" -> Some "C1"
  | "VAL", "CG2" -> Some "C1"
  | "VAL", "N" -> Some "N1"
  | "VAL", "O" -> Some "O2"
  | "VAL", "OT1" -> Some "O2"
  | "VAL", "OT2" -> Some "O2"
  | "VAL", "OXT" -> Some "O2"
  | _,_ -> None (* atom.Atom.type_symbol *)

let type_of_atom atom = 
  match type_of_atom_option atom with
  | Some t -> t
  | None -> atom.Atom.type_symbol

(* solvation parameters [kcal/mol A^2] *)
  
let solv_of_atom atom = 
  match type_of_atom atom with
  | "C1" ->  0.008
  | "C2" -> -0.008
  | "O1" -> -0.172
  | "N1" -> -0.132
  | "C3" ->  0.427
  | "O2" -> -0.038 
  | "S1" -> -0.021
  | _    ->  0.0

let standard_types = [ "C1"; "C2"; "O1"; "N1"; "C3"; "O2"; "S1" ]

