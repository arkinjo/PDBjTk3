open Printf
open PDBjTk3
open PDBjBasis

let print_result (dbName,lst) =
  printf "datablock: %s\n" dbName;
  List.iter
    (fun (cat,items) ->
      printf "Category: %s\n" cat;
      List.iter 
	(fun l ->
	  List.iter (fun (k,v) ->	printf "\t%s = %s; " k v) l;
	  print_newline ())
	items)
    lst


let mmcif () =
  let cif_file = Sys.argv.(1) in
  let datablock = MmCIF.parse_file cif_file in
  MmCIF.print stdout datablock


let pdbml () =
  let pdbml_file = Sys.argv.(1) in
  let datablock = PDBML.parse_file pdbml_file in
  PDBML.print stdout datablock

let pdbf () =
  let file = Sys.argv.(1) in
  let datablock = PDBFormat.parse_file file in
  let lst = Datablock.to_PDBML datablock in
  print_result lst

let dict () =
  let dict_file = Sys.argv.(1) in
  let dict = PDBx.read_dictionary dict_file in
  let proc_table table = 
    printf "category: %s\n" table.PDBx.table_name;
    List.iter (fun item ->
      printf "\t%s:\t%s\n" item.PDBx.i_name item.PDBx.i_type)
      table.PDBx.items in
  List.iter proc_table dict.PDBx.tables;
  ()

let _ = 
  try pdbml ()
  with _ -> 
    try mmcif ()
    with _ -> pdbf ()

