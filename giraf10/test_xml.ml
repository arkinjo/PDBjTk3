open PDBjTk
open PDBjBasis

let _ =
  let file = Sys.argv.(1) in
  let doc = Pdbml.parse_xml_file file in
  prerr_endline "doc parsed"; flush stderr;
  let dblk = Pdbml.datablock_of_xml doc in
  prerr_endline "dblk parsed"; flush stderr;
  let doc = PDBjUtil.xml_of_datablock dblk in
  print_endline (Xml.to_string_fmt doc);
  ()

  
