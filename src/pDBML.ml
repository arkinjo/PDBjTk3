open ExtLib
open Xml
open Printf
open PDBjBasis

exception Invalid_PDBML of string

let strip_pdbx name = snd(String.replace ~str:name ~sub:"PDBx:" ~by:"")

let proc_celem cat = function
  | Element (cat,atts,kids) -> 
      let items = List.filter_map 
	(function
	  | Element(item,_,[PCData v]) -> 
	      let iname = PDBjDict.pdbml_to_mmcif_item (strip_pdbx item) in
	      Some(iname,v)
	  | _ -> None)
	kids in
      List.append atts items
  | _ -> raise (Invalid_PDBML ("Category element: " ^ cat))

let proc_category = function
  | Element(cat,atts,kids) when String.ends_with cat "Category" ->
      let _,cat = String.replace ~str:cat ~sub:"Category" ~by:"" in
      (strip_pdbx cat),(List.map (proc_celem cat) kids)
  | _ -> raise (Invalid_PDBML "Category")

let doc_to_alist = function
  | Element("PDBx:datablock",atts,kids) ->
      let dbName = List.assoc "datablockName" atts in
      let categories = List.map proc_category kids in
      dbName,categories
  | _ -> raise (Invalid_PDBML "datablock")


let parse_string doc =
  let doc = Xml.parse_string doc in
  Datablock.of_mmCIF (doc_to_alist doc)

let parse ic = 
  let doc = Xml.parse_in ic in
  Datablock.of_mmCIF (doc_to_alist doc)

let parse_file file =
  Ecomm.read_gzipped_maybe parse file

let doc_of_table (cname,elems) =
  let cname_tag = "PDBx:" ^ cname in
  let proc items = 
    let atts,elems = List.fold_left 
	(fun (atts,elems) (k,v) ->
	  let ename = PDBjDict.mmcif_to_pdbml_item k in
	  if PDBjDict.item_is_pkey (cname,k)
	  then ((ename,v)::atts),elems
	  else atts,(Element("PDBx:" ^ ename,[],[PCData v])::elems))
	([],[])	items in
    Element(cname_tag,atts,elems)
  in
  Element(cname_tag^ "Category",[],List.map proc elems)

let xml_of_datablock datablock =  
  let dbName, tables = Datablock.to_mmCIF datablock in
  Element("PDBx:datablock",
	  [("datablockName",dbName);
	   ("xmlns:PDBx","http://pdbml.pdb.org/schema/" ^ PDBjDict.xsd_file);
	   ("xmlns:xsi","http://www.w3.org/2001/XMLSchema-instance");
	   ("xsi:schemaLocation",
	    sprintf "http://pdbml.pdb.org/schema/%s %s" PDBjDict.xsd_file PDBjDict.xsd_file)],
	  List.map doc_of_table tables)



let print oc datablock =
  let doc = xml_of_datablock datablock in
  output_string oc (Xml.to_string_fmt doc);
  flush oc
