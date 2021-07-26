open ExtLib
open Printf

module Ht = Hashtbl

let warn s = fprintf stderr "<<Warn>> %s\n" s; flush stderr
let warn s = ()

let ech = stdout

let data_types dict tdef c =
  let nerror = ref 0 in
  List.iteri 
    (fun irow data ->
      let cl = List.combine c.MmCIF.items data in
      List.iteri 
	(fun jcol (i,d) ->
	  let idef = PDBx.get_item tdef i in
	  let ddef = PDBx.get_dtype dict idef.PDBx.i_type in
	  if Pcre.pmatch ~rex:ddef.PDBx.d_re d || d = "." || d = "?" 
	  then ()
	  else begin
	    incr nerror;
	    fprintf ech 
	      "Error(data_types): %s.%s ( row %d , col %d ) = <<%s>>: %s /%s/\n"
	      c.MmCIF.name i (irow + 1) (jcol + 1) d
	      ddef.PDBx.d_code ddef.PDBx.d_construct;
	    flush ech
	  end)
	cl)
    c.MmCIF.data;
  !nerror

let mandatory tdef c =
  let nerror = ref 0 in
  let mc = List.filter_map 
      (fun i -> 
	if i.PDBx.i_mandatory 
	then Some i.PDBx.i_name
	else None) 
      tdef.PDBx.items in
  List.iter 
    (fun mi ->
      if List.mem mi c.MmCIF.items then ()
      else begin
	incr nerror;
	fprintf ech 
	  "Error(mandatory): %s.%s is missing.\n" c.MmCIF.name mi;
	flush ech
      end)
    mc;
  List.iteri 
    (fun irow data ->
      let cl = List.combine c.MmCIF.items data in    
      List.iteri 
	(fun jcol (i,d) ->
	  if List.mem i mc && d = "?" then begin
	    incr nerror;
	    fprintf ech 
	      "Error(mandatory): %s.%s ( row %d , col %d ) = '?'.\n" 
	      c.MmCIF.name i (irow+1) (jcol+1);
	    flush ech
	  end)
	cl)
    c.MmCIF.data;
  !nerror

  
let primary_key tdef c =
  let nerror = ref 0 in
  let ht = Ht.create 1 in
  List.iteri 
    (fun irow data ->
      let cl = List.combine c.MmCIF.items data in    
      let keys = List.filter 
	  (fun (i,d) -> List.mem i tdef.PDBx.pkey)
	  cl in
      List.iter 
	(fun (i,d) ->
	  if d = "?" then begin
	    fprintf ech "Error(primary_key): %s.%s not defined\n"
	      c.MmCIF.name i;
	    flush ech;
	  end)
	keys;
      if Ht.mem ht keys then 
	let key = String.concat "," (List.map fst keys) in
	let vals = String.concat "," (List.map snd keys) in
	fprintf ech "Error(primary_key): %s row %d: key (%s) = (%s) already defined in row %d.\n"
	  c.MmCIF.name (irow+1) key vals ((Ht.find ht keys)+1);
	flush ech
      else Ht.add ht keys irow)
    c.MmCIF.data;
  !nerror


let enumeration dict tdef c =
  let nerror = ref 0 in
  let c_enum = List.filter 
      (fun item ->
	match item.PDBx.i_enum with
	| [] -> false
	| _ -> true)
      tdef.PDBx.items in
  let c_enum = List.map 
      (fun item ->
	let ddef = PDBx.get_dtype dict item.PDBx.i_type in
	if ddef.PDBx.d_primitive = "uchar" 
	then 
	  {item with PDBx.i_enum = List.map String.uppercase item.PDBx.i_enum}
	else item)
      c_enum in
  List.iteri 
    (fun irow data -> 
      let cl = List.combine c.MmCIF.items data in
      List.iteri 
	(fun icol (i,d) ->
	  if not(List.exists (fun it -> it.PDBx.i_name = i) c_enum)
	  then ()
	  else
	    let it = List.find (fun it -> it.PDBx.i_name = i) c_enum in
	    let enum = it.PDBx.i_enum in
	    let idef = PDBx.get_item tdef i in
	    let ddef = PDBx.get_dtype dict idef.PDBx.i_type in
	    let d = 
	      if ddef.PDBx.d_primitive = "uchar" 
	      then String.uppercase d
	      else d in
	    if List.mem d enum || d = "?"
	    then ()
	    else begin
	      incr nerror;
	      fprintf ech "Error(enumeration): %s.%s ( row %d , col %d )= <<%s>> not valid ENUM (%s).\n"
		c.MmCIF.name i (irow+1) (icol+1) d  (String.concat "," enum);
	      flush ech;
	    end)
	cl)
    c.MmCIF.data;
  !nerror
	  
let linked_group_1 cif cross_ref =
  let nerror = ref 0 in
  let src = MmCIF.get_category cif cross_ref.PDBx.t_src in
  let dst = MmCIF.get_category cif cross_ref.PDBx.t_dst in
  let isrc,idst = List.split cross_ref.PDBx.item_pairs in
  let lbsrc = String.concat "," isrc and lbdst = String.concat "," idst in
  let htdst = Ht.create 100 in
  if List.exists (fun i -> not(List.mem i src.MmCIF.items)) isrc
  then ()
  else if List.exists (fun i -> not(List.mem i dst.MmCIF.items)) idst 
  then ()
  else begin
    List.iteri 
      (fun irow data ->
	let cl = List.combine dst.MmCIF.items data in
	let dl = List.map (fun i -> List.assoc i cl) idst in
	Ht.add htdst dl irow)
      dst.MmCIF.data;
    List.iteri 
      (fun irow data ->
	let cl = List.combine src.MmCIF.items data in
	let dl = List.map (fun i -> List.assoc i cl) isrc in
	match dl with
	| [] -> ()
	| dl ->
	    let ldst = List.sort ~cmp:(-) (Ht.find_all htdst dl) in
	    let key = String.concat "," dl in
	    let ndst = List.length ldst in
	    if ndst > 1 then 
	      let inds = String.concat "," 
		  (List.map 
		     (fun i -> string_of_int (i+1))
		     (List.take 3 ldst)) in
	      let inds = 
		if List.length ldst > 3 
		then sprintf "%s,...(%d rows)" inds (List.length ldst)
		else inds in
	      incr nerror;
	      fprintf ech "Error(linked_group): Not_unique: %s(%s=%s) [Row %d] -> %s(%s) [Rows %s] .\n"
		src.MmCIF.name lbsrc key (irow+1) dst.MmCIF.name lbdst inds;
	      flush ech
	    else if ndst = 0 && 
	      not(List.exists (fun a -> a = "." || a = "?") dl) then 
	      (incr nerror;
	       fprintf ech
		 "Error(linked_group): Not_found: %s(%s=%s) [Row %d] -> %s(%s) .\n"
		 src.MmCIF.name lbsrc key (irow+1) dst.MmCIF.name lbdst;
	       flush ech)
	    else ())
      src.MmCIF.data;
    let label = cross_ref.PDBx.t_src ^ ":" ^ cross_ref.PDBx.t_dst 
      ^ ":" ^ cross_ref.PDBx.cross_id in
    fprintf ech "Cross-ref %-50s : %d errors.\n" label !nerror;
    flush ech
  end
  
let linked_group dict cif =
  List.iter 
    (fun cross_ref -> 
      if MmCIF.exists_category cif cross_ref.PDBx.t_src &&
	MmCIF.exists_category cif cross_ref.PDBx.t_dst 
      then linked_group_1 cif cross_ref
      else ())
    dict.PDBx.cross_refs

let validate dict cif =
  List.iter (fun c -> 
    let tdef = PDBx.get_table_def dict c.MmCIF.name in
    let cname = c.MmCIF.name in
    let ne1 = data_types dict tdef c in
    warn (cname ^ " data_types done");
    let ne2 = mandatory tdef c in
    warn (cname ^ " mandatory done");
    let ne3 = primary_key tdef c in
    warn (cname ^ " primary_key done");
    let ne4 = enumeration dict tdef c in
    warn (cname ^ " enumeration done");
    fprintf ech "Category %-40s : %d data type ; %d mandatory item ; %d primary key ; %d enumeration , errors\n" c.MmCIF.name ne1 ne2 ne3 ne4;
    flush ech) cif;
  warn "Check.validate(1)";
  linked_group dict cif
