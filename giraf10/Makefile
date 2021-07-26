TARGET= giraf_load giraf_ifs giraf_search 
MODULES = util prep_struct annot cmp_obj \
	frame_t frame_j \
	refset interface patom gi ir load_data \
	prepin save_results
# godi's camlzip doesn't work for 64-bit ocamlopt for some reason...
INCDIRS = -I ../lib 
INSTALLDIR=/work_local/akinjo/giraf/bin

LINK_PKG = unix,bigarray,num,str,pcre,camlzip,extlib,threads,postgresql,xml-light,gsl,ocamlgraph,atdgen,parmap
COMP_PKG = unix,bigarray,num,str,pcre,camlzip,extlib,threads,postgresql,xml-light,gsl,ocamlgraph,atdgen,parmap
LIBS =  PDBjTk
NCFLAGS = -thread -inline 100 
BCFLAGS = -thread -g
SRCS = $(MODULES:=.ml)
TARGET_SRCS = $(TARGET:=.ml)
NOBJS = $(MODULES:=.cmx)
BOBJS = $(MODULES:=.cmo)
NCLIBS = $(LIBS:=.cmxa)
BCLIBS = $(LIBS:=.cma)
TARGET_BC = $(TARGET:=_bc)
OCAMLFIND = ocamlfind
OCAMLOPT = ocamlopt
OCAMLC = ocamlc

.SUFFIXES: .cma .cmxa .cmi .cmo .cmx .ml


all: $(NOBJS) $(TARGET) ../lib/PDBjTk.cmxa
bc: $(TARGET_BC)

install: all
	cp $(TARGET) $(INSTALLDIR)

frame_j.mli: frame.atd
frame_j.ml: frame.atd
	atdgen -j $^
frame_t.mli: frame.atd
frame_t.ml: frame.atd
	atdgen -t $^
test: $(NOBJS) test.cmx
	$(OCAMLFIND) $(OCAMLOPT) $(NCFLAGS) -o $@ -package $(LINK_PKG) -linkpkg $(INCDIRS) $(NCLIBS) $(NOBJS) test.cmx
fit_emotif: fit_emotif.cmx $(NOBJS) 
	$(OCAMLFIND) $(OCAMLOPT) $(NCFLAGS) -o $@ -package $(LINK_PKG) -linkpkg $(INCDIRS) $(NCLIBS) $(NOBJS) $<
make_gi_query: make_gi_query.cmx $(NOBJS) 
	$(OCAMLFIND) $(OCAMLOPT) $(NCFLAGS) -o $@ -package $(LINK_PKG) -linkpkg $(INCDIRS) $(NCLIBS) $(NOBJS) $<
greedy_cluster: greedy_cluster.cmx $(NOBJS) 
	$(OCAMLFIND) $(OCAMLOPT) $(NCFLAGS) -o $@ -package $(LINK_PKG) -linkpkg $(INCDIRS) $(NCLIBS) $(NOBJS) $<
cluster_by_tanimoto: cluster_by_tanimoto.cmx $(NOBJS) 
	$(OCAMLFIND) $(OCAMLOPT) $(NCFLAGS) -o $@ -package $(LINK_PKG) -linkpkg $(INCDIRS) $(NCLIBS) $(NOBJS) $<
extract_ligands: extract_ligands.cmx $(NOBJS) 
	$(OCAMLFIND) $(OCAMLOPT) $(NCFLAGS) -o $@ -package $(LINK_PKG) -linkpkg $(INCDIRS) $(NCLIBS) $(NOBJS) $<
fill_gi_refsets: fill_gi_refsets.cmx $(NOBJS) 
	$(OCAMLFIND) $(OCAMLOPT) $(NCFLAGS) -o $@ -package $(LINK_PKG) -linkpkg $(INCDIRS) $(NCLIBS) $(NOBJS) $<
decomp_graph: decomp_graph.cmx
	$(OCAMLFIND) $(OCAMLOPT) $(NCFLAGS) -o $@ -package $(LINK_PKG) -linkpkg $(INCDIRS) $(NCLIBS) decomp_graph.cmx
seq_pair_histo: seq_pair_histo.cmx
	$(OCAMLFIND) $(OCAMLOPT) $(NCFLAGS) -o $@ -package $(LINK_PKG) -linkpkg $(INCDIRS) $(NCLIBS) seq_pair_histo.cmx
align_ifs: align_ifs.cmx
	$(OCAMLFIND) $(OCAMLOPT) $(NCFLAGS) -o $@ -package $(LINK_PKG) -linkpkg $(INCDIRS) $(NCLIBS) $(NOBJS) align_ifs.cmx
label_cluster: label_cluster.cmx
	$(OCAMLFIND) $(OCAMLOPT) $(NCFLAGS) -o $@ -package $(LINK_PKG) -linkpkg 
label_cmotif: label_cmotif.cmx
	$(OCAMLFIND) $(OCAMLOPT) $(NCFLAGS) -o $@ -package $(LINK_PKG) -linkpkg $(INCDIRS) $(NCLIBS) $(NOBJS) $<
decomp_blast_motif: decomp_blast_motif.cmx
	$(OCAMLFIND) $(OCAMLOPT) $(NCFLAGS) -o $@ -package $(LINK_PKG) -linkpkg $(INCDIRS) $(NCLIBS) $(NOBJS) decomp_blast_motif.cmx
cluster_blast: cluster_blast.cmx
	$(OCAMLFIND) $(OCAMLOPT) $(NCFLAGS) -o $@ -package $(LINK_PKG) -linkpkg $(INCDIRS) $(NCLIBS) $(NOBJS) cluster_blast.cmx
cluster_blast2: cluster_blast2.cmx
	$(OCAMLFIND) $(OCAMLOPT) $(NCFLAGS) -o $@ -package $(LINK_PKG) -linkpkg $(INCDIRS) $(NCLIBS) $(NOBJS) cluster_blast2.cmx
load_blast: load_blast.cmx
	$(OCAMLFIND) $(OCAMLOPT) $(NCFLAGS) -o $@ -package $(LINK_PKG) -linkpkg $(INCDIRS) $(NCLIBS) $(NOBJS) load_blast.cmx
giraf_hello: giraf_hello.cmx
	$(OCAMLFIND) $(OCAMLOPT) $(NCFLAGS) -o $@ -package $(LINK_PKG) -linkpkg $(INCDIRS) $(NCLIBS) $(NOBJS) giraf_hello.cmx
cluster_objects: cluster_objects.cmx
	$(OCAMLFIND) $(OCAMLOPT) $(NCFLAGS) -o $@ -package $(LINK_PKG) -linkpkg $(INCDIRS) $(NCLIBS) $(NOBJS) cluster_objects.cmx
test_client: test_client.cmx
	$(OCAMLFIND) $(OCAMLOPT) $(NCFLAGS) -o $@ -package $(LINK_PKG) -linkpkg $(INCDIRS) $(NCLIBS) $(NOBJS) test_client.cmx
test_server: test_server.cmx
	$(OCAMLFIND) $(OCAMLOPT) $(NCFLAGS) -o $@ -package $(LINK_PKG) -linkpkg $(INCDIRS) $(NCLIBS) $(NOBJS) test_server.cmx
giraf_detweight: giraf_detweight.cmx
	$(OCAMLFIND) $(OCAMLOPT) $(NCFLAGS) -o $@ -package $(LINK_PKG) -linkpkg $(INCDIRS) $(NCLIBS) $(NOBJS) giraf_detweight.cmx
girafd: girafd.cmx
	$(OCAMLFIND) $(OCAMLOPT) $(NCFLAGS) -o $@ -package $(LINK_PKG) -linkpkg $(INCDIRS) $(NCLIBS) $(NOBJS) girafd.cmx
giraf_ifs: giraf_ifs.cmx
	$(OCAMLFIND) $(OCAMLOPT) $(NCFLAGS) -o $@ -package $(LINK_PKG) -linkpkg $(INCDIRS) $(NCLIBS) $(NOBJS) giraf_ifs.cmx
giraf_dock: giraf_dock.cmx
	$(OCAMLFIND) $(OCAMLOPT) $(NCFLAGS) -o $@ -package $(LINK_PKG) -linkpkg $(INCDIRS) $(NCLIBS) $(NOBJS) giraf_dock.cmx
giraf_search: giraf_search.cmx
	$(OCAMLFIND) $(OCAMLOPT) $(NCFLAGS) -o $@ -package $(LINK_PKG) -linkpkg $(INCDIRS) $(NCLIBS) $(NOBJS) giraf_search.cmx
giraf_align: giraf_align.cmx
	$(OCAMLFIND) $(OCAMLOPT) $(NCFLAGS) -o $@ -package $(LINK_PKG) -linkpkg $(INCDIRS) $(NCLIBS) $(NOBJS) giraf_align.cmx
giraf_load: giraf_load.cmx
	$(OCAMLFIND) $(OCAMLOPT) $(NCFLAGS) -o $@ -package $(LINK_PKG) -linkpkg $(INCDIRS) $(NCLIBS) $(NOBJS) giraf_load.cmx

frame_t.cmi: frame_t.mli
	$(OCAMLFIND) $(OCAMLOPT) $(NCFLAGS) -package $(COMP_PKG) $(INCDIRS) -c $<
frame_j.cmi: frame_j.mli
	$(OCAMLFIND) $(OCAMLOPT) $(NCFLAGS) -package $(COMP_PKG) $(INCDIRS) -c $<

.mli.cmi:
	$(OCAMLFIND) $(OCAMLOPT) $(NCFLAGS) -package $(COMP_PKG) $(INCDIRS) -c $<
.ml.cmx:
	$(OCAMLFIND) $(OCAMLOPT) $(NCFLAGS) -package $(COMP_PKG) $(INCDIRS) -c $<
.ml.cmo:
	$(OCAMLFIND) $(OCAMLC) -package $(COMP_PKG) $(BCFLAGS) $(INCDIRS) -c $<
depend:
	$(OCAMLFIND) ocamldep $(SRCS) $(TARGET_SRCS) test*.ml > .depend
clean:
	rm -f *.cmi *.cmo *.cmx *.o
realclean: clean
	rm -rf .depend $(TARGET) test

-include .depend
