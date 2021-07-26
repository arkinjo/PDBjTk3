prefix = ../
exec_prefix = ${prefix}
BINDIR = ${exec_prefix}/bin
LIBDIR = ${exec_prefix}/lib
DESTDIR= .
TMODULE= PDBjTk
MODULES0 = ecomm star_syntax star_parser star_lexer star pDBx
MODULES = $(MODULES0) pDBjBasis vec3 fheap wbm \
	mmCIF pDBML atom_t atom_j atom \
	pDBFormat intCell pDBjUtil oONS asa blast biounit bLOSUM \
	sql
TARGET= $(TMODULE).cmxa

CFLAGS= -thread
LFLAGS= -thread
OFLAGS= -for-pack $(TMODULE)
LIBS= 
LINK_PKG= unix,threads,extlib,str,pcre,ocamlgraph,xml-light,gsl,atdgen,postgresql
COMP_PKG= unix,threads,extlib,str,pcre,ocamlgraph,xml-light,gsl,atdgen,postgresql
SRCS = star_parser.mly star_lexer.mll $(MODULES:=.ml)
NOBJS0 = $(MODULES0:=.cmx)
NOBJS = $(MODULES:=.cmx)

INSTALL = /usr/bin/install -c
INSTALL_PROGRAM = ${INSTALL}
INSTALL_DATA = ${INSTALL} -m 644
INSTALL_LIB = ${INSTALL} -m 644

.SUFFIXES: .ml .mli .cmo .cmx .cmi .mll .mly

all: $(TARGET)
install: all
	mkdir -p $(DESTDIR)/$(LIBDIR)
	$(INSTALL_LIB) $(TARGET) $(TMODULE).a $(TMODULE).cmi $(DESTDIR)/$(LIBDIR)

dict2modules: dict2modules.cmx $(NOBJS0)
	ocamlfind ocamlopt -o $@ $(LFLAGS) -package $(LINK_PKG) -linkpkg $(LIBS) $(NOBJS0) $<

test.cmx: PDBjTk.cmxa
test: test.cmx 
	ocamlfind ocamlopt -o $@ $(LFLAGS) -package $(LINK_PKG) -linkpkg $(LIBS) PDBjTk.cmxa $<
$(TMODULE).cmxa: $(TMODULE).cmx
	$(OCAMLFIND) ocamlopt -a $(LFLAGS) -o $@ $^
$(TMODULE).cmx: $(NOBJS)
	$(OCAMLFIND) ocamlopt $(CFLAGS) -pack -o $@ $^
atom_t.mli: atom.atd
atom_t.ml: atom.atd
	atdgen -t $^
atom_j.mli: atom.atd

atom_j.ml: atom.atd
	atdgen -j $^
pDBjBasis.ml: dict2modules
	./dict2modules ../data/mmcif_pdbx_v40.dic > $@
starUtil.cmxa: $(NOBJS)
	ocamlfind ocamlopt -a -o starUtil.cmxa $(LFLAGS) -package $(LINK_PKG) $(NOBJS)
lib: starUtil.cmxa
depend:
	ocamldep *.mli *.ml *.mly *.mll > .depend
clean:
	rm -f *.a *.cmxa *.cma *.cmo *.cmi *.cmx *.o star_parser.ml star_parser.mli star_lexer.ml
realclean: clean
	rm -f $(TARGET) $(LIBS) test
# SUFFIXES
.ml.cmo:
	ocamlfind ocamlc $(CFLAGS) -package $(COMP_PKG) $(CFLAGS) -c $<
.ml.cmx:
	ocamlfind ocamlopt $(CFLAGS) $(OFLAGS) -package $(COMP_PKG) $(CFLAGS) -c $<
.mli.cmi:
	ocamlfind ocamlc -package $(COMP_PKG) $(CFLAGS) $<
.mll.ml:
	ocamllex $<
.mly.ml:
	ocamlyacc $<
star_lexer.ml: star_lexer.mll
star_parser.mli: star_parser.mly
star_parser.ml: star_parser.mly
star_parser.cmi: star_parser.mli
star_parser.cmo: star_syntax.cmo star_parser.cmi 
star_parser.cmx: star_syntax.cmx star_parser.cmi 
star_lexer.cmo: star_parser.cmi 
star_lexer.cmx: star_parser.cmx 
star.cmo: star_syntax.cmo star_parser.cmi star_lexer.cmo 
star.cmx: star_syntax.cmx star_parser.cmx star_lexer.cmx 

-include .depend
