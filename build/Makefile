SHELL:=bash

CAMLC:=ocamlfind ocamlc
CAMLOPT:=ocamlfind ocamlopt

CAMLDEP:=ocamlfind ocamldep -one-line
OREBUILD:=./orebuild
OCAMLDOC:=ocamlfind ocamldoc

CAMLCINCLUDES:=str.cma -I ../src_ext/e3/build e3.cma
CAMLOPTINCLUDES:=str.cmxa -I ../src_ext/e3/build e3.cmxa

COMPFLAGS?=# maybe set some additional compilation options

MLS:=$(filter-out interactive.ml, $(shell ocamlfind ocamldep -sort *.ml *.mli))
SRC_DIR:=../src

MLS_TO_EXEC:=p4_examples.ml p4_test.ml # executables
IGNORE_FOR_LIB:=$(MLS_TO_EXEC) interactive.ml # don't want these in the lib
MLS_FOR_LIB_1:=$(shell ocamlfind ocamldep -sort $(MLS))
MLS_FOR_LIB:=$(filter-out $(IGNORE_FOR_LIB), $(MLS_FOR_LIB_1)) # p1_examples.ml is an executable
LIB:=p4
CMO:=$(MLS_FOR_LIB:.ml=.cmo)


all: ../src_ext/e3/build/e3.cma ../src_ext/e3/build/e3.cmxa before_all
	$(MAKE) cmo
	$(MAKE) cmx
	$(MAKE) $(LIB).cma $(LIB).cmxa $(MLS_TO_EXEC:.ml=.native)

../src_ext/e3/build/e3.cma ../src_ext/e3/build/e3.cmxa:
	cd ../src_ext/e3/build && make

doc: p4_lib.mli
	-mkdir ocamldoc
	$(OCAMLDOC) -d ocamldoc -html p4_lib.mli

.depend:
	$(CAMLDEP) *.mli *.ml >.depend

# this compiles all .ml and .mli files not just $(CMO)
cmo: $(MLS)
#	echo The following command will be run: $(CAMLC) $(CAMLCINCLUDES) $(COMPFLAGS) -c `$(OREBUILD) .depend $?`
	$(CAMLC) $(CAMLCINCLUDES) $(COMPFLAGS) -c `$(OREBUILD) .depend $?`
	touch cmo



cmx: $(MLS) 
	$(CAMLOPT) $(COMPFLAGS) $(CAMLOPTINCLUDES) -c `$(OREBUILD) .depend $?`
	touch cmx


$(LIB).cma: $(MLS_FOR_LIB:.ml=.cmo)
	$(CAMLC) $(COMPFLAGS) -a -o $@ $(CMO)

$(LIB).cmxa: $(MLS_FOR_LIB:.ml=.cmx)
	$(CAMLOPT) $(COMPFLAGS) -a -o $@ $(CMO:.cmo=.cmx)

$(MLS_TO_EXEC:.ml=.native): %.native: %.ml $(LIB).cmxa
	$(CAMLOPT) $(COMPFLAGS) $(CAMLOPTINCLUDES) -o $@ $(LIB).cmxa $<


before_all: link .depend

link: $(SRC_DIR)
	find $(SRC_DIR) -maxdepth 1 -type f -exec ln -sf \{\} . \;


clean:
	rm -f *.ml *.mli *.cmo *.cmi *.cmx *.native *.a *.o *.cma *.cmxa .depend cmo cmx
	rm -rf ocamldoc orebuild.exe pending.ml.todo

