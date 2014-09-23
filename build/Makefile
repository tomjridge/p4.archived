SHELL=bash

CAMLC=$(shell command -v ocamlc.opt || command -v ocamlc || echo missing_ocamlc)
CAMLOPT=$(shell command -v ocamlopt.opt || command -v ocamlopt || echo missing_ocamlopt)
CAMLDEP=$(shell command -v ocamldep.opt || command -v ocamldep || echo missing_ocamldep) -one-line
OCAMLDOC=$(shell command -v ocamldoc.opt || command -v ocamldoc || echo missing_ocamldoc)

# FIXME probably want to look at warning 8 a bit more closely
WARNINGS=-w -8 #-w -26-8
COMPFLAGS=$(WARNINGS)


E3=e3_core.cmo

P4=p4_util.cmo \
p4_core.cmo \
p4_e3_ds.cmo \
p4_e3.cmo \
p4_basic_parsers.cmo \
p4_lib.cmo \

CMO=$(E3) $(P4)

NATIVES=p4_examples.native p4_test.native# 

all: p4.cma p4.cmxa


p4.cma: $(CMO)
	$(CAMLC) $(CAMLCINCLUDES) -a -o $@ $(CMO)

p4.cmxa: $(CMO:.cmo=.cmx)
	$(CAMLOPT) -a -o $@ $(CMO:.cmo=.cmx)

$(NATIVES): %.native: %.ml p3.cmxa
	$(CAMLOPT) $(COMPFLAGS) $(CAMLOPTINCLUDES) -o $@ p3.cmxa $<


# Default rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) $(CAMLCINCLUDES) $(COMPFLAGS) -c $<

.mli.cmi:
	$(CAMLC) $(CAMLCINCLUDES) $(COMPFLAGS) -c $<

.ml.cmx:
	$(CAMLOPT) $(FORPACK) $(COMPFLAGS) $(CAMLOPTINCLUDES) -c $<


doc: $(P3)
	$(OCAMLDOC) -html $(P3:.cmo=.ml)


depend:
	$(CAMLDEP) $(INCLUDES) *.mli *.ml >.depend

include .depend


clean:
	rm -f $(CMO) $(CMO:.cmo=.cmi) $(CMO:.cmo=.cmx) $(NATIVES:.native=.cmo) $(NATIVES:.native=.cmi) $(NATIVES:.native=.cmx) 
	rm -f *.cma *.cmxa *.a $(CMO:.cmo=.o) *.native $(NATIVES:.native=.o) 
	rm -f *.{html,css}

realclean:
