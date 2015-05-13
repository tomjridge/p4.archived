SHELL=bash

include ../Makefile.common

CAMLC:=ocamlfind ocamlc
CAMLOPT:=ocamlfind ocamlopt
CAMLCINCLUDES:=-package str $(P1_PKG) $(E3_PKG) -I ../build p4.cma
CAMLOPTINCLUDES:=$(CAMLCINCLUDES:.cma=.cmxa)

all: 
	make test.native
	./test.native test.txt

test.ml: test.g 
	$(P1_GEN) -g $< >$@

test.native: test.ml
	$(CAMLOPT) $(COMPFLAGS) $(CAMLOPTINCLUDES) -linkpkg -o $@ $<

clean: 
	rm -f *.{cmi,cmx,native,o} test.ml
