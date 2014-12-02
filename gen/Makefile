SHELL=bash

CAMLC:=ocamlfind ocamlc
CAMLOPT:=ocamlfind ocamlopt
CAMLCINCLUDES:=str.cma -I ../src_ext/p1/build p1.cma -I ../src_ext/e3/build e3.cma -I ../build p4.cma
CAMLOPTINCLUDES:=$(CAMLCINCLUDES:.cma=.cmxa)

P1_GEN:=../src_ext/p1/build/p1_gen_p4.native

all: ../src_ext/p1/build/p1.cma ../src_ext/e3/build/e3.cma ../build/p4.cma $(P1_GEN)
	make test.native
	./test.native test.txt

test.ml: test.g 
	$(P1_GEN) -g $< >$@

test.native: test.ml
	$(CAMLOPT) $(COMPFLAGS) $(CAMLOPTINCLUDES) -o $@ $<

clean: 
	rm -f *.{cmi,cmx,native,o} test.ml
