SHELL=bash

CAMLC:=ocamlfind ocamlc
CAMLOPT:=ocamlfind ocamlopt
CAMLCINCLUDES:=str.cma -I ../src_ext/p1/build p1.cma -I ../src_ext/e3/build e3.cma -I ../build p4.cma
CAMLOPTINCLUDES:=$(CAMLCINCLUDES:.cma=.cmxa)

P1_GEN:=../src_ext/p1/build/p1_gen_p4.native

all: ../src_ext/p1/build/p1.cmxa ../src_ext/e3/build/e3.cmxa ../build/p4.cmxa $(P1_GEN)
	make test.native
	./test.native test.txt

../src_ext/p1/build/p1.cmxa:
	cd ../src_ext/p1/build && make

../src_ext/e3/build/e3.cmxa:
	cd ../src_ext/e3/build && make

../build/p4.cmxa:
	cd ../build && make

test.ml: test.g 
	$(P1_GEN) -g $< >$@

test.native: test.ml
	$(CAMLOPT) $(COMPFLAGS) $(CAMLOPTINCLUDES) -o $@ $<

clean: 
	rm -f *.{cmi,cmx,native,o} test.ml
