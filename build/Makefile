SHELL=bash

SRC_DIR:=../src
MLS_TO_EXEC:=p4_examples.ml p4_test.ml # executables
IGNORE_FOR_LIB:=$(MLS_TO_EXEC) interactive.ml # don't want these in the lib
MLS_FOR_LIB_1=$(shell ocamlfind ocamldep -sort *.ml)
MLS_FOR_LIB=$(filter-out $(IGNORE_FOR_LIB), $(MLS_FOR_LIB_1)) # p1_examples.ml is an executable
CMO=$(MLS_FOR_LIB:.ml=.cmo)

LIB:=p4

CAMLCINCLUDES=str.cma -I ../src_ext/e3/build e3.cma
CAMLOPTINCLUDES=str.cmxa -I ../src_ext/e3/build e3.cmxa

# FIXME probably want to look at warning 8 a bit more closely
WARNINGS=-w -8 #-w -26-8
COMPFLAGS=$(WARNINGS)

all: ../src_ext/e3/build/e3.cma ../src_ext/e3/build/e3.cmxa before_all
	make $(CMO) p4.cma p4.cmxa $(MLS_TO_EXEC:.ml=.native)

../src_ext/e3/build/e3.cma ../src_ext/e3/build/e3.cmxa:
	cd ../src_ext/e3/build && make

doc: p4_lib.mli
	-mkdir ocamldoc
	$(OCAMLDOC) -d ocamldoc -html p4_lib.mli

include Makefile.include

clean:
	rm -f *.ml *.mli *.cmo *.cmi *.cmx *.native *.a *.o *.cma *.cmxa
	rm -f *.{html,css}
	rm -rf ocamldoc
