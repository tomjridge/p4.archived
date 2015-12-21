ROOT_DIR:=$(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

all: FORCE
	cd build && $(MAKE)
	cd gen && $(MAKE)

install: all
	ocamlfind install p4 META build/*.cmi  build/p4.cma build/p4.cmxa build/p4.a

test: all
	./build/p4_test.native
	./build/p4_examples.native

clean:
	cd build && $(MAKE) clean
	cd gen && $(MAKE) clean
	ocamlbuild -clean

with_ocamlbuild:
	ocamlbuild -use-ocamlfind -cflags -I,$(ROOT_DIR)/../e3/build -cflags e3.cma p4_lib.cma

FORCE:
