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

FORCE:
