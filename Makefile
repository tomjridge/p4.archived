all:
	echo "Make sure to build src_ext first (e.g. type: make src_ext), then type: make build"
	exit 1 # force an error to alert user

build: FORCE
	cd build && $(MAKE)
	./build/p4_test.native
	./build/p4_examples.native
	cd gen && $(MAKE)

src_ext: FORCE
	cd src_ext && $(MAKE) e3_from_git p1_from_git

clean:
	cd build && $(MAKE) clean
	cd gen && $(MAKE) clean

realclean:
	cd src_ext && $(MAKE) realclean
	cd build && $(MAKE) clean

FORCE:
