all:
	echo "Make sure to build src_ext first (e.g. type: make src_ext), then type: make build"
	exit 1 # force an error to alert user

build: FORCE
	cd build && make
	./build/p4_test.native
	./build/p4_examples.native

src_ext: FORCE
	cd src_ext && make e3_from_git

clean:
	cd build && make clean

realclean:
	cd src_ext && make realclean
	cd build && make clean

FORCE:
