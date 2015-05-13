all: FORCE
	cd build && $(MAKE)
	cd gen && $(MAKE)

test: all
	./build/p4_test.native
	./build/p4_examples.native

clean:
	cd build && $(MAKE) clean
	cd gen && $(MAKE) clean

FORCE:
