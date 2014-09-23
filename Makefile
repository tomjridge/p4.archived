all:
	cd src_ext && make e3_from_git
	cd build && make

clean:
	cd build && make clean

realclean:
	cd src_ext && make realclean
	cd build && make clean
