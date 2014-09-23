SHELL=bash

E3=../../e3

all: 
	echo "Please link the e3 repository into this directory (see the rest of this Makefile for example)"

e3:
	echo "Attempting to link e3 project into src_ext, assuming e3 is available at $(E3)"
	if [ -d $(E3) ]; then ln -s $(E3) ./e3; else echo "Failed to find $(E3) directory"; exit 1; fi

clean:
	rm -f e3
