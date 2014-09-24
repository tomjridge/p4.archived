SHELL=bash

E3_LINK=../../e3
E3_URL=https://github.com/tomjridge/e3.git
BRANCH=master

all: 
	echo "Please link the e3 repository into this directory (see the rest of this Makefile for example)"

# use this if you already have a local copy of e3
e3_link:
	echo "Attempting to link e3 project into src_ext, assuming e3 is available at $(E3_LINK)"
	if [ -d $(E3_LINK) ]; then ln -s $(E3_LINK) ./e3; else echo "Failed to find $(E3_LINK) directory"; exit 1; fi

# use this to checkout e3 from git
e3_from_git:
	echo "Attempting to clone e3 repo from $(E3_URL)"
	git clone -b $(BRANCH) --depth 1 $(E3_URL)

clean:
	rm -f e3

realclean:
	rm -rf e3

