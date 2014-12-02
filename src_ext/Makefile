SHELL=bash

TRAVIS_BRANCH?=$(shell git symbolic-ref --short -q HEAD)
P1_LINK:=../../p1
P1_URL:=https://github.com/tomjridge/p1.git
P1_BRANCH=$(TRAVIS_BRANCH)

E3_LINK:=../../e3
E3_URL:=https://github.com/tomjridge/e3.git
# the following line checks out the version of e3 corresponding to the
# branch of p4 we are on (eg dev or master)
E3_BRANCH=$(TRAVIS_BRANCH)

all: 
	echo "Please link the e3 repository into this directory (see the rest of this Makefile for example)"

# use this if you already have a local copy of e3
p1_link:
	echo "Attempting to link e3 project into src_ext, assuming e3 is available at $(P1_LINK)"
	if [ -d $(P1_LINK) ] && [ ! -e ./p1 ]; then ln -s $(P1_LINK) ./p1; fi
	if [ ! -e ./p1 ]; then echo "Failed to find ./p1 link"; exit 1; fi

# use this to checkout e3 from git
p1_from_git:
	echo "Attempting to clone p1 repo from $(P1_URL)"
	git clone -b $(P1_BRANCH) --depth 1 $(P1_URL)

# use this if you already have a local copy of e3
e3_link:
	echo "Attempting to link e3 project into src_ext, assuming e3 is available at $(E3_LINK)"
	if [ -d $(E3_LINK) ] && [ ! -e ./e3 ]; then ln -s $(E3_LINK) ./e3; fi
	if [ ! -e ./e3 ]; then echo "Failed to find ./e3 link"; exit 1; fi

# use this to checkout e3 from git
e3_from_git:
	echo "Attempting to clone e3 repo from $(E3_URL)"
	git clone -b $(E3_BRANCH) --depth 1 $(E3_URL)

clean:
	rm -f e3

realclean:
	rm -rf e3

