FLAGS = -g $(shell nf-config --flibs) -std=f2003 -Wall -Wunused-dummy-argument -fbacktrace -fbounds-check

all: 
	( cd src; $(MAKE) FLAGS=$(FLAGS) all )
	( cp src/sw_runner . )