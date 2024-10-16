FLAGS = -g $(shell nf-config --flibs) -std=f2003 -Wall -Wunused-dummy-argument -fbacktrace -fbounds-check

all: 
	( cd src; $(MAKE) FLAGS="$(FLAGS)" all )
	( mv src/sws_runner . )

clean:
	( cd src; $(MAKE) clean )
	rm sws_runner

