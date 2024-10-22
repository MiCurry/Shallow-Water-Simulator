FLAGS = -g $(shell nf-config --flibs) -std=f2003  -fbacktrace -fbounds-check -fno-leading-underscore

all: 
	$(MAKE) -C src
	cp src/sws_runner sws_runner

clean:
	rm sws_runner
	$(MAKE) -C src clean
