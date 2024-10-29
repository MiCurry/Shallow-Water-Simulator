FLAGS="-g $(shell nf-config --flibs) -std=f2003 -fbacktrace -fbounds-check"

all:
	$(MAKE) -C ./src FLAGS=$(FLAGS)
	(cp ./src/sws_runner . )

clean:
	$(MAKE) -C ./src clean