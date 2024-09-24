FLAGS = $(shell nf-config --flibs) -std=f2003 -Wall -Wunused-dummy-argument

all: solver.f90 observer settings
	mpifort -o sw_sim solver.f90 observer.o settings.o $(FLAGS)

observer: observer.f90
	mpifort -c observer.f90 -I$(shell nc-config --includedir) -lnetcdff $(FLAGS)

settings: settings.f90
	mpifort -c settings.f90

clean: observer.o observer.mod
	rm *.mod *.o sw_sim
