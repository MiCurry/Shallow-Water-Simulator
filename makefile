FLAGS = -g $(shell nf-config --flibs) -std=f2003 -Wall -Wunused-dummy-argument -fbacktrace -fbounds-check

all: solver.f90 observer settings shallow_water
	mpifort -o sw_sim solver.f90 observer.o settings.o shallow_water.o $(FLAGS)

observer: observer.f90
	mpifort $(FLAGS) -c observer.f90 -I$(shell nc-config --includedir) -lnetcdff $(FLAGS)

settings: settings.f90
	mpifort $(FLAGS) -c settings.f90

shallow_water: shallow_water.f90
	mpifort $(FLAGS) -c shallow_water.f90

clean: observer.o observer.mod
	rm *.mod *.o sw_sim
