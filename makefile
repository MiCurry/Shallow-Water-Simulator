FLAGS = -std=f2003 -Wall -Wunused-dummy-argument

all: solver.f90 observer
	gfortran $(FLAGS) -o shallow_water_simulator solver.f90 observer.o

observer: observer.f90
	gfortran $(FLAGS) -c observer.f90 -lnetcdf
