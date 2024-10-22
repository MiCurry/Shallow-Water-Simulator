all: SWS_Simulator.a

SWS_Simulator.a: SWS_Solver.o SWS_Equations.o SWS_Simulator.f90
	mpifort $(FLAGS) -c SWS_Simulator.f90
	ar -ur libSWSSimulator.a SWS_Equations.o SWS_Solver.o SWS_Simulator.o

SWS_Solver.o: SWS_Solver.f90
	mpifort $(FLAGS) -c SWS_Solver.f90

SWS_Equations.o: SWS_Equations.f90
	mpifort $(FLAGS) -c SWS_Equations.f90

clean:
	rm *.mod *.o