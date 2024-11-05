all: SWS_Runner

SWS_Runner: SWS_Orchastrator.f90 SWS_SettingsManager.o SWS_Simulator.o SWS_Timers_module.o
	mpifort -L./ $(FLAGS) -o sws_runner SWS_Orchastrator.f90 SWS_SettingsManager.o SWS_Timers.o cspeed.o -lSWSDataManager -lSWSSimulator 

SWS_SettingsManager.o: SWS_SettingsManager.f90
	mpifort $(FLAGS) -c SWS_SettingsManager.f90

SWS_Simulator.o: SWS_Equations.o SWS_Solver.o SWS_DataManager.o SWS_Simulator.f90
	mpifort $(FLAGS) -c SWS_Simulator.f90
	ar -ur libSWSSimulator.a SWS_Simulator.o SWS_Solver.o SWS_Equations.o

SWS_Solver.o: SWS_Solver.f90
	mpifort $(FLAGS) -c SWS_Solver.f90

SWS_Equations.o: SWS_Equations.f90
	mpifort $(FLAGS) -c SWS_Equations.f90

SWS_DataManager.o: SWS_DataManager.f90 SWS_Observer.o
	mpifort $(FLAGS) -c SWS_DataManager.f90
	ar -ur libSWSDataManager.a SWS_DataManager.o SWS_Observer.o

SWS_Observer.o: SWS_Observer.f90
	mpifort $(FLAGS) -c SWS_Observer.f90 -I$(shell nc-config --includedir) -lnetcdff $(FLAGS)

SWS_Timers_module.o: cspeed.o SWS_Timers.f90
	mpifort -c SWS_Timers.f90

cspeed.o: cspeed.c
	mpicc -c cspeed.c

clean:
	rm *.a *.mod *.o