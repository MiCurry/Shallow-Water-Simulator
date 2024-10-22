all: SWS_Orchastrator.o 

SWS_Orchastrator.o: simulator framework SWS_Orchastrator.f90 SettingsManager.o
	mpifort $(FLAGS) -c SWS_Orchastrator.f90
	mpifort $(FLAGS) -L. -L./framework -L./simulator -o sws_runner SettingsManager.o SWS_Orchastrator.o -lSWSSimulator -lSWSDataManager

SettingsManager.o: SettingsManager.f90
	mpifort $(FLAGS) -c SettingsManager.f90

simulator:
	$(MAKE) -C simulator

framework:
	$(MAKE) -C framework

clean:
	rm *.o *.mod
	$(MAKE) -C simulator clean
	$(MAKE) -C framework clean
