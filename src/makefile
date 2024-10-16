
all: orchastrator settingsManager

orchastrator: settingsManager SWS_Orchastrator.f90
	mpifort $(FLAGS) -o sws_runner SWS_Orchastrator.f90 SettingsManager.o

settingsManager: settingsManager.f90
	mpifort $(FLAGS) -c SettingsManager.f90

clean:
	rm *.o *.mod
