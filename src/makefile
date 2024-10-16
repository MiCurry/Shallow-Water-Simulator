
all: orchastrator settingsManager

runner: Runner.f90
	mpifort $(FLAGS) -o sw_runner Orchestrator.o SettingsManager.o

orchastrator: Orchestrator.f90
	mpifort $(FLAGS) -c Orchestrator.f90

settingsManager: settings.f90
	mpifort $(FLAGS) -c SettingsManager.f90