all: SWS_DataManager.a

SWS_DataManager.a: SWS_Observer.o SWS_DataManager.f90
	mpifort $(FLAGS) -c SWS_DataManager.f90
	ar -ur libSWSDataManager.a SWS_DataManager.o SWS_Observer.o

SWS_Observer.o: SWS_Observer.f90
	mpifort $(FLAGS) -c SWS_Observer.f90 -I$(shell nc-config --includedir) -lnetcdff $(FLAGS)


clean:
	rm *.mod *.o