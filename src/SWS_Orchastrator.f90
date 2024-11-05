program SWS_Orchastrator

    use SWS_Settings_module
    use SWS_Simulator_module 

    implicit none

    type (SWS_Simulator) :: simulation
    character (len=*), parameter :: namelist_filename = "namelist.input"

    write(0,*) "================================================="
    write(0,*) "       Shallow Water Simulator Started           "


    call simulation % init(namelist_filename)
    call simulation % start()

    write(0,*) "       Shallow Water Simulator Ended"
    write(0,*) "================================================="


end program SWS_Orchastrator 