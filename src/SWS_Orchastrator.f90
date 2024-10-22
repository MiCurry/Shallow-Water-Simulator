program SWS_Orchastrator

    use SWS_Settings_module

    implicit none

    type (SWS_Settings) :: settings
    character (len=*), parameter :: namelist_filename = "namelist.input"


    write(0,*) "================================================="
    write(0,*) "       Shallow Water Simulator Started           "

    call settings % initalize(namelist_filename)

    write(0,*) "       Shallow Water Simulator Ended"
    write(0,*) "================================================="

contains

    subroutine setup()

        implicit none

    end subroutine

    subroutine read_commandline_args()

        implicit none

    end subroutine

    subroutine start_model()

        implicit none

    end subroutine start_model


end program SWS_Orchastrator 