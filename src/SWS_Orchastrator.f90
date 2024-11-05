program SWS_Orchastrator

    use SWS_Settings_module
    use SWS_Simulator_module 
    use SWS_Timers_module

    implicit none

    type (SWS_Simulator) :: simulation
    character (len=*), parameter :: namelist_filename = "namelist.input"

    integer :: timer_id, sec, nsec

    timer_id = 1

    write(0,*) "================================================="
    write(0,*) "       Shallow Water Simulator Started           "

    write(0,*) "Starting timer..."
    call start_timer(timer_id)
    write(0,*) ""
    write(0,*) ""

    call simulation % init(namelist_filename)
    call simulation % start()
    call simulation % finalize()


    call end_timer(timer_id, sec, nsec)
    write(0,*) ""
    write(0,*) ""
    write(0,*) "Total Time: ", sec, nsec
    write(0,*) "       Shallow Water Simulator Ended"
    write(0,*) "================================================="


end program SWS_Orchastrator 