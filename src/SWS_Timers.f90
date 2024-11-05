module SWS_Timers_module

    implicit none

    contains

    subroutine start_timer(timer_id)

        use iso_c_binding, only : c_int

        implicit none
    
        interface
            subroutine timer_start(timer_id) bind(C)
                use iso_c_binding, only : c_int
                integer (c_int), intent(in), value :: timer_id
            end subroutine timer_start
        end interface
 
        integer, intent(in) :: timer_id
        integer (c_int) :: timer_id_c

        timer_id_c = timer_id

        call timer_start(timer_id_c)

    end subroutine start_timer

    subroutine end_timer(timer_id, sec, nsec)

        use iso_c_binding, only : c_int

        implicit none

        interface
            subroutine timer_stop(timer_id, sec, nsec) bind(C)
                use iso_c_binding, only : c_int
                integer (c_int), intent(in), value :: timer_id
                integer (c_int), intent(out) :: sec, nsec
            end subroutine timer_stop
        end interface


        integer, intent(in) :: timer_id
        integer, intent(out) :: sec, nsec
        integer (c_int) :: timer_id_c, sec_c, nsec_c

        timer_id_c = timer_id

        call timer_stop(timer_id_c, sec_c, nsec_c)

        sec = sec_c
        nsec = nsec_c

    end subroutine end_timer

end module SWS_Timers_module