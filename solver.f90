program solver

    use, intrinsic :: ieee_arithmetic
    use observer
    use settings_mod
    use shallow_water

    implicit none

    type(Settings_class) :: settings

    call settings % initalize("namelist.input")

    call observer_init(settings % filename, settings % nx, settings % ny)

    call run() 

    call observer_finialize()

    contains

    subroutine run()

        integer :: i
        integer :: nSteps
        real :: dt
        integer :: time

        real, dimension(:,:,:), allocatable :: state
        real, dimension(:,:,:), allocatable :: k1, k2, k3, k4

        dt = settings % dt
        nSteps = settings % nSteps

        write(0,*) "==============================="
        write(0,*) "Simulation Start"
        write(0,*) "     dt: ", settings % dt
        write(0,*) " nSteps: ", settings % nSteps
        write(0,*) "     nx: ", settings % nx
        write(0,*) "     ny: ", settings % nx
        write(0,*) " Output: ", settings % output_frequency

        allocate(state(settings % nx, settings % ny, 3))
        allocate(k1(settings % nx, settings % ny, 3))

        call initalize(state)
        time = 1
        call observer_write_height(state(:,:,H), time)
        call observer_write_u(state(:,:,U), time)
        call observer_write_V(state(:,:,V), time)
        time = 2

        do i = 1, nSteps, 1
            write(0,*) ""
            write(0,*) "Starting time step ", i, "... dt: ", dt

            k1 = dt * f(state(:,:,:))
            k2 = dt * f(state(:,:,:) + k1 / 2.0)
            k3 = dt * f(state(:,:,:) + k2 / 2.0)
            k4 = dt * f(state(:,:,:) + k3)

            state(:,:,:) = state(:,:,:) + 1./6. * k1 + 1./3. * k2 + 1./3. * k3 + 1./6. * k4

            if (mod(i, settings % output_frequency) == 0) then
                write(0,*) "Writing output!"
                call observer_write_height(state(:,:,H), time)
                call observer_write_u(state(:,:,U), time)
                call observer_write_v(state(:,:,V), time)
                time = time + 1
            end if

            ! Stop the model if nans are detected
            call nan_check(i, state)

            ! Print the min and max of each variable
            call report(i, state)
        end do
       
        write(0,*) "==============================="
        write(0,*) "Simulation End!"
        write(0,*) "     dt: ", settings % dt
        write(0,*) " nSteps: ", settings % nSteps
        write(0,*) "     nx: ", settings % nx
        write(0,*) "     ny: ", settings % nx
        write(0,*) " Output: ", settings % output_frequency
        write(0,*) "==============================="

    end subroutine run

    subroutine nan_check(step, state) 

        implicit none

        integer, intent(in) :: step
        real, dimension(:,:,:), intent(in) :: state

        logical :: nan

        nan = .false.

        if (.not. settings % perform_nan_check .or. .not. mod(step, settings % nan_check_freq) == 0) then
            return
        end if

        write(0,*) "Performing NaN Check"

        if (perform_nan_check(state(:,:,H))) then
            write(0,*) "NaN's detected in the 'height'! Stopping the model"
            nan = .true.
        end if
        if (perform_nan_check(state(:,:,V))) then
            write(0,*) "NaN's detected in the 'V'! Stopping the model"
            nan = .true.
        end if
        if (perform_nan_check(state(:,:,V))) then
            write(0,*) "NaN's detected in the 'U'! Stopping the model"
            nan = .true.
        end if

        if (nan) then
            write(0,*) "NaN detected - Calling observer finalizing and stopping the model"
            call observer_finialize()
            stop
        end if

    end subroutine 

    function perform_nan_check(state) result(nan)
        
        implicit none

        real, dimension(:,:), intent(in) :: state
        logical :: nan

        integer :: i, j

        nan = .false.

        do i = 1, size(state,1), 1
            do j = 1, size(state,2), 2
                if (IEEE_is_nan(state(i,j))) then
                    write(0,*) "NaN Detected at ", i, j
                    nan = .True.
                    return
                end if
            end do
        end do

    end function perform_nan_check

    subroutine report(step, state)

        implicit none

        integer, intent(in) :: step
        real, dimension(:,:,:), intent(in) :: state

        if (.not. settings % perform_report .or. .not. mod(step, settings % report_freq) == 1) then
            return
        end if

        write(0,*) "Performing Min/Max Report"

        write(0,fmt="(A)",advance='no') "Max Height: "
        call report_max(state, H)
        write(0,fmt="(A)",advance='no') "Max V: "
        call report_max(state, V)
        write(0,fmt="(A)",advance='no') "Max U: "
        call report_max(state, U)

        write(0,fmt="(A)",advance='no') "Min Height: "
        call report_min(state, H)

        write(0,fmt="(A)",advance='no') "Min V: "
        call report_min(state, V)

        write(0,fmt="(A)",advance='no') "Min U: "
        call report_min(state, U)

    end subroutine report

    subroutine report_min(state, dim)

        implicit none

        real, dimension(:,:,:), intent(in) :: state 
        integer, intent(in) :: dim
        real :: min_val
        integer, dimension(2) :: min_val_loc

        min_val = minval(state(:,:,dim))
        min_val_loc = minloc(state(:,:,dim))

        write(0,*) min_val, " was at: ", min_val_loc

    end subroutine report_min

    subroutine report_max(state, dim)

        implicit none

        real, dimension(:,:,:), intent(in) :: state 
        integer, intent(in) :: dim
        real :: max_val
        integer, dimension(2) :: max_val_loc

        max_val = maxval(state(:,:,dim))
        max_val_loc = maxloc(state(:,:,dim))

        write(0,*) max_val, " was at: ", max_val_loc

    end subroutine report_max

    subroutine initalize(state)

        implicit none

        real, dimension(:,:,:), intent(out) :: state

        call initalize_height(state(:,:,H))
        call initalize_u(state(:,:,U), 5.0)
        call initalize_v(state(:,:,V), 2.0)

    end subroutine initalize 


    subroutine initalize_height(heights)
        
        implicit none

        real, dimension(:,:), intent(inout) :: heights

        real :: nx, ny
        integer :: i, j

        integer, parameter :: icenter = 25
        real, parameter :: decay = 5.0

        nx = size(heights, 1)
        ny = size(heights, 2)

        do i = 1, size(heights, 1), 1
            do j = 1, size(heights, 2), 1
                heights(i,j) = 1.0
            end do
        end do

    end subroutine initalize_height


    subroutine initalize_u(us, u)
        
        implicit none
        real, dimension(:,:), intent(inout) :: us
        real, intent(in) :: u
        integer :: i, j
        integer, parameter :: icenter = 5
        real, parameter :: decay = 10.0

        do i = 1, size(us, 1), 1
            do j = 1, size(us, 2), 1
                us(i,j) = exp(-decay * (j - icenter)**2)
            end do
        end do

    end subroutine initalize_u


    subroutine initalize_v(vs, v)

        implicit none

        real, dimension(:,:), intent(inout) :: vs
        real, intent(in) :: v
        integer :: i, j
        integer, parameter :: icenter = 5
        real, parameter :: decay = 10.0

        do i = 1, size(vs, 1), 1
            do j = 1, size(vs, 2), 1
                vs(i, j) = exp(-decay * (i - icenter)**2)
            end do
        end do

    end subroutine initalize_v

end program solver
