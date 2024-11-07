module SWS_Simulator_module

    use SWS_Solver_module
    use SWS_DataManager_module
    use SWS_Equations_module
    use SWS_Settings_module
    use SWS_Observer_module

    implicit none

    private

    type, public :: SWS_Sim_Data
        integer, pointer :: step
        integer, pointer :: nsteps
        integer :: nx
        integer :: ny
        real, pointer :: dt
        real, pointer, dimension(:,:,:) :: state
    end type SWS_Sim_Data

    type, public :: SWS_Simulator
        type (SWS_Settings) :: settings
        type (SWS_DataManager) :: datamgr
        type (SWS_Solver) :: solver
        type (SWS_Equations) :: sws_equations
        type (SWS_Sim_Data) :: data
    contains
        procedure :: init => sim_initalize
        procedure :: initalize_h 
        procedure :: initalize_u
        procedure :: initalize_v
        procedure :: start => simulator_loop
        procedure :: finalize => sim_finalize
    end type SWS_Simulator

contains

subroutine sim_initalize(this, namelist_filename)
    implicit none

    class(SWS_Simulator), intent(inout) :: this
    character (len=*), intent(in) :: namelist_filename

    call this % settings % initalize(namelist_filename)

    call this % datamgr % init()

    ! Initalize Data
    allocate(this % data % step)
    allocate(this % data % nSteps)
    allocate(this % data % dt)

    this % data % step = 1
    this % data % nSteps = this % settings % nsteps
    this % data % dt = this % settings % dt
    this % data % nx = this % settings % nx 
    this % data % ny = this % settings % ny

    ! Allocate and Initalize State 
    allocate(this % data % state(this % data % nx, this % data % ny, 3))
    this % data % state(:,:,:) = 0.0
    call this % initalize_h(this % data % state)
    call this % initalize_u(this % data % state)
    call this % initalize_v(this % data % state)

    write(0,*) "Step: ", this % data % step
    write(0,*) "nSteps: ",  this % data % nSteps
    write(0,*) "dt: ", this % data % dt

    ! Intialize Solver
    call this % solver % init(this % data % nx, this % data % ny, this % data % dt, this % settings % dx, & 
        this % settings % solver_choice)

    ! Initalize Observer

    call observer_init(this % settings % filename, this % data % nx, this % data % ny)

end subroutine sim_initalize

subroutine initalize_h(this, state)

    implicit none

    class(SWS_Simulator), intent(inout) :: this
    real, dimension(:,:,:), pointer, intent(inout) :: state

    integer :: i, j

    do i = 1, this % data % ny, 1
        do j = 1, this % data % nx, 1
            if (i > 10 .and. i < 50) then
                state(i,j,H) = 10
            else
                state (i,j,H) = 3
            end if
        end do
    end do

end subroutine initalize_h

subroutine initalize_u(this, state)

    implicit none

    class(SWS_Simulator), intent(inout) :: this
    real, dimension(:,:,:), pointer, intent(inout) :: state

    integer :: i, j

    do i = 1, this % data % ny, 1
        do j = 1, this % data % nx, 1
            state(i,j,U) = 1
        end do
    end do

end subroutine initalize_u

subroutine initalize_v(this, state)

    implicit none

    class(SWS_Simulator), intent(inout) :: this
    real, dimension(:,:,:), pointer, intent(inout) :: state

    integer :: i, j

    do i = 1, this % data % ny, 1
        do j = 1, this % data % nx, 1
            state(i,j,V) = 10
        end do
    end do

end subroutine initalize_v

subroutine simulator_loop(this)

    use SWS_Equations_module

    implicit none

    class(SWS_Simulator), intent(inout) :: this
    real, pointer :: dt
    integer, pointer :: nsteps
    integer :: step
    integer :: time

    time = 1

    call observer_write_height(this % data % state(:,:,H), time)
    call observer_write_U(this % data % state(:,:,U), time)
    call observer_write_V(this % data % state(:,:,V), time)

    time = 2

    do step = this % data % step, this % data % nsteps, 1
        this % data % step = step

        write(0,*) "In Step: ", this % data % step 
        call this % solver % solver(this % data % dt, this % data % state)

        if (mod(step, this % settings % nan_check_freq) == 0) then
            if (do_nan_checking(this)) then
                exit
            end if
        end if 


        if (mod(step, this % settings % output_frequency) == 0) then
            write(0,*) "Time to write output: ", time
            call observer_write_height(this % data % state(:,:,H), time)
            call observer_write_u(this % data % state(:,:,U), time)
            call observer_write_v(this % data % state(:,:,V), time)
            time = time + 1 
        end if
    end do

    call observer_finialize()
end subroutine

function do_nan_checking(this) result(nan)

    implicit none

    class(SWS_Simulator), intent(inout) :: this
    logical :: nan

    nan = .false.

    write(0,*) "Performing nan check ..."

    if (this % datamgr % nan_check(this % data % state(:,:,H))) then
        write(0,*) "NaN dected in the 'Height (H)' field"
        nan = .true.
    end if

    if (this % datamgr % nan_check(this % data % state(:,:,V))) then
        write(0,*) "NaN dected in the 'v' field"
        nan = .true.
    end if

    if (this % datamgr % nan_check(this % data % state(:,:,U))) then
        write(0,*) "NaN dected in the 'u' field"
        nan = .true.
    end if

end function do_nan_checking

subroutine sim_finalize(this)

    implicit none

    class(SWS_Simulator), intent(inout) :: this

    call this % datamgr % finialize()

end subroutine sim_finalize

end module SWS_Simulator_module