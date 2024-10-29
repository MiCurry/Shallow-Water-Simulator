module SWS_Simulator_module

    use SWS_Solver_module
    use SWS_DataManager_module
    use SWS_Equations_module
    use SWS_Settings_module

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
        procedure :: init => initalize
        procedure :: initalize_h 
        procedure :: initalize_u
        procedure :: initalize_v
        procedure :: start => simulator_loop
    end type SWS_Simulator

contains

subroutine initalize(this, namelist_filename)
    implicit none

    class(SWS_Simulator), intent(inout) :: this
    character (len=*), intent(in) :: namelist_filename

    call this % settings % initalize(namelist_filename)

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
    allocate(this % data % state(this % data % ny, this % data % nx, 3))
    call this % initalize_h(this % data % state)
    call this % initalize_u(this % data % state)
    call this % initalize_v(this % data % state)

    write(0,*) this % data % state(:,:,1)
    write(0,*) this % data % state(:,:,2)
    write(0,*) this % data % state(:,:,3)

    write(0,*) "Step: ", this % data % step
    write(0,*) "nSteps: ",  this % data % nSteps
    write(0,*) "dt: ", this % data % dt

    ! Intialize Solver
    call this % solver % init(this % data % nx, this % data % ny, this % data % dt, this % settings % dx, & 
        this % settings % solver_choice)

    ! Initalize Observer

    call observer_init(this % settings % filename, this % data % nx, this % data % ny)

end subroutine initalize

subroutine initalize_h(this, state)

    implicit none

    class(SWS_Simulator), intent(inout) :: this
    real, dimension(:,:,:), pointer, intent(inout) :: state

    integer :: i, j

    do i = 1, this % data % ny, 1
        do j = 1, this % data % nx, 1
            state(i,j,H) = 1
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
            state(i,j,U) = 2
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
            state(i,j,V) = 3
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


        write(0,*) mod(this % settings % output_frequency, step)
        if (mod(this % settings % output_frequency, step) == 1) then
            write(0,*) "Time to write output!"
            time = time + 1 
            call observer_write_height(this % data % state(:,:,H), time)
            call observer_write_u(this % data % state(:,:,U), time)
            call observer_write_v(this % data % state(:,:,V), time)
        end if
    end do

    call observer_finialize()
end subroutine

subroutine do_time_step(this)

    implicit none

    class(SWS_Simulator), intent(inout) :: this

end subroutine

end module SWS_Simulator_module