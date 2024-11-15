module SWS_Settings_module

    implicit none
    
    private
    integer :: fid

    type, public :: SWS_Settings 
        integer, private :: fid

        ! IO
        character (len=80) :: filename
        integer :: output_frequency

        ! Data Manager
        integer :: nan_check_freq

        ! Simulator
        real :: g ! Gravity
        integer :: nx, ny
        integer :: nsteps
        integer :: dt
        real :: dx

        ! Solver
        character (len=80) :: solver_choice

     contains
        procedure :: initalize => initalize
    end type SWS_Settings

contains

    subroutine initalize(this, filename)
        implicit none

        class(SWS_Settings), intent(inout) :: this
        character (len=*), intent(in) :: filename


        this % filename = filename
        this % fid = 42
        open(this % fid, file="namelist.input", status='old',form='formatted',action='read')

        call read_io(this)
        call read_swater_sim(this)
        call read_solver(this)
        call read_datamgr(this)

        close(this % fid)

    end subroutine initalize

    subroutine read_io(this)

        implicit none

        class(SWS_Settings), intent(inout) :: this

        character (len=80) :: filename
        integer :: output_frequency

        namelist /io/ filename, output_frequency
        read(this % fid, io)

        this%filename = filename
        this%output_frequency = output_frequency

    end subroutine read_io

    subroutine read_swater_sim(this)
        
        implicit none

        class(SWS_Settings), intent(inout) :: this

        real :: g
        integer :: nx, ny
        integer :: nsteps
        integer :: dt
        real :: dx

        namelist /simulator/ g, nx, ny, nsteps, dt, dx
        read(this % fid, simulator)

        this % g = g
        this % nx = nx
        this % ny = ny
        this % nsteps = nsteps
        this % dt = dt
        this % dx = dx

    end subroutine

    subroutine read_solver(this)

        implicit none

        class(SWS_Settings), intent(inout) :: this

        character (len=80) :: solver_choice

        namelist /solver/ solver_choice
        read(this % fid, solver)

        this % solver_choice = solver_choice

    end subroutine read_solver

    subroutine read_datamgr(this)

        implicit none

        class(SWS_Settings), intent(inout) :: this

        integer :: nan_check_freq

        namelist /datamgr/ nan_check_freq
        read(this % fid, datamgr)

        this % nan_check_freq = nan_check_freq

    end subroutine read_datamgr

end module SWS_Settings_module
