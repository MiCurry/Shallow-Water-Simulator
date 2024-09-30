module settings_mod

    implicit none
    
    private

    type, public :: Settings_class
        integer, private :: fid

        ! IO
        character (len=80) :: filename
        integer :: output_frequency

        ! Simulator
        integer :: nx, ny
        integer :: nsteps
        real :: dt
        real :: dx

        ! Solver
        logical :: perform_nan_check
        integer :: nan_check_freq

        logical :: perform_report
        integer :: report_freq

     contains
        procedure :: initalize => initalize
    end type Settings_class

contains

    subroutine initalize(this, filename)
        implicit none

        class(Settings_class), intent(inout) :: this
        character (len=*), intent(in) :: filename


        this % filename = filename
        this % fid = 42
        open(this % fid, file='namelist.input',status='old',form='formatted',action='read')

        call read_io(this)
        call read_swater_sim(this)
        call read_solver(this)

        close(this % fid)

    end subroutine initalize

    subroutine read_io(this)

        implicit none

        class(Settings_class), intent(inout) :: this

        character (len=80) :: filename
        integer :: output_frequency

        namelist /io/ filename, output_frequency
        read(this % fid, io)

        this%filename = filename
        this%output_frequency = output_frequency

    end subroutine read_io

    subroutine read_swater_sim(this)
        
        implicit none

        class(Settings_class), intent(inout) :: this

        integer :: nx, ny
        integer :: nsteps
        real :: dt
        real :: dx

        namelist /sw_sim/ nx, ny, nsteps, dt, dx
        read(this % fid, sw_sim)

        this % nx = nx
        this % ny = ny
        this % nsteps = nsteps
        this % dt = dt
        this % dx = dx

    end subroutine read_swater_sim

    subroutine read_solver(this)

        implicit none

        class(Settings_class), intent (inout) :: this

        logical :: perform_nan_check
        integer :: nan_check_freq

        logical :: perform_report
        integer :: report_freq

        namelist /solver/ perform_nan_check, nan_check_freq, perform_report, report_freq
        read(this % fid, solver)

        this % perform_nan_check = perform_nan_check
        this % nan_check_freq = nan_check_freq

        this % perform_report = perform_report 
        this % report_freq = report_freq

    end subroutine read_solver

end module settings_mod
