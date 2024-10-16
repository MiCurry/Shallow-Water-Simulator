module SWS_Settings_module

    implicit none
    
    private
    integer :: fid

    type, public :: SWS_Settings 
        integer, private :: fid

        ! IO
        character (len=80) :: filename
        integer :: output_frequency

        ! Simulator
        integer :: nx, ny
        integer :: nsteps
        integer :: dt
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
        open(this % fid, file='namelist.input',status='old',form='formatted',action='read')

        call read_io(this)
        call read_swater_sim(this)

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

        integer :: nx, ny
        integer :: nsteps
        integer :: dt

        namelist /sw_sim/ nx, ny, nsteps, dt
        read(this % fid, sw_sim)

        this % nx = nx
        this % ny = ny
        this % nsteps = nsteps
        this % dt = dt

    end subroutine

end module SWS_Settings_module
