module settings_mod

    implicit none
    
    private
    integer :: fid

    type, public :: Settings_class
        character (len=80) :: filename
        integer, private :: fid
        integer :: nx, ny
        integer :: nsteps
        integer :: dt
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

        close(this % fid)

    end subroutine initalize

    subroutine read_io(this)

        implicit none

        class(Settings_class), intent(inout) :: this

        character (len=80) :: filename

        namelist /io/ filename
        read(this % fid, io)

        this%filename = filename

    end subroutine read_io

    subroutine read_swater_sim(this)
        
        implicit none

        class(Settings_class), intent(inout) :: this

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

end module settings_mod
