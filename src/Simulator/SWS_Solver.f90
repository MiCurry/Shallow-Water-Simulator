module SWS_Solver_module

    implicit none

    type, public :: SWS_Solver
        integer :: nx, ny, dt
        procedure (SWS_Solver_func_interface), pointer :: solver
        ! SWS Simulator
    contains
        procedure :: init => solver_init
    end type SWS_Solver

    abstract interface
        function SWS_f_interface(time, state) result(f)
            real, intent(in) :: time
            real, dimension(:,:,:), intent(in) :: state
            real, dimension(size(state,1), size(state,2), size(state,3)) :: f
        end function SWS_f_interface
    end interface

    abstract interface
        subroutine SWS_Solver_func_interface(this, dt, state)
            import SWS_Solver
            class(SWS_Solver), intent(inout) :: this
            real, intent(in) :: dt
            real, dimension(:,:,:), intent(inout) :: state
        end subroutine SWS_Solver_func_interface
    end interface



    type, public :: SWS_Euler
        real, dimension(:,:,:), allocatable :: k1, k2, k3, k4
    contains
        procedure :: init => euler_init
        procedure :: euler => euler
    end type SWS_Euler

contains

    subroutine solver_init(this, nx, ny, dt, solver_type)

        implicit none

        class(SWS_Solver), intent(inout) :: this
        integer, intent(in) :: nx
        integer, intent(in) :: ny
        real, intent(in) :: dt
        character (len=80), intent(in) :: solver_type

        this % nx = nx
        this % ny = ny
        this % dt = dt

        if (solver_type == "euler") then
        !    this % solver => euler
        else
        !    this % solver => rk4_solver
        end if

    end subroutine

    subroutine euler_init(this)

        implicit none

        class(SWS_Euler), intent(inout) :: this

    end subroutine

    subroutine euler(this, dt, state, f)

        implicit none

        class(SWS_Euler), intent(inout) :: this
        real, intent(in) :: dt
        real, dimension(:,:,:), pointer, intent(inout) :: state
        procedure (SWS_f_interface) :: f

        integer :: i
        integer :: nx, ny

        real, dimension(:,:,:), allocatable :: k1, k2, k3, k4

        nx = size(state, 2)
        ny = size(state, 1)

        allocate(k1(ny, nx, 3))
        allocate(k1(ny, nx, 3))

        k1 = dt * f(dt, state(:,:,:))
        k2 = dt * f(dt, state(:,:,:) + k1 / 2.0)
        k3 = dt * f(dt, state(:,:,:) + k2 / 2.0)
        k4 = dt * f(dt, state(:,:,:) + k3)

        state(:,:,:) = state(:,:,:) + 1.0 / 6.0 * k1 + 1.0 / 3.0 * k2 + 1.0 / 3.0 * k3 + 1.0 / 6.0 * k4
    
    end subroutine

    subroutine rk4_solver()

        implicit none

    end subroutine rk4_solver
    

end module SWS_Solver_module