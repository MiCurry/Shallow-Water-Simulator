module SWS_Solver_module

    use SWS_Equations_module    

    implicit none

    type, public :: SWS_Solver
        integer :: nx, ny, dt
        type (SWS_Equations) :: sws_equations
        procedure (SWS_Solver_func_interface), pointer :: solver
        ! SWS Simulator
    contains
        procedure :: init => solver_init
    end type SWS_Solver

    abstract interface
        function SWS_f_interface(this, time, state) result(f)
            import SWS_Equations
            class(SWS_Equations), intent(inout) :: this
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
            real, dimension(:,:,:), pointer, intent(inout) :: state
        end subroutine SWS_Solver_func_interface
    end interface

contains

    subroutine solver_init(this, nx, ny, dt, dx, solver_type)

        implicit none

        class(SWS_Solver), intent(inout) :: this
        integer, intent(in) :: nx
        integer, intent(in) :: ny
        real, intent(in) :: dt
        real, intent(in) :: dx
        character (len=80), intent(in) :: solver_type

        write(0,*) "SOLVER: Initalizing Solver!"

        this % nx = nx
        this % ny = ny
        this % dt = dt

        if (solver_type == "euler") then
            this % solver => euler
        else
            write(0,*) "====================================================="
            write(0,*) "ERROR: Unsuproted 'solver_choice' type: ", solver_type
            write(0,*) "ERROR: Only 'euler' is supported for 'solver_choice' option"
            write(0,*) "ERROR: in the '&solver' section of the namelist"
            write(0,*) "====================================================="
            stop
        !    this % solver => rk4_solver
        end if

        this % sws_equations % dx = dx

    end subroutine

    subroutine euler(this, dt, state)

        implicit none

        class(SWS_Solver), intent(inout) :: this
        real, intent(in) :: dt
        real, dimension(:,:,:), pointer, intent(inout) :: state

        integer :: i
        integer :: nx, ny

        real, dimension(:,:,:), allocatable :: k1, k2, k3, k4

        write(0,*) "In Euler doing a solve step: dt:", dt

        nx = size(state, 2)
        ny = size(state, 1)

        allocate(k1(nx, ny, 3))
        allocate(k2(nx, ny, 3))
        allocate(k3(nx, ny, 3))
        allocate(k4(nx, ny, 3))

        k1(:,:,:) = 0.0
        k2(:,:,:) = 0.0
        k3(:,:,:) = 0.0
        k4(:,:,:) = 0.0

        k1 = dt * this % sws_equations % f(dt, state(:,:,:))

        k2 = dt * this % sws_equations % f(dt, state(:,:,:) + k1 / 2.0)

        k3 = dt * this % sws_equations % f(dt, state(:,:,:) + k2 / 2.0)

        k4 = dt * this % sws_equations % f(dt, state(:,:,:) + k3)

        state(:,:,:) = state(:,:,:) + 1.0 / 6.0 * k1 + 1.0 / 3.0 * k2 + 1.0 / 3.0 * k3 + 1.0 / 6.0 * k4

        deallocate(k1)
        deallocate(k2)
        deallocate(k3)
        deallocate(k4)
   
    end subroutine

    subroutine rk4_solver()

        implicit none

    end subroutine rk4_solver
    

end module SWS_Solver_module