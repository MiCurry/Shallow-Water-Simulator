program solver

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

        dt = 0.0
        time = 1
        nSteps = settings % nSteps

        allocate(state(settings % nx, settings % ny, 3))
        allocate(k1(settings % nx, settings % ny, 3))

        call initalize(state)

        do i = 1, nSteps, 1
            k1 = dt * f(dt, state(:,:,:))
            k2 = dt * f(dt, state(:,:,:) + k1 / 2.0)
            k3 = dt * f(dt, state(:,:,:) + k2 / 2.0)
            k4 = dt * f(dt, state(:,:,:) + k3)

            state(:,:,:) = state(:,:,:) + 1.0 / 6.0 * k1 + 1.0 / 3.0 * k2 + 1.0 / 3.0 * k3 + 1.0 / 6.0 * k4

            call observer_write_height(state(:,:,H), time)
            call observer_write_u(state(:,:,U), time)
            call observer_write_V(state(:,:,V), time)

            time = time + 1
        end do

    end subroutine run

    subroutine initalize(state)

        implicit none

        real, dimension(:,:,:), intent(out) :: state

        call initalize_height(state(:,:,H), 10.0)
        call initalize_u(state(:,:,U), 1.0)
        call initalize_v(state(:,:,V), 2.0)

    end subroutine initalize 


    subroutine initalize_height(heights, h)
        
        implicit none

        real, dimension(:,:), intent(inout) :: heights
        real, intent(in) :: h
        integer :: i, j

        do i = 1, size(heights, 1), 1
            do j = 1, size(heights, 2), 1
                heights(j, i) = h
            end do
        end do

    end subroutine initalize_height


    subroutine initalize_u(us, u)
        
        implicit none
        real, dimension(:,:), intent(inout) :: us
        real, intent(in) :: u
        integer :: i, j

        do i = 1, size(us, 1), 1
            do j = 1, size(us, 2), 1
                us(j, i) = u
            end do
        end do

    end subroutine initalize_u


    subroutine initalize_v(vs, v)

        implicit none

        real, dimension(:,:), intent(inout) :: vs
        real, intent(in) :: v
        integer :: i, j

        do i = 1, size(vs, 1), 1
            do j = 1, size(vs, 2), 1
                vs(j, i) = v
            end do
        end do

    end subroutine initalize_v

end program solver
