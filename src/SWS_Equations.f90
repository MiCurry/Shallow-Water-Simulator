module SWS_Equations_module
    
    implicit none

    public

    ! Dimensions
    integer, parameter :: H = 1
    integer, parameter :: U = 2
    integer, parameter :: V = 3

    type, public :: SWS_Equations
        real :: dx
        real :: g = 9.806
        logical :: periodicity = .true.
    contains
        procedure :: f => f
    end type SWS_Equations

    contains

    function f(this, time, s)

        class (SWS_Equations), intent(inout) :: this
        real, intent(in) :: time
        real, dimension(:,:,:), intent(in) :: s
        real, dimension(size(s,1),size(s,2),size(s,3)) :: f

        integer :: i,j
        integer :: im, ip, jm, jp ! Periodicity 
        real :: dx
        real :: g = 9.806

        f = 0

        dx = this % dx
        g = this % g

        do j = 1, size(s, 2), 1
            do i = 1, size(s, 1), 1

                ip = i
                im = i

                if (i == size(s,1)) then
                    ip = 1
                else if (i < 1) then 
                    im = size(s,1)
                end if

                jp = j
                jm = j

                if (j == size(s,2)) then
                    jp = 1
                else if (j < 1) then
                    jm = size(s,2)
                end if

                 
                ! H Components
                f(j, i, H) = - (s(j,ip,U) * s(jp,i,H) - s(jm,i,V) * s(jm,i,H)) / (2.0 * dx)

                f(j, i, H) = f(j, i, H) + (-(s(jp,i,V) * s(jp,i,H) - s(jm,i,V) * s(jm,i,H)) / (2.0 * dx))


                ! V Components
                f(j, i, V) = - f(j,i,V) * (s(jp,i,V) - s(jm,i,V)) / (2.0 * dx) - g * (s(jp,i,H) - s(jm,i,H)) / (2.0 * dx)

                f(j, i, V) = f(j,i,V) - s(j,i,U) - s(j,i,U) * (s(j,ip,V) - s(j,im,V)) / (2.0 * dx)


                ! U Components
                f(j, i, U) = -f(j,i,U) * (s(j,ip,U) - s(j,im,U)) / (2.0 * dx) - g * (s(j,ip,H) - s(j,im,H)) / (2.0 * dx)

                f(j, i, U) = f(j,i,U) - s(j,i,V) * (s(jp,i,U) - s(jm,i,U)) / (2.0 * dx)

            end do
       end do
    end function 

end module SWS_Equations_module