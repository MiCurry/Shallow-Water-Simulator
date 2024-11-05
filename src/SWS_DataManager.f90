module SWS_DataManager_module

    use, intrinsic :: ieee_arithmetic
    use SWS_Observer_module

    implicit none

    type, public :: SWS_DataManager
        contains
            procedure :: nan_check
    end type SWS_DataManager

contains

    function nan_check(this, state) result(nan)

        implicit none


        class(SWS_DataManager), intent(inout) :: this
        real, dimension(:,:), intent(in) :: state
        logical :: nan

        integer :: i, j

        do i = 1, size(state, 1), 1
            do j = 1, size(state, 2), 1
                if (IEEE_is_nan(state(i,j))) then
                    write(0,*) "NaN Dected at: ", i, j
                    nan = .true.
                    return
                end if
            end do
        end do

    end function nan_check

end module SWS_DataManager_module