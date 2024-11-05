module SWS_DataManager_module

    use, intrinsic :: ieee_arithmetic
    use mpi

    implicit none

    type, public :: SWS_DataManager
        integer :: num_tasks
        integer :: my_proc_id

    contains

        procedure :: nan_check
        procedure :: init => initalize
        procedure :: finialize 
    end type SWS_DataManager

contains

    subroutine initalize(this)

        implicit none

        class(SWS_DataManager), intent(inout) :: this
        integer :: ierr

        call mpi_init(ierr)
        if (ierr /= MPI_SUCCESS) then
            write(0,*) "Error MPI_init failed"
            stop
        end if

        call mpi_comm_size(MPI_COMM_WORLD, this % num_tasks, ierr)
        if (ierr /= MPI_SUCCESS) then
            write(0,*) "Error MPI_comm_size failed"
            stop
        end if

        call mpi_comm_rank(MPI_COMM_WORLD, this % my_proc_id, ierr)
        if (ierr /= MPI_SUCCESS) then
            write(0,*) "Error MPI_comm_rank failed"
            stop
        end if

        write(0, *) "I am: ", this % my_proc_id, " of ", this % num_tasks

    end subroutine initalize

    subroutine finialize(this)

        implicit none

        class(SWS_DataManager), intent(inout) :: this
        integer :: ierr

        call MPI_finalize(ierr)
        if (ierr /= MPI_SUCCESS) then
            write(0,*) "Error calling MPI_finalize"
            stop
        end if 

    end subroutine finialize

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