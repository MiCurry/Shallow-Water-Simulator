module observer

    use netcdf

    implicit none

    private

    public :: observer_init, observer_finalize
    public :: observer_write_height, observer_write_u, observer_write_v

    character (len=80) :: fname
    integer :: n
    integer :: ncid
    integer :: hvar, uvar, vvar
    integer :: n_xDims, n_yDims

    contains

    subroutine observer_init(filename, nx, ny)

        implicit none

        integer, intent(in) :: nx, ny

        integer :: ierr
        integer :: time_dim_id, nx_dim_id, ny_dim_id
        
        integer :: u_var_id, v_var_id, h_var_id

        n_xDims = nx
        n_yDims = ny

        ierr = nf90_create(filename, NF90_CLOBBER, ncid)
        if (ierr /= NF90_NOERR) then
            write(0,*) '*********************************************************************************'
            write(0,*) 'Error creating NetCDF file '//fname
            write(0,*) 'ierr = ', nf90_strerror(ierr)
            write(0,*)'*********************************************************************************'
            stop
        end if

        ierr = nf90_def_dim(ncid, 'time', NF90_UNLIMITED, time_dim_id)
        if (ierr /= NF90_NOERR) then
            write(0,*) '*********************************************************************************'
            write(0,*) 'Error time dimension with unlimited legnth '//fname
            write(0,*) 'ierr = ', nf90_strerror(ierr)
            write(0,*)'*********************************************************************************'
            stop
        end if

        ierr = nf90_def_dim(ncid, 'nx', nx, nx_dim_id)
        if (ierr /= NF90_NOERR) then
            write(0,*) '*********************************************************************************'
            write(0,*) 'Error creating "nx" dimension for: '//fname
            write(0,*) 'ierr = ', nf90_strerror(ierr)
            write(0,*)'*********************************************************************************'
            stop
        end if

        ierr = nf90_def_dim(ncid, 'ny', ny, ny_dim_id)
        if (ierr /= NF90_NOERR) then
            write(0,*) '*********************************************************************************'
            write(0,*) 'Error creating "ny" dimension for: '//fname
            write(0,*) 'ierr = ', nf90_strerror(ierr)
            write(0,*)'*********************************************************************************'
            stop
        end if

        ierr = nf90_def_var(ncid, 'h', NF90_REAL, (/nx_dim_id, ny_dim_id, time_dim_id/), h_var_id)
        if (ierr /= NF90_NOERR) then
            write(0,*) '*********************************************************************************'
            write(0,*) 'Error creating "h" variable for: '//fname
            write(0,*) 'ierr = ', nf90_strerror(ierr)
            write(0,*)'*********************************************************************************'
            stop
        end if

        ierr = nf90_def_var(ncid, 'u', NF90_REAL, (/nx_dim_id, ny_dim_id, time_dim_id/), u_var_id)
        if (ierr /= NF90_NOERR) then
            write(0,*) '*********************************************************************************'
            write(0,*) 'Error creating "u" variable for: '//fname
            write(0,*) 'ierr = ', nf90_strerror(ierr)
            write(0,*)'*********************************************************************************'
            stop
        end if

        ierr = nf90_def_var(ncid, 'v', NF90_REAL, (/nx_dim_id, ny_dim_id, time_dim_id/), v_var_id)
        if (ierr /= NF90_NOERR) then
            write(0,*) '*********************************************************************************'
            write(0,*) 'Error creating "v" variable for: '//fname
            write(0,*) 'ierr = ', nf90_strerror(ierr)
            write(0,*)'*********************************************************************************'
            stop
        end if

    end subroutine observer_init

    subroutine observer_write_height(height, t)

        implicit none

        real, dimension(:), intent(in) :: height
        integer, intent(in) :: t
        integer :: ierr

        ! Write the height
        ierr = nf90_put_var(ncid, h_var_id, height, start=(/1,1,t/), count=(/nx,ny,1/))
        if (ierr /= NF90_NOERR) then
            write(0,*) '*********************************************************************************'
            write(0,*) 'Error writing var "height" for for file: '//fname
            write(0,*) 'ierr = ', ierr
            write(0,*)'*********************************************************************************'
            stop
        end if

    end subroutine observer_write_height


    subroutine observer_write_u(u, t)

        implicit none

        real, dimension(:), intent(in) :: u
        integer, intent(in) :: t
        integer :: ierr

        ! Write the height
        ierr = nf90_put_var(ncid, u_var_id, u, start=(/1,1,t/), count=(/nx,ny,1/))
        if (ierr /= NF90_NOERR) then
            write(0,*) '*********************************************************************************'
            write(0,*) 'Error writing var "u" for for file: '//fname
            write(0,*) 'ierr = ', ierr
            write(0,*)'*********************************************************************************'
            stop
        end if

    end subroutine observer_write_u


    subroutine observer_write_v(v, t)

        implicit none

        real, dimension(:), intent(in) :: v
        integer, intent(in) :: t
        integer :: ierr

        ! Write the height
        ierr = nf90_put_var(ncid, u_var_id, v, start=(/1,1,t/), count=(/nx,ny,1/))
        if (ierr /= NF90_NOERR) then
            write(0,*) '*********************************************************************************'
            write(0,*) 'Error writing var "v" for for file: '//fname
            write(0,*) 'ierr = ', ierr
            write(0,*)'*********************************************************************************'
            stop
        end if

    end subroutine observer_write_v


    subroutine observer_finialize()

        implicit none
        integer :: ierr

        ierr = nf90_close(ncid)
        if (ierr /= NF90_NOERR) then
            write(0,*) '*********************************************************************************'
            write(0,*) 'Error creating closing NetCDF file '//fname
            write(0,*) 'ierr = ', ierr
            write(0,*)'*********************************************************************************'
            stop
        end if

    end subroutine observer_finialize

end module observer
