program solver

    use observer

    implicit none

    call observer_init("example.nc", 10, 10)


    call observer_finalize()

end program solver
