program solver

    use observer
    use settings_mod

    implicit none

    type(Settings_class) :: settings

    call settings % initalize("namelist.input")

    call observer_init(settings % filename, settings % nx, settings % ny)

    call observer_finialize()

end program solver
