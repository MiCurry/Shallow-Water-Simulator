module SWS_Simulator_module

    implicit none

    private

    type, public :: SWS_Simulator
        class(SWS_DataManager) :: datamgr
        class(SWS_Solver) :: solver
        class(SWS_ShallowWater) :: equations
        type(SWS_Sim_Data) :: data
    contains
        procedure :: init => initalize
    end type SWS_Simulator

    type, public :: SWS_Sim_Data
        integer :: step
        integer :: nsteps
        real :: dt
        real, pointer, dimension(:,:,:) :: state
    end type SWS_Sim_Data

contains


subroutine initalize(this, dataManager, solver, equations, simData)

    implicit none

    class(SWS_Simulator), intent(inout) :: this
    class(SWS_DataManager), intent(in) :: dataManager
    class(SWS_Solver), intent(in) :: solver
    class(SWS_ShallowWater), intent(in) :: equations
    type(SWS_Sim_Data), intent(in) :: simData

    this % datamgr = dataManager
    this % solver = solver
    this % equations = equations
    this % data = simData

end subroutine initalize

subroutine simulator_loop(this)

    implicit none

    integer, pointer :: dt
    integer, pointer :: nsteps
    integer, pointer :: step

    step = this % data % step
    nsteps = this % data % nsteps
    dt = this % data % dt

    do step, nSteps, 1
        call this % do_time_step()

        if (this % datamgr % is_time_for_output()) then
            call this % datamgr % write_output()
        end if

    end do

end subroutine

subroutine do_time_step(this)

end subroutine

end module SWS_Simulator_module