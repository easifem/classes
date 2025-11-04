SUBMODULE(UP2DFEM_Class) RunMethods

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                    Solve
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Run
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Run()"
#endif
INTEGER(I4B) :: itime

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')

IF (debug) CALL Display("Simulation Starts")

CALL obj%SetInitialAcceleration()
CALL obj%SetInitialVelocity()
CALL obj%SetInitialDisplacement()

CALL obj%AssembleTanmat()
CALL obj%WriteData()

DO itime = 1, obj%totalTimeSteps

  CALL Display(obj%currentTime, myname//" current time: ")
  CALL obj%AssembleTanmat()
  ! CALL obj%AssembleBodySource()
  CALL obj%AssembleSurfaceSource()
  ! CALL obj%AssemblePointSource()
  CALL obj%AssembleRHS()
  CALL obj%ApplyDirichletBC()
  CALL obj%Solve()
  CALL obj%Update()
  CALL obj%WriteData()

END DO

IF (debug) CALL Display("Simulation is Finished")

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
& '[END]')

END PROCEDURE obj_Run

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE RunMethods
