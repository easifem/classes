! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
! Vikas Sharma, Ph.D., vickysharma0812@gmail.com
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>
!

SUBMODULE(ElastoDynamics2DFEM_Class) SetMethods

IMPLICIT NONE

REAL(DFP), PARAMETER :: one = 1.0_DFP, zero = 0.0_DFP, minus_one = -1.0_DFP, &
                        half = 0.5_DFP

CONTAINS

!----------------------------------------------------------------------------
!                                                                   obj_Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set()"
#endif
LOGICAL(LGT) :: isok

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[START] ')

obj%currentTimeStep = 1
obj%currentTime = obj%timeRange(1)

CALL obj%InitiateFEDOF()
CALL obj%InitiateFields()
CALL obj%InitiateStressStrainFields()

isok = ALLOCATED(obj%dbc)

IF (isok) THEN
  obj%dbcIndx = obj%sol%GetNodeLoc(dbc=obj%dbc)
END IF

CALL obj%linsolver%set(obj%tanmat)

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[END] ')

END PROCEDURE obj_Set

!----------------------------------------------------------------------------
!                                                      setInitialAcceleration
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetInitialAcceleration
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetInitialAcceleration()"
#endif

LOGICAL(LGT) :: isok

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[START] ')

isok = ASSOCIATED(obj%initialAcc)
IF (.NOT. isok) THEN
  CALL obj%a0%set(VALUE=zero)
  RETURN
END IF

! WARN: currently initial acceleration is directly set with
! nodal value. It does not support all interpolation
CALL obj%a0%SetByFunction(obj%initialAcc)

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[END] ')

END PROCEDURE obj_SetInitialAcceleration

!----------------------------------------------------------------------------
!                                                         SetInitialVelocity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetInitialVelocity
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetInitialVelocity()"
#endif

LOGICAL(LGT) :: isok

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[START] ')

isok = ASSOCIATED(obj%initialVel)
IF (.NOT. isok) THEN
  CALL obj%v0%set(VALUE=zero)
  RETURN
END IF

! WARN: currently initial value is directly set with
! nodal value. It does not support all interpolation
CALL obj%v0%SetByFunction(obj%initialVel)

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[END] ')

END PROCEDURE obj_SetInitialVelocity

!----------------------------------------------------------------------------
!                                                         SetInitialVelocity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetInitialDisplacement
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetInitialDisplacement()"
#endif

LOGICAL(LGT) :: isok

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[START] ')

isok = ASSOCIATED(obj%initialDisp)
IF (.NOT. isok) THEN
  CALL obj%u0%set(VALUE=zero)
  RETURN
END IF

! WARN: currently initial value is directly set with
! nodal value. It does not support all interpolation
CALL obj%u0%SetByFunction(obj%initialDisp)

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[END] ')

END PROCEDURE obj_SetInitialDisplacement

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SetMethods
