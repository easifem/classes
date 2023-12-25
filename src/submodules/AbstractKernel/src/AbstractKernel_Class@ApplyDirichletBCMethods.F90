! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
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

SUBMODULE(AbstractKernel_Class) ApplyDirichletMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                         ApplyDirichletBC
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ApplyDirichletBC
CHARACTER(*), PARAMETER :: myName = "obj_ApplyDirichletBC()"
CHARACTER(:), ALLOCATABLE :: name0
LOGICAL(LGT) :: isok, problem
TYPE(CPUTime_) :: TypeCPUTime


IF (obj%showTime) CALL TypeCPUTime%SetStartTime()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

IF (PRESENT(name)) THEN
  name0 = UpperCase(name)
ELSE
  name0 = "NONE"
END IF

isok = ALLOCATED(obj%dbc)
IF (isok) THEN

  SELECT CASE (name0)
  CASE ("DISP", "DISPLACEMENT")
    problem = ASSOCIATED(obj%displacement)
    IF (problem) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: displacement is not ASSOCIATED.')
      RETURN
    END IF
    CALL obj%displacement%ApplyDirichletBC(dbc=obj%dbc, times=times,  &
      & extField=extField)

  CASE ("VEL", "VELOCITY")
    problem = ASSOCIATED(obj%velocity)
    IF (problem) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: velocity is not ASSOCIATED.')
      RETURN
    END IF
    CALL obj%velocity%ApplyDirichletBC(dbc=obj%dbc, times=times,  &
      & extField=extField)

  CASE ("ACC", "ACCELERATION")
    problem = ASSOCIATED(obj%acceleration)
    IF (problem) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: acceleration is not ASSOCIATED.')
      RETURN
    END IF
    CALL obj%acceleration%ApplyDirichletBC(dbc=obj%dbc, times=times,  &
    & extField=extField)

  CASE DEFAULT
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: No case found for name0')
    RETURN
  END SELECT

END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

IF (obj%showTime) THEN
  CALL TypeCPUTime%SetEndTime()
  CALL obj%showTimeFile%WRITE(val=TypeCPUTime%GetStringForKernelLog( &
  & currentTime=obj%currentTime, currentTimeStep=obj%currentTimeStep, &
  & methodName=myName))
END IF
END PROCEDURE obj_ApplyDirichletBC

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ApplyDirichletMethods
