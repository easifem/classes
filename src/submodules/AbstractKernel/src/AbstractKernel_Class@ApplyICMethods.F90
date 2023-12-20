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

SUBMODULE(AbstractKernel_Class) ApplyICMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    ApplyIC
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ApplyIC
CHARACTER(*), PARAMETER :: myName = "obj_ApplyIC()"
CHARACTER(:), ALLOCATABLE :: name0
LOGICAL(LGT) :: problem, isfunc, isext

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

IF (PRESENT(name)) THEN
  name0 = UpperCase(name)
ELSE
  name0 = "NONE"
END IF

isext = PRESENT(extField)
isfunc = PRESENT(func)

SELECT CASE (name0)
CASE ("DISP", "DISPLACEMENT")
  problem = .NOT. ASSOCIATED(obj%displacement)
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: displacement is not ASSOCIATED.')
    RETURN
  END IF

  CALL obj%displacement%Set(VALUE=0.0_DFP)

  IF (isfunc) THEN
    CALL obj%displacement%Set(func=func, times=times,  &
      & ivar=ivar, idof=idof, spaceCompo=spaceCompo, timeCompo=timeCompo)
  END IF

  IF (isext) THEN
    CALL obj%displacement%Copy(obj2=extField)
  END IF

CASE ("VEL", "VELOCITY")
  problem = .NOT. ASSOCIATED(obj%velocity)
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: velocity is not ASSOCIATED.')
    RETURN
  END IF

  CALL obj%velocity%Set(VALUE=0.0_DFP)

  IF (isfunc) THEN
    CALL obj%velocity%Set(func=func, times=times, &
      & ivar=ivar, idof=idof, spaceCompo=spaceCompo, timeCompo=timeCompo)
  END IF

  IF (isext) THEN
    CALL obj%velocity%Copy(obj2=extField)
  END IF

CASE ("ACC", "ACCELERATION")
  problem = .NOT. ASSOCIATED(obj%acceleration)
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: acceleration is not ASSOCIATED.')
    RETURN
  END IF

  CALL obj%acceleration%Set(VALUE=0.0_DFP)

  IF (isfunc) THEN
    CALL obj%acceleration%Set(func=func, times=times, &
      & ivar=ivar, idof=idof, spaceCompo=spaceCompo, timeCompo=timeCompo)
  END IF

  IF (isext) THEN
    CALL obj%acceleration%Copy(obj2=extField)
  END IF

CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: No case found for name0')
  RETURN
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER
END PROCEDURE obj_ApplyIC

END SUBMODULE ApplyICMethods
