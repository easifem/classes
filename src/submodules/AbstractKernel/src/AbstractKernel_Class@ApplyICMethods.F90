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
!
!----------------------------------------------------------------------------

SUBROUTINE applyIC2Vec(obj, name, func, extField, times, ivar,  &
    & idof, spaceCompo, timeCompo)
  CLASS(AbstractKernel_), INTENT(INOUT) :: obj
  CHARACTER(*), OPTIONAL, INTENT(IN) :: name
  CLASS(UserFunction_), OPTIONAL, INTENT(INOUT) :: func
  CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: extField
  REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: idof
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: spaceCompo
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: timeCompo

  CHARACTER(*), PARAMETER :: myName = "ApplyIC2Vec()"
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

END SUBROUTINE applyIC2Vec

!----------------------------------------------------------------------------
!                                                              ApplyIC2Scalar
!----------------------------------------------------------------------------

SUBROUTINE applyIC2Scalar(obj, name, func, extField, times, ivar,  &
    & idof, spaceCompo, timeCompo)
  CLASS(AbstractKernel_), INTENT(INOUT) :: obj
  CHARACTER(*), OPTIONAL, INTENT(IN) :: name
  CLASS(UserFunction_), OPTIONAL, INTENT(INOUT) :: func
  CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: extField
  REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: idof
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: spaceCompo
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: timeCompo

  CHARACTER(*), PARAMETER :: myName = "applyIC2Scalar()"
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
  CASE ("PRESSURE", "PRES")
    problem = .NOT. ASSOCIATED(obj%pressure)
    IF (problem) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: pressure is not ASSOCIATED.')
      RETURN
    END IF

    CALL obj%pressure%Set(VALUE=0.0_DFP)

    IF (isfunc) THEN
      CALL obj%pressure%Set(func=func, times=times,  &
        & ivar=ivar, idof=idof, spaceCompo=spaceCompo, timeCompo=timeCompo)
    END IF

    IF (isext) THEN
      CALL obj%pressure%Copy(obj2=extField)
    END IF

  CASE ("PVEL", "PVELOCITY")
    problem = .NOT. ASSOCIATED(obj%p_velocity)
    IF (problem) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: p_velocity is not ASSOCIATED.')
      RETURN
    END IF

    CALL obj%p_velocity%Set(VALUE=0.0_DFP)

    IF (isfunc) THEN
      CALL obj%p_velocity%Set(func=func, times=times, &
        & ivar=ivar, idof=idof, spaceCompo=spaceCompo, timeCompo=timeCompo)
    END IF

    IF (isext) THEN
      CALL obj%p_velocity%Copy(obj2=extField)
    END IF

  CASE ("PACC", "PACCELERATION")
    problem = .NOT. ASSOCIATED(obj%p_acceleration)
    IF (problem) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: p_acceleration is not ASSOCIATED.')
      RETURN
    END IF

    CALL obj%p_acceleration%Set(VALUE=0.0_DFP)

    IF (isfunc) THEN
      CALL obj%p_acceleration%Set(func=func, times=times, &
        & ivar=ivar, idof=idof, spaceCompo=spaceCompo, timeCompo=timeCompo)
    END IF

    IF (isext) THEN
      CALL obj%p_acceleration%Copy(obj2=extField)
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

END SUBROUTINE applyIC2Scalar

!----------------------------------------------------------------------------
!                                                                    ApplyIC
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ApplyIC
CHARACTER(*), PARAMETER :: myName = "obj_ApplyIC()"
TYPE(CPUTime_) :: TypeCPUTime

IF (obj%showTime) CALL TypeCPUTime%SetStartTime()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

SELECT CASE (obj%problemType)
CASE (KernelProblemType%scalar)
  CALL ApplyIC2Scalar(obj=obj, name=name, func=func,  &
    & extField=extField, times=times, ivar=ivar,  &
    & idof=idof, spaceCompo=spaceCompo, timeCompo=timeCompo)

CASE (KernelProblemType%vector)

  CALL ApplyIC2Vec(obj=obj, name=name, func=func,  &
    & extField=extField, times=times, ivar=ivar,  &
    & idof=idof, spaceCompo=spaceCompo, timeCompo=timeCompo)

CASE (KernelProblemType%multiPhysics)

  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[WIP] :: not implemented yet.')
  RETURN

CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: No case found for KernelProblemType')
  RETURN
END SELECT

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

END PROCEDURE obj_ApplyIC

END SUBMODULE ApplyICMethods
