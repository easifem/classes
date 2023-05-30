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

SUBMODULE(AbstractBC_Class) ConstructorMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_CheckEssentialParam
CHARACTER(*), PARAMETER :: myName = "bc_CheckEssentialParam"
CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'This routine should be implemented by children of AbstractBC_')
END PROCEDURE bc_CheckEssentialParam

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_Initiate
CHARACTER(*), PARAMETER :: myName = "bc_Initiate"
CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'This routine should be implemented by children of AbstractBC_')
END PROCEDURE bc_Initiate

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_Deallocate
obj%isInitiated = .FALSE.
obj%name = ''
obj%idof = 0
obj%nodalValueType = -1
obj%useFunction = .FALSE.
obj%isNormal = .FALSE.
obj%isTangent = .FALSE.
obj%useExternal = .FALSE.
IF (ALLOCATED(obj%nodalValue)) DEALLOCATE (obj%nodalValue)
IF (ASSOCIATED(obj%SpaceTimeFunction)) obj%SpaceTimeFunction => NULL()
IF (ASSOCIATED(obj%SpaceFunction)) obj%SpaceFunction => NULL()
IF (ASSOCIATED(obj%TimeFunction)) obj%TimeFunction => NULL()
CALL obj%boundary%DEALLOCATE()
IF (ASSOCIATED(obj%dom)) obj%dom => NULL()
END PROCEDURE bc_Deallocate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE AbstractBCcheckEssentialParam
CHARACTER(*), PARAMETER :: myName = "AbstractBCcheckEssentialParam"
INTEGER(I4B) :: ii
INTEGER(I4B), PARAMETER :: maxEssentialParam = 7
TYPE(String) :: essentialParam(maxEssentialParam)

essentialParam(1) = prefix//"/name"
essentialParam(2) = prefix//"/idof"
essentialParam(3) = prefix//"/nodalValueType"
essentialParam(4) = prefix//"/useFunction"
essentialParam(5) = prefix//"/isNormal"
essentialParam(6) = prefix//"/isTangent"
essentialParam(7) = prefix//"/useExternal"

DO ii = 1, maxEssentialParam
  IF (.NOT. param%isPresent(key=TRIM(essentialParam(ii)%chars()))) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & TRIM(essentialParam(ii)%chars())//' should be present in param')
  END IF
END DO

END PROCEDURE AbstractBCcheckEssentialParam

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE setAbstractBCParam
INTEGER(I4B) :: ierr
CHARACTER(*), PARAMETER :: myName = "setAbstractBCParam"

IF (PRESENT(name)) THEN
  ierr = param%set(key=TRIM(prefix)//"/name", VALUE=TRIM(name))
ELSE
  ierr = param%set(key=TRIM(prefix)//"/name", VALUE="AbstractBC")
END IF

IF (PRESENT(idof)) THEN
  ierr = param%set(key=TRIM(prefix)//"/idof", VALUE=idof)
ELSE
  ierr = param%set(key=TRIM(prefix)//"/idof", VALUE=0_I4B)
END IF

IF (PRESENT(nodalValueType)) THEN
  ierr = param%set(key=TRIM(prefix)//"/nodalValueType", VALUE=nodalValueType)
ELSE
  ierr = param%set(key=TRIM(prefix)//"/nodalValueType", VALUE=-1_I4B)
END IF

IF (PRESENT(useFunction)) THEN
  ierr = param%set(key=TRIM(prefix)//"/useFunction", VALUE=useFunction)
ELSE
  ierr = param%set(key=TRIM(prefix)//"/useFunction", VALUE=.FALSE.)
END IF

IF (PRESENT(isNormal)) THEN
  ierr = param%set(key=TRIM(prefix)//"/isNormal", VALUE=isNormal)
  IF (PRESENT(idof)) THEN
    IF (idof .GT. 0 .AND. isNormal) THEN
      CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'When isNormal is true, idof cannot be greater than 0')
    END IF
  END IF
ELSE
  ierr = param%set(key=TRIM(prefix)//"/isNormal", VALUE=.FALSE.)
END IF

IF (PRESENT(isTangent)) THEN
  ierr = param%set(key=TRIM(prefix)//"/isTangent", VALUE=isTangent)
  IF (PRESENT(idof)) THEN
    IF (idof .GT. 0 .AND. isTangent) THEN
      CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'When isTangent is true, idof cannot be greater than 0')
    END IF
  END IF
ELSE
  ierr = param%set(key=TRIM(prefix)//"/isTangent", VALUE=.FALSE.)
END IF

IF (PRESENT(useExternal)) THEN
  ierr = param%set(key=TRIM(prefix)//"/useExternal", VALUE=useExternal)
ELSE
  ierr = param%set(key=TRIM(prefix)//"/useExternal", VALUE=.FALSE.)
END IF

END PROCEDURE setAbstractBCParam

!----------------------------------------------------------------------------
!                                                     AbstractBCInitiate
!----------------------------------------------------------------------------

MODULE PROCEDURE AbstractBCInitiate
CHARACTER(*), PARAMETER :: myName = "AbstractBCInitiate"
CHARACTER(:), ALLOCATABLE :: char_var
INTEGER(I4B) :: ierr

IF (obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'AbstractBC_ object is already initiated')
END IF

CALL obj%checkEssentialParam(param=param)

obj%isInitiated = .TRUE.
obj%boundary = boundary
obj%dom => dom
!
! name
!
ALLOCATE (CHARACTER(param%DataSizeInBytes( &
  & key=TRIM(prefix)//"/name")) :: char_var)
ierr = param%get(key=TRIM(prefix)//"/name", VALUE=char_var)
obj%name = char_var
DEALLOCATE (char_var)
!
! idof
!
ierr = param%get(key=TRIM(prefix)//"/idof", VALUE=obj%idof)
!
! nodalValueType
!
ierr = param%get(key=TRIM(prefix)//"/nodalValueType", &
  & VALUE=obj%nodalValueType)
!
! useFunction
!
ierr = param%get(key=TRIM(prefix)//"/useFunction", &
  & VALUE=obj%useFunction)
!
! isNormal
!
ierr = param%get(key=TRIM(prefix)//"/isNormal", &
  & VALUE=obj%isNormal)
!
! isTangent
!
ierr = param%get(key=TRIM(prefix)//"/isTangent", &
  & VALUE=obj%isTangent)
!
! useExternal
!
ierr = param%get(key=TRIM(prefix)//"/useExternal", &
  & VALUE=obj%useExternal)
!
! check
!
IF (boundary%isSelectionByMeshID &
    & .AND. (.NOT. obj%useFunction) &
    & .AND. (.NOT. obj%useExternal)) THEN
  IF (obj%nodalValueType .NE. Constant) THEN
    CALL e%raiseWarning(modName//'::'//myName//" - "// &
        & "When meshSelection is by MeshID &
        & and `useFunction` is false, then &
        & `nodalValueType` in `AbstractBC_` &
        & object should be Constant.")
  END IF
END IF

END PROCEDURE AbstractBCInitiate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods
