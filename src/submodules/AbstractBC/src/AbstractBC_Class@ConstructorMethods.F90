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
USE FPL_Method
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_Deallocate
obj%isInitiated = .FALSE.
obj%name = ''
obj%idof = 0
obj%nodalValueType = -1
obj%isUserFunction = .FALSE.
obj%isNormal = .FALSE.
obj%isTangent = .FALSE.
obj%useExternal = .FALSE.
IF (ALLOCATED(obj%nodalValue)) DEALLOCATE (obj%nodalValue)

CALL obj%boundary%DEALLOCATE()
IF (ASSOCIATED(obj%dom)) obj%dom => NULL()
IF (ASSOCIATED(obj%func)) THEN
  CALL obj%func%DEALLOCATE()
END IF
obj%func => NULL()

END PROCEDURE bc_Deallocate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_Checkessentialparam
CHARACTER(*), PARAMETER :: myName = "bc_CheckEssentialParam"
INTEGER(I4B) :: ii
TYPE(String), ALLOCATABLE :: essentialParam(:)
TYPE(String) :: astr, prefix0

IF (PRESENT(prefix)) THEN
  prefix0 = prefix
ELSE
  prefix0 = obj%GetPrefix()
END IF

astr = "/name/idof/nodalValueType/isNormal/isTangent/"//  &
& "useExternal/isUserFunction"

CALL astr%Split(essentialParam, sep="/")
CALL CheckEssentialParam(obj=param,  &
  & keys=essentialParam,  &
  & prefix=prefix0%chars(),  &
  & myName=myName,  &
  & modName=modName)
!NOTE: CheckEssentialParam param is defined in easifemClasses FPL_Method

IF (ALLOCATED(essentialParam)) THEN
  DO ii = 1, SIZE(essentialParam)
    essentialParam(ii) = ""
  END DO
  DEALLOCATE (essentialParam)
END IF
astr = ""
prefix0 = ""
END PROCEDURE bc_CheckEssentialParam

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE SetAbstractBCParam
CHARACTER(*), PARAMETER :: myName = "SetAbstractBCParam()"

CALL Set(param, datatype="char", prefix=prefix, key="name",  &
  & VALUE=input(option=name, default=default_name))

CALL Set(param, datatype=0_I4B, prefix=prefix, key="idof",  &
  & VALUE=input(option=idof, default=default_idof))

CALL Set(param, datatype=0_I4B, prefix=prefix, key="nodalValueType",  &
  & VALUE=input(option=nodalValueType, default=default_nodalValueType))

CALL Set(param, datatype=.TRUE., prefix=prefix, key="isNormal",  &
  & VALUE=input(option=isNormal, default=default_isNormal))

CALL Set(param, datatype=.TRUE., prefix=prefix, key="isTangent",  &
  & VALUE=input(option=isTangent, default=default_isTangent))

CALL Set(param, datatype=.TRUE., prefix=prefix, key="useExternal",  &
  & VALUE=input(option=useExternal, default=default_useExternal))

IF (PRESENT(isUserFunction)) THEN
  CALL Set(param, datatype=.TRUE., prefix=prefix, key="isUserFunction",  &
    & VALUE=isUserFunction)
ELSE
  CALL Set(param, datatype=.TRUE., prefix=prefix, key="isUserFunction",  &
    & VALUE=default_isUserFunction)
END IF

IF (PRESENT(isNormal) .AND. PRESENT(idof)) THEN
  IF (idof .GT. 0 .AND. isNormal) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
    & '[CONFIG ERROR] :: When isNormal is true, '//  &
    & 'idof CANNOT be greater than 0.')
  END IF
END IF

IF (PRESENT(isTangent) .AND. PRESENT(idof)) THEN
  IF (idof .GT. 0 .AND. isTangent) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
    & '[CONFIG ERROR] :: When isTangent is true, '//  &
    & 'idof cannot be greater than 0.')
  END IF
END IF

END PROCEDURE SetAbstractBCParam

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_Initiate
CHARACTER(*), PARAMETER :: myName = "bc_Initiate()"
LOGICAL(LGT) :: isSelectionByMeshID, abool
TYPE(String) :: prefix

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] Initiate()')
#endif

prefix = obj%GetPrefix()
CALL obj%DEALLOCATE()

CALL obj%CheckEssentialParam(param=param, prefix=prefix%chars())

obj%isInitiated = .TRUE.
obj%boundary = boundary
obj%dom => dom

! name
CALL GetValue(obj=param, prefix=prefix%chars(), key="name", VALUE=obj%name)

! idof
CALL GetValue(obj=param, prefix=prefix%chars(), key="idof", VALUE=obj%idof)

! nodalValueType
CALL GetValue(obj=param, prefix=prefix%chars(), key="nodalValueType", &
  & VALUE=obj%nodalValueType)

! isUserFunction
CALL GetValue(obj=param, prefix=prefix%chars(), key="isUserFunction", &
  & VALUE=obj%isUserFunction)

! isNormal
CALL GetValue(obj=param, prefix=prefix%chars(), key="isNormal", &
  & VALUE=obj%isNormal)

! isTangent
CALL GetValue(obj=param, prefix=prefix%chars(), key="isTangent", &
  & VALUE=obj%isTangent)

! useExternal
CALL GetValue(obj=param, prefix=prefix%chars(), key="useExternal", &
  & VALUE=obj%useExternal)

! Check
CALL boundary%GetParam(isSelectionByMeshID=isSelectionByMeshID)
abool = boundary%isSelectionByMeshID  &
  & .AND. (.NOT. obj%isUserFunction) &
  & .AND. (.NOT. obj%useExternal) &
  & .AND. (obj%nodalValueType .NE. Constant)

IF (abool) THEN
  CALL e%RaiseWarning(modName//'::'//myName//" - "// &
      & "When meshSelection is by MeshID"//CHAR_LF//  &
      & " and `isUserFunction` is false, then"//CHAR_LF//  &
      & " `nodalValueType` in `AbstractBC_`"//CHAR_LF//  &
      & " object should be Constant.")
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] AbstractBCInitiate()')
#endif
END PROCEDURE bc_Initiate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods
