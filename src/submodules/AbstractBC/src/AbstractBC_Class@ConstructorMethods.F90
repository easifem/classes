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
USE InputUtility, ONLY: Input

USE FPL_Method, ONLY: CheckEssentialParam, Set, GetValue

USE GlobalData, ONLY: Char_LF

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
obj%isInitiated = .FALSE.
obj%name = ''
obj%idof = 0
obj%nodalValueType = -1
obj%isUserFunction = .FALSE.
obj%isNormal = .FALSE.
obj%isTangent = .FALSE.
obj%useExternal = .FALSE.
obj%nrow = 0
obj%ncol = 0

IF (ALLOCATED(obj%nodalValue)) DEALLOCATE (obj%nodalValue)

CALL obj%boundary%DEALLOCATE()

obj%dom => NULL()

IF (ASSOCIATED(obj%func)) THEN
  CALL obj%func%DEALLOCATE()
  DEALLOCATE (obj%func)
END IF

obj%func => NULL()

END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Checkessentialparam
CHARACTER(*), PARAMETER :: myName = "obj_CheckEssentialParam"
CHARACTER(:), ALLOCATABLE :: astr, prefix0

IF (PRESENT(prefix)) THEN
  prefix0 = prefix
ELSE
  prefix0 = obj%GetPrefix()
END IF

astr = "/name/idof/nodalValueType/isNormal/isTangent/"// &
       "useExternal/isUserFunction"

CALL CheckEssentialParam(obj=param, keys=astr, prefix=prefix0, &
                         myName=myName, modName=modName)
!NOTE: CheckEssentialParam param is defined in easifemClasses FPL_Method

astr = ""
prefix0 = ""
END PROCEDURE obj_CheckEssentialParam

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE SetAbstractBCParam
CHARACTER(*), PARAMETER :: myName = "SetAbstractBCParam()"

CALL Set(param, datatype="char", prefix=prefix, key="name", &
         VALUE=Input(option=name, default=default_name))

CALL Set(param, datatype=0_I4B, prefix=prefix, key="idof", &
         VALUE=Input(option=idof, default=default_idof))

CALL Set(param, datatype=0_I4B, prefix=prefix, key="nodalValueType", &
         VALUE=Input(option=nodalValueType, default=default_nodalValueType))

CALL Set(param, datatype=.TRUE., prefix=prefix, key="isNormal", &
         VALUE=Input(option=isNormal, default=default_isNormal))

CALL Set(param, datatype=.TRUE., prefix=prefix, key="isTangent", &
         VALUE=Input(option=isTangent, default=default_isTangent))

CALL Set(param, datatype=.TRUE., prefix=prefix, key="useExternal", &
         VALUE=Input(option=useExternal, default=default_useExternal))

IF (PRESENT(isUserFunction)) THEN
  CALL Set(param, datatype=.TRUE., prefix=prefix, key="isUserFunction", &
           VALUE=isUserFunction)
ELSE
  CALL Set(param, datatype=.TRUE., prefix=prefix, key="isUserFunction", &
           VALUE=default_isUserFunction)
END IF

IF (PRESENT(isNormal) .AND. PRESENT(idof)) THEN
  IF (idof .GT. 0 .AND. isNormal) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
                      '[CONFIG ERROR] :: When isNormal is true, '// &
                      'idof CANNOT be greater than 0.')
  END IF
END IF

IF (PRESENT(isTangent) .AND. PRESENT(idof)) THEN
  IF (idof .GT. 0 .AND. isTangent) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
                      '[CONFIG ERROR] :: When isTangent is true, '// &
                      'idof cannot be greater than 0.')
  END IF
END IF

END PROCEDURE SetAbstractBCParam

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
CHARACTER(*), PARAMETER :: myName = "obj_Initiate()"
LOGICAL(LGT) :: isSelectionByMeshID, abool
CHARACTER(:), ALLOCATABLE :: prefix

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

prefix = obj%GetPrefix()
CALL obj%DEALLOCATE()

CALL obj%CheckEssentialParam(param=param, prefix=prefix)

obj%isInitiated = .TRUE.
obj%boundary = boundary
obj%dom => dom

! name
CALL GetValue(obj=param, prefix=prefix, key="name", VALUE=obj%name)

! idof
CALL GetValue(obj=param, prefix=prefix, key="idof", VALUE=obj%idof)

! nodalValueType
CALL GetValue(obj=param, prefix=prefix, key="nodalValueType", &
              VALUE=obj%nodalValueType)

! isUserFunction
CALL GetValue(obj=param, prefix=prefix, key="isUserFunction", &
              VALUE=obj%isUserFunction)

! isNormal
CALL GetValue(obj=param, prefix=prefix, key="isNormal", &
              VALUE=obj%isNormal)

! isTangent
CALL GetValue(obj=param, prefix=prefix, key="isTangent", &
              VALUE=obj%isTangent)

! useExternal
CALL GetValue(obj=param, prefix=prefix, key="useExternal", &
              VALUE=obj%useExternal)

! Check
CALL boundary%GetParam(isSelectionByMeshID=isSelectionByMeshID)
abool = boundary%isSelectionByMeshID &
        .AND. (.NOT. obj%isUserFunction) &
        .AND. (.NOT. obj%useExternal) &
        .AND. (obj%nodalValueType .NE. TypeFEVariableOpt%constant)

IF (abool) THEN
  CALL e%RaiseWarning(modName//'::'//myName//" - "// &
                      "When meshSelection is by MeshID"//CHAR_LF// &
                      " and `isUserFunction` is false, then"//CHAR_LF// &
                      " `nodalValueType` in `AbstractBC_`"//CHAR_LF// &
                      " object should be Constant.")
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods
