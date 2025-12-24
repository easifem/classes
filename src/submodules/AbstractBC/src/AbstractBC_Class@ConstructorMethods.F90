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
USE GlobalData, ONLY: Char_LF
USE InputUtility, ONLY: Input
USE FPL_Method, ONLY: CheckEssentialParam, Set, GetValue
USE Display_Method, ONLY: ToString

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isInit = .FALSE.
obj%isUserFunction = .FALSE.
obj%isNormal = .FALSE.
obj%isTangent = .FALSE.
obj%isUseExternal = .FALSE.
obj%isElemToFace = .FALSE.
obj%isElemToEdge = .FALSE.

obj%name = ''

obj%idof = 0
obj%nodalValueType = -1
obj%nrow = 0
obj%ncol = 0
obj%tElemToFace = 0
obj%tElemToEdge = 0

isok = ALLOCATED(obj%nodalValue)
IF (isok) DEALLOCATE (obj%nodalValue)

isok = ALLOCATED(obj%nodenum)
IF (isok) DEALLOCATE (obj%nodenum)

isok = ALLOCATED(obj%elemToFace)
IF (isok) DEALLOCATE (obj%elemToFace)

isok = ALLOCATED(obj%elemToEdge)
IF (isok) DEALLOCATE (obj%elemToEdge)

CALL obj%boundary%DEALLOCATE()

obj%dom => NULL()

isok = ASSOCIATED(obj%func)
IF (isok) THEN
  CALL obj%func%DEALLOCATE()
  DEALLOCATE (obj%func)
END IF

obj%func => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Checkessentialparam
CHARACTER(*), PARAMETER :: myName = "obj_CheckEssentialParam"
CHARACTER(:), ALLOCATABLE :: astr, prefix0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (PRESENT(prefix)) THEN
  prefix0 = prefix
ELSE
  prefix0 = obj%GetPrefix()
END IF

astr = "/name/idof/nodalValueType/isNormal/isTangent/isUseExternal/&
  &isUserFunction"

CALL CheckEssentialParam(obj=param, keys=astr, prefix=prefix0, &
                         myName=myName, modName=modName)
!note: CheckEssentialParam param is defined in easifemClasses FPL_Method

astr = ""
prefix0 = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_CheckEssentialParam

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE SetAbstractBCParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "SetAbstractBCParam()"
LOGICAL(LGT) :: isok
#endif

CHARACTER(:), ALLOCATABLE :: tempchar
INTEGER(I4B) :: tempint
LOGICAL(LGT) :: tempbool

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tempchar = Input(option=name, default=default_name)
CALL Set(param, datatype=tempchar, prefix=prefix, key="name", &
         VALUE=tempchar)

tempint = Input(option=idof, default=default_idof)
CALL Set(param, datatype=tempint, prefix=prefix, key="idof", &
         VALUE=tempint)

tempint = Input(option=nodalValueType, default=default_nodalValueType)
CALL Set(param, datatype=tempint, prefix=prefix, key="nodalValueType", &
         VALUE=tempint)

tempbool = Input(option=isNormal, default=default_isNormal)
CALL Set(param, datatype=tempbool, prefix=prefix, key="isNormal", &
         VALUE=tempbool)

tempbool = Input(option=isTangent, default=default_isTangent)
CALL Set(param, datatype=tempbool, prefix=prefix, key="isTangent", &
         VALUE=tempbool)

tempbool = Input(option=isUseExternal, default=default_useExternal)
CALL Set(param, datatype=tempbool, prefix=prefix, key="isUseExternal", &
         VALUE=tempbool)

tempbool = Input(option=isUserFunction, default=default_isUserFunction)
CALL Set(param, datatype=tempbool, prefix=prefix, key="isUserFunction", &
         VALUE=tempbool)

#ifdef DEBUG_VER
isok = PRESENT(isNormal) .AND. PRESENT(idof)
IF (isok) THEN
  tempbool = (idof .GT. 0) .AND. isNormal
  IF (tempbool) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[CONFIG ERROR] :: When isNormal is true, '// &
                      'idof CANNOT be greater than 0.')
  END IF
END IF
#endif

#ifdef DEBUG_VER
isok = PRESENT(isTangent) .AND. PRESENT(idof)
IF (isok) THEN
  tempbool = (idof .GT. 0) .AND. isTangent
  IF (tempbool) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
                      '[CONFIG ERROR] :: When isTangent is true, '// &
                      'idof cannot be greater than 0.')
  END IF
END IF
#endif

tempchar = ''

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE SetAbstractBCParam

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate1
CHARACTER(*), PARAMETER :: myName = "obj_Initiate1()"
LOGICAL(LGT) :: isSelectionByMeshID, abool
CHARACTER(:), ALLOCATABLE :: prefix

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

prefix = obj%GetPrefix()
CALL obj%DEALLOCATE()

CALL obj%CheckEssentialParam(param=param, prefix=prefix)

obj%isInit = .TRUE.
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

! isUseExternal
CALL GetValue(obj=param, prefix=prefix, key="isUseExternal", &
              VALUE=obj%isUseExternal)

! Check
CALL boundary%GetParam(isSelectionByMeshID=isSelectionByMeshID)
abool = isSelectionByMeshID &
        .AND. (.NOT. obj%isUserFunction) &
        .AND. (.NOT. obj%isUseExternal) &
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
END PROCEDURE obj_Initiate1

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate2()"
LOGICAL(LGT) :: isok
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()

obj%isInit = .TRUE.
obj%boundary = boundary
obj%dom => dom

obj%name = Input(option=name, default=default_name)
obj%idof = Input(option=idof, default=default_idof)
obj%nodalValueType = Input(option=nodalValueType, &
                           default=default_nodalValueType)

obj%isNormal = Input(option=isNormal, default=default_isNormal)
obj%isTangent = Input(option=isTangent, default=default_isTangent)
obj%isUseExternal = Input(option=isUseExternal, default=default_useExternal)
obj%isUserFunction = Input(option=isUserFunction, &
                           default=default_isUserFunction)

#ifdef DEBUG_VER
isok = obj%isNormal .AND. (obj%idof .EQ. 0)
CALL AssertError1(isok, myName, &
                  'When isNormal is true, idof CANNOT be greater than 0.')
#endif

#ifdef DEBUG_VER
isok = obj%isTangent .AND. (obj%idof .EQ. 0)
CALL AssertError1(isok, myName, &
                  'When isTangent is true, idof CANNOT be greater than 0.')
#endif

#ifdef DEBUG_VER
CALL obj%boundary%GetParam(isSelectionByMeshID=isok)
isok = isok .AND. (.NOT. obj%isUserFunction)
isok = isok .AND. (.NOT. obj%isUseExternal)
isok = isok .AND. (obj%nodalValueType .NE. TypeFEVariableOpt%constant)

IF (isok) THEN
  CALL e%RaiseWarning(modName//'::'//myName//" - "// &
                      "When meshSelection is by MeshID"//CHAR_LF// &
                      " and `isUserFunction` is false, then"//CHAR_LF// &
                      " `nodalValueType` in `AbstractBC_`"//CHAR_LF// &
                      " object should be Constant.")
END IF
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
