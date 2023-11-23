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

SUBMODULE(AbstractBC_Class) SetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_set
CHARACTER(*), PARAMETER :: myName = "bc_set"
LOGICAL(LGT) :: notFunc_notExt, isTimeFunc, isSpaceFunc, isSTFunc, isFunc,  &
  & isConstVal, isSpaceVal, isSTVal, isTimeVal, bool1, bool2

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[CONFIG ERROR ] :: AbstractBC_ object is not initiated.')
END IF

notFunc_notExt = (.NOT. obj%useFunction) .AND.  &
& (.NOT. obj%useExternal)

isTimeFunc = PRESENT(timeFunction)
isSpaceFunc = PRESENT(spaceFunction)
isSTFunc = PRESENT(spaceTimeFunction)
isFunc = isSpaceFunc .OR. isTimeFunc .OR. isSTFunc
isConstVal = PRESENT(constantNodalValue)
isSpaceVal = PRESENT(spaceNodalValue)
isTimeVal = PRESENT(timeNodalValue)
isSTVal = PRESENT(spaceTimeNodalValue)

IF (notFunc_notExt .AND. isFunc) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & "[CONFIG ERROR] :: AbstractBC_::obj is initiated "//CHAR_LF// &
  & "with useFunction=.FALSE. and useExternal=.FALSE."//CHAR_LF// &
  & "So you cannot provide timeFunction, spaceFunction, spaceTimeFunction")
END IF

bool1 = notFunc_notExt .AND. isConstVal
bool2 = bool1 .AND. obj%nodalValueType .NE. Constant
IF (bool2) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & "[CONFIG ERROR] :: AbstractBC_::obj is not initiated "// &
    & "with nodalValueType=Constant "//CHAR_LF// &
    & 'So, constantNodalValue cannot be present')
  RETURN
END IF

! constant
IF (bool1) THEN
  CALL Reallocate(obj%NodalValue, 1, 1)
  obj%NodalValue = constantNodalValue
  RETURN
END IF

! spaceNodalValue
bool1 = notFunc_notExt .AND. isSpaceVal
bool2 = bool1 .AND. (obj%nodalValueType .NE. Space)
IF (bool2) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & "[CONFIG ERROR] :: AbstractBC_::obj is not initiated "// &
    & "with nodalValueType=Space"//CHAR_LF// &
    & 'So, spaceNodalValue cannot be present.')
  RETURN
END IF

IF (bool1) THEN
  CALL Reallocate(obj%NodalValue, SIZE(spaceNodalValue), 1)
  obj%NodalValue(:, 1) = spaceNodalValue
  RETURN
END IF

! timeNodalValue
bool1 = notFunc_notExt .AND. isTimeVal
bool2 = bool1 .AND. (obj%nodalValueType .NE. Time)
IF (bool2) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & "[CONFIG ERROR] :: AbstractBC_::obj is not initiated "// &
    & "with nodalValueType=Time"//CHAR_LF// &
    & 'So, timeNodalValue cannot be present.')
  RETURN
END IF

IF (bool1) THEN
  CALL Reallocate(obj%NodalValue, SIZE(timeNodalValue), 1)
  obj%NodalValue(:, 1) = timeNodalValue
  RETURN
END IF

! spaceTimeNodalValue
bool1 = notFunc_notExt .AND. isSTVal
bool2 = bool1 .AND. (obj%nodalValueType .NE. SpaceTime)
IF (bool2) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & "[CONFIG ERROR] :: AbstractBC_::obj is not initiated with "//  &
  & " nodalValueType=SpaceTime"// &
  & CHAR_LF// &
  & 'So, spaceTimeNodalValue cannot be present')
  RETURN
END IF
IF (bool1) THEN
  obj%NodalValue = spaceTimeNodalValue
  RETURN
END IF

! spaceFunction
bool1 = isSpaceFunc .AND. (obj%nodalValueType .NE. Space)
IF (bool1) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & "[CONFIG ERROR] :: AbstractBC_::obj is not initiated "// &
  & "with nodalValueType=Space"// &
  & CHAR_LF// &
  & 'So, spaceFunction cannot be present')
  RETURN
END IF

IF (isSpaceFunc) THEN
  obj%spaceFunction => spaceFunction
  RETURN
END IF

! timeFunction
bool1 = isTimeFunc .AND. (obj%nodalValueType .NE. Time)
IF (bool1) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & "[CONFIG ERROR] :: AbstractBC_::obj is not initiated with "// &
    & "nodalValueType=Time"// &
    & CHAR_LF// &
    & 'So, timeFunction cannot be present')
  RETURN
END IF

IF (isTimeFunc) THEN
  obj%timeFunction => timeFunction
  RETURN
END IF

! spaceTimeFunction
bool1 = isSTFunc .AND. (obj%nodalValueType .NE. SpaceTime)
IF (bool1) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & "[CONFIG ERROR] :: AbstractBC_::obj is not initiated with "// &
  & "nodalValueType=SpaceTime"// &
  & CHAR_LF// &
  & 'So, spaceTimeFunction cannot be present')
  RETURN
END IF

IF (isSTFunc) THEN
  obj%spaceTimeFunction => spaceTimeFunction
  RETURN
END IF

IF (obj%useFunction .AND. (.NOT. isFunc)) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & "[CONFIG ERROR] :: AbstractBC_::obj is initiated "// &
  & "with useFunction=TRUE"// &
  & CHAR_LF// &
  & 'So, spaceFunction, timeFunction, or spaceTimeFunction '// &
  & 'should be present')
  RETURN
END IF

END PROCEDURE bc_set

END SUBMODULE SetMethods
