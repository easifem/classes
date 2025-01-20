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

MODULE PROCEDURE bc_Set
CHARACTER(*), PARAMETER :: myName = "bc_Set"
LOGICAL(LGT) :: notFunc_notExt, isConstVal, isSpaceVal, isSTVal,  &
  & isTimeVal, bool1, bool2, isUserFunction

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] Set()')
#endif

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[CONFIG ERROR ] :: AbstractBC_ object is not initiated.')
END IF

notFunc_notExt = (.NOT. obj%isUserFunction) .AND. (.NOT. obj%useExternal)

isUserFunction = PRESENT(userFunction)
isConstVal = PRESENT(constantNodalValue)
isSpaceVal = PRESENT(spaceNodalValue)
isTimeVal = PRESENT(timeNodalValue)
isSTVal = PRESENT(spaceTimeNodalValue)

IF (notFunc_notExt .AND. isUserFunction) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & "[CONFIG ERROR] :: AbstractBC_::obj is initiated "//CHAR_LF// &
  & "with useFunction=.FALSE. and useExternal=.FALSE."//CHAR_LF// &
  & "So you cannot provide userFunction.")
END IF

bool1 = notFunc_notExt .AND. isConstVal
bool2 = bool1 .AND. obj%nodalValueType .NE. Constant
IF (bool2) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & "[CONFIG ERROR] :: AbstractBC_::obj is not initiated "// &
    & "with nodalValueType=Constant "//CHAR_LF// &
    & 'So, constantNodalValue cannot be present.')
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

! userFunction
bool1 = isUserFunction .AND. obj%isUserFunction
IF (.NOT. bool1) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & "[CONFIG ERROR] :: AbstractBC_::obj is not correctly initiated"// &
    & " for userFunction")
  RETURN
END IF

IF (isUserFunction) THEN
  obj%func => userFunction
  RETURN
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] Set()')
#endif

END PROCEDURE bc_Set

END SUBMODULE SetMethods
