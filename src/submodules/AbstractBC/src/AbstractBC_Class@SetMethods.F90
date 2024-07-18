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
USE GlobalData, ONLY: CHAR_LF
USE ReallocateUtility, ONLY: Reallocate

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set
CHARACTER(*), PARAMETER :: myName = "obj_Set()"
LOGICAL(LGT) :: abool

INTEGER(I4B) :: acase

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

#ifdef DEBUG_VER
CALL set_check_error(obj, constantNodalValue, spaceNodalValue, &
                     timeNodalValue, spaceTimeNodalValue, userFunction)
#endif

abool = (.NOT. obj%isUserFunction) .AND. (.NOT. obj%useExternal)

acase = 0
IF (abool) THEN
  IF (PRESENT(constantNodalValue)) THEN
    acase = 1
  ELSE IF (PRESENT(spaceNodalValue)) THEN
    acase = 2
  ELSEIF (PRESENT(timeNodalValue)) THEN
    acase = 3
  ELSEIF (PRESENT(spaceTimeNodalValue)) THEN
    acase = 4
  END IF

ELSE

  IF (obj%isUserFunction .AND. PRESENT(userFunction)) THEN
    acase = 5
  END IF

END IF

SELECT CASE (acase)
CASE (1)
  obj%nrow = 1
  obj%ncol = 1
  CALL Reallocate(obj%nodalvalue, obj%nrow, obj%ncol)
  obj%nodalvalue(1:obj%nrow, 1:obj%ncol) = constantNodalValue

CASE (2)
  obj%nrow = SIZE(spaceNodalValue, 1)
  obj%ncol = 1
  CALL Reallocate(obj%nodalvalue, obj%nrow, obj%ncol)
  obj%nodalvalue(1:obj%nrow, 1) = spaceNodalValue

CASE (3)
  obj%nrow = SIZE(timeNodalValue)
  obj%ncol = 1

  CALL Reallocate(obj%nodalvalue, obj%nrow, obj%ncol)
  obj%nodalvalue(1:obj%nrow, 1) = timeNodalValue

CASE (4)
  obj%nrow = SIZE(spaceTimeNodalValue, 1)
  obj%ncol = SIZE(spaceTimeNodalValue, 2)
  CALL Reallocate(obj%NodalValue, obj%nrow, obj%ncol)
  obj%nodalvalue(1:obj%nrow, 1:obj%ncol) = spaceTimeNodalValue

CASE (5)
  obj%func => userFunction

CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[CONFIG ERROR] :: Invalid case')
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif

END PROCEDURE obj_Set

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE set_check_error(obj, constantNodalValue, spaceNodalValue, &
                           timeNodalValue, spaceTimeNodalValue, userFunction)
  CLASS(AbstractBC_), INTENT(INOUT) :: obj
  REAL(DFP), OPTIONAL, INTENT(IN) :: constantNodalValue
  REAL(DFP), OPTIONAL, INTENT(IN) :: spaceNodalValue(:)
  REAL(DFP), OPTIONAL, INTENT(IN) :: timeNodalValue(:)
  REAL(DFP), OPTIONAL, INTENT(IN) :: spaceTimeNodalValue(:, :)
  TYPE(UserFunction_), TARGET, OPTIONAL, INTENT(IN) :: userFunction

  LOGICAL(LGT) :: isConstVal, isSpaceVal, isTimeVal, isSTVal, &
                  isUserFunction, bool1, bool2, notFunc_notExt

  CHARACTER(*), PARAMETER :: myname = "set_check_error()"

  IF (.NOT. obj%isInitiated) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
                    '[CONFIG ERROR ] :: AbstractBC_ object is not initiated.')
    RETURN
  END IF

  notFunc_notExt = (.NOT. obj%isUserFunction) .AND. (.NOT. obj%useExternal)

  isUserFunction = PRESENT(userFunction)
  isConstVal = PRESENT(constantNodalValue)
  isSpaceVal = PRESENT(spaceNodalValue)
  isTimeVal = PRESENT(timeNodalValue)
  isSTVal = PRESENT(spaceTimeNodalValue)

  IF (notFunc_notExt .AND. isUserFunction) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
               "[CONFIG ERROR] :: AbstractBC_::obj is initiated "//CHAR_LF// &
               "with useFunction=.FALSE. and useExternal=.FALSE."//CHAR_LF// &
                      "So you cannot provide userFunction.")
    RETURN
  END IF

  bool1 = notFunc_notExt .AND. isConstVal
  bool2 = bool1 .AND. (obj%nodalValueType .NE. TypeFEVariableOpt%constant)
  IF (bool2) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
                    "[CONFIG ERROR] :: AbstractBC_::obj is not initiated "// &
                      "with nodalValueType=Constant "//CHAR_LF// &
                      'So, constantNodalValue cannot be present.')
    RETURN
  END IF

! spaceNodalValue
  bool1 = notFunc_notExt .AND. isSpaceVal
  bool2 = bool1 .AND. (obj%nodalValueType .NE. TypeFEVariableOpt%space)
  IF (bool2) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
                    "[CONFIG ERROR] :: AbstractBC_::obj is not initiated "// &
                      "with nodalValueType=Space"//CHAR_LF// &
                      'So, spaceNodalValue cannot be present.')
    RETURN
  END IF

  bool1 = notFunc_notExt .AND. isTimeVal
  bool2 = bool1 .AND. (obj%nodalValueType .NE. TypeFEVariableOpt%time)
  IF (bool2) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
                    "[CONFIG ERROR] :: AbstractBC_::obj is not initiated "// &
                      "with nodalValueType=Time"//CHAR_LF// &
                      'So, timeNodalValue cannot be present.')
    RETURN
  END IF

  bool1 = notFunc_notExt .AND. isSTVal
  bool2 = bool1 .AND. (obj%nodalValueType .NE. TypeFeVariableOpt%spacetime)
  IF (bool2) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
               "[CONFIG ERROR] :: AbstractBC_::obj is not initiated with "// &
                      " nodalValueType=SpaceTime"// &
                      CHAR_LF// &
                      'So, spaceTimeNodalValue cannot be present')
    RETURN
  END IF

  bool1 = isUserFunction .AND. obj%isUserFunction
  IF (.NOT. bool1) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
           "[CONFIG ERROR] :: AbstractBC_::obj is not correctly initiated"// &
                      " for userFunction")
    RETURN
  END IF
END SUBROUTINE set_check_error

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods
