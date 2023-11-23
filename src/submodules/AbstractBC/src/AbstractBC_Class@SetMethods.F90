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

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'AbstractBC_ object is not initiated, initiate it first.')
END IF

IF (.NOT. obj%UseFunction .AND. .NOT. obj%useExternal) THEN

  IF (PRESENT(TimeFunction) .OR. PRESENT(SpaceFunction) &
    & .OR. PRESENT(SpaceTimeFunction)) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
    & "AbstractBC_::obj is initiated with useFunction=.FALSE."//CHAR_LF// &
    & " and useExternal=.FALSE."//CHAR_LF// &
    & "So you cannot provide TimeFunction, SpaceFunction, SpaceTimeFunction")
  END IF

  ! constant

  IF (PRESENT(ConstantNodalValue)) THEN

    IF (obj%nodalValueType .NE. Constant) THEN

      CALL e%raiseError(modName//'::'//myName//" - "// &
       & "AbstractBC_::obj is not initiated with nodalValueType=Constant"// &
       & CHAR_LF// &
       & 'So, ConstantNodalValue cannot be present')

    ELSE

      CALL Reallocate(obj%NodalValue, 1, 1)
      obj%NodalValue = ConstantNodalValue
      RETURN

    END IF

  END IF

  ! SpaceNodalValue

  IF (PRESENT(SpaceNodalValue)) THEN

    IF (obj%nodalValueType .NE. Space) THEN

      CALL e%raiseError(modName//'::'//myName//" - "// &
        & "AbstractBC_::obj is not initiated with nodalValueType=Space"// &
        & CHAR_LF// &
        & 'So, SpaceNodalValue cannot be present')

    ELSE

      CALL Reallocate(obj%NodalValue, SIZE(SpaceNodalValue), 1)
      obj%NodalValue(:, 1) = SpaceNodalValue
      RETURN

    END IF

  END IF

  ! TimeNodalValue

  IF (PRESENT(TimeNodalValue)) THEN

    IF (obj%nodalValueType .NE. Time) THEN

      CALL e%raiseError(modName//'::'//myName//" - "// &
        & "AbstractBC_::obj is not initiated with nodalValueType=Time"// &
        & CHAR_LF// &
        & 'So, TimeNodalValue cannot be present')

    ELSE

      CALL Reallocate(obj%NodalValue, SIZE(TimeNodalValue), 1)
      obj%NodalValue(:, 1) = TimeNodalValue
      RETURN

    END IF

  END IF

  ! SpaceTimeNodalValue

  IF (PRESENT(SpaceTimeNodalValue)) THEN

    IF (obj%nodalValueType .NE. SpaceTime) THEN

      CALL e%raiseError(modName//'::'//myName//" - "// &
      & "AbstractBC_::obj is not initiated with nodalValueType=SpaceTime"// &
      & CHAR_LF// &
      & 'So, SpaceTimeNodalValue cannot be present')

    ELSE

      obj%NodalValue = SpaceTimeNodalValue
      RETURN

    END IF

  END IF

ELSE

  ! SpaceFunction

  IF (PRESENT(SpaceFunction)) THEN

    IF (obj%nodalValueType .NE. Space) THEN

      CALL e%raiseError(modName//'::'//myName//" - "// &
      & "AbstractBC_::obj is not initiated with nodalValueType=Space"// &
      & CHAR_LF// &
      & 'So, SpaceFunction cannot be present')

    ELSE

      obj%SpaceFunction => SpaceFunction
      RETURN

    END IF

  END IF

  ! TimeNodalValue

  IF (PRESENT(TimeFunction)) THEN

    IF (obj%nodalValueType .NE. Time) THEN

      CALL e%raiseError(modName//'::'//myName//" - "// &
      & "AbstractBC_::obj is not initiated with nodalValueType=Time"// &
      & CHAR_LF// &
      & 'So, TimeFunction cannot be present')

    ELSE

      obj%TimeFunction => TimeFunction
      RETURN

    END IF

  END IF

  ! SpaceTimeNodalValue

  IF (PRESENT(SpaceTimeFunction)) THEN

    IF (obj%nodalValueType .NE. SpaceTime) THEN

      CALL e%raiseError(modName//'::'//myName//" - "// &
      & "AbstractBC_::obj is not initiated with nodalValueType=SpaceTime"// &
      & CHAR_LF// &
      & 'So, SpaceTimeFunction cannot be present')

    ELSE

      obj%SpaceTimeFunction => SpaceTimeFunction
      RETURN

    END IF

  END IF

  CALL e%raiseError(modName//'::'//myName//" - "// &
    & "AbstractBC_::obj is initiated with useFunction=TRUE"// &
    & CHAR_LF// &
  & 'So, SpaceFunction, TimeFunction, or SpaceTimeFunction should be present')

END IF

END PROCEDURE bc_set

END SUBMODULE SetMethods
