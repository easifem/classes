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

SUBMODULE (DirichletBC_Class) SetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 bc_set
!----------------------------------------------------------------------------

MODULE PROCEDURE bc_set
  CHARACTER( LEN = * ), PARAMETER :: myName="bc_set"
  !> check
  IF( .NOT. obj%isInitiated ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'DiricheltBC object is not initiated, initiate it first.' )
  END IF

  IF( .NOT. obj%UseFunction ) THEN
    IF( PRESENT( TimeFunction ) .OR. PRESENT( SpaceFunction ) &
      & .OR. PRESENT( SpaceTimeFunction ) ) THEN
      CALL e%raiseError(modName//'::'//myName// " - "// &
        & 'You can either set values using a function, or by provinding &
        & nodalValue, but not the both')
    END IF
    !> constant
    IF( PRESENT( ConstantNodalValue ) ) THEN
      !> check
      IF( obj%nodalValueType .NE. Constant ) &
        & CALL e%raiseError(modName//'::'//myName// " - "// &
        & 'nodalValueType is not Constant')
      CALL Reallocate( obj%NodalValue, 1, 1)
      obj%NodalValue = ConstantNodalValue
      RETURN
    END IF
    !> SpaceNodalValue
    IF( PRESENT( SpaceNodalValue ) ) THEN
      !> check
      IF( obj%nodalValueType .NE. Space ) &
        & CALL e%raiseError(modName//'::'//myName// " - "// &
        & 'nodalValueType is not Space type')
      CALL Reallocate( obj%NodalValue, SIZE( SpaceNodalValue ), 1 )
      obj%NodalValue( :, 1 ) = SpaceNodalValue
      RETURN
    END IF
    !> TimeNodalValue
    IF( PRESENT( TimeNodalValue ) ) THEN
      !> check
      IF( obj%nodalValueType .NE. Time ) &
        & CALL e%raiseError(modName//'::'//myName// " - "// &
        & 'nodalValueType is not Time type')
      CALL Reallocate( obj%NodalValue, SIZE( TimeNodalValue ), 1 )
      obj%NodalValue( :, 1 ) = TimeNodalValue
      RETURN
    END IF
    !> SpaceTimeNodalValue
    IF( PRESENT( SpaceTimeNodalValue ) ) THEN
      !> check
      IF( obj%nodalValueType .NE. SpaceTime ) &
        & CALL e%raiseError(modName//'::'//myName// " - "// &
        & 'nodalValueType is not SpaceTime type')
      obj%NodalValue = SpaceTimeNodalValue
      RETURN
    END IF
    !>
  ELSE
    !> SpaceFunction
    IF( PRESENT( SpaceFunction ) ) THEN
      IF( obj%nodalValueType .NE. Space ) &
      & CALL e%raiseError(modName//'::'//myName// " - "// &
      & 'nodalValueType is not Space type')
      obj%SpaceFunction => SpaceFunction
      RETURN
    END IF
    !> TimeNodalValue
    IF( PRESENT( TimeFunction ) ) THEN
      IF( obj%nodalValueType .NE. Time ) &
      & CALL e%raiseError(modName//'::'//myName// " - "// &
      & 'nodalValueType is not Time type')
      obj%TimeFunction => TimeFunction
      RETURN
    END IF
    !> SpaceTimeNodalValue
    IF( PRESENT( SpaceTimeFunction ) ) THEN
      IF( obj%nodalValueType .NE. SpaceTime ) &
      & CALL e%raiseError(modName//'::'//myName// " - "// &
      & 'nodalValueType is not SpaceTime type')
      obj%SpaceTimeFunction => SpaceTimeFunction
      RETURN
    END IF
    !> check
    CALL e%raiseError(modName//'::'//myName// " - "// &
      & 'When UseFunction is true then user must provide function')
  END IF
END PROCEDURE bc_set

END SUBMODULE SetMethods