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

SUBMODULE(ScalarField_Class) SetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   set
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_set1
  CHARACTER( LEN = * ), PARAMETER :: myName="sField_set1"
  INTEGER( I4B ) :: localNode

  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Scalar field object is not initiated' )
  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) THEN
    CALL set( obj%realVec, indx=[1], value=[value]  )
  ELSE
    localNode = obj%domain%getLocalNodeNumber( globalNode )
    IF( localNode .NE. 0 ) THEN
      CALL set( obj%realVec, indx=[localNode], value=[value] )
    ELSE
      CALL e%raiseError(modName//'::'//myName// " - " &
      & // 'globalNode :: '// trim(str(globalNode, .true.)) &
      & // " is out of bound for the domain." )
    END IF
  END IF
END PROCEDURE sField_set1

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_set2
  CHARACTER( LEN = * ), PARAMETER :: myName = "sField_set2"

  IF( .NOT. obj%isInitiated ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
      & 'Scalar field object is not initiated' )
  ELSE
    CALL set( obj%realVec, value=value )
  END IF
END PROCEDURE sField_set2

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_set3
  CHARACTER( LEN = * ), PARAMETER :: myName="sField_set3"

  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Scalar field object is not initiated' )

  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This routine should not be called for constant field type.' )
  ELSE
    IF ( obj%tSize .NE. SIZE(value) ) &
      & CALL e%raiseError(modName//'::'//myName// " - "// &
      & 'Size of given value is not same as the size of scalar field vector' )
      CALL set( obj%realVec, value=value )
  END IF
END PROCEDURE sField_set3

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_set4
  REAL( DFP ) :: val( SIZE(globalNode) )
  val = value
  CALL obj%set( globalNode, val )
END PROCEDURE sField_set4

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_set5
  CHARACTER( LEN = * ), PARAMETER :: myName="sField_set5"
  INTEGER( I4B ) :: localNode( SIZE( globalNode ) )

  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Scalar field object is not initiated' )
  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
      & 'This routine should not be called for constant field type.' )
  ELSE
    localNode = obj%domain%getLocalNodeNumber( globalNode )
    IF( ANY( localNode .GT. obj%tSize ) ) &
      & CALL e%raiseError(modName//'::'//myName// " - "// &
      & 'Some of the globalNode are out of bound' )
    CALL set( obj%realVec, indx=localNode, value=value )
  END IF
END PROCEDURE sField_set5

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_set6
  CHARACTER( LEN = * ), PARAMETER :: myName="sField_set6"
  INTEGER( I4B ) :: globalNode( INT( 1+ (iend-istart)/stride ) ), ii, jj

  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Scalar field object is not initiated' )
  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
      & 'This routine should not be called for constant field type.' )
  ELSE
    jj = 0
    DO ii = istart, iend, stride
      jj = jj + 1
      globalNode( jj ) = ii
    END DO
    CALL obj%set( globalNode=globalNode, value=value )
  END IF
END PROCEDURE sField_set6

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_set7
  CHARACTER( LEN = * ), PARAMETER :: myName="sField_set7"
  INTEGER( I4B ) :: globalNode( INT( 1+ (iend-istart)/stride ) ), ii, jj

  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Scalar field object is not initiated' )

  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) THEN
    CALL e%raiseError(modName//'::'//myName// " - "// &
      & 'This routine should not be called for constant field type.' )
  ELSE
    jj = 0
    DO ii = istart, iend, stride
      jj = jj + 1
      globalNode( jj ) = ii
    END DO
    CALL obj%set( globalNode=globalNode, value=value )
  END IF
END PROCEDURE sField_set7

!----------------------------------------------------------------------------
!                                                                  set
!----------------------------------------------------------------------------

module procedure sField_set8
  obj%realVec = obj2%realVec
end procedure sField_set8

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods
