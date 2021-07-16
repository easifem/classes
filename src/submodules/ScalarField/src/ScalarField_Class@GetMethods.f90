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

SUBMODULE( ScalarField_Class ) GetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   get
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_get1
  CHARACTER( LEN = * ), PARAMETER :: myName="sField_get1"
  INTEGER( I4B ) :: localNode

  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseInformation(modName//'::'//myName// " - "// &
    & 'ScalarField object is not initiated')
  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) THEN
    value = get( obj=obj%realVec, indx=1, dataType= 1.0_DFP )
  ELSE
    localNode = obj%domain%getLocalNodeNumber( globalNode )
    IF( localNode .GT. obj%tsize ) &
      & CALL e%raiseInformation(modName//'::'//myName// " - "// &
      & 'Out of bound index')
    value = get( obj=obj%realVec, indx=localNode, dataType= 1.0_DFP )
  END IF
END PROCEDURE sField_get1

!----------------------------------------------------------------------------
!                                                                   get
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_get2
  CHARACTER( LEN = * ), PARAMETER :: myName="sField_get2"

  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseInformation(modName//'::'//myName// " - "// &
    & 'ScalarField object is not initiated')
  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) THEN
    ALLOCATE( value( obj%tsize ) )
    value = get( obj=obj%realVec, indx=1, dataType= 1.0_DFP )
  ELSE
    value = get( obj=obj%realVec, dataType= 1.0_DFP )
  END IF
END PROCEDURE sField_get2

!----------------------------------------------------------------------------
!                                                                   get
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_get3
  CHARACTER( LEN = * ), PARAMETER :: myName="sField_get3"
  INTEGER( I4B ) :: localNode( SIZE( globalNode ) )
  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseInformation(modName//'::'//myName// " - "// &
    & 'ScalarField object is not initiated')
  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) THEN
    CALL e%raiseInformation(modName//'::'//myName// " - "// &
    & 'this routine is not callable for constant field type')
  ELSE
    localNode = obj%domain%getLocalNodeNumber( globalNode )
    IF( ANY(localNode .GT. obj%tsize) ) &
      & CALL e%raiseInformation(modName//'::'//myName// " - "// &
      & 'Out of bound index')
    value = get( obj=obj%realVec, indx=localNode, dataType= 1.0_DFP )
  END IF
END PROCEDURE sField_get3

!----------------------------------------------------------------------------
!                                                                   get
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_get4
  CHARACTER( LEN = * ), PARAMETER :: myName="sField_set6"
  INTEGER( I4B ) :: globalNode( INT( 1+ (iend-istart)/stride ) ), ii, jj

  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Scalar field object is not initiated' )
  IF( obj%fieldType .EQ. FIELD_TYPE_CONSTANT ) THEN
    CALL e%raiseInformation(modName//'::'//myName// " - "// &
      & 'this routine is not callable for constant field type')
  ELSE
    jj = 0
    DO ii = istart, iend, stride
      jj = jj + 1
      globalNode( jj ) = ii
    END DO
    CALL obj%get( globalNode=globalNode, value=value )
  END IF
END PROCEDURE sField_get4

!----------------------------------------------------------------------------
!                                                                 getPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_getPointer
  ans => getPointer( obj=obj%realVec, dofobj=obj%dof, dofno=1_I4B )
END PROCEDURE sField_getPointer

END SUBMODULE GetMethods