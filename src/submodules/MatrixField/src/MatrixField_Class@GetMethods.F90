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

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This module contains constructor method for [[MatrixField_]]

SUBMODULE( MatrixField_Class ) GetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                       SIZE
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_Size
  ans = SIZE( obj%mat, dim )
END PROCEDURE mField_Size

!----------------------------------------------------------------------------
!                                                                      SHAPE
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_Shape
  ans = SHAPE( obj%mat )
END PROCEDURE mField_Shape

!----------------------------------------------------------------------------
!                                                                     getRow
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_getRow
  INTEGER( I4B ) :: inode
  REAL( DFP ), POINTER :: realVec( : )
  CHARACTER( LEN = * ), PARAMETER :: myName="mField_getRow"
  !
  inode = obj%domain%getLocalNodeNumber( globalNode )
  IF( PRESENT( val ) ) THEN
    CALL getRow( obj=obj%mat, inode=inode, idof=idof, val=val, scale=scale,&
      & addContribution=addContribution )
  ELSE IF( PRESENT( nodeFieldVal ) ) THEN
    IF( obj%mat%csr%dof .NE. nodeFieldVal%dof ) &
      & CALL e%raiseError(modName//'::'//myName// " - "// &
      & 'DOF data of matrix is not same as the DOF data of nodefieldVal')
      realVec => NULL()
      realVec => nodeFieldVal%getPointer( )
      CALL getRow( obj=obj%mat, inode=inode, idof=idof, val=realVec, &
        & scale=scale, addContribution=addContribution )
  END IF
  NULLIFY( realVec )
END PROCEDURE mField_getRow

!----------------------------------------------------------------------------
!                                                                 getColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_getColumn
  INTEGER( I4B ) :: inode
  REAL( DFP ), POINTER :: realVec( : )
  CHARACTER( LEN = * ), PARAMETER :: myName="mField_getColumn"
  !
  inode = obj%domain%getLocalNodeNumber( globalNode )
  IF( PRESENT( val ) ) THEN
    CALL getColumn( obj=obj%mat, inode=inode, idof=idof, val=val, &
    & scale=scale, addContribution=addContribution )
  ELSE IF( PRESENT( nodeFieldVal ) ) THEN
    IF( obj%mat%csr%dof .NE. nodeFieldVal%dof ) &
      & CALL e%raiseError(modName//'::'//myName// " - "// &
      & 'DOF data of matrix is not same as the DOF data of nodefieldVal')
      realVec => NULL()
      realVec => nodeFieldVal%getPointer( )
      CALL getColumn( obj=obj%mat, inode=inode, idof=idof, val=realVec, &
        & scale=scale, addContribution=addContribution )
  END IF
  NULLIFY( realVec )
END PROCEDURE mField_getColumn

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetMethods