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

SUBMODULE(MatrixField_Class) SetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_set1
  CHARACTER( LEN = * ), PARAMETER :: myName="mField_set1"
  INTEGER( I4B ) :: localNode( SIZE( globalNode ) ), nn
  REAL( DFP ) ::  alpha
  !> main
  nn = (.tdof. obj%mat%csr%dof)*SIZE(globalNode)
  !> check
  IF( SIZE( val, 1 ) .NE. SIZE( val, 2 )  &
    & .OR. SIZE( val,1 ) .NE. nn ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'val is not square matrix, or its shape is inconsistent &
    & with the degree of freedom stored in MatrixField')
  localNode = obj%domain%getLocalNodeNumber(globalNode)
  IF( PRESENT( addContribution ) ) THEN
    alpha = INPUT( default=1.0_DFP, option=scale )
    CALL add( obj=obj%mat, nptrs=localNode, val=val, storageFMT=storageFMT, &
    & scale=alpha )
  ELSE
    CALL set( obj=obj%mat, nptrs=localNode, val=val, storageFMT=storageFMT )
  END IF
END PROCEDURE mField_set1

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_set2
  INTEGER( I4B ), ALLOCATABLE :: localNode( : )
  IF( PRESENT( addContribution ) ) THEN
    IF( PRESENT( globalNode ) ) THEN
      localNode = obj%domain%getLocalNodeNumber( globalNode )
      CALL add( obj=obj%mat, nptrs=localNode, &
        & scale=INPUT( default=1.0_DFP, option=scale ), val=val )
      DEALLOCATE( localNode )
    ELSE
      CALL add( obj=obj%mat, val=val,  &
        & scale=INPUT( default=1.0_DFP, option=scale ) )
    END IF
  ELSE
    IF( PRESENT( globalNode ) ) THEN
      localNode = obj%domain%getLocalNodeNumber(globalNode)
      CALL set( obj=obj%mat, nptrs=localNode, val=val )
      DEALLOCATE( localNode )
    ELSE
      CALL set( obj=obj%mat, val=val )
    END IF
  END IF
END PROCEDURE mField_set2

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_set3
  INTEGER( I4B ) :: s( 2 )
  REAL( DFP ) :: alpha
  s = obj%domain%getLocalNodeNumber( [rowNodeNum, colNodeNum ] )
  IF( PRESENT( addContribution ) ) THEN
    alpha = INPUT( default=1.0_DFP, option=scale )
    CALL add(obj=obj%mat,rowNodeNum=s(1),colNodeNum=s(2),rowDOF=rowDOF, &
    & colDOF=colDOF, val=val, scale=alpha)
  ELSE
    CALL set(obj=obj%mat,rowNodeNum=s(1),colNodeNum=s(2), &
    & rowDOF=rowDOF,colDOF=colDOF,val=val)
  END IF
END PROCEDURE mField_set3

!----------------------------------------------------------------------------
!                                                                    setRow
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_setRow
  CHARACTER( LEN = * ), PARAMETER :: myName="mField_setRow"
  INTEGER( I4B ) :: inode
  inode = obj%domain%getLocalNodeNumber( globalNode )
  IF( PRESENT( scalarVal ) ) THEN
    CALL setRow( obj=obj%mat, inode=inode, idof=idof, val=scalarVal )
  ELSE IF( PRESENT( vecVal ) ) THEN
    CALL setRow( obj=obj%mat, inode=inode, idof=idof, val=vecVal )
  ELSE
    CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This routine currently is not applicable for node field nodeFieldVal')
  END IF
END PROCEDURE mField_setRow

!----------------------------------------------------------------------------
!                                                                 setColumn
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_setColumn
  CHARACTER( LEN = * ), PARAMETER :: myName="mField_setColumn"
  INTEGER( I4B ) :: inode
  inode = obj%domain%getLocalNodeNumber( globalNode )
  IF( PRESENT( scalarVal ) ) THEN
    CALL setColumn( obj=obj%mat, inode=inode, idof=idof, val=scalarVal )
  ELSE IF( PRESENT( vecVal ) ) THEN
    CALL setColumn( obj=obj%mat, inode=inode, idof=idof, val=vecVal )
  ELSE
    CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'This routine currently is not applicable for node field nodeFieldVal')
  END IF
END PROCEDURE mField_setColumn


END SUBMODULE SetMethods