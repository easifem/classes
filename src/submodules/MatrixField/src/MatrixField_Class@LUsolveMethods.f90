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
! summary: This module contains matrix vector method for [[MatrixField_]]

SUBMODULE( MatrixField_Class ) LUSolveMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    LUSOLVE
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_LUSOLVE1
  CHARACTER( LEN = * ), PARAMETER :: myName="mField_LUSOLVE1"
  INTEGER( I4B ) :: s( 2 )
  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'MatrixField_ object is not initiated.')

  IF( .NOT. ASSOCIATED( obj%Pmat ) ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'obj%Pmat is not associted/ allocated. LUSOLVE needs LU &
    & decomposition, but it is not found, you can call &
    & setPrecondition() method to build LU matrix in obj%Pmat.')

  s = obj%shape()

  IF( SIZE( sol ) .NE. SIZE( rhs ) .OR. SIZE( sol ) .NE. s( 1 ) ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Size of sol vector should be equal to the size of rhs')

  CALL LUSOLVE( sol=sol, rhs=rhs, alu=obj%pmat%A, &
    & jlu=obj%pmat%JA, ju=obj%pmat%JU )

END PROCEDURE mField_LUSOLVE1

!----------------------------------------------------------------------------
!                                                                   LUSOLVE
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_LUSOLVE2
  REAL( DFP ), POINTER :: solVal( : ), rhsVal( : )
  CHARACTER( LEN = * ), PARAMETER :: myName="mField_LUSOLVE2"
  solVal => sol%getPointer()
  rhsVal => rhs%getPointer()
  CALL obj%LUSOLVE( sol=solVal, rhs=rhsVal )
  NULLIFY( solVal, rhsVal )
END PROCEDURE mField_LUSOLVE2

!----------------------------------------------------------------------------
!                                                                   LUTSOLVE
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_LUTSOLVE1
  CHARACTER( LEN = * ), PARAMETER :: myName="mField_LUTSOLVE1"
  INTEGER( I4B ) :: s( 2 )
  IF( .NOT. obj%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'MatrixField_ object is not initiated.')
  !
  IF( .NOT. ASSOCIATED( obj%Pmat ) ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'obj%Pmat is not associted/ allocated.LUTSOLVE needs LU &
    & decomposition, but it is not found, you can call &
    & setPrecondition() method to build LU matrix in obj%Pmat.')
  !
  s = obj%shape()
  !
  IF( SIZE( sol ) .NE. SIZE( rhs ) .OR. SIZE( sol ) .NE. s( 1 ) ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Size of sol vector should be equal to the size of rhs')
  !
  CALL LUTSOLVE( sol=sol, rhs=rhs, alu=obj%pmat%A, &
    & jlu=obj%pmat%JA, ju=obj%pmat%JU )
END PROCEDURE mField_LUTSOLVE1

!----------------------------------------------------------------------------
!                                                                  LUTSOLVE
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_LUTSOLVE2
  REAL( DFP ), POINTER :: solVal( : ), rhsVal( : )
  CHARACTER( LEN = * ), PARAMETER :: myName="mField_LUSOLVE2"
  solVal => sol%getPointer()
  rhsVal => rhs%getPointer()
  CALL obj%LUTSOLVE( sol=solVal, rhs=rhsVal )
  NULLIFY( solVal, rhsVal )
END PROCEDURE mField_LUTSOLVE2

END SUBMODULE LUSolveMethods