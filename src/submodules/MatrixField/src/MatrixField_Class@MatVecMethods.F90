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

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This module contains matrix vector method for [[MatrixField_]]

SUBMODULE(MatrixField_Class) MatVecMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Matvec
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_Matvec1
  !!
#ifdef DEBUG_VER
  CHARACTER( LEN = * ), PARAMETER :: myName="mField_Matvec1"
  INTEGER( I4B ) :: s( 2 )
  s = obj%shape()
  IF( SIZE( y ) .NE. s( 1 ) .OR. SIZE( x ) .NE. s(2) ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'There is some mismatch in dimension of matrix and vectors' // &
    & 'The shape of MatrixField_ instance is ' &
    & // trim( str( s(1), .true. ) ) // ", " &
    & // trim( str( s(2), .true. ) ) // ", " &
    & // 'However, the size of x is ' &
    & // trim( str( SIZE( x ), .true. ) ) // ", " &
    & // 'and, the size of y is ' &
    & // trim( str( SIZE( y ), .true. ) ) // ", " )
#endif
  !!
  CALL Matvec( obj=obj%mat, y=y, x=x, transp=transp )
  !!
END PROCEDURE mField_Matvec1

!----------------------------------------------------------------------------
!                                                                    Matvec
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_Matvec2
  REAL( DFP ), POINTER :: xvec( : )
  REAL( DFP ), POINTER :: yvec( : )
  !!
  xvec => x%getPointer()
  yvec => y%getPointer()
  !!
  CALL Matvec( obj=obj%mat, y=yvec, x=xvec, transp=transp )
  !!
  NULLIFY( xvec, yvec )
  !!
END PROCEDURE mField_Matvec2

END SUBMODULE MatVecMethods