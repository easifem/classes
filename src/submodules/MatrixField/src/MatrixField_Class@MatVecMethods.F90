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
CHARACTER(*), PARAMETER :: myName = "mField_Matvec1"
INTEGER(I4B) :: s(2), y1, x1

s = obj%SHAPE()
y1 = SIZE(y)
x1 = SIZE(x)

IF (y1 .NE. s(1) .OR. x1 .NE. s(2)) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'There is some mismatch in dimension of matrix and vectors'// &
    & 'The shape of MatrixField_ instance is ' &
    & //tostring(s(1))//", " &
    & //tostring(s(2))//", " &
    & //'However, the size of x is ' &
    & //tostring(x1)//", " &
    & //'and, the size of y is ' &
    & //tostring(y1))
END IF

CALL Matvec( &
  & obj=obj%mat, &
  & y=y, &
  & x=x, &
  & isTranspose=isTranspose, &
  & addContribution=addContribution, &
  & scale=scale)

END PROCEDURE mField_Matvec1

!----------------------------------------------------------------------------
!                                                                    Matvec
!----------------------------------------------------------------------------

MODULE PROCEDURE mField_Matvec2
CHARACTER(*), PARAMETER :: myName = "mField_Matvec2"
REAL(DFP), POINTER :: xvec(:)
REAL(DFP), POINTER :: yvec(:)

xvec => x%getPointer()
yvec => y%getPointer()

CALL Matvec( &
  & obj=obj%mat, &
  & y=yvec, &
  & x=xvec, &
  & isTranspose=isTranspose, &
  & addContribution=addContribution, &
  & scale=scale)

NULLIFY (xvec, yvec)

END PROCEDURE mField_Matvec2

END SUBMODULE MatVecMethods
