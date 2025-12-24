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
USE Display_Method, ONLY: ToString

USE CSRMatrix_Method, ONLY: Matvec

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                    Matvec
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Matvec1
#ifdef DEBUG_VER
INTEGER(I4B) :: s(2), y1, x1
LOGICAL(LGT) :: problem
CHARACTER(*), PARAMETER :: myName = "obj_Matvec1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')

s = obj%SHAPE()
y1 = SIZE(y)
x1 = SIZE(x)

problem = y1 .NE. s(1) .OR. x1 .NE. s(2)

IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
               'There is some mismatch in dimension of matrix and vectors'// &
                    'The shape of MatrixField_ instance is ' &
                    //ToString(s(1))//", " &
                    //ToString(s(2))//", " &
                    //'However, the size of x is ' &
                    //ToString(x1)//", " &
                    //'and, the size of y is ' &
                    //ToString(y1))
END IF
#endif

CALL Matvec(obj=obj%mat, y=y, x=x, isTranspose=isTranspose, &
            addContribution=addContribution, scale=scale)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Matvec1

!----------------------------------------------------------------------------
!                                                                    Matvec
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Matvec2
#ifdef DEBUG_VER

CHARACTER(*), PARAMETER :: myName = "obj_Matvec2()"
#endif

REAL(DFP), POINTER :: xvec(:)
REAL(DFP), POINTER :: yvec(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

xvec => x%GetPointer()
yvec => y%GetPointer()

CALL Matvec(obj=obj%mat, y=yvec, x=xvec, isTranspose=isTranspose, &
            addContribution=addContribution, scale=scale)

NULLIFY (xvec, yvec)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Matvec2

END SUBMODULE MatVecMethods
