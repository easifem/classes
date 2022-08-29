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

MODULE Lagrange2D_Class
USE GlobalData
USE Polynomial2D_Class
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                               Lagrange2D_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 July 2022
! summary: Lagrange2D class is defined

TYPE, EXTENDS(Polynomial2D_) :: Lagrange2D_
CONTAINS
  FINAL :: func_Final
END TYPE Lagrange2D_

PUBLIC :: Lagrange2D_

!----------------------------------------------------------------------------
!                                                        Lagrange2DPointer_
!----------------------------------------------------------------------------

TYPE :: Lagrange2DPointer_
  CLASS(Lagrange2D_), POINTER :: ptr => NULL()
END TYPE Lagrange2DPointer_

PUBLIC :: Lagrange2DPointer_

!----------------------------------------------------------------------------
!                                                  Final@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 July 2022
! summary: Finalizer

INTERFACE
  MODULE SUBROUTINE func_final(obj)
    TYPE(Lagrange2D_), INTENT(INOUT) :: obj
  END SUBROUTINE func_final
END INTERFACE

!----------------------------------------------------------------------------
!                                             Lagrange2D@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Construct the Lagrange polynomial in 2D
!
!# Introduction
!
!- This routine constructs the lagrange polynomial in 2D
!- It solves a linear system by LU decomposition by using Lapack lib

INTERFACE
  MODULE FUNCTION func_Lagrange2D1(i, x, order, varname1, varname2, elemType) &
    & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: i
  !! ith lagrange polynomial
    REAL(DFP), INTENT(IN) :: x(:, :)
  !! interpolation points in $x_{iJ}$ format
    INTEGER(I4B), INTENT(IN) :: order
  !! order
    CHARACTER(LEN=*), INTENT(IN) :: varname1
  !! variable varname
    CHARACTER(LEN=*), INTENT(IN) :: varname2
  !! variable varname
    INTEGER(I4B), INTENT(IN) :: elemType
  !! Triangle, Quadrangle
    TYPE(Lagrange2D_) :: ans
  !! Polynomial in 2D
  END FUNCTION func_Lagrange2D1
END INTERFACE

INTERFACE Lagrange2D
  MODULE PROCEDURE func_Lagrange2D1
END INTERFACE Lagrange2D

PUBLIC :: Lagrange2D

!----------------------------------------------------------------------------
!                                              Lagrange2D@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Construct the Lagrange2D
!
!# Introduction
!
!- This routine returns the Lagrange polynomial in 2D
!- User provides the vandermonde matrix
!- the routine copies vandermonde matrix internally and solves a
! linear system by using Lapack lib
!

INTERFACE
  MODULE FUNCTION func_Lagrange2D2(i, v, order, varname1, varname2, elemType, &
    & isVandermonde) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: i
  !! ith lagrange polynomial
    REAL(DFP), INTENT(IN) :: v(:, :)
  !! Vandermonde matrix
    INTEGER(I4B), INTENT(IN) :: order
    CHARACTER(LEN=*), INTENT(IN) :: varname1
  !! variable varname
    CHARACTER(LEN=*), INTENT(IN) :: varname2
  !! variable varname
    INTEGER(I4B), INTENT(IN) :: elemType
  !! "Triangle" or "Quadrangle"
    LOGICAL(LGT), INTENT(IN) :: isVandermonde
  !! This is just to resolve interface issue
    TYPE(Lagrange2D_) :: ans
  END FUNCTION func_Lagrange2D2
END INTERFACE

INTERFACE Lagrange2D
  MODULE PROCEDURE func_Lagrange2D2
END INTERFACE Lagrange2D

!----------------------------------------------------------------------------
!                                              Lagrange2D@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Construct the Lagrange2D
!
!# Introduction
!
!- This routine returns the Lagrange polynomial in 2D
!- User provides the LU deomposition of vandermonde matrix
!- The LU decomposition should be obtained by calling Lapack lib
!- The GetLU method should be used to obtain LU decomposition and ipiv
!- linear system of equations is solved by using Lapack lib
!
INTERFACE
 MODULE FUNCTION func_Lagrange2D3(i, v, order, ipiv, varname1, varname2, elemType) &
      & RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: i
  !! ith lagrange polynomial
    REAL(DFP), INTENT(INOUT) :: v(:, :)
  !! LU decomposition of Vandermonde matrix
    INTEGER(I4B), INTENT(IN) :: order
    INTEGER(I4B), INTENT(IN) :: ipiv(:)
  !! inverse pivoting mapping, compes from LU decomposition
    CHARACTER(LEN=*), INTENT(IN) :: varname1
  !! variable varname
    CHARACTER(LEN=*), INTENT(IN) :: varname2
  !! variable varname
    INTEGER(I4B), INTENT(IN) :: elemType
  !! "Triangle" or "Quadrangle"
    TYPE(Lagrange2D_) :: ans
  END FUNCTION func_Lagrange2D3
END INTERFACE

INTERFACE Lagrange2D
  MODULE PROCEDURE func_Lagrange2D3
END INTERFACE Lagrange2D

!----------------------------------------------------------------------------
!                                             Lagrange2D@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Construct the Lagrange polynomial in 2D
!
!# Introduction
!
!- This routine constructs the lagrange polynomial in 2D
!- It solves a linear system by LU decomposition by using Lapack lib

INTERFACE
  MODULE FUNCTION func_Lagrange2D4(x, order, varname1, varname2, elemType) &
    & RESULT(ans)
    REAL(DFP), INTENT(IN) :: x(:, :)
  !! interpolation points in $x_{iJ}$ format
    INTEGER(I4B), INTENT(IN) :: order
  !! order
    CHARACTER(LEN=*), INTENT(IN) :: varname1
  !! variable varname
    CHARACTER(LEN=*), INTENT(IN) :: varname2
  !! variable varname
    INTEGER(I4B), INTENT(IN) :: elemType
  !! Triangle, Quadrangle
    TYPE(Lagrange2D_), ALLOCATABLE :: ans(:)
  !! Lagrange polynomials in 2D
  END FUNCTION func_Lagrange2D4
END INTERFACE

INTERFACE Lagrange2D
  MODULE PROCEDURE func_Lagrange2D4
END INTERFACE Lagrange2D

END MODULE Lagrange2D_Class
