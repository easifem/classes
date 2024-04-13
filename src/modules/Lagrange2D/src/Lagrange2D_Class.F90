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
  PRIVATE
  PROCEDURE, PASS(obj) :: Initiate1
  PROCEDURE, PASS(obj) :: Initiate2
  PROCEDURE, PASS(obj) :: Initiate3
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

INTERFACE
  MODULE SUBROUTINE Initiate1(obj, i, xij, order, &
    & varname1, varname2, elemType)
    CLASS(Lagrange2D_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: i
    REAL(DFP), INTENT(IN) :: xij(:, :)
    INTEGER(I4B), INTENT(IN) :: order
    CHARACTER(LEN=*), INTENT(IN) :: varname1
    CHARACTER(LEN=*), INTENT(IN) :: varname2
    INTEGER(I4B), INTENT(IN) :: elemType
  END SUBROUTINE Initiate1
END INTERFACE

!----------------------------------------------------------------------------
!                                             Lagrange2D@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE Initiate2(obj, i, v, order, varname1, varname2, &
    & elemType, isVandermonde)
    CLASS(Lagrange2D_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: i
    REAL(DFP), INTENT(IN) :: v(:, :)
    INTEGER(I4B), INTENT(IN) :: order
    CHARACTER(LEN=*), INTENT(IN) :: varname1
    CHARACTER(LEN=*), INTENT(IN) :: varname2
    INTEGER(I4B), INTENT(IN) :: elemType
    LOGICAL(LGT), INTENT(IN) :: isVandermonde
  END SUBROUTINE Initiate2
END INTERFACE

!----------------------------------------------------------------------------
!                                             Lagrange2D@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE Initiate3(obj, i, v, ipiv, order, varname1, &
    & varname2, elemType)
    CLASS(Lagrange2D_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: i
    REAL(DFP), INTENT(INOUT) :: v(:, :)
    INTEGER(I4B), INTENT(IN) :: ipiv(:)
    INTEGER(I4B), INTENT(IN) :: order
    CHARACTER(LEN=*), INTENT(IN) :: varname1
    CHARACTER(LEN=*), INTENT(IN) :: varname2
    INTEGER(I4B), INTENT(IN) :: elemType
  END SUBROUTINE Initiate3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 18 Oct 2022
! summary:         Constructor

INTERFACE
  MODULE SUBROUTINE Initiate4(obj, xij, order, varname1, varname2, elemType)
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! interpolation points in xij format
    TYPE(Lagrange2D_), INTENT(INOUT) :: obj(SIZE(xij, 2))
    !! n lagrange polynomials
    INTEGER(I4B), INTENT(IN) :: order
    !! order of approximation
    CHARACTER(LEN=*), INTENT(IN) :: varname1
    !! variable name
    CHARACTER(LEN=*), INTENT(IN) :: varname2
    !! variable name
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type
  END SUBROUTINE Initiate4
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
  MODULE FUNCTION func_Lagrange2D1(i, xij, order, varname1, varname2, &
    & elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: i
    !! ith lagrange polynomial
    REAL(DFP), INTENT(IN) :: xij(:, :)
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

INTERFACE
  MODULE FUNCTION func_Lagrange2D2(i, v, order, varname1, varname2, &
    & elemType, isVandermonde) RESULT(ans)
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

INTERFACE
  MODULE FUNCTION func_Lagrange2D3(i, v, order, ipiv, varname1, &
    & varname2, elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: i
    !! ith lagrange polynomial
    REAL(DFP), INTENT(INOUT) :: v(:, :)
    !! LU decomposition of Vandermonde matrix
    INTEGER(I4B), INTENT(IN) :: order
    !! order of element
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
  MODULE FUNCTION func_Lagrange2D4(xij, order, varname1, varname2, elemType) &
    & RESULT(ans)
    REAL(DFP), INTENT(IN) :: xij(:, :)
  !! interpolation points in $x_{iJ}$ format
    INTEGER(I4B), INTENT(IN) :: order
  !! order
    CHARACTER(LEN=*), INTENT(IN) :: varname1
  !! variable varname
    CHARACTER(LEN=*), INTENT(IN) :: varname2
  !! variable varname
    INTEGER(I4B), INTENT(IN) :: elemType
  !! Triangle, Quadrangle
    TYPE(Lagrange2D_) :: ans(SIZE(xij, 2))
  !! Lagrange polynomials in 2D
  END FUNCTION func_Lagrange2D4
END INTERFACE

INTERFACE Lagrange2D
  MODULE PROCEDURE func_Lagrange2D4
END INTERFACE Lagrange2D

!----------------------------------------------------------------------------
!                                                Lagrange2D_Pointer@Methods
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
  MODULE FUNCTION func_Lagrange2P1(i, xij, order, varname1, varname2, &
    & elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: i
    !! ith lagrange polynomial
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! interpolation points in $x_{iJ}$ format
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    CHARACTER(LEN=*), INTENT(IN) :: varname1
    !! variable varname
    CHARACTER(LEN=*), INTENT(IN) :: varname2
    !! variable varname
    INTEGER(I4B), INTENT(IN) :: elemType
    !! Triangle, Quadrangle
    CLASS(Lagrange2D_), POINTER :: ans
    !! Polynomial in 2D
  END FUNCTION func_Lagrange2P1
END INTERFACE

INTERFACE Lagrange2D_Pointer
  MODULE PROCEDURE func_Lagrange2P1
END INTERFACE Lagrange2D_Pointer

PUBLIC :: Lagrange2D_Pointer

!----------------------------------------------------------------------------
!                                                Lagrange2D_Pointer@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Construct the Lagrange2D_Pointer
!
!# Introduction
!
!- This routine returns the Lagrange polynomial in 2D
!- User provides the vandermonde matrix
!- the routine copies vandermonde matrix internally and solves a
! linear system by using Lapack lib

INTERFACE
  MODULE FUNCTION func_Lagrange2P2(i, v, order, varname1, varname2, &
    & elemType, isVandermonde) RESULT(ans)
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
    CLASS(Lagrange2D_), POINTER :: ans
  END FUNCTION func_Lagrange2P2
END INTERFACE

INTERFACE Lagrange2D_Pointer
  MODULE PROCEDURE func_Lagrange2P2
END INTERFACE Lagrange2D_Pointer

!----------------------------------------------------------------------------
!                                                Lagrange2D_Pointer@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Construct the Lagrange2D_Pointer
!
!# Introduction
!
!- This routine returns the Lagrange polynomial in 2D
!- User provides the LU deomposition of vandermonde matrix
!- The LU decomposition should be obtained by calling Lapack lib
!- The GetLU method should be used to obtain LU decomposition and ipiv
!- linear system of equations is solved by using Lapack lib

INTERFACE
  MODULE FUNCTION func_Lagrange2P3(i, v, order, ipiv, varname1, &
    & varname2, elemType) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: i
    !! ith lagrange polynomial
    REAL(DFP), INTENT(INOUT) :: v(:, :)
    !! LU decomposition of Vandermonde matrix
    INTEGER(I4B), INTENT(IN) :: order
    !! order of element
    INTEGER(I4B), INTENT(IN) :: ipiv(:)
    !! inverse pivoting mapping, compes from LU decomposition
    CHARACTER(LEN=*), INTENT(IN) :: varname1
    !! variable varname
    CHARACTER(LEN=*), INTENT(IN) :: varname2
    !! variable varname
    INTEGER(I4B), INTENT(IN) :: elemType
    !! "Triangle" or "Quadrangle"
    CLASS(Lagrange2D_), POINTER :: ans
  END FUNCTION func_Lagrange2P3
END INTERFACE

INTERFACE Lagrange2D_Pointer
  MODULE PROCEDURE func_Lagrange2P3
END INTERFACE Lagrange2D_Pointer

END MODULE Lagrange2D_Class
