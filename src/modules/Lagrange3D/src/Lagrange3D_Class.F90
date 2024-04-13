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

MODULE Lagrange3D_Class
USE GlobalData
USE Polynomial3D_Class
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                               Lagrange3D_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 July 2022
! summary: Lagrange3D class is defined

TYPE, EXTENDS(Polynomial3D_) :: Lagrange3D_
CONTAINS
  PROCEDURE, PASS(obj) :: Initiate1
  PROCEDURE, PASS(obj) :: Initiate2
  PROCEDURE, PASS(obj) :: Initiate3
  FINAL :: func_Final
END TYPE Lagrange3D_

PUBLIC :: Lagrange3D_

!----------------------------------------------------------------------------
!                                                        Lagrange3DPointer_
!----------------------------------------------------------------------------

TYPE :: Lagrange3DPointer_
  CLASS(Lagrange3D_), POINTER :: ptr => NULL()
END TYPE Lagrange3DPointer_

PUBLIC :: Lagrange3DPointer_

!----------------------------------------------------------------------------
!                                                  Final@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 July 2022
! summary: Finalizer

INTERFACE
  MODULE SUBROUTINE func_final(obj)
    TYPE(Lagrange3D_), INTENT(INOUT) :: obj
  END SUBROUTINE func_final
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary: Initiate the object

INTERFACE
  MODULE SUBROUTINE Initiate1(obj, i, xij, order, varname1, varname2, &
    & varname3, elemType)
    CLASS(Lagrange3D_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: i
    REAL(DFP), INTENT(IN) :: xij(:, :)
    INTEGER(I4B), INTENT(IN) :: order
    CHARACTER(LEN=*), INTENT(IN) :: varname1
    CHARACTER(LEN=*), INTENT(IN) :: varname2
    CHARACTER(LEN=*), INTENT(IN) :: varname3
    INTEGER(I4B), INTENT(IN) :: elemType
  END SUBROUTINE Initiate1
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary: Initiate the object

INTERFACE
  MODULE SUBROUTINE Initiate2(obj, i, v, order, varname1, varname2, &
    & varname3, elemType)
    CLASS(Lagrange3D_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: i
    REAL(DFP), INTENT(IN) :: v(:, :)
    INTEGER(I4B), INTENT(IN) :: order
    CHARACTER(LEN=*), INTENT(IN) :: varname1
    CHARACTER(LEN=*), INTENT(IN) :: varname2
    CHARACTER(LEN=*), INTENT(IN) :: varname3
    INTEGER(I4B), INTENT(IN) :: elemType
  END SUBROUTINE Initiate2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary: Initiate the object

INTERFACE
  MODULE SUBROUTINE Initiate3(obj, i, v, ipiv, order, varname1, &
    & varname2, varname3, elemType)
    CLASS(Lagrange3D_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: i
    REAL(DFP), INTENT(INOUT) :: v(:, :)
    INTEGER(I4B), INTENT(IN) :: ipiv(:)
    INTEGER(I4B), INTENT(IN) :: order
    CHARACTER(LEN=*), INTENT(IN) :: varname1
    CHARACTER(LEN=*), INTENT(IN) :: varname2
    CHARACTER(LEN=*), INTENT(IN) :: varname3
    INTEGER(I4B), INTENT(IN) :: elemType
  END SUBROUTINE Initiate3
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Initiate@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 18 Oct 2022
! summary:         Initiate the familt of lagrange polynomials

INTERFACE
  MODULE SUBROUTINE Initiate4(obj, xij, order, varname1, varname2, &
    & varname3, elemType)
    REAL(DFP), INTENT(IN) :: xij(:, :)
    TYPE(Lagrange3D_), INTENT(INOUT) :: obj(SIZE(xij, 2))
    INTEGER(I4B), INTENT(IN) :: order
    CHARACTER(LEN=*), INTENT(IN) :: varname1
    CHARACTER(LEN=*), INTENT(IN) :: varname2
    CHARACTER(LEN=*), INTENT(IN) :: varname3
    INTEGER(I4B), INTENT(IN) :: elemType
  END SUBROUTINE Initiate4
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Lagrange3D@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Construct the Lagrange polynomial in 3D
!
!# Introduction
!
!- This routine constructs the lagrange polynomial in 3D
!- It solves a linear system by LU decomposition by using Lapack lib

INTERFACE
  MODULE FUNCTION func_Lagrange3D1(i, xij, order, varname1, varname2, &
    & varname3, elemType) RESULT(ans)
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
    CHARACTER(LEN=*), INTENT(IN) :: varname3
    !! variable varname
    INTEGER(I4B), INTENT(IN) :: elemType
    !! Triangle, Quadrangle
    TYPE(Lagrange3D_) :: ans
    !! Polynomial in 3D
  END FUNCTION func_Lagrange3D1
END INTERFACE

INTERFACE Lagrange3D
  MODULE PROCEDURE func_Lagrange3D1
END INTERFACE Lagrange3D

PUBLIC :: Lagrange3D

!----------------------------------------------------------------------------
!                                                        Lagrange3D@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Construct the Lagrange3D
!
!# Introduction
!
!- This routine returns the Lagrange polynomial in 3D
!- User provides the vandermonde matrix
!- the routine copies vandermonde matrix internally and solves a
! linear system by using Lapack lib

INTERFACE
  MODULE FUNCTION func_Lagrange3D2(i, v, order, varname1, varname2, &
    & varname3, elemType, isVandermonde) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: i
  !! ith lagrange polynomial
    REAL(DFP), INTENT(IN) :: v(:, :)
  !! Vandermonde matrix
    INTEGER(I4B), INTENT(IN) :: order
    CHARACTER(LEN=*), INTENT(IN) :: varname1
  !! variable varname
    CHARACTER(LEN=*), INTENT(IN) :: varname2
  !! variable varname
    CHARACTER(LEN=*), INTENT(IN) :: varname3
  !! variable varname
    INTEGER(I4B), INTENT(IN) :: elemType
  !! "Triangle" or "Quadrangle"
    LOGICAL(LGT), INTENT(IN) :: isVandermonde
  !! This is just to resolve interface issue
    TYPE(Lagrange3D_) :: ans
  END FUNCTION func_Lagrange3D2
END INTERFACE

INTERFACE Lagrange3D
  MODULE PROCEDURE func_Lagrange3D2
END INTERFACE Lagrange3D

!----------------------------------------------------------------------------
!                                                        Lagrange3D@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Construct the Lagrange3D
!
!# Introduction
!
!- This routine returns the Lagrange polynomial in 3D
!- User provides the LU deomposition of vandermonde matrix
!- The LU decomposition should be obtained by calling Lapack lib
!- The GetLU method should be used to obtain LU decomposition and ipiv
!- linear system of equations is solved by using Lapack lib

INTERFACE
  MODULE FUNCTION func_Lagrange3D3(i, v, order, ipiv, varname1, varname2, &
    & varname3, elemType) RESULT(ans)
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
    CHARACTER(LEN=*), INTENT(IN) :: varname3
    !! variable varname
    INTEGER(I4B), INTENT(IN) :: elemType
    !! "Triangle" or "Quadrangle"
    TYPE(Lagrange3D_) :: ans
  END FUNCTION func_Lagrange3D3
END INTERFACE

INTERFACE Lagrange3D
  MODULE PROCEDURE func_Lagrange3D3
END INTERFACE Lagrange3D

!----------------------------------------------------------------------------
!                                                        Lagrange3D@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Construct the Lagrange polynomial in 3D
!
!# Introduction
!
!- This routine constructs the lagrange polynomial in 3D
!- It solves a linear system by LU decomposition by using Lapack lib

INTERFACE
  MODULE FUNCTION func_Lagrange3D4(xij, order, varname1, varname2, &
    & varname3, elemType) RESULT(ans)
    REAL(DFP), INTENT(IN) :: xij(:, :)
    !! interpolation points in $x_{iJ}$ format
    INTEGER(I4B), INTENT(IN) :: order
    !! order
    CHARACTER(LEN=*), INTENT(IN) :: varname1
    !! variable varname
    CHARACTER(LEN=*), INTENT(IN) :: varname2
    !! variable varname
    CHARACTER(LEN=*), INTENT(IN) :: varname3
    !! variable varname
    INTEGER(I4B), INTENT(IN) :: elemType
    !! Triangle, Quadrangle
    TYPE(Lagrange3D_), ALLOCATABLE :: ans(:)
    !! Lagrange polynomials in 3D
  END FUNCTION func_Lagrange3D4
END INTERFACE

INTERFACE Lagrange3D
  MODULE PROCEDURE func_Lagrange3D4
END INTERFACE Lagrange3D

!----------------------------------------------------------------------------
!                                                Lagrange3D_Pointer@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Construct the Lagrange polynomial in 3D
!
!# Introduction
!
!- This routine constructs the lagrange polynomial in 3D
!- It solves a linear system by LU decomposition by using Lapack lib

INTERFACE
  MODULE FUNCTION func_Lagrange3P1(i, xij, order, varname1, varname2, &
    & varname3, elemType) RESULT(ans)
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
    CHARACTER(LEN=*), INTENT(IN) :: varname3
    !! variable varname
    INTEGER(I4B), INTENT(IN) :: elemType
    !! Triangle, Quadrangle
    CLASS(Lagrange3D_), POINTER :: ans
    !! Polynomial in 3D
  END FUNCTION func_Lagrange3P1
END INTERFACE

INTERFACE Lagrange3D_Pointer
  MODULE PROCEDURE func_Lagrange3P1
END INTERFACE Lagrange3D_Pointer

PUBLIC :: Lagrange3D_Pointer

!----------------------------------------------------------------------------
!                                              Lagrange3D_Pointer@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Construct the Lagrange3D_Pointer
!
!# Introduction
!
!- This routine returns the Lagrange polynomial in 3D
!- User provides the vandermonde matrix
!- the routine copies vandermonde matrix internally and solves a
! linear system by using Lapack lib
!

INTERFACE
  MODULE FUNCTION func_Lagrange3P2(i, v, order, varname1, varname2, &
    & varname3, elemType, isVandermonde) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: i
  !! ith lagrange polynomial
    REAL(DFP), INTENT(IN) :: v(:, :)
  !! Vandermonde matrix
    INTEGER(I4B), INTENT(IN) :: order
    CHARACTER(LEN=*), INTENT(IN) :: varname1
  !! variable varname
    CHARACTER(LEN=*), INTENT(IN) :: varname2
  !! variable varname
    CHARACTER(LEN=*), INTENT(IN) :: varname3
  !! variable varname
    INTEGER(I4B), INTENT(IN) :: elemType
  !! "Triangle" or "Quadrangle"
    LOGICAL(LGT), INTENT(IN) :: isVandermonde
  !! This is just to resolve interface issue
    CLASS(Lagrange3D_), POINTER :: ans
    !! Polynomial in 3D
  END FUNCTION func_Lagrange3P2
END INTERFACE

INTERFACE Lagrange3D_Pointer
  MODULE PROCEDURE func_Lagrange3P2
END INTERFACE Lagrange3D_Pointer

!----------------------------------------------------------------------------
!                                              Lagrange3D_Pointer@Methods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Construct the Lagrange3D_Pointer
!
!# Introduction
!
!- This routine returns the Lagrange polynomial in 3D
!- User provides the LU deomposition of vandermonde matrix
!- The LU decomposition should be obtained by calling Lapack lib
!- The GetLU method should be used to obtain LU decomposition and ipiv
!- linear system of equations is solved by using Lapack lib

INTERFACE
  MODULE FUNCTION func_Lagrange3P3(i, v, order, ipiv, varname1, varname2, &
    & varname3, elemType) RESULT(ans)
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
    CHARACTER(LEN=*), INTENT(IN) :: varname3
    !! variable varname
    INTEGER(I4B), INTENT(IN) :: elemType
    !! "Triangle" or "Quadrangle"
    CLASS(Lagrange3D_), POINTER :: ans
    !! Polynomial in 3D
  END FUNCTION func_Lagrange3P3
END INTERFACE

INTERFACE Lagrange3D_Pointer
  MODULE PROCEDURE func_Lagrange3P3
END INTERFACE Lagrange3D_Pointer

END MODULE Lagrange3D_Class
