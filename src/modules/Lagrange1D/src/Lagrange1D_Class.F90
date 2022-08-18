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

MODULE Lagrange1D_Class
USE GlobalData
USE Polynomial1D_Class
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                    Lagrange1D_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 July 2022
! summary: Lagrange1D class is defined
!

TYPE, EXTENDS( Polynomial1D_ ) :: Lagrange1D_
  CONTAINS
    !!
    !! @ConstructorMethods
    !!
    FINAL :: func_Final
    !!
END TYPE Lagrange1D_

PUBLIC :: Lagrange1D_

!----------------------------------------------------------------------------
!                                              Lagrange1DPointer_
!----------------------------------------------------------------------------

TYPE :: Lagrange1DPointer_
  CLASS( Lagrange1D_ ), POINTER :: ptr => NULL()
END TYPE Lagrange1DPointer_

PUBLIC :: Lagrange1DPointer_

!----------------------------------------------------------------------------
!                                                  Final@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 July 2022
! summary: Finalizer

INTERFACE
MODULE SUBROUTINE func_final( obj )
  TYPE( Lagrange1D_ ), INTENT( INOUT ) :: obj
END SUBROUTINE func_final
END INTERFACE

!----------------------------------------------------------------------------
!                                             Lagrange1D@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Construct the Lagrange polynomial in 1D
!
!# Introduction
!
!- This routine constructs the lagrange polynomial in 1D
!- It solves a linear system by LU decomposition by using Lapack lib

INTERFACE
MODULE FUNCTION func_Lagrange1D1( i, x, order, varname ) RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: i
  !! ith lagrange polynomial
  REAL( DFP ), INTENT( IN ) :: x( : )
  !! points, order = size(x) - 1
  INTEGER( I4B ), INTENT( IN ) :: order
  !! order
  CHARACTER( LEN = * ), INTENT( IN ) :: varname
  !! variable name
  TYPE( Lagrange1D_ ) :: ans
END FUNCTION func_Lagrange1D1
END INTERFACE

INTERFACE Lagrange1D
  MODULE PROCEDURE func_Lagrange1D1
END INTERFACE Lagrange1D

PUBLIC :: Lagrange1D

!----------------------------------------------------------------------------
!                                              Lagrange1D@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Construct the Lagrange1D
!
!# Introduction
!
!- This routine returns the Lagrange polynomial in 1D
!- User provides the vandermonde matrix
!- the routine copies vandermonde matrix internally and solves a
! linear system by using Lapack lib
!

INTERFACE
MODULE FUNCTION func_Lagrange1D2( i, v, order, varname ) RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: i
  !! ith lagrange polynomial
  REAL( DFP ), INTENT( IN ) :: v( :, : )
  !! Vandermonde matrix
  INTEGER( I4B ), INTENT( IN ) :: order
  !! order
  CHARACTER( LEN = * ), INTENT( IN ) :: varname
  !! variable name
  TYPE( Lagrange1D_ ) :: ans
END FUNCTION func_Lagrange1D2
END INTERFACE

INTERFACE Lagrange1D
  MODULE PROCEDURE func_Lagrange1D2
END INTERFACE Lagrange1D

!----------------------------------------------------------------------------
!                                   Lagrange1D@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Construct the Lagrange1D
!
!# Introduction
!
!- This routine returns the Lagrange polynomial in 1D
!- User provides the LU deomposition of vandermonde matrix
!- The LU decomposition should be obtained by calling Lapack lib
!- The GetLU method should be used to obtain LU decomposition and ipiv
!- linear system of equations is solved by using Lapack lib
!
INTERFACE
MODULE FUNCTION func_Lagrange1D3( i, v, order, ipiv, varname) &
  & RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: i
  !! ith lagrange polynomial
  REAL( DFP ), INTENT( INOUT ) :: v( :, : )
  !! LU decomposition of Vandermonde matrix
  INTEGER( I4B ), INTENT( IN ) :: order
  !! order
  INTEGER( I4B ), INTENT( IN ) :: ipiv( : )
  !! inverse pivoting mapping, compes from LU decomposition
  CHARACTER( LEN = * ), INTENT( IN ) :: varname
  !! variable name
  TYPE( Lagrange1D_ ) :: ans
END FUNCTION func_Lagrange1D3
END INTERFACE

INTERFACE Lagrange1D
  MODULE PROCEDURE func_Lagrange1D3
END INTERFACE Lagrange1D

!----------------------------------------------------------------------------
!                                     Lagrange1D_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Construct the Lagrange polynomial in 1D
!
!# Introduction
!
!- This routine constructs the lagrange polynomial in 1D
!- It solves a linear system by LU decomposition by using Lapack lib

INTERFACE
MODULE FUNCTION func_Lagrange1D_P1( i, x, order, varname ) RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: i
  !! ith lagrange polynomial
  REAL( DFP ), INTENT( IN ) :: x( : )
  !! points, order = size(x) - 1
  INTEGER( I4B ), INTENT( IN ) :: order
  !! order
  CHARACTER( LEN = * ), INTENT( IN ) :: varname
  !! variable name
  CLASS( Lagrange1D_ ), POINTER :: ans
END FUNCTION func_Lagrange1D_P1
END INTERFACE

INTERFACE Lagrange1D_Pointer
  MODULE PROCEDURE func_Lagrange1D_P1
END INTERFACE Lagrange1D_Pointer

PUBLIC :: Lagrange1D_Pointer

!----------------------------------------------------------------------------
!                                     Lagrange1D_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Construct the Lagrange1D_Pointer
!
!# Introduction
!
!- This routine returns the Lagrange polynomial in 1D
!- User provides the vandermonde matrix
!- the routine copies vandermonde matrix internally and solves a
! linear system by using Lapack lib
!

INTERFACE
MODULE FUNCTION func_Lagrange1D_P2( i, v, order, varname ) RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: i
  !! ith lagrange polynomial
  REAL( DFP ), INTENT( IN ) :: v( :, : )
  !! Vandermonde matrix
  INTEGER( I4B ), INTENT( IN ) :: order
  !! order
  CHARACTER( LEN = * ), INTENT( IN ) :: varname
  !! variable name
  CLASS( Lagrange1D_ ), POINTER :: ans
END FUNCTION func_Lagrange1D_P2
END INTERFACE

INTERFACE Lagrange1D_Pointer
  MODULE PROCEDURE func_Lagrange1D_P2
END INTERFACE Lagrange1D_Pointer

!----------------------------------------------------------------------------
!                                     Lagrange1D_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Construct the Lagrange1D_Pointer
!
!# Introduction
!
!- This routine returns the Lagrange polynomial in 1D
!- User provides the LU deomposition of vandermonde matrix
!- The LU decomposition should be obtained by calling Lapack lib
!- The GetLU method should be used to obtain LU decomposition and ipiv
!- linear system of equations is solved by using Lapack lib
!
INTERFACE
MODULE FUNCTION func_Lagrange1D_P3( i, v, order, ipiv, varname) &
  & RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: i
  !! ith lagrange polynomial
  REAL( DFP ), INTENT( INOUT ) :: v( :, : )
  !! LU decomposition of Vandermonde matrix
  INTEGER( I4B ), INTENT( IN ) :: order
  !! order
  INTEGER( I4B ), INTENT( IN ) :: ipiv( : )
  !! inverse pivoting mapping, compes from LU decomposition
  CHARACTER( LEN = * ), INTENT( IN ) :: varname
  !! variable name
  CLASS( Lagrange1D_ ), POINTER :: ans
END FUNCTION func_Lagrange1D_P3
END INTERFACE

INTERFACE Lagrange1D_Pointer
  MODULE PROCEDURE func_Lagrange1D_P3
END INTERFACE Lagrange1D_Pointer

END MODULE Lagrange1D_Class