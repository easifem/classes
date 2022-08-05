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

MODULE LagrangePolynomial1D_Class
USE GlobalData
USE Polynomial1D_Class
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                    LagrangePolynomial1D_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 July 2022
! summary: LagrangePolynomial1D class is defined
!

TYPE, EXTENDS( Polynomial1D_ ) :: LagrangePolynomial1D_
  CONTAINS
    !!
    !! @ConstructorMethods
    !!
    FINAL :: func_Final
    !!
END TYPE LagrangePolynomial1D_

PUBLIC :: LagrangePolynomial1D_

!----------------------------------------------------------------------------
!                                              LagrangePolynomial1DPointer_
!----------------------------------------------------------------------------

TYPE :: LagrangePolynomial1DPointer_
  CLASS( LagrangePolynomial1D_ ), POINTER :: ptr => NULL()
END TYPE LagrangePolynomial1DPointer_

PUBLIC :: LagrangePolynomial1DPointer_

!----------------------------------------------------------------------------
!                                                  Final@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 July 2022
! summary: Finalizer

INTERFACE
MODULE SUBROUTINE func_final( obj )
  TYPE( LagrangePolynomial1D_ ), INTENT( INOUT ) :: obj
END SUBROUTINE func_final
END INTERFACE

!----------------------------------------------------------------------------
!                                   LagrangePolynomial1D@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Construct the LagrangePolynomial1D

INTERFACE
MODULE FUNCTION func_LagrangePolynomial1D1( i, x, varname ) &
  & RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: i
  !! ith lagrange polynomial
  REAL( DFP ), INTENT( IN ) :: x( : )
  !! points, order = size(x) - 1
  CHARACTER( LEN = * ), INTENT( IN ) :: varname
  !! variable name
  TYPE( LagrangePolynomial1D_ ) :: ans
END FUNCTION func_LagrangePolynomial1D1
END INTERFACE

INTERFACE LagrangePolynomial1D
  MODULE PROCEDURE func_LagrangePolynomial1D1
END INTERFACE LagrangePolynomial1D

PUBLIC :: LagrangePolynomial1D

!----------------------------------------------------------------------------
!                                   LagrangePolynomial1D@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Construct the LagrangePolynomial1D

INTERFACE
MODULE FUNCTION func_LagrangePolynomial1D2( i, v, varname ) &
  & RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: i
  !! ith lagrange polynomial
  REAL( DFP ), INTENT( IN ) :: v( :, : )
  !! Vandermonde matrix
  CHARACTER( LEN = * ), INTENT( IN ) :: varname
  !! variable name
  TYPE( LagrangePolynomial1D_ ) :: ans
END FUNCTION func_LagrangePolynomial1D2
END INTERFACE

INTERFACE LagrangePolynomial1D
  MODULE PROCEDURE func_LagrangePolynomial1D2
END INTERFACE LagrangePolynomial1D

!----------------------------------------------------------------------------
!                                   LagrangePolynomial1D@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Construct the LagrangePolynomial1D

INTERFACE
MODULE FUNCTION func_LagrangePolynomial1D3( i, v, ipiv, varname) &
  & RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: i
  !! ith lagrange polynomial
  REAL( DFP ), INTENT( INOUT ) :: v( :, : )
  !! LU decomposition of Vandermonde matrix
  INTEGER( I4B ), INTENT( IN ) :: ipiv( : )
  !! inverse pivoting mapping, compes from LU decomposition
  CHARACTER( LEN = * ), INTENT( IN ) :: varname
  !! variable name
  TYPE( LagrangePolynomial1D_ ) :: ans
END FUNCTION func_LagrangePolynomial1D3
END INTERFACE

INTERFACE LagrangePolynomial1D
  MODULE PROCEDURE func_LagrangePolynomial1D3
END INTERFACE LagrangePolynomial1D

!----------------------------------------------------------------------------
!                           LagrangePolynomial1D_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Construct the LagrangePolynomial1D

INTERFACE
MODULE FUNCTION func_LagrangePolynomial1D_Pointer1( i, x, varname ) &
  & RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: i
  REAL( DFP ), INTENT( IN ) :: x( : )
  CHARACTER( LEN = * ), INTENT( IN ) :: varname
  CLASS( LagrangePolynomial1D_ ), POINTER :: ans
END FUNCTION func_LagrangePolynomial1D_Pointer1
END INTERFACE

INTERFACE LagrangePolynomial1D_Pointer
  MODULE PROCEDURE func_LagrangePolynomial1D_Pointer1
END INTERFACE LagrangePolynomial1D_Pointer

PUBLIC :: LagrangePolynomial1D_Pointer

END MODULE LagrangePolynomial1D_Class