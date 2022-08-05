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

MODULE JacobiBasis1D_Class
USE GlobalData
USE AbstractBasis_Class
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                            JacobiBasis1D_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 July 2022
! summary: JacobiBasis1D class is defined
!

TYPE, EXTENDS( AbstractBasis1D_ ) :: JacobiBasis1D_
  PRIVATE
  LOGICAL( LGT ) :: isMonic = .FALSE.
  LOGICAL( LGT ) :: isOrthonormal = .FALSE.
  INTEGER( I4B ) :: alpha=0_I4B, beta=0_I4B
  CONTAINS
  PRIVATE
    PROCEDURE(func_Eval1), DEFERRED, PUBLIC, PASS( Obj ) :: Eval
    PROCEDURE(func_EvalGradient1), DEFERRED, PUBLIC, PASS( Obj ) :: &
      & EvalGradient
    PROCEDURE, PUBLIC, PASS( Obj ) :: Deallocate => func_Deallocate1
    FINAL :: func_Final
END TYPE JacobiBasis1D_

PUBLIC :: JacobiBasis1D_

!----------------------------------------------------------------------------
!                                              JacobiBasis1DPointer_
!----------------------------------------------------------------------------

TYPE :: JacobiBasis1DPointer_
  CLASS( JacobiBasis1D_ ), POINTER :: ptr => NULL()
END TYPE JacobiBasis1DPointer_

PUBLIC :: JacobiBasis1DPointer_

!----------------------------------------------------------------------------
!                                                  Final@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 July 2022
! summary: Finalizer

INTERFACE
MODULE SUBROUTINE func_final( obj )
  TYPE( JacobiBasis1D_ ), INTENT( INOUT ) :: obj
END SUBROUTINE func_final
END INTERFACE

!----------------------------------------------------------------------------
!                                   JacobiBasis1D@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Construct the JacobiBasis1D

INTERFACE
MODULE FUNCTION func_JacobiBasis1D1( i, x, varname ) &
  & RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: i
  !! ith Jacobi polynomial
  REAL( DFP ), INTENT( IN ) :: x( : )
  !! points, order = size(x) - 1
  CHARACTER( LEN = * ), INTENT( IN ) :: varname
  !! variable name
  TYPE( JacobiBasis1D_ ) :: ans
END FUNCTION func_JacobiBasis1D1
END INTERFACE

INTERFACE JacobiBasis1D
  MODULE PROCEDURE func_JacobiBasis1D1
END INTERFACE JacobiBasis1D

PUBLIC :: JacobiBasis1D

!----------------------------------------------------------------------------
!                                   JacobiBasis1D@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Construct the JacobiBasis1D

INTERFACE
MODULE FUNCTION func_JacobiBasis1D2( i, v, varname ) &
  & RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: i
  !! ith Jacobi polynomial
  REAL( DFP ), INTENT( IN ) :: v( :, : )
  !! Vandermonde matrix
  CHARACTER( LEN = * ), INTENT( IN ) :: varname
  !! variable name
  TYPE( JacobiBasis1D_ ) :: ans
END FUNCTION func_JacobiBasis1D2
END INTERFACE

INTERFACE JacobiBasis1D
  MODULE PROCEDURE func_JacobiBasis1D2
END INTERFACE JacobiBasis1D

!----------------------------------------------------------------------------
!                                   JacobiBasis1D@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Construct the JacobiBasis1D

INTERFACE
MODULE FUNCTION func_JacobiBasis1D3( i, v, ipiv, varname) &
  & RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: i
  !! ith Jacobi polynomial
  REAL( DFP ), INTENT( INOUT ) :: v( :, : )
  !! LU decomposition of Vandermonde matrix
  INTEGER( I4B ), INTENT( IN ) :: ipiv( : )
  !! inverse pivoting mapping, compes from LU decomposition
  CHARACTER( LEN = * ), INTENT( IN ) :: varname
  !! variable name
  TYPE( JacobiBasis1D_ ) :: ans
END FUNCTION func_JacobiBasis1D3
END INTERFACE

INTERFACE JacobiBasis1D
  MODULE PROCEDURE func_JacobiBasis1D3
END INTERFACE JacobiBasis1D

!----------------------------------------------------------------------------
!                           JacobiBasis1D_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Construct the JacobiBasis1D

INTERFACE
MODULE FUNCTION func_JacobiBasis1D_Pointer1( i, x, varname ) &
  & RESULT( ans )
  INTEGER( I4B ), INTENT( IN ) :: i
  REAL( DFP ), INTENT( IN ) :: x( : )
  CHARACTER( LEN = * ), INTENT( IN ) :: varname
  CLASS( JacobiBasis1D_ ), POINTER :: ans
END FUNCTION func_JacobiBasis1D_Pointer1
END INTERFACE

INTERFACE JacobiBasis1D_Pointer
  MODULE PROCEDURE func_JacobiBasis1D_Pointer1
END INTERFACE JacobiBasis1D_Pointer

PUBLIC :: JacobiBasis1D_Pointer

END MODULE JacobiBasis1D_Class