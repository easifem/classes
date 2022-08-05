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

MODULE JacobiPolynomial1D_Class
USE String_Class, ONLY: String
USE GlobalData
USE AbstractBasis_Class
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                        JacobiPolynomial1D_
!----------------------------------------------------------------------------

TYPE, EXTENDS( AbstractBasis1D_ ) :: JacobiPolynomial1D_
  PRIVATE
  INTEGER( I4B ) :: n = 0
    !! order of Jacobi polynomial
  REAL( DFP ) :: an_1= 0.0_DFP
    !! $\alpha_{n-1}$
  REAL( DFP ) :: bn_1 = 0.0_DFP
    !! $\beta_{n-1}$
  REAL( DFP ) :: sn_1 = 1.0_DFP
    !! scale for $J_{n-1}$
    !! For monic orthogonal $s_{n-1}$ = 1.0
    !! For monic orthonormal $s_{n-1}$ = 1.0 / sqrt( b_n )
    !! For non-monic orthogonal
    !! For non-monic orthonormal
  REAL( DFP ) :: sn_2 = 1.0_DFP
    !! scale for $J_{n-2}$
    !! For monic orthogonal sn_2 = 1.0
    !! For monic orthonormal sn_2 = 1.0 / sqrt( b_n * b_{n-1} )
    !! For non-monic orthogonal
    !! For non-monic orthonormal
  REAL( DFP ) :: alpha = 0.0_DFP
    !! alpha + 1 > 0
  REAL( DFP ) :: beta= 0.0_DFP
    !! beta + 1 > 0
  CLASS( JacobiPolynomial1D_ ), POINTER :: Jn_1 => NULL()
    !! Jacobi polynomial of order n-1
  CLASS( JacobiPolynomial1D_ ), POINTER :: Jn_2 => NULL()
    !! Jacobi polynomial of order n-2
  CONTAINS
  !!
  !! @ConstructorMethods
  !!
  PROCEDURE, PUBLIC, PASS( obj ) :: Deallocate => J_Deallocate
  FINAL :: J_Final
  PROCEDURE, PUBLIC, PASS( obj ) :: Initiate => J_Initiate
  PROCEDURE, PRIVATE, PASS( obj ) :: Initiate1 => &
    & J_Initiate1
  PROCEDURE, PRIVATE, PASS( obj ) :: Initiate2 => &
    & J_Initiate2
  PROCEDURE, PUBLIC, PASS( obj ) :: isInitiated => J_isInitiated
  !!
  !! @IOMethods
  !!
  PROCEDURE, PUBLIC, PASS( obj ) :: Display => J_Display
  !!
  !! @GetMethods
  !!
  PROCEDURE, PUBLIC, PASS( obj ) :: GetStringToDisplay => J_GetStringToDisplay
  PROCEDURE, PUBLIC, PASS( obj ) :: GetStringForUID => J_GetStringForUID
  PROCEDURE, PUBLIC, PASS( obj ) :: EvalScalar => J_EvalScalar
  PROCEDURE, PUBLIC, PASS( obj ) :: EvalGradient => J_EvalGradientScalar
  PROCEDURE, PUBLIC, PASS( obj ) :: Weight => J_Weight
  PROCEDURE, PUBLIC, PASS( obj ) :: GetRecurrenceCoeff => J_GetRecurrenceCoeff
  PROCEDURE, PUBLIC, PASS( obj ) :: Zeros => J_Zeros
  PROCEDURE, PUBLIC, PASS( obj ) :: GaussQuadrature => J_GaussQuadrature
  PROCEDURE, PUBLIC, PASS( obj ) :: GaussRadauQuadrature => &
    & J_GaussRadauQuadrature
  PROCEDURE, PUBLIC, PASS( obj ) :: GaussLobattoQuadrature => &
    & J_GaussLobattoQuadrature
  !! Get the recurrence coefficients
  !!
  !! @SetMethods
  !!
  PROCEDURE, PRIVATE, PASS( obj ) :: SetParam => J_SetParam
END TYPE JacobiPolynomial1D_

PUBLIC :: JacobiPolynomial1D_

!----------------------------------------------------------------------------
!                                                 JacobiPolynomial1DPointer_
!----------------------------------------------------------------------------

TYPE :: JacobiPolynomial1DPointer_
  CLASS( JacobiPolynomial1D_ ), POINTER :: ptr => NULL()
END TYPE JacobiPolynomial1DPointer_

PUBLIC :: JacobiPolynomial1DPointer_

!----------------------------------------------------------------------------
!                                     JacobiPolynomial1D@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2 Aug 2022
! summary: Jacobi polynomial 1D

INTERFACE
MODULE FUNCTION JacobiPolynomial1D1( varname, n, alpha, beta, &
  & isMonic, isOrthonormal ) RESULT( ans )
  CHARACTER( LEN = * ), INTENT( IN ) :: varname
  INTEGER( I4B ), INTENT( IN ) :: n
  REAL( DFP ), INTENT( IN ) :: alpha
    !! 1+alpha > 0
  REAL( DFP ), INTENT( IN ) :: beta
    !! 1+beta > 0
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isMonic
    !! Default is .FALSE.
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isOrthonormal
    !! Default is .FALSE.
  TYPE( JacobiPolynomial1D_ ) :: ans
    !! tree to be built
END FUNCTION JacobiPolynomial1D1
END INTERFACE

INTERFACE JacobiPolynomial1D
  MODULE PROCEDURE JacobiPolynomial1D1
END INTERFACE JacobiPolynomial1D

PUBLIC :: JacobiPolynomial1D

!----------------------------------------------------------------------------
!                              JacobiPolynomial1D_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2 Aug 2022
! summary: Jacobi polynomial 1D

INTERFACE
MODULE FUNCTION JacobiPolynomial1D_Pointer1( varname, n, alpha, beta, &
  & isMonic, isOrthonormal ) RESULT( ans )
  CHARACTER( LEN = * ), INTENT( IN ) :: varname
  INTEGER( I4B ), INTENT( IN ) :: n
  REAL( DFP ), INTENT( IN ) :: alpha
    !! 1+alpha > 0
  REAL( DFP ), INTENT( IN ) :: beta
    !! 1+beta > 0
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isMonic
    !! Default is .FALSE.
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isOrthonormal
    !! Default is .FALSE.
  CLASS( JacobiPolynomial1D_ ), POINTER :: ans
    !! tree to be built
END FUNCTION JacobiPolynomial1D_Pointer1
END INTERFACE

INTERFACE JacobiPolynomial1D_Pointer
  MODULE PROCEDURE JacobiPolynomial1D_Pointer1
END INTERFACE JacobiPolynomial1D_Pointer

PUBLIC :: JacobiPolynomial1D_Pointer

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 July 2022
! summary: Deallocate the tree

INTERFACE
MODULE RECURSIVE SUBROUTINE J_Deallocate( obj )
  CLASS( JacobiPolynomial1D_ ), INTENT( INOUT ) :: obj
END SUBROUTINE J_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                  Final@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE J_Final( obj )
  TYPE( JacobiPolynomial1D_ ), INTENT( INOUT ) :: obj
END SUBROUTINE J_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 July 2022
! summary: Build the 3 term recurrence tree

INTERFACE
MODULE RECURSIVE SUBROUTINE J_Initiate( obj, varname, n, alpha, beta, &
  & isMonic, isOrthonormal )
  CLASS( JacobiPolynomial1D_ ), INTENT( INOUT ) :: obj
    !! tree to be built
  CHARACTER( LEN = * ), INTENT( IN ) :: varname
  INTEGER( I4B ), INTENT( IN ) :: n
  REAL( DFP ), INTENT( IN ) :: alpha
    !! 1+alpha > 0
  REAL( DFP ), INTENT( IN ) :: beta
    !! 1+beta > 0
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isMonic
    !! Default is .FALSE.
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isOrthonormal
    !! Default is .FALSE.
END SUBROUTINE J_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 July 2022
! summary: Build the 3 term recurrence tree

INTERFACE
MODULE RECURSIVE SUBROUTINE J_Initiate1( obj, varname, n, &
    & alpha, beta, isMonic, isOrthonormal, alphaCoeff, betaCoeff, s1, &
    & s2 )
  CLASS( JacobiPolynomial1D_ ), INTENT( INOUT ) :: obj
    !! tree to be built
  CHARACTER( LEN = * ), INTENT( IN ) :: varname
  INTEGER( I4B ), INTENT( IN ) :: n
  REAL( DFP ), INTENT( IN ) :: alpha
    !! 1+alpha > 0
  REAL( DFP ), INTENT( IN ) :: beta
    !! 1+beta > 0
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isMonic
    !! Default is .FALSE.
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isOrthonormal
    !! Default is .FALSE.
  REAL( DFP ), INTENT( IN ) :: alphaCoeff( 0: )
  REAL( DFP ), INTENT( IN ) :: betaCoeff( 0: )
  REAL( DFP ), INTENT( IN ) :: s1( 0: )
  REAL( DFP ), INTENT( IN ) :: s2( 0: )
END SUBROUTINE J_Initiate1
END INTERFACE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 July 2022
! summary: Build the 3 term recurrence tree

INTERFACE
MODULE RECURSIVE SUBROUTINE J_Initiate2( obj, j1 )
  CLASS( JacobiPolynomial1D_ ), INTENT( INOUT ) :: obj
  CLASS( JacobiPolynomial1D_ ), TARGET, INTENT( INOUT ) :: j1
END SUBROUTINE J_Initiate2
END INTERFACE

!----------------------------------------------------------------------------
!                                             isInitiated@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 July 2022
! summary: If left or right is associated then it returns true

INTERFACE
MODULE ELEMENTAL FUNCTION J_isInitiated( obj ) RESULT( ans )
  CLASS( JacobiPolynomial1D_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ) :: ans
END FUNCTION J_isInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                                                      Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 July 2022
! summary: Display the recurrence tree

INTERFACE
MODULE RECURSIVE SUBROUTINE J_Display( obj, msg, unitno )
  CLASS( JacobiPolynomial1D_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitno
END SUBROUTINE J_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetStringForUID@GetMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE ELEMENTAL FUNCTION J_GetStringForUID( obj ) RESULT( ans )
  CLASS( JacobiPolynomial1D_ ), INTENT( IN ) :: obj
  TYPE( String ) :: ans
END FUNCTION J_GetStringForUID
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetStringToDisplay@GetMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION J_GetStringToDisplay( obj ) RESULT( ans )
  CLASS( JacobiPolynomial1D_ ), INTENT( IN ) :: obj
  TYPE( String ) :: ans
END FUNCTION J_GetStringToDisplay
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Eval@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate for single variable function

INTERFACE
  MODULE ELEMENTAL FUNCTION J_EvalScalar( obj, x ) RESULT( ans )
    CLASS( JacobiPolynomial1D_ ), INTENT( IN ) :: obj
    REAL( DFP ), INTENT( IN ) :: x
    REAL( DFP ) :: ans
  END FUNCTION J_EvalScalar
END INTERFACE

!----------------------------------------------------------------------------
!                                                   EvalGradient@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate gradient for 1d argument function

INTERFACE
  MODULE ELEMENTAL FUNCTION J_EvalGradientScalar( obj, x ) RESULT( ans )
    CLASS( JacobiPolynomial1D_ ), INTENT( IN ) :: obj
    REAL( DFP ), INTENT( IN ) :: x
    REAL( DFP ) :: ans
  END FUNCTION J_EvalGradientScalar
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Weight@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2 Aug 2022
! summary: Evaluate the weight
!

INTERFACE
MODULE ELEMENTAL FUNCTION J_Weight( obj, x ) RESULT( ans )
  CLASS( JacobiPolynomial1D_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( IN ) :: x
  REAL( DFP ) :: ans
END FUNCTION J_Weight
END INTERFACE

!----------------------------------------------------------------------------
!                                            J_GetRecurrenceCoeff@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2 Aug 2022
! summary: Returns the recurrence coefficient
!

INTERFACE
MODULE PURE SUBROUTINE J_GetRecurrenceCoeff( obj, coeff )
  CLASS( JacobiPolynomial1D_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( OUT ) :: coeff( 0:, 0: )
END SUBROUTINE J_GetRecurrenceCoeff
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Zeros@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Aug 2022
! summary: 	Returns zeros of jacobi polynomial

INTERFACE
MODULE FUNCTION J_Zeros( obj ) RESULT( ans )
  CLASS( JacobiPolynomial1D_ ), INTENT( IN ) :: obj
  REAL( DFP ), ALLOCATABLE :: ans( : )
END FUNCTION J_Zeros
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussQuadrature@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 Aug 2022
! summary: 	Returns Gauss quadrature points and weights

INTERFACE
MODULE FUNCTION J_GaussQuadrature( obj ) RESULT( ans )
  CLASS( JacobiPolynomial1D_ ), INTENT( IN ) :: obj
  REAL( DFP ), ALLOCATABLE :: ans( :, : )
END FUNCTION J_GaussQuadrature
END INTERFACE

!----------------------------------------------------------------------------
!                                          GaussRadauQuadrature@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 Aug 2022
! summary: 	Returns GaussRadau quadrature points and weights

INTERFACE
MODULE FUNCTION J_GaussRadauQuadrature( obj, a ) RESULT( ans )
  CLASS( JacobiPolynomial1D_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( IN ) :: a
  !! it should be either + 1 or -1
  REAL( DFP ), ALLOCATABLE :: ans( :, : )
END FUNCTION J_GaussRadauQuadrature
END INTERFACE

!----------------------------------------------------------------------------
!                                          GaussLobattoQuadrature@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 Aug 2022
! summary: 	Returns GaussLobatto quadrature points and weights

INTERFACE
MODULE FUNCTION J_GaussLobattoQuadrature( obj ) RESULT( ans )
  CLASS( JacobiPolynomial1D_ ), INTENT( IN ) :: obj
  REAL( DFP ), ALLOCATABLE :: ans( :, : )
END FUNCTION J_GaussLobattoQuadrature
END INTERFACE

!----------------------------------------------------------------------------
!                                                  SetParam@SetMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE J_SetParam( obj, n, alpha, beta, &
  &  an_1, bn_1, sn_1, sn_2, varname )
  CLASS( JacobiPolynomial1D_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: n
  REAL( DFP ), INTENT( IN ) :: alpha
  REAL( DFP ), INTENT( IN ) :: beta
  REAL( DFP ), INTENT( IN ) :: an_1
  REAL( DFP ), INTENT( IN ) :: bn_1
  REAL( DFP ), INTENT( IN ) :: sn_1
  REAL( DFP ), INTENT( IN ) :: sn_2
  CHARACTER( LEN = * ), INTENT( IN ) :: varname
END SUBROUTINE J_SetParam
END INTERFACE


END MODULE JacobiPolynomial1D_Class