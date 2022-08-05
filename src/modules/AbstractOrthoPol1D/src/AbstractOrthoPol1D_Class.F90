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

MODULE AbstractOrthopol1D_Class
USE String_Class, ONLY: String
USE GlobalData
USE AbstractBasis_Class
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                           AbstractOrthopol1D_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 Aug 2022
! summary: 	Abstract orthogonal polynomial

TYPE, ABSTRACT,  EXTENDS( AbstractBasis1D_ ) :: &
  & AbstractOrthopol1D_
  PRIVATE
  INTEGER( I4B ) :: n = 0
    !! order of orthogonal polynomial
  REAL( DFP ) :: an_1= 0.0_DFP
    !! $\alpha_{n-1}$
  REAL( DFP ) :: bn_1 = 0.0_DFP
    !! $\beta_{n-1}$
  REAL( DFP ) :: sn_1 = 1.0_DFP
    !! scale for $Orthopol_{n-1}$
  REAL( DFP ) :: sn_2 = 1.0_DFP
    !! scale for $Orthopol_{n-2}$
  CLASS( AbstractOrthopol1D_ ), POINTER :: Jn_1 => NULL()
    !! Jacobi polynomial of order n-1
  CLASS( AbstractOrthopol1D_ ), POINTER :: Jn_2 => NULL()
    !! Jacobi polynomial of order n-2
  CONTAINS
  !!
  !! @ConstructorMethods
  !!
  PROCEDURE, PUBLIC, PASS( obj ) :: Initiate => Orthopol_Initiate
  PROCEDURE, PRIVATE, PASS( obj ) :: Initiate1 => &
    & Orthopol_Initiate1
  PROCEDURE, PRIVATE, PASS( obj ) :: Initiate2 => &
    & Orthopol_Initiate2
  PROCEDURE, PUBLIC, PASS( obj ) :: Deallocate => Orthopol_Deallocate
  !! Deallocate the data
  PROCEDURE, PRIVATE, PASS( obj ) :: Deallocate1 => Orthopol_Deallocate1
  !! Deallocate the data
  PROCEDURE, PRIVATE, PASS( obj ) :: Deallocate2 => Orthopol_Deallocate2
  !! Deallocate the data
  PROCEDURE, PUBLIC, PASS( obj ) :: isInitiated => Orthopol_isInitiated
  !! Returns true if the object is initiated
  !!
  !! @IOMethods
  !!
  PROCEDURE, PUBLIC, PASS( obj ) :: Display => Orthopol_Display
  !! Display the content
  !!
  !! @GetMethods
  !!
  PROCEDURE, PUBLIC, PASS( obj ) :: GetOrder => Orthopol_GetOrder
  !! Return the order of the polynomial
  PROCEDURE, PUBLIC, PASS( obj ) :: EvalScalar => Orthopol_EvalScalar
  !! Evaluate the polynomial
  PROCEDURE, PUBLIC, PASS( obj ) :: P0 => Orthopol_P0
  !! When n=0 this function is called for evaluting polynomial
  PROCEDURE, PUBLIC, PASS( obj ) :: Pm1 => Orthopol_Pm1
  !! When n=-1 this function is called for evaluting polynomial
  PROCEDURE, PUBLIC, PASS( obj ) :: dP0 => Orthopol_dP0
  !! When n=0 this function is called for evaluting polynomial
  PROCEDURE, PUBLIC, PASS( obj ) :: dPm1 => Orthopol_dPm1
  !! When n=-1 this function is called for evaluting polynomial
  PROCEDURE, PUBLIC, PASS( obj ) :: EvalGradient => &
    & Orthopol_EvalGradientScalar
  !! Evaluate the polynomial Gradient
  PROCEDURE, PUBLIC, PASS( obj ) :: GetStringToDisplay => &
    & Orthopol_GetStringToDisplay
  !! Get the string for display
  PROCEDURE(Orthopol_GetStringForUID), DEFERRED, PUBLIC, PASS( obj ) :: &
    & GetStringForUID
  !! Get String for unique ID
  PROCEDURE(Orthopol_Weight), DEFERRED,  PUBLIC, PASS( obj ) :: Weight
  !! Weight function
  PROCEDURE(Orthopol_GetRecurrenceCoeff), DEFERRED,  PUBLIC, PASS( obj ) :: &
    & GetRecurrenceCoeff
  PROCEDURE(Orthopol_GetCoeffScale), DEFERRED,  PUBLIC, PASS( obj ) :: &
    & GetCoeffScale
  !! Get recurrence coefficient
  PROCEDURE(Orthopol_Zeros), DEFERRED, PUBLIC, PASS( obj ) :: Zeros
  !! zeros of polynomial
  PROCEDURE(Orthopol_GaussQuadrature), DEFERRED, PUBLIC, PASS( obj ) :: &
    & GaussQuadrature
  !! Gauss quadrature points and weights
  PROCEDURE(Orthopol_GaussRadauQuadrature), DEFERRED, PUBLIC, PASS( obj ) :: &
    & GaussRadauQuadrature
  !! Gauss-Radau quadrature points
  PROCEDURE(Orthopol_GaussLobattoQuadrature), DEFERRED, PUBLIC, PASS( obj ) &
    & :: GaussLobattoQuadrature
  !! Gauss-Lobatto quadrature points and weights
  !!
  !! @SetMethods
  !!
  PROCEDURE, PRIVATE, PASS( obj ) :: SetParam => Orthopol_SetParam
  !!
END TYPE AbstractOrthopol1D_

PUBLIC :: AbstractOrthopol1D_

!----------------------------------------------------------------------------
!                                                 AbstractOrthopol1DPointer_
!----------------------------------------------------------------------------

TYPE :: AbstractOrthopol1DPointer_
  CLASS( AbstractOrthopol1D_ ), POINTER :: ptr => NULL()
END TYPE AbstractOrthopol1DPointer_

PUBLIC :: AbstractOrthopol1DPointer_

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 July 2022
! summary: Deallocate the object

INTERFACE
MODULE RECURSIVE SUBROUTINE Orthopol_Deallocate( obj )
  CLASS( AbstractOrthopol1D_ ), INTENT( INOUT ) :: obj
END SUBROUTINE Orthopol_Deallocate
END INTERFACE

INTERFACE AbstractOrthopol1DDeallocate
  MODULE PROCEDURE Orthopol_Deallocate
END INTERFACE AbstractOrthopol1DDeallocate

PUBLIC :: AbstractOrthopol1DDeallocate

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 July 2022
! summary: Deallocate the object

INTERFACE
MODULE RECURSIVE SUBROUTINE Orthopol_Deallocate1( obj )
  CLASS( AbstractOrthopol1D_ ), INTENT( INOUT ) :: obj
END SUBROUTINE Orthopol_Deallocate1
END INTERFACE

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 July 2022
! summary: Deallocate the object

INTERFACE
MODULE RECURSIVE SUBROUTINE Orthopol_Deallocate2( obj )
  CLASS( AbstractOrthopol1D_ ), INTENT( INOUT ) :: obj
END SUBROUTINE Orthopol_Deallocate2
END INTERFACE

!----------------------------------------------------------------------------
!                                             isInitiated@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 July 2022
! summary: If left or right is associated then it returns true

INTERFACE
MODULE ELEMENTAL FUNCTION Orthopol_isInitiated( obj ) RESULT( ans )
  CLASS( AbstractOrthopol1D_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ) :: ans
END FUNCTION Orthopol_isInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 July 2022
! summary: Build the 3 term recurrence tree

INTERFACE
MODULE RECURSIVE SUBROUTINE Orthopol_Initiate( obj, varname, n, &
  & coeff, scale )
  CLASS( AbstractOrthopol1D_ ), INTENT( INOUT ) :: obj
    !! tree to be built
  CHARACTER( LEN = * ), INTENT( IN ) :: varname
    !! variable
  INTEGER( I4B ), INTENT( IN ) :: n
    !! order of polynomial
  REAL( DFP ), INTENT( IN ) :: coeff( 0:, 1: )
    !! recurrence coefficient
  REAL( DFP ), INTENT( IN ) :: scale( 0:, 1: )
    !! additional scaling coefficient
END SUBROUTINE Orthopol_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 July 2022
! summary: Build the 3 term recurrence tree

INTERFACE
MODULE RECURSIVE SUBROUTINE Orthopol_Initiate1( obj, varname, n, &
    & coeff, scale )
  CLASS( AbstractOrthopol1D_ ), INTENT( INOUT ) :: obj
    !! tree to be built
  CHARACTER( LEN = * ), INTENT( IN ) :: varname
  INTEGER( I4B ), INTENT( IN ) :: n
    !! order of polynomial
  REAL( DFP ), INTENT( IN ) :: coeff( 0:, 1: )
  REAL( DFP ), INTENT( IN ) :: scale( 0:, 1: )
END SUBROUTINE Orthopol_Initiate1
END INTERFACE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 July 2022
! summary: Build the 3 term recurrence tree

INTERFACE
MODULE RECURSIVE SUBROUTINE Orthopol_Initiate2( obj, j1 )
  CLASS( AbstractOrthopol1D_  ), INTENT( INOUT ) :: obj
  CLASS( AbstractOrthopol1D_  ), TARGET, INTENT( INOUT ) :: j1
END SUBROUTINE Orthopol_Initiate2
END INTERFACE

!----------------------------------------------------------------------------
!                                                      Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 31 July 2022
! summary: Display the orthogonal polynomial

INTERFACE
MODULE RECURSIVE SUBROUTINE Orthopol_Display( obj, msg, unitno )
  CLASS( AbstractOrthopol1D_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitno
END SUBROUTINE Orthopol_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetOrder@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 Aug 2022
! summary: Returnt the order of polynomial

INTERFACE
MODULE PURE FUNCTION Orthopol_GetOrder( obj ) RESULT( ans )
  CLASS( AbstractOrthopol1D_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: ans
END FUNCTION Orthopol_GetOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Eval@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate for single variable function

INTERFACE
  MODULE ELEMENTAL FUNCTION Orthopol_EvalScalar( obj, x ) RESULT( ans )
    CLASS( AbstractOrthopol1D_ ), INTENT( IN ) :: obj
    REAL( DFP ), INTENT( IN ) :: x
    REAL( DFP ) :: ans
  END FUNCTION Orthopol_EvalScalar
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Eval@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate for single variable function

INTERFACE
  MODULE ELEMENTAL FUNCTION Orthopol_P0( obj, x ) RESULT( ans )
    CLASS( AbstractOrthopol1D_ ), INTENT( IN ) :: obj
    REAL( DFP ), INTENT( IN ) :: x
    REAL( DFP ) :: ans
  END FUNCTION Orthopol_P0
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Eval@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate for single variable function

INTERFACE
  MODULE ELEMENTAL FUNCTION Orthopol_Pm1( obj, x ) RESULT( ans )
    CLASS( AbstractOrthopol1D_ ), INTENT( IN ) :: obj
    REAL( DFP ), INTENT( IN ) :: x
    REAL( DFP ) :: ans
  END FUNCTION Orthopol_Pm1
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Eval@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate for single variable function

INTERFACE
  MODULE ELEMENTAL FUNCTION Orthopol_dP0( obj, x ) RESULT( ans )
    CLASS( AbstractOrthopol1D_ ), INTENT( IN ) :: obj
    REAL( DFP ), INTENT( IN ) :: x
    REAL( DFP ) :: ans
  END FUNCTION Orthopol_dP0
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Eval@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate for single variable function

INTERFACE
  MODULE ELEMENTAL FUNCTION Orthopol_dPm1( obj, x ) RESULT( ans )
    CLASS( AbstractOrthopol1D_ ), INTENT( IN ) :: obj
    REAL( DFP ), INTENT( IN ) :: x
    REAL( DFP ) :: ans
  END FUNCTION Orthopol_dPm1
END INTERFACE

!----------------------------------------------------------------------------
!                                                   EvalGradient@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 May 2022
! summary: Evaluate gradient for 1d argument function

INTERFACE
  MODULE ELEMENTAL FUNCTION Orthopol_EvalGradientScalar( obj, x ) &
    & RESULT( ans )
    CLASS( AbstractOrthopol1D_ ), INTENT( IN ) :: obj
    REAL( DFP ), INTENT( IN ) :: x
    REAL( DFP ) :: ans
  END FUNCTION Orthopol_EvalGradientScalar
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetStringForUID@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 Aug 2022
! summary: Return the string for UID

ABSTRACT INTERFACE
ELEMENTAL FUNCTION Orthopol_GetStringForUID( obj ) RESULT( ans )
  IMPORT AbstractOrthopol1D_, String
  CLASS( AbstractOrthopol1D_ ), INTENT( IN ) :: obj
  TYPE( String ) :: ans
END FUNCTION Orthopol_GetStringForUID
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetStringToDisplay@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 Aug 2022
! summary: 	Return the string to display

INTERFACE
MODULE PURE FUNCTION Orthopol_GetStringToDisplay( obj ) RESULT( ans )
  CLASS( AbstractOrthopol1D_ ), INTENT( IN ) :: obj
  TYPE( String ) :: ans
END FUNCTION Orthopol_GetStringToDisplay
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Weight@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2 Aug 2022
! summary: Evaluate the weight function

ABSTRACT INTERFACE
ELEMENTAL FUNCTION Orthopol_Weight( obj, x ) RESULT( ans )
  IMPORT AbstractOrthopol1D_, DFP
  CLASS( AbstractOrthopol1D_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( IN ) :: x
  REAL( DFP ) :: ans
END FUNCTION Orthopol_Weight
END INTERFACE

!----------------------------------------------------------------------------
!                                      Orthopol_GetRecurrenceCoeff@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2 Aug 2022
! summary: Returns the recurrence coefficient

ABSTRACT INTERFACE
PURE FUNCTION Orthopol_GetRecurrenceCoeff(obj, n) RESULT(ans)
  IMPORT AbstractOrthopol1D_, I4B, DFP
  CLASS( AbstractOrthopol1D_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: n
  REAL( DFP ) :: ans( 0:n-1, 2 )
END FUNCTION Orthopol_GetRecurrenceCoeff
END INTERFACE

!----------------------------------------------------------------------------
!                                                  GetCoeffScale@GetMethods
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
PURE SUBROUTINE Orthopol_GetCoeffScale( obj, n, coeff, scale, &
  & isMonic, isOrthonormal )
  IMPORT AbstractOrthopol1D_, I4B, DFP, LGT
  CLASS( AbstractOrthopol1D_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: n
  REAL( DFP ), INTENT( OUT ) :: coeff(0:,1:)
  REAL( DFP ), INTENT( OUT ) :: scale(0:,1:)
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isMonic
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isOrthonormal
END SUBROUTINE Orthopol_GetCoeffScale
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Zeros@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2 Aug 2022
! summary: 	Returns zeros of jacobi polynomial

ABSTRACT INTERFACE
FUNCTION Orthopol_Zeros( obj ) RESULT( ans )
  IMPORT AbstractOrthopol1D_, DFP
  CLASS( AbstractOrthopol1D_ ), INTENT( IN ) :: obj
  REAL( DFP ), ALLOCATABLE :: ans( : )
END FUNCTION Orthopol_Zeros
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussQuadrature@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 Aug 2022
! summary: 	Returns Gauss quadrature points and weights

ABSTRACT INTERFACE
FUNCTION Orthopol_GaussQuadrature( obj ) RESULT( ans )
  IMPORT AbstractOrthopol1D_, DFP
  CLASS( AbstractOrthopol1D_ ), INTENT( IN ) :: obj
  REAL( DFP ), ALLOCATABLE :: ans( :, : )
END FUNCTION Orthopol_GaussQuadrature
END INTERFACE

!----------------------------------------------------------------------------
!                                          GaussRadauQuadrature@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 Aug 2022
! summary: 	Returns GaussRadau quadrature points and weights

ABSTRACT INTERFACE
FUNCTION Orthopol_GaussRadauQuadrature( obj, a ) RESULT( ans )
  IMPORT AbstractOrthopol1D_, DFP
  CLASS( AbstractOrthopol1D_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( IN ) :: a
  !! it should be either + 1 or -1
  REAL( DFP ), ALLOCATABLE :: ans( :, : )
END FUNCTION Orthopol_GaussRadauQuadrature
END INTERFACE

!----------------------------------------------------------------------------
!                                          GaussLobattoQuadrature@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 Aug 2022
! summary: 	Returns GaussLobatto quadrature points and weights

ABSTRACT INTERFACE
FUNCTION Orthopol_GaussLobattoQuadrature( obj ) RESULT( ans )
  IMPORT AbstractOrthopol1D_, DFP
  CLASS( AbstractOrthopol1D_ ), INTENT( IN ) :: obj
  REAL( DFP ), ALLOCATABLE :: ans( :, : )
END FUNCTION Orthopol_GaussLobattoQuadrature
END INTERFACE

!----------------------------------------------------------------------------
!                                                  SetParam@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 Aug 2022
! summary: Setting the parameters of orthogonal polynomial

INTERFACE
MODULE PURE SUBROUTINE Orthopol_SetParam( obj, n, an_1, bn_1, sn_1, &
  & sn_2, varname )
  CLASS( AbstractOrthopol1D_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: n
    !! order of orthopol
  REAL( DFP ), INTENT( IN ) :: an_1
    !! alpha recurrence coeff
  REAL( DFP ), INTENT( IN ) :: bn_1
    !! beta recurrence coeff
  REAL( DFP ), INTENT( IN ) :: sn_1
    !! scalae for n-1 term
  REAL( DFP ), INTENT( IN ) :: sn_2
    !! scale for n-2 term
  CHARACTER( LEN = * ), INTENT( IN ) :: varname
    !! variable name
END SUBROUTINE Orthopol_SetParam
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE AbstractOrthopol1D_Class
