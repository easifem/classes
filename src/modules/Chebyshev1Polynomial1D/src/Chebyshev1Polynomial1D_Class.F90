
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

MODULE Chebyshev1Polynomial1D_Class
USE String_Class, ONLY: String
USE GlobalData
USE AbstractBasis_Class
USE AbstractOrthoPol1D_Class
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                  Chebyshev1Polynomial1D_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 Aug 2022
! summary: Chebyshev1 orthogonal polynomial
!
!{!pages/Chebyshev1Polynomial1D_.md!}

TYPE, EXTENDS( AbstractOrthoPol1D_ ) :: Chebyshev1Polynomial1D_
  CONTAINS
  PRIVATE
  !!
  !! @GetMethods
  !!
  PROCEDURE, PUBLIC, PASS( obj ) :: &
    & GetStringForUID => Orthopol_GetStringForUID
  !! Get String for unique ID
  PROCEDURE, PUBLIC, PASS( obj ) :: Weight => Orthopol_Weight
  !! Weight function
  PROCEDURE, PUBLIC, PASS( obj ) :: GetRecurrenceCoeff => &
    & Orthopol_GetRecurrenceCoeff
  PROCEDURE, PUBLIC, PASS( obj ) :: GetCoeffScale => &
    & Orthopol_GetCoeffScale
  !! Get recurrence coefficient
  PROCEDURE, PUBLIC, PASS( obj ) :: Zeros => Orthopol_Zeros
  !! zeros of polynomial
  PROCEDURE, PUBLIC, PASS( obj ) :: GaussQuadrature => &
    & Orthopol_GaussQuadrature
  !! Gauss quadrature points and weights
  PROCEDURE, PUBLIC, PASS( obj ) :: GaussRadauQuadrature => &
    &  Orthopol_GaussRadauQuadrature
  !! Gauss-Radau quadrature points
  PROCEDURE, PUBLIC, PASS( obj ) :: GaussLobattoQuadrature => &
    & Orthopol_GaussLobattoQuadrature
  !! Gauss-Lobatto quadrature points and weights
  !!
END TYPE Chebyshev1Polynomial1D_

PUBLIC :: Chebyshev1Polynomial1D_

!----------------------------------------------------------------------------
!                                           Chebyshev1Polynomial1DPointer_
!----------------------------------------------------------------------------

TYPE :: Chebyshev1Polynomial1DPointer_
  CLASS( Chebyshev1Polynomial1D_ ), POINTER :: ptr => NULL()
END TYPE Chebyshev1Polynomial1DPointer_

PUBLIC :: Chebyshev1Polynomial1DPointer_

!----------------------------------------------------------------------------
!                                 Chebyshev1Polynomial1D@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 Aug 2022
! summary: Constructor for Chebyshev polynomial of first kind

INTERFACE
MODULE FUNCTION Chebyshev1Polynomial1D1( varname, n, isMonic, &
    & isOrthonormal ) RESULT( ans )
  CHARACTER( LEN = * ), INTENT( IN ) :: varname
    !! variable name
  INTEGER( I4B ), INTENT( IN ) :: n
    !! order of chebyshev polynomial
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isMonic
    !! Default is .FALSE., if true then leading coeff of poly is 1
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isOrthonormal
    !! Default is .FALSE., if true then the polynomials are orthonormal
  TYPE( Chebyshev1Polynomial1D_ ) :: ans
    !! Chebyshev polynomial of first kind
END FUNCTION Chebyshev1Polynomial1D1
END INTERFACE

INTERFACE Chebyshev1Polynomial1D
  MODULE PROCEDURE Chebyshev1Polynomial1D1
END INTERFACE Chebyshev1Polynomial1D

PUBLIC :: Chebyshev1Polynomial1D

!----------------------------------------------------------------------------
!                          Chebyshev1Polynomial1D_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 Aug 2022
! summary: Constructor for Chebyshev1 polynomial of first kind

INTERFACE
MODULE FUNCTION Chebyshev1Polynomial1D_Pointer1( varname, n, &
  & isMonic, isOrthonormal ) RESULT( ans )
  CHARACTER( LEN = * ), INTENT( IN ) :: varname
    !! variable name
  INTEGER( I4B ), INTENT( IN ) :: n
    !! order of chebyshev polynomial
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isMonic
    !! Default is .FALSE., if true then leading coeff of poly is 1
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: isOrthonormal
    !! Default is .FALSE., if true then the polynomials are orthonormal
  CLASS( Chebyshev1Polynomial1D_ ), POINTER :: ans
    !! Chebyshev polynomial of first kind
END FUNCTION Chebyshev1Polynomial1D_Pointer1
END INTERFACE

INTERFACE Chebyshev1Polynomial1D_Pointer
  MODULE PROCEDURE Chebyshev1Polynomial1D_Pointer1
END INTERFACE Chebyshev1Polynomial1D_Pointer

PUBLIC :: Chebyshev1Polynomial1D_Pointer

!----------------------------------------------------------------------------
!                                                GetStringForUID@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 Aug 2022
! summary: Return the string for UID

INTERFACE
MODULE ELEMENTAL FUNCTION Orthopol_GetStringForUID( obj ) RESULT( ans )
  CLASS( Chebyshev1Polynomial1D_ ), INTENT( IN ) :: obj
  TYPE( String ) :: ans
END FUNCTION Orthopol_GetStringForUID
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Weight@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2 Aug 2022
! summary: Evaluate the weight function

INTERFACE
MODULE ELEMENTAL FUNCTION Orthopol_Weight( obj, x ) RESULT( ans )
  CLASS( Chebyshev1Polynomial1D_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( IN ) :: x
  REAL( DFP ) :: ans
END FUNCTION Orthopol_Weight
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetRecurrenceCoeff@GetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2 Aug 2022
! summary: Returns the recurrence coefficient

INTERFACE
MODULE PURE FUNCTION Orthopol_GetRecurrenceCoeff(obj, n) RESULT(ans)
  CLASS( Chebyshev1Polynomial1D_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: n
  REAL( DFP ) :: ans( 0:n-1, 1:2 )
END FUNCTION Orthopol_GetRecurrenceCoeff
END INTERFACE

!----------------------------------------------------------------------------
!                                                  GetCoeffScale@GetMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Orthopol_GetCoeffScale( obj, n, coeff, scale, &
  & isMonic, isOrthonormal )
  CLASS( Chebyshev1Polynomial1D_ ), INTENT( IN ) :: obj
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

INTERFACE
MODULE FUNCTION Orthopol_Zeros( obj ) RESULT( ans )
  CLASS( Chebyshev1Polynomial1D_ ), INTENT( IN ) :: obj
  REAL( DFP ), ALLOCATABLE :: ans( : )
END FUNCTION Orthopol_Zeros
END INTERFACE

!----------------------------------------------------------------------------
!                                                GaussQuadrature@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 Aug 2022
! summary: 	Returns Gauss quadrature points and weights

INTERFACE
MODULE FUNCTION Orthopol_GaussQuadrature( obj ) RESULT( ans )
  CLASS( Chebyshev1Polynomial1D_ ), INTENT( IN ) :: obj
  REAL( DFP ), ALLOCATABLE :: ans( :, : )
END FUNCTION Orthopol_GaussQuadrature
END INTERFACE

!----------------------------------------------------------------------------
!                                          GaussRadauQuadrature@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 3 Aug 2022
! summary: 	Returns GaussRadau quadrature points and weights

INTERFACE
MODULE FUNCTION Orthopol_GaussRadauQuadrature( obj, a ) RESULT( ans )
  CLASS( Chebyshev1Polynomial1D_ ), INTENT( IN ) :: obj
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

INTERFACE
MODULE FUNCTION Orthopol_GaussLobattoQuadrature( obj ) RESULT( ans )
  CLASS( Chebyshev1Polynomial1D_ ), INTENT( IN ) :: obj
  REAL( DFP ), ALLOCATABLE :: ans( :, : )
END FUNCTION Orthopol_GaussLobattoQuadrature
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE Chebyshev1Polynomial1D_Class
