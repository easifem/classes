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

SUBMODULE(Chebyshev1Polynomial1D_Class) GetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                           GetStringForUID
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetStringForUID
  ans = "T" // TRIM( STR( obj%GetOrder() ) ) // obj%varname%chars()
END PROCEDURE Orthopol_GetStringForUID

!----------------------------------------------------------------------------
!                                                                 Weight
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_Weight
  ans = SQRT( 1.0_DFP - x**2 )
  IF( ans .APPROXEQ. zero ) THEN
    RETURN
  ELSE
    ans = 1.0_DFP / ans
  END IF
END PROCEDURE Orthopol_Weight

!----------------------------------------------------------------------------
!                                                        GetRecurrenceCoeff
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetRecurrenceCoeff
  CALL GetChebyshev1RecurrenceCoeff( n=n, alphaCoeff=ans(0:,1), &
  & betaCoeff=ans(0:,2) )
END PROCEDURE Orthopol_GetRecurrenceCoeff

!----------------------------------------------------------------------------
!                                                  GetCoeffScale@GetMethods
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetCoeffScale
  !!
  REAL( DFP ) :: coeff0( 0:n, 1:2 )
  LOGICAL( LGT ) :: isMonic0, isOrthonomal0
  INTEGER( I4B ) :: ii
  !!
  coeff0 = obj%GetRecurrenceCoeff( n = n+1 )
  coeff = coeff0(0:n-1, 1:2)
  !!
  !!
  isMonic0 = INPUT( option=isMonic, default=.FALSE. )
  isOrthonomal0 = INPUT( option=isOrthonormal, default=.FALSE. )
  !!
  IF( .NOT. isMonic0 ) THEN
    !!
    scale( :, 1 ) = 2.0_DFP
    !!
    IF( isOrthonomal0 ) THEN
      scale( 0, 1 ) = 1.0_DFP / SQRT( 2.0_DFP )
    ELSE
      scale( 0, 1 ) = 1.0_DFP
    END IF
    !!
    scale( 0, 2 ) = scale( 0, 1 )
    !!
    DO ii = 1, n-1
      scale( ii, 2 ) = scale( ii, 1 ) * scale( ii-1, 1)
    END DO
    !!
  ELSE
    !!
    scale = 1.0_DFP
    !!
    IF( isOrthonomal0 ) THEN
      scale( 0:n-1, 1 ) = 1.0_DFP / SQRT( coeff0( 1:n, 2 ) )
      scale( 0:n-1, 2 ) = 1.0_DFP / SQRT( &
        & coeff0( 1:n, 2 )*coeff0( 0:n-1, 2 ) )
      !!
    END IF
    !!
  END IF
END PROCEDURE Orthopol_GetCoeffScale

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_Zeros
  INTEGER( I4B ) :: ii, n
  n = obj%getOrder()
  ALLOCATE( ans( n ) )
  DO ii = 1, n
    ans( ii ) = -COS( (2.0_DFP*ii-1.0_DFP)*pi*0.5_DFP/n )
  END DO
END PROCEDURE Orthopol_Zeros

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GaussQuadrature
  INTEGER( I4B ) :: n
  n = obj%getOrder()
  ALLOCATE( ans( n, 2 ) )
  ans( :, 1 ) = obj%Zeros()
  ans( :, 2 ) = pi/n
END PROCEDURE Orthopol_GaussQuadrature

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GaussRadauQuadrature
  INTEGER( I4B ) :: n, ii
  REAL( DFP ) :: avar, avar2
  !!
  n = obj%getOrder()
  ALLOCATE( ans( n+1, 2 ) )
  !!
  avar = 2.0_DFP * pi / (2.0_DFP * n + 1.0_DFP)
  avar2 = pi / (2.0_DFP * n + 1.0_DFP)
  !!
  DO ii = 0, n
    ans( ii+1, 1 ) = -COS( avar*ii )
    ans( ii+1, 2 ) = avar2
  END DO
  !!
  ans( 1, 2 ) = ans( 1, 2 ) / 2.0_DFP
  !!
END PROCEDURE Orthopol_GaussRadauQuadrature

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GaussLobattoQuadrature
  INTEGER( I4B ) :: n, ii
  REAL( DFP ) :: avar
  !!
  n = obj%getOrder()
  ALLOCATE( ans( n+2, 2 ) )
  !!
  avar = pi / (n + 1.0_DFP)
  !!
  DO ii = 0, n+1
    ans( ii+1, 1 ) = -COS( avar*ii )
    ans( ii+1, 2 ) = avar
  END DO
  !!
  ans( 1, 2 ) = ans( 1, 2 ) / 2.0_DFP
  ans( n+2, 2 ) = ans( n+2, 2 ) / 2.0_DFP
  !!
END PROCEDURE Orthopol_GaussLobattoQuadrature

END SUBMODULE GetMethods