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

SUBMODULE(Jacobi1D_Class) GetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                           GetStringForUID
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetStringForUID
  ans = "P" // TRIM( STR( obj%GetOrder() ) ) // obj%varname%chars()
END PROCEDURE Orthopol_GetStringForUID

!----------------------------------------------------------------------------
!                                                                  Weight
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_Weight
  ans = ( 1.0_DFP - x ) ** obj%alpha + (1.0_DFP + x ) ** obj%beta
END PROCEDURE Orthopol_Weight

!----------------------------------------------------------------------------
!                                                        GetRecurrenceCoeff
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetRecurrenceCoeff
  !!
  CALL GetJacobiRecurrenceCoeff( &
    & n=n, &
    & alpha=obj%alpha, &
    & beta=obj%beta, &
    & alphaCoeff=ans(0:,1), &
    & betaCoeff=ans(0:,2) )
  !!
END PROCEDURE Orthopol_GetRecurrenceCoeff

!----------------------------------------------------------------------------
!                                                  GetCoeffScale@GetMethods
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GetCoeffScale
  !!
  REAL( DFP ) :: alpha, beta, coeff0( 0:n, 1:2 ), r1, r2, r3, r4
  LOGICAL( LGT ) :: isMonic0, isOrthonormal0
  INTEGER( I4B ) :: ii
  !!
  alpha = obj%alpha
  beta = obj%beta
  coeff0 = obj%GetRecurrenceCoeff( n = n+1 )
  coeff = coeff0(0:n-1, 1:2)
  !!
  isMonic0 = INPUT( option=isMonic, default=.FALSE. )
  isOrthonormal0 = INPUT( option=isOrthonormal, default=.FALSE. )
  !!
  IF( .NOT. isMonic0 ) THEN
    !!
    r1 = 2.0_DFP+alpha+beta
    r2 = 1.0_DFP
    r3 = 2.0_DFP
    scale( 0, 1 ) = r1*r2/r3
    !!
    DO ii = 1, n-1
      !!
      r1 = 2.0_DFP*ii+alpha+beta + 2.0_DFP
      r2 = r1 - 1.0_DFP
      r3 = 2.0_DFP * (ii+1.0) * (ii+alpha+beta+1.0_DFP)
      scale( ii, 1 ) = r1*r2/r3
      !!
    END DO
    !!
    IF( isOrthonormal0 ) THEN
      !!
      IF( n .GT. 1 ) THEN
        !!
        r1 = alpha+beta+3.0_DFP
        r2 = 1.0_DFP
        r3 = (1.0 + alpha)*(1.0 + beta)
        scale( 1, 1 ) = scale( 1, 1 ) * SQRT(r1*r2/r3)
        !!
      END IF
      !!
      DO ii = 2, n-1
        !!
        r1 = 2.0_DFP*ii+alpha+beta + 1.0_DFP
        r2 = ii * (ii+alpha+beta)
        r3 = (r1-2.0_DFP)*(ii+beta)*(ii+alpha)
        r4 = r1*r2/r3
        scale( ii, 1 ) = scale( ii, 1 ) * SQRT(r4)
        !!
      END DO
      !!
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
    IF( isOrthonormal0 ) THEN
      scale( 0:n-1, 1 ) = 1.0_DFP / SQRT( coeff0( 1:n, 2 ) )
      scale( 0:n-1, 2 ) = 1.0_DFP / SQRT( &
        & coeff0( 1:n, 2 )*coeff0( 0:n-1, 2 ) )
      !!
    END IF
    !!
  END IF
  !!
END PROCEDURE Orthopol_GetCoeffScale

!----------------------------------------------------------------------------
!                                                                   Zeros
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_Zeros
  ans = JacobiZeros( n = obj%GetOrder(), alpha=obj%alpha, beta=obj%beta )
END PROCEDURE Orthopol_Zeros

!----------------------------------------------------------------------------
!                                                           GaussQuadrature
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GaussQuadrature
  CALL Reallocate( ans, obj%GetOrder(), 2_I4B )
  !!
  CALL JacobiGaussQuadrature( &
    & n=obj%GetOrder(), &
    & alpha=obj%alpha, &
    & beta=obj%beta, &
    & pt=ans(:,1), &
    & wt=ans(:,2) )
  !!
END PROCEDURE Orthopol_GaussQuadrature

!----------------------------------------------------------------------------
!                                                     GaussRadauQuadrature
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GaussRadauQuadrature
  CALL Reallocate( ans, obj%GetOrder()+1, 2_I4B )
  !!
  CALL JacobiGaussRadauQuadrature( &
    & a = a, &
    & n=obj%GetOrder(), &
    & alpha=obj%alpha, &
    & beta=obj%beta, &
    & pt=ans(:,1), &
    & wt=ans(:,2) )
  !!
END PROCEDURE Orthopol_GaussRadauQuadrature

!----------------------------------------------------------------------------
!                                                     GaussLobattoQuadrature
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_GaussLobattoQuadrature
  CALL Reallocate( ans, obj%GetOrder()+2, 2_I4B )
  !!
  CALL JacobiGaussLobattoQuadrature( &
    & n=obj%GetOrder(), &
    & alpha=obj%alpha, &
    & beta=obj%beta, &
    & pt=ans(:,1), &
    & wt=ans(:,2) )
  !!
END PROCEDURE Orthopol_GaussLobattoQuadrature

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetMethods