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
    IF( isOrthonormal0 ) THEN
      !!
      scale( 0:n-1, 1 ) = 1.0_DFP / SQRT( coeff0( 1:n, 2 ) )
      !!
    ELSE
      !!
      scale( 0, 1 ) = (2.0_DFP+alpha+beta)/2.0_DFP
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
    END IF
    !!
  ELSE
    !!
    IF( isOrthonormal0 ) THEN
      scale( 0:n-1, 1 ) = 1.0_DFP / SQRT( coeff0( 1:n, 2 ) )
    ELSE
      scale = 1.0_DFP
    END IF
    !!
  END IF
  !!
  scale( 0, 2 ) = scale( 0, 1 ) / SQRT( coeff0( 0, 2 ) )
  !!
  DO ii = 1, n-1
    scale( ii, 2 ) = scale( ii, 1 ) * scale( ii-1, 1)
  END DO
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
!                                                          BasisEvalGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_BasisEvalGradientScalar
  REAL( DFP ) :: Jn_1, Jn_2, J( size( ans ) )
  REAL( DFP ) :: a, b, ox2, alpha, beta, bn, avar1
  REAL( DFP ), PARAMETER :: tol=1.0E-10
  INTEGER( I4B ) :: ii
  !!
  ox2 = 1.0_DFP - x**2
  alpha = obj%alpha
  beta = obj%beta
  !!
  ans( 1 ) = obj%dP0( x=x )
  IF( n .EQ. 0_I4B ) RETURN
  !!
  !!
  !!
  IF( SOFTEQ( ox2, zero, tol=tol) ) THEN
    IF( x .GT. 0.0_DFP ) THEN
      avar1 = GAMMA( alpha + 2.0_DFP )
      DO ii = 1, n
        a = 0.5_DFP*(ii+alpha+beta+1.0)*GAMMA(ii+alpha+1.0)
        b = factorial( ii - 1 )
        ans( ii+1 ) = a / b / avar1
      END DO
    ELSE
      avar1 = GAMMA( beta + 2.0_DFP )
      DO ii = 1, n
        a = (-1)**(ii-1)*0.5_DFP*(ii+alpha+beta+1.0)*GAMMA(ii+beta+1.0)
        b = factorial( ii - 1 )
        ans( ii+1 ) = a / b / avar1
      END DO
    END IF
    RETURN
  END IF
  !!
  Jn_1 = obj%P0(x)
  Jn_2 = obj%Pm1(x)
  J( 1 ) = Jn_1
  J( 2 ) = (x-coeff(0,1))*scale(0,1)*Jn_1 - coeff(0,2)*scale(0,2)*Jn_2
  !!
  bn = 4.0*(1.0+alpha)*(1.0+beta) / (2.0+alpha+beta)**2 / (3.0+alpha+beta)
  a = bn *(2.0+alpha+beta+1.0)*scale(0, 1)
  avar1 = (alpha-beta)/(2.0+alpha+beta)
  b = avar1 - x
  ans( 2 ) = a * J(1) + b * J(2)
  ans( 2 ) = ans( 2 ) / ox2
  Jn_1 = J( 2 )
  Jn_2 = J( 1 )
  IF( n .EQ. 1_I4B ) RETURN
  !!
  DO ii = 3, n+1
    !!
    !! n = ii - 1
    !!
    J( ii ) = (x-coeff(ii-2,1))*scale(ii-2,1)*Jn_1 &
      & - coeff(ii-2,2)*scale(ii-2,2)*Jn_2
    !!
    Jn_1 = J( ii )
    Jn_2 = J( ii-1 )
    !!
    a = coeff( ii-1, 2 )*(2.0*(ii-1)+alpha+beta+1.0)*scale(ii-2, 1)
    avar1 = (alpha-beta)/(2.0*(ii-1)+alpha+beta)
    b = (ii-1) * (avar1 - x)
    ans( ii ) = a * J(ii-1) + b * J(ii)
    ans( ii ) = ans( ii ) / ox2
    !!
  END DO
  !!
  !! correct ans( n+1 )
  !!
  ii = n + 1
  !!
  bn = 4.0*n*(n+alpha)*(n+beta)*(n+alpha+beta) &
    & / (2.0*n+alpha+beta)**2 / (2.0*n+alpha+beta+1.0)/(2.0*n+alpha+beta-1.0)
  a = bn*(2.0*n+alpha+beta+1.0)*scale(ii-2, 1)
  !!
  ans( ii ) = a * J(ii-1) + b * J(ii)
  ans( ii ) = ans( ii ) / ox2
  !!
END PROCEDURE Orthopol_BasisEvalGradientScalar

!----------------------------------------------------------------------------
!                                                         BasisEvalGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthopol_BasisEvalGradientVector
  REAL( DFP ) :: J( size(x), n+1 )
  REAL( DFP ), DIMENSION( size(x) ) :: b, ox2, Jn_1, Jn_2
  REAL( DFP ) :: a, alpha, beta, bn, avar1
  REAL( DFP ), PARAMETER :: tol=1.0E-10
  INTEGER( I4B ) :: ii, jj
  !!
  ox2 = 1.0_DFP - x**2
  alpha = obj%alpha
  beta = obj%beta
  !!
  ans( :, 1 ) = obj%dP0( x=x )
  IF( n .EQ. 0_I4B ) RETURN
  !!
  DO jj = 1, SIZE(x)
    IF( SOFTEQ( ox2(jj), zero, tol=tol) ) THEN
      IF( x(jj) .GT. 0.0_DFP ) THEN
        avar1 = GAMMA( alpha + 2.0_DFP )
        DO ii = 1, n
          a = 0.5_DFP*(ii+alpha+beta+1.0)*GAMMA(ii+alpha+1.0)
          b( jj ) = factorial( ii - 1 )
          ans( jj, ii+1 ) = a / b( jj ) / avar1
        END DO
      ELSE
        avar1 = GAMMA( beta + 2.0_DFP )
        DO ii = 1, n
          a = (-1)**(ii-1)*0.5_DFP*(ii+alpha+beta+1.0)*GAMMA(ii+beta+1.0)
          b( jj ) = factorial( ii - 1 )
          ans( jj, ii+1 ) = a / b( jj ) / avar1
        END DO
      END IF
      RETURN
    END IF
  END DO
  !!
  Jn_1 = obj%P0(x)
  Jn_2 = obj%Pm1(x)
  J( :, 1 ) = Jn_1
  J( :, 2 ) = (x-coeff(0,1))*scale(0,1)*Jn_1 - coeff(0,2)*scale(0,2)*Jn_2
  !!
  bn = 4.0*(1.0+alpha)*(1.0+beta) / (2.0+alpha+beta)**2 / (3.0+alpha+beta)
  a = bn *(2.0+alpha+beta+1.0)*scale(0, 1)
  avar1 = (alpha-beta)/(2.0+alpha+beta)
  b = avar1 - x
  ans( :, 2 ) = a * J(:, 1) + b * J(:, 2)
  ans( :, 2 ) = ans( :, 2 ) / ox2
  Jn_1 = J( :, 2 )
  Jn_2 = J( :, 1 )
  IF( n .EQ. 1_I4B ) RETURN
  !!
  DO ii = 3, n+1
    !!
    !! n = ii - 1
    !!
    J( :, ii ) = (x-coeff(ii-2,1))*scale(ii-2,1)*Jn_1 &
      & - coeff(ii-2,2)*scale(ii-2,2)*Jn_2
    !!
    Jn_1 = J( :, ii )
    Jn_2 = J( :, ii-1 )
    !!
    a = coeff( ii-1, 2 )*(2.0*(ii-1)+alpha+beta+1.0)*scale(ii-2, 1)
    avar1 = (alpha-beta)/(2.0*(ii-1)+alpha+beta)
    b = (ii-1) * (avar1 - x)
    ans( :, ii ) = a * J(:, ii-1) + b * J(:, ii)
    ans( :, ii ) = ans( :, ii ) / ox2
    !!
  END DO
  !!
  !! correct ans( n+1 )
  !!
  ii = n + 1
  !!
  bn = 4.0*n*(n+alpha)*(n+beta)*(n+alpha+beta) &
    & / (2.0*n+alpha+beta)**2 / (2.0*n+alpha+beta+1.0)/(2.0*n+alpha+beta-1.0)
  a = bn*(2.0*n+alpha+beta+1.0)*scale(ii-2, 1)
  !!
  ans( :, ii ) = a * J(:, ii-1) + b * J(:, ii)
  ans( :, ii ) = ans( :, ii ) / ox2
  !!
END PROCEDURE Orthopol_BasisEvalGradientVector

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetMethods