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

SUBMODULE(JacobiPolynomial1D_Class) GetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Eval
!----------------------------------------------------------------------------

MODULE PROCEDURE J_EvalScalar
REAL( DFP ) :: Jn_1, Jn_2
SELECT CASE( obj%n )
CASE( -1_I4B )
  ans = 0.0_DFP
CASE( 0_I4B )
  ans = 1.0_DFP
CASE DEFAULT
  !!
  IF( ASSOCIATED( obj%Jn_1 ) ) THEN
    Jn_1 = obj%Jn_1%Eval( x )
  ELSE
    Jn_1 = 0.0_DFP
  END IF
  !!
  IF( ASSOCIATED( obj%Jn_2 ) ) THEN
    Jn_2 = obj%Jn_2%Eval( x )
  ELSE
    Jn_2 = 0.0_DFP
  END IF
  !!
  ans = (x-obj%an_1)*obj%sn_1*Jn_1 - obj%bn_1*obj%sn_2*Jn_2
  !!
END SELECT
END PROCEDURE J_EvalScalar

!----------------------------------------------------------------------------
!                                                              EvalGradient
!----------------------------------------------------------------------------

MODULE PROCEDURE J_EvalGradientScalar
REAL( DFP ) :: GJn_1, GJn_2, Jn_1
SELECT CASE( obj%n )
CASE( -1_I4B, 0_I4B )
  ans = 0.0_DFP
CASE DEFAULT
  !!
  IF( ASSOCIATED( obj%Jn_1 ) ) THEN
    GJn_1 = obj%Jn_1%EvalGradient( x )
    Jn_1 = obj%Jn_1%Eval( x )
  ELSE
    GJn_1 = 0.0_DFP
    Jn_1 = 0.0_DFP
  END IF
  !!
  IF( ASSOCIATED( obj%Jn_2 ) ) THEN
    GJn_2 = obj%Jn_2%EvalGradient( x )
  ELSE
    GJn_2 = 0.0_DFP
  END IF
  !!
  ans = (x-obj%an_1) * obj%sn_1 * GJn_1 &
    & - obj%bn_1 * obj%sn_2 * GJn_2 &
    & + obj%sn_1 * Jn_1
  !!
END SELECT
!!
END PROCEDURE J_EvalGradientScalar

!----------------------------------------------------------------------------
!                                                           GetStringForUID
!----------------------------------------------------------------------------

MODULE PROCEDURE J_GetStringForUID
  ans = "J" // TRIM( STR( obj%n ) ) // obj%varname%chars()
END PROCEDURE J_GetStringForUID

!----------------------------------------------------------------------------
!                                                         GetStringToDisplay
!----------------------------------------------------------------------------

MODULE PROCEDURE J_GetStringToDisplay
  INTEGER( I4B ) :: n_1, n_2
  !!
  IF( obj%n .EQ. 0 ) THEN
    ans = "J_0( " // obj%varname%chars() // " ) = 1"
  ELSEIF( obj%n .EQ. -1 ) THEN
    ans = ""
  ELSEIF( obj%n .EQ. 1 ) THEN
    ans = "J_1( " // obj%varname%chars() // " ) = " // &
    & "( x-"// TRIM(STR( n=obj%an_1, fm="(f0.3)")) // " )* " // &
    & TRIM(STR( n=obj%sn_1, fm="(f0.3)")) // "* J_0"
  ELSE
    !!
    IF( ASSOCIATED( obj%Jn_1 ) ) THEN
      n_1 = obj%Jn_1%n
    ELSE
      n_1 = 0
    END IF
    !!
    IF( ASSOCIATED( obj%Jn_2 ) ) THEN
      n_2 = obj%Jn_2%n
    ELSE
      n_2 = 0
    END IF
    !!
    ans = "J_" // TRIM( STR( obj%n ) ) // "( " //  &
      & obj%varname%chars() // " ) = " //&
      &  "( x-"// TRIM(STR( n=obj%an_1, fm="(f0.3)")) //&
      & " )* " // TRIM(STR( n=obj%sn_1, fm="(f0.3)")) //&
      &  "* J_" // &
      & TRIM( STR( n_1 ) ) // &
      & "( " //  obj%varname%chars() // " ) - " // &
      & TRIM( STR( n=obj%bn_1, fm="(f0.3)") ) // &
      & "* " // TRIM(STR( n=obj%sn_2, fm="(f0.3)")) //&
      & "* J_" // TRIM( STR( n_2 ) ) // &
      & "( " //  obj%varname%chars() // " )"
  END IF
END PROCEDURE J_GetStringToDisplay

!----------------------------------------------------------------------------
!                                                                  Weight
!----------------------------------------------------------------------------

MODULE PROCEDURE J_Weight
  ans = ( 1.0_DFP - x ) ** obj%alpha + (1.0_DFP + x ) ** obj%beta
END PROCEDURE J_Weight

!----------------------------------------------------------------------------
!                                                        GetRecurrenceCoeff
!----------------------------------------------------------------------------

MODULE PROCEDURE J_GetRecurrenceCoeff
  !!
  CALL GetJacobiRecurrenceCoeff( &
    & n=obj%n, &
    & alpha=obj%alpha, &
    & beta=obj%beta, &
    & alphaCoeff=coeff(0:,1), &
    & betaCoeff=coeff(0:,2) )
  !!
END PROCEDURE J_GetRecurrenceCoeff

!----------------------------------------------------------------------------
!                                                                   Zeros
!----------------------------------------------------------------------------

MODULE PROCEDURE J_Zeros
  ans = JacobiZeros( n = obj%n, alpha=obj%alpha, beta=obj%beta )
END PROCEDURE J_Zeros

!----------------------------------------------------------------------------
!                                                           GaussQuadrature
!----------------------------------------------------------------------------

MODULE PROCEDURE J_GaussQuadrature
  CALL Reallocate( ans, obj%n, 2_I4B )
  !!
  CALL JacobiGaussQuadrature( &
    & n=obj%n, &
    & alpha=obj%alpha, &
    & beta=obj%beta, &
    & pt=ans(:,1), &
    & wt=ans(:,2) )
  !!
END PROCEDURE J_GaussQuadrature

!----------------------------------------------------------------------------
!                                                     GaussRadauQuadrature
!----------------------------------------------------------------------------

MODULE PROCEDURE J_GaussRadauQuadrature
  CALL Reallocate( ans, obj%n+1, 2_I4B )
  !!
  CALL JacobiGaussRadauQuadrature( &
    & a = a, &
    & n=obj%n, &
    & alpha=obj%alpha, &
    & beta=obj%beta, &
    & pt=ans(:,1), &
    & wt=ans(:,2) )
  !!
END PROCEDURE J_GaussRadauQuadrature

!----------------------------------------------------------------------------
!                                                     GaussLobattoQuadrature
!----------------------------------------------------------------------------

MODULE PROCEDURE J_GaussLobattoQuadrature
  CALL Reallocate( ans, obj%n+2, 2_I4B )
  !!
  CALL JacobiGaussLobattoQuadrature( &
    & n=obj%n, &
    & alpha=obj%alpha, &
    & beta=obj%beta, &
    & pt=ans(:,1), &
    & wt=ans(:,2) )
  !!
END PROCEDURE J_GaussLobattoQuadrature

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetMethods