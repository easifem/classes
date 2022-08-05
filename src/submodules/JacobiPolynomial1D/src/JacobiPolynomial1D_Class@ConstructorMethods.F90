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

SUBMODULE(JacobiPolynomial1D_Class) ConstructorMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                       JacobiPolynomial1D
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiPolynomial1D1
  CALL ans%Initiate( &
    & varname=varname,  &
    & n=n,  &
    & alpha=alpha,  &
    & beta=beta,  &
    & isMonic=isMonic, &
    & isOrthonormal=isOrthonormal )
END PROCEDURE JacobiPolynomial1D1

!----------------------------------------------------------------------------
!                                                JacobiPolynomial1D_Pointer1
!----------------------------------------------------------------------------

MODULE PROCEDURE JacobiPolynomial1D_Pointer1
  ALLOCATE( JacobiPolynomial1D_:: ans )
  CALL ans%Initiate( &
    & varname=varname,  &
    & n=n,  &
    & alpha=alpha,  &
    & beta=beta,  &
    & isMonic=isMonic, &
    & isOrthonormal=isOrthonormal )
END PROCEDURE JacobiPolynomial1D_Pointer1

!----------------------------------------------------------------------------
!                                                                Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE J_Deallocate
  CALL J_Deallocate2( obj )
  IF( ASSOCIATED( obj%Jn_1 ) ) THEN
    CALL J_Deallocate2( obj%Jn_1 )
  END IF
  CALL J_Deallocate1( obj )
END PROCEDURE J_Deallocate

!----------------------------------------------------------------------------
!                                                                 Deallocate1
!----------------------------------------------------------------------------

RECURSIVE SUBROUTINE J_Deallocate1( obj )
  CLASS( JacobiPolynomial1D_ ), INTENT( INOUT ) :: obj
  !!
  CALL AbstractBasis1DDeallocate( obj )
  obj%n = 0
  obj%an_1 = 0.0_DFP
  obj%bn_1 = 0.0_DFP
  obj%sn_1 = 1.0_DFP
  obj%sn_2 = 1.0_DFP
  obj%alpha = 0.0_DFP
  obj%beta = 0.0_DFP
  !!
  IF( ASSOCIATED( obj%Jn_1 ) ) THEN
    CALL J_Deallocate1(obj%Jn_1)
    DEALLOCATE(obj%Jn_1)
    obj%Jn_1 => NULL()
  END IF
  !!
END SUBROUTINE J_Deallocate1

!----------------------------------------------------------------------------
!                                                                 Deallocate1
!----------------------------------------------------------------------------

RECURSIVE SUBROUTINE J_Deallocate2( obj )
  CLASS( JacobiPolynomial1D_ ), INTENT( INOUT ) :: obj
  !!
  IF( ASSOCIATED( obj%Jn_2 ) ) THEN
    !!
    CALL J_Deallocate2( obj = obj%Jn_2 )
    NULLIFY( obj%Jn_2 )
    !!
  END IF
  !!
END SUBROUTINE J_Deallocate2


!----------------------------------------------------------------------------
!                                                                 Final
!----------------------------------------------------------------------------

MODULE PROCEDURE J_Final
  CALL obj%Deallocate()
END PROCEDURE J_Final

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE J_Initiate
  REAL( DFP ) :: a0( 0:n-1 )
  REAL( DFP ) :: b0( 0:n-1 )
  REAL( DFP ) :: s1( 0:n-1 )
  REAL( DFP ) :: s2( 0:n-1 )
  REAL( DFP ) :: r1, r2, r3, r4
  LOGICAL( LGT ) :: isMonic0
  LOGICAL( LGT ) :: isOrthonormal0
  INTEGER( I4B ) :: ii
  !!
  isMonic0 = INPUT( option=isMonic, default=.FALSE. )
  isOrthonormal0 = INPUT( option=isOrthonormal, default=.FALSE. )
  !!
  CALL obj%Deallocate()
  !!
  CALL GetJacobiRecurrenceCoeff( &
    & n=n, &
    & alpha=alpha, &
    & beta=beta, &
    & alphaCoeff=a0, &
    & betaCoeff=b0 )
  !!
  s1 = 1.0_DFP
  s2 = 1.0_DFP
  !!
  IF( .NOT. isMonic0 ) THEN
    !!
    r1 = 2.0_DFP+alpha+beta
    r2 = 1.0_DFP
    r3 = 2.0_DFP
    s1( 0 ) = r1*r2/r3
    s2( 0 ) = s1( 1 )
    !!
    DO ii = 1, n-1
      r1 = 2.0_DFP*ii+alpha+beta + 2.0_DFP
      r2 = r1 - 1.0_DFP
      r3 = 2.0_DFP * (ii+1.0) * (ii+alpha+beta+1.0_DFP)
      s1( ii ) = r1*r2/r3
      s2( ii ) = s1( ii ) * s1( ii-1 )
    END DO
    !!
    IF( isOrthonormal0 ) THEN
      !!
      IF( n .GT. 1 ) THEN
        r1 = alpha+beta+3.0_DFP
        r2 = 1.0_DFP
        r3 = (1.0 + alpha)*(1.0 + beta)
        s1( 1 ) = s1( 1 ) * SQRT(r1*r2/r3)
        s2( 1 ) = s1( 1 )
      END IF
      !!
      DO ii = 2, n-1
        r1 = 2.0_DFP*ii+alpha+beta + 1.0_DFP
        r2 = ii * (ii+alpha+beta)
        r3 = (r1-2.0_DFP)*(ii+beta)*(ii+alpha)
        r4 = r1*r2/r3
        s1( ii ) = s1( ii ) * SQRT(r4)
        !!
        r1 = r1 - 2.0_DFP
        r2 = (ii-1.0_DFP)*(ii+alpha+beta-1.0_DFP)
        r3 = (r1-2.0_DFP)*(ii+beta-1.0_DFP)*(ii+alpha-1.0_DFP)
        !!
        s2( ii ) = s2( ii ) * SQRT( r4 * r1*r2/r3 )
      END DO
      !!
    END IF
    !!
    !!
  ELSE
    !! TODO #113
  END IF
  !!
  CALL obj%Initiate1( &
    & n=n, &
    & varname=varname, &
    & alpha=alpha,  &
    & beta=beta, &
    & alphaCoeff=a0, &
    & betaCoeff=b0, &
    & s1=s1, &
    & s2=s2 )
  ! !!
  CALL obj%Initiate2( j1 = obj )
  CALL obj%Initiate2( j1 = obj%Jn_1 )
  ! !!
END PROCEDURE J_Initiate

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE J_Initiate1
  !!
  IF( n .LE. 0_I4B ) THEN
    !!
    CALL obj%SetParam(  &
      & n=n, &
      & alpha=alpha, &
      & beta=beta, &
      & an_1=0.0_DFP, &
      & bn_1=0.0_DFP, &
      & sn_1=1.0_DFP, &
      & sn_2=1.0_DFP, &
      & varname=varname )
    !!
    obj%Jn_1 => NULL()
    obj%Jn_2 => NULL()
  !!
  ELSEIF( n .EQ. 1_I4B ) THEN
    !!
    CALL obj%SetParam( &
      & n=1, &
      & alpha=alpha, &
      & beta=beta, &
      & an_1=alphaCoeff( 0 ), &
      & bn_1=betaCoeff( 0 ), &
      & sn_1=s1( 0 ), &
      & sn_2=s2( 0 ), &
      & varname=varname )
    !!
    ALLOCATE( obj%Jn_1 )
    !!
    CALL obj%Jn_1%Initiate1( &
      & varname=varname, &
      & n=0_I4B, &
      & alpha=alpha, &
      & beta=beta, &
      & alphaCoeff = alphaCoeff, &
      & betaCoeff = betaCoeff, &
      & s1 = s1, &
      & s2 = s2 )
    !!
  ELSE
    !!
    CALL obj%SetParam( &
      & n=n, &
      & alpha=alpha, &
      & beta=beta, &
      & an_1=alphaCoeff( n-1 ), &
      & bn_1=betaCoeff( n-1 ), &
      & sn_1=s1(n-1), &
      & sn_2=s2(n-1), &
      & varname=varname )
    !!
    ALLOCATE( obj%Jn_1 )
    !!
    CALL obj%Jn_1%Initiate1( &
      & varname=varname, &
      & n=n-1, &
      & alpha=alpha, &
      & beta=beta,  &
      & alphaCoeff = alphaCoeff, &
      & betaCoeff = betaCoeff, &
      & s1 = s1, &
      & s2 = s2 )
    !!
  END IF
  !!
END PROCEDURE J_Initiate1

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE J_Initiate2
  INTEGER( I4B ) :: n
  !!
  n = j1%n
  !!
  IF( n .LE. 1_I4B ) THEN
    RETURN
  ELSE
    !!
    j1%Jn_2 => j1%Jn_1%Jn_1
    !!
    CALL obj%Initiate2( j1 = j1%Jn_2 )
    !!
  END IF
  !!
END PROCEDURE J_Initiate2


!----------------------------------------------------------------------------
!                                                              isInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE J_isInitiated
  ans = .FALSE.
  IF( ASSOCIATED( obj%Jn_1 ) ) ans = .TRUE.
  IF( ASSOCIATED( obj%Jn_2 ) ) ans = .TRUE.
END PROCEDURE J_isInitiated

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods