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

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBMODULE(Lagrange2D_Class) ConstructorMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                     Final
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Final
  CALL obj%Deallocate()
END PROCEDURE func_Final

!----------------------------------------------------------------------------
!                                             Lagrange2D@ConstructorMethods
!----------------------------------------------------------------------------

SUBROUTINE Initiate1( ans, i, x, order, name1, name2, elemType)
  CLASS( Lagrange2D_ ), INTENT( INOUT ) :: ans
  INTEGER( I4B ), INTENT( IN ) :: i
  REAL( DFP ), INTENT( IN ) :: x( :, : )
  INTEGER( I4B ), INTENT( IN ) :: order
  CHARACTER( LEN = * ), INTENT( IN ) :: name1
  CHARACTER( LEN = * ), INTENT( IN ) :: name2
  INTEGER( I4B ), INTENT( IN ) :: elemType
  !!
  !! main
  !!
  REAL( DFP ), ALLOCATABLE :: V( :, : )
  REAL( DFP ), ALLOCATABLE :: coeff( : )
  INTEGER( I4B ), ALLOCATABLE :: ipiv( : )
  INTEGER( I4B ), ALLOCATABLE :: degree( :, : )
  INTEGER( I4B ) :: n, info
  !!
  n = SIZE( x, 2 )
  ALLOCATE( V(n, n), coeff( n ), ipiv( n ), degree(n,2) )
  coeff = 0.0_DFP; coeff( i ) = 1.0_DFP
  !!
  V = LagrangeVandermonde( order=order, x=x, elemType=elemType )
  !!
  degree = LagrangeDegree( order=order, elemType=elemType )
  !!
  ipiv = 0_I4B
  !!
  CALL GetLU( A=V, IPIV=ipiv, info=info )
  CALL LUSolve( A=V, B=coeff, IPIV=ipiv, info=info )
  !!
  ans = Polynomial2D( &
    & coeff=coeff, &
    & degree=degree, &
    & name1=name1, &
    & name2=name2 )
  !!
END SUBROUTINE Initiate1

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

SUBROUTINE Initiate2( ans, i, v, order, name1, name2, elemType)
  CLASS( Lagrange2D_ ), INTENT( INOUT ) :: ans
  INTEGER( I4B ), INTENT( IN ) :: i
  REAL( DFP ), INTENT( IN ) :: v( :, : )
  INTEGER( I4B ), INTENT( IN ) :: order
  CHARACTER( LEN = * ), INTENT( IN ) :: name1
  CHARACTER( LEN = * ), INTENT( IN ) :: name2
  INTEGER( I4B ), INTENT( IN ) :: elemType
  !!
  REAL( DFP ), DIMENSION( SIZE(v, 1) ) :: coeff
  REAL( DFP ), DIMENSION( SIZE(v, 1), SIZE(v,2) ) :: v0
  INTEGER( I4B ), DIMENSION( SIZE(v, 1) ) :: ipiv
  INTEGER( I4B ), DIMENSION( SIZE(v, 1), 2 ) :: degree
  INTEGER( I4B ) :: info
  !!
  v0 = v; coeff = 0.0_DFP; coeff( i ) = 1.0_DFP
  !!
  degree = LagrangeDegree( order=order, elemType=elemType )
  !!
  ipiv = 0_I4B
  CALL GetLU( A=v0, IPIV=ipiv, info=info )
  CALL LUSolve( A=v0, B=coeff, IPIV=ipiv, info=info )
  ans = Polynomial2D( &
    & coeff=coeff, &
    & degree=degree, &
    & name1=name1, &
    & name2=name2 )
  !!
END SUBROUTINE Initiate2

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

SUBROUTINE Initiate3( ans, i, v, ipiv, order, name1, name2, elemType)
  CLASS( Lagrange2D_ ), INTENT( INOUT ) :: ans
  INTEGER( I4B ), INTENT( IN ) :: i
  REAL( DFP ), INTENT( INOUT ) :: v( :, : )
  INTEGER( I4B ), INTENT( IN ) :: ipiv( : )
  INTEGER( I4B ), INTENT( IN ) :: order
  CHARACTER( LEN = * ), INTENT( IN ) :: name1
  CHARACTER( LEN = * ), INTENT( IN ) :: name2
  INTEGER( I4B ), INTENT( IN ) :: elemType
  !!
  REAL( DFP ), DIMENSION( SIZE(v, 1) ) :: coeff
  INTEGER( I4B ), DIMENSION( SIZE(v, 1), 2 ) :: degree
  INTEGER( I4B ) :: info
  !!
  coeff = 0.0_DFP; coeff( i ) = 1.0_DFP
  !!
  degree = LagrangeDegree( order=order, elemType=elemType )
  !!
  CALL LUSolve( A=v, B=coeff, IPIV=ipiv, info=info )
  ans = Polynomial2D( &
    & coeff=coeff, &
    & degree=degree, &
    & name1=name1, &
    & name2=name2 )
  !!
END SUBROUTINE Initiate3

!----------------------------------------------------------------------------
!                                             Lagrange2D@ConstructorMethods
!----------------------------------------------------------------------------

SUBROUTINE Initiate4( ans, x, order, name1, name2, elemType)
  TYPE( Lagrange2D_ ), ALLOCATABLE, INTENT( INOUT ) :: ans( : )
  REAL( DFP ), INTENT( IN ) :: x( :, : )
  INTEGER( I4B ), INTENT( IN ) :: order
  CHARACTER( LEN = * ), INTENT( IN ) :: name1
  CHARACTER( LEN = * ), INTENT( IN ) :: name2
  INTEGER( I4B ), INTENT( IN ) :: elemType
  !!
  !! main
  !!
  REAL( DFP ), ALLOCATABLE :: V( :, : )
  REAL( DFP ), ALLOCATABLE :: coeff( :, : )
  INTEGER( I4B ), ALLOCATABLE :: ipiv( : )
  INTEGER( I4B ), ALLOCATABLE :: degree( :, : )
  INTEGER( I4B ) :: n, info, ii
  !!
  degree = LagrangeDegree( order=order, elemType=elemType )
  n = SIZE( x, 2 )
  ALLOCATE( V(n, n), coeff( n, n ), ipiv( n ), degree(n,2) )
  coeff = eye( n )
  !!
  V = LagrangeVandermonde( order=order, x=x, elemType=elemType )
  !!
  ipiv = 0_I4B
  CALL GetLU( A=V, IPIV=ipiv, info=info )
  !!
  CALL LUSolve( A=V, B=coeff, IPIV=ipiv, info=info )
  !!
  ALLOCATE( ans( n ) )
  !!
  DO ii = 1, n
    ans(ii) = Polynomial2D( &
      & coeff=coeff(:,ii), &
      & degree=degree, &
      & name1=name1, &
      & name2=name2 )
  END DO
  !!
END SUBROUTINE Initiate4

!----------------------------------------------------------------------------
!                                                                Lagrange2D
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Lagrange2D1
  CALL Initiate1( ans=ans, i=i, x=x, order=order, name1=name1, &
    & name2=name2, elemType=elemType )
END PROCEDURE  func_Lagrange2D1

!----------------------------------------------------------------------------
!                                                                 Lagrange2D
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Lagrange2D2
  CALL Initiate2( ans=ans, i=i, v=v, order=order, name1=name1, &
    & name2=name2, elemType=elemType)
END PROCEDURE  func_Lagrange2D2

!----------------------------------------------------------------------------
!                                                              Polynomial1D
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Lagrange2D3
  CALL Initiate3( ans=ans, i=i, v=v, ipiv=ipiv, order=order, &
    & name1=name1, name2=name2, elemType=elemType)
END PROCEDURE  func_Lagrange2D3

!----------------------------------------------------------------------------
!                                                                Lagrange2D
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Lagrange2D4
  CALL Initiate4( ans=ans, x=x, order=order, name1=name1, &
    & name2=name2, elemType=elemType )
END PROCEDURE  func_Lagrange2D4

END SUBMODULE ConstructorMethods