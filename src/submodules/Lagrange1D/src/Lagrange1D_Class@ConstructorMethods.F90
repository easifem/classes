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

SUBMODULE(Lagrange1D_Class) ConstructorMethods
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
!                                             Lagrange1D@ConstructorMethods
!----------------------------------------------------------------------------

SUBROUTINE Initiate1( ans, i, x, varname )
  CLASS( Lagrange1D_ ), INTENT( INOUT ) :: ans
  INTEGER( I4B ), INTENT( IN ) :: i
  !! ith lagrange polynomial
  REAL( DFP ), INTENT( IN ) :: x( : )
  !! points, order = size(x) - 1
  CHARACTER( LEN = * ), INTENT( IN ) :: varname
  !! variable name
  REAL( DFP ), DIMENSION( SIZE(x), SIZE(x) ) :: V
  REAL( DFP ), DIMENSION( SIZE(x) ) :: coeff
  INTEGER( I4B ) :: order, info, ipiv( SIZE( x ) ), &
    & degree( SIZE( x ) )
  !!
  !! vandermonde matrix
  !!
  order = SIZE(x)-1_I4B
  degree = arange(0_I4B, order, 1_I4B)
  ipiv = 0_I4B
  V = VanderMondeMatrix( order=order, x=x )
  CALL GetLU( A=V, IPIV=ipiv, info=info)
  coeff = 0.0_DFP
  coeff( i ) = 1.0_DFP
  CALL LUSolve( A=V, B=coeff, IPIV=ipiv, info=info)
  !!
  ans = Polynomial1D(coeff=coeff, degree=degree, varname=varname)
  !!
END SUBROUTINE Initiate1

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

SUBROUTINE Initiate2( ans, i, v, varname )
  CLASS( Lagrange1D_ ), INTENT( INOUT ) :: ans
  !!
  INTEGER( I4B ), INTENT( IN ) :: i
  !! ith lagrange polynomial
  REAL( DFP ), INTENT( IN ) :: v( :, : )
  !! Vandermonde matrix
  CHARACTER( LEN = * ), INTENT( IN ) :: varname
  !! variable name
  REAL( DFP ), DIMENSION( SIZE(v, 1) ) :: coeff
  REAL( DFP ), DIMENSION( SIZE(v, 1), SIZE(v,2) ) :: v0
  INTEGER( I4B ), DIMENSION( SIZE(v, 1) ) :: ipiv, degree
  INTEGER( I4B ) :: order, info
  !!
  v0 = v
  order = SIZE(degree)-1_I4B
  degree = arange(0_I4B, order, 1_I4B)
  ipiv = 0_I4B
  CALL getLU( A=v0, IPIV=ipiv, info=info )
  coeff = 0.0_DFP
  coeff( i ) = 1.0_DFP
  CALL LUSolve( A=v0, B=coeff, IPIV=ipiv, info=info )
  ans = Polynomial1D(coeff=coeff, degree=degree, varname=varname)
  !!
END SUBROUTINE Initiate2

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

SUBROUTINE Initiate3( ans, i, v, ipiv, varname)
  CLASS( Lagrange1D_ ), INTENT( INOUT ) :: ans
  INTEGER( I4B ), INTENT( IN ) :: i
  !! ith lagrange polynomial
  REAL( DFP ), INTENT( INOUT ) :: v( :, : )
  !! LU decomposition of Vandermonde matrix
  INTEGER( I4B ), INTENT( IN ) :: ipiv( : )
  !! inverse pivoting mapping, compes from LU decomposition
  CHARACTER( LEN = * ), INTENT( IN ) :: varname
  !! variable name
  REAL( DFP ), DIMENSION( SIZE(v, 1) ) :: coeff
  INTEGER( I4B ), DIMENSION( SIZE(v, 1) ) :: degree
  INTEGER( I4B ) :: order, info
  !!
  order = SIZE(degree)-1_I4B
  degree = arange(0_I4B, order, 1_I4B)
  coeff = 0.0_DFP; coeff( i ) = 1.0_DFP
  CALL LUSolve( A=v, B=coeff, IPIV=ipiv, info=info )
  ans = Polynomial1D(coeff=coeff, degree=degree, varname=varname)
  !!
END SUBROUTINE Initiate3

!----------------------------------------------------------------------------
!                                                                Lagrange1D
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Lagrange1D1
  CALL Initiate1( ans=ans, i=i, x=x, varname=varname )
END PROCEDURE  func_Lagrange1D1

!----------------------------------------------------------------------------
!                                                                 Lagrange1D
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Lagrange1D2
  CALL Initiate2( ans=ans, i=i, v=v, varname=varname)
END PROCEDURE  func_Lagrange1D2

!----------------------------------------------------------------------------
!                                                              Polynomial1D
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Lagrange1D3
  CALL Initiate3(ans=ans, i=i, v=v, ipiv=ipiv, varname=varname)
END PROCEDURE  func_Lagrange1D3

!----------------------------------------------------------------------------
!                                                                Lagrange1D
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Lagrange1D_P1
  ALLOCATE( ans )
  CALL Initiate1( ans=ans, i=i, x=x, varname=varname )
END PROCEDURE  func_Lagrange1D_P1

!----------------------------------------------------------------------------
!                                                                 Lagrange1D
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Lagrange1D_P2
  ALLOCATE( ans )
  CALL Initiate2( ans=ans, i=i, v=v, varname=varname)
END PROCEDURE  func_Lagrange1D_P2

!----------------------------------------------------------------------------
!                                                              Polynomial1D
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Lagrange1D_P3
  ALLOCATE( ans )
  CALL Initiate3(ans=ans, i=i, v=v, ipiv=ipiv, varname=varname)
END PROCEDURE  func_Lagrange1D_P3

END SUBMODULE ConstructorMethods