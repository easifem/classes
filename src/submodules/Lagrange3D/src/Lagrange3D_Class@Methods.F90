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

SUBMODULE(Lagrange3D_Class) ConstructorMethods
USE BaseMethod
USE PolynomialFactory
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                     Final
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Final
CALL obj%Deallocate()
END PROCEDURE func_Final

!----------------------------------------------------------------------------
!                                             Lagrange3D@ConstructorMethods
!----------------------------------------------------------------------------

MODULE PROCEDURE Initiate1
REAL(DFP), ALLOCATABLE :: V(:, :)
REAL(DFP), ALLOCATABLE :: coeff(:)
INTEGER(I4B), ALLOCATABLE :: ipiv(:)
INTEGER(I4B), ALLOCATABLE :: degree(:, :)
INTEGER(I4B) :: n, info, nsd
!!
n = SIZE(x, 2); nsd = SIZE(x, 1)
ALLOCATE (V(n, n), coeff(n), ipiv(n), degree(n, nsd))
coeff = 0.0_DFP; coeff(i) = 1.0_DFP
V = LagrangeVandermonde(order=order, x=x, elemType=elemType)
degree = LagrangeDegree(order=order, elemType=elemType)
ipiv = 0_I4B
CALL GetLU(A=V, IPIV=ipiv, info=info)
CALL LUSolve(A=V, B=coeff, IPIV=ipiv, info=info)
!!
obj = Polynomial3D( &
  & coeff=coeff, &
  & degree=degree, &
  & varname1=varname1, &
  & varname2=varname2, &
  & varname3=varname3)
!!
END PROCEDURE Initiate1

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Initiate2
REAL(DFP), DIMENSION(SIZE(v, 1)) :: coeff
REAL(DFP), DIMENSION(SIZE(v, 1), SIZE(v, 2)) :: v0
INTEGER(I4B), DIMENSION(SIZE(v, 1)) :: ipiv
INTEGER(I4B), DIMENSION(SIZE(v, 1), 2) :: degree
INTEGER(I4B) :: info
!!
v0 = v; coeff = 0.0_DFP; coeff(i) = 1.0_DFP
degree = LagrangeDegree(order=order, elemType=elemType)
!!
ipiv = 0_I4B
CALL GetLU(A=v0, IPIV=ipiv, info=info)
CALL LUSolve(A=v0, B=coeff, IPIV=ipiv, info=info)
obj = Polynomial3D( &
  & coeff=coeff, &
  & degree=degree, &
  & varname1=varname1, &
  & varname2=varname2, &
  & varname3=varname3)
!!
END PROCEDURE Initiate2

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Initiate3
REAL(DFP), DIMENSION(SIZE(v, 1)) :: coeff
INTEGER(I4B), DIMENSION(SIZE(v, 1), 2) :: degree
INTEGER(I4B) :: info
!!
coeff = 0.0_DFP; coeff(i) = 1.0_DFP
degree = LagrangeDegree(order=order, elemType=elemType)
CALL LUSolve(A=v, B=coeff, IPIV=ipiv, info=info)
obj = Polynomial3D( &
  & coeff=coeff, &
  & degree=degree, &
  & varname1=varname1, &
  & varname3=varname3, &
  & varname2=varname2)
!!
END PROCEDURE Initiate3

!----------------------------------------------------------------------------
!                                             Lagrange3D@ConstructorMethods
!----------------------------------------------------------------------------

SUBROUTINE Initiate4(ans, x, order, varname1, varname2, varname3, elemType)
  TYPE(Lagrange3D_), ALLOCATABLE, INTENT(INOUT) :: ans(:)
  REAL(DFP), INTENT(IN) :: x(:, :)
  INTEGER(I4B), INTENT(IN) :: order
  CHARACTER(LEN=*), INTENT(IN) :: varname1
  CHARACTER(LEN=*), INTENT(IN) :: varname2
  CHARACTER(LEN=*), INTENT(IN) :: varname3
  INTEGER(I4B), INTENT(IN) :: elemType
  !! main
  REAL(DFP), ALLOCATABLE :: V(:, :)
  REAL(DFP), ALLOCATABLE :: coeff(:, :)
  INTEGER(I4B), ALLOCATABLE :: ipiv(:)
  INTEGER(I4B), ALLOCATABLE :: degree(:, :)
  INTEGER(I4B) :: n, info, ii, nsd
  !!
  degree = LagrangeDegree(order=order, elemType=elemType)
  n = SIZE(x, 2); nsd = SIZE(x, 1)
  ALLOCATE (V(n, n), coeff(n, n), ipiv(n), degree(n, nsd))
  coeff = eye(n)
  !!
  V = LagrangeVandermonde(order=order, x=x, elemType=elemType)
  !!
  ipiv = 0_I4B
  CALL GetLU(A=V, IPIV=ipiv, info=info)
  CALL LUSolve(A=V, B=coeff, IPIV=ipiv, info=info)
  ALLOCATE (ans(n))
  !!
  DO ii = 1, n
    ans(ii) = Polynomial3D( &
      & coeff=coeff(:, ii), &
      & degree=degree, &
      & varname1=varname1, &
      & varname3=varname3, &
      & varname2=varname2)
  END DO
  !!
END SUBROUTINE Initiate4

!----------------------------------------------------------------------------
!                                                                Lagrange3D
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Lagrange3D1
CALL Initiate1( &
  & obj=ans, &
  & i=i, &
  & x=x, &
  & order=order, &
  & varname1=varname1, &
  & varname2=varname2, &
  & varname3=varname3, &
  & elemType=elemType)
END PROCEDURE func_Lagrange3D1

!----------------------------------------------------------------------------
!                                                                 Lagrange3D
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Lagrange3D2
CALL Initiate2( &
  & obj=ans, &
  & i=i, &
  & v=v, &
  & order=order, &
  & varname1=varname1, &
  & varname2=varname2, &
  & varname3=varname3, &
  & elemType=elemType)
END PROCEDURE func_Lagrange3D2

!----------------------------------------------------------------------------
!                                                              Polynomial1D
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Lagrange3D3
CALL Initiate3( &
  & obj=ans, &
  & i=i, &
  & v=v, &
  & ipiv=ipiv, &
  & order=order, &
  & varname1=varname1, &
  & varname2=varname2, &
  & varname3=varname3, &
  & elemType=elemType)
END PROCEDURE func_Lagrange3D3

!----------------------------------------------------------------------------
!                                                                Lagrange3D
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Lagrange3D4
CALL Initiate4( &
  & ans=ans, &
  & x=x, &
  & order=order, &
  & varname1=varname1, &
  & varname2=varname2, &
  & varname3=varname3, &
  & elemType=elemType)
END PROCEDURE func_Lagrange3D4

END SUBMODULE ConstructorMethods
