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

MODULE PROCEDURE Initiate1
REAL(DFP) :: coeff(SIZE(xij, 2))
INTEGER(I4B) :: degree(SIZE(xij, 2), 2)
!!
degree = LagrangeDegree(order=order, elemType=elemType)
coeff = LagrangeCoeff(order=order, elemType=elemType, i=i, xij=xij)
!!
obj = Polynomial2D( &
  & coeff=coeff, &
  & degree=degree, &
  & varname1=varname1, &
  & varname2=varname2)
!!
END PROCEDURE Initiate1

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Initiate2
REAL(DFP), DIMENSION(SIZE(v, 1)) :: coeff
INTEGER(I4B), DIMENSION(SIZE(v, 1), 2) :: degree
!!
degree = LagrangeDegree(order=order, elemType=elemType)
coeff = LagrangeCoeff(order=order, elemType=elemType, i=i, v=v, &
  & isVandermonde=.TRUE.)
obj = Polynomial2D( &
  & coeff=coeff, &
  & degree=degree, &
  & varname1=varname1, &
  & varname2=varname2)
END PROCEDURE Initiate2

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE Initiate3
REAL(DFP), DIMENSION(SIZE(v, 1)) :: coeff
INTEGER(I4B), DIMENSION(SIZE(v, 1), 2) :: degree
!!
degree = LagrangeDegree(order=order, elemType=elemType)
coeff = LagrangeCoeff(order=order, elemType=elemType, i=i, v=v, ipiv=ipiv)
obj = Polynomial2D( &
  & coeff=coeff, &
  & degree=degree, &
  & varname1=varname1, &
  & varname2=varname2)
END PROCEDURE Initiate3

!----------------------------------------------------------------------------
!                                             Lagrange2D@ConstructorMethods
!----------------------------------------------------------------------------

MODULE PROCEDURE Initiate4
REAL(DFP) :: coeff(SIZE(xij, 2), SIZE(xij, 2))
INTEGER(I4B) :: degree(SIZE(xij, 2), 2), n, ii
!!
n = SIZE(xij, 2)
degree = LagrangeDegree(order=order, elemType=elemType)
coeff = LagrangeCoeff(order=order, elemType=elemType, xij=xij)
!!
DO ii = 1, n
  obj(ii) = Polynomial2D( &
    & coeff=coeff(:, ii), &
    & degree=degree, &
    & varname1=varname1, &
    & varname2=varname2)
END DO
!!
END PROCEDURE Initiate4

!----------------------------------------------------------------------------
!                                                                Lagrange2D
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Lagrange2D1
CALL ans%Initiate1(i=i, xij=xij, order=order, varname1=varname1, &
  & varname2=varname2, elemType=elemType)
END PROCEDURE func_Lagrange2D1

!----------------------------------------------------------------------------
!                                                                 Lagrange2D
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Lagrange2D2
CALL ans%Initiate2(i=i, v=v, order=order, varname1=varname1, &
  & varname2=varname2, elemType=elemType, isVandermonde=.TRUE.)
END PROCEDURE func_Lagrange2D2

!----------------------------------------------------------------------------
!                                                              Polynomial1D
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Lagrange2D3
CALL ans%Initiate3(i=i, v=v, ipiv=ipiv, order=order, &
  & varname1=varname1, varname2=varname2, elemType=elemType)
END PROCEDURE func_Lagrange2D3

!----------------------------------------------------------------------------
!                                                                Lagrange2D
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Lagrange2D4
CALL Initiate4(obj=ans, xij=xij, order=order, varname1=varname1, &
  & varname2=varname2, elemType=elemType)
END PROCEDURE func_Lagrange2D4

!----------------------------------------------------------------------------
!                                                                Lagrange2D
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Lagrange2P1
ALLOCATE (ans)
CALL ans%Initiate1(i=i, xij=xij, order=order, varname1=varname1, &
  & varname2=varname2, elemType=elemType)
END PROCEDURE func_Lagrange2P1

!----------------------------------------------------------------------------
!                                                                 Lagrange2D
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Lagrange2P2
ALLOCATE (ans)
CALL ans%Initiate2(i=i, v=v, order=order, varname1=varname1, &
  & varname2=varname2, elemType=elemType, isVandermonde=.TRUE.)
END PROCEDURE func_Lagrange2P2

!----------------------------------------------------------------------------
!                                                              Polynomial1D
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Lagrange2P3
ALLOCATE (ans)
CALL ans%Initiate3(i=i, v=v, ipiv=ipiv, order=order, &
  & varname1=varname1, varname2=varname2, elemType=elemType)
END PROCEDURE func_Lagrange2P3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods
