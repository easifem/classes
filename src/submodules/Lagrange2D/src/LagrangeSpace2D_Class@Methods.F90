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

SUBMODULE(LagrangeSpace2D_Class) Methods
USE BaseMethod
USE PolynomialFactory
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Deallocate
INTEGER(I4B) :: ii, n
CALL AbstractPolynomialSpace2DDeallocate(obj)
IF (ALLOCATED(obj%x)) THEN
  n = SIZE(obj%x)
  DO ii = 1, n
    CALL obj%x(ii)%Deallocate()
  END DO
  DEALLOCATE (obj%x)
END IF
END PROCEDURE func_Deallocate

!----------------------------------------------------------------------------
!                                                                     Final
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Final
CALL obj%Deallocate()
END PROCEDURE func_Final

!----------------------------------------------------------------------------
!                                                            LagrangeSpace2D
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Initiate
INTEGER(I4B) :: ii, n, nsd
REAL(DFP), ALLOCATABLE :: V(:, :)
INTEGER(I4B), ALLOCATABLE :: ipiv(:), degree(:, :)
INTEGER(I4B) :: info
!!
nsd = SIZE(x, 1)
n = SIZE(x, 2)
ALLOCATE (V(n, n), ipiv(n), degree(n, 2))
ipiv = 0_I4B
degree = LagrangeDegree(order=order, elemType=elemType)
V = LagrangeVanderMonde(order=order, xij=x, elemType=elemType)
CALL GetLU(A=V, IPIV=ipiv, info=info)
CALL GETRI(A=V, IPIV=ipiv, info=info)
CALL obj%SetParam(n=order)
ALLOCATE (obj%x(n))
DO ii = 1, n
  obj%x(ii) = Polynomial2D( &
    & coeff=V(:, ii), &
    & degree=degree, &
    & varname2=varname2, &
    & varname1=varname1)
END DO
!!
IF (ALLOCATED(V)) DEALLOCATE (V)
IF (ALLOCATED(ipiv)) DEALLOCATE (ipiv)
IF (ALLOCATED(degree)) DEALLOCATE (degree)
END PROCEDURE func_Initiate

!----------------------------------------------------------------------------
!                                                            LagrangeSpace2D
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeSpace2D1
CALL ans%Initiate( &
  & order=order, &
  & x=x, &
  & varname1=varname1, &
  & varname2=varname2, &
  & elemType=elemType)
END PROCEDURE LagrangeSpace2D1

!----------------------------------------------------------------------------
!                                                            LagrangeSpace2D
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeSpace2D_Pointer1
ALLOCATE (ans)
CALL ans%Initiate( &
  & order=order, &
  & x=x, &
  & varname1=varname1, &
  & varname2=varname2, &
  & elemType=elemType)
END PROCEDURE LagrangeSpace2D_Pointer1

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Display
INTEGER(I4B) :: n, ii
  !!
CALL AbstractPolynomialSpace2DDisplay(obj=obj, msg="", unitno=unitno)
IF (ALLOCATED(obj%x)) THEN
  n = SIZE(obj%x)
  CALL Display(n, "Total Basis=", unitno=unitno)
  DO ii = 1, n
    CALL obj%x(ii)%Display( &
      & msg="N("//tostring(ii)//")=", &
      & unitno=unitno)
  END DO
END IF
END PROCEDURE func_Display

END SUBMODULE Methods
