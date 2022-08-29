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

SUBMODULE(LagrangeSpace1D_Class) Methods
USE BaseMethod
USE PolynomialFactory
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Deallocate
INTEGER(I4B) :: ii, n
CALL AbstractPolynomialSpace1DDeallocate(obj)
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
!                                                            LagrangeSpace1D
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Initiate
INTEGER(I4B) :: ii, n
REAL(DFP), ALLOCATABLE :: V(:, :)
INTEGER(I4B), ALLOCATABLE :: ipiv(:), degree(:)
INTEGER(I4B) :: info
!!
n = SIZE(x)
ALLOCATE (V(n, n), ipiv(n), degree(n))
degree = arange(0_I4B, order, 1_I4B)
ipiv = 0_I4B
V = VanderMondeMatrix(order=order, x=x)
CALL GetLU(A=V, IPIV=ipiv, info=info)
CALL GETRI(A=V, IPIV=ipiv, info=info)
!!
CALL obj%SetParam(n=order)
ALLOCATE (obj%x(n))
!!
DO ii = 1, n
  obj%x(ii) = Polynomial1D( &
    & coeff=V(:, ii), &
    & degree=degree, &
    & varname=varname)
END DO
!!
IF (ALLOCATED(V)) DEALLOCATE (V)
IF (ALLOCATED(ipiv)) DEALLOCATE (ipiv)
IF (ALLOCATED(degree)) DEALLOCATE (degree)
END PROCEDURE func_Initiate

!----------------------------------------------------------------------------
!                                                            LagrangeSpace1D
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeSpace1D1
CALL ans%Initiate(order=order, x=x, varname=varname)
END PROCEDURE LagrangeSpace1D1

!----------------------------------------------------------------------------
!                                                            LagrangeSpace1D
!----------------------------------------------------------------------------

MODULE PROCEDURE LagrangeSpace1D_Pointer1
ALLOCATE (ans)
CALL ans%Initiate(order=order, x=x, varname=varname)
END PROCEDURE LagrangeSpace1D_Pointer1

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Display
INTEGER(I4B) :: n, ii
  !!
CALL AbstractPolynomialSpace1DDisplay(obj=obj, msg="", unitno=unitno)
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
