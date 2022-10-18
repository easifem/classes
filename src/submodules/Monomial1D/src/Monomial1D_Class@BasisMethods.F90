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

SUBMODULE(Monomial1D_Class) BasisMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                               Monomials1D
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Monomials1D
INTEGER(I4B) :: ii
ALLOCATE (ans(order + 1))
DO ii = 1, order + 1
  CALL ans(ii)%Initiate(degree=ii - 1_I4B, varname=varname)
END DO
END PROCEDURE func_Monomials1D

!----------------------------------------------------------------------------
!                                                           EvenMonomials1D
!----------------------------------------------------------------------------

MODULE PROCEDURE func_EvenMonomials1D
INTEGER(I4B) :: ii, order0, jj
  !!
order0 = order / 2
ALLOCATE (ans(order0 + 1))
  !!
DO ii = 1, order0 + 1
  jj = 2 * (ii - 1_I4B)
  CALL ans(ii)%Initiate(degree=jj, varname=varname)
END DO
END PROCEDURE func_EvenMonomials1D

!----------------------------------------------------------------------------
!                                                           OddMonomials1D
!----------------------------------------------------------------------------

MODULE PROCEDURE func_OddMonomials1D
INTEGER(I4B) :: ii, order0, jj
  !!
order0 = (order + 1) / 2
ALLOCATE (ans(order0))
  !!
DO ii = 1, order0
  jj = 2 * ii - 1_I4B
  CALL ans(ii)%Initiate(degree=jj, varname=varname)
END DO
END PROCEDURE func_OddMonomials1D

END SUBMODULE BasisMethods
