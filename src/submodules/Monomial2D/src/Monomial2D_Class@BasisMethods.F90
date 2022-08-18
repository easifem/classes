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

SUBMODULE(Monomial2D_Class) BaseMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                               Monomials2D
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Monomials2D
  INTEGER( I4B ) :: n, ii
  INTEGER( I4B ), ALLOCATABLE :: degree( :, : )
  !!
  degree = LagrangeDegree( order=order, elemType=elemType )
  !!
  n = SIZE( degree, 1 )
  !!
  ALLOCATE( ans( n ) )
  !!
  DO ii = 1, n
    CALL ans( ii )%Initiate( n1=degree(ii,1), &
      & n2=degree(ii,2), name1=name1, name2=name2 )
  END DO
  !!
  IF(ALLOCATED(degree)) DEALLOCATE(degree)
  !!
END PROCEDURE func_Monomials2D

END SUBMODULE BaseMethods