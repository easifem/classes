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

SUBMODULE(Monomial3D_Class) BaseMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                               Monomials3D
!----------------------------------------------------------------------------

MODULE PROCEDURE func_Monomials3D
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
    CALL ans( ii )%Initiate( &
      & n1=degree(ii,1), &
      & n2=degree(ii,2), &
      & n3=degree(ii,3), &
      & varname1=varname1, &
      & varname2=varname2, &
      & varname3=varname3 )
  END DO
  !!
  IF(ALLOCATED(degree)) DEALLOCATE(degree)
  !!
END PROCEDURE func_Monomials3D

END SUBMODULE BaseMethods