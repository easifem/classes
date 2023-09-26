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

SUBMODULE(RefLine_Class) Methods
USE BaseMethod
USE RefPoint_Class
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                  RefCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_RefCoord
ans = RefCoord_Line("BIUNIT")
END PROCEDURE refelem_RefCoord

!----------------------------------------------------------------------------
!                                                                    GetName
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetName
ans = Line2
END PROCEDURE refelem_GetName

!----------------------------------------------------------------------------
!                                                           GetFacetElements
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetFacetElements
INTEGER(I4B), PARAMETER :: tFacet = 2_I4B
INTEGER(I4B) :: ii
TYPE(string) :: baseContinuity0, baseInterpolation0
REAL(DFP), ALLOCATABLE :: xij(:, :)
ALLOCATE (ans(tFacet))
CALL obj%getParam( &
& baseInterpolation=baseInterpolation0, &
& baseContinuity=baseContinuity0, &
& xij=xij)
DO ii = 1, tFacet
  ALLOCATE (RefPoint_ :: ans(ii)%ptr)
  CALL ans(ii)%ptr%Initiate( &
  & nsd=obj%getNSD(),  &
  & baseContinuity=baseContinuity0%chars(),  &
  & baseInterpolation=baseInterpolation0%chars(), &
  & xij=xij(:, ii:ii))
END DO
END PROCEDURE refelem_GetFacetElements

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
