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
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_Initiate
REAL(DFP) :: xij0(3, 2)
INTEGER(I4B) :: entityCounts(4), xidimension, name
TYPE(String) :: nameStr
TYPE(Topology_) :: topology(3)
  !!
xij0 = 0.0_DFP
xij0(1, 1) = -1.0_DFP
xij0(1, 2) = 1.0_DFP
  !!
entityCounts = [2, 1, 0, 0]
xidimension = 1
name = Line2
nameStr = "Line2"
  !!
topology = obj%GetTopology()
  !!
CALL obj%SetParam( &
  & xij=xij0, &
  & entityCounts=entityCounts, &
  & nsd=nsd, &
  & xidimension=xidimension, &
  & name=name, &
  & nameStr=nameStr%chars(), &
  & topology=topology)
  !!
END PROCEDURE refelem_Initiate

!----------------------------------------------------------------------------
!                                                           GetFacetElements
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetFacetElements
INTEGER(I4B), PARAMETER :: tFacet = 2_I4B
INTEGER(I4B) :: ii
  !!
ALLOCATE (ans(tFacet))
  !!
DO ii = 1, tFacet
  ALLOCATE (RefPoint_ :: ans(ii)%ptr)
  CALL ans(ii)%ptr%Initiate(nsd=obj%getNSD())
END DO
  !!
END PROCEDURE refelem_GetFacetElements

!----------------------------------------------------------------------------
!                                                           GetFacetTopology
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetFacetTopology
INTEGER(I4B), PARAMETER :: n = 2_I4B
INTEGER(I4B), ALLOCATABLE :: nptrs(:)
INTEGER(I4B) :: ii
  !!
ALLOCATE (ans(n))
  !!
nptrs = obj%getNptrs()
  !!
DO ii = 1, n
  CALL ans(ii)%Initiate(nptrs=nptrs(ii:ii), name=Point, &
    & xidimension=0_I4B)
END DO
  !!
END PROCEDURE refelem_GetFacetTopology

!----------------------------------------------------------------------------
!                                                                GetTopology
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetTopology
INTEGER(I4B), PARAMETER :: n = 3_I4B
  !!
ALLOCATE (ans(n))
  !!
  !! point
  !!
CALL ans(1)%Initiate(nptrs=[1_I4B], name=Point, &
  & xidimension=0_I4B)
CALL ans(2)%Initiate(nptrs=[2_I4B], name=Point, &
  & xidimension=0_I4B)
CALL ans(3)%Initiate(nptrs=[1_I4B, 2_I4B], &
  & name=Line2, xidimension=1_I4B)
  !!
END PROCEDURE refelem_GetTopology

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
