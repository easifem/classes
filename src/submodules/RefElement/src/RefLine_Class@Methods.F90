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
!                                                           GenerateTopology
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GenerateTopology
INTEGER(I4B), PARAMETER :: np = 2_I4B, ne = 1_I4B
!!
ALLOCATE (obj%pointTopology(np))
ALLOCATE (obj%edgeTopology(ne))
!!
CALL obj%pointTopology(1)%Initiate(&
  & nptrs=[1_I4B], &
  & name=Point, &
  & xidimension=0_I4B)
CALL obj%pointTopology(2)%Initiate( &
  & nptrs=[2_I4B], &
  & name=Point, &
  & xidimension=0_I4B)
!!
CALL obj%edgeTopology(1)%Initiate(nptrs=[1_I4B, 2_I4B], &
  & name=Line2, xidimension=1_I4B)
!!
END PROCEDURE refelem_GenerateTopology

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
