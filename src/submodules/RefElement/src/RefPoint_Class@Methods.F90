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

SUBMODULE(RefPoint_Class) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                  RefCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_RefCoord
CALL Reallocate(ans, 3_I4B, 1_I4B)
ans = 0.0_DFP
END PROCEDURE refelem_RefCoord

!----------------------------------------------------------------------------
!                                                                    GetName
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetName
ans = Point1
END PROCEDURE refelem_GetName

!----------------------------------------------------------------------------
!                                                           GetFacetElements
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetFacetElements
ALLOCATE (ans(0))
END PROCEDURE refelem_GetFacetElements

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
