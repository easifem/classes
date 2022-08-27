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

SUBMODULE(RefPyramid_Class) Methods
USE BaseMethod
USE RefElementFactory
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 GetName
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetName
ans = Pyramid5
END PROCEDURE refelem_GetName

!----------------------------------------------------------------------------
!                                                           GetFacetElements
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GetFacetElements
INTEGER(I4B), PARAMETER :: n = 5_I4B
INTEGER(I4B) :: ii
!!
ALLOCATE (ans(n))
!!
ALLOCATE (RefQuadrangle_ :: ans(1)%ptr)
CALL ans(1)%ptr%Initiate(nsd=obj%getNSD())
!!
DO ii = 2, 5
  ALLOCATE (RefTriangle_ :: ans(ii)%ptr)
  CALL ans(ii)%ptr%Initiate(nsd=obj%getNSD())
END DO
!!
END PROCEDURE refelem_GetFacetElements

!----------------------------------------------------------------------------
!                                                           GenerateTopology
!----------------------------------------------------------------------------

MODULE PROCEDURE refelem_GenerateTopology
INTEGER(I4B), PARAMETER :: np = 5_I4B
INTEGER(I4B), PARAMETER :: ne = 8_I4B
INTEGER(I4B), PARAMETER :: nf = 5_I4B
INTEGER(I4B), PARAMETER :: nc = 1_I4B
INTEGER(I4B), PARAMETER :: nptrs(np) = [1, 2, 3, 4, 5]
INTEGER(I4B) :: edges(2, ne)
INTEGER(I4B) :: faces(4, nf), faceHelp(2, nf)
INTEGER(I4B) :: ii
!!
ALLOCATE (obj%pointTopology(np))
ALLOCATE (obj%edgeTopology(ne))
ALLOCATE (obj%faceTopology(nf))
ALLOCATE (obj%cellTopology(nc))
!!
!! point
!!
DO ii = 1, np
  CALL obj%pointTopology(ii)%Initiate( &
    & nptrs=[ii], &
    & name=Point, &
    & xidimension=0_I4B)
END DO
!!
!! Lines = 8
!!
edges(:, 1) = [1, 2]
edges(:, 2) = [1, 4]
edges(:, 3) = [1, 5]
edges(:, 4) = [2, 3]
edges(:, 5) = [2, 5]
edges(:, 6) = [3, 4]
edges(:, 7) = [3, 5]
edges(:, 8) = [4, 5]
!!
DO ii = 1, ne
  CALL obj%edgeTopology(ii)%Initiate( &
    & nptrs=edges(:, ii), &
    & name=Line2, &
    & xidimension=1_I4B)
END DO
!!
!! face
!!
faces(:, 1) = [1, 4, 3, 2]; faceHelp(:, 1) = [4, Quadrangle4]
faces(:, 2) = [2, 3, 5, 0]; faceHelp(:, 2) = [3, Triangle3]
faces(:, 3) = [3, 4, 5, 0]; faceHelp(:, 3) = [3, Triangle3]
faces(:, 4) = [1, 5, 4, 0]; faceHelp(:, 4) = [3, Triangle3]
faces(:, 5) = [1, 2, 5, 0]; faceHelp(:, 5) = [3, Triangle3]
!!
DO ii = 1, nf
  CALL obj%faceTopology(ii)%Initiate( &
    & nptrs=faces(1:faceHelp(1, ii), ii), &
    & name=faceHelp(2, ii), &
    & xidimension=2_I4B)
END DO
!!
!! cell
!!
CALL obj%cellTopology(1)%Initiate( &
  & nptrs=nptrs, &
  & name=Pyramid5, &
  & xidimension=3_I4B)
!!
END PROCEDURE refelem_GenerateTopology

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
