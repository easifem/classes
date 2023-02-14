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

!> authors: Vikas Sharma, Ph. D.
! date: 28 Aug 2021
! summary: This module defines a data type for mesh selection

SUBMODULE(MeshSelection_Class) GetNodeNumMethods
USE BaseMethod
USE Mesh_Class, ONLY: Mesh_
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                getNodeNum
!----------------------------------------------------------------------------

MODULE PROCEDURE meshSelect_getNodeNum1
IF (isAllocated(obj%NodeNum)) ans = obj%NodeNum
END PROCEDURE meshSelect_getNodeNum1

!----------------------------------------------------------------------------
!                                                                getNodeNum
!----------------------------------------------------------------------------

MODULE PROCEDURE meshSelect_getNodeNum2
TYPE(IntVector_) :: aintvec
INTEGER(I4B), ALLOCATABLE :: indx(:)
INTEGER(I4B) :: ii
  !!
  !! isSelectionByNodeNum
  !!
IF (obj%isSelectionByNodeNum) THEN
  CALL APPEND(aintvec, obj%getNodeNum())
END IF
  !!
  !! isSelectionByMeshID
  !!
IF (obj%isSelectionByMeshID) THEN
    !!
  IF (obj%isMeshIDAllocated(dim=dim)) THEN
      !!
    indx = obj%getMeshID(dim=dim)
      !!
    CALL APPEND( &
      & aintvec, &
      & domain%getNptrs(dim=dim, entityNum=indx))
      !!
    IF (ALLOCATED(indx)) DEALLOCATE (indx)
      !!
  END IF
    !!
END IF
  !!
  !! isSelectionByElemNum
  !!
IF (obj%isSelectionByElemNum) THEN
    !!
  indx = obj%getElemNum(dim=dim)
    !!
  DO ii = 1, SIZE(indx)
      !!
    CALL APPEND( &
      & aintvec, &
      & domain%getConnectivity(globalElement=indx(ii)))
      !!
  END DO
    !!
  IF (ALLOCATED(indx)) DEALLOCATE (indx)
    !!
END IF
  !!
IF (isAllocated(aIntVec)) THEN
  CALL RemoveDuplicates(aIntVec)
  ans = aIntVec
  CALL DEALLOCATE (aIntVec)
END IF
  !!
END PROCEDURE meshSelect_getNodeNum2

!----------------------------------------------------------------------------
!                                                                getNodeNum
!----------------------------------------------------------------------------

MODULE PROCEDURE meshSelect_getNodeNum3
CHARACTER(LEN=*), PARAMETER :: myName = "meshSelect_getNodeNum3"
TYPE(IntVector_) :: aintvec
INTEGER(I4B), ALLOCATABLE :: indx(:), nptrs(:)
INTEGER(I4B) :: ii, dim, nsd
  !!
  !! isSelectionByNodeNum
  !!
IF (obj%isSelectionByNodeNum) THEN
  nptrs = obj%getNodeNum()
  IF (ALLOCATED(nptrs)) THEN
    CALL APPEND(aintvec, nptrs)
    DEALLOCATE (nptrs)
  END IF
END IF
  !!
  !! isSelectionByMeshID
  !!
IF (obj%isSelectionByMeshID) THEN
    !!
  nsd = domain%getNSD()
    !!
  DO dim = 0, nsd
      !!
    IF (obj%isMeshIDAllocated(dim=dim)) THEN
        !!
      indx = obj%getMeshID(dim=dim)
        !!
      nptrs = domain%getNptrs(dim=dim, entityNum=indx)
      IF (ALLOCATED(nptrs)) THEN
        CALL APPEND(aintvec, nptrs)
        DEALLOCATE (nptrs)
      END IF
        !!
      IF (ALLOCATED(indx)) DEALLOCATE (indx)
        !!
    END IF
      !!
  END DO
    !!
END IF
  !!
  !! isSelectionByElemNum
  !!
IF (obj%isSelectionByElemNum) THEN
    !!
  indx = obj%getElemNum()
    !!
  DO ii = 1, SIZE(indx)
      !!
    CALL APPEND( &
      & aintvec, &
      & domain%getConnectivity(globalElement=indx(ii)))
      !!
  END DO
    !!
  IF (ALLOCATED(indx)) DEALLOCATE (indx)
    !!
END IF
  !!
IF (isAllocated(aIntVec)) THEN
  CALL RemoveDuplicates(aIntVec)
  ans = aIntVec
  CALL DEALLOCATE (aIntVec)
END IF
  !!
END PROCEDURE meshSelect_getNodeNum3

END SUBMODULE GetNodeNumMethods
