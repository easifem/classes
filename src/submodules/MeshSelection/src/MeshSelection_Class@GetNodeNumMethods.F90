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

SUBMODULE(MeshSelection_Class) GetnodeNumMethods
USE BaseMethod
USE Mesh_Class, ONLY: Mesh_
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                GetnodeNum
!----------------------------------------------------------------------------

MODULE PROCEDURE meshSelect_GetnodeNum1
IF (isAllocated(obj%nodeNum)) THEN
  CALL Reallocate(ans, SIZE(obj%nodeNum))
  ans = obj%nodeNum
ELSE
  CALL reallocate(ans, 0_I4B)
END IF
END PROCEDURE meshSelect_GetnodeNum1

!----------------------------------------------------------------------------
!                                                                GetnodeNum
!----------------------------------------------------------------------------

MODULE PROCEDURE meshSelect_GetnodeNum2
CHARACTER(*), PARAMETER :: myName = "meshSelect_GetnodeNum2()"
TYPE(IntVector_) :: aintvec
INTEGER(I4B), ALLOCATABLE :: indx(:)
INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] GetnodeNum()')
#endif

! isSelectionBynodeNum
IF (obj%isSelectionBynodeNum) THEN
  CALL APPEND(aintvec, obj%GetnodeNum())
END IF

! isSelectionByMeshID
IF (obj%isSelectionByMeshID) THEN
  IF (obj%isMeshIDAllocated(dim=dim)) THEN
    indx = obj%GetMeshID(dim=dim)
    CALL APPEND( &
      & aintvec, &
      & domain%GetNptrs(dim=dim, entityNum=indx))
    IF (ALLOCATED(indx)) DEALLOCATE (indx)
  END IF
END IF

! isSelectionByElemNum
IF (obj%isSelectionByElemNum) THEN
  indx = obj%GetElemNum(dim=dim)
  DO ii = 1, SIZE(indx)
    CALL APPEND( &
      & aintvec, &
      & domain%GetConnectivity(globalElement=indx(ii)))
  END DO
  IF (ALLOCATED(indx)) DEALLOCATE (indx)
END IF

IF (isAllocated(aIntVec)) THEN
  CALL RemoveDuplicates(aIntVec)
  CALL Reallocate(ans, SIZE(aintvec))
  ans = aIntVec
  CALL DEALLOCATE (aIntVec)
ELSE
  CALL Reallocate(ans, 0_I4B)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] GetnodeNum()')
#endif
END PROCEDURE meshSelect_GetnodeNum2

!----------------------------------------------------------------------------
!                                                                GetnodeNum
!----------------------------------------------------------------------------

MODULE PROCEDURE meshSelect_GetnodeNum3
CHARACTER(*), PARAMETER :: myName = "meshSelect_GetnodeNum3()"
TYPE(IntVector_) :: aintvec
INTEGER(I4B), ALLOCATABLE :: indx(:), nptrs(:)
INTEGER(I4B) :: ii, dim, nsd

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START]')
#endif

! isSelectionBynodeNum
IF (obj%isSelectionBynodeNum) THEN
  nptrs = obj%GetnodeNum()
  IF (ALLOCATED(nptrs)) THEN
    CALL APPEND(aintvec, nptrs)
    DEALLOCATE (nptrs)
  END IF
END IF

! isSelectionByMeshID
IF (obj%isSelectionByMeshID) THEN
  nsd = domain%GetNSD()
  DO dim = 0, nsd
    IF (obj%isMeshIDAllocated(dim=dim)) THEN
      indx = obj%GetMeshID(dim=dim)
      nptrs = domain%GetNptrs(dim=dim, entityNum=indx)
      IF (ALLOCATED(nptrs)) THEN
        CALL APPEND(aintvec, nptrs)
        DEALLOCATE (nptrs)
      END IF
      IF (ALLOCATED(indx)) DEALLOCATE (indx)
    END IF
  END DO
END IF

! isSelectionByElemNum
IF (obj%isSelectionByElemNum) THEN
  indx = obj%GetElemNum()
  DO ii = 1, SIZE(indx)
    CALL APPEND( &
      & aintvec, &
      & domain%GetConnectivity(globalElement=indx(ii)))
  END DO
  IF (ALLOCATED(indx)) DEALLOCATE (indx)
END IF

IF (isAllocated(aIntVec)) THEN
  CALL RemoveDuplicates(aIntVec)
  CALL Reallocate(ans, SIZE(aIntVec))
  ans = aIntVec
  CALL DEALLOCATE (aIntVec)
ELSE
  CALL Reallocate(ans, 0_I4B)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END]')
#endif
END PROCEDURE meshSelect_GetnodeNum3

END SUBMODULE GetnodeNumMethods
