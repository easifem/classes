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

SUBMODULE(MeshSelection_Class) GetMethods
USE IntVector_Method, ONLY: isAllocated, ASSIGNMENT(=), GetPointer, Size

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 GetMeshID
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMeshID
SELECT CASE (dim)
CASE (0)
  IF (isAllocated(obj%pointMeshID)) ans = obj%pointMeshID
CASE (1)
  IF (isAllocated(obj%curveMeshID)) ans = obj%curveMeshID
CASE (2)
  IF (isAllocated(obj%surfaceMeshID)) ans = obj%surfaceMeshID
CASE (3)
  IF (isAllocated(obj%volumeMeshID)) ans = obj%volumeMeshID
END SELECT
IF (.NOT. ALLOCATED(ans)) ALLOCATE (ans(0))
END PROCEDURE obj_GetMeshID

!----------------------------------------------------------------------------
!                                                         GetMeshIDPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMeshIDPointer
SELECT CASE (dim)
CASE (0)
  tsize = SIZE(obj%pointMeshID)
  ans => GetPointer(obj%pointMeshID, tsize)

CASE (1)
  tsize = SIZE(obj%curveMeshID)
  ans => GetPointer(obj%curveMeshID, tsize)

CASE (2)
  tsize = SIZE(obj%surfaceMeshID)
  ans => GetPointer(obj%surfaceMeshID, tsize)

CASE (3)
  tsize = SIZE(obj%volumeMeshID)
  ans => GetPointer(obj%volumeMeshID, tsize)

END SELECT

END PROCEDURE obj_GetMeshIDPointer

!----------------------------------------------------------------------------
!                                                         isMeshIDAllocated
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isMeshIDAllocated
SELECT CASE (dim)
CASE (0)
  ans = isAllocated(obj%pointMeshID)
CASE (1)
  ans = isAllocated(obj%curveMeshID)
CASE (2)
  ans = isAllocated(obj%surfaceMeshID)
CASE (3)
  ans = isAllocated(obj%volumeMeshID)
CASE DEFAULT
  ans = .FALSE.
END SELECT
END PROCEDURE obj_isMeshIDAllocated

!----------------------------------------------------------------------------
!                                                         isElemNumAllocated
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isElemNumAllocated
SELECT CASE (dim)
CASE (0)
  ans = isAllocated(obj%pointElemNum)
CASE (1)
  ans = isAllocated(obj%curveElemNum)
CASE (2)
  ans = isAllocated(obj%surfaceElemNum)
CASE (3)
  ans = isAllocated(obj%volumeElemNum)
CASE default
  ans = .FALSE.
END SELECT
END PROCEDURE obj_isElemNumAllocated

!----------------------------------------------------------------------------
!                                                       isnodenumAllocated
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isnodenumAllocated
ans = isAllocated(obj%nodenum)
END PROCEDURE obj_isnodenumAllocated

!----------------------------------------------------------------------------
!                                                                 GetQuery
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetParam
IF (PRESENT(isInitiated)) THEN
  isInitiated = obj%isinit
END IF
IF (PRESENT(isSelectionByMeshID)) THEN
  isSelectionByMeshID = obj%ms(1)
END IF
IF (PRESENT(isSelectionByElemNum)) THEN
  isSelectionByElemNum = obj%ms(2)
END IF
IF (PRESENT(isSelectionBynodenum)) THEN
  isSelectionBynodenum = obj%ms(3)
END IF
IF (PRESENT(isSelectionByBox)) THEN
  isSelectionByBox = obj%ms(4)
END IF
END PROCEDURE obj_GetParam

!----------------------------------------------------------------------------
!                                                                 GetPrefix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPrefix
ans = myprefix
END PROCEDURE obj_GetPrefix

END SUBMODULE GetMethods
