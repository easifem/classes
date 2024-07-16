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

SUBMODULE(MeshSelection_Class) SetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                       Add
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Add
CHARACTER(*), PARAMETER :: myName = "obj_Add()"
LOGICAL(LGT) :: bool1

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START]')
#endif

bool1 = PRESENT(dim) .AND. PRESENT(meshID)
IF (bool1) THEN
  obj%isSelectionByMeshID = .TRUE.
  SELECT CASE (dim)
  CASE (0)
    CALL Append(obj%pointMeshID, meshID)
  CASE (1)
    CALL Append(obj%curveMeshID, meshID)
  CASE (2)
    CALL Append(obj%surfaceMeshID, meshID)
  CASE (3)
    CALL Append(obj%volumeMeshID, meshID)
  END SELECT
END IF

bool1 = PRESENT(dim) .AND. PRESENT(elemNum)
IF (bool1) THEN
  obj%isSelectionByElemNum = .TRUE.
  SELECT CASE (dim)
  CASE (0)
    CALL Append(obj%pointElemNum, elemNum)
  CASE (1)
    CALL Append(obj%curveElemNum, elemNum)
  CASE (2)
    CALL Append(obj%surfaceElemNum, elemNum)
  CASE (3)
    CALL Append(obj%volumeElemNum, elemNum)
  END SELECT
END IF

bool1 = PRESENT(nodeNum) .AND. (.NOT. PRESENT(dim))
IF (bool1) THEN
  obj%isSelectionByNodeNum = .TRUE.
  CALL Append(obj%nodeNum, nodeNum)
END IF

bool1 = PRESENT(nodeNum) .AND. (PRESENT(dim))
IF (bool1) THEN
  obj%isSelectionByNodeNum = .TRUE.
  SELECT CASE (dim)
  CASE (0)
    CALL Append(obj%pointNodeNum, nodeNum)
  CASE (1)
    CALL Append(obj%curveNodeNum, nodeNum)
  CASE (2)
    CALL Append(obj%surfaceNodeNum, nodeNum)
  CASE (3)
    CALL Append(obj%volumeNodeNum, nodeNum)
  END SELECT
END IF

bool1 = PRESENT(dim) .AND. PRESENT(box)
IF (bool1) THEN
  SELECT CASE (dim)
  CASE (0)
    CALL Append(obj%pointBox, box)
  CASE (1)
    CALL Append(obj%curveBox, box)
  CASE (2)
    CALL Append(obj%surfaceBox, box)
  CASE (3)
    CALL Append(obj%volumeBox, box)
  END SELECT
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END]')
#endif
END PROCEDURE obj_Add

!----------------------------------------------------------------------------
!                                                                     Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set
IF (isAllocated(obj%pointMeshID)) THEN
  CALL RemoveDuplicates(obj%pointMeshID)
END IF
IF (isAllocated(obj%curveMeshID)) THEN
  CALL RemoveDuplicates(obj%curveMeshID)
END IF
IF (isAllocated(obj%surfaceMeshID)) THEN
  CALL RemoveDuplicates(obj%surfaceMeshID)
END IF
IF (isAllocated(obj%volumeMeshID)) THEN
  CALL RemoveDuplicates(obj%volumeMeshID)
END IF
IF (isAllocated(obj%pointElemNum)) THEN
  CALL RemoveDuplicates(obj%pointElemNum)
END IF
IF (isAllocated(obj%curveElemNum)) THEN
  CALL RemoveDuplicates(obj%curveElemNum)
END IF
IF (isAllocated(obj%surfaceElemNum)) THEN
  CALL RemoveDuplicates(obj%surfaceElemNum)
END IF
IF (isAllocated(obj%volumeElemNum)) THEN
  CALL RemoveDuplicates(obj%volumeElemNum)
END IF
IF (isAllocated(obj%pointNodeNum)) CALL Append(obj%nodeNum, obj%pointNodeNum)
IF (isAllocated(obj%curveNodeNum)) CALL Append(obj%nodeNum, obj%curveNodeNum)
if(isAllocated(obj%surfaceNodeNum) ) CALL Append(obj%nodeNum, obj%surfaceNodeNum)
if(isAllocated(obj%volumeNodeNum) ) CALL Append(obj%nodeNum, obj%volumeNodeNum)
IF (isAllocated(obj%nodeNum)) THEN
  CALL RemoveDuplicates(obj%nodeNum)
END IF
END PROCEDURE obj_Set

!----------------------------------------------------------------------------
!                                                         MeshSelectionSet
!----------------------------------------------------------------------------

MODULE PROCEDURE meshSelection_Set_Vec
CHARACTER(*), PARAMETER :: myName = "meshSelection_Set_Vec()"
INTEGER(I4B) :: ii, tMaterials
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

tMaterials = SIZE(obj)
DO ii = 1, tMaterials
  CALL obj(ii)%Set()
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER
END PROCEDURE meshSelection_Set_Vec

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods
