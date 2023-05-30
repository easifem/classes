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

SUBMODULE(MeshSelection_Class) GetElemNumMethods
USE BaseMethod
USE Mesh_Class, ONLY: Mesh_
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                getElemNum
!----------------------------------------------------------------------------

MODULE PROCEDURE meshSelect_getElemNum1
CHARACTER(*), PARAMETER :: myName = "meshSelect_getElemNum1"

IF (.NOT. obj%isSelectionByElemNum) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'This method works only when obj%isSelectionByElemNum is true')
END IF

SELECT CASE (dim)
CASE (0)
  IF (isAllocated(obj%PointElemNum)) ans = obj%PointElemNum
CASE (1)
  IF (isAllocated(obj%CurveElemNum)) ans = obj%CurveElemNum
CASE (2)
  IF (isAllocated(obj%SurfaceElemNum)) ans = obj%SurfaceElemNum
CASE (3)
  IF (isAllocated(obj%VolumeElemNum)) ans = obj%VolumeElemNum
END SELECT
IF (.NOT. ALLOCATED(ans)) THEN
  CALL e%raiseDebug(modName//'::'//myName//' - '// &
    & 'No element found in the mesh of given dimension!!')
  ALLOCATE (ans(0))
END IF
END PROCEDURE meshSelect_getElemNum1

!----------------------------------------------------------------------------
!                                                                 getElemNum
!----------------------------------------------------------------------------

MODULE PROCEDURE meshSelect_getElemNum2
CHARACTER(*), PARAMETER :: myname = "meshSelect_getElemNum2"
CLASS(Mesh_), POINTER :: meshptr
INTEGER(I4B) :: ii
!
! isSelectionByElemNum
!
IF (obj%isSelectionByElemNum) THEN
  ans = obj%getElemNum(dim=dim)
END IF
!
! isSelectionByMeshID
!
IF (obj%isSelectionByMeshID) THEN
  meshptr => NULL()
  SELECT CASE (dim)
  CASE (0)
    DO ii = 1, SIZE(obj%pointMeshID)
      meshptr => domain%getMeshPointer( &
        & dim=dim, &
        & entityNum=obj%pointMeshID%val(ii))
      CALL append(ans, meshptr%getElemNum())
    END DO
  CASE (1)
    DO ii = 1, SIZE(obj%curveMeshID)
      meshptr => domain%getMeshPointer(dim=dim, &
        & entityNum=obj%curveMeshID%val(ii))
      CALL append(ans, meshptr%getElemNum())
    END DO
  CASE (2)
    DO ii = 1, SIZE(obj%surfaceMeshID)
      meshptr => domain%getMeshPointer(dim=dim, &
        & entityNum=obj%surfaceMeshID%val(ii))
      CALL append(ans, meshptr%getElemNum())
    END DO
  CASE (3)
    DO ii = 1, SIZE(obj%volumeMeshID)
      meshptr => domain%getMeshPointer(dim=dim, &
        & entityNum=obj%volumeMeshID%val(ii))
      CALL append(ans, meshptr%getElemNum())
    END DO
  END SELECT
END IF
! IF (.NOT. ALLOCATED(ans)) ALLOCATE (ans(0))

IF (.NOT. ALLOCATED(ans)) THEN
  CALL e%raiseDebug(modName//'::'//myName//' - '// &
    & 'No element found in the mesh of given dimension!!')
  ALLOCATE (ans(0))
END IF
! TODO enhance getElemNum in [[MeshSelection_]] so that it works
! when isSelectionByNodeNum and isSelectionByBox is true.
END PROCEDURE meshSelect_getElemNum2

!----------------------------------------------------------------------------
!                                                                getElemNum
!----------------------------------------------------------------------------

MODULE PROCEDURE meshSelect_getElemNum3
INTEGER(I4B) :: ii
DO ii = 0, 3
  CALL append(ans, obj%getElemNum(dim=ii))
END DO
END PROCEDURE meshSelect_getElemNum3

!----------------------------------------------------------------------------
!                                                                getElemNum
!----------------------------------------------------------------------------

MODULE PROCEDURE meshSelect_getElemNum4
!
INTEGER(I4B) :: ii
INTEGER(I4B), ALLOCATABLE :: intvec(:)
!
! main
!
DO ii = 0, 3
  intvec = obj%getElemNum(dim=ii, domain=domain)
  CALL append(ans, intvec)
END DO
!
END PROCEDURE meshSelect_getElemNum4

END SUBMODULE GetElemNumMethods
