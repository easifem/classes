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
!                                                                GetElemNum
!----------------------------------------------------------------------------

MODULE PROCEDURE meshSelect_GetElemNum1
CHARACTER(*), PARAMETER :: myName = "meshSelect_GetElemNum1()"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

IF (.NOT. obj%isSelectionByElemNum) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: This method works only when '//  &
    & ' obj%isSelectionByElemNum is true')
  CALL Reallocate(ans, 0)
  RETURN
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
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & 'No element found in the mesh of dimension = '//tostring(dim))
  CALL Reallocate(ans, 0)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE meshSelect_GetElemNum1

!----------------------------------------------------------------------------
!                                                                 GetElemNum
!----------------------------------------------------------------------------

MODULE PROCEDURE meshSelect_GetElemNum2
CHARACTER(*), PARAMETER :: myname = "meshSelect_GetElemNum2()"
CLASS(Mesh_), POINTER :: meshptr
INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

! isSelectionByElemNum
IF (obj%isSelectionByElemNum) THEN
  ans = obj%GetElemNum(dim=dim)
END IF

! isSelectionByMeshID
IF (obj%isSelectionByMeshID) THEN
  meshptr => NULL()
  SELECT CASE (dim)
  CASE (0)
    DO ii = 1, SIZE(obj%pointMeshID)
      meshptr => domain%GetMeshPointer(dim=dim,  &
        & entityNum=obj%pointMeshID%val(ii))
      CALL Append(ans, meshptr%GetElemNum())
    END DO

  CASE (1)
    DO ii = 1, SIZE(obj%curveMeshID)
      meshptr => domain%GetMeshPointer(dim=dim, &
        & entityNum=obj%curveMeshID%val(ii))
      CALL Append(ans, meshptr%GetElemNum())
    END DO
  CASE (2)
    DO ii = 1, SIZE(obj%surfaceMeshID)
      meshptr => domain%GetMeshPointer(dim=dim, &
        & entityNum=obj%surfaceMeshID%val(ii))
      CALL Append(ans, meshptr%GetElemNum())
    END DO
  CASE (3)
    DO ii = 1, SIZE(obj%volumeMeshID)
      meshptr => domain%GetMeshPointer(dim=dim, &
        & entityNum=obj%volumeMeshID%val(ii))
      CALL Append(ans, meshptr%GetElemNum())
    END DO
  END SELECT
END IF

IF (.NOT. ALLOCATED(ans)) THEN
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & 'No element found in the mesh of dimension = '//tostring(dim))
  ALLOCATE (ans(0))
END IF
! TODO enhance GetElemNum in [[MeshSelection_]] so that it works
! when isSelectionByNodeNum and isSelectionByBox is true.

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE meshSelect_GetElemNum2

!----------------------------------------------------------------------------
!                                                                GetElemNum
!----------------------------------------------------------------------------

MODULE PROCEDURE meshSelect_GetElemNum3
INTEGER(I4B) :: ii
CHARACTER(*), PARAMETER :: myName = "meshSelect_GetElemNum3()"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

DO ii = 0, 3
  CALL Append(ans, obj%GetElemNum(dim=ii))
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER
END PROCEDURE meshSelect_GetElemNum3

!----------------------------------------------------------------------------
!                                                                GetElemNum
!----------------------------------------------------------------------------

MODULE PROCEDURE meshSelect_GetElemNum4
CHARACTER(*), PARAMETER :: myName = "meshSelect_GetElemNum4()"
INTEGER(I4B) :: ii
INTEGER(I4B), ALLOCATABLE :: intvec(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

DO ii = 0, 3
  intvec = obj%GetElemNum(dim=ii, domain=domain)
  CALL Append(ans, intvec)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER
END PROCEDURE meshSelect_GetElemNum4

END SUBMODULE GetElemNumMethods
