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

SUBMODULE (MeshSelection_Class) ConstructorMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                               addSurrogate
!----------------------------------------------------------------------------

MODULE PROCEDURE meshSelect_addSurrogate
  CALL e%addSurrogate( UserObj )
END PROCEDURE meshSelect_addSurrogate

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE meshSelect_Initiate
  IF( .NOT. obj%isInitiated ) THEN
    obj%isInitiated = .TRUE.
    IF( PRESENT( isSelectionByMeshID ) ) THEN
      obj%isSelectionByMeshID = isSelectionByMeshID
    ELSE
      obj%isSelectionByMeshID = .FALSE.
    END IF
    IF( PRESENT( isSelectionByElemNum ) ) THEN
      obj%isSelectionByElemNum = isSelectionByElemNum
    ELSE
      obj%isSelectionByElemNum = .FALSE.
    END IF
    IF( PRESENT( isSelectionByNodeNum ) ) THEN
      obj%isSelectionByNodeNum = isSelectionByNodeNum
    ELSE
      obj%isSelectionByNodeNum = .FALSE.
    END IF
    IF( PRESENT( isSelectionByBox ) ) THEN
      obj%isSelectionByBox = isSelectionByBox
    ELSE
      obj%isSelectionByBox = .FALSE.
    END IF
  ELSE
    IF( PRESENT( isSelectionByMeshID ) ) THEN
      obj%isSelectionByMeshID = isSelectionByMeshID
    END IF
    IF( PRESENT( isSelectionByElemNum ) ) THEN
      obj%isSelectionByElemNum = isSelectionByElemNum
    END IF
    IF( PRESENT( isSelectionByNodeNum ) ) THEN
      obj%isSelectionByNodeNum = isSelectionByNodeNum
    END IF
    IF( PRESENT( isSelectionByBox ) ) THEN
      obj%isSelectionByBox = isSelectionByBox
    END IF
  END IF
END PROCEDURE meshSelect_Initiate

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE meshSelect_DeallocateData
  obj%isInitiated = .FALSE.
  obj%isSelectionByElemNum = .FALSE.
  obj%isSelectionByNodeNum = .FALSE.
  obj%isSelectionByMeshID = .FALSE.
  obj%isSelectionByBox = .FALSE.
  CALL DeallocateData( obj%PointMeshID )
  CALL DeallocateData( obj%CurveMeshID )
  CALL DeallocateData( obj%SurfaceMeshID )
  CALL DeallocateData( obj%VolumeMeshID )
  CALL DeallocateData( obj%PointElemNum )
  CALL DeallocateData( obj%CurveElemNum )
  CALL DeallocateData( obj%SurfaceElemNum )
  CALL DeallocateData( obj%VolumeElemNum )
  CALL DeallocateData( obj%NodeNum )
END PROCEDURE meshSelect_DeallocateData

!----------------------------------------------------------------------------
!                                                                     Final
!----------------------------------------------------------------------------

MODULE PROCEDURE meshSelect_Final
  CALL obj%DeallocateData()
END PROCEDURE meshSelect_Final

!----------------------------------------------------------------------------
!                                                                     Copy
!----------------------------------------------------------------------------

MODULE PROCEDURE meshSelect_Copy
  obj%isInitiated = obj2%isInitiated
  obj%isSelectionByMeshID = obj2%isSelectionByMeshID
  obj%isSelectionByElemNum = obj2%isSelectionByElemNum
  obj%isSelectionByNodeNum = obj2%isSelectionByNodeNum
  obj%isSelectionByBox = obj2%isSelectionByBox
  !>
  IF( isAllocated( obj2%PointMeshID ) ) obj%PointMeshID = obj2%PointMeshID
  IF( isAllocated( obj2%CurveMeshID ) ) obj%CurveMeshID = obj2%CurveMeshID
  IF( isAllocated( obj2%SurfaceMeshID ) ) obj%SurfaceMeshID = obj2%SurfaceMeshID
  IF( isAllocated( obj2%VolumeMeshID ) ) obj%VolumeMeshID = obj2%VolumeMeshID
  !>
  IF( isAllocated( obj2%PointElemNum ) ) obj%PointElemNum = obj2%PointElemNum
  IF( isAllocated( obj2%CurveElemNum ) ) obj%CurveElemNum = obj2%CurveElemNum
  IF( isAllocated( obj2%SurfaceElemNum ) ) obj%SurfaceElemNum = obj2%SurfaceElemNum
  IF( isAllocated( obj2%VolumeElemNum ) ) obj%VolumeElemNum = obj2%VolumeElemNum
  !>
  IF( isAllocated( obj2%NodeNum ) ) obj%NodeNum = obj2%NodeNum
END PROCEDURE meshSelect_Copy

END SUBMODULE ConstructorMethods