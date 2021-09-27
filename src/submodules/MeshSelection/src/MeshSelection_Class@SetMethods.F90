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

SUBMODULE (MeshSelection_Class) SetMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                       Add
!----------------------------------------------------------------------------

MODULE PROCEDURE meshSelect_Add
  CHARACTER( LEN = * ), PARAMETER :: myName="meshSelect_Add"
  IF( PRESENT( xidim ) .AND. PRESENT( meshID ) ) THEN
    obj%isSelectionByMeshID = .TRUE.
    SELECT CASE( xidim )
    CASE( 0 )
      CALL APPEND( obj%PointMeshID, meshID )
    CASE( 1 )
      CALL APPEND( obj%CurveMeshID, meshID )
    CASE( 2 )
      CALL APPEND( obj%SurfaceMeshID, meshID )
    CASE( 3 )
      CALL APPEND( obj%VolumeMeshID, meshID )
    END SELECT
    RETURN
  END IF
  IF( PRESENT( xidim ) .AND. PRESENT( elemNum ) ) THEN
    obj%isSelectionByElemNum = .TRUE.
    SELECT CASE( xidim )
    CASE( 0 )
      CALL APPEND( obj%PointElemNum, elemNum )
    CASE( 1 )
      CALL APPEND( obj%CurveElemNum, elemNum )
    CASE( 2 )
      CALL APPEND( obj%SurfaceElemNum, elemNum )
    CASE( 3 )
      CALL APPEND( obj%VolumeElemNum, elemNum )
    END SELECT
    RETURN
  END IF
  IF( PRESENT( nodeNum ) ) THEN
    obj%isSelectionByNodeNum = .TRUE.
    CALL APPEND( obj%NodeNum, nodeNum )
    RETURN
  END IF
  CALL e%raiseError( modName//'::'//myName//'-'// &
    & 'Currently mesh selection is possible through (xidim, meshID), &
    & and (xidim, elemNum). We are working on it' )
END PROCEDURE meshSelect_Add

!----------------------------------------------------------------------------
!                                                                     Set
!----------------------------------------------------------------------------

MODULE PROCEDURE meshSelect_Set
  IF( isAllocated( obj%PointMeshID ) ) THEN
    CALL RemoveDuplicates( obj%PointMeshID )
  END IF
  IF( isAllocated( obj%CurveMeshID ) ) THEN
    CALL RemoveDuplicates( obj%CurveMeshID )
  END IF
  IF( isAllocated( obj%SurfaceMeshID ) ) THEN
    CALL RemoveDuplicates( obj%SurfaceMeshID )
  END IF
  IF( isAllocated( obj%VolumeMeshID ) ) THEN
    CALL RemoveDuplicates( obj%VolumeMeshID )
  END IF
  IF( isAllocated( obj%PointElemNum ) ) THEN
    CALL RemoveDuplicates( obj%PointElemNum )
  END IF
  IF( isAllocated( obj%CurveElemNum ) ) THEN
    CALL RemoveDuplicates( obj%CurveElemNum )
  END IF
  IF( isAllocated( obj%SurfaceElemNum ) ) THEN
    CALL RemoveDuplicates( obj%SurfaceElemNum )
  END IF
  IF( isAllocated( obj%VolumeElemNum ) ) THEN
    CALL RemoveDuplicates( obj%VolumeElemNum )
  END IF
  IF( isAllocated( obj%NodeNum ) ) THEN
    CALL RemoveDuplicates( obj%NodeNum )
  END IF
END PROCEDURE meshSelect_Set

END SUBMODULE SetMethods