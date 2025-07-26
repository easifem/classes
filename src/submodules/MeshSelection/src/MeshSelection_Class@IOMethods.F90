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

SUBMODULE(MeshSelection_Class) IOMethods
USE Display_Method, ONLY: Display, ToString
USE BoundingBox_Method, ONLY: BoundingBox_Display => Display
USE GlobalData, ONLY: CHAR_LF, stdout
USE IntVector_Method, ONLY: Intvector_Display => Display, &
                            ASSIGNMENT(=), isAllocated

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display()"
#endif

LOGICAL(LGT) :: bool1
INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Display(msg, unitNo=unitNo)
CALL Display(obj%isInit, "IsInitiated: ", unitNo=unitNo)
IF (.NOT. obj%isinit) RETURN

CALL Display(obj%ms(1), "IsSelectionByMeshID : ", unitNo=unitNo)
CALL Display(obj%ms(2), "IsSelectionByElemNum : ", unitNo=unitNo)
CALL Display(obj%ms(3), "IsSelectionByNodeNum : ", unitNo=unitNo)
CALL Display(obj%ms(4), "IsSelectionByBox : ", unitNo=unitNo)

bool1 = IsAllocated(obj%pointMeshID)
CALL Display(bool1, "PointMeshID ALLOCATED :", unitNo=unitNo)

bool1 = IsAllocated(obj%curveMeshID)
CALL Display(bool1, "CurveMeshID ALLOCATED :", unitNo=unitNo)

bool1 = IsAllocated(obj%surfaceMeshID)
CALL Display(bool1, "SurfaceMeshID ALLOCATED :", unitNo=unitNo)

bool1 = IsAllocated(obj%volumeMeshID)
CALL Display(bool1, "VolumeMeshID ALLOCATED :", unitNo=unitNo)

bool1 = IsAllocated(obj%pointElemNum)
CALL Display(bool1, "PointElemNum ALLOCATED :", unitNo=unitNo)

bool1 = IsAllocated(obj%curveElemNum)
CALL Display(bool1, "CurveElemNum ALLOCATED :", unitNo=unitNo)

bool1 = IsAllocated(obj%surfaceElemNum)
CALL Display(bool1, "SurfaceElemNum ALLOCATED :", unitNo=unitNo)

bool1 = IsAllocated(obj%volumeElemNum)
CALL Display(bool1, "VolumeElemNum ALLOCATED :", unitNo=unitNo)

bool1 = IsAllocated(obj%pointNodeNum)
CALL Display(bool1, "PointNodeNum ALLOCATED :", unitNo=unitNo)

bool1 = IsAllocated(obj%curveNodeNum)
CALL Display(bool1, "CurveNodeNum ALLOCATED :", unitNo=unitNo)

bool1 = IsAllocated(obj%surfaceNodeNum)
CALL Display(bool1, "SurfaceNodeNum ALLOCATED :", unitNo=unitNo)

bool1 = IsAllocated(obj%volumeNodeNum)
CALL Display(bool1, "VolumeNodeNum ALLOCATED :", unitNo=unitNo)

bool1 = ALLOCATED(obj%pointBox)
CALL Display(bool1, "PointBox ALLOCATED :", unitNo=unitNo)

bool1 = ALLOCATED(obj%curveBox)
CALL Display(bool1, "CurveBox ALLOCATED :", unitNo=unitNo)

bool1 = ALLOCATED(obj%surfaceBox)
CALL Display(bool1, "SurfaceBox ALLOCATED :", unitNo=unitNo)

bool1 = ALLOCATED(obj%volumeBox)
CALL Display(bool1, "VolumeBox ALLOCATED :", unitNo=unitNo)

bool1 = IsAllocated(obj%pointMeshID)
IF (bool1) THEN
  CALL Intvector_Display(obj%pointMeshID, "PointMeshID : ", unitNo=unitNo)
END IF

bool1 = IsAllocated(obj%curveMeshID)
IF (bool1) THEN
  CALL Intvector_Display(obj%curveMeshID, "CurveMeshID : ", unitNo=unitNo)
END IF

bool1 = IsAllocated(obj%surfaceMeshID)
IF (bool1) THEN
  CALL Intvector_Display(obj%surfaceMeshID, "SurfaceMeshID : ", unitNo=unitNo)
END IF

bool1 = IsAllocated(obj%volumeMeshID)
IF (bool1) THEN
  CALL Intvector_Display(obj%volumeMeshID, "VolumeMeshID : ", unitNo=unitNo)
END IF

bool1 = IsAllocated(obj%pointElemNum)
IF (bool1) THEN
  CALL Intvector_Display(obj%pointElemNum, "PointElemNum : ", unitNo=unitNo)
END IF

bool1 = IsAllocated(obj%curveElemNum)
IF (bool1) THEN
  CALL Intvector_Display(obj%curveElemNum, "CurveElemNum : ", unitNo=unitNo)
END IF

bool1 = IsAllocated(obj%surfaceElemNum)
IF (bool1) THEN
  CALL Intvector_Display(obj%surfaceElemNum, "SurfaceElemNum : ", &
                         unitNo=unitNo)
END IF

bool1 = IsAllocated(obj%volumeElemNum)
IF (bool1) THEN
  CALL Intvector_Display(obj%volumeElemNum, "VolumeElemNum : ", unitNo=unitNo)
END IF

bool1 = IsAllocated(obj%pointNodeNum)
IF (bool1) THEN
  CALL Intvector_Display(obj%pointNodeNum, "PointNodeNum : ", unitNo=unitNo)
END IF

bool1 = IsAllocated(obj%curveNodeNum)
IF (bool1) THEN
  CALL Intvector_Display(obj%curveNodeNum, "CurveNodeNum : ", unitNo=unitNo)
END IF

bool1 = IsAllocated(obj%surfaceNodeNum)
IF (bool1) THEN
  CALL Intvector_Display(obj%surfaceNodeNum, "SurfaceNodeNum : ", &
                         unitNo=unitNo)
END IF

bool1 = IsAllocated(obj%volumeNodeNum)
IF (bool1) THEN
  CALL Intvector_Display(obj%volumeNodeNum, "VolumeNodeNum : ", unitNo=unitNo)
END IF

bool1 = ALLOCATED(obj%pointBox)
IF (bool1) THEN
  DO ii = 1, SIZE(obj%pointBox)
    CALL BoundingBox_Display(obj%pointBox(ii), &
                             "PointBox("//tostring(ii)//") : ", &
                             unitNo=unitNo)
  END DO
END IF

bool1 = ALLOCATED(obj%curveBox)
IF (bool1) THEN
  DO ii = 1, SIZE(obj%curveBox)
    CALL BoundingBox_Display(obj%curveBox(ii), &
                             "curveBox("//tostring(ii)//") : ", unitNo=unitNo)
  END DO
END IF

bool1 = ALLOCATED(obj%surfaceBox)
IF (bool1) THEN
  DO ii = 1, SIZE(obj%surfaceBox)
    CALL BoundingBox_Display(obj%surfaceBox(ii), &
                           "surfaceBox("//tostring(ii)//") : ", unitNo=unitNo)
  END DO
END IF

bool1 = ALLOCATED(obj%volumeBox)
IF (bool1) THEN
  DO ii = 1, SIZE(obj%volumeBox)
    CALL BoundingBox_Display(obj%volumeBox(ii), &
                            "volumeBox("//tostring(ii)//") : ", unitNo=unitNo)
  END DO
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                             Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE IOMethods
