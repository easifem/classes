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

SUBMODULE(MeshSelection_Class) ConstructorMethods
USE InputUtility, ONLY: Input
USE ReallocateUtility, ONLY: Reallocate
USE IntVector_Method, ONLY: IntVector_Deallocate => DEALLOCATE, &
                            IntVector_IsAllocated => IsAllocated, &
                            IntVector_Copy => Copy
USE FPL_Method, ONLY: CheckEssentialParam, Set, GetValue
USE BoundingBox_Method, ONLY: BoundingBox_Deallocate => DEALLOCATE, &
                              BoundingBox_Initiate => Initiate, &
                              BoundingBox_Reallocate => Reallocate, &
                              BoundingBox_Copy => Copy

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                           IsInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsInitiated
ans = obj%isinit
END PROCEDURE obj_IsInitiated

!----------------------------------------------------------------------------
!                                                       IsSelectionByMeshID
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsSelectionByMeshID
ans = obj%ms(1)
END PROCEDURE obj_IsSelectionByMeshID

!----------------------------------------------------------------------------
!                                                       IsSelectionByElemNum
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsSelectionByElemNum
ans = obj%ms(2)
END PROCEDURE obj_IsSelectionByElemNum

!----------------------------------------------------------------------------
!                                                       IsSelectionBynodeNum
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsSelectionBynodeNum
ans = obj%ms(3)
END PROCEDURE obj_IsSelectionBynodeNum

!----------------------------------------------------------------------------
!                                                       IsSelectionByBox
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsSelectionByBox
ans = obj%ms(4)
END PROCEDURE obj_IsSelectionByBox

!----------------------------------------------------------------------------
!                                                       GetTotalPointMeshid
!----------------------------------------------------------------------------

! MODULE PROCEDURE obj_GetTotalPointMeshid
! ans = obj%totalPointMeshid
! END PROCEDURE obj_GetTotalPointMeshid

!----------------------------------------------------------------------------
!                                                       GetTotalCurveMeshid
!----------------------------------------------------------------------------

! MODULE PROCEDURE obj_GetTotalCurveMeshid
! ans = obj%totalCurveMeshid
! END PROCEDURE obj_GetTotalCurveMeshid

!----------------------------------------------------------------------------
!                                                       GetTotalSurfaceMeshid
!----------------------------------------------------------------------------

! MODULE PROCEDURE obj_GetTotalSurfaceMeshid
! ans = obj%totalSurfaceMeshid
! END PROCEDURE obj_GetTotalSurfaceMeshid

!----------------------------------------------------------------------------
!                                                       GetTotalVolumeMeshid
!----------------------------------------------------------------------------

! MODULE PROCEDURE obj_GetTotalVolumeMeshid
! ans = obj%totalVolumeMeshid
! END PROCEDURE obj_GetTotalVolumeMeshid

!----------------------------------------------------------------------------
!                                                       GetTotalPointElemNum
!----------------------------------------------------------------------------

! MODULE PROCEDURE obj_GetTotalPointElemNum
! ans = obj%totalPointElemNum
! END PROCEDURE obj_GetTotalPointElemNum

!----------------------------------------------------------------------------
!                                                       GetTotalCurveElemNum
!----------------------------------------------------------------------------

! MODULE PROCEDURE obj_GetTotalCurveElemNum
! ans = obj%totalCurveElemNum
! END PROCEDURE obj_GetTotalCurveElemNum

!----------------------------------------------------------------------------
!                                                       GetTotalSurfaceElemNum
!----------------------------------------------------------------------------

! MODULE PROCEDURE obj_GetTotalSurfaceElemNum
! ans = obj%totalSurfaceElemNum
! END PROCEDURE obj_GetTotalSurfaceElemNum

!----------------------------------------------------------------------------
!                                                       GetTotalVolumeElemNum
!----------------------------------------------------------------------------

! MODULE PROCEDURE obj_GetTotalVolumeElemNum
! ans = obj%totalVolumeElemNum
! END PROCEDURE obj_GetTotalVolumeElemNum

!----------------------------------------------------------------------------
!                                                       GetTotalPointNodenum
!----------------------------------------------------------------------------

! MODULE PROCEDURE obj_GetTotalPointNodenum
! ans = obj%totalPoint
! END PROCEDURE obj_GetTotalPointNodenum

!----------------------------------------------------------------------------
!                                                       CheckEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_CheckEssentialParam
CHARACTER(*), PARAMETER :: myName = "obj_CheckEssentialParam()"
CHARACTER(:), ALLOCATABLE :: astr, prefix0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (PRESENT(prefix)) THEN
  prefix0 = prefix
ELSE
  prefix0 = obj%GetPrefix()
END IF

astr = "/isSelectionByMeshID/isSelectionBynodeNum/"// &
       "isSelectionByBox/isSelectionByElemNum"

CALL CheckEssentialParam(obj=param, keys=astr, prefix=prefix0, &
                         myName=myName, modName=modName)
!note: CheckEssentialParam param is defined in easifemClasses FPL_Method

astr = ""
prefix0 = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_CheckEssentialParam

!----------------------------------------------------------------------------
!                                                    SetMeshSelectionParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetMeshSelectionParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "SetMeshSelectionParam()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Set(param, datatype=.TRUE., prefix=prefix, &
         key="isSelectionByElemNum", &
         VALUE=Input(option=isSelectionByElemNum, default=.FALSE.))

CALL Set(param, datatype=.TRUE., prefix=prefix, &
         key="isSelectionBynodeNum", &
         VALUE=Input(option=isSelectionBynodeNum, default=.FALSE.))

CALL Set(param, datatype=.TRUE., prefix=prefix, &
         key="isSelectionByBox", &
         VALUE=Input(option=isSelectionByBox, default=.FALSE.))

CALL Set(param, datatype=.TRUE., prefix=prefix, &
         key="isSelectionByMeshID", &
         VALUE=Input(option=isSelectionByMeshID, default=.FALSE.))

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE SetMeshSelectionParam

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

CALL obj%DEALLOCATE()

obj%isinit = .TRUE.
obj%ms(1) = Input(option=isSelectionByMeshID, default=.FALSE.)
obj%ms(2) = Input(option=isSelectionByElemNum, default=.FALSE.)
obj%ms(3) = Input(option=isSelectionBynodeNum, default=.FALSE.)
obj%ms(4) = Input(option=isSelectionByBox, default=.FALSE.)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_Initiate1

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate2()"
#endif

CHARACTER(:), ALLOCATABLE :: prefix

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

CALL obj%DEALLOCATE()
obj%isinit = .TRUE.
prefix = obj%GetPrefix()
CALL GetValue(obj=param, prefix=prefix, key="isSelectionByMeshID", &
              VALUE=obj%ms(1))
CALL GetValue(obj=param, prefix=prefix, key="isSelectionByElemNum", &
              VALUE=obj%ms(2))
CALL GetValue(obj=param, prefix=prefix, key="isSelectionBynodeNum", &
              VALUE=obj%ms(3))
CALL GetValue(obj=param, prefix=prefix, key="isSelectionByBox", &
              VALUE=obj%ms(4))

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_Initiate2

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isinit = .FALSE.
obj%ms = .FALSE.
obj%totalPointMeshid = 0
obj%totalCurveMeshid = 0
obj%totalSurfaceMeshid = 0
obj%totalVolumeMeshid = 0
obj%totalPointElemNum = 0
obj%totalCurveElemNum = 0
obj%totalSurfaceElemNum = 0
obj%totalVolumeElemNum = 0
obj%totalPointNodenum = 0
obj%totalCurveNodenum = 0
obj%totalSurfaceNodenum = 0
obj%totalVolumeNodenum = 0

CALL IntVector_DEALLOCATE(obj%pointMeshID)
CALL IntVector_Deallocate(obj%curveMeshID)
CALL IntVector_Deallocate(obj%surfaceMeshID)
CALL IntVector_Deallocate(obj%volumeMeshID)

CALL IntVector_Deallocate(obj%pointElemNum)
CALL IntVector_Deallocate(obj%curveElemNum)
CALL IntVector_Deallocate(obj%surfaceElemNum)
CALL IntVector_Deallocate(obj%volumeElemNum)

CALL IntVector_Deallocate(obj%nodeNum)
CALL IntVector_Deallocate(obj%pointNodeNum)
CALL IntVector_Deallocate(obj%curveNodeNum)
CALL IntVector_Deallocate(obj%surfaceNodeNum)
CALL IntVector_Deallocate(obj%volumeNodeNum)

CALL BoundingBox_Deallocate(obj%pointBox)
CALL BoundingBox_Deallocate(obj%curveBox)
CALL BoundingBox_Deallocate(obj%surfaceBox)
CALL BoundingBox_Deallocate(obj%volumeBox)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                     Final
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Final
CALL obj%DEALLOCATE()
END PROCEDURE obj_Final

!----------------------------------------------------------------------------
!                                                                     Copy
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Copy
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Copy()"
#endif

LOGICAL(LGT) :: isok
INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isinit = obj2%isinit
obj%ms = obj2%ms

obj%totalPointMeshid = obj2%totalPointMeshid
obj%totalCurveMeshid = obj2%totalCurveMeshid
obj%totalSurfaceMeshid = obj2%totalSurfaceMeshid
obj%totalVolumeMeshid = obj2%totalVolumeMeshid
obj%totalPointElemnum = obj2%totalPointElemnum
obj%totalCurveElemnum = obj2%totalCurveElemnum
obj%totalSurfaceElemnum = obj2%totalSurfaceElemnum
obj%totalVolumeElemnum = obj2%totalVolumeElemnum
obj%totalPointNodenum = obj2%totalPointNodenum
obj%totalCurveNodenum = obj2%totalCurveNodenum
obj%totalSurfaceNodenum = obj2%totalSurfaceNodenum
obj%totalVolumeNodenum = obj2%totalVolumeNodenum

CALL IntVector_Copy(obj=obj%pointMeshID, obj2=obj2%pointMeshID)
CALL IntVector_Copy(obj=obj%curveMeshID, obj2=obj2%curveMeshID)
CALL IntVector_Copy(obj=obj%surfaceMeshID, obj2=obj2%surfaceMeshID)
CALL IntVector_Copy(obj=obj%volumeMeshID, obj2=obj2%volumeMeshID)

CALL IntVector_Copy(obj=obj%pointElemNum, obj2=obj2%pointElemNum)
CALL IntVector_Copy(obj=obj%curveElemNum, obj2=obj2%curveElemNum)
CALL IntVector_Copy(obj=obj%surfaceElemNum, obj2=obj2%surfaceElemNum)
CALL IntVector_Copy(obj=obj%volumeElemNum, obj2=obj2%volumeElemNum)

CALL IntVector_Copy(obj=obj%pointNodeNum, obj2=obj2%pointNodeNum)
CALL IntVector_Copy(obj=obj%curveNodeNum, obj2=obj2%curveNodeNum)
CALL IntVector_Copy(obj=obj%surfaceNodeNum, obj2=obj2%surfaceNodeNum)
CALL IntVector_Copy(obj=obj%volumeNodeNum, obj2=obj2%volumeNodeNum)

CALL IntVector_Copy(obj=obj%nodeNum, obj2=obj2%nodeNum)

isok = ALLOCATED(obj2%pointBox)
IF (isok) THEN
  tsize = SIZE(obj2%pointBox)
  CALL BoundingBox_Reallocate(obj%pointBox, tsize)
  CALL BoundingBox_Copy(obj%pointBox, obj2%pointBox)
END IF

isok = ALLOCATED(obj2%curveBox)
IF (isok) THEN
  tsize = SIZE(obj2%curveBox)
  CALL BoundingBox_Reallocate(obj%curveBox, tsize)
  CALL BoundingBox_Copy(obj%curveBox, obj2%curveBox)
END IF

isok = ALLOCATED(obj2%surfaceBox)
IF (isok) THEN
  tsize = SIZE(obj2%surfaceBox)
  CALL BoundingBox_Reallocate(obj%surfaceBox, tsize)
  CALL BoundingBox_Copy(obj%surfaceBox, obj2%surfaceBox)
END IF

isok = ALLOCATED(obj2%volumeBox)
IF (isok) THEN
  tsize = SIZE(obj2%volumeBox)
  CALL BoundingBox_Reallocate(obj%volumeBox, tsize)
  CALL BoundingBox_Copy(obj%volumeBox, obj2%volumeBox)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Copy

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Deallocate_Vector
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Deallocate_Vector()"
#endif

#include "../../include/deallocate_vector.F90"
END PROCEDURE Deallocate_Vector

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Deallocate_Ptr_Vector
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Deallocate_Ptr_Vector()"
#endif

#include "../../include/deallocate_vector_ptr.F90"
END PROCEDURE Deallocate_Ptr_Vector

!----------------------------------------------------------------------------
!                                                                  Reallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Vector
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Reallocate_Vector()"
#endif

#include "../../include/reallocate_vector.F90"
END PROCEDURE Reallocate_Vector

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Reallocate_Ptr_Vector
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Reallocate_Ptr_Vector()"
#endif

#include "../../include/reallocate_vector_ptr.F90"
END PROCEDURE Reallocate_Ptr_Vector

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods
