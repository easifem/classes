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
USE IntVector_Method, ONLY: DEALLOCATE, isAllocated
USE FPL_Method, ONLY: CheckEssentialParam, Set, GetValue
USE BoundingBox_Method, ONLY: BB_Deallocate => DEALLOCATE

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                       CheckEssentialParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_CheckEssentialParam
CHARACTER(*), PARAMETER :: myName = "obj_CheckEssentialParam()"
CHARACTER(:), ALLOCATABLE :: astr, prefix0

IF (PRESENT(prefix)) THEN
  prefix0 = prefix
ELSE
  prefix0 = obj%GetPrefix()
END IF

astr = "/isSelectionByMeshID/isSelectionBynodeNum/"// &
       "isSelectionByBox/isSelectionByElemNum"

CALL CheckEssentialParam(obj=param, keys=astr, prefix=prefix0, &
                         myName=myName, modName=modName)
!NOTE: CheckEssentialParam param is defined in easifemClasses FPL_Method

astr = ""
prefix0 = ""
END PROCEDURE obj_CheckEssentialParam

!----------------------------------------------------------------------------
!                                                    SetMeshSelectionParam
!----------------------------------------------------------------------------

MODULE PROCEDURE SetMeshSelectionParam
! CHARACTER(*), PARAMETER :: myName = "SetMeshSelectionParam()"

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

END PROCEDURE SetMeshSelectionParam

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate1
CHARACTER(*), PARAMETER :: myName = "obj_Initiate1()"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

CALL obj%DEALLOCATE()

obj%isInitiated = .TRUE.
obj%isSelectionByMeshID = Input(option=isSelectionByMeshID, &
                                default=.FALSE.)

obj%isSelectionByElemNum = Input(option=isSelectionByElemNum, &
                                 default=.FALSE.)

obj%isSelectionBynodeNum = Input(option=isSelectionBynodeNum, &
                                 default=.FALSE.)

obj%isSelectionByBox = Input(option=isSelectionByBox, &
                             default=.FALSE.)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_Initiate1

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate2
CHARACTER(*), PARAMETER :: myName = "obj_Initiate2()"
CHARACTER(:), ALLOCATABLE :: prefix

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

CALL obj%DEALLOCATE()
obj%isInitiated = .TRUE.
prefix = obj%GetPrefix()
CALL GetValue(obj=param, prefix=prefix, key="isSelectionByBox", &
              VALUE=obj%isSelectionByBox)
CALL GetValue(obj=param, prefix=prefix, key="isSelectionByMeshID", &
              VALUE=obj%isSelectionByMeshID)
CALL GetValue(obj=param, prefix=prefix, key="isSelectionBynodeNum", &
              VALUE=obj%isSelectionBynodeNum)
CALL GetValue(obj=param, prefix=prefix, key="isSelectionByElemNum", &
              VALUE=obj%isSelectionByElemNum)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif

END PROCEDURE obj_Initiate2

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
obj%isInitiated = .FALSE.
obj%isSelectionByElemNum = .FALSE.
obj%isSelectionBynodeNum = .FALSE.
obj%isSelectionByMeshID = .FALSE.
obj%isSelectionByBox = .FALSE.
CALL DEALLOCATE (obj%pointMeshID)
CALL DEALLOCATE (obj%curveMeshID)
CALL DEALLOCATE (obj%surfaceMeshID)
CALL DEALLOCATE (obj%volumeMeshID)
CALL DEALLOCATE (obj%pointElemNum)
CALL DEALLOCATE (obj%curveElemNum)
CALL DEALLOCATE (obj%surfaceElemNum)
CALL DEALLOCATE (obj%volumeElemNum)
CALL DEALLOCATE (obj%nodeNum)
CALL DEALLOCATE (obj%pointNodeNum)
CALL DEALLOCATE (obj%curveNodeNum)
CALL DEALLOCATE (obj%surfaceNodeNum)
CALL DEALLOCATE (obj%volumeNodeNum)
CALL BB_Deallocate(obj%pointBox)
CALL BB_Deallocate(obj%curveBox)
CALL BB_Deallocate(obj%surfaceBox)
CALL BB_Deallocate(obj%volumeBox)
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
obj%isInitiated = obj2%isInitiated
obj%isSelectionByMeshID = obj2%isSelectionByMeshID
obj%isSelectionByElemNum = obj2%isSelectionByElemNum
obj%isSelectionBynodeNum = obj2%isSelectionBynodeNum
obj%isSelectionByBox = obj2%isSelectionByBox

IF (isAllocated(obj2%pointMeshID)) obj%pointMeshID = obj2%pointMeshID
IF (isAllocated(obj2%curveMeshID)) obj%curveMeshID = obj2%curveMeshID
IF (isAllocated(obj2%surfaceMeshID)) obj%surfaceMeshID = obj2%surfaceMeshID
IF (isAllocated(obj2%volumeMeshID)) obj%volumeMeshID = obj2%volumeMeshID

IF (isAllocated(obj2%pointElemNum)) obj%pointElemNum = obj2%pointElemNum
IF (isAllocated(obj2%curveElemNum)) obj%curveElemNum = obj2%curveElemNum
IF (isAllocated(obj2%surfaceElemNum)) obj%surfaceElemNum = obj2%surfaceElemNum
IF (isAllocated(obj2%volumeElemNum)) obj%volumeElemNum = obj2%volumeElemNum

IF (isAllocated(obj2%nodeNum)) obj%nodeNum = obj2%nodeNum
END PROCEDURE obj_Copy

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Deallocate_Vector
INTEGER(I4B) :: ii
IF (ALLOCATED(obj)) THEN
  DO ii = 1, SIZE(obj)
    CALL obj(ii)%DEALLOCATE()
  END DO
  DEALLOCATE (obj)
END IF
END PROCEDURE Deallocate_Vector

!----------------------------------------------------------------------------
!                                                             Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE Deallocate_Ptr_Vector
INTEGER(I4B) :: ii
IF (ALLOCATED(obj)) THEN
  DO ii = 1, SIZE(obj)
    IF (ASSOCIATED(obj(ii)%ptr)) THEN
      CALL obj(ii)%ptr%DEALLOCATE()
      obj(ii)%ptr => NULL()
    END IF
  END DO
  DEALLOCATE (obj)
END IF
END PROCEDURE Deallocate_Ptr_Vector

END SUBMODULE ConstructorMethods
