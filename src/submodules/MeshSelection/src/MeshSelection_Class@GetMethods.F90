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
USE IntVector_Method, ONLY: ASSIGNMENT(=)
USE IntVector_Method, ONLY: IsAllocated
USE IntVector_Method, ONLY: IntVectorGet_ => Get_
USE IntVector_Method, ONLY: IntVectorGet => Get
USE ReallocateUtility, ONLY: Reallocate
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "MeshSelection_Class@GetMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                 GetMeshID
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMeshID
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetMeshID()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

SELECT CASE (dim)
CASE (0)
  ans = IntVectorGet(obj=obj%pointMeshID, datatype=math%one_i)
CASE (1)
  ans = IntVectorGet(obj=obj%curveMeshID, datatype=math%one_i)
CASE (2)
  ans = IntVectorGet(obj=obj%surfaceMeshID, datatype=math%one_i)
CASE (3)
  ans = IntVectorGet(obj=obj%volumeMeshID, datatype=math%one_i)
CASE DEFAULT
#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, "No case found for dim.")
#endif
  CALL Reallocate(ans, math%zero_i)
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetMeshID

!----------------------------------------------------------------------------
!                                                                 GetMeshID_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMeshID_
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetMeshID_()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = 0

SELECT CASE (dim)
CASE (0)
  CALL IntVectorGet_(obj=obj%pointMeshID, ans=ans, tsize=tsize)
CASE (1)
  CALL IntVectorGet_(obj=obj%curveMeshID, ans=ans, tsize=tsize)
CASE (2)
  CALL IntVectorGet_(obj=obj%surfaceMeshID, ans=ans, tsize=tsize)
CASE (3)
  CALL IntVectorGet_(obj=obj%volumeMeshID, ans=ans, tsize=tsize)
CASE DEFAULT
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetMeshID_

!----------------------------------------------------------------------------
!                                                         IsMeshIDAllocated
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsMeshIDAllocated
SELECT CASE (dim)
CASE (0)
  ans = IsAllocated(obj%pointMeshID)
CASE (1)
  ans = IsAllocated(obj%curveMeshID)
CASE (2)
  ans = IsAllocated(obj%surfaceMeshID)
CASE (3)
  ans = IsAllocated(obj%volumeMeshID)
CASE DEFAULT
  ans = .FALSE.
END SELECT
END PROCEDURE obj_IsMeshIDAllocated

!----------------------------------------------------------------------------
!                                                         isElemNumAllocated
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isElemNumAllocated
SELECT CASE (dim)
CASE (0)
  ans = IsAllocated(obj%pointElemNum)
CASE (1)
  ans = IsAllocated(obj%curveElemNum)
CASE (2)
  ans = IsAllocated(obj%surfaceElemNum)
CASE (3)
  ans = IsAllocated(obj%volumeElemNum)
CASE DEFAULT
  ans = .FALSE.
END SELECT
END PROCEDURE obj_isElemNumAllocated

!----------------------------------------------------------------------------
!                                                       isnodenumAllocated
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_isnodenumAllocated
ans = IsAllocated(obj%nodenum)
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
!                                                             Include Errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetMethods
