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
USE AbstractMesh_Class, ONLY: AbstractMesh_

USE AppendUtility, ONLY: Append

USE IntVector_Method, ONLY: isAllocated, ASSIGNMENT(=), size, &
                            GetPointer

USE Display_Method, ONLY: ToString

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                         GetTotalElemNum
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalElemNum1
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalElemNum1()"
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = 0

isok = obj%isSelectionByElemNum
IF (.NOT. isok) RETURN

SELECT CASE (dim)
CASE (0)
  ans = SIZE(obj%pointElemNum)
CASE (1)
  ans = SIZE(obj%curveElemNum)
CASE (2)
  ans = SIZE(obj%surfaceElemNum)
CASE (3)
  ans = SIZE(obj%volumeElemNum)
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetTotalElemNum1

!----------------------------------------------------------------------------
!                                                           GetTotalElemNum
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalElemNum2
CHARACTER(*), PARAMETER :: myname = "obj_GetTotalElemNum2()"
CLASS(AbstractMesh_), POINTER :: meshptr
INTEGER(I4B) :: ii, tsize
INTEGER(I4B), POINTER :: intptr(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = 0
IF (obj%isSelectionByElemNum) THEN
  ans = obj%GetTotalElemNum(dim=dim)
END IF

! isSelectionByMeshID
IF (obj%isSelectionByMeshID) THEN

  meshptr => dom%GetMeshPointer(dim=dim)

  SELECT CASE (dim)

  CASE (0)
    tsize = SIZE(obj%pointMeshID)
    intptr => GetPointer(obj%pointMeshID, 1_I4B)

  CASE (1)
    tsize = SIZE(obj%curveMeshID)
    intptr => GetPointer(obj%curveMeshID, 1_I4B)

  CASE (2)
    tsize = SIZE(obj%surfaceMeshID)
    intptr => GetPointer(obj%surfaceMeshID, 1_I4B)

  CASE (3)
    tsize = SIZE(obj%volumeMeshID)
    intptr => GetPointer(obj%volumeMeshID, 1_I4B)

  END SELECT

  DO ii = 1, tsize
    ans = ans + meshptr%GetTotalElements(meshid=intptr(ii))
  END DO
  intptr => NULL()

END IF

! TODO enhance GetElemNum in [[MeshSelection_]] so that it works
! when isSelectionByNodeNum and isSelectionByBox is true.

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetTotalElemNum2

!----------------------------------------------------------------------------
!                                                           GetTotalElemNum
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalElemNum3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalElemNum3()"
#endif

INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = 0
DO ii = 0, 3
  ans = ans + obj%GetTotalElemNum(dim=ii)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalElemNum3

!----------------------------------------------------------------------------
!                                                            GetTotalElemNum
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalElemNum4
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalElemNum4()"
#endif

INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = 0

DO ii = 0, 3
  ans = ans + obj%GetTotalElemNum(dim=ii, dom=dom)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalElemNum4

!----------------------------------------------------------------------------
!                                                                GetElemNum
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElemNum1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetElemNum1()"
#endif

LOGICAL(LGT) :: isok
INTEGER(I4B), POINTER :: intptr(:)
INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = 0

isok = obj%isSelectionByElemNum
IF (.NOT. isok) RETURN

SELECT CASE (dim)
CASE (0)
  tsize = SIZE(obj%pointElemNum)
  intptr => GetPointer(obj%pointElemNum, 1_I4B)

CASE (1)
  tsize = SIZE(obj%curveElemNum)
  intptr => GetPointer(obj%curveElemNum, 1_I4B)

CASE (2)
  tsize = SIZE(obj%surfaceElemNum)
  intptr => GetPointer(obj%surfaceElemNum, 1_I4B)

CASE (3)
  tsize = SIZE(obj%volumeElemNum)
  intptr => GetPointer(obj%volumeElemNum, 1_I4B)

END SELECT

DO ii = 1, tsize
  ans(ii) = intptr(ii)
END DO

intptr => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetElemNum1

!----------------------------------------------------------------------------
!                                                                 GetElemNum
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElemNum2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myname = "obj_GetElemNum2()"
#endif

CLASS(AbstractMesh_), POINTER :: meshptr

INTEGER(I4B) :: ii, mysize, jj
INTEGER(I4B), POINTER :: intptr(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = 0

IF (obj%isSelectionByElemNum) THEN
  CALL obj%GetElemNum(dim=dim, ans=ans, tsize=tsize)
END IF

! isSelectionByMeshID
IF (obj%isSelectionByMeshID) THEN

  meshptr => dom%GetMeshPointer(dim=dim)

  SELECT CASE (dim)

  CASE (0)
    mysize = SIZE(obj%pointMeshID)
    intptr => GetPointer(obj%pointMeshID, 1_I4B)

  CASE (1)
    mysize = SIZE(obj%curveMeshID)
    intptr => GetPointer(obj%curveMeshID, 1_I4B)

  CASE (2)
    mysize = SIZE(obj%surfaceMeshID)
    intptr => GetPointer(obj%surfaceMeshID, 1_I4B)

  CASE (3)
    mysize = SIZE(obj%volumeMeshID)
    intptr => GetPointer(obj%volumeMeshID, 1_I4B)

  END SELECT
END IF

DO ii = 1, mysize
  CALL meshptr%GetElemNum_(meshid=intptr(ii), ans=ans(tsize + 1:), tsize=jj, &
                           islocal=.FALSE.)
  tsize = tsize + jj
END DO
intptr => NULL()

! TODO enhance GetElemNum in [[MeshSelection_]] so that it works
! when isSelectionByNodeNum and isSelectionByBox is true.

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetElemNum2

!----------------------------------------------------------------------------
!                                                                GetElemNum
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElemNum3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetElemNum3()"
#endif

INTEGER(I4B) :: ii, mysize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = 0

DO ii = 0, 3
  CALL obj%GetElemNum(ans=ans(tsize + 1:), dim=ii, tsize=mysize)
  tsize = tsize + mysize
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetElemNum3

!----------------------------------------------------------------------------
!                                                                GetElemNum
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElemNum4
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetElemNum4()"
#endif

INTEGER(I4B) :: ii, mysize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

DO ii = 0, 3
  CALL obj%GetElemNum(ans=ans(tsize + 1:), dim=ii, tsize=mysize, dom=dom)
  tsize = tsize + mysize
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetElemNum4

END SUBMODULE GetElemNumMethods
