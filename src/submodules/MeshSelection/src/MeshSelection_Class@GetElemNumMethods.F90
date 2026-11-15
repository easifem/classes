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
USE IntVector_Method, ONLY: IntVectorSize => Size
USE IntVector_Method, ONLY: IntVectorGet_ => Get_
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "MeshSelection_Class@GetElemNumMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                            GetTotalElemNum
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalElemNum1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalElemNum1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = 0
SELECT CASE (dim)
CASE (0)
  ans = IntVectorSize(obj%pointElemNum)
CASE (1)
  ans = IntVectorSize(obj%curveElemNum)
CASE (2)
  ans = IntVectorSize(obj%surfaceElemNum)
CASE (3)
  ans = IntVectorSize(obj%volumeElemNum)

CASE DEFAULT
#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, "No case found for dim")
#endif

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
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalElemNum2()"
#endif

CLASS(AbstractMesh_), POINTER :: meshptr
INTEGER(I4B) :: ii
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%GetTotalElemNum(dim=dim)

meshptr => dom%GetMeshPointer(dim=dim)

isok = ASSOCIATED(meshptr)
IF (.NOT. isok) THEN
  meshptr => NULL()
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

SELECT CASE (dim)

! isSelectionByMeshID
CASE (0)
  DO ii = 1, obj%pointMeshID%tsize
    ans = ans + meshptr%GetTotalElements(meshid=obj%pointMeshID%val(ii))
  END DO
  ! TODO enhance GetElemNum in MeshSelection_ so that it works
  ! when isSelectionByNodeNum and isSelectionByBox is true.

CASE (1)
  DO ii = 1, obj%curveMeshID%tsize
    ans = ans + meshptr%GetTotalElements(meshid=obj%curveMeshID%val(ii))
  END DO
  ! TODO enhance GetElemNum in MeshSelection_ so that it works
  ! when isSelectionByNodeNum and isSelectionByBox is true.

CASE (2)
  DO ii = 1, obj%surfaceMeshID%tsize
    ans = ans + meshptr%GetTotalElements(meshid=obj%surfaceMeshID%val(ii))
  END DO
  ! TODO enhance GetElemNum in MeshSelection_ so that it works
  ! when isSelectionByNodeNum and isSelectionByBox is true.

CASE (3)
  DO ii = 1, obj%volumeMeshID%tsize
    ans = ans + meshptr%GetTotalElements(meshid=obj%volumeMeshID%val(ii))
  END DO
  ! TODO enhance GetElemNum in MeshSelection_ so that it works
  ! when isSelectionByNodeNum and isSelectionByBox is true.

CASE DEFAULT
#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, "No case found for dim")
#endif
END SELECT

meshptr => NULL()

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
INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = 0

SELECT CASE (dim)

CASE (0)
  tsize = obj%pointElemNum%tsize
  DO ii = 1, tsize
    ans(ii) = obj%pointElemNum%val(ii)
  END DO

CASE (1)
  tsize = obj%curveElemNum%tsize
  DO ii = 1, tsize
    ans(ii) = obj%curveElemNum%val(ii)
  END DO

CASE (2)
  tsize = obj%surfaceElemNum%tsize
  DO ii = 1, tsize
    ans(ii) = obj%surfaceElemNum%val(ii)
  END DO

CASE (3)
  tsize = obj%volumeElemNum%tsize
  DO ii = 1, tsize
    ans(ii) = obj%volumeElemNum%val(ii)
  END DO

CASE DEFAULT
#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, "No case found for dim")
#endif
END SELECT

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
INTEGER(I4B) :: ii, jj
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! isSelectionByElemNum
CALL obj%GetElemNum(dim=dim, ans=ans, tsize=tsize)

meshptr => dom%GetMeshPointer(dim=dim)
isok = ASSOCIATED(meshptr)
IF (.NOT. isok) THEN
  meshptr => NULL()
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

! isSelectionByMeshID
SELECT CASE (dim)
CASE (0)
  DO ii = 1, obj%pointMeshID%tsize
    CALL meshptr%GetElemNum_(meshid=obj%pointMeshID%val(ii), &
                             ans=ans(tsize + 1:), tsize=jj, &
                             islocal=math%no)
    tsize = tsize + jj
  END DO

CASE (1)
  DO ii = 1, obj%curveMeshID%tsize
    CALL meshptr%GetElemNum_(meshid=obj%curveMeshID%val(ii), &
                             ans=ans(tsize + 1:), tsize=jj, &
                             islocal=math%no)
    tsize = tsize + jj
  END DO

CASE (2)
  DO ii = 1, obj%surfaceMeshID%tsize
    CALL meshptr%GetElemNum_(meshid=obj%surfaceMeshID%val(ii), &
                             ans=ans(tsize + 1:), tsize=jj, &
                             islocal=math%no)
    tsize = tsize + jj
  END DO

CASE (3)
  DO ii = 1, obj%volumeMeshID%tsize
    CALL meshptr%GetElemNum_(meshid=obj%volumeMeshID%val(ii), &
                             ans=ans(tsize + 1:), tsize=jj, &
                             islocal=math%no)
    tsize = tsize + jj
  END DO

CASE DEFAULT
#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, "No case found for dim")
#endif
END SELECT

! TODO: enhance GetElemNum in [[MeshSelection_]] so that it works
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

tsize = 0
DO ii = 0, 3
  CALL obj%GetElemNum(ans=ans(tsize + 1:), dim=ii, tsize=mysize, dom=dom)
  tsize = tsize + mysize
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetElemNum4

!----------------------------------------------------------------------------
!                                                             Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetElemNumMethods
