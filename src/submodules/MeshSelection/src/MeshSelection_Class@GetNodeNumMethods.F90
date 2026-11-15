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

SUBMODULE(MeshSelection_Class) GetNodeNumMethods
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE InputUtility, ONLY: Input
USE IntVector_Method, ONLY: Size
USE IntVector_Method, ONLY: IntVectorGet_ => Get_
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "MeshSelection_Class@GetNodeNumMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                          GetTotalNodenum
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalNodenum1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalNodenum1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = SIZE(obj%nodeNum)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetTotalNodenum1

!----------------------------------------------------------------------------
!                                                           GetTotalNodenum
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalNodenum2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalNodenum2()"
#endif
INTEGER(I4B) :: ii, jj, kk, mysize
CLASS(AbstractMesh_), POINTER :: meshptr
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

ans = 0

isok = Input(default=math%no, option=onlydim)
IF (.NOT. isok .AND. obj%IsSelectionByNodeNum()) THEN
  ans = ans + obj%GetTotalNodenum()
END IF

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
CASE (0)

  ! IsSelectionByMeshID
  DO ii = 1, obj%pointMeshID%tsize
    kk = obj%pointMeshID%val(ii)
    mysize = meshptr%GetTotalNodes(meshid=kk)
    ans = ans + mysize
  END DO

  ! obj%IsSelectionByElemNum()
  jj = obj%pointElemNum%tsize
  IF (jj .GT. 0) THEN
    mysize = meshptr%GetTotalNodes( &
             globalElement=obj%pointElemNum%val(1:jj), islocal=math%no)
    ans = ans + mysize
  END IF

CASE (1)

  ! IsSelectionByMeshID
  DO ii = 1, obj%curveMeshID%tsize
    kk = obj%curveMeshID%val(ii)
    mysize = meshptr%GetTotalNodes(meshid=kk)
    ans = ans + mysize
  END DO

  ! IsSelectionByElemNum
  jj = obj%curveElemNum%tsize
  IF (jj .GT. 0) THEN
    mysize = meshptr%GetTotalNodes( &
             globalElement=obj%curveElemNum%val(1:jj), islocal=math%no)
    ans = ans + mysize
  END IF

CASE (2)

  ! IsSelectionByMeshID
  DO ii = 1, obj%surfaceMeshID%tsize
    kk = obj%surfaceMeshID%val(ii)
    mysize = meshptr%GetTotalNodes(meshid=kk)
    ans = ans + mysize
  END DO

  ! obj%IsSelectionByElemNum()
  jj = obj%surfaceElemNum%tsize
  IF (jj .GT. 0) THEN
    mysize = meshptr%GetTotalNodes( &
             globalElement=obj%surfaceElemNum%val(1:jj), &
             islocal=math%no)
    ans = ans + mysize
  END IF

CASE (3)

  ! IsSelectionByMeshID
  DO ii = 1, obj%volumeMeshID%tsize
    kk = obj%volumeMeshID%val(ii)
    mysize = meshptr%GetTotalNodes(meshid=kk)
    ans = ans + mysize
  END DO

  ! IsSelectionByElemNum
  jj = obj%volumeElemNum%tsize
  IF (jj .GT. 0) THEN
    mysize = meshptr%GetTotalNodes( &
             globalElement=obj%volumeElemNum%val(1:jj), islocal=math%no)
    ans = ans + mysize
  END IF

CASE DEFAULT
#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, "No case found for dim")
#endif
END SELECT

meshptr => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_GetTotalNodeNum2

!----------------------------------------------------------------------------
!                                                            GetTotalNodeNum
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalNodeNum3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalNodeNum3()"
#endif

INTEGER(I4B) :: ii, nsd, mysize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%GetTotalNodenum(dim=0, dom=dom)
nsd = dom%GetNSD()
DO ii = 1, nsd
  mysize = obj%GetTotalNodeNum(dim=ii, dom=dom, onlydim=.TRUE.)
  ans = ans + mysize
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalNodeNum3

!----------------------------------------------------------------------------
!                                                                GetNodeNum
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeNum1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeNum1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL IntVectorGet_(obj=obj%NodeNum, ans=ans, tsize=tsize)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetNodeNum1

!----------------------------------------------------------------------------
!                                                                GetNodeNum
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeNum2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeNum2()"
#endif

INTEGER(I4B) :: ii, jj, kk, mysize
CLASS(AbstractMesh_), POINTER :: meshptr
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

tsize = 0

isok = Input(default=math%no, option=onlydim)
IF (.NOT. isok .AND. obj%IsSelectionByNodeNum()) THEN
  CALL obj%GetNodenum(ans=ans, tsize=tsize)
END IF

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
CASE (0)
  DO ii = 1, obj%pointMeshID%tsize
    kk = obj%pointMeshID%val(ii)
    CALL meshptr%GetNptrs_(meshid=kk, ans=ans(tsize + 1:), tsize=mysize)
    tsize = tsize + mysize
  END DO

  jj = obj%pointElemNum%tsize
  IF (jj .GT. 0) THEN
    CALL meshptr%GetNptrs_(globalElement=obj%pointElemNum%val(1:jj), &
                           islocal=math%no, ans=ans(tsize + 1:), &
                           tsize=mysize)
    tsize = tsize + mysize
  END IF

CASE (1)
  DO ii = 1, obj%curveMeshID%tsize
    kk = obj%curveMeshID%val(ii)
    CALL meshptr%GetNptrs_(meshid=kk, ans=ans(tsize + 1:), tsize=mysize)
    tsize = tsize + mysize
  END DO

  jj = obj%curveElemNum%tsize
  IF (jj .GT. 0) THEN
    CALL meshptr%GetNptrs_(globalElement=obj%curveElemNum%val(1:jj), &
                           islocal=math%no, ans=ans(tsize + 1:), &
                           tsize=mysize)
    tsize = tsize + mysize
  END IF

CASE (2)
  DO ii = 1, obj%surfaceMeshID%tsize
    kk = obj%surfaceMeshID%val(ii)
    CALL meshptr%GetNptrs_(meshid=kk, ans=ans(tsize + 1:), tsize=mysize)
    tsize = tsize + mysize
  END DO

  jj = obj%surfaceElemNum%tsize
  IF (jj .GT. 0) THEN
    CALL meshptr%GetNptrs_(globalElement=obj%surfaceElemNum%val(1:jj), &
                           islocal=math%no, ans=ans(tsize + 1:), &
                           tsize=mysize)
    tsize = tsize + mysize
  END IF

CASE (3)
  DO ii = 1, obj%volumeMeshID%tsize
    kk = obj%volumeMeshID%val(ii)
    CALL meshptr%GetNptrs_(meshid=kk, ans=ans(tsize + 1:), tsize=mysize)
    tsize = tsize + mysize
  END DO

  jj = obj%volumeElemNum%tsize
  IF (jj .GT. 0) THEN
    CALL meshptr%GetNptrs_(globalElement=obj%volumeElemNum%val(1:jj), &
                           islocal=math%no, ans=ans(tsize + 1:), &
                           tsize=mysize)
    tsize = tsize + mysize
  END IF

CASE DEFAULT
#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, "No case found for dim")
#endif
END SELECT

meshptr => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_GetNodeNum2

!----------------------------------------------------------------------------
!                                                                GetNodeNum
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeNum3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNodeNum3()"
#endif

INTEGER(I4B) :: ii, nsd, mysize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = 0
CALL obj%GetNodeNum(dim=0, dom=dom, ans=ans, tsize=tsize)
nsd = dom%GetNSD()
DO ii = 1, nsd
  CALL obj%GetNodeNum(dim=ii, dom=dom, onlydim=.TRUE., &
                      ans=ans(tsize + 1:), tsize=mysize)
  tsize = tsize + mysize
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetNodeNum3

!----------------------------------------------------------------------------
!                                                             Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetNodeNumMethods
