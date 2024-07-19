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

SUBMODULE(MeshSelection_Class) GetnodeNumMethods
USE AbstractMesh_Class, ONLY: AbstractMesh_

USE IntVector_Method, ONLY: isAllocated, ASSIGNMENT(=), size, &
                            GetPointer, Append

USE Display_Method, ONLY: ToString

! USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                          GetTotalNodenum
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalNodenum1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetnodeNum1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = SIZE(obj%nodenum)

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
INTEGER(I4B), POINTER :: intptr(:)
CLASS(AbstractMesh_), POINTER :: meshptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

ans = 0

IF (.NOT. PRESENT(onlydim)) THEN
  IF (obj%ms(3)) THEN
    ans = ans + obj%GetTotalNodenum()
  END IF
END IF

IF (obj%ms(1)) THEN
  CALL obj%GetMeshIDPointer(dim=dim, ans=intptr, tsize=jj)
  meshptr => dom%GetMeshPointer(dim=dim)

  DO ii = 1, jj
    kk = intptr(ii)
    mysize = meshptr%GetTotalNodes(meshid=kk)
    ans = ans + mysize
  END DO

  intptr => NULL()
  meshptr => NULL()
END IF

IF (obj%ms(2)) THEN
  jj = obj%GetTotalElemNum(dim=dim)
  ALLOCATE (intptr(jj))

  IF (jj .GT. 0) THEN
    CALL obj%GetElemNum(dim=dim, ans=intptr, tsize=jj)
    meshptr => dom%GetMeshPointer(dim=dim)
    mysize = meshptr%GetTotalNodes(globalElement=intptr, islocal=.FALSE.)
    ans = ans + mysize
    meshptr => NULL()
  END IF

  DEALLOCATE (intptr)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_GetTotalNodeNum2

!----------------------------------------------------------------------------
!                                                            GetTotalNodeNum
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalNodeNum3
INTEGER(I4B) :: ii, nsd, mysize
ans = obj%GetTotalNodenum(dim=0, dom=dom)
nsd = dom%GetNSD()
DO ii = 1, nsd
  mysize = obj%GetTotalNodeNum(dim=ii, dom=dom, onlydim=.TRUE.)
  ans = ans + mysize
END DO
END PROCEDURE obj_GetTotalNodeNum3

!----------------------------------------------------------------------------
!                                                                GetnodeNum
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetnodeNum1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetnodeNum1()"
#endif

INTEGER(I4B) :: ii
INTEGER(I4B), POINTER :: intptr(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = SIZE(obj%nodenum)
intptr => GetPointer(obj%nodenum, tsize)
DO ii = 1, tsize
  ans(ii) = intptr(ii)
END DO
intptr => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetnodeNum1

!----------------------------------------------------------------------------
!                                                                GetnodeNum
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetnodeNum2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetnodeNum2()"
#endif

INTEGER(I4B) :: ii, jj, kk, mysize
INTEGER(I4B), POINTER :: intptr(:)
CLASS(AbstractMesh_), POINTER :: meshptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

tsize = 0

IF (.NOT. PRESENT(onlydim)) THEN
  IF (obj%ms(3)) THEN
    CALL obj%GetNodenum(ans=ans, tsize=tsize)
  END IF
END IF

IF (obj%ms(1)) THEN
  CALL obj%GetMeshIDPointer(dim=dim, ans=intptr, tsize=jj)
  meshptr => dom%GetMeshPointer(dim=dim)

  DO ii = 1, jj
    kk = intptr(ii)
    CALL meshptr%GetNptrs_(meshid=kk, ans=ans(tsize + 1:), tsize=mysize)
    tsize = tsize + mysize
  END DO

  intptr => NULL()
  meshptr => NULL()
END IF

IF (obj%ms(2)) THEN
  jj = obj%GetTotalElemNum(dim=dim)
  ALLOCATE (intptr(jj))

  IF (jj .GT. 0) THEN
    CALL obj%GetElemNum(dim=dim, ans=intptr, tsize=jj)
    meshptr => dom%GetMeshPointer(dim=dim)
    CALL meshptr%GetNptrs_(globalElement=intptr, islocal=.FALSE., &
                           ans=ans(tsize + 1:), tsize=mysize)
    ans = ans + mysize
    meshptr => NULL()
  END IF

  DEALLOCATE (intptr)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_GetnodeNum2

!----------------------------------------------------------------------------
!                                                                GetnodeNum
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNodeNum3
INTEGER(I4B) :: ii, nsd, mysize

tsize = 0
CALL obj%GetNodeNum(dim=0, dom=dom, ans=ans, tsize=tsize)
nsd = dom%GetNSD()
DO ii = 1, nsd
  CALL obj%GetNodeNum(dim=ii, dom=dom, onlydim=.TRUE., &
                      ans=ans(tsize + 1:), tsize=mysize)
  tsize = tsize + mysize
END DO
END PROCEDURE obj_GetNodeNum3

END SUBMODULE GetnodeNumMethods
