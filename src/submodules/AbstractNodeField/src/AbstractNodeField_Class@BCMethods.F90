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
!

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-11
! summary: Boundary condition methods of [[AbstractNodeField_]]
!
! These methods were moved from AbstractField_Class on 2026-11-11.

SUBMODULE(AbstractNodeField_Class) BCMethods
USE ReallocateUtility, ONLY: Reallocate
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "AbstractNodeField_Class@BCMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                GetTotalNBC
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalNBC
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalNBC()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = ALLOCATED(obj%nbc)
ans = 0
IF (isok) ans = SIZE(obj%nbc)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalNBC

!----------------------------------------------------------------------------
!                                                           GetTotalPointNBC
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalPointNBC
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalNBC()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = ALLOCATED(obj%nbc_point)
ans = 0
IF (isok) ans = SIZE(obj%nbc_point)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalPointNBC

!----------------------------------------------------------------------------
!                                                               GetNBCPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNBCPointer
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetNBCPointer()"
INTEGER(I4B) :: tsize
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = ALLOCATED(obj%nbc)

IF (.NOT. isok) THEN
  ans => NULL()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

#ifdef DEBUG_VER
tsize = SIZE(obj%nbc)
CALL AssertError3(indx, tsize, myName, &
                  "a=indx, b=size(obj%nbc)")

#endif

ans => obj%nbc(indx)%ptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetNBCPointer

!----------------------------------------------------------------------------
!                                                          GetPointNBCPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPointNBCPointer
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetPointNBCPointer()"
INTEGER(I4B) :: tsize
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = ALLOCATED(obj%nbc_point)

IF (.NOT. isok) THEN
  ans => NULL()
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

#ifdef DEBUG_VER
tsize = SIZE(obj%nbc_point)
CALL AssertError3(indx, tsize, myName, &
                  "a=indx, b=size(obj%nbc_point)")

#endif

ans => obj%nbc_point(indx)%ptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetPointNBCPointer

!----------------------------------------------------------------------------
!                                                    GetMaxTotalNodeNumForBC
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMaxTotalNodeNumForBC1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetMaxTotalNodeNumForBC1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (obj%isMaxTotalNodeNumForBCSet) THEN
  ans = obj%maxTotalNodeNumForBC

ELSE
  CALL obj%SetMaxTotalNodeNumForBC()
  ans = obj%maxTotalNodeNumForBC
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetMaxTotalNodeNumForBC1

!----------------------------------------------------------------------------
!                                                    GetMaxTotalNodeNumForBC
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMaxTotalNodeNumForBC2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetMaxTotalNodeNumForBC2()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (obj%isMaxTotalNodeNumForBCSet) THEN
  ans = obj%maxTotalNodeNumForBC

ELSE
  CALL obj%SetMaxTotalNodeNumForBC(ivar=ivar)
  ans = obj%maxTotalNodeNumForBC
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetMaxTotalNodeNumForBC2

!----------------------------------------------------------------------------
!                                                    GetTotalDirichletBCIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalDirichletBCIndex1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalDirichletBCIndex1()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = ALLOCATED(obj%dbc)
ans = 0
IF (isok) ans = obj%GetTotalNodeLoc(dbc=obj%dbc, ivar=ivar)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalDirichletBCIndex1

!----------------------------------------------------------------------------
!                                                   GetTotalDirichletBCIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalDirichletBCIndex2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalDirichletBCIndex2()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%GetTotalNodeLoc(dbc=dbc, ivar=ivar)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalDirichletBCIndex2

!----------------------------------------------------------------------------
!                                                         GetDirichletBCIndex
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetDirichletBCIndex
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetDirichletBCIndex()"
#endif

INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = obj%GetTotalDirichletBCIndex(ivar=ivar)
CALL Reallocate(ans, tsize)
CALL obj%GetDirichletBCIndex_(ans=ans, tsize=tsize, ivar=ivar)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetDirichletBCIndex

!----------------------------------------------------------------------------
!                                                       GetDirichletBCIndex_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetDirichletBCIndex1_
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetDirichletBCIndex1_()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = ALLOCATED(obj%dbc)
tsize = 0
IF (isok) CALL obj%GetNodeLoc_(dbc=obj%dbc, ivar=ivar, ans=ans, tsize=tsize)
!! Here we are calling GetNodeLoc_3

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetDirichletBCIndex1_

!----------------------------------------------------------------------------
!                                                       GetDirichletBCIndex_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetDirichletBCIndex2_
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetDirichletBCIndex2_()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%GetNodeLoc_(dbc=dbc, ivar=ivar, ans=ans, tsize=tsize)
!! Here we are calling GetNodeLoc_3

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetDirichletBCIndex2_

!----------------------------------------------------------------------------
!                                                    SetMaxTotalNodeNumForBC
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMaxTotalNodeNumForBC1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetMaxTotalNodeNumForBC1()"
#endif

LOGICAL(LGT) :: isok, ispresent, notset
INTEGER(I4B) :: ibc, tbc, ans1, ans2, ans

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans1 = obj%maxTotalNodeNumForBC
ans2 = ans1
ans = ans1

ispresent = PRESENT(dbc)
IF (ispresent) THEN
  ans2 = dbc%GetTotalNodeNum(fedof=obj%fedof)
  ans = MAX(ans1, ans2)
  ans1 = ans
END IF

ispresent = PRESENT(dbcvec)
IF (ispresent) THEN
  tbc = SIZE(dbcvec)
  DO ibc = 1, tbc
    isok = ASSOCIATED(dbcvec(ibc)%ptr)
    IF (.NOT. isok) CYCLE
    ans2 = dbcvec(ibc)%ptr%GetTotalNodeNum(fedof=obj%fedof)
    ans = MAX(ans1, ans2)
    ans1 = ans
  END DO
END IF

notset = .NOT. obj%isMaxTotalNodeNumForBCSet

ispresent = ALLOCATED(obj%dbc) .AND. notset
IF (ispresent) THEN
  tbc = SIZE(obj%dbc)
  DO ibc = 1, tbc
    isok = ASSOCIATED(obj%dbc(ibc)%ptr)
    IF (.NOT. isok) CYCLE
    ans2 = obj%dbc(ibc)%ptr%GetTotalNodeNum(fedof=obj%fedof)
    ans = MAX(ans1, ans2)
    ans1 = ans
  END DO
END IF

ispresent = ALLOCATED(obj%nbc) .AND. notset
IF (ispresent) THEN
  tbc = SIZE(obj%nbc)
  DO ibc = 1, tbc
    isok = ASSOCIATED(obj%nbc(ibc)%ptr)
    IF (.NOT. isok) CYCLE
    ans2 = obj%nbc(ibc)%ptr%GetTotalNodeNum(fedof=obj%fedof)
    ans = MAX(ans1, ans2)
    ans1 = ans
  END DO
END IF

ispresent = ALLOCATED(obj%nbc_point) .AND. notset
IF (ispresent) THEN
  tbc = SIZE(obj%nbc_point)
  DO ibc = 1, tbc
    isok = ASSOCIATED(obj%nbc_point(ibc)%ptr)
    IF (.NOT. isok) CYCLE
    ans2 = obj%nbc_point(ibc)%ptr%GetTotalNodeNum(fedof=obj%fedof)
    ans = MAX(ans1, ans2)
    ans1 = ans
  END DO
END IF

obj%maxTotalNodeNumForBC = ans
obj%isMaxTotalNodeNumForBCSet = .TRUE.

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetMaxTotalNodeNumForBC1

!----------------------------------------------------------------------------
!                                                    SetMaxTotalNodeNumForBC
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMaxTotalNodeNumForBC2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetMaxTotalNodeNumForBC2()"
#endif

LOGICAL(LGT) :: isok, ispresent, notset
INTEGER(I4B) :: ibc, tbc, ans1, ans2, ans

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans1 = obj%maxTotalNodeNumForBC
ans2 = ans1
ans = ans1

ispresent = PRESENT(dbc)
IF (ispresent) THEN
  ans2 = dbc%GetTotalNodeNum(fedof=obj%fedofs(ivar)%ptr)
  ans = MAX(ans1, ans2)
  ans1 = ans
END IF

ispresent = PRESENT(dbcvec)
IF (ispresent) THEN
  tbc = SIZE(dbcvec)
  DO ibc = 1, tbc
    isok = ASSOCIATED(dbcvec(ibc)%ptr)
    IF (.NOT. isok) CYCLE
    ans2 = dbcvec(ibc)%ptr%GetTotalNodeNum(fedof=obj%fedofs(ivar)%ptr)
    ans = MAX(ans1, ans2)
    ans1 = ans
  END DO
END IF

notset = .NOT. obj%isMaxTotalNodeNumForBCSet

ispresent = ALLOCATED(obj%dbc) .AND. notset
IF (ispresent) THEN
  tbc = SIZE(obj%dbc)
  DO ibc = 1, tbc
    isok = ASSOCIATED(obj%dbc(ibc)%ptr)
    IF (.NOT. isok) CYCLE
    ans2 = obj%dbc(ibc)%ptr%GetTotalNodeNum(fedof=obj%fedofs(ivar)%ptr)
    ans = MAX(ans1, ans2)
    ans1 = ans
  END DO
END IF

ispresent = ALLOCATED(obj%nbc) .AND. notset
IF (ispresent) THEN
  tbc = SIZE(obj%nbc)
  DO ibc = 1, tbc
    isok = ASSOCIATED(obj%nbc(ibc)%ptr)
    IF (.NOT. isok) CYCLE
    ans2 = obj%nbc(ibc)%ptr%GetTotalNodeNum(fedof=obj%fedofs(ivar)%ptr)
    ans = MAX(ans1, ans2)
    ans1 = ans
  END DO
END IF

ispresent = ALLOCATED(obj%nbc_point) .AND. notset
IF (ispresent) THEN
  tbc = SIZE(obj%nbc_point)
  DO ibc = 1, tbc
    isok = ASSOCIATED(obj%nbc_point(ibc)%ptr)
    IF (.NOT. isok) CYCLE
    ans2 = obj%nbc_point(ibc)%ptr%GetTotalNodeNum(fedof=obj%fedofs(ivar)%ptr)
    ans = MAX(ans1, ans2)
    ans1 = ans
  END DO
END IF

obj%maxTotalNodeNumForBC = ans
obj%isMaxTotalNodeNumForBCSet = .TRUE.

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetMaxTotalNodeNumForBC2

!----------------------------------------------------------------------------
!                                                             Include Errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE BCMethods
