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

SUBMODULE(AbstractField_Class) SetMethods
USE Display_Method, ONLY: ToString
USE GlobalData, ONLY: CHAR_LF
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetParam()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: ii, tfedof

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (PRESENT(isInitiated)) obj%isInit = isInitiated
IF (PRESENT(fieldType)) obj%fieldType = fieldType
IF (PRESENT(name)) obj%name = TRIM(name)
IF (PRESENT(engine)) obj%engine = TRIM(engine)
IF (PRESENT(comm)) obj%comm = comm
IF (PRESENT(myRank)) obj%myRank = myRank
IF (PRESENT(numProcs)) obj%numProcs = numProcs
IF (PRESENT(global_n)) obj%global_n = global_n
IF (PRESENT(local_n)) obj%local_n = local_n
IF (PRESENT(is)) obj%is = is
IF (PRESENT(ie)) obj%ie = ie
IF (PRESENT(lis_ptr)) obj%lis_ptr = lis_ptr
IF (PRESENT(fedof)) obj%fedof => fedof

IF (PRESENT(fedofs)) THEN

#ifdef DEBUG_VER
  isok = ALLOCATED(obj%fedofs)
  CALL AssertError1(isok, myName, &
                    'AbstractField_::Obj%fedofs is not allocated ')
#endif

#ifdef DEBUG_VER
  tfedof = SIZE(obj%fedofs)
  ii = SIZE(fedofs)

  isok = tfedof .EQ. ii
  CALL AssertError1(isok, myName, &
                    'AbstractField_::Obj%fedofs '// &
                    CHAR_LF//'size is not same as size of fedofs')
#endif

  tfedof = SIZE(fedofs)
  DO ii = 1, tfedof
    obj%fedofs(ii)%ptr => fedofs(ii)%ptr
  END DO

END IF

!
!SELECT TYPE (obj)
!CLASS IS (AbstractNodeField_)
!  IF (PRESENT(tSize)) obj%tSize = tSize
!  IF (PRESENT(realVec)) obj%realVec = realVec
!  IF (PRESENT(dof)) obj%dof = dof
!CLASS IS (AbstractMatrixField_)
!  IF (PRESENT(isPMatInitiated)) obj%isPMatInitiated = isPMatInitiated
!END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetParam

!----------------------------------------------------------------------------
!                                                                     SetName
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetName
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetName()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%name = TRIM(name)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetName

!----------------------------------------------------------------------------
!                                                                      SetAll
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetAll
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetAll()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  'This routine should be implemented by child classes')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetAll

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

END SUBMODULE SetMethods
