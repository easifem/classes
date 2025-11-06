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

SUBMODULE(AbstractField_Class) IOMethods
USE Display_Method, ONLY: Display, ToString
USE FieldOpt_Class, ONLY: TypeField => TypeFieldOpt

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display()"
#endif

INTEGER(I4B) :: ii, tsize
LOGICAL(LGT) :: isok, abool

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Display(msg, unitNo=unitNo)

isok = obj%IsInitiated()
CALL Display(isok, msg="isInitiated: ", unitNo=unitNo)
IF (.NOT. isok) RETURN

CALL Display(obj%name%chars(), msg="name : ", unitNo=unitNo)

CALL Display(TypeField%ToString(obj%fieldType), msg='fieldType : ', &
             unitNo=unitNo)

CALL Display(obj%engine%chars(), msg='engine : ', unitNo=unitNo)
CALL Display(obj%comm, msg='comm: ', unitNo=unitNo)
CALL Display(obj%myRank, msg='myRank: ', unitNo=unitNo)
CALL Display(obj%numProcs, msg='numProcs: ', unitNo=unitNo)
CALL Display(obj%global_n, msg='global_n: ', unitNo=unitNo)
CALL Display(obj%local_n, msg='local_n: ', unitNo=unitNo)
CALL Display(obj%is, msg='is: ', unitNo=unitNo)
CALL Display(obj%ie, msg='ie: ', unitNo=unitNo)
CALL Display(obj%lis_ptr, msg='lis_ptr: ', unitNo=unitNo)

isok = ASSOCIATED(obj%fedof)
CALL Display(isok, "fedof ASSOCIATED: ", unitNo=unitNo)

isok = ASSOCIATED(obj%timefedof)
CALL Display(isok, "timefedof ASSOCIATED: ", unitNo=unitNo)

isok = ALLOCATED(obj%fedofs)
CALL Display(isok, "fedofs ALLOCATED: ", unitNo=unitNo)
tsize = 0
IF (isok) THEN
  tsize = SIZE(obj%fedofs)
  CALL Display("fedofs : ALLOCATED ["//ToString(tsize)//"]", unitNo=unitNo)
END IF

! tsize is zero if not allocated, so it is safe to use
DO ii = 1, tsize
  isok = ASSOCIATED(obj%fedofs(ii)%ptr)
  CALL Display(isok, "fedofs("//ToString(ii)//")%ptr ASSOCIATED: ", &
               unitNo=unitNo)
END DO

isok = ALLOCATED(obj%timefedofs)
CALL Display(isok, "timefedofs ALLOCATED: ", unitNo=unitNo)
tsize = 0
IF (isok) THEN
  tsize = SIZE(obj%timefedofs)
 CALL Display("timefedofs : ALLOCATED ["//ToString(tsize)//"]", unitNo=unitNo)
END IF

! tsize is zero if not allocated, so it is safe to use
DO ii = 1, tsize
  isok = ASSOCIATED(obj%timefedofs(ii)%ptr)
  CALL Display(isok, "timefedofs("//ToString(ii)//")%ptr ASSOCIATED: ", &
               unitNo=unitNo)
END DO

isok = ASSOCIATED(obj%exact)
CALL Display(isok, "Userfunction exact is associated: ", unitNo=unitNo)
IF (isok) THEN
  CALL obj%exact%Display(msg="exact: ", unitNo=unitNo)
END IF

CALL Display(obj%saveErrorNorm, "saveErrorNorm: ", unitNo=unitNo)
CALL Display(obj%errorType, "errorType: ", unitNo=unitNo)
CALL Display(obj%plotWithResult, "plotWithResult: ", unitNo=unitNo)
CALL Display(obj%plotErrorNorm, "plotErrorNorm: ", unitNo=unitNo)

isok = ALLOCATED(obj%dbc)
CALL Display(isok, "dbc ALLOCATED: ", unitNo=unitNo)
IF (isok) THEN
  tsize = SIZE(obj%dbc)
  CALL Display(tsize, "Size of dbc: ", unitNo=unitNo)
  DO ii = 1, tsize
    abool = ASSOCIATED(obj%dbc(ii)%ptr)
    CALL Display(abool, "dbc("//ToString(ii)//")%ptr ASSOCIATED: ", &
                 unitNo=unitNo)
  END DO
END IF

isok = ALLOCATED(obj%nbc)
CALL Display(isok, "nbc ALLOCATED: ", unitNo=unitNo)
IF (isok) THEN
  tsize = SIZE(obj%nbc)
  CALL Display(tsize, "Size of nbc: ", unitNo=unitNo)
  DO ii = 1, tsize
    abool = ASSOCIATED(obj%nbc(ii)%ptr)
    CALL Display(abool, "nbc("//ToString(ii)//")%ptr ASSOCIATED: ", &
                 unitNo=unitNo)
  END DO
END IF

isok = ALLOCATED(obj%nbc_point)
CALL Display(isok, "nbc_point ALLOCATED: ", unitNo=unitNo)
IF (isok) THEN
  tsize = SIZE(obj%nbc_point)
  CALL Display(tsize, "Size of nbc_point: ", unitNo=unitNo)
  DO ii = 1, tsize
    abool = ASSOCIATED(obj%nbc_point(ii)%ptr)
    CALL Display(abool, "nbc_point("//ToString(ii)//")%ptr ASSOCIATED: ", &
                 unitNo=unitNo)
  END DO
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                              Include Errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE IOMethods
