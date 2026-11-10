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

SUBMODULE(AbstractNodeField_Class) IOMethods
USE AbstractField_Class, ONLY: AbstractFieldDisplay
USE Display_Method, ONLY: Display
USE Display_Method, ONLY: ToString
USE DOF_Method, ONLY: DOF_Display => Display
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: &
  modName = "AbstractNodeField_Class@IOMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                   Display
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

CALL AbstractFieldDisplay(obj=obj, msg=msg, unitNo=unitNo)

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
  CALL Display("timefedofs : ALLOCATED ["//ToString(tsize)//"]", &
               unitNo=unitNo)
END IF

! tsize is zero if not allocated, so it is safe to use
DO ii = 1, tsize
  isok = ASSOCIATED(obj%timefedofs(ii)%ptr)
  CALL Display(isok, "timefedofs("//ToString(ii)//")%ptr ASSOCIATED: ", &
               unitNo=unitNo)
END DO

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

CALL Display(obj%isMaxTotalNodeNumForBCSet, &
             "isMaxTotalNodeNumForBCSet: ", unitNo=unitNo)

CALL Display(obj%maxTotalNodeNumForBC, &
             "maxTotalNodeNumForBC: ", unitNo=unitNo)

isok = ALLOCATED(obj%nodalValue)
CALL Display(isok, "nodalValue ALLOCATED: ", unitNo=unitNo)
IF (isok) THEN
  CALL Display(SHAPE(obj%nodalValue), "Shape of nodalValue: ", &
               unitNo=unitNo)
END IF

isok = ALLOCATED(obj%nodeNum)
CALL Display(isok, "nodeNum ALLOCATED: ", unitNo=unitNo)
IF (isok) THEN
  CALL Display(SIZE(obj%nodeNum), "Size of nodeNum: ", &
               unitNo=unitNo)
END IF

CALL DOF_Display(obj=obj%dof, msg="dof: ", unitNo=unitNo)

CALL Display(obj%dof_tPhysicalVars, "dof_tPhysicalVars: ", unitNo=unitNo)
CALL Display(obj%dof_storageFMT, "dof_storageFMT: ", unitNo=unitNo)
CALL Display(obj%dof_spaceCompo, "dof_spaceCompo: ", unitNo=unitNo)
CALL Display(obj%dof_timeCompo, "dof_timeCompo: ", unitNo=unitNo)
CALL Display(obj%dof_tNodes, "dof_tNodes: ", unitNo=unitNo)
CALL Display(obj%tSize, "tSize: ", unitNo=unitNo)
CALL DOF_Display(obj%realVec, obj%dof, "realVec: ", unitNo=unitNo)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE IOMethods
