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

SUBMODULE(NeumannBC_Class) IOMethods
USE Display_Method, ONLY: Display, ToString
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display_Vector
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display_Vector()"
#endif

INTEGER(I4B) :: tsize, ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = SIZE(obj)
CALL Display(msg, unitNo=unitNo)
CALL Display("nbc: SIZE["//ToString(tsize)//']', unitNo=unitNo)
DO ii = 1, tsize
  CALL obj(ii)%Display("nbc("//ToString(ii)//"): ", unitNo=unitNo)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Display_Vector

!----------------------------------------------------------------------------
!                                                                   Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display_Ptr_Vector
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display_Ptr_Vector()"
#endif

INTEGER(I4B) :: tsize, ii
LOGICAL(LGT) :: bool1

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = SIZE(obj)
CALL Display(msg, unitNo=unitNo)
CALL Display("nbc: SIZE["//ToString(tsize)//']', unitNo=unitNo)
DO ii = 1, tsize
  bool1 = ASSOCIATED(obj(ii)%ptr)
  CALL Display(bool1, "nbc("//ToString(ii)//") ASSOCIATED: ", unitNo=unitNo)
  IF (bool1) THEN
    CALL obj(ii)%ptr%Display("nbc("//ToString(ii)//"): ", unitNo=unitNo)
  END IF
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Display_Ptr_Vector

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE IOMethods
