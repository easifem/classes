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

SUBMODULE(MatrixFieldLis_Class) IOMethods
USE MatrixField_Class, ONLY: MatrixFieldDisplay
USE Display_Method, ONLY: Display

IMPLICIT NONE

#include "lisf.h"

CONTAINS

!----------------------------------------------------------------------------
!                                                                  Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL MatrixFieldDisplay(obj=obj, msg=msg, unitno=unitno)

IF (obj%engine%chars() .EQ. "LIS_OMP") THEN

  isok = ALLOCATED(obj%lis_ia)
  IF (isok) THEN
    CALL Display("lis_ia ALLOCATED", unitNo=unitNo)
  ELSE
    CALL Display("lis_ia NOT ALLOCATED", unitNo=unitNo)
  END IF

  isok = ALLOCATED(obj%lis_ja)
  IF (isok) THEN
    CALL Display("lis_ja ALLOCATED", unitNo=unitNo)
  ELSE
    CALL Display("lis_ja NOT ALLOCATED", unitNo=unitNo)
  END IF

END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE IOMethods
