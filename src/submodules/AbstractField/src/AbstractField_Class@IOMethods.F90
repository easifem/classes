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
USE Display_Method, ONLY: Display
USE FieldOpt_Class, ONLY: TypeField => TypeFieldOpt
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "AbstractField_Class@IOMethods.F90"
#endif
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Display
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

CALL Display(msg, unitNo=unitNo)

isok = obj%IsInitiated()
CALL Display(isok, msg="isInitiated: ", unitNo=unitNo)
IF (.NOT. isok) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

CALL Display(obj%name%Chars(), msg="name : ", unitNo=unitNo)

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
