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

SUBMODULE(FortranFile_Class) SetMethods
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = 'FortranFile_Class@SetMethods.F90'
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                 SetStatus
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetStatus
#ifdef DEBUG_VER
CHARACTER(LEN=*), PARAMETER :: myName = 'obj_SetStatus()'
#endif

TYPE(String) :: new_status

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

new_status = status
new_status = new_status%upper()

SELECT CASE (new_status%chars())
CASE (fileopt%old)
  !!File already exists
  obj%newstat = math%no
  obj%overwrite = math%no

CASE (fileopt%new)
  !!File does not exist and will be created
  obj%newstat = math%yes
  obj%overwrite = math%yes

! CASE ('SCRATCH', 'REPLACE', 'UNKNOWN')
CASE (fileopt%scratch, fileopt%replace, fileopt%unknown)
  obj%newstat = math%yes
  obj%overwrite = math%yes

CASE DEFAULT

#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, &
                    'value ('//status//') for input argument STATUS!')
#endif
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetStatus

!----------------------------------------------------------------------------
!                                                             Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SetMethods
