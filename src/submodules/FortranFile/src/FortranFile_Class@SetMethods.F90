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
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 setStatus
!----------------------------------------------------------------------------

MODULE PROCEDURE ff_setStatus
  CHARACTER(LEN=*), PARAMETER :: myName = 'ff_setStatus'
  TYPE(String) :: new_status

  new_status = status
  new_status = new_status%upper()
  SELECT CASE (new_status%chars())
  CASE ('OLD')
      !!File already exists
    obj%newstat = .FALSE.
    obj%overwrite = .FALSE.
  CASE ('NEW')
      !!File does not exist and will be created
    obj%newstat = .TRUE.
    obj%overwrite = .TRUE.
  CASE (  'SCRATCH', 'REPLACE', 'UNKNOWN')
    obj%newstat = .TRUE.
    obj%overwrite = .TRUE.
  CASE DEFAULT
    CALL e%raiseError(modName//'::'//myName//' - Illegal '// &
      & 'value ('//status//') for input argument STATUS!')
  END SELECT
END PROCEDURE ff_setStatus

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END SUBMODULE SetMethods
