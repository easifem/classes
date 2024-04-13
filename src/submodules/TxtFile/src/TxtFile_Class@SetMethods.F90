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

SUBMODULE(TxtFile_Class) SetMethods
USE ISO_FORTRAN_ENV, ONLY: IOSTAT_EOR, IOSTAT_END
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                               setEchoStat
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_setEchoStat
  obj%echostat = bool
END PROCEDURE txt_setEchoStat

!----------------------------------------------------------------------------
!                                                               setEchoUnit
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_setEchoUnit
  CHARACTER(LEN=*), PARAMETER :: myName = 'txt_setEchoUnit'
  !!
  IF( (0 .LT. unitno) &
    & .AND. (unitno .NE. stdout) &
    & .AND. (unitno .NE. stderr)) THEN
    obj%echounit = unitno
  ELSE
    CALL e%raiseError('Incorrect input to '//modName//'::'// &
      & myName//' - Illegal value for unit number!')
  END IF
END PROCEDURE txt_setEchoUnit

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods
