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

SUBMODULE(TxtFile_Class) GetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                               getEchoStat
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_getEchoStat
  ans = obj%echostat
END PROCEDURE txt_getEchoStat

!----------------------------------------------------------------------------
!                                                               getEchoUnit
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_getEchoUnit
  ans = obj%echounit
END PROCEDURE txt_getEchoUnit

!----------------------------------------------------------------------------
!                                                            GetTotalRecords
!----------------------------------------------------------------------------

MODULE PROCEDURE txt_getTotalRecords
  TYPE(String) :: aline
  INTEGER( I4B ) :: iostat
  !!
  !! check
  !!
  IF (.NOT. obj%isInitiated() .OR. &
    & .NOT. obj%isOpen() .OR. &
    & .NOT. obj%isRead()) THEN
    ans = 0; RETURN
  END IF
  !!
  !! main
  !!
  ans = 0
  CALL obj%REWIND()
  DO
    CALL obj%readLine(val=aline, iostat=iostat )
    IF ( obj%isEOF() ) EXIT
    IF( obj%isValidRecord( aline=aline, &
      & ignoreBlank=ignoreBlank, &
      & ignoreComment=ignoreComment, &
      & commentSymbol=commentSymbol ) ) ans = ans + 1
  END DO
  CALL obj%REWIND()
  !!
END PROCEDURE txt_getTotalRecords

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END SUBMODULE GetMethods
