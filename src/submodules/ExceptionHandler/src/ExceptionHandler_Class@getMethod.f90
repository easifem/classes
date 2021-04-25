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

SUBMODULE( ExceptionHandler_Class ) getMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                               addSurrogate
!----------------------------------------------------------------------------

MODULE PROCEDURE getSurrogate
  obj2 => obj%surrogate
END PROCEDURE getSurrogate

!----------------------------------------------------------------------------
!                                                                 getCounter
!----------------------------------------------------------------------------

MODULE PROCEDURE getCounterAll
  IF(ASSOCIATED(obj%surrogate)) THEN
    ans(EXCEPTION_INFORMATION)=obj%surrogate%nInfo
    ans(EXCEPTION_WARNING)=obj%surrogate%nWarn
    ans(EXCEPTION_DEBUG)=obj%surrogate%nDebug
    ans(EXCEPTION_ERROR)=obj%surrogate%nError
    ans(EXCEPTION_FATAL_ERROR)=obj%surrogate%nFatal
  ELSE
    ans(EXCEPTION_INFORMATION)=obj%nInfo
    ans(EXCEPTION_WARNING)=obj%nWarn
    ans(EXCEPTION_DEBUG)=obj%nDebug
    ans(EXCEPTION_ERROR)=obj%nError
    ans(EXCEPTION_FATAL_ERROR)=obj%nFatal
  ENDIF
END PROCEDURE getCounterAll

!----------------------------------------------------------------------------
!                                                                 getCounter
!----------------------------------------------------------------------------

MODULE PROCEDURE getCounter
  ans=-1
  IF(ASSOCIATED(obj%surrogate)) THEN
    SELECTCASE(i)
    CASE(EXCEPTION_INFORMATION)
      ans=obj%surrogate%nInfo
    CASE(EXCEPTION_WARNING)
      ans=obj%surrogate%nWarn
    CASE(EXCEPTION_DEBUG)
      ans=obj%surrogate%nDebug
    CASE(EXCEPTION_ERROR)
      ans=obj%surrogate%nError
    CASE(EXCEPTION_FATAL_ERROR)
      ans=obj%surrogate%nFatal
    ENDSELECT
  ELSE
    SELECTCASE(i)
    CASE(EXCEPTION_INFORMATION)
      ans=obj%nInfo
    CASE(EXCEPTION_WARNING)
      ans=obj%nWarn
    CASE(EXCEPTION_DEBUG)
      ans=obj%nDebug
    CASE(EXCEPTION_ERROR)
      ans=obj%nError
    CASE(EXCEPTION_FATAL_ERROR)
      ans=obj%nFatal
    ENDSELECT
  ENDIF
END PROCEDURE getCounter

!----------------------------------------------------------------------------
!                                                             getLastMessage
!----------------------------------------------------------------------------

MODULE PROCEDURE getLastMessage
  ans=obj%lastMesg
  IF(ASSOCIATED(obj%surrogate)) ans=obj%surrogate%lastMesg
END PROCEDURE getLastMessage

!----------------------------------------------------------------------------
!                                                            getLogFileUnit
!----------------------------------------------------------------------------

MODULE PROCEDURE getLogFileUnit
  ans=obj%logFileUnit
  IF(ASSOCIATED(obj%surrogate)) ans=obj%surrogate%logFileUnit
END PROCEDURE getLogFileUnit

END SUBMODULE getMethod