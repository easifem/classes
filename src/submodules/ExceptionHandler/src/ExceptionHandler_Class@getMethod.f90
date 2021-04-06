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
  Obj2 => Obj%surrogate
END PROCEDURE getSurrogate

!----------------------------------------------------------------------------
!                                                                 getCounter
!----------------------------------------------------------------------------

MODULE PROCEDURE getCounterAll
  IF(ASSOCIATED(Obj%surrogate)) THEN
    Ans(EXCEPTION_INFORMATION)=Obj%surrogate%nInfo
    Ans(EXCEPTION_WARNING)=Obj%surrogate%nWarn
    Ans(EXCEPTION_DEBUG)=Obj%surrogate%nDebug
    Ans(EXCEPTION_ERROR)=Obj%surrogate%nError
    Ans(EXCEPTION_FATAL_ERROR)=Obj%surrogate%nFatal
  ELSE
    Ans(EXCEPTION_INFORMATION)=Obj%nInfo
    Ans(EXCEPTION_WARNING)=Obj%nWarn
    Ans(EXCEPTION_DEBUG)=Obj%nDebug
    Ans(EXCEPTION_ERROR)=Obj%nError
    Ans(EXCEPTION_FATAL_ERROR)=Obj%nFatal
  ENDIF
END PROCEDURE getCounterAll

!----------------------------------------------------------------------------
!                                                                 getCounter
!----------------------------------------------------------------------------

MODULE PROCEDURE getCounter
  Ans=-1
  IF(ASSOCIATED(Obj%surrogate)) THEN
    SELECTCASE(i)
    CASE(EXCEPTION_INFORMATION)
      Ans=Obj%surrogate%nInfo
    CASE(EXCEPTION_WARNING)
      Ans=Obj%surrogate%nWarn
    CASE(EXCEPTION_DEBUG)
      Ans=Obj%surrogate%nDebug
    CASE(EXCEPTION_ERROR)
      Ans=Obj%surrogate%nError
    CASE(EXCEPTION_FATAL_ERROR)
      Ans=Obj%surrogate%nFatal
    ENDSELECT
  ELSE
    SELECTCASE(i)
    CASE(EXCEPTION_INFORMATION)
      Ans=Obj%nInfo
    CASE(EXCEPTION_WARNING)
      Ans=Obj%nWarn
    CASE(EXCEPTION_DEBUG)
      Ans=Obj%nDebug
    CASE(EXCEPTION_ERROR)
      Ans=Obj%nError
    CASE(EXCEPTION_FATAL_ERROR)
      Ans=Obj%nFatal
    ENDSELECT
  ENDIF
END PROCEDURE getCounter

!----------------------------------------------------------------------------
!                                                             getLastMessage
!----------------------------------------------------------------------------

MODULE PROCEDURE getLastMessage
  Ans=Obj%lastMesg
  IF(ASSOCIATED(Obj%surrogate)) Ans=Obj%surrogate%lastMesg
END PROCEDURE getLastMessage

!----------------------------------------------------------------------------
!                                                            getLogFileUnit
!----------------------------------------------------------------------------

MODULE PROCEDURE getLogFileUnit
  Ans=Obj%logFileUnit
  IF(ASSOCIATED(Obj%surrogate)) Ans=Obj%surrogate%logFileUnit
END PROCEDURE getLogFileUnit

END SUBMODULE getMethod