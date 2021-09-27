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

SUBMODULE( ExceptionHandler_Class ) raiseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                           raiseInformation
!----------------------------------------------------------------------------


MODULE PROCEDURE raiseInformation
  LOGICAL( LGT ) :: toLog
  IF( ASSOCIATED( obj%surrogate ) ) THEN
    obj%surrogate%nInfo=obj%surrogate%nInfo+1
    obj%surrogate%lastMesg=mesg
    toLog=(obj%surrogate%logFileActive .AND. &
      & obj%surrogate%verbose(EXCEPTION_INFORMATION))
    CALL exceptionMessage( &
      & eCode = EXCEPTION_INFORMATION, &
      & isQuiet = obj%surrogate%quiet(EXCEPTION_INFORMATION), &
      & isLogActive = toLog, &
      & logUnit = obj%surrogate%logFileUnit, &
      & mesg = obj%surrogate%lastMesg )
  ELSE
    obj%nInfo = obj%nInfo+1
    obj%lastMesg = mesg
    toLog=( obj%logFileActive .AND. obj%verbose(EXCEPTION_INFORMATION))
    CALL exceptionMessage( &
      & eCode = EXCEPTION_INFORMATION, &
      & isQuiet = obj%quiet(EXCEPTION_INFORMATION), &
      & isLogActive = toLog, &
      & logUnit = obj%logFileUnit, &
      & mesg = obj%lastMesg )
  ENDIF
END PROCEDURE raiseInformation

!----------------------------------------------------------------------------
!                                                               raiseWarning
!----------------------------------------------------------------------------


MODULE PROCEDURE raiseWarning
  LOGICAL( LGT ) :: toLog
  IF( ASSOCIATED( obj%surrogate ) ) THEN
    obj%surrogate%nWarn=obj%surrogate%nWarn+1
    obj%surrogate%lastMesg=mesg
    toLog=(obj%surrogate%logFileActive .AND. &
      & obj%surrogate%verbose(EXCEPTION_WARNING))
    CALL exceptionMessage( &
      & eCode = EXCEPTION_WARNING, &
      & isQuiet = obj%surrogate%quiet(EXCEPTION_WARNING), &
      & isLogActive = toLog, &
      & logUnit = obj%surrogate%logFileUnit, &
      & mesg = obj%surrogate%lastMesg )
  ELSE
    obj%nWarn = obj%nWarn+1
    obj%lastMesg = mesg
    toLog=( obj%logFileActive .AND. obj%verbose(EXCEPTION_WARNING))
    CALL exceptionMessage( &
      & eCode = EXCEPTION_WARNING, &
      & isQuiet = obj%quiet(EXCEPTION_WARNING), &
      & isLogActive = toLog, &
      & logUnit = obj%logFileUnit, &
      & mesg = obj%lastMesg )
  ENDIF
END PROCEDURE raiseWarning

!----------------------------------------------------------------------------
!                                                                 raiseDebug
!----------------------------------------------------------------------------


MODULE PROCEDURE raiseDebug
  LOGICAL( LGT ) :: toLog
  IF( ASSOCIATED( obj%surrogate ) ) THEN
    obj%surrogate%nDebug=obj%surrogate%nDebug+1
    obj%surrogate%lastMesg=mesg
    toLog=(obj%surrogate%logFileActive .AND. &
      & obj%surrogate%verbose(EXCEPTION_DEBUG))
    CALL exceptionMessage( &
      & eCode = EXCEPTION_DEBUG, &
      & isQuiet = obj%surrogate%quiet(EXCEPTION_DEBUG), &
      & isLogActive = toLog, &
      & logUnit = obj%surrogate%logFileUnit, &
      & mesg = obj%surrogate%lastMesg )
  ELSE
    obj%nDebug = obj%nDebug+1
    obj%lastMesg = mesg
    toLog=( obj%logFileActive .AND. obj%verbose(EXCEPTION_DEBUG))
    CALL exceptionMessage( &
      & eCode = EXCEPTION_DEBUG, &
      & isQuiet = obj%quiet(EXCEPTION_DEBUG), &
      & isLogActive = toLog, &
      & logUnit = obj%logFileUnit, &
      & mesg = obj%lastMesg )
  ENDIF
END PROCEDURE raiseDebug

!----------------------------------------------------------------------------
!                                                                 raiseError
!----------------------------------------------------------------------------


MODULE PROCEDURE raiseError
  LOGICAL( LGT ) :: toLog
  IF( ASSOCIATED( obj%surrogate ) ) THEN
    obj%surrogate%nError=obj%surrogate%nError+1
    obj%surrogate%lastMesg=mesg
    toLog=(obj%surrogate%logFileActive .AND. &
      & obj%surrogate%verbose(EXCEPTION_ERROR))
    CALL exceptionMessage( &
      & eCode = EXCEPTION_ERROR, &
      & isQuiet = obj%surrogate%quiet(EXCEPTION_ERROR), &
      & isLogActive = toLog, &
      & logUnit = obj%surrogate%logFileUnit, &
      & mesg = obj%surrogate%lastMesg )
    CALL exceptionStop(obj%surrogate%stopOnError)
  ELSE
    obj%nError = obj%nError+1
    obj%lastMesg = mesg
    toLog=( obj%logFileActive .AND. obj%verbose(EXCEPTION_ERROR))
    CALL exceptionMessage( &
      & eCode = EXCEPTION_ERROR, &
      & isQuiet = obj%quiet(EXCEPTION_ERROR), &
      & isLogActive = toLog, &
      & logUnit = obj%logFileUnit, &
      & mesg = obj%lastMesg )
    CALL exceptionStop(obj%stopOnError)
  ENDIF
END PROCEDURE raiseError

!----------------------------------------------------------------------------
!                                                            raiseFatalError
!----------------------------------------------------------------------------

MODULE PROCEDURE raiseFatalError
  LOGICAL( LGT ) :: tmpQuiet
  tmpQuiet=.FALSE.

  IF( ASSOCIATED( obj%surrogate ) ) THEN
    obj%surrogate%nFatal=obj%surrogate%nFatal+1
    obj%surrogate%lastMesg=mesg
    CALL exceptionMessage( &
      & eCode = EXCEPTION_FATAL_ERROR, &
      & isQuiet = tmpQuiet, &
      & isLogActive = obj%surrogate%logFileActive, &
      & logUnit = obj%surrogate%logFileUnit, &
      & mesg = obj%surrogate%lastMesg )
  ELSE
    obj%nFatal = obj%nFatal+1
    obj%lastMesg = mesg
    CALL exceptionMessage( &
      & eCode = EXCEPTION_FATAL_ERROR, &
      & isQuiet = tmpQuiet, &
      & isLogActive = obj%logFileActive, &
      & logUnit = obj%logFileUnit, &
      & mesg = obj%lastMesg )
  ENDIF
  CALL exceptionStop(.TRUE.)
END PROCEDURE raiseFatalError

END SUBMODULE raiseMethod