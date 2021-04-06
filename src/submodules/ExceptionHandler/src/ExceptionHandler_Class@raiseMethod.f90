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
  IF( ASSOCIATED( Obj%surrogate ) ) THEN
    Obj%surrogate%nInfo=Obj%surrogate%nInfo+1
    Obj%surrogate%lastMesg=mesg
    toLog=(Obj%surrogate%logFileActive .AND. &
      & Obj%surrogate%verbose(EXCEPTION_INFORMATION))
    CALL exceptionMessage( &
      & eCode = EXCEPTION_INFORMATION, &
      & isQuiet = Obj%surrogate%quiet(EXCEPTION_INFORMATION), &
      & isLogActive = toLog, &
      & logUnit = Obj%surrogate%logFileUnit, &
      & mesg = Obj%surrogate%lastMesg )
  ELSE
    Obj%nInfo = Obj%nInfo+1
    Obj%lastMesg = mesg
    toLog=( Obj%logFileActive .AND. Obj%verbose(EXCEPTION_INFORMATION))
    CALL exceptionMessage( &
      & eCode = EXCEPTION_INFORMATION, &
      & isQuiet = Obj%quiet(EXCEPTION_INFORMATION), &
      & isLogActive = toLog, &
      & logUnit = Obj%logFileUnit, &
      & mesg = Obj%lastMesg )
  ENDIF
END PROCEDURE raiseInformation

!----------------------------------------------------------------------------
!                                                               raiseWarning
!----------------------------------------------------------------------------


MODULE PROCEDURE raiseWarning
  LOGICAL( LGT ) :: toLog
  IF( ASSOCIATED( Obj%surrogate ) ) THEN
    Obj%surrogate%nWarn=Obj%surrogate%nWarn+1
    Obj%surrogate%lastMesg=mesg
    toLog=(Obj%surrogate%logFileActive .AND. &
      & Obj%surrogate%verbose(EXCEPTION_WARNING))
    CALL exceptionMessage( &
      & eCode = EXCEPTION_WARNING, &
      & isQuiet = Obj%surrogate%quiet(EXCEPTION_WARNING), &
      & isLogActive = toLog, &
      & logUnit = Obj%surrogate%logFileUnit, &
      & mesg = Obj%surrogate%lastMesg )
  ELSE
    Obj%nWarn = Obj%nWarn+1
    Obj%lastMesg = mesg
    toLog=( Obj%logFileActive .AND. Obj%verbose(EXCEPTION_WARNING))
    CALL exceptionMessage( &
      & eCode = EXCEPTION_WARNING, &
      & isQuiet = Obj%quiet(EXCEPTION_WARNING), &
      & isLogActive = toLog, &
      & logUnit = Obj%logFileUnit, &
      & mesg = Obj%lastMesg )
  ENDIF
END PROCEDURE raiseWarning

!----------------------------------------------------------------------------
!                                                                 raiseDebug
!----------------------------------------------------------------------------


MODULE PROCEDURE raiseDebug
  LOGICAL( LGT ) :: toLog
  IF( ASSOCIATED( Obj%surrogate ) ) THEN
    Obj%surrogate%nDebug=Obj%surrogate%nDebug+1
    Obj%surrogate%lastMesg=mesg
    toLog=(Obj%surrogate%logFileActive .AND. &
      & Obj%surrogate%verbose(EXCEPTION_DEBUG))
    CALL exceptionMessage( &
      & eCode = EXCEPTION_DEBUG, &
      & isQuiet = Obj%surrogate%quiet(EXCEPTION_DEBUG), &
      & isLogActive = toLog, &
      & logUnit = Obj%surrogate%logFileUnit, &
      & mesg = Obj%surrogate%lastMesg )
  ELSE
    Obj%nDebug = Obj%nDebug+1
    Obj%lastMesg = mesg
    toLog=( Obj%logFileActive .AND. Obj%verbose(EXCEPTION_DEBUG))
    CALL exceptionMessage( &
      & eCode = EXCEPTION_DEBUG, &
      & isQuiet = Obj%quiet(EXCEPTION_DEBUG), &
      & isLogActive = toLog, &
      & logUnit = Obj%logFileUnit, &
      & mesg = Obj%lastMesg )
  ENDIF
END PROCEDURE raiseDebug

!----------------------------------------------------------------------------
!                                                                 raiseError
!----------------------------------------------------------------------------


MODULE PROCEDURE raiseError
  LOGICAL( LGT ) :: toLog
  IF( ASSOCIATED( Obj%surrogate ) ) THEN
    Obj%surrogate%nError=Obj%surrogate%nError+1
    Obj%surrogate%lastMesg=mesg
    toLog=(Obj%surrogate%logFileActive .AND. &
      & Obj%surrogate%verbose(EXCEPTION_ERROR))
    CALL exceptionMessage( &
      & eCode = EXCEPTION_ERROR, &
      & isQuiet = Obj%surrogate%quiet(EXCEPTION_ERROR), &
      & isLogActive = toLog, &
      & logUnit = Obj%surrogate%logFileUnit, &
      & mesg = Obj%surrogate%lastMesg )
    CALL exceptionStop(Obj%surrogate%stopOnError)
  ELSE
    Obj%nError = Obj%nError+1
    Obj%lastMesg = mesg
    toLog=( Obj%logFileActive .AND. Obj%verbose(EXCEPTION_ERROR))
    CALL exceptionMessage( &
      & eCode = EXCEPTION_ERROR, &
      & isQuiet = Obj%quiet(EXCEPTION_ERROR), &
      & isLogActive = toLog, &
      & logUnit = Obj%logFileUnit, &
      & mesg = Obj%lastMesg )
    CALL exceptionStop(Obj%stopOnError)
  ENDIF
END PROCEDURE raiseError

!----------------------------------------------------------------------------
!                                                            raiseFatalError
!----------------------------------------------------------------------------

MODULE PROCEDURE raiseFatalError
  LOGICAL( LGT ) :: tmpQuiet
  tmpQuiet=.FALSE.

  IF( ASSOCIATED( Obj%surrogate ) ) THEN
    Obj%surrogate%nFatal=Obj%surrogate%nFatal+1
    Obj%surrogate%lastMesg=mesg
    CALL exceptionMessage( &
      & eCode = EXCEPTION_FATAL_ERROR, &
      & isQuiet = tmpQuiet, &
      & isLogActive = Obj%surrogate%logFileActive, &
      & logUnit = Obj%surrogate%logFileUnit, &
      & mesg = Obj%surrogate%lastMesg )
  ELSE
    Obj%nFatal = Obj%nFatal+1
    Obj%lastMesg = mesg
    CALL exceptionMessage( &
      & eCode = EXCEPTION_FATAL_ERROR, &
      & isQuiet = tmpQuiet, &
      & isLogActive = Obj%logFileActive, &
      & logUnit = Obj%logFileUnit, &
      & mesg = Obj%lastMesg )
  ENDIF
  CALL exceptionStop(.TRUE.)
END PROCEDURE raiseFatalError

END SUBMODULE raiseMethod