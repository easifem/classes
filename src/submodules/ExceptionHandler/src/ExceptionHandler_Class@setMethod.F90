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

SUBMODULE( ExceptionHandler_Class ) setMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                               addSurrogate
!----------------------------------------------------------------------------


MODULE PROCEDURE addSurrogate
  obj%surrogate => obj2
  IF(ASSOCIATED(obj2%surrogate)) obj%surrogate => obj2%surrogate
END PROCEDURE addSurrogate

!----------------------------------------------------------------------------
!                                                                 setCounter
!----------------------------------------------------------------------------


MODULE PROCEDURE setCounter_All
  INTEGER( I4B ) :: lcounter(EXCEPTION_SIZE)
  lcounter=counter
  WHERE(counter < 0) lcounter=0
  IF(ASSOCIATED(obj%surrogate)) THEN
    obj%surrogate%nInfo=lcounter(EXCEPTION_INFORMATION)
    obj%surrogate%nWarn=lcounter(EXCEPTION_WARNING)
    obj%surrogate%nError=lcounter(EXCEPTION_ERROR)
    obj%surrogate%nFatal=lcounter(EXCEPTION_FATAL_ERROR)
    obj%surrogate%nDebug=lcounter(EXCEPTION_DEBUG)
  ELSE
    obj%nInfo=lcounter(EXCEPTION_INFORMATION)
    obj%nWarn=lcounter(EXCEPTION_WARNING)
    obj%nError=lcounter(EXCEPTION_ERROR)
    obj%nFatal=lcounter(EXCEPTION_FATAL_ERROR)
    obj%nDebug=lcounter(EXCEPTION_DEBUG)
  ENDIF
END PROCEDURE setCounter_All

!----------------------------------------------------------------------------
!                                                                 setCounter
!----------------------------------------------------------------------------

MODULE PROCEDURE setCounter_eCode
  INTEGER( I4B ) :: lcount
  lcount=0
  IF(count > 0) lcount=count
  IF(ASSOCIATED(obj%surrogate)) THEN
    SELECTCASE(i)
    CASE(EXCEPTION_INFORMATION)
      obj%surrogate%nInfo=lcount
    CASE(EXCEPTION_WARNING)
      obj%surrogate%nWarn=lcount
    CASE(EXCEPTION_DEBUG)
      obj%surrogate%nDebug=lcount
    CASE(EXCEPTION_ERROR)
      obj%surrogate%nError=lcount
    CASE(EXCEPTION_FATAL_ERROR)
      obj%surrogate%nFatal=lcount
    ENDSELECT
  ELSE
    SELECTCASE(i)
    CASE(EXCEPTION_INFORMATION)
      obj%nInfo=lcount
    CASE(EXCEPTION_WARNING)
      obj%nWarn=lcount
    CASE(EXCEPTION_DEBUG)
      obj%nDebug=lcount
    CASE(EXCEPTION_ERROR)
      obj%nError=lcount
    CASE(EXCEPTION_FATAL_ERROR)
      obj%nFatal=lcount
    ENDSELECT
  ENDIF
END PROCEDURE setCounter_eCode

!----------------------------------------------------------------------------
!                                                             setQuietMode
!----------------------------------------------------------------------------

MODULE PROCEDURE setQuietMode_all
  IF(ASSOCIATED(obj%surrogate)) CALL copyFromSurrogate(obj)
  obj%quiet=ans
END PROCEDURE setQuietMode_all

!----------------------------------------------------------------------------
!                                                             setQuietMode
!----------------------------------------------------------------------------

MODULE PROCEDURE setQuietMode_eCode
  IF( ASSOCIATED( obj%surrogate ) ) CALL copyFromSurrogate( obj )
  IF( EXCEPTION_OK < eCode .AND. eCode < EXCEPTION_SIZE) &
    obj%quiet( eCode ) = ans
END PROCEDURE setQuietMode_eCode

!----------------------------------------------------------------------------
!                                                             setQuietMode
!----------------------------------------------------------------------------

MODULE PROCEDURE setQuietMode_array
  INTEGER( I4B ) :: n
  IF( ASSOCIATED( obj%surrogate ) ) CALL copyFromSurrogate( obj )
  n = MIN( EXCEPTION_SIZE-1, SIZE( ans ) )
  obj%quiet( 1:n ) = ans( 1:n )
END PROCEDURE setQuietMode_array

!----------------------------------------------------------------------------
!                                                                 setVerbose
!----------------------------------------------------------------------------

MODULE PROCEDURE setVerboseMode_all
  IF( ASSOCIATED( obj%surrogate ) ) CALL copyFromSurrogate( obj )
  obj%verbose=ans
END PROCEDURE setVerboseMode_all

!----------------------------------------------------------------------------
!                                                                 setVerbose
!----------------------------------------------------------------------------

MODULE PROCEDURE setVerboseMode_eCode
  IF( ASSOCIATED( obj%surrogate ) ) CALL copyFromSurrogate( obj )
  IF( EXCEPTION_OK < eCode .AND. eCode < EXCEPTION_SIZE ) &
    obj%verbose(eCode)=ans
END PROCEDURE setVerboseMode_eCode

!----------------------------------------------------------------------------
!                                                                 setVerbose
!----------------------------------------------------------------------------

MODULE PROCEDURE setVerboseMode_array
  INTEGER( I4B ) :: n
  IF( ASSOCIATED( obj%surrogate ) ) CALL copyFromSurrogate( obj )
  n = MIN( EXCEPTION_SIZE-1, SIZE( ans ) )
  obj%verbose( 1:n ) = ans( 1:n )
END PROCEDURE setVerboseMode_array

!----------------------------------------------------------------------------
!                                                             setLogFileUnit
!----------------------------------------------------------------------------

MODULE PROCEDURE setLogFileUnit
  LOGICAL( LGT ) :: tmpQuiet
  IF( ASSOCIATED( obj%surrogate ) ) CALL copyFromSurrogate( obj )

  !Try to set the log file unit number. Check that it is a valid
  !value. If not display a warning.
  IF( unit /= stdout .AND. unit /= stderr .AND. unit > 0 ) THEN
    obj%logFileUnit=unit
  ELSE
    obj%lastMesg="Illegal unit number for log file. " // &
      & "Log file unit not set."
    tmpQuiet=.FALSE.
    obj%nWarn=obj%nWarn+1
    CALL exceptionMessage( EXCEPTION_WARNING, tmpQuiet, .FALSE., &
      & stderr, obj%lastMesg )
  END IF
END PROCEDURE setLogFileUnit

!----------------------------------------------------------------------------
!                                                              setLogActive
!----------------------------------------------------------------------------

MODULE PROCEDURE setLogActive
  IF( ASSOCIATED( obj%surrogate ) ) CALL copyFromSurrogate( obj )
  IF( isactive ) THEN
    CALL obj%checkLogFileOK()
    obj%logFileActive=.TRUE.
  ELSE
    obj%logFileActive=.FALSE.
  END IF
END PROCEDURE setLogActive

!----------------------------------------------------------------------------
!                                                             setStopOnError
!----------------------------------------------------------------------------

MODULE PROCEDURE setStopOnError
  IF( ASSOCIATED( obj%surrogate ) ) CALL copyFromSurrogate( obj )
  obj%stopOnError=ans
END PROCEDURE setStopOnError


END SUBMODULE SetMethod