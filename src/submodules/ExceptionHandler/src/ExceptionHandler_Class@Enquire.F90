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

SUBMODULE( ExceptionHandler_Class ) Enquire
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                           isQuietMode_all
!----------------------------------------------------------------------------

MODULE PROCEDURE isQuietMode_all
  IF( ASSOCIATED( obj%surrogate ) ) THEN
    ans = ALL( obj%surrogate%quiet )
  ELSE
    ans = ALL( obj%quiet )
  END IF
END PROCEDURE isQuietMode_all

!----------------------------------------------------------------------------
!                                                          isQuietMode_eCode
!----------------------------------------------------------------------------

MODULE PROCEDURE isQuietMode_eCode
  ans=.FALSE.
  IF((EXCEPTION_OK < eCode) .AND. (eCode <= EXCEPTION_SIZE-1)) THEN
    ans=obj%quiet(eCode)
    IF(ASSOCIATED(obj%surrogate)) ans=obj%surrogate%quiet(eCode)
  ENDIF
END PROCEDURE isQuietMode_eCode

!----------------------------------------------------------------------------
!                                                          isVerboseMode_all
!----------------------------------------------------------------------------

MODULE PROCEDURE isVerboseMode_all
  ans = ALL( obj%verbose )
  IF( ASSOCIATED( obj%surrogate ) ) ans=ALL( obj%surrogate%verbose )
END PROCEDURE isVerboseMode_all

!----------------------------------------------------------------------------
!                                                        isVerboseMode_eCode
!----------------------------------------------------------------------------

MODULE PROCEDURE isVerboseMode_eCode
  ans = .FALSE.
  IF( ( EXCEPTION_OK < eCode ) .AND. ( eCode <= EXCEPTION_SIZE-1 ) ) THEN
    ans = obj%verbose(eCode)
    IF( ASSOCIATED( obj%surrogate ) ) ans = obj%surrogate%verbose(eCode)
  ENDIF
END PROCEDURE isVerboseMode_eCode

!----------------------------------------------------------------------------
!                                                                isLogActive
!----------------------------------------------------------------------------

MODULE PROCEDURE isLogActive
  ans=.FALSE.
  IF( ASSOCIATED( obj%surrogate ) ) THEN
    IF( obj%surrogate%logFileActive ) CALL obj%surrogate%checkLogFileOK()
    ans=obj%surrogate%logFileActive
  ELSE
    IF( obj%logFileActive ) CALL obj%checkLogFileOK()
    ans=obj%logFileActive
  ENDIF
END PROCEDURE isLogActive

!----------------------------------------------------------------------------
!                                                             checkLogFileOK
!----------------------------------------------------------------------------

MODULE PROCEDURE checkLogFileOK
  LOGICAL( LGT ) :: isOpen
  INTEGER( I4B ) :: nDebugOld
  CHARACTER( LEN=10 ) :: fprop

  IF( ASSOCIATED( obj%surrogate ) ) THEN
    CALL checkLogFileOK( obj%surrogate )
  ELSE
    !Since the state of the log file can change (e.g. closed) check it's
    !integrity
    nDebugOld=obj%nDebug
    obj%logFileActive=.FALSE.

    !Test if the file is open
    INQUIRE( UNIT = obj%logFileUnit, OPENED=isOpen )
    IF( .NOT. isOpen ) CALL raiseDebug( obj, 'Log file is not open! '// &
      & 'Log file status is inactive.' )

    !Test if the file is a formatted file
    INQUIRE( UNIT = obj%logFileUnit, FORM=fprop )
    IF(TRIM(fprop) /= 'FORMATTED') CALL raiseDebug(obj, &
      & 'Log file is not a formatted file! Log file status is inactive.')

    !Test if the file is sequential
    INQUIRE( UNIT = obj%logFileUnit, ACCESS=fprop )
    IF(TRIM(fprop) /= 'SEQUENTIAL') CALL raiseDebug(obj, &
      & 'Log file is not a sequential file! Log file status is inactive.')

    !Test if the file has been opened for writing
    INQUIRE( UNIT = obj%logFileUnit, ACTION=fprop )
    IF( .NOT. ( TRIM(fprop) == 'WRITE' .OR. TRIM(fprop) == 'READWRITE' ) ) &
      & CALL raiseDebug( obj,'Log file is not open for writing! '// &
      & 'Log file status is inactive.' )

    !If none of the checks produced a new warning then the log file check
    !passes the return value can be set to .TRUE. otherwise it is .FALSE.
    IF(nDebugOld == obj%nDebug) obj%logFileActive=.TRUE.
  ENDIF
END PROCEDURE checkLogFileOK

!----------------------------------------------------------------------------
!                                                              isStopOnError
!----------------------------------------------------------------------------

MODULE PROCEDURE isStopOnError
  ans=obj%stopOnError
  IF(ASSOCIATED(obj%surrogate)) ans=obj%surrogate%stopOnError
END PROCEDURE isStopOnError

END SUBMODULE Enquire