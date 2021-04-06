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
  IF( ASSOCIATED( Obj%surrogate ) ) THEN
    Ans = ALL( Obj%surrogate%quiet )
  ELSE
    Ans = ALL( Obj%quiet )
  END IF
END PROCEDURE isQuietMode_all

!----------------------------------------------------------------------------
!                                                          isQuietMode_eCode
!----------------------------------------------------------------------------

MODULE PROCEDURE isQuietMode_eCode
  Ans=.FALSE.
  IF((EXCEPTION_OK < eCode) .AND. (eCode <= EXCEPTION_SIZE-1)) THEN
    Ans=Obj%quiet(eCode)
    IF(ASSOCIATED(Obj%surrogate)) Ans=Obj%surrogate%quiet(eCode)
  ENDIF
END PROCEDURE isQuietMode_eCode

!----------------------------------------------------------------------------
!                                                          isVerboseMode_all
!----------------------------------------------------------------------------

MODULE PROCEDURE isVerboseMode_all
  Ans = ALL( Obj%verbose )
  IF( ASSOCIATED( Obj%surrogate ) ) Ans=ALL( Obj%surrogate%verbose )
END PROCEDURE isVerboseMode_all

!----------------------------------------------------------------------------
!                                                        isVerboseMode_eCode
!----------------------------------------------------------------------------

MODULE PROCEDURE isVerboseMode_eCode
  Ans = .FALSE.
  IF( ( EXCEPTION_OK < eCode ) .AND. ( eCode <= EXCEPTION_SIZE-1 ) ) THEN
    Ans = Obj%verbose(eCode)
    IF( ASSOCIATED( Obj%surrogate ) ) Ans = Obj%surrogate%verbose(eCode)
  ENDIF
END PROCEDURE isVerboseMode_eCode

!----------------------------------------------------------------------------
!                                                                isLogActive
!----------------------------------------------------------------------------

MODULE PROCEDURE isLogActive
  Ans=.FALSE.
  IF( ASSOCIATED( Obj%surrogate ) ) THEN
    IF( Obj%surrogate%logFileActive ) CALL Obj%surrogate%checkLogFileOK()
    Ans=Obj%surrogate%logFileActive
  ELSE
    IF( Obj%logFileActive ) CALL Obj%checkLogFileOK()
    Ans=Obj%logFileActive
  ENDIF
END PROCEDURE isLogActive

!----------------------------------------------------------------------------
!                                                             checkLogFileOK
!----------------------------------------------------------------------------

MODULE PROCEDURE checkLogFileOK
  LOGICAL( LGT ) :: isOpen
  INTEGER( I4B ) :: nDebugOld
  CHARACTER( LEN=10 ) :: fprop

  IF( ASSOCIATED( Obj%surrogate ) ) THEN
    CALL checkLogFileOK( Obj%surrogate )
  ELSE
    !Since the state of the log file can change (e.g. closed) check it's
    !integrity
    nDebugOld=Obj%nDebug
    Obj%logFileActive=.FALSE.

    !Test if the file is open
    INQUIRE( UNIT = Obj%logFileUnit, OPENED=isOpen )
    IF( .NOT. isOpen ) CALL raiseDebug( Obj, 'Log file is not open! '// &
      & 'Log file status is inactive.' )

    !Test if the file is a formatted file
    INQUIRE( UNIT = Obj%logFileUnit, FORM=fprop )
    IF(TRIM(fprop) /= 'FORMATTED') CALL raiseDebug(Obj, &
      & 'Log file is not a formatted file! Log file status is inactive.')

    !Test if the file is sequential
    INQUIRE( UNIT = Obj%logFileUnit, ACCESS=fprop )
    IF(TRIM(fprop) /= 'SEQUENTIAL') CALL raiseDebug(Obj, &
      & 'Log file is not a sequential file! Log file status is inactive.')

    !Test if the file has been opened for writing
    INQUIRE( UNIT = Obj%logFileUnit, ACTION=fprop )
    IF( .NOT. ( TRIM(fprop) == 'WRITE' .OR. TRIM(fprop) == 'READWRITE' ) ) &
      & CALL raiseDebug( Obj,'Log file is not open for writing! '// &
      & 'Log file status is inactive.' )

    !If none of the checks produced a new warning then the log file check
    !passes the return value can be set to .TRUE. otherwise it is .FALSE.
    IF(nDebugOld == Obj%nDebug) Obj%logFileActive=.TRUE.
  ENDIF
END PROCEDURE checkLogFileOK

!----------------------------------------------------------------------------
!                                                              isStopOnError
!----------------------------------------------------------------------------

MODULE PROCEDURE isStopOnError
  Ans=Obj%stopOnError
  IF(ASSOCIATED(Obj%surrogate)) Ans=Obj%surrogate%stopOnError
END PROCEDURE isStopOnError

END SUBMODULE Enquire