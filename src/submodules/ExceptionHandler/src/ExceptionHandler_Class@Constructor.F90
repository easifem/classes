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

!> authors: Vikas Sharma, Ph. D.
! date: 4 April 2021
! update: 4 Nov 2021
! summary: Constructor method for [[ExceptionHandler_]]

SUBMODULE(ExceptionHandler_Class) Constructor
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             exceptionStop
!----------------------------------------------------------------------------

MODULE PROCEDURE exceptionStop
#ifdef HAVE_MPI
INCLUDE 'mpif.h'
INTEGER(I4B) :: ierr
#endif
#ifdef HAVE_MPI
IF (stopmode) CALL MPI_Abort(MPI_COMM_WORLD, 666, ierr)
#else
IF (stopmode) THEN
  STOP 666
END IF
#endif
END PROCEDURE exceptionStop

!----------------------------------------------------------------------------
!                                                           exceptionMessage
!----------------------------------------------------------------------------

MODULE PROCEDURE exceptionMessage
! Define internal variables
TYPE(String) :: prefix
TYPE(String) :: modname
TYPE(String) :: myname
TYPE(String) :: msg
TYPE(String), ALLOCATABLE :: substr(:)
INTEGER(I4B) :: ioerr1, ioerr2, prefixLen, ii, nn
!> main
prefix = TRIM(mesg); myname = ""; modname = ""; msg = ""
IF (prefix%nmatchstr(":") .GT. 0) THEN
  CALL prefix%split(substr, "::")
  modname = substr(1)
  prefix = ""
  nn = SIZE(substr)
  DO ii = 2, nn - 1
    prefix = prefix//substr(ii)//"::"
  END DO
  prefix = prefix//substr(nn)
  DEALLOCATE (substr)
END IF
IF (prefix%nmatchstr("-") .GT. 0) THEN
  CALL prefix%split(substr, "-")
  nn = SIZE(substr)
  myname = substr(1)
  DO ii = 2, nn - 1
    msg = msg//substr(ii)//"-"
  END DO
  msg = msg//substr(nn)
  DEALLOCATE (substr)
END IF
prefix = ""
!Set the appropriate prefix and printing options
SELECT CASE (eCode)
CASE (EXCEPTION_INFORMATION)
  prefix = CHAR_LF// &
      & '[💚 INFORMATION ✅]'//CHAR_LF//&
      & '  [🚀 Module ] '//TRIM(ADJUSTL(modname))//CHAR_LF//&
      & '  [🎇 Method ] '//TRIM(ADJUSTL(myname))//CHAR_LF//&
      & '  [🔊 Message] '//TRIM(ADJUSTL(msg))//CHAR_LF
  IF (isLogActive) isQuiet = .TRUE.
CASE (EXCEPTION_WARNING)
  prefix = CHAR_LF// &
      & '[⚠ WARNING ❗]'//CHAR_LF//&
      & '  [🚀 Module ] '//TRIM(ADJUSTL(modname))//CHAR_LF//&
      & '  [🎇 Method ] '//TRIM(ADJUSTL(myname))//CHAR_LF//&
      & '  [🔊 Message] '//TRIM(ADJUSTL(msg))//CHAR_LF
CASE (EXCEPTION_ERROR)
  prefix = CHAR_LF// &
      & '[❎ ERROR ⛔]'//CHAR_LF//&
      & '  [🚀 Module ] '//TRIM(ADJUSTL(modname))//CHAR_LF//&
      & '  [🎇 Method ] '//TRIM(ADJUSTL(myname))//CHAR_LF//&
      & '  [🔊 Message] '//TRIM(ADJUSTL(msg))//CHAR_LF
CASE (EXCEPTION_FATAL_ERROR)
  prefix = CHAR_LF// &
      & '[🔥☠ FATAL_ERROR ❌]'//CHAR_LF//&
      & '  [🚀 Module ] '//TRIM(ADJUSTL(modname))//CHAR_LF//&
      & '  [🎇 Method ] '//TRIM(ADJUSTL(myname))//CHAR_LF//&
      & '  [🔊 Message] '//TRIM(ADJUSTL(msg))//CHAR_LF
  isQuiet = .FALSE.
CASE (EXCEPTION_DEBUG)
  prefix = CHAR_LF// &
      & '[❓ DEBUG 🐛]'//CHAR_LF//&
      & '  [🚀 Module ] '//TRIM(ADJUSTL(modname))//CHAR_LF//&
      & '  [🎇 Method ] '//TRIM(ADJUSTL(myname))//CHAR_LF//&
      & '  [🔊 Message] '//TRIM(ADJUSTL(msg))//CHAR_LF
END SELECT

!Write to the default standard error output
IF (.NOT. isQuiet) THEN
  WRITE (stderr, '(a)') prefix%chars()
  FLUSH (stderr)
END IF

!Write to the log file
IF (isLogActive) THEN
  WRITE (logUnit, '(a)', IOSTAT=ioerr1) prefix%chars()
  FLUSH (logUnit)
  !Additional error message if problem writing to log file
  IF (ioerr1 /= 0 .OR. ioerr2 /= 0) THEN
    IF (isQuiet) THEN
      msg = '[❎ ERROR ⛔]'//CHAR_LF//"Problem writing to logfile"//CHAR_LF// &
        & "Original message follows: "//CHAR_LF
      prefix = msg//CHAR_LF//prefix//CHAR_LF
      WRITE (stderr, "(a)") prefix%chars()
      FLUSH (stderr)
    ELSE
      !Message was already printed.
      WRITE (stderr, '(6x,a)') 'Problem writing above message '// &
        & 'to log file.'
    END IF
  END IF
END IF

!Set the message to be included as one line back to exception object
prefixLen = LEN_TRIM(prefix) + 3
WRITE (mesg, '(a)') TRIM(prefix)//' - '//&
     & TRIM(mesg(1:EXCEPTION_MAX_MESG_LENGTH - prefixLen))
END PROCEDURE exceptionMessage

!----------------------------------------------------------------------------
!                                                          copyFromSurrogate
!----------------------------------------------------------------------------

MODULE PROCEDURE copyFromSurrogate
TYPE(ExceptionHandler_), POINTER :: tmpE
!> main
tmpE => obj%surrogate
NULLIFY (obj%surrogate)
obj%stopOnError = tmpE%stopOnError
obj%logFileActive = tmpE%logFileActive
obj%logFileUnit = tmpE%logFileUnit
obj%nInfo = tmpE%nInfo
obj%nWarn = tmpE%nWarn
obj%nDebug = tmpE%nDebug
obj%nError = tmpE%nError
obj%nFatal = tmpE%nFatal
obj%quiet = tmpE%quiet
obj%verbose = tmpE%verbose
obj%lastMesg = tmpE%lastMesg
NULLIFY (tmpE)
END PROCEDURE copyFromSurrogate

!----------------------------------------------------------------------------
!                                                             ASSIGNMENT(=)
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_assign_obj
obj1%nInfo = obj2%nInfo
obj1%nWarn = obj2%nWarn
obj1%nDebug = obj2%nDebug
obj1%nError = obj2%nError
obj1%nFatal = obj2%nFatal
obj1%lastMesg = obj2%lastMesg
obj1%logFileUnit = obj2%logFileUnit
obj1%stopOnError = obj2%stopOnError
obj1%logFileActive = obj2%logFileActive
obj1%quiet = obj2%quiet
obj1%verbose = obj2%verbose
obj1%surrogate => obj2%surrogate
END PROCEDURE obj_assign_obj

!----------------------------------------------------------------------------!                                                                 initCounter
!----------------------------------------------------------------------------

MODULE PROCEDURE initCounter
IF (ASSOCIATED(obj%surrogate)) CALL copyFromSurrogate(obj)
obj%nInfo = 0
obj%nWarn = 0
obj%nError = 0
obj%nFatal = 0
obj%nDebug = 0
obj%lastMesg = ''
END PROCEDURE initCounter

!----------------------------------------------------------------------------
!                                                                      reset
!----------------------------------------------------------------------------

MODULE PROCEDURE reset
NULLIFY (obj%surrogate)
obj%nInfo = 0
obj%nWarn = 0
obj%nError = 0
obj%nFatal = 0
obj%nDebug = 0
obj%lastMesg = ''
obj%logFileUnit = 666
obj%stopOnError = .TRUE.
obj%logFileActive = .FALSE.
obj%quiet = (/.FALSE., .FALSE., .TRUE., .FALSE./)
obj%verbose = (/.TRUE., .TRUE., .FALSE., .TRUE./)
END PROCEDURE reset

END SUBMODULE Constructor
