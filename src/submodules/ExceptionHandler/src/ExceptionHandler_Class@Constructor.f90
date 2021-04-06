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
! date: 	4 April 2021
! summary: 	Constructor method for [[ExceptionHandler_]]

SUBMODULE( ExceptionHandler_Class ) Constructor
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             exceptionStop
!----------------------------------------------------------------------------

MODULE PROCEDURE exceptionStop
#ifdef HAVE_MPI
  INCLUDE 'mpif.h'
  INTEGER( I4B ) :: ierr
#endif
#ifdef HAVE_MPI
  IF( stopmode ) CALL MPI_Abort( MPI_COMM_WORLD, 666, ierr )
#else
  IF(stopmode) THEN
    STOP 666
  ENDIF
#endif
END PROCEDURE exceptionStop

!----------------------------------------------------------------------------
!                                                           exceptionMessage
!----------------------------------------------------------------------------

MODULE PROCEDURE exceptionMessage
  ! Define internal variables
  CHARACTER( LEN=EXCEPTION_MAX_MESG_LENGTH ) :: prefix
  INTEGER( I4B ) :: ioerr1,ioerr2,prefixLen

  !Set the appropriate prefix and printing options
  SELECT CASE( eCode )
  CASE( EXCEPTION_INFORMATION )
    prefix=''
    WRITE(mesg,'(a)')  'EXCEPTION_INFORMATION: ' // TRIM(mesg)
    IF(isLogActive) isQuiet=.TRUE.
  CASE(EXCEPTION_WARNING)
    WRITE(prefix,'(a)')  '#### EXCEPTION_WARNING ####'
  CASE(EXCEPTION_ERROR)
    WRITE(prefix,'(a)')  '#### EXCEPTION_ERROR ####'
  CASE(EXCEPTION_FATAL_ERROR)
    WRITE(prefix,'(a)')  '#### EXCEPTION_FATAL_ERROR ####'
    isQuiet=.FALSE.
  CASE(EXCEPTION_DEBUG)
    WRITE(prefix,'(a)')  '#### EXCEPTION_DEBUG_MESG ####'
  ENDSELECT

  !Write to the default standard error output
  IF(.NOT.isQuiet) THEN
    WRITE(stderr,'(a)') TRIM(prefix)
    WRITE(stderr,'(6x,a)') TRIM(mesg)
    FLUSH(stderr)
  ENDIF

  !Write to the log file
  IF(isLogActive) THEN
    WRITE(logUnit,'(a)',IOSTAT=ioerr1) TRIM(prefix)
    WRITE(logUnit,'(6x,a)',IOSTAT=ioerr2) TRIM(mesg)
    FLUSH(logUnit)

    !Additional error message if problem writing to log file
    IF(ioerr1 /= 0 .OR. ioerr2 /= 0) THEN
      WRITE(stderr,'(a)') '#### EXCEPTION_INFORMATION ####'
      IF(isQuiet) THEN
        !If quiet mode, then print the message that failed.
        WRITE(stderr,'(6x,a)') 'Problem writing to log file.'
        WRITE(stderr,'(6x,a)') 'Original Message: "'// &
          & TRIM(prefix)//' - '//TRIM(mesg)//'"'
      ELSE
        !Message was already printed.
        WRITE(stderr,'(6x,a)') 'Problem writing above message '// &
          & 'to log file.'
      ENDIF
    ENDIF
  ENDIF

  !Set the message to be included as one line back to exception object
  prefixLen = LEN_TRIM(prefix)+3
  WRITE(mesg,'(a)') TRIM(prefix)//' - '//TRIM(mesg(1:EXCEPTION_MAX_MESG_LENGTH-prefixLen))
END PROCEDURE exceptionMessage

!----------------------------------------------------------------------------
!                                                          copyFromSurrogate
!----------------------------------------------------------------------------

MODULE PROCEDURE copyFromSurrogate
  TYPE( ExceptionHandler_ ), POINTER :: tmpE
  tmpE => Obj%surrogate
  NULLIFY(Obj%surrogate)
  Obj%stopOnError=tmpE%stopOnError
  Obj%logFileActive=tmpE%logFileActive
  Obj%quiet=tmpE%quiet
  Obj%logFileUnit=tmpE%logFileUnit
  Obj%nInfo=tmpE%nInfo
  Obj%nWarn=tmpE%nWarn
  Obj%nError=tmpE%nError
  Obj%nFatal=tmpE%nFatal
  Obj%nDebug=tmpE%nDebug
  Obj%verbose=tmpE%verbose
  Obj%lastMesg=tmpE%lastMesg
  NULLIFY(tmpE)
END PROCEDURE copyFromSurrogate

!----------------------------------------------------------------------------
!                                                             ASSIGNMENT(=)
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_assign_obj
  Obj1%nInfo=Obj2%nInfo
  Obj1%nWarn=Obj2%nWarn
  Obj1%nDebug=Obj2%nDebug
  Obj1%nError=Obj2%nError
  Obj1%nFatal=Obj2%nFatal
  Obj1%lastMesg=Obj2%lastMesg
  Obj1%logFileUnit=Obj2%logFileUnit
  Obj1%stopOnError=Obj2%stopOnError
  Obj1%logFileActive=Obj2%logFileActive
  Obj1%quiet=Obj2%quiet
  Obj1%verbose=Obj2%verbose
  Obj1%surrogate => Obj2%surrogate
END PROCEDURE obj_assign_obj

!----------------------------------------------------------------------------!                                                                 initCounter
!----------------------------------------------------------------------------

MODULE PROCEDURE initCounter
  IF(ASSOCIATED(Obj%surrogate)) CALL copyFromSurrogate(Obj)
  Obj%nInfo=0
  Obj%nWarn=0
  Obj%nError=0
  Obj%nFatal=0
  Obj%nDebug=0
  Obj%lastMesg=''
END PROCEDURE initCounter

!----------------------------------------------------------------------------
!                                                                      reset
!----------------------------------------------------------------------------

MODULE PROCEDURE reset
  NULLIFY(Obj%surrogate)
  Obj%nInfo=0
  Obj%nWarn=0
  Obj%nError=0
  Obj%nFatal=0
  Obj%nDebug=0
  Obj%lastMesg=''
  Obj%logFileUnit=666
  Obj%stopOnError=.TRUE.
  Obj%logFileActive=.FALSE.
  Obj%quiet=(/.FALSE.,.FALSE.,.TRUE.,.FALSE./)
  Obj%verbose=(/.TRUE.,.TRUE.,.FALSE.,.TRUE./)
END PROCEDURE reset

END SUBMODULE Constructor