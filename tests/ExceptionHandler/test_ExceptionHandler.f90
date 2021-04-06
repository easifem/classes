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
! date: 	5 April 2021
! summary: 	Tutorial for [[exceptionHandler_]] data type

module test_exceptionHandler
use easifemBase
use easifemClasses
implicit none
contains
!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
subroutine test1
  WRITE(*,*) 'TESTING PARAMETERS'
  WRITE(*,*) '  Passed:              EXCEPTION_OK = ',EXCEPTION_OK
  WRITE(*,*) '  Passed:     EXCEPTION_INFORMATION = ',EXCEPTION_INFORMATION
  WRITE(*,*) '  Passed:         EXCEPTION_WARNING = ',EXCEPTION_WARNING
  WRITE(*,*) '  Passed:           EXCEPTION_DEBUG = ',EXCEPTION_DEBUG
  WRITE(*,*) '  Passed:           EXCEPTION_ERROR = ',EXCEPTION_ERROR
  WRITE(*,*) '  Passed:     EXCEPTION_FATAL_ERROR = ',EXCEPTION_FATAL_ERROR
  WRITE(*,*) '  Passed:            EXCEPTION_SIZE = ',EXCEPTION_SIZE
  WRITE(*,*) '  Passed: EXCEPTION_MAX_MESG_LENGTH = ', EXCEPTION_MAX_MESG_LENGTH
end

subroutine test2
  type( exceptionHandler_ ) :: obj
  CALL Display( obj%isQuietMode(),'%isQuietMode() = ' )
  CALL Display( Obj%isQuietMode(EXCEPTION_INFORMATION),'%isQuietMode(INFO)=')
  CALL Display( Obj%isQuietMode(EXCEPTION_WARNING),'%isQuietMode(WARNING)=')
  CALL Display( Obj%isQuietMode(EXCEPTION_DEBUG),'%isQuietMode(DEBUG)=')
  CALL Display( Obj%isQuietMode(EXCEPTION_ERROR),'%isQuietMode(ERROR)=')
  CALL Display( Obj%isQuietMode(EXCEPTION_FATAL_ERROR),'%isQuietMode(FATAL_ERROR)=')
  CALL Display( Obj%isQuietMode(EXCEPTION_OK),'%isQuietMode(OK) =')
  CALL Display(Obj%isStopOnError(),'%isStopOnError =')
  CALL Display(ALL(Obj%getCounterAll() == 0),'getCounterAll() =')
  CALL Display(Obj%getCounter(EXCEPTION_INFORMATION) == 0,'INFO =')
  CALL Display(Obj%getCounter(EXCEPTION_WARNING) == 0,'WARN =')
  CALL Display(Obj%getCounter(EXCEPTION_DEBUG) == 0,'DEBUG =')
  CALL Display(Obj%getCounter(EXCEPTION_ERROR) == 0,'ERROR =')
  CALL Display(Obj%getCounter(EXCEPTION_FATAL_ERROR) == 0,'FATAL_ERROR =')
  CALL Display(Obj%getCounter(EXCEPTION_OK) == -1,'OK =')
  CALL Display(Obj%getLastMessage() == '','%getLastMessage() = ' )
  CALL Display(Obj%getLogFileUnit() == 666,'%getLogFileUnit = ')
  CALL Display(.NOT.Obj%isLogActive(),'%isLogActive = ')

  CALL Obj%setQuietMode(.TRUE.)
  CALL Display(Obj%isQuietMode(),'%setQuietMode(T)=')
  CALL Display(Obj%isQuietMode(EXCEPTION_INFORMATION),'%setQuietMode(T) INFO =')
  CALL Display(Obj%isQuietMode(EXCEPTION_WARNING),'%setQuietMode(T) WARN=')
  CALL Display(Obj%isQuietMode(EXCEPTION_DEBUG),'%setQuietMode(T) DEBUG=')
  CALL Display(Obj%isQuietMode(EXCEPTION_ERROR),'%setQuietMode(T) ERROR=')
  CALL Display(.NOT.Obj%isQuietMode(EXCEPTION_FATAL_ERROR),'%setQuietMode(T) FATAL=')

  CALL Obj%setQuietMode(.FALSE.)
  CALL Display(.NOT.Obj%isQuietMode(),'%setQuietMode(F)=')
  CALL Display(.NOT.Obj%isQuietMode(EXCEPTION_INFORMATION),'%setQuietMode(F) INFO =')
  CALL Display(.NOT.Obj%isQuietMode(EXCEPTION_WARNING),'%setQuietMode(F) WARNING =')
  CALL Display(.NOT.Obj%isQuietMode(EXCEPTION_DEBUG),'%setQuietMode(F) DEBUG=')
  CALL Display(.NOT.Obj%isQuietMode(EXCEPTION_ERROR),'%setQuietMode(F) ERROR=')
  CALL Display(.NOT.Obj%isQuietMode(EXCEPTION_FATAL_ERROR),'%setQuietMode(F) FATAL=')

  CALL Obj%setQuietMode(EXCEPTION_INFORMATION,.TRUE.)
  CALL Display(.NOT.Obj%isQuietMode(),'%setQuietMode(INFO,T)=')
  CALL Display(Obj%isQuietMode(EXCEPTION_INFORMATION),'%setQuietMode(INFO,T) INFO=')
  CALL Obj%setQuietMode(EXCEPTION_WARNING,.TRUE.)
  CALL Display(.NOT.Obj%isQuietMode(),'%setQuietMode(WARN,T)=')
  CALL Display(Obj%isQuietMode(EXCEPTION_WARNING),'%setQuietMode(WARN,T) WARN=')
  CALL Obj%setQuietMode(EXCEPTION_DEBUG,.FALSE.)
  CALL Display(.NOT.Obj%isQuietMode(),'%setQuietMode(DEBUG,F)=')
  CALL Display(.NOT.Obj%isQuietMode(EXCEPTION_DEBUG),'%setQuietMode(DEBUG,F) DEBUG=')
  CALL Obj%setQuietMode(EXCEPTION_ERROR,.TRUE.)
  CALL Display(.NOT.Obj%isQuietMode(),'%setQuietMode(ERROR,T)=')
  CALL Display(Obj%isQuietMode(EXCEPTION_ERROR),'%setQuietMode(ERROR,T) ERROR=')
  CALL Obj%setQuietMode(EXCEPTION_FATAL_ERROR,.TRUE.)
  CALL Display(.NOT.Obj%isQuietMode(),'%setQuietMode(FATAL,T)=')
  CALL Display(.NOT.Obj%isQuietMode(EXCEPTION_FATAL_ERROR),'%setQuietMode(FATAL,T) FATAL=')
  CALL Obj%setQuietMode(EXCEPTION_OK,.FALSE.)
  CALL Display(.NOT.Obj%isQuietMode(),'%setQuietMode(OK,T)=')
  CALL Display(.NOT.Obj%isQuietMode(EXCEPTION_OK),'%setQuietMode(OK,T) OK=')
  CALL Obj%setQuietMode((/.FALSE.,.FALSE.,.FALSE.,.FALSE./))

  CALL Obj%setVerboseMode(.TRUE.)
  CALL Display(Obj%isVerboseMode(),'%setVerboseMode(T)=')
  CALL Display(Obj%isVerboseMode(EXCEPTION_INFORMATION),'%setVerboseMode(T) INFO=')
  CALL Display(Obj%isVerboseMode(EXCEPTION_WARNING),'%setVerboseMode(T) WARN=')
  CALL Display(Obj%isVerboseMode(EXCEPTION_DEBUG),'%setVerboseMode(T) DEBUG=')
  CALL Display(Obj%isVerboseMode(EXCEPTION_ERROR),'%setVerboseMode(T) ERROR=')
  CALL Display(.NOT.Obj%isVerboseMode(EXCEPTION_FATAL_ERROR),'%setVerboseMode(T) FATAL=')
  CALL Obj%setVerboseMode(.FALSE.)
  CALL Display(.NOT.Obj%isVerboseMode(),'%setVerboseMode(F)=')
  CALL Display(.NOT.Obj%isVerboseMode(EXCEPTION_INFORMATION),'%setVerboseMode(F) INFO=')
  CALL Display(.NOT.Obj%isVerboseMode(EXCEPTION_WARNING),'%setVerboseMode(F) WARNING=')
  CALL Display(.NOT.Obj%isVerboseMode(EXCEPTION_DEBUG),'%setVerboseMode(F) DEBUG=')
  CALL Display(.NOT.Obj%isVerboseMode(EXCEPTION_ERROR),'%setVerboseMode(F) ERROR=')
  CALL Display(.NOT.Obj%isVerboseMode(EXCEPTION_FATAL_ERROR),'%setVerboseMode(F) FATAL=')

  CALL Obj%setVerboseMode(EXCEPTION_INFORMATION,.TRUE.)
  CALL Display(.NOT.Obj%isVerboseMode(),'%setVerboseMode(INFO,T)=')
  CALL Display(Obj%isVerboseMode(EXCEPTION_INFORMATION),'%setVerboseMode(INFO,T) INFO=')
  CALL Obj%setVerboseMode(EXCEPTION_WARNING,.TRUE.)
  CALL Display(.NOT.Obj%isVerboseMode(),'%setVerboseMode(WARN,T)=')
  CALL Display(Obj%isVerboseMode(EXCEPTION_WARNING),'%setVerboseMode(WARN,T) WARN=')
  CALL Obj%setVerboseMode(EXCEPTION_DEBUG,.FALSE.)
  CALL Display(.NOT.Obj%isVerboseMode(),'%setVerboseMode(DEBUG,F)=')
  CALL Display(.NOT.Obj%isVerboseMode(EXCEPTION_DEBUG),'%setVerboseMode(DEBUG,F) DEBUG=')
  CALL Obj%setVerboseMode(EXCEPTION_ERROR,.TRUE.)
  CALL Display(.NOT.Obj%isVerboseMode(),'%setVerboseMode(ERROR,T)=')
  CALL Display(Obj%isVerboseMode(EXCEPTION_ERROR),'%setVerboseMode(ERROR,T) ERROR=')
  CALL Obj%setVerboseMode(EXCEPTION_FATAL_ERROR,.TRUE.)
  CALL Display(.NOT.Obj%isVerboseMode(),'%setVerboseMode(FATAL,T)=')
  CALL Display(.NOT.Obj%isVerboseMode(EXCEPTION_FATAL_ERROR),'%setVerboseMode(FATAL,T) FATAL=')
  CALL Obj%setVerboseMode(EXCEPTION_OK,.FALSE.)
  CALL Display(.NOT.Obj%isVerboseMode(),'%setVerboseMode(OK,T)=')
  CALL Display(.NOT.Obj%isVerboseMode(EXCEPTION_OK),'%setVerboseMode(OK,T) OK=')
  CALL Obj%setVerboseMode((/.TRUE.,.TRUE.,.TRUE.,.TRUE./))
end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test3
  type( exceptionHandler_ ) :: Obj
  CHARACTER( LEN = EXCEPTION_MAX_MESG_LENGTH ) :: mesg

  CALL Obj%raiseInformation('Test information')
  CALL Display(ALL(Obj%getCounterAll() == (/1,0,0,0,0/)),'%counterall=')
  CALL Display(Obj%getCounter(EXCEPTION_INFORMATION) == 1,'%counter(INFO)=')
  mesg=' - EXCEPTION_INFORMATION: Test information'
  CALL Display( Obj%getLastMessage(), "last mesg=")
  CALL Display(Obj%getLastMessage() == TRIM(mesg),'mesg=')

  CALL Obj%raiseWarning('Test warning')
  CALL Display(ALL(Obj%getCounterAll() == (/1,1,0,0,0/)),'%counterall=')
  CALL Display(Obj%getCounter(EXCEPTION_WARNING) == 1,'%counter(WARN)=')
  mesg='#### EXCEPTION_WARNING #### - Test warning'
  CALL Display( Obj%getLastMessage(), "last mesg=")
  CALL Display(Obj%getLastMessage() == TRIM(mesg),'mesg=')

  CALL Obj%raiseDebug('Test debug')
  CALL Display(ALL(Obj%getCounterAll() == (/1,1,1,0,0/)),'%raiseDebug=')
  CALL Display(Obj%getCounter(EXCEPTION_DEBUG) == 1,'%counter(DEBUG)=')
  mesg='#### EXCEPTION_DEBUG_MESG #### - Test debug'
  CALL Display(Obj%getLastMessage() == TRIM(mesg),'mesg=')

  CALL Obj%setStopOnError(.FALSE.)
  CALL Display(.NOT.Obj%isStopOnError(),'setStopOnError(F)=')
  CALL Obj%setStopOnError(.TRUE.)
  CALL Display(Obj%isStopOnError(),'setStopOnError(T)=')
  CALL Obj%setStopOnError(.FALSE.)
  CALL Display(.NOT.Obj%isStopOnError(),'setStopOnError(F)=')

  CALL Obj%raiseError('Test error')
  CALL Display(ALL(Obj%getCounterAll() == (/1,1,1,1,0/)),'%raiseError=')
  CALL Display(Obj%getCounter(EXCEPTION_ERROR) == 1,'%counter(ERROR)=')
  mesg='#### EXCEPTION_ERROR #### - Test error'
  CALL Display(Obj%getLastMessage() == TRIM(mesg),'mesg=')

  CALL Obj%initCounter()
  CALL Display(ALL(Obj%getCounterAll() == 0),'counterAll=')
  CALL Display(Obj%getCounter(EXCEPTION_INFORMATION) == 0,'%counter(INFO)=')
  CALL Display(Obj%getCounter(EXCEPTION_WARNING) == 0,'%counter(WARN)=')
  CALL Display(Obj%getCounter(EXCEPTION_DEBUG) == 0,'%counter(DEBUG)=')
  CALL Display(Obj%getCounter(EXCEPTION_ERROR) == 0,'%counter(ERROR)=')
  CALL Display(Obj%getCounter(EXCEPTION_FATAL_ERROR) == 0,'%counter(FATAL)=')
  CALL Display(Obj%getLastMessage() == '','mesg=')

  CALL Obj%raiseWarning('Very                                              '//&
                          'long                                              '//&
                          'message                                           '//&
                          'exceeding                                         '//&
                          'size                                              '//&
                          'of                                                '//&
                          'character                                         '//&
                          'length                                            '//&
                          'limit                                             '//&
                          'of                                                '//&
                          '512.....The remainder of this message will be truncated')

end

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

subroutine test4
  type( exceptionHandler_ ) :: obj

  CALL Display( "test04")
  CALL EqualLine()
  CALL obj%setQuietMode(.TRUE.)
  CALL obj%setLogFileUnit(stdout)
  CALL obj%setLogFileUnit(stderr)
  CALL obj%setLogFileUnit(-1)

  CALL Display(obj%getCounter(EXCEPTION_WARNING),'%counter(WARN)=')
  CALL Display(TRIM(Obj%getLastMessage()),'%getLastMessage=')
  CALL Obj%setLogFileUnit(23)
  CALL Display(Obj%getLogFileUnit() == 23,'setLogFileUnit(23)=')
  CALL Obj%setQuietMode(.FALSE.)
  CALL Display(.NOT.Obj%isLogActive(),'%isLogActive=')
  CALL Obj%setLogActive(.TRUE.)
  CALL Display(.NOT.Obj%isLogActive(),'%setLogActive=')
  CALL Obj%setLogActive(.FALSE.)
  CALL Display(.NOT.Obj%isLogActive(),'%setLogActive=')

  OPEN(UNIT=Obj%getLogFileUnit(),FILE='Exception.log', &
      ACCESS='SEQUENTIAL', FORM='FORMATTED')
  CALL Obj%setLogActive(.TRUE.)
  CALL Display(Obj%isLogActive(),'%setLogActive ')
  CALL Obj%setLogActive(.FALSE.)
  CALL Display(.NOT.Obj%isLogActive(),'%setLogActive ')
  CALL Obj%setLogActive(.TRUE.)
  CALL Obj%setQuietMode(.TRUE.)
  CALL Obj%setStopOnError(.FALSE.)
  CALL Obj%raiseInformation('Test information')
  CALL Obj%raiseWarning('Test warning')
  CALL Obj%raiseDebug('Test debug')
  CALL Obj%raiseError('Test error')
  CLOSE(Obj%getLogFileUnit())

  ! OPEN(UNIT=testE%getLogFileUnit(),FILE='Exception.log', &
  !     ACCESS='SEQUENTIAL',FORM='FORMATTED',ACTION='READ')
  ! READ(testE%getLogFileUnit(),'(a)') mesg2
  ! mesg=''
  ! ASSERT(TRIM(mesg) == TRIM(mesg2),'blank line')
  ! READ(testE%getLogFileUnit(),'(a)') mesg2
  ! mesg='      EXCEPTION_INFORMATION: Test information'
  ! ASSERT(TRIM(mesg) == TRIM(mesg2),TRIM(mesg))
  ! READ(testE%getLogFileUnit(),'(a)') mesg2
  ! mesg='#### EXCEPTION_WARNING ####'
  ! ASSERT(TRIM(mesg) == TRIM(mesg2),TRIM(mesg))
  ! READ(testE%getLogFileUnit(),'(a)') mesg2
  ! mesg='      Test warning'
  ! ASSERT(TRIM(mesg) == TRIM(mesg2),TRIM(mesg))
  ! READ(testE%getLogFileUnit(),'(a)') mesg2
  ! mesg='#### EXCEPTION_DEBUG_MESG ####'
  ! ASSERT(TRIM(mesg) == TRIM(mesg2),TRIM(mesg))
  ! READ(testE%getLogFileUnit(),'(a)') mesg2
  ! mesg='      Test debug'
  ! ASSERT(TRIM(mesg) == TRIM(mesg2),TRIM(mesg))
  ! READ(testE%getLogFileUnit(),'(a)') mesg2
  ! mesg='#### EXCEPTION_ERROR ####'
  ! ASSERT(TRIM(mesg) == TRIM(mesg2),TRIM(mesg))
  ! READ(testE%getLogFileUnit(),'(a)') mesg2
  ! mesg='      Test error'
  ! ASSERT(TRIM(mesg) == TRIM(mesg2),TRIM(mesg))
  ! CLOSE(testE%getLogFileUnit())
end

! SUBROUTINE testVerbosity()
!   INTEGER(SIK) :: ioerr
!   OPEN(UNIT=testE%getLogFileUnit(),FILE='Exception.log', &
!       ACCESS='SEQUENTIAL',FORM='FORMATTED',STATUS='REPLACE')
!   CALL testE%setQuietMode(.FALSE.)
!   CALL testE%setVerboseMode(EXCEPTION_INFORMATION,.FALSE.)
!   CALL testE%setVerboseMode(EXCEPTION_WARNING,.FALSE.)
!   CALL testE%setVerboseMode(EXCEPTION_DEBUG,.FALSE.)
!   CALL testE%setVerboseMode(EXCEPTION_ERROR,.FALSE.)
!   CALL testE%raiseInformation('Test information no log')
!   CALL testE%raiseWarning('Test warning no log')
!   CALL testE%raiseDebug('Test debug no log')
!   CALL testE%raiseError('Test error no log')
!   CLOSE(testE%getLogFileUnit())
!   OPEN(UNIT=testE%getLogFileUnit(),FILE='Exception.log', &
!       ACCESS='SEQUENTIAL',FORM='FORMATTED',ACTION='READ')
!   READ(testE%getLogFileUnit(),'(a)',IOSTAT=ioerr) mesg2
!   ASSERT(ioerr == IOSTAT_END,'EOF Log file')
! ENDSUBROUTINE testVerbosity
! !
! !-------------------------------------------------------------------------------
! SUBROUTINE testSurrogate()
!   CALL testE2%addSurrogate(testE)
!   ASSERT(testE2%isStopOnError() .EQV. testE%isStopOnError(),'isStopOnError')
!   ASSERT(testE2%getLogFileUnit() == testE%getLogFileUnit(),'getLogFileUnit')
!   ASSERT(testE2%isQuietMode() .EQV. testE%isQuietMode(),'isQuiet')
!   ASSERT(ALL(testE2%getCounterAll() == testE%getCounterAll()),'getCounterAll')
!   ASSERT(testE2%getLastMessage() == testE%getLastMessage(),'getLastMessage')
!   ASSERT(testE2%isLogActive() .EQV. testE%isLogActive(),'isLogActive')

!   CALL testE2%getSurrogate(testE3)
!   ASSERT(ASSOCIATED(testE3,testE), 'getSurrogate')

!   CALL testE2%setQuietMode(.TRUE.)
!   ASSERT(testE2%isQuietMode() .NEQV. testE%isQuietMode(),'isQuiet (NEQV)')
! ENDSUBROUTINE testSurrogate
! !
! !-------------------------------------------------------------------------------
! SUBROUTINE testAssignment()
!   testE2=testE
!   ASSERT(testE2%isStopOnError() .EQV. testE%isStopOnError(),'isStopOnError')
!   ASSERT(testE2%getLogFileUnit() == testE%getLogFileUnit(),'getLogFileUnit')
!   ASSERT(testE2%isQuietMode() .EQV. testE%isQuietMode(),'isQuiet')
!   ASSERT(ALL(testE2%getCounterAll() == testE%getCounterAll()),'getCounterAll')
!   ASSERT(testE2%getLastMessage() == testE%getLastMessage(),'getLastMessage')
!   ASSERT(testE2%isLogActive() .EQV. testE%isLogActive(),'isLogActive')
! ENDSUBROUTINE testAssignment
! !
! !-------------------------------------------------------------------------------
! SUBROUTINE testReset()
!   CALL testE%reset()
!   ASSERT(.NOT.testE%isQuietMode(),'%isQuietMode()')
!   ASSERT(testE%isStopOnError(),'%isStopOnError')
!   ASSERT(testE%getLogFileUnit() == 666,'%getLogFileUnit')
!   ASSERT(ALL(testE%getCounterAll() == 0),'getCounterAll()')
!   ASSERT(testE%getLastMessage() == '','%getLastMessage()')
!   ASSERT(.NOT.testE%isLogActive(),'%isLogActive')
! ENDSUBROUTINE testReset
! !
! !-------------------------------------------------------------------------------
! SUBROUTINE testSetCounter()
!   ASSERT(ALL(testE%getCounterAll() == 0),'getCounterAll()')
!   CALL testE%setCounter((/-1,-1,-1,-1,1/))
!   ASSERT(ALL(testE%getCounterAll() == (/0,0,0,0,1/)),'setCounterAll() -1 for first 4')
!   CALL testE%setCounter(EXCEPTION_INFORMATION,2)
!   ASSERT(ALL(testE%getCounterAll() == (/2,0,0,0,1/)),'setCounter() Info')
!   CALL testE%setCounter(EXCEPTION_WARNING,3)
!   ASSERT(ALL(testE%getCounterAll() == (/2,3,0,0,1/)),'setCounter() Warning')
!   CALL testE%setCounter(EXCEPTION_DEBUG,4)
!   ASSERT(ALL(testE%getCounterAll() == (/2,3,4,0,1/)),'setCounter() Debug')
!   CALL testE%setCounter(EXCEPTION_ERROR,5)
!   ASSERT(ALL(testE%getCounterAll() == (/2,3,4,5,1/)),'setCounter() Error')
!   CALL testE%setCounter(EXCEPTION_FATAL_ERROR,6)
!   ASSERT(ALL(testE%getCounterAll() == (/2,3,4,5,6/)),'setCounter() Error')
! ENDSUBROUTINE testSetCounter

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

end module

program main
use test_exceptionHandler
implicit none
call test1
call test2
call test3
call test4
end program main
