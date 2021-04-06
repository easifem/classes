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
! This module is modified from the ExceptionHandler in FUtility library
! which is maintained by Futility Development Group. https://github.com/CASL/Futility

!> authors: Vikas Sharma, Ph. D.
! date: 	3 April 2021
! summary: Simple error and exception handling
!
!### Introduction
!
! More details about this module is avaiable at https://github.com/CASL/Futility
!
! This module provides a Fortran 2003 object for basic exception handling
! and error reporting. It defines 5 types of exceptions. The defined
! exceptions and their definitions are:
! - INFORMATION: this is an exception just for reporting information. This
!    may be useful if additional information about another exception needs to
!    be reported. If the log file is active then information exceptions are
!    not printed to standard error.
! - WARNING: this exception is for providing a warning to the user.
!    Warnings cannot stop execution. They can be reported to the log file or
!    standard error.
! - ERROR: this exception is for reporting errors. If the option stopOnError is false then execution will continue, otherwise it will be halted.
! - FATAL ERROR: this exception is for reporting fatal errors. It is
!    assumed that the program cannot recover from a fatal error so this
!    exception will always halt program execution.
! - FAILURE: this exception is for reporting a failure. It's exact purpose
!    is not yet defined, so its more of a placeholder at present. One
!    possibility might be for unit testing to report a test condition failure.
!
! The exception handler object default is to report exceptions to standard
! error and stop execution when an error is reported. No default log file
! is assumed, so a log file is not active by default also.

MODULE ExceptionHandler_Class
USE GlobalData
IMPLICIT NONE
PRIVATE

INTEGER( I4B ),PARAMETER, PUBLIC :: EXCEPTION_OK=0
  !! OK condition
INTEGER( I4B ), PARAMETER, PUBLIC :: EXCEPTION_INFORMATION=1
  !! INFORMATION exception
INTEGER( I4B ), PARAMETER, PUBLIC :: EXCEPTION_WARNING=2
  !! WARNING exception
INTEGER( I4B ), PARAMETER, PUBLIC :: EXCEPTION_DEBUG=3
  !! DEBUG exception
INTEGER( I4B ), PARAMETER, PUBLIC :: EXCEPTION_ERROR=4
  !! An enumeration for defining an ERROR exception
INTEGER( I4B ), PARAMETER, PUBLIC :: EXCEPTION_FATAL_ERROR=5
  !! FATAL ERROR exception
INTEGER( I4B ), PARAMETER, PUBLIC :: EXCEPTION_SIZE=5
  !! The number of exception types
INTEGER( I4B ), PARAMETER, PUBLIC :: EXCEPTION_MAX_MESG_LENGTH=512
  !! The maximum size of an exception message

!----------------------------------------------------------------------------
!                                                          ExceptionHandler_
!----------------------------------------------------------------------------

TYPE :: ExceptionHandler_
  LOGICAL( LGT ), PRIVATE :: stopOnError=.TRUE.
    !! Defines whether or not to stop executaion when an error is raised
  LOGICAL( LGT ), PRIVATE :: logFileActive=.FALSE.
    !! Defines whether or not to report exceptions to a log file
  INTEGER( I4B ),PRIVATE :: logFileUnit=666
    !! The output unit identifier for the log file
  INTEGER( I4B ),PRIVATE :: nInfo=0
    !! The number of INFORMATION exceptions that have been raised
  INTEGER( I4B ),PRIVATE :: nWarn=0
    !! The number of WARNING exceptions that have been raised
  INTEGER( I4B ),PRIVATE :: nDebug=0
    !! The number of DEBUG exceptions that have been raised
  INTEGER( I4B ),PRIVATE :: nError=0
    !! The number of ERROR exceptions that have been raised
  INTEGER( I4B ),PRIVATE :: nFatal=0
    !! The number of FATAL ERROR exceptions that have been raised
  LOGICAL( LGT ),PRIVATE :: quiet(EXCEPTION_SIZE-1)= &
      (/.FALSE.,.FALSE.,.TRUE.,.FALSE./)
    !! Defines whether or not to report exceptions to standard error
  LOGICAL( LGT ),PRIVATE :: verbose(EXCEPTION_SIZE-1)= &
      (/.TRUE.,.TRUE.,.FALSE.,.TRUE./)
    !! Logical array that allows for selective verbosity of exception types
  CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH),PRIVATE :: lastMesg=''
    !! The last exception message that was reported
  TYPE(ExceptionHandler_ ), POINTER, PRIVATE :: surrogate => NULL()
    !! Surrogate exception handler to which most functions are delegated.
  CONTAINS
    GENERIC, PUBLIC :: setCounter => setCounter_all,setCounter_eCode
    GENERIC, PUBLIC :: setVerboseMode => setVerboseMode_all,&
      & setVerboseMode_eCode, setVerboseMode_array
    GENERIC, PUBLIC :: setQuietMode => setQuietMode_all,setQuietMode_eCode, setQuietMode_array
    GENERIC, PUBLIC :: isQuietMode => isQuietMode_all,isQuietMode_eCode
    GENERIC, PUBLIC :: isVerboseMode => isVerboseMode_all,isVerboseMode_eCode

    PROCEDURE, PUBLIC, PASS( Obj ) :: addSurrogate
    PROCEDURE, PUBLIC, PASS( Obj ) :: getSurrogate
    PROCEDURE, PUBLIC, PASS( Obj ) :: initCounter
    PROCEDURE, PUBLIC, PASS( Obj ) :: reset
    PROCEDURE, PUBLIC, PASS( Obj ) :: getCounterAll
    PROCEDURE, PUBLIC, PASS( Obj ) :: getCounter
    PROCEDURE, PRIVATE, PASS( Obj ) :: setCounter_All
    PROCEDURE, PRIVATE, PASS( Obj ) :: setCounter_eCode
    PROCEDURE, PUBLIC, PASS( Obj ) :: getLastMessage
    PROCEDURE, PRIVATE, PASS( Obj ) :: setQuietMode_all
    PROCEDURE, PRIVATE, PASS( Obj ) :: setQuietMode_eCode
    PROCEDURE, PRIVATE, PASS( Obj ) :: setQuietMode_array
    PROCEDURE, PRIVATE, PASS( Obj ) :: isQuietMode_all
    PROCEDURE, PRIVATE, PASS( Obj ) :: isQuietMode_eCode
    PROCEDURE, PRIVATE, PASS( Obj ) :: setVerboseMode_all
    PROCEDURE, PRIVATE, PASS( Obj ) :: setVerboseMode_eCode
    PROCEDURE, PRIVATE, PASS( Obj ) :: setVerboseMode_array
    PROCEDURE, PRIVATE, PASS( Obj ) :: isVerboseMode_all
    PROCEDURE, PRIVATE, PASS( Obj ) :: isVerboseMode_eCode
    PROCEDURE, PUBLIC, PASS( Obj ) :: setLogFileUnit
    PROCEDURE, PUBLIC, PASS( Obj ) :: getLogFileUnit
    PROCEDURE, PUBLIC, PASS( Obj ) :: setLogActive
    PROCEDURE, PUBLIC, PASS( Obj ) :: isLogActive
    PROCEDURE, PUBLIC, PASS( Obj ) :: checkLogFileOK
    PROCEDURE, PUBLIC, PASS( Obj ) :: setStopOnError
    PROCEDURE, PUBLIC, PASS( Obj ) :: isStopOnError
    PROCEDURE, PUBLIC, PASS( Obj ) :: raiseInformation
    PROCEDURE, PUBLIC, PASS( Obj ) :: raiseWarning
    PROCEDURE, PUBLIC, PASS( Obj ) :: raiseDebug
    PROCEDURE, PUBLIC, PASS( Obj ) :: raiseError
    PROCEDURE, PUBLIC, PASS( Obj ) :: raiseFatalError
END TYPE ExceptionHandler_

PUBLIC :: ExceptionHandler_

TYPE( ExceptionHandler_ ), PARAMETER, PUBLIC :: TypeExceptionHandler=&
  & ExceptionHandler_()

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE ExceptionHandlerPointer_
  CLASS( ExceptionHandler_ ), POINTER :: Ptr => NULL()
END TYPE ExceptionHandlerPointer_


INTERFACE ASSIGNMENT(=)
  MODULE PROCEDURE obj_assign_obj
ENDINTERFACE

PUBLIC :: ASSIGNMENT( = )

!----------------------------------------------------------------------------
!                                                  exceptionStop@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE exceptionStop( stopmode )
  LOGICAL( LGT ), INTENT( IN ) :: stopmode
END SUBROUTINE exceptionStop
END INTERFACE

PUBLIC :: exceptionStop

!----------------------------------------------------------------------------
!                                               exceptionMessage@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE exceptionMessage( eCode, isQuiet, isLogActive, logUnit, mesg )
  INTEGER( I4B ), INTENT( IN ) :: eCode
  LOGICAL( LGT ), INTENT( INOUT ) :: isQuiet
  LOGICAL( LGT ), INTENT( IN ) :: isLogActive
  INTEGER( I4B ), INTENT( IN ) :: logUnit
  CHARACTER( LEN=EXCEPTION_MAX_MESG_LENGTH ), INTENT( INOUT ) :: mesg
END SUBROUTINE exceptionMessage
END INTERFACE

PUBLIC :: exceptionMessage

!----------------------------------------------------------------------------
!                                              copyFromSurrogate@Constructor
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE copyFromSurrogate( Obj )
  CLASS( ExceptionHandler_ ), INTENT( INOUT ) :: Obj
END SUBROUTINE copyFromSurrogate
END INTERFACE

PUBLIC :: copyFromSurrogate

!----------------------------------------------------------------------------
!                                                  obj_assign_obj@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	4 April 2021
! summary: 	copy a [[ExceptionHandler_]] instance into another instance

INTERFACE
MODULE SUBROUTINE obj_assign_obj(Obj1, Obj2)
  TYPE( ExceptionHandler_ ), INTENT( INOUT ) :: Obj1
  TYPE( ExceptionHandler_ ), INTENT( IN ) :: Obj2
END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                   addSurrogate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	4 April 2021
! summary: 	add Surrogate to [[ExceptionHandler_]]

INTERFACE
MODULE SUBROUTINE addSurrogate( Obj, Obj2 )
  CLASS( ExceptionHandler_ ), INTENT( INOUT ) :: Obj
  TYPE( ExceptionHandler_ ), TARGET, INTENT( IN ) :: Obj2
END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!                                                   getSurrogate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	4 April 2021
! summary: 	Returns a pointer to an exception handler's surrogate

INTERFACE
MODULE SUBROUTINE getSurrogate( Obj, Obj2 )
  CLASS( ExceptionHandler_ ), INTENT( IN ) :: Obj
  CLASS( ExceptionHandler_ ), POINTER, INTENT( INOUT ) :: Obj2
END SUBROUTINE getSurrogate
END INTERFACE

!----------------------------------------------------------------------------
!                                                    initCounter@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	4 April 2021
! summary: 	Initialize the exception counters for an exception object.

INTERFACE
MODULE PURE SUBROUTINE initCounter( Obj )
  CLASS( ExceptionHandler_ ), INTENT( INOUT) :: Obj
END SUBROUTINE initCounter
END INTERFACE

!----------------------------------------------------------------------------
!                                                          reset@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	4 April 2021
! summary: 	Resets an exception handler to its default state.

INTERFACE
MODULE PURE SUBROUTINE reset( Obj )
  CLASS( ExceptionHandler_ ), INTENT( INOUT) :: Obj
END SUBROUTINE reset
END INTERFACE

!----------------------------------------------------------------------------
!                                                  getCounterAll@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	4 April 2021
! summary: Get the counters for the exception object.

INTERFACE
MODULE PURE FUNCTION getCounterAll( Obj ) RESULT( Ans )
  CLASS( ExceptionHandler_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ) :: Ans( EXCEPTION_SIZE )
END FUNCTION getCounterAll
END INTERFACE

!----------------------------------------------------------------------------
!                                                  getCounterAll@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	4 April 2021
! summary: Get a count of one exception type for the exception

INTERFACE
MODULE PURE FUNCTION getCounter( Obj, i ) RESULT( Ans )
  CLASS( ExceptionHandler_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: i
  INTEGER( I4B ) :: Ans
END FUNCTION getCounter
END INTERFACE

!----------------------------------------------------------------------------
!                                                      setCounter@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	4 April 2021
! summary: 	Set the counters for the exception object.

INTERFACE
MODULE SUBROUTINE setCounter_All( Obj, counter )
  CLASS( ExceptionHandler_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: counter( EXCEPTION_SIZE )
END SUBROUTINE setCounter_All
END INTERFACE


!----------------------------------------------------------------------------
!                                                       setCounter@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	4 April 2021
! summary: 	Set a count of one exception type for the exception object.

INTERFACE
MODULE SUBROUTINE setCounter_eCode( Obj, i, count )
  CLASS( ExceptionHandler_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: i
  INTEGER( I4B ), INTENT( IN ) :: count
END SUBROUTINE setCounter_eCode
END INTERFACE

!----------------------------------------------------------------------------
!                                                  getLastMessage@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	4 March 2021
! summary: 	Gets the last exception message.

INTERFACE
MODULE PURE FUNCTION getLastMessage( Obj ) RESULT( Ans )
  CLASS( ExceptionHandler_ ), INTENT( IN ) :: Obj
  CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH) :: Ans
END FUNCTION getLastMessage
END INTERFACE

!----------------------------------------------------------------------------
!                                                  setQuietMode@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	4 April 2021
! summary: Suppress/Unsupress exception reporting to standard error.

INTERFACE
MODULE PURE SUBROUTINE setQuietMode_all( Obj, Ans )
  CLASS( ExceptionHandler_ ), INTENT( INOUT ) :: Obj
  LOGICAL( LGT ),INTENT( IN ) :: Ans
END SUBROUTINE setQuietMode_all
END INTERFACE

!----------------------------------------------------------------------------
!                                                  setQuietMode@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	4 April 2021
! summary: Suppress/Unsupress which exceptions will be reported to standard

INTERFACE
MODULE PURE SUBROUTINE setQuietMode_eCode( Obj, eCode, Ans)
  CLASS( ExceptionHandler_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: eCode
  LOGICAL( LGT ), INTENT( IN ) :: Ans
END SUBROUTINE setQuietMode_eCode
END INTERFACE

!----------------------------------------------------------------------------
!                                                  setQuietMode@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	4 April 2021
! summary: Get the status of the quiet mode. Whether or not exception reporting

INTERFACE
MODULE PURE SUBROUTINE setQuietMode_array( Obj, Ans )
  CLASS( ExceptionHandler_ ), INTENT( INOUT ) :: Obj
  LOGICAL( LGT ), INTENT( IN ) :: Ans(:)
END SUBROUTINE setQuietMode_array
END INTERFACE

!----------------------------------------------------------------------------
!                                                        isQuietMode@Enquire
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION isQuietMode_all( Obj ) RESULT( Ans )
  CLASS( ExceptionHandler_ ), INTENT( IN ) :: Obj
  LOGICAL( LGT ) :: Ans
END FUNCTION isQuietMode_all
END INTERFACE

!----------------------------------------------------------------------------
!                                                        isQuietMode@Enquire
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION isQuietMode_eCode( Obj, eCode ) RESULT( Ans )
  CLASS( ExceptionHandler_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: eCode
  LOGICAL( LGT ) :: Ans
END FUNCTION isQuietMode_eCode
END INTERFACE

!----------------------------------------------------------------------------
!                                                       setVerbose@setMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE setVerboseMode_all( Obj, Ans )
  CLASS( ExceptionHandler_ ), INTENT( INOUT ) :: Obj
  LOGICAL( LGT ), INTENT( IN ) :: Ans
END SUBROUTINE setVerboseMode_all
END INTERFACE

!----------------------------------------------------------------------------
!                                                       setVerbose@setMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE setVerboseMode_eCode( Obj, eCode, Ans )
  CLASS( ExceptionHandler_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: eCode
  LOGICAL( LGT ), INTENT( IN ) :: Ans
END SUBROUTINE setVerboseMode_eCode
END INTERFACE

!----------------------------------------------------------------------------
!                                                       setVerbose@setMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE setVerboseMode_array( Obj, Ans )
  CLASS( ExceptionHandler_ ), INTENT( INOUT ) :: Obj
  LOGICAL( LGT ), INTENT( IN ) :: Ans(:)
END SUBROUTINE setVerboseMode_array
END INTERFACE

!----------------------------------------------------------------------------
!                                                     isVerboseMode@Enquire
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION isVerboseMode_all( Obj ) RESULT( Ans )
  CLASS( ExceptionHandler_ ), INTENT( IN ) :: Obj
  LOGICAL( LGT ) :: Ans
END FUNCTION isVerboseMode_all
END INTERFACE

!----------------------------------------------------------------------------
!                                                     isVerboseMode@Enquire
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION isVerboseMode_eCode( Obj, eCode ) RESULT( Ans )
  CLASS( ExceptionHandler_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: eCode
  LOGICAL( LGT ) :: Ans
END FUNCTION isVerboseMode_eCode
END INTERFACE

!----------------------------------------------------------------------------
!                                                   setLogFileUnit@setMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE setLogFileUnit( Obj, unit )
  CLASS( ExceptionHandler_ ), INTENT( INOUT ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: unit
END SUBROUTINE setLogFileUnit
END INTERFACE

!----------------------------------------------------------------------------
!                                                   getLogFileUnit@getMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION getLogFileUnit( Obj ) RESULT( Ans )
  CLASS( ExceptionHandler_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ) :: Ans
END FUNCTION getLogFileUnit
END INTERFACE

!----------------------------------------------------------------------------
!                                                    setLogActive@setMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE setLogActive( Obj, isactive )
  CLASS( ExceptionHandler_ ), INTENT( INOUT ) :: Obj
  LOGICAL( LGT ), INTENT( IN ) :: isactive
END SUBROUTINE setLogActive
END INTERFACE

!----------------------------------------------------------------------------
!                                                       isLogActive@Enquire
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION isLogActive( Obj ) RESULT( Ans )
  CLASS( ExceptionHandler_ ) :: Obj
  LOGICAL( LGT ) :: Ans
END FUNCTION isLogActive
END INTERFACE

!----------------------------------------------------------------------------
!                                                     checkLogFileOK@Enquire
!----------------------------------------------------------------------------

INTERFACE
MODULE RECURSIVE SUBROUTINE checkLogFileOK( Obj )
  CLASS( ExceptionHandler_ ), INTENT( INOUT ) :: Obj
END SUBROUTINE checkLogFileOK
END INTERFACE

!----------------------------------------------------------------------------
!                                                   setStopOnError@setMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE setStopOnError( Obj, Ans )
  CLASS( ExceptionHandler_ ), INTENT( INOUT ) :: Obj
  LOGICAL( LGT ), INTENT( IN ) :: Ans
END SUBROUTINE setStopOnError
END INTERFACE

!----------------------------------------------------------------------------
!                                                      isStopOnError@Enquire
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION isStopOnError( Obj ) RESULT( Ans )
  CLASS( ExceptionHandler_ ), INTENT( IN ) :: Obj
  LOGICAL( LGT ) :: Ans
END FUNCTION isStopOnError
END INTERFACE

!----------------------------------------------------------------------------
!                                               raiseInformation@raiseMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE raiseInformation( Obj, mesg )
  CLASS( ExceptionHandler_ ), INTENT( INOUT ) :: Obj
  CHARACTER( LEN=* ),INTENT( IN ) :: mesg
END SUBROUTINE raiseInformation
END INTERFACE

!----------------------------------------------------------------------------
!                                                  raiseWarning@raiseMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE raiseWarning( Obj, mesg )
  CLASS( ExceptionHandler_ ), INTENT( INOUT ) :: Obj
  CHARACTER( LEN=* ),INTENT( IN ) :: mesg
END SUBROUTINE raiseWarning
END INTERFACE

!----------------------------------------------------------------------------
!                                                     raiseDebug@raiseMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE raiseDebug( Obj, mesg )
  CLASS( ExceptionHandler_ ), INTENT( INOUT ) :: Obj
  CHARACTER( LEN=* ),INTENT( IN ) :: mesg
END SUBROUTINE raiseDebug
END INTERFACE

!----------------------------------------------------------------------------
!                                                    raiseError@raiseMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE raiseError( Obj, mesg )
  CLASS( ExceptionHandler_ ), INTENT( INOUT ) :: Obj
  CHARACTER( LEN=* ),INTENT( IN ) :: mesg
END SUBROUTINE raiseError
END INTERFACE

!----------------------------------------------------------------------------
!                                               raiseFatalError@raiseMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE raiseFatalError( Obj, mesg )
  CLASS( ExceptionHandler_ ), INTENT( INOUT ) :: Obj
  CHARACTER( LEN=* ),INTENT( IN ) :: mesg
END SUBROUTINE raiseFatalError
END INTERFACE

END MODULE ExceptionHandler_Class

