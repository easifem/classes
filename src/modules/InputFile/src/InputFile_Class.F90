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
! date: 	2 May 2021
! summary: 	module for I/O defines the derived type for a Log File object.
!
! The log file type is an extension of the Fortran file type. Specifically it
! is a write-only text file that overwrites any existing file when opened. The
! purpose of the log file is to provide a log of the execution of a program or
! subprogram. It provides additional methods "message" which writes a message
! to the log file and
! optionally to the prompt. The "setEcho"
! method can be used to control echoeing of messages to the prompt, similarly
! the "message" method has an optional input to over-ride the value of
! echostat.

MODULE InputFile_Class
USE GlobalData, ONLY: DFP, I4B, LGT, stdout, stderr, stdin
USE String_Class, ONLY : String
USE ExceptionHandler_Class
USE FortranFile_Class
IMPLICIT NONE
PRIVATE
CHARACTER(LEN=*),PARAMETER :: modName='InputFile_Class'
INTEGER(I4B), PARAMETER :: maxStrLen=256
TYPE(ExceptionHandler_), PRIVATE :: e

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE, EXTENDS( FortranFile_ ) :: InputFile_
  PRIVATE
  LOGICAL(LGT) :: echostat=.FALSE.
    !! Control variable for echoing messages to standard output.
  INTEGER(I4B) :: echounit=-1
    !! Unit number for the file to echo result of fgetl to.
  CHARACTER(LEN=1) :: probe=''
    !! The first character of oneline
  CHARACTER(LEN=1) :: lastprobe=''
    !! The first character of the line before the line that was just read
  CONTAINS
    PRIVATE
    PROCEDURE, PUBLIC, PASS( obj ) :: addSurrogate => inp_addSurrogate
    PROCEDURE, PUBLIC, PASS(obj) :: initiate => inp_initiate
    PROCEDURE, PUBLIC, PASS(obj) :: rewind => inp_rewind
    PROCEDURE, PUBLIC, PASS(obj) :: backspace => inp_backspace
    PROCEDURE, PUBLIC, PASS(obj) :: clear => inp_clear
    PROCEDURE, PUBLIC, PASS(obj) :: readline => inp_readline
    PROCEDURE, PUBLIC, PASS(obj) :: setEchoStat => inp_setEchoStat
    PROCEDURE, PUBLIC, PASS(obj) :: getEchoStat => inp_getEchoStat
    PROCEDURE, PUBLIC, PASS(obj) :: setEchoUnit => inp_setEchoUnit
    PROCEDURE, PUBLIC, PASS(obj) :: getEchoUnit => inp_getEchoUnit
    PROCEDURE, PUBLIC, PASS(obj) :: getProbe => inp_getProbe
END TYPE InputFile_

PUBLIC :: InputFile_
TYPE( InputFile_ ), PUBLIC, PARAMETER :: TypeInputFile = InputFile_()

TYPE :: InputFilePointer_
  CLASS( InputFile_ ), POINTER :: Ptr => NULL()
END TYPE

PUBLIC :: InputFilePointer_

!----------------------------------------------------------------------------
!                                                               addSurrogate
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE inp_addSurrogate( obj, UserObj )
  CLASS( InputFile_ ), INTENT( INOUT ) :: obj
  TYPE( ExceptionHandler_ ), INTENT( IN ) :: UserObj
END SUBROUTINE inp_addSurrogate
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	2 May 2021
! summary: 	Initiate input file
!
! - Initializes the input file object.
! - `obj` input file object.
! - `unit` Unit number to use for the file.
! - `file` Full path name of the file.
! - `status` Optional input is not used by this routine.
! - `access` Optional input is not used by this routine.
! - `form` Optional input is not used by this routine.
! - `position` Optional input is not used by this routine.
! - `action` Optional input is not used by this routine.
! - `pad` Optional input is not used by this routine.
! - `recl` Optional input is not used by this routine.

INTERFACE
MODULE SUBROUTINE inp_initiate(obj,filename,unit,status,access,form, &
  & position,action,pad,recl, comment, separator, delimiter)
  CLASS( InputFile_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: filename
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unit
  CHARACTER( LEN=* ), OPTIONAL, INTENT( IN ) :: status
  CHARACTER( LEN=* ), OPTIONAL, INTENT( IN ) :: access
  CHARACTER( LEN=* ), OPTIONAL, INTENT( IN ) :: form
  CHARACTER( LEN=* ), OPTIONAL, INTENT( IN ) :: position
  CHARACTER( LEN=* ), OPTIONAL, INTENT( IN ) :: action
  CHARACTER( LEN=* ), OPTIONAL, INTENT( IN ) :: pad
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: recl
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: comment
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: separator
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: delimiter
END SUBROUTINE inp_initiate
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE inp_rewind( Obj )
  CLASS( InputFile_ ), INTENT( INOUT ) :: obj
END SUBROUTINE inp_rewind
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------


INTERFACE
MODULE SUBROUTINE inp_backspace( obj )
  CLASS( InputFile_ ), INTENT( INOUT ) :: obj
END SUBROUTINE inp_backspace
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE inp_clear( obj, Delete )
  CLASS( InputFile_ ), INTENT( INOUT ) :: obj
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: Delete
END SUBROUTINE inp_clear
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE inp_readLine( obj, line )
  CLASS( InputFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( OUT ) :: line
END SUBROUTINE inp_readLine
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE inp_setEchoStat( obj, bool )
  CLASS( InputFile_ ), INTENT( INOUT ) :: obj
  LOGICAL( LGT ), INTENT( IN ) :: bool
END SUBROUTINE inp_setEchoStat
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION inp_getEchoStat( obj ) RESULT( ans )
  CLASS( InputFile_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ) :: ans
END FUNCTION inp_getEchoStat
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE inp_setEchoUnit( obj, unitno )
  CLASS( InputFile_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: unitno
END SUBROUTINE inp_setEchoUnit
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION inp_getEchoUnit( obj ) RESULT( ans )
  CLASS( InputFile_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: ans
END FUNCTION inp_getEchoUnit
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION inp_getProbe( obj ) RESULT( ans )
  CLASS( InputFile_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = 1 ) :: ans
END FUNCTION inp_getProbe
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE InputFile_Class