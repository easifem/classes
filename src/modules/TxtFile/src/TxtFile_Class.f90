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
! summary: 	module for I/O defines the derived type for a Fortran File object.
!
! The developement of this module is inspired from the
! `FileType_Fortran.f90` of Futility package. The original source is located at https://github.com/CASL/Futility/blob/master/src/AbstractFile_.f90. The original code has been modified as per the code-standard of easifem library.
!
! The Fortan file type is an extension of the abstract `AbstractFile_`
! It provides a simplified interface to the native Fortran
! file capabilities and includes error checking.

MODULE TxtFile_Class
USE GlobalData, ONLY: DFP, I4B, LGT, stdout, stderr, stdin
USE BaseType, ONLY : String
USE ExceptionHandler_Class
USE FortranFile_Class
IMPLICIT NONE
PRIVATE
CHARACTER(LEN=*),PARAMETER :: modName='TXTFILE_CLASS'
INTEGER(I4B), PARAMETER :: maxStrLen=256
TYPE(ExceptionHandler_), PRIVATE :: e

!----------------------------------------------------------------------------
!                                                                 TxtFile_
!----------------------------------------------------------------------------

TYPE, EXTENDS(FortranFile_) :: TxtFile_
  PRIVATE
  LOGICAL( LGT ) :: echostat = .FALSE.
  INTEGER( I4B ) :: echounit = -1
  !
  CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS( obj ) :: addSurrogate => txt_addSurrogate
  PROCEDURE, PUBLIC, PASS( Obj ) :: initiate => txt_initiate
  PROCEDURE, PUBLIC, PASS( Obj ) :: clear => txt_clear
  PROCEDURE, PUBLIC, PASS( Obj ) :: readLine => txt_readLine
  PROCEDURE, PUBLIC, PASS( Obj ) :: setEchoStat => txt_setEchoStat
  PROCEDURE, PUBLIC, PASS( Obj ) :: getEchoStat => txt_getEchoStat
  PROCEDURE, PUBLIC, PASS( Obj ) :: setEchoUnit => txt_setEchoUnit
  PROCEDURE, PUBLIC, PASS( Obj ) :: getEchoUnit => txt_getEchoUnit
END TYPE TxtFile_

PUBLIC :: TxtFile_
TYPE( TxtFile_ ), PUBLIC, PARAMETER :: TypeTxtFile=TxtFile_()

TYPE :: TxtFilePointer_
  CLASS( TxtFile_ ), POINTER :: ptr => NULL()
END TYPE

PUBLIC :: TxtFilePointer_

!----------------------------------------------------------------------------
!                                                               addSurrogate
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE txt_addSurrogate( obj, UserObj )
  CLASS( TxtFile_ ), INTENT( INOUT ) :: obj
  TYPE( ExceptionHandler_ ), INTENT( IN ) :: UserObj
END SUBROUTINE txt_addSurrogate
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE txt_initiate(obj,file,unit,status,access,form, &
  & position,action,pad,recl, comment, separator, delimiter)
  CLASS( TxtFile_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: file
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unit
    !! User specified unit number, it should  not be equal to `stdout, stdin, stderr`
  CHARACTER( LEN = * ), OPTIONAL,INTENT( IN ) :: status
    !! OLD, NEW, SCRATCH, REPLACE, UNKNOWN
    !! If UNKNOWN then we use REPLACE
    !! Default is REPLACE
  CHARACTER( LEN = * ), OPTIONAL,INTENT( IN ) :: access
    !! DIRECT, SEQUENTIAL, STREAM
    !! Default is SEQUENTIAL
  CHARACTER( LEN = * ), OPTIONAL,INTENT( IN ) :: form
    !! FORMATTED, UNFORMATTED
    !! Default is FORMATTED
  CHARACTER( LEN = * ), OPTIONAL,INTENT( IN ) :: position
    !! REWIND, APPEND, ASIS
    !! Default is ASIS
  CHARACTER( LEN = * ), OPTIONAL,INTENT( IN ) :: action
    !! READ, WRITE, READWRITE
  CHARACTER( LEN = * ), OPTIONAL,INTENT( IN ) :: pad
    !! YES, NO
    !! Default is YES
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: recl
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: comment
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: separator
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: delimiter
END SUBROUTINE txt_initiate
END INTERFACE

INTERFACE InitiateTxtFile
  MODULE PROCEDURE txt_initiate
END INTERFACE InitiateTxtFile

PUBLIC :: InitiateTxtFile

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE txt_clear( obj, Delete )
  CLASS( TxtFile_ ), INTENT( INOUT ) :: obj
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: Delete
END SUBROUTINE txt_clear
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE txt_readLine( obj, line )
  CLASS( TxtFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( OUT ) :: line
END SUBROUTINE txt_readLine
END INTERFACE


!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE txt_setEchoStat( obj, bool )
  CLASS( TxtFile_ ), INTENT( INOUT ) :: obj
  LOGICAL( LGT ), INTENT( IN ) :: bool
END SUBROUTINE txt_setEchoStat
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION txt_getEchoStat( obj ) RESULT( ans )
  CLASS( TxtFile_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ) :: ans
END FUNCTION txt_getEchoStat
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE txt_setEchoUnit( obj, unitno )
  CLASS( TxtFile_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: unitno
END SUBROUTINE txt_setEchoUnit
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION txt_getEchoUnit( obj ) RESULT( ans )
  CLASS( TxtFile_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: ans
END FUNCTION txt_getEchoUnit
END INTERFACE

END MODULE TxtFile_Class
