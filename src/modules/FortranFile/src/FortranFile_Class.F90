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
! date: 2 May 2021
! summary: module for I/O defines the derived type for a Fortran File object.
!
! The developement of this module is inspired from the
! `FileType_Fortran.F90` of Futility package. The original source is located
! at https://github.com/CASL/Futility/blob/master/src/AbstractFile_.F90. The
! original code has been modified as per the code-standard of easifem library.
!
! The Fortan file type is an extension of the abstract `AbstractFile_`
! It provides a simplified interface to the native Fortran
! file capabilities and includes error checking.

MODULE FortranFile_Class
USE GlobalData
USE String_Class, ONLY: String
USE ExceptionHandler_Class
USE AbstractFile_Class
IMPLICIT NONE
PRIVATE
CHARACTER(LEN=*),PARAMETER :: modName='FortranFile_Class'
CHARACTER(LEN=*),PARAMETER :: hash="#"
CHARACTER(LEN=*),PARAMETER :: comma=","
INTEGER(I4B), PARAMETER :: maxStrLen = 256
TYPE(ExceptionHandler_), PRIVATE :: e

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-07
! update: 2021-11-07
! summary: Datatype for handling fortran files
!
! {!pages/FortranFile_.md!}
!
! TODO
!
! Method-1
!   - name: ReadLine(obj, aline)
!   - spec: Read a line in string `aline`
!
! Method-2
!   - name: ReadLines(obj, lines())
!   - spec: Read lines in string vector lines()
!
! Reference:: [[String_Method.F90]]

TYPE, EXTENDS(AbstractFile_) :: FortranFile_
  PRIVATE
  LOGICAL(LGT) :: initstat = .FALSE.
    !! file initiated or not
  INTEGER(I4B) :: unitno = -1
    !! unit number
  INTEGER(I4B) :: reclval = -1
    !! record length for direct access
  LOGICAL(LGT) :: formatstat = .FALSE.
    !! file is formatted or not
  LOGICAL(LGT) :: accessstat = .FALSE.
    !! direct or sequential access
  LOGICAL(LGT) :: newstat = .FALSE.
    !! the new status of a file
  LOGICAL(LGT) :: overwrite = .FALSE.
    !! replace or not
  LOGICAL(LGT) :: padstat = .FALSE.
    !! Whether or not the file is being padded
  LOGICAL(LGT) :: getNewUnit = .FALSE.
  CHARACTER(LEN=6) :: posopt = 'ASIS  '
  CHARACTER(LEN=1), PUBLIC :: comment = hash
  CHARACTER(LEN=1), PUBLIC :: separator = " "
  CHARACTER(LEN=2), PUBLIC :: delimiter = "\n"
  !
CONTAINS
  PRIVATE
  !!
  !! @ConstructorMethods
  !!
  PROCEDURE, PUBLIC, PASS(obj) :: addSurrogate => ff_addSurrogate
  PROCEDURE, PUBLIC, PASS(Obj) :: initiate => ff_initiate
  PROCEDURE, PUBLIC, PASS(Obj) :: Deallocate => ff_Deallocate
  FINAL :: ff_final
  PROCEDURE, PUBLIC, PASS(Obj) :: open => ff_open
  PROCEDURE, PUBLIC, PASS(Obj) :: close => ff_close
  PROCEDURE, PUBLIC, PASS(Obj) :: delete => ff_delete
  PROCEDURE, PUBLIC, PASS(Obj) :: backspace => ff_backspace
  PROCEDURE, PUBLIC, PASS(Obj) :: rewind => ff_rewind
  !!
  !! @SetMethods
  !!
  PROCEDURE, PUBLIC, PASS(Obj) :: setStatus => ff_setStatus
  !!
  !! @GetMethods
  !!
  PROCEDURE, PUBLIC, PASS(Obj) :: getUnitNo => ff_getUnitNo
  PROCEDURE, PUBLIC, PASS(Obj) :: getRecLen => ff_getRecLen
  !!
  !! @EnquireMethods
  !!
  PROCEDURE, PUBLIC, PASS(Obj) :: isFormatted => ff_isFormatted
  PROCEDURE, PUBLIC, PASS(Obj) :: isDirect => ff_isDirect
  PROCEDURE, PUBLIC, PASS(Obj) :: isPadded => ff_isPadded
  PROCEDURE, PUBLIC, PASS(Obj) :: isNew => ff_isNew
  PROCEDURE, PUBLIC, PASS(Obj) :: isOverwrite => ff_isOverwrite
  PROCEDURE, PUBLIC, PASS(Obj) :: isInitiated => ff_isInitiated
END TYPE FortranFile_

PUBLIC :: FortranFile_
TYPE(FortranFile_), PUBLIC, PARAMETER :: TypeFortranFile = FortranFile_()

TYPE :: FortranFilePointer_
  CLASS(FortranFile_), POINTER :: ptr => NULL()
END TYPE

PUBLIC :: FortranFilePointer_

!----------------------------------------------------------------------------
!                                            addSurrogate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July, 2022
! summary: Add surrogate to the module

INTERFACE
  MODULE SUBROUTINE ff_addSurrogate(obj, UserObj)
    CLASS(FortranFile_), INTENT(INOUT) :: obj
    TYPE(ExceptionHandler_), INTENT(IN) :: UserObj
  END SUBROUTINE ff_addSurrogate
END INTERFACE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Initiate the fortran file

INTERFACE
  MODULE SUBROUTINE ff_initiate( obj, filename, unit, status, access, form, &
    & position, action, pad, recl, comment, separator, delimiter)
    CLASS(FortranFile_), INTENT(INOUT) :: obj
    CHARACTER(LEN=*), INTENT(IN) :: filename
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unit
    !! unit number, should not be equal to `stdout, stdin, stderr`
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: status
    !! OLD, NEW, SCRATCH, REPLACE, UNKNOWN
    !! If UNKNOWN then we use REPLACE
    !! Default is REPLACE
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: access
    !! DIRECT, SEQUENTIAL, STREAM
    !! Default is SEQUENTIAL
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: form
    !! FORMATTED, UNFORMATTED
    !! Default is FORMATTED
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: position
    !! REWIND, APPEND, ASIS
    !! Default is ASIS
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: action
    !! READ, WRITE, READWRITE
    !! Default is READWRITE
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: pad
    !! YES, NO
    !! Default is YES
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: recl
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: comment
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: separator
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: delimiter
  END SUBROUTINE ff_initiate
END INTERFACE

INTERFACE FortranFileInitiate
  MODULE PROCEDURE ff_initiate
END INTERFACE FortranFileInitiate

PUBLIC :: FortranFileInitiate

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July, 2022
! summary: 	Clear the content of fortran file

INTERFACE
  MODULE SUBROUTINE ff_Deallocate(obj, delete)
    CLASS(FortranFile_), INTENT(INOUT) :: obj
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: delete
  END SUBROUTINE ff_Deallocate
END INTERFACE

INTERFACE FortranFileDeallocate
  MODULE PROCEDURE ff_Deallocate
END INTERFACE FortranFileDeallocate

PUBLIC :: FortranFileDeallocate

!----------------------------------------------------------------------------
!                                                  Final@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July, 2022
! summary: 	Deallocate the content of fortran file

INTERFACE
  MODULE SUBROUTINE ff_Final(obj)
    TYPE(FortranFile_), INTENT(INOUT) :: obj
  END SUBROUTINE ff_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Open@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Open the fortran file

INTERFACE
  MODULE SUBROUTINE ff_open(obj)
    CLASS(FortranFile_), INTENT(INOUT) :: obj
  END SUBROUTINE ff_open
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Close@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Close the fortran file

INTERFACE
  MODULE SUBROUTINE ff_close(obj)
    CLASS(FortranFile_), INTENT(INOUT) :: obj
  END SUBROUTINE ff_close
END INTERFACE

!----------------------------------------------------------------------------
!                                                 Delete@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Delete the fortran file

INTERFACE
  MODULE SUBROUTINE ff_delete(obj)
    CLASS(FortranFile_), INTENT(INOUT) :: obj
  END SUBROUTINE ff_delete
END INTERFACE

!----------------------------------------------------------------------------
!                                              Backspace@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Move one line back

INTERFACE
  MODULE SUBROUTINE ff_backspace(obj)
    CLASS(FortranFile_), INTENT(INOUT) :: obj
  END SUBROUTINE ff_backspace
END INTERFACE

INTERFACE FortranFileBackspace
  MODULE PROCEDURE ff_backspace
END INTERFACE FortranFileBackspace

PUBLIC :: FortranFileBackspace

!----------------------------------------------------------------------------
!                                                  Rewind@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: 	Move to the begining

INTERFACE
  MODULE SUBROUTINE ff_rewind(obj)
    CLASS(FortranFile_), INTENT(INOUT) :: obj
  END SUBROUTINE ff_rewind
END INTERFACE

INTERFACE FortranFileRewind
  MODULE PROCEDURE ff_rewind
END INTERFACE FortranFileRewind

PUBLIC :: FortranFileRewind

!----------------------------------------------------------------------------
!                                                   ff_setStatus@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Set the status of the file

INTERFACE
  MODULE SUBROUTINE ff_setStatus(obj, status)
    CLASS(FortranFile_), INTENT(INOUT) :: obj
    CHARACTER(LEN=*), INTENT(IN) :: status
    LOGICAL(LGT) :: ans
  END SUBROUTINE ff_setStatus
END INTERFACE

!----------------------------------------------------------------------------
!                                                      getUnitNo@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Get the unit number of the fortran file

INTERFACE
  MODULE PURE FUNCTION ff_getUnitNo(obj) RESULT(ans)
    CLASS(FortranFile_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION ff_getUnitNo
END INTERFACE

!----------------------------------------------------------------------------
!                                                      getRecLen@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Get the length of the record

INTERFACE
  MODULE PURE FUNCTION ff_getRecLen(obj) RESULT(ans)
    CLASS(FortranFile_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION ff_getRecLen
END INTERFACE

!----------------------------------------------------------------------------
!                                                 isFormatted@EnquireMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Returns true if the file is formatted

INTERFACE
  MODULE PURE FUNCTION ff_isFormatted(obj) RESULT(ans)
    CLASS(FortranFile_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION ff_isFormatted
END INTERFACE

!----------------------------------------------------------------------------
!                                                    isDirect@EnquireMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: returns true if direct access

INTERFACE
  MODULE PURE FUNCTION ff_isDirect(obj) RESULT(ans)
    CLASS(FortranFile_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION ff_isDirect
END INTERFACE

!----------------------------------------------------------------------------
!                                                   isPadded@EnquireMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Return true if padded

INTERFACE
  MODULE PURE FUNCTION ff_isPadded(obj) RESULT(ans)
    CLASS(FortranFile_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION ff_isPadded
END INTERFACE

!----------------------------------------------------------------------------
!                                                      isNew@EnquireMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Return true if the file is new

INTERFACE
  MODULE PURE FUNCTION ff_isNew(obj) RESULT(Ans)
    CLASS(FortranFile_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION ff_isNew
END INTERFACE

!----------------------------------------------------------------------------
!                                                 isOverwrite@EnquireMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Return true if overwrite status is set

INTERFACE
  MODULE PURE FUNCTION ff_isOverwrite(obj) RESULT(Ans)
    CLASS(FortranFile_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION ff_isOverwrite
END INTERFACE

!----------------------------------------------------------------------------
!                                                isInitiated@EnquireMethods
!----------------------------------------------------------------------------


!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Returns true if the file is initiated

INTERFACE
  MODULE PURE FUNCTION ff_isInitiated(obj) RESULT(Ans)
    CLASS(FortranFile_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION ff_isInitiated
END INTERFACE

END MODULE FortranFile_Class
