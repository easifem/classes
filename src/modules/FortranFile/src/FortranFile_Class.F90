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
USE ExceptionHandler_Class, ONLY: e, EXCEPTION_ERROR
USE AbstractFile_Class
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = 'FortranFile_Class'
CHARACTER(*), PARAMETER :: hash = "#"
CHARACTER(*), PARAMETER :: comma = ","
INTEGER(I4B), PARAMETER :: maxStrLen = 256
PUBLIC :: FortranFile_
PUBLIC :: FortranFilePointer_
PUBLIC :: FortranFileInitiate
PUBLIC :: FortranFileDeallocate
PUBLIC :: FortranFileBackspace
PUBLIC :: FortranFileRewind

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-07
! update: 2021-11-07
! summary: Datatype for handling fortran files
!
!{!pages/FortranFile_.md!}

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
  CHARACTER(6) :: posopt = 'ASIS  '
  CHARACTER(1), PUBLIC :: comment = hash
  CHARACTER(1), PUBLIC :: separator = " "
  CHARACTER(2), PUBLIC :: delimiter = "\n"

CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  !! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(Obj) :: Initiate => ff_Initiate
  PROCEDURE, PUBLIC, PASS(Obj) :: DEALLOCATE => ff_Deallocate
  FINAL :: ff_final
  PROCEDURE, PUBLIC, PASS(Obj) :: OPEN => ff_Open
  PROCEDURE, PUBLIC, PASS(Obj) :: CLOSE => ff_Close
  PROCEDURE, PUBLIC, PASS(Obj) :: Delete => ff_Delete
  PROCEDURE, PUBLIC, PASS(Obj) :: BACKSPACE => ff_Backspace
  PROCEDURE, PUBLIC, PASS(Obj) :: REWIND => ff_Rewind

  ! SET:
  ! @SetMethods
  PROCEDURE, PUBLIC, PASS(Obj) :: SetStatus => ff_SetStatus

  ! GET:
  ! @GetMethods
  PROCEDURE, PUBLIC, PASS(Obj) :: GetUnitNo => ff_GetUnitNo
  PROCEDURE, PUBLIC, PASS(Obj) :: GetRecLen => ff_GetRecLen

  ! GET:
  ! @EnquireMethods
  PROCEDURE, PUBLIC, PASS(Obj) :: IsFormatted => ff_IsFormatted
  PROCEDURE, PUBLIC, PASS(Obj) :: IsDirect => ff_IsDirect
  PROCEDURE, PUBLIC, PASS(Obj) :: IsPadded => ff_IsPadded
  PROCEDURE, PUBLIC, PASS(Obj) :: IsNew => ff_IsNew
  PROCEDURE, PUBLIC, PASS(Obj) :: IsOverwrite => ff_IsOverwrite
  PROCEDURE, PUBLIC, PASS(Obj) :: IsInitiated => ff_IsInitiated
END TYPE FortranFile_

TYPE(FortranFile_), PUBLIC, PARAMETER :: TypeFortranFile = FortranFile_()

TYPE :: FortranFilePointer_
  CLASS(FortranFile_), POINTER :: ptr => NULL()
END TYPE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Initiate the fortran file

INTERFACE FortranFileInitiate
  MODULE SUBROUTINE ff_initiate(obj, filename, unit, status, access, form, &
    & position, action, pad, recl, comment, separator, delimiter)
    CLASS(FortranFile_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: filename
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unit
    !! unit number, should not be equal to `stdout, stdin, stderr`
    CHARACTER(*), OPTIONAL, INTENT(IN) :: status
    !! OLD, NEW, SCRATCH, REPLACE, UNKNOWN
    !! If UNKNOWN then we use REPLACE
    !! Default is REPLACE
    CHARACTER(*), OPTIONAL, INTENT(IN) :: access
    !! DIRECT, SEQUENTIAL, STREAM
    !! Default is SEQUENTIAL
    CHARACTER(*), OPTIONAL, INTENT(IN) :: form
    !! FORMATTED, UNFORMATTED
    !! Default is FORMATTED
    CHARACTER(*), OPTIONAL, INTENT(IN) :: position
    !! REWIND, APPEND, ASIS
    !! Default is ASIS
    CHARACTER(*), OPTIONAL, INTENT(IN) :: action
    !! READ, WRITE, READWRITE
    !! Default is READWRITE
    CHARACTER(*), OPTIONAL, INTENT(IN) :: pad
    !! YES, NO
    !! Default is YES
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: recl
    CHARACTER(*), OPTIONAL, INTENT(IN) :: comment
    CHARACTER(*), OPTIONAL, INTENT(IN) :: separator
    CHARACTER(*), OPTIONAL, INTENT(IN) :: delimiter
  END SUBROUTINE ff_initiate
END INTERFACE FortranFileInitiate

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July, 2022
! summary:         Clear the content of fortran file

INTERFACE FortranFileDeallocate
  MODULE SUBROUTINE ff_Deallocate(obj, delete)
    CLASS(FortranFile_), INTENT(INOUT) :: obj
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: delete
  END SUBROUTINE ff_Deallocate
END INTERFACE FortranFileDeallocate

!----------------------------------------------------------------------------
!                                                  Final@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July, 2022
! summary:         Deallocate the content of fortran file

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

INTERFACE FortranFileBackspace
  MODULE SUBROUTINE ff_backspace(obj)
    CLASS(FortranFile_), INTENT(INOUT) :: obj
  END SUBROUTINE ff_backspace
END INTERFACE FortranFileBackspace

!----------------------------------------------------------------------------
!                                                  Rewind@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary:         Move to the begining

INTERFACE FortranFileRewind
  MODULE SUBROUTINE ff_rewind(obj)
    CLASS(FortranFile_), INTENT(INOUT) :: obj
  END SUBROUTINE ff_rewind
END INTERFACE FortranFileRewind

!----------------------------------------------------------------------------
!                                                   ff_setStatus@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Set the status of the file

INTERFACE
  MODULE SUBROUTINE ff_setStatus(obj, status)
    CLASS(FortranFile_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: status
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
