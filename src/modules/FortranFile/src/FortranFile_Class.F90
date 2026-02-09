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

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-08
! summary: FortranFile_Class module defines FortranFile_ class.
!
!# FortranFile
!
! FortranFile_Class define FortranFile_ class, which extends AbstractFile_.
! FortranFile_ will be extended to TxtFile_Class.
!
MODULE FortranFile_Class
USE GlobalData, ONLY: DFP, I4B, LGT
USE String_Class, ONLY: String
USE ExceptionHandler_Class, ONLY: e, EXCEPTION_ERROR
USE AbstractFile_Class, ONLY: AbstractFile_
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: fileopt => TypeFileOpt
IMPLICIT NONE

PRIVATE
CHARACTER(*), PARAMETER :: modName = 'FortranFile_Class()'

PUBLIC :: FortranFile_
PUBLIC :: FortranFilePointer_
PUBLIC :: FortranFileInitiate
PUBLIC :: FortranFileDeallocate
PUBLIC :: FortranFileBackspace
PUBLIC :: FortranFileRewind

!----------------------------------------------------------------------------
!                                                               FortranFile_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-07
! summary: Datatype for handling fortran files
!
!# FortranFile_
!
! `FortranFile_` is an extension of `AbstractFile_`.
! It provides a simplified interface to the native Fortran
! file capabilities and includes error checking.
!
! Note that `FortranFile_` does not provide any method to
! write and read data from the file.
!
! This is because the data may be written in ASCII or Binary format.
! For this reason `FortranFile_` is extended to following file formats.

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
  CHARACTER(1), PUBLIC :: comment = fileopt%hash
  CHARACTER(1), PUBLIC :: separator = fileopt%space
  CHARACTER(2), PUBLIC :: delimiter = fileopt%newline

CONTAINS
  PRIVATE

  !! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(Obj) :: Initiate => obj_Initiate
  PROCEDURE, PUBLIC, PASS(Obj) :: DEALLOCATE => obj_Deallocate
  FINAL :: obj_final
  PROCEDURE, PUBLIC, PASS(Obj) :: OPEN => obj_Open
  PROCEDURE, PUBLIC, PASS(Obj) :: CLOSE => obj_Close
  PROCEDURE, PUBLIC, PASS(Obj) :: Delete => obj_Delete
  PROCEDURE, PUBLIC, PASS(Obj) :: BACKSPACE => obj_Backspace
  PROCEDURE, PUBLIC, PASS(Obj) :: REWIND => obj_Rewind

  ! @SetMethods
  PROCEDURE, PUBLIC, PASS(Obj) :: SetStatus => obj_SetStatus

  ! @GetMethods
  PROCEDURE, PUBLIC, PASS(Obj) :: GetUnitNo => obj_GetUnitNo
  PROCEDURE, PUBLIC, PASS(Obj) :: GetRecLen => obj_GetRecLen

  ! @EnquireMethods
  PROCEDURE, PUBLIC, PASS(Obj) :: IsFormatted => obj_IsFormatted
  PROCEDURE, PUBLIC, PASS(Obj) :: IsDirect => obj_IsDirect
  PROCEDURE, PUBLIC, PASS(Obj) :: IsPadded => obj_IsPadded
  PROCEDURE, PUBLIC, PASS(Obj) :: IsNew => obj_IsNew
  PROCEDURE, PUBLIC, PASS(Obj) :: IsOverwrite => obj_IsOverwrite
  PROCEDURE, PUBLIC, PASS(Obj) :: IsInitiated => obj_IsInitiated
END TYPE FortranFile_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE(FortranFile_), PUBLIC, PARAMETER :: TypeFortranFile = FortranFile_()

!----------------------------------------------------------------------------
!                                                        FortranFilePointer_
!----------------------------------------------------------------------------

TYPE :: FortranFilePointer_
  CLASS(FortranFile_), POINTER :: ptr => NULL()
END TYPE FortranFilePointer_

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Initiate the fortran file

INTERFACE FortranFileInitiate
  MODULE SUBROUTINE obj_Initiate( &
    obj, filename, unit, status, access, form, position, action, pad, &
    recl, comment, separator, delimiter)
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
  END SUBROUTINE obj_Initiate
END INTERFACE FortranFileInitiate

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July, 2022
! summary: Clear the content of fortran file

INTERFACE FortranFileDeallocate
  MODULE SUBROUTINE obj_Deallocate(obj, delete)
    CLASS(FortranFile_), INTENT(INOUT) :: obj
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: delete
  END SUBROUTINE obj_Deallocate
END INTERFACE FortranFileDeallocate

!----------------------------------------------------------------------------
!                                                   Final@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July, 2022
! summary: Deallocate the content of fortran file

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(FortranFile_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                    Open@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Open the fortran file

INTERFACE
  MODULE SUBROUTINE obj_Open(obj)
    CLASS(FortranFile_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Open
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Close@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Close the fortran file

INTERFACE
  MODULE SUBROUTINE obj_Close(obj)
    CLASS(FortranFile_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Close
END INTERFACE

!----------------------------------------------------------------------------
!                                                  Delete@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Delete the fortran file

INTERFACE
  MODULE SUBROUTINE obj_Delete(obj)
    CLASS(FortranFile_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Delete
END INTERFACE

!----------------------------------------------------------------------------
!                                               Backspace@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Move one line back

INTERFACE FortranFileBackspace
  MODULE SUBROUTINE obj_Backspace(obj)
    CLASS(FortranFile_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Backspace
END INTERFACE FortranFileBackspace

!----------------------------------------------------------------------------
!                                                  Rewind@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary:         Move to the begining

INTERFACE FortranFileRewind
  MODULE SUBROUTINE obj_Rewind(obj)
    CLASS(FortranFile_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Rewind
END INTERFACE FortranFileRewind

!----------------------------------------------------------------------------
!                                                       SetStatus@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Set the status of the file

INTERFACE
  MODULE SUBROUTINE obj_SetStatus(obj, status)
    CLASS(FortranFile_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: status
    LOGICAL(LGT) :: ans
  END SUBROUTINE obj_SetStatus
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetUnitNo@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Get the unit number of the fortran file

INTERFACE
  MODULE PURE FUNCTION obj_GetUnitNo(obj) RESULT(ans)
    CLASS(FortranFile_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetUnitNo
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetRecLen@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Get the length of the record

INTERFACE
  MODULE PURE FUNCTION obj_GetRecLen(obj) RESULT(ans)
    CLASS(FortranFile_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetRecLen
END INTERFACE

!----------------------------------------------------------------------------
!                                                 IsFormatted@EnquireMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Returns true if the file is formatted

INTERFACE
  MODULE PURE FUNCTION obj_IsFormatted(obj) RESULT(ans)
    CLASS(FortranFile_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_IsFormatted
END INTERFACE

!----------------------------------------------------------------------------
!                                                    IsDirect@EnquireMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: returns true if direct access

INTERFACE
  MODULE PURE FUNCTION obj_IsDirect(obj) RESULT(ans)
    CLASS(FortranFile_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_IsDirect
END INTERFACE

!----------------------------------------------------------------------------
!                                                    IsPadded@EnquireMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Return true if padded

INTERFACE
  MODULE PURE FUNCTION obj_IsPadded(obj) RESULT(ans)
    CLASS(FortranFile_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_IsPadded
END INTERFACE

!----------------------------------------------------------------------------
!                                                       IsNew@EnquireMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Return true if the file is new

INTERFACE
  MODULE PURE FUNCTION obj_IsNew(obj) RESULT(Ans)
    CLASS(FortranFile_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_IsNew
END INTERFACE

!----------------------------------------------------------------------------
!                                                 IsOverwrite@EnquireMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Return true if overwrite status is set

INTERFACE
  MODULE PURE FUNCTION obj_IsOverwrite(obj) RESULT(Ans)
    CLASS(FortranFile_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_IsOverwrite
END INTERFACE

!----------------------------------------------------------------------------
!                                                IsInitiated@EnquireMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 July 2022
! summary: Returns true if the file is initiated

INTERFACE
  MODULE PURE FUNCTION obj_IsInitiated(obj) RESULT(Ans)
    CLASS(FortranFile_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_IsInitiated
END INTERFACE

END MODULE FortranFile_Class
