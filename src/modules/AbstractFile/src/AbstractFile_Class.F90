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
! date:         2 May 2021
! summary:         module for I/O defines the base file type object.
!
! The developement of this module is inspired from the
! `FileType_Base.F90` of Futility package. The original source is located at
! https://github.com/CASL/Futility/blob/master/src/FileType_Base.F90.
!
! The original code has been modified as per the code-standard of easifem
! library.
!
! This type is an abstract type, so it has no specific implementation. It
! exists only to provide a base for the extended types. It specifies the
! maximum lengths for the file path, name, and extension, some basic
! attributes of a file such as whether or not it is open and also if it is
! open for reading or writing are provided. Methods to interface to all
! attributes are also provided.
!
! Since this is an abstract type, it has no specific implementation so see
! one of it's extended types for examples on how it should be used.

MODULE AbstractFile_Class
USE GlobalData
USE String_Class, ONLY: String
USE ExceptionHandler_Class, ONLY: e
IMPLICIT NONE
PRIVATE
!List of Public Members
CHARACTER(LEN=*), PARAMETER :: modName = 'AbstractFile_Class'
INTEGER(I4B), PARAMETER :: maxStrLen = 256
!! TYPE(ExceptionHandler_), PRIVATE :: e
  !! The exception handler for the object
  !! List of type bound procedures (methods) for the Base File Type object

!----------------------------------------------------------------------------
!                                                             AbstractFile_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         2 May 2021
! summary: This is an abstract type which means it has no basic implementation

TYPE, ABSTRACT :: AbstractFile_
  PRIVATE
  INTEGER(I4B) :: pathlen = 0
    !! The length of the path of the file
  INTEGER(I4B) :: fnamelen = 0
    !! The length of the file name string
  INTEGER(I4B) :: extlen = 0
    !! The length of the file name extension string
  TYPE(String) :: path
    !! The path of the file
  TYPE(String) :: fileName
    !! The name of the file (without the file extension)
  TYPE(String) :: ext
    !! The extension of the file name
  LOGICAL(LGT) :: openstat = .FALSE.
    !! Whether or not the file is open
  LOGICAL(LGT) :: EOFstat = .FALSE.
    !! Whether or not the end of file has been reached
  LOGICAL(LGT) :: readstat = .FALSE.
    !! Whether or not the file is open for reading
  LOGICAL(LGT) :: writestat = .FALSE.
    !! Whether or not the file is open for writing
CONTAINS
  PRIVATE
    !!
    !! ConstructorMethods
    !!
  PROCEDURE, PUBLIC, PASS(Obj) :: Deallocate => AbstractFileDeallocate
    !!
    !! @SetMethods
    !!
  PROCEDURE, PUBLIC, PASS(obj) :: setFilePath => aFile_setFilePath
  PROCEDURE, PUBLIC, PASS(obj) :: setFileName => aFile_setFileName
  PROCEDURE, PUBLIC, PASS(obj) :: setFileExt => aFile_setFileExt
  PROCEDURE, PUBLIC, PASS(obj) :: setEOFstat => aFile_setEOFStat
  PROCEDURE, PUBLIC, PASS(obj) :: setOpenStat => aFile_setOpenStat
  PROCEDURE, PUBLIC, PASS(obj) :: setReadStat => aFile_setReadStat
  PROCEDURE, PUBLIC, PASS(obj) :: setWriteStat => aFile_setWriteStat
    !!
    !! @GetMethods
    !!
  PROCEDURE, PUBLIC, PASS(obj) :: getFilePath => aFile_getFilePath
  PROCEDURE, PUBLIC, PASS(obj) :: getFileName => aFile_getFileName
  PROCEDURE, PUBLIC, PASS(obj) :: getFileExt => aFile_getFileExt
  PROCEDURE, PUBLIC, PASS(obj) :: getFileParts => aFile_getFileParts
    !!
    !! @EnquireMethods
    !!
  PROCEDURE, PUBLIC, PASS(obj) :: isOpen => aFile_isOpen
  PROCEDURE, PUBLIC, PASS(obj) :: isEOF => aFile_isEOF
  PROCEDURE, PUBLIC, PASS(obj) :: isRead => aFile_isRead
  PROCEDURE, PUBLIC, PASS(obj) :: isWrite => aFile_isWrite
    !!
    !! Deferred Methods
    !!
  PROCEDURE(aFile_open), PUBLIC, DEFERRED, PASS(obj) :: open
  PROCEDURE(aFile_close), PUBLIC, DEFERRED, PASS(obj) :: close
  PROCEDURE(aFile_delete), PUBLIC, DEFERRED, PASS(obj) :: delete
END TYPE AbstractFile_

PUBLIC :: AbstractFile_

!----------------------------------------------------------------------------
!                                                       AbstractFilePointer_
!----------------------------------------------------------------------------

TYPE :: AbstractFilePointer_
  CLASS(AbstractFile_), POINTER :: ptr => NULL()
END TYPE AbstractFilePointer_

PUBLIC :: AbstractFilePointer_

!----------------------------------------------------------------------------
!                                             Dealalocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 May 2022
! summary: Deallocate the data stored in the file

INTERFACE
  MODULE SUBROUTINE AbstractFileDeallocate(obj, delete)
    CLASS(AbstractFile_), INTENT(INOUT) :: obj
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: delete
  END SUBROUTINE AbstractFileDeallocate
END INTERFACE

PUBLIC :: AbstractFileDeallocate

!----------------------------------------------------------------------------
!                                                    setFilePath@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 May 2022
! summary: Set the path of file

INTERFACE
  MODULE SUBROUTINE aFile_setFilePath(obj, path)
    CLASS(AbstractFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: path
  END SUBROUTINE aFile_setFilePath
END INTERFACE

!----------------------------------------------------------------------------
!                                                    setFileName@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 May 2022
! summary: Set the file name

INTERFACE
  MODULE SUBROUTINE aFile_setFileName(obj, fileName)
    CLASS(AbstractFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: fileName
  END SUBROUTINE aFile_setFileName
END INTERFACE

!----------------------------------------------------------------------------
!                                                     setFileExt@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 May 2022
! summary: Set the file extension

INTERFACE
  MODULE SUBROUTINE aFile_setFileExt(obj, Ext)
    CLASS(AbstractFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: Ext
  END SUBROUTINE aFile_setFileExt
END INTERFACE

!----------------------------------------------------------------------------
!                                                      setEOFstat@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 May 2022
! summary: Set the end of file status of file

INTERFACE
  MODULE SUBROUTINE aFile_setEOFstat(obj, stat)
    CLASS(AbstractFile_), INTENT(INOUT) :: obj
    LOGICAL(LGT), INTENT(IN) :: stat
  END SUBROUTINE aFile_setEOFstat
END INTERFACE

!----------------------------------------------------------------------------
!                                                     setOpenStat@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 May 2022
! summary: Set the openStat

INTERFACE
  MODULE SUBROUTINE aFile_setOpenStat(obj, stat)
    CLASS(AbstractFile_), INTENT(INOUT) :: obj
    LOGICAL(LGT), INTENT(IN) :: stat
  END SUBROUTINE aFile_setOpenStat
END INTERFACE

!----------------------------------------------------------------------------
!                                                     setReadStat@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 May 2022
! summary: Set the readStat

INTERFACE
  MODULE SUBROUTINE aFile_setReadStat(obj, stat)
    CLASS(AbstractFile_), INTENT(INOUT) :: obj
    LOGICAL(LGT), INTENT(IN) :: stat
  END SUBROUTINE aFile_setReadStat
END INTERFACE

!----------------------------------------------------------------------------
!                                                    setWriteStat@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 May 2022
! summary: Set the writeStat

INTERFACE
  MODULE SUBROUTINE aFile_setWriteStat(obj, stat)
    CLASS(AbstractFile_), INTENT(INOUT) :: obj
    LOGICAL(LGT), INTENT(IN) :: stat
  END SUBROUTINE aFile_setWriteStat
END INTERFACE

!----------------------------------------------------------------------------
!                                                    getFilePath@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 May 2022
! summary: Returns the path of the file

INTERFACE
  MODULE FUNCTION aFile_getFilePath(obj) RESULT(path)
    CLASS(AbstractFile_), INTENT(IN) :: obj
    TYPE(String) :: path
  END FUNCTION aFile_getFilePath
END INTERFACE

!----------------------------------------------------------------------------
!                                                    getFileName@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 May 2022
! summary: Returns the name of the file

INTERFACE
  MODULE FUNCTION aFile_getFileName(obj) RESULT(fileName)
    CLASS(AbstractFile_), INTENT(IN) :: obj
    TYPE(String) :: fileName
  END FUNCTION aFile_getFileName
END INTERFACE

!----------------------------------------------------------------------------
!                                                     getFileExt@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 May 2022
! summary: Returns the extension of the file

INTERFACE
  MODULE FUNCTION aFile_getFileExt(obj) RESULT(Ext)
    CLASS(AbstractFile_), INTENT(IN) :: obj
    TYPE(String) :: Ext
  END FUNCTION aFile_getFileExt
END INTERFACE

!----------------------------------------------------------------------------
!                                                    getFileParts@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 May 2022
! summary: Returns the path, filename, and extension of the file

INTERFACE
  MODULE PURE SUBROUTINE aFile_getFileParts(obj, path, fileName, ext)
    CLASS(AbstractFile_), INTENT(IN) :: obj
    TYPE(String), INTENT(OUT) :: path
    TYPE(String), INTENT(OUT) :: fileName
    TYPE(String), INTENT(OUT) :: ext
  END SUBROUTINE aFile_getFileParts
END INTERFACE

!----------------------------------------------------------------------------
!                                                         isOpen@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 May 2022
! summary: Returns true if the file is open

INTERFACE
  MODULE FUNCTION aFile_isOpen(obj) RESULT(ans)
    CLASS(AbstractFile_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION aFile_isOpen
END INTERFACE

!----------------------------------------------------------------------------
!                                                      isEOF@EnquireMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 May 2022
! summary: Returns true if the end of the file is reached

INTERFACE
  MODULE FUNCTION aFile_isEOF(obj) RESULT(ans)
    CLASS(AbstractFile_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION aFile_isEOF
END INTERFACE

!----------------------------------------------------------------------------
!                                                      isRead@EnquireMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 May 2022
! summary: Returns true if the file has read access

INTERFACE
  MODULE FUNCTION aFile_isRead(obj) RESULT(ans)
    CLASS(AbstractFile_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION aFile_isRead
END INTERFACE

!----------------------------------------------------------------------------
!                                                     isWrite@EnquireMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 May 2022
! summary: Returns true if the file has write access

INTERFACE
  MODULE FUNCTION aFile_isWrite(obj) RESULT(ans)
    CLASS(AbstractFile_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION aFile_isWrite
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 May 2022
! summary: Open the file

ABSTRACT INTERFACE
  SUBROUTINE aFile_Open(obj)
    IMPORT :: AbstractFile_
    CLASS(AbstractFile_), INTENT(INOUT) :: obj
  END SUBROUTINE aFile_Open
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 May 2022
! summary: Close the file

ABSTRACT INTERFACE
  SUBROUTINE aFile_Close(obj)
    IMPORT :: AbstractFile_
    CLASS(AbstractFile_), INTENT(INOUT) :: obj
  END SUBROUTINE aFile_Close
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 May 2022
! summary: Delete the file

ABSTRACT INTERFACE
  SUBROUTINE aFile_Delete(obj)
    IMPORT :: AbstractFile_
    CLASS(AbstractFile_), INTENT(INOUT) :: obj
  END SUBROUTINE aFile_Delete
END INTERFACE

ENDMODULE AbstractFile_Class

