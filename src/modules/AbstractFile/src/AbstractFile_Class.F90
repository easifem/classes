! ThIs program Is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! ThIs program Is free software: you can redIstribute it and/or modify
! it under the terms of the GNU General Public License as publIshed by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! ThIs program Is dIstributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with thIs program.  If not, see <https: //www.gnu.org/licenses/>
!

!> authors: Vikas Sharma, Ph. D.
! date:         2 May 2021
! summary:         module for I/O defines the base file type object.
!
! The developement of thIs module Is inspired from the
! `FileType_Base.F90` of Futility package. The original source Is located at
! https://github.com/CASL/Futility/blob/master/src/FileType_Base.F90.
!
! The original code has been modified as per the code-standard of easifem
! library.
!
! ThIs type Is an abstract type, so it has no specific implementation. It
! exIsts only to provide a base for the extended types. It specifies the
! maximum lengths for the file path, name, and extension, some basic
! attributes of a file such as whether or not it Is open and also if it Is
! open for reading or writing are provided. Methods to interface to all
! attributes are also provided.
!
! Since thIs Is an abstract type, it has no specific implementation so see
! one of it's extended types for examples on how it should be used.

MODULE AbstractFile_Class
USE GlobalData
USE String_Class, ONLY: String
USE ExceptionHandler_Class, ONLY: e
IMPLICIT NONE
PRIVATE
!LIst of Public Members
CHARACTER(*), PARAMETER :: modName = 'AbstractFile_Class'
INTEGER(I4B), PARAMETER :: maxStrLen = 256
!! TYPE(ExceptionHandler_), PRIVATE :: e
  !! The exception handler for the object
  !! LIst of type bound procedures (methods) for the Base File Type object

PUBLIC :: AbstractFile_
PUBLIC :: AbstractFilePointer_
PUBLIC :: AbstractFileDeallocate

!----------------------------------------------------------------------------
!                                                             AbstractFile_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:         2 May 2021
! summary: A n abstract type which means it has no basic implementation

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
    !! Whether or not the file Is open
  LOGICAL(LGT) :: EOFstat = .FALSE.
    !! Whether or not the end of file has been reached
  LOGICAL(LGT) :: readstat = .FALSE.
    !! Whether or not the file Is open for reading
  LOGICAL(LGT) :: writestat = .FALSE.
    !! Whether or not the file Is open for writing
CONTAINS
  PRIVATE
  ! CONSTRUCTOR:1
  ! ConstructorMethods
  PROCEDURE, PUBLIC, PASS(Obj) :: DEALLOCATE => AbstractFileDeallocate

  ! SET:
  ! @SetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: SetFilePath => aFile_SetFilePath
  PROCEDURE, PUBLIC, PASS(obj) :: SetFileName => aFile_SetFileName
  PROCEDURE, PUBLIC, PASS(obj) :: SetFileExt => aFile_SetFileExt
  PROCEDURE, PUBLIC, PASS(obj) :: SetEOFstat => aFile_SetEOFStat
  PROCEDURE, PUBLIC, PASS(obj) :: SetOpenStat => aFile_SetOpenStat
  PROCEDURE, PUBLIC, PASS(obj) :: SetReadStat => aFile_SetReadStat
  PROCEDURE, PUBLIC, PASS(obj) :: SetWriteStat => aFile_SetWriteStat

  ! GET:
  ! @GetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: GetFilePath => aFile_GetFilePath
  PROCEDURE, PUBLIC, PASS(obj) :: GetFileName => aFile_GetFileName
  PROCEDURE, PUBLIC, PASS(obj) :: GetFileExt => aFile_GetFileExt
  PROCEDURE, PUBLIC, PASS(obj) :: GetFileParts => aFile_GetFileParts

  ! GET:
  ! @EnquireMethods
  PROCEDURE, PUBLIC, PASS(obj) :: IsOpen => aFile_IsOpen
  PROCEDURE, PUBLIC, PASS(obj) :: IsEOF => aFile_IsEOF
  PROCEDURE, PUBLIC, PASS(obj) :: IsRead => aFile_IsRead
  PROCEDURE, PUBLIC, PASS(obj) :: IsWrite => aFile_IsWrite

  ! SET:
  ! Deferred Methods
  PROCEDURE(aFile_open), PUBLIC, DEFERRED, PASS(obj) :: OPEN
  PROCEDURE(aFile_close), PUBLIC, DEFERRED, PASS(obj) :: CLOSE
  PROCEDURE(aFile_Delete), PUBLIC, DEFERRED, PASS(obj) :: Delete
END TYPE AbstractFile_

!----------------------------------------------------------------------------
!                                                       AbstractFilePointer_
!----------------------------------------------------------------------------

TYPE :: AbstractFilePointer_
  CLASS(AbstractFile_), POINTER :: ptr => NULL()
END TYPE AbstractFilePointer_

!----------------------------------------------------------------------------
!                                             Dealalocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 May 2022
! summary: Deallocate the data stored in the file

INTERFACE
  MODULE SUBROUTINE AbstractFileDeallocate(obj, Delete)
    CLASS(AbstractFile_), INTENT(INOUT) :: obj
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: Delete
  END SUBROUTINE AbstractFileDeallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                    SetFilePath@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 May 2022
! summary: Set the path of file

INTERFACE
  MODULE SUBROUTINE aFile_SetFilePath(obj, path)
    CLASS(AbstractFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: path
  END SUBROUTINE aFile_SetFilePath
END INTERFACE

!----------------------------------------------------------------------------
!                                                    SetFileName@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 May 2022
! summary: Set the file name

INTERFACE
  MODULE SUBROUTINE aFile_SetFileName(obj, fileName)
    CLASS(AbstractFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: fileName
  END SUBROUTINE aFile_SetFileName
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetFileExt@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 May 2022
! summary: Set the file extension

INTERFACE
  MODULE SUBROUTINE aFile_SetFileExt(obj, Ext)
    CLASS(AbstractFile_), INTENT(INOUT) :: obj
    TYPE(String), INTENT(IN) :: Ext
  END SUBROUTINE aFile_SetFileExt
END INTERFACE

!----------------------------------------------------------------------------
!                                                      SetEOFstat@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 May 2022
! summary: Set the end of file status of file

INTERFACE
  MODULE SUBROUTINE aFile_SetEOFstat(obj, stat)
    CLASS(AbstractFile_), INTENT(INOUT) :: obj
    LOGICAL(LGT), INTENT(IN) :: stat
  END SUBROUTINE aFile_SetEOFstat
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetOpenStat@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 May 2022
! summary: Set the openStat

INTERFACE
  MODULE SUBROUTINE aFile_SetOpenStat(obj, stat)
    CLASS(AbstractFile_), INTENT(INOUT) :: obj
    LOGICAL(LGT), INTENT(IN) :: stat
  END SUBROUTINE aFile_SetOpenStat
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetReadStat@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 May 2022
! summary: Set the readStat

INTERFACE
  MODULE SUBROUTINE aFile_SetReadStat(obj, stat)
    CLASS(AbstractFile_), INTENT(INOUT) :: obj
    LOGICAL(LGT), INTENT(IN) :: stat
  END SUBROUTINE aFile_SetReadStat
END INTERFACE

!----------------------------------------------------------------------------
!                                                    SetWriteStat@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 May 2022
! summary: Set the writeStat

INTERFACE
  MODULE SUBROUTINE aFile_SetWriteStat(obj, stat)
    CLASS(AbstractFile_), INTENT(INOUT) :: obj
    LOGICAL(LGT), INTENT(IN) :: stat
  END SUBROUTINE aFile_SetWriteStat
END INTERFACE

!----------------------------------------------------------------------------
!                                                    GetFilePath@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 May 2022
! summary: Returns the path of the file

INTERFACE
  MODULE FUNCTION aFile_GetFilePath(obj) RESULT(path)
    CLASS(AbstractFile_), INTENT(IN) :: obj
    TYPE(String) :: path
  END FUNCTION aFile_GetFilePath
END INTERFACE

!----------------------------------------------------------------------------
!                                                    GetFileName@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 May 2022
! summary: Returns the name of the file

INTERFACE
  MODULE FUNCTION aFile_GetFileName(obj) RESULT(fileName)
    CLASS(AbstractFile_), INTENT(IN) :: obj
    TYPE(String) :: fileName
  END FUNCTION aFile_GetFileName
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetFileExt@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 May 2022
! summary: Returns the extension of the file

INTERFACE
  MODULE FUNCTION aFile_GetFileExt(obj) RESULT(Ext)
    CLASS(AbstractFile_), INTENT(IN) :: obj
    TYPE(String) :: Ext
  END FUNCTION aFile_GetFileExt
END INTERFACE

!----------------------------------------------------------------------------
!                                                    GetFileParts@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 May 2022
! summary: Returns the path, filename, and extension of the file

INTERFACE
  MODULE PURE SUBROUTINE aFile_GetFileParts(obj, path, fileName, ext)
    CLASS(AbstractFile_), INTENT(IN) :: obj
    TYPE(String), INTENT(OUT) :: path
    TYPE(String), INTENT(OUT) :: fileName
    TYPE(String), INTENT(OUT) :: ext
  END SUBROUTINE aFile_GetFileParts
END INTERFACE

!----------------------------------------------------------------------------
!                                                         IsOpen@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 May 2022
! summary: Returns true if the file Is open

INTERFACE
  MODULE FUNCTION aFile_IsOpen(obj) RESULT(ans)
    CLASS(AbstractFile_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION aFile_IsOpen
END INTERFACE

!----------------------------------------------------------------------------
!                                                      IsEOF@EnquireMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 May 2022
! summary: Returns true if the end of the file Is reached

INTERFACE
  MODULE FUNCTION aFile_IsEOF(obj) RESULT(ans)
    CLASS(AbstractFile_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION aFile_IsEOF
END INTERFACE

!----------------------------------------------------------------------------
!                                                      IsRead@EnquireMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 May 2022
! summary: Returns true if the file has read access

INTERFACE
  MODULE FUNCTION aFile_IsRead(obj) RESULT(ans)
    CLASS(AbstractFile_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION aFile_IsRead
END INTERFACE

!----------------------------------------------------------------------------
!                                                     IsWrite@EnquireMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 8 May 2022
! summary: Returns true if the file has write access

INTERFACE
  MODULE FUNCTION aFile_IsWrite(obj) RESULT(ans)
    CLASS(AbstractFile_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION aFile_IsWrite
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
