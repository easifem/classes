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

!> author: Vikas Sharma, Ph. D.
! date: 2021-05-02
! update:
!   - 2026-10-27: renamed aFile_* to obj_*, docs
! summary: Abstract base type for files
!
! The development of this module is inspired from the
! `FileType_Base.F90` of Futility package. The original source is
! located at
! https://github.com/CASL/Futility/blob/master/src/FileType_Base.F90.
!
! The original code has been modified as per the code-standard of
! easifem library.
!
! AbstractFile_ is an abstract type, so it has no specific
! implementation. It exists only to provide a base for the extended
! types (for example [[FortranFile_]], [[HDF5File_]], and
! [[XMLFile_]]). It stores the path, name, and extension of a file,
! and some basic attributes of a file, such as whether or not it is
! open and whether it is open for reading or writing. Methods to get
! and set all these attributes are also provided. Open, Close, and
! Delete are deferred and implemented by the extended types.

MODULE AbstractFile_Class
USE ExceptionHandler_Class, ONLY: e
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE String_Class, ONLY: String
IMPLICIT NONE
PRIVATE

INTEGER(I4B), PARAMETER :: maxStrLen = 256

PUBLIC :: AbstractFile_
PUBLIC :: AbstractFilePointer_
PUBLIC :: AbstractFileDeallocate

!----------------------------------------------------------------------------
!                                                             AbstractFile_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-05-02
! update:
!   - 2026-10-27: renamed aFile_* to obj_*, docs
! summary: Abstract file type, base of all file types
!
! The full name of the file is made of three parts: path, fileName,
! and ext (which starts with a dot, for example `.txt`). How the full
! name is built from these parts depends on the extended type; for
! example [[FortranFile_]] uses `path // fileName // ext`, where path
! ends with the directory separator (for example `./`). The lengths
! of these parts (without trailing blanks) are stored in pathlen,
! fnamelen, and extlen.
!
! The logical components store the state of the file: openstat (the
! file is open), EOFstat (the end of file has been reached), readstat
! and writestat (the file has read and write access). The path, name,
! extension, read status, and write status can be changed only while
! the file is not open.
!
! The type has no default initialization method; extended types set
! the components with the Set methods (for example in Initiate) and
! reset them with [[AbstractFileDeallocate]].
!
! Open, Close, and Delete are deferred bindings, see [[obj_Open]],
! [[obj_Close]], and [[obj_Delete]].

TYPE, ABSTRACT :: AbstractFile_
  PRIVATE
  INTEGER(I4B) :: pathlen = 0
  !! length of the path of the file
  INTEGER(I4B) :: fnamelen = 0
  !! length of the file name (without extension)
  INTEGER(I4B) :: extlen = 0
  !! length of the file name extension
  TYPE(String) :: path
  !! path of the file (directory part of the full name)
  TYPE(String) :: fileName
  !! name of the file (without the file extension)
  TYPE(String) :: ext
  !! extension of the file name, including the dot
  LOGICAL(LGT) :: openstat = .FALSE.
  !! true if the file is open
  LOGICAL(LGT) :: EOFstat = .FALSE.
  !! true if the end of file has been reached
  LOGICAL(LGT) :: readstat = .FALSE.
  !! true if the file has read access
  LOGICAL(LGT) :: writestat = .FALSE.
  !! true if the file has write access
CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => AbstractFileDeallocate
  !! Reset the path, name, extension, and status of the file

  ! SET:
  ! @SetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: SetFilePath => obj_SetFilePath
  !! Set the path of the file
  PROCEDURE, PUBLIC, PASS(obj) :: SetFileName => obj_SetFileName
  !! Set the name of the file (without extension)
  PROCEDURE, PUBLIC, PASS(obj) :: SetFileExt => obj_SetFileExt
  !! Set the extension of the file
  PROCEDURE, PUBLIC, PASS(obj) :: SetEOFStat => obj_SetEOFStat
  !! Set the end of file status
  PROCEDURE, PUBLIC, PASS(obj) :: SetOpenStat => obj_SetOpenStat
  !! Set the open status
  PROCEDURE, PUBLIC, PASS(obj) :: SetReadStat => obj_SetReadStat
  !! Set the read access status
  PROCEDURE, PUBLIC, PASS(obj) :: SetWriteStat => obj_SetWriteStat
  !! Set the write access status

  ! GET:
  ! @GetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: GetFilePath => obj_GetFilePath
  !! Returns the path of the file
  PROCEDURE, PUBLIC, PASS(obj) :: GetFileName => obj_GetFileName
  !! Returns the name of the file (without extension)
  PROCEDURE, PUBLIC, PASS(obj) :: GetFileExt => obj_GetFileExt
  !! Returns the extension of the file
  PROCEDURE, PUBLIC, PASS(obj) :: GetFileParts => obj_GetFileParts
  !! Returns the path, name, and extension of the file

  ! GET:
  ! @EnquireMethods
  PROCEDURE, PUBLIC, PASS(obj) :: IsOpen => obj_IsOpen
  !! Returns true if the file is open
  PROCEDURE, PUBLIC, PASS(obj) :: IsEOF => obj_IsEOF
  !! Returns true if the end of file has been reached
  PROCEDURE, PUBLIC, PASS(obj) :: IsRead => obj_IsRead
  !! Returns true if the file has read access
  PROCEDURE, PUBLIC, PASS(obj) :: IsWrite => obj_IsWrite
  !! Returns true if the file has write access

  ! SET:
  ! Deferred Methods
  PROCEDURE(obj_Open), PUBLIC, DEFERRED, PASS(obj) :: OPEN
  !! Open the file (deferred)
  PROCEDURE(obj_Close), PUBLIC, DEFERRED, PASS(obj) :: CLOSE
  !! Close the file (deferred)
  PROCEDURE(obj_Delete), PUBLIC, DEFERRED, PASS(obj) :: Delete
  !! Delete the file (deferred)
END TYPE AbstractFile_

!----------------------------------------------------------------------------
!                                                       AbstractFilePointer_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-05-02
! update:
!   - 2026-10-27: docs
! summary: Container of a pointer to AbstractFile_
!
! It is used to store pointers to files of different extended types,
! for example in an array.

TYPE :: AbstractFilePointer_
  CLASS(AbstractFile_), POINTER :: ptr => NULL()
  !! pointer to a file
END TYPE AbstractFilePointer_

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-05-08
! update:
!   - 2026-10-27: docs
! summary: Reset the path, name, extension, and status of the file
!
! The path, fileName, and ext are set to empty strings, their lengths
! are set to zero, and the open, end of file, read, and write status
! are set to false. The file itself is not closed or deleted, and the
! Delete argument is not used by this routine.
!
! The extended types override the Deallocate binding, close or delete
! the file, and then call AbstractFileDeallocate by name (it is a
! public routine of this module).
!
! Arguments:
!   obj : CLASS(AbstractFile_) - file
!   Delete : LOGICAL(LGT) - (optional) not used here; used by the
!     extended types to delete the file
!
! Examples
!
!```fortran
! PROGRAM main
!   USE AbstractFile_Class, ONLY: AbstractFileDeallocate
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FortranFile_Class, ONLY: FortranFile_
!   USE String_Class, ONLY: String
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = "./Deallocate_test_1.txt"
!   TYPE(FortranFile_) :: obj
!   TYPE(String) :: path, fname, ext
!
!   CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL obj%Initiate(filename=filename)
!   CALL AbstractFileDeallocate(obj)
!
!   CALL obj%GetFileParts(path=path, fileName=fname, ext=ext)
!   CALL OK(path%Chars() .EQ. "", "Deallocate: path")
!   CALL OK(fname%Chars() .EQ. "", "Deallocate: fileName")
!   CALL OK(ext%Chars() .EQ. "", "Deallocate: ext")
!   CALL OK(.NOT. obj%IsOpen(), "Deallocate: IsOpen")
!   CALL OK(.NOT. obj%IsRead(), "Deallocate: IsRead")
!   CALL OK(.NOT. obj%IsWrite(), "Deallocate: IsWrite")
!
!   CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[FortranFile_]]

INTERFACE
  MODULE SUBROUTINE AbstractFileDeallocate(obj, Delete)
    CLASS(AbstractFile_), INTENT(INOUT) :: obj
    !! file
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: Delete
    !! not used here; extended types use it to delete the file
  END SUBROUTINE AbstractFileDeallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetFilePath@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-05-08
! update:
!   - 2026-10-27: renamed aFile_* to obj_*, docs
! summary: Set the path of the file
!
! The path and its length (without trailing blanks) are stored in
! obj. How the path is used to build the full name of the file
! depends on the extended type; for example [[FortranFile_]] uses
! `path // fileName // ext`, so there the path ends with the
! directory separator, for example `./output/`.
!
! The path cannot be changed while the file is open; in that case an
! error is raised and the path is not changed.
!
! Arguments:
!   obj : CLASS(AbstractFile_) - file
!   path : TYPE(String) - path of the file
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FortranFile_Class, ONLY: FortranFile_
!   USE String_Class, ONLY: String
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = "./SetFilePath_test_1.txt"
!   TYPE(FortranFile_) :: obj
!   TYPE(String) :: path, fname, ext
!
!   CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL obj%Initiate(filename=filename)
!
!   path = "./output/"
!   CALL obj%SetFilePath(path)
!   fname = "result"
!   CALL obj%SetFileName(fname)
!   ext = ".dat"
!   CALL obj%SetFileExt(ext)
!
!   CALL obj%GetFileParts(path=path, fileName=fname, ext=ext)
!   CALL OK(path%Chars() .EQ. "./output/", "SetFilePath")
!   CALL OK(fname%Chars() .EQ. "result", "SetFileName")
!   CALL OK(ext%Chars() .EQ. ".dat", "SetFileExt")
!
!   CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_SetFileName]], [[obj_SetFileExt]],
! [[obj_GetFilePath]], [[obj_GetFileParts]]

INTERFACE
  MODULE SUBROUTINE obj_SetFilePath(obj, path)
    CLASS(AbstractFile_), INTENT(INOUT) :: obj
    !! file
    TYPE(String), INTENT(IN) :: path
    !! path of the file
  END SUBROUTINE obj_SetFilePath
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetFileName@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-05-08
! update:
!   - 2026-10-27: renamed aFile_* to obj_*, docs
! summary: Set the name of the file (without extension)
!
! The file name and its length (without trailing blanks) are stored
! in obj. The name does not contain the path or the extension.
!
! The name cannot be changed while the file is open; in that case an
! error is raised and the name is not changed.
!
! Arguments:
!   obj : CLASS(AbstractFile_) - file
!   fileName : TYPE(String) - name of the file without extension
!
! See also: [[obj_SetFilePath]] (contains an example),
! [[obj_GetFileName]]

INTERFACE
  MODULE SUBROUTINE obj_SetFileName(obj, fileName)
    CLASS(AbstractFile_), INTENT(INOUT) :: obj
    !! file
    TYPE(String), INTENT(IN) :: fileName
    !! name of the file without extension
  END SUBROUTINE obj_SetFileName
END INTERFACE

!----------------------------------------------------------------------------
!                                                      SetFileExt@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-05-08
! update:
!   - 2026-10-27: renamed aFile_* to obj_*, docs
! summary: Set the extension of the file
!
! The extension and its length (without trailing blanks) are stored
! in obj. The extension includes the dot, for example `.txt`.
!
! The extension cannot be changed while the file is open; in that
! case an error is raised and the extension is not changed.
!
! Arguments:
!   obj : CLASS(AbstractFile_) - file
!   Ext : TYPE(String) - extension of the file
!
! See also: [[obj_SetFilePath]] (contains an example),
! [[obj_GetFileExt]]

INTERFACE
  MODULE SUBROUTINE obj_SetFileExt(obj, Ext)
    CLASS(AbstractFile_), INTENT(INOUT) :: obj
    !! file
    TYPE(String), INTENT(IN) :: Ext
    !! extension of the file, including the dot
  END SUBROUTINE obj_SetFileExt
END INTERFACE

!----------------------------------------------------------------------------
!                                                      SetEOFStat@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-05-08
! update:
!   - 2026-10-27: renamed aFile_* to obj_*, docs
! summary: Set the end of file status of the file
!
! The end of file status can be changed only if the file is open. If
! the file is not open, a debug message is raised and the status is
! not changed.
!
! Arguments:
!   obj : CLASS(AbstractFile_) - file
!   stat : LOGICAL(LGT) - end of file status
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FortranFile_Class, ONLY: FortranFile_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = "./SetEOFStat_test_1.txt"
!   TYPE(FortranFile_) :: obj
!
!   CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL obj%Initiate(filename=filename)
!   CALL OK(.NOT. obj%IsEOF(), "IsEOF: default")
!
!   CALL obj%SetEOFStat(math%yes)
!   CALL OK(.NOT. obj%IsEOF(), "SetEOFStat: ignored on closed file")
!
!   CALL obj%OPEN()
!   CALL obj%SetEOFStat(math%yes)
!   CALL OK(obj%IsEOF(), "SetEOFStat: true")
!   CALL obj%SetEOFStat(math%no)
!   CALL OK(.NOT. obj%IsEOF(), "SetEOFStat: false")
!
!   CALL obj%Delete()
!   CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_IsEOF]]

INTERFACE
  MODULE SUBROUTINE obj_SetEOFStat(obj, STAT)
    CLASS(AbstractFile_), INTENT(INOUT) :: obj
    !! file
    LOGICAL(LGT), INTENT(IN) :: STAT
    !! end of file status
  END SUBROUTINE obj_SetEOFStat
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetOpenStat@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-05-08
! update:
!   - 2026-10-27: renamed aFile_* to obj_*, docs
! summary: Set the open status of the file
!
! Only the flag returned by IsOpen is changed; the file is not opened
! or closed. This routine is called by the Open and Close methods of
! the extended types.
!
! Arguments:
!   obj : CLASS(AbstractFile_) - file
!   stat : LOGICAL(LGT) - open status
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FortranFile_Class, ONLY: FortranFile_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = "./SetOpenStat_test_1.txt"
!   TYPE(FortranFile_) :: obj
!
!   CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL obj%Initiate(filename=filename)
!
!   CALL obj%SetOpenStat(math%yes)
!   CALL OK(obj%IsOpen(), "SetOpenStat: true")
!
!   CALL obj%SetOpenStat(math%no)
!   CALL OK(.NOT. obj%IsOpen(), "SetOpenStat: false")
!
!   CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_IsOpen]]

INTERFACE
  MODULE SUBROUTINE obj_SetOpenStat(obj, STAT)
    CLASS(AbstractFile_), INTENT(INOUT) :: obj
    !! file
    LOGICAL(LGT), INTENT(IN) :: STAT
    !! open status
  END SUBROUTINE obj_SetOpenStat
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetReadStat@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-05-08
! update:
!   - 2026-10-27: renamed aFile_* to obj_*, docs
! summary: Set the read access status of the file
!
! The read status can be changed only if the file is not open. If the
! file is open, a debug message is raised and the status is not
! changed.
!
! Arguments:
!   obj : CLASS(AbstractFile_) - file
!   stat : LOGICAL(LGT) - read access status
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FortranFile_Class, ONLY: FortranFile_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = "./SetReadStat_test_1.txt"
!   TYPE(FortranFile_) :: obj
!
!   CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL obj%Initiate(filename=filename, action="WRITE")
!   CALL OK(.NOT. obj%IsRead(), "IsRead: WRITE")
!
!   CALL obj%SetReadStat(math%yes)
!   CALL OK(obj%IsRead(), "SetReadStat: true")
!
!   CALL obj%SetWriteStat(math%no)
!   CALL OK(.NOT. obj%IsWrite(), "SetWriteStat: false")
!
!   CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_SetWriteStat]], [[obj_IsRead]]

INTERFACE
  MODULE SUBROUTINE obj_SetReadStat(obj, STAT)
    CLASS(AbstractFile_), INTENT(INOUT) :: obj
    !! file
    LOGICAL(LGT), INTENT(IN) :: STAT
    !! read access status
  END SUBROUTINE obj_SetReadStat
END INTERFACE

!----------------------------------------------------------------------------
!                                                    SetWriteStat@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-05-08
! update:
!   - 2026-10-27: renamed aFile_* to obj_*, docs
! summary: Set the write access status of the file
!
! The write status can be changed only if the file is not open. If
! the file is open, a debug message is raised and the status is not
! changed.
!
! Arguments:
!   obj : CLASS(AbstractFile_) - file
!   stat : LOGICAL(LGT) - write access status
!
! See also: [[obj_SetReadStat]] (contains an example), [[obj_IsWrite]]

INTERFACE
  MODULE SUBROUTINE obj_SetWriteStat(obj, STAT)
    CLASS(AbstractFile_), INTENT(INOUT) :: obj
    !! file
    LOGICAL(LGT), INTENT(IN) :: STAT
    !! write access status
  END SUBROUTINE obj_SetWriteStat
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetFilePath@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-05-08
! update:
!   - 2026-10-27: renamed aFile_* to obj_*, docs
! summary: Returns the path of the file
!
! Arguments:
!   obj : CLASS(AbstractFile_) - file
!
! Returns:
!   path : TYPE(String) - path of the file
!
! See also: [[obj_GetFileParts]] (contains an example),
! [[obj_SetFilePath]]

INTERFACE
  MODULE FUNCTION obj_GetFilePath(obj) RESULT(path)
    CLASS(AbstractFile_), INTENT(IN) :: obj
    !! file
    TYPE(String) :: path
    !! path of the file
  END FUNCTION obj_GetFilePath
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetFileName@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-05-08
! update:
!   - 2026-10-27: renamed aFile_* to obj_*, docs
! summary: Returns the name of the file (without extension)
!
! Arguments:
!   obj : CLASS(AbstractFile_) - file
!
! Returns:
!   fileName : TYPE(String) - name of the file without path and
!     extension
!
! See also: [[obj_GetFileParts]] (contains an example),
! [[obj_SetFileName]]

INTERFACE
  MODULE FUNCTION obj_GetFileName(obj) RESULT(fileName)
    CLASS(AbstractFile_), INTENT(IN) :: obj
    !! file
    TYPE(String) :: fileName
    !! name of the file without path and extension
  END FUNCTION obj_GetFileName
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetFileExt@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-05-08
! update:
!   - 2026-10-27: renamed aFile_* to obj_*, docs
! summary: Returns the extension of the file
!
! Arguments:
!   obj : CLASS(AbstractFile_) - file
!
! Returns:
!   Ext : TYPE(String) - extension of the file, including the dot
!
! See also: [[obj_GetFileParts]] (contains an example),
! [[obj_SetFileExt]]

INTERFACE
  MODULE FUNCTION obj_GetFileExt(obj) RESULT(Ext)
    CLASS(AbstractFile_), INTENT(IN) :: obj
    !! file
    TYPE(String) :: Ext
    !! extension of the file, including the dot
  END FUNCTION obj_GetFileExt
END INTERFACE

!----------------------------------------------------------------------------
!                                                    GetFileParts@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-05-08
! update:
!   - 2026-10-27: renamed aFile_* to obj_*, docs
! summary: Returns the path, name, and extension of the file
!
! These are the three parts stored by the Set methods. How the full
! name of the file is built from them depends on the extended type.
!
! Arguments:
!   obj : CLASS(AbstractFile_) - file
!   path : TYPE(String) - path of the file
!   fileName : TYPE(String) - name of the file without extension
!   ext : TYPE(String) - extension of the file, including the dot
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FortranFile_Class, ONLY: FortranFile_
!   USE String_Class, ONLY: String
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = "./GetFileParts_test_1.txt"
!   TYPE(FortranFile_) :: obj
!   TYPE(String) :: path, fname, ext
!
!   CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL obj%Initiate(filename=filename)
!
!   path = obj%GetFilePath()
!   CALL OK(path%Chars() .EQ. "./", "GetFilePath")
!
!   fname = obj%GetFileName()
!   CALL OK(fname%Chars() .EQ. "GetFileParts_test_1", "GetFileName")
!
!   ext = obj%GetFileExt()
!   CALL OK(ext%Chars() .EQ. ".txt", "GetFileExt")
!
!   CALL obj%GetFileParts(path=path, fileName=fname, ext=ext)
!   CALL OK(path%Chars() .EQ. "./", "GetFileParts: path")
!   CALL OK(fname%Chars() .EQ. "GetFileParts_test_1", &
!           "GetFileParts: fileName")
!   CALL OK(ext%Chars() .EQ. ".txt", "GetFileParts: ext")
!
!   CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetFilePath]], [[obj_GetFileName]],
! [[obj_GetFileExt]], [[obj_SetFilePath]]

INTERFACE
  MODULE PURE SUBROUTINE obj_GetFileParts(obj, path, fileName, ext)
    CLASS(AbstractFile_), INTENT(IN) :: obj
    !! file
    TYPE(String), INTENT(OUT) :: path
    !! path of the file
    TYPE(String), INTENT(OUT) :: fileName
    !! name of the file without extension
    TYPE(String), INTENT(OUT) :: ext
    !! extension of the file, including the dot
  END SUBROUTINE obj_GetFileParts
END INTERFACE

!----------------------------------------------------------------------------
!                                                     IsOpen@EnquireMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-05-08
! update:
!   - 2026-10-27: renamed aFile_* to obj_*, docs
! summary: Returns true if the file is open
!
! The open status is set by the Open, Close, and Delete methods of the
! extended types (through [[obj_SetOpenStat]]).
!
! Arguments:
!   obj : CLASS(AbstractFile_) - file
!
! Returns:
!   ans : LOGICAL(LGT) - true if the file is open
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FortranFile_Class, ONLY: FortranFile_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = "./IsOpen_test_1.txt"
!   TYPE(FortranFile_) :: obj
!
!   CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL obj%Initiate(filename=filename)
!   CALL OK(.NOT. obj%IsOpen(), "IsOpen: after Initiate")
!
!   CALL obj%OPEN()
!   CALL OK(obj%IsOpen(), "IsOpen: after Open")
!
!   CALL obj%CLOSE()
!   CALL OK(.NOT. obj%IsOpen(), "IsOpen: after Close")
!
!   CALL obj%OPEN()
!   CALL obj%Delete()
!   CALL OK(.NOT. obj%IsOpen(), "IsOpen: after Delete")
!
!   CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_SetOpenStat]], [[obj_Open]], [[obj_Close]],
! [[obj_Delete]]

INTERFACE
  MODULE FUNCTION obj_IsOpen(obj) RESULT(ans)
    CLASS(AbstractFile_), INTENT(IN) :: obj
    !! file
    LOGICAL(LGT) :: ans
    !! true if the file is open
  END FUNCTION obj_IsOpen
END INTERFACE

!----------------------------------------------------------------------------
!                                                      IsEOF@EnquireMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-05-08
! update:
!   - 2026-10-27: renamed aFile_* to obj_*, docs
! summary: Returns true if the end of the file has been reached
!
! Arguments:
!   obj : CLASS(AbstractFile_) - file
!
! Returns:
!   ans : LOGICAL(LGT) - true if the end of file has been reached
!
! See also: [[obj_SetEOFStat]] (contains an example)

INTERFACE
  MODULE FUNCTION obj_IsEOF(obj) RESULT(ans)
    CLASS(AbstractFile_), INTENT(IN) :: obj
    !! file
    LOGICAL(LGT) :: ans
    !! true if the end of file has been reached
  END FUNCTION obj_IsEOF
END INTERFACE

!----------------------------------------------------------------------------
!                                                     IsRead@EnquireMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-05-08
! update:
!   - 2026-10-27: renamed aFile_* to obj_*, docs
! summary: Returns true if the file has read access
!
! Arguments:
!   obj : CLASS(AbstractFile_) - file
!
! Returns:
!   ans : LOGICAL(LGT) - true if the file has read access
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FortranFile_Class, ONLY: FortranFile_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = "./IsRead_test_1.txt"
!   TYPE(FortranFile_) :: obj
!
!   CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL obj%Initiate(filename=filename, action="READWRITE")
!   CALL OK(obj%IsRead(), "IsRead: READWRITE")
!   CALL OK(obj%IsWrite(), "IsWrite: READWRITE")
!   CALL obj%DEALLOCATE()
!
!   CALL obj%Initiate(filename=filename, action="WRITE")
!   CALL OK(.NOT. obj%IsRead(), "IsRead: WRITE")
!   CALL OK(obj%IsWrite(), "IsWrite: WRITE")
!   CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_IsWrite]], [[obj_SetReadStat]]

INTERFACE
  MODULE FUNCTION obj_IsRead(obj) RESULT(ans)
    CLASS(AbstractFile_), INTENT(IN) :: obj
    !! file
    LOGICAL(LGT) :: ans
    !! true if the file has read access
  END FUNCTION obj_IsRead
END INTERFACE

!----------------------------------------------------------------------------
!                                                    IsWrite@EnquireMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-05-08
! update:
!   - 2026-10-27: renamed aFile_* to obj_*, docs
! summary: Returns true if the file has write access
!
! Arguments:
!   obj : CLASS(AbstractFile_) - file
!
! Returns:
!   ans : LOGICAL(LGT) - true if the file has write access
!
! See also: [[obj_IsRead]] (contains an example), [[obj_SetWriteStat]]

INTERFACE
  MODULE FUNCTION obj_IsWrite(obj) RESULT(ans)
    CLASS(AbstractFile_), INTENT(IN) :: obj
    !! file
    LOGICAL(LGT) :: ans
    !! true if the file has write access
  END FUNCTION obj_IsWrite
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Open@Deferred
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-05-08
! update:
!   - 2026-10-27: renamed aFile_* to obj_*, docs
! summary: Open the file (deferred)
!
! Abstract interface of the deferred binding OPEN. The extended types
! open the file and set the open status with [[obj_SetOpenStat]].
!
! Arguments:
!   obj : CLASS(AbstractFile_) - file
!
! See also: [[obj_IsOpen]] (contains an example), [[obj_Close]],
! [[obj_Delete]]

ABSTRACT INTERFACE
  SUBROUTINE obj_Open(obj)
    IMPORT :: AbstractFile_
    CLASS(AbstractFile_), INTENT(INOUT) :: obj
    !! file
  END SUBROUTINE obj_Open
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Close@Deferred
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-05-08
! update:
!   - 2026-10-27: renamed aFile_* to obj_*, docs
! summary: Close the file (deferred)
!
! Abstract interface of the deferred binding CLOSE. The extended types
! close the file and set the open status to false with
! [[obj_SetOpenStat]].
!
! Arguments:
!   obj : CLASS(AbstractFile_) - file
!
! See also: [[obj_IsOpen]] (contains an example), [[obj_Open]],
! [[obj_Delete]]

ABSTRACT INTERFACE
  SUBROUTINE obj_Close(obj)
    IMPORT :: AbstractFile_
    CLASS(AbstractFile_), INTENT(INOUT) :: obj
    !! file
  END SUBROUTINE obj_Close
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Delete@Deferred
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-05-08
! update:
!   - 2026-10-27: renamed aFile_* to obj_*, docs
! summary: Delete the file (deferred)
!
! Abstract interface of the deferred binding Delete. The extended
! types delete the file from the disk; the exact rules (for example
! whether the file has to be open) depend on the extended type.
!
! Arguments:
!   obj : CLASS(AbstractFile_) - file
!
! See also: [[obj_IsOpen]] (contains an example), [[obj_Open]],
! [[obj_Close]]

ABSTRACT INTERFACE
  SUBROUTINE obj_Delete(obj)
    IMPORT :: AbstractFile_
    CLASS(AbstractFile_), INTENT(INOUT) :: obj
    !! file
  END SUBROUTINE obj_Delete
END INTERFACE

END MODULE AbstractFile_Class
