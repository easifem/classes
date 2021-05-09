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
! summary: 	module for I/O defines the base file type object.
!
! The developement of this module is inspired from the
! `FileType_Base.f90` of Futility package. The original source is located at https://github.com/CASL/Futility/blob/master/src/FileType_Base.f90.
!
! The original code has been modified as per the code-standard of easifem library.
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
USE GlobalData, ONLY: DFP, I4B, LGT
USE BaseType, ONLY: String
USE ExceptionHandler_Class
IMPLICIT NONE
PRIVATE

!List of Public Members
CHARACTER(LEN=*),PARAMETER :: modName='ABSTRACTFILE_CLASS'
INTEGER(I4B), PARAMETER :: maxStrLen=256

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	2 May 2021
! summary: This is an abstract type which means it has no basic implementation

TYPE,ABSTRACT :: AbstractFile_
  PRIVATE
  INTEGER(I4B) :: pathlen=0
    !! The length of the path string for this file
  INTEGER(I4B) :: fnamelen=0
    !! The length of the name string for this file
  INTEGER(I4B) :: extlen=0
    !! The length of the file name extension string for this file
  TYPE(String) :: path
    !! The path string to the file
  TYPE(String) :: fileName
    !! The name of the file (without the file extension)
  TYPE(String) :: ext
    !! The extension of the file name
  LOGICAL(LGT) :: openstat=.FALSE.
    !! Whether or not the file is open
  LOGICAL(LGT) :: EOFstat=.FALSE.
    !! Whether or not the end of file has been reached
  LOGICAL(LGT) :: readstat=.FALSE.
    !! Whether or not the file is open for reading
  LOGICAL(LGT) :: writestat=.FALSE.
    !! Whether or not the file is open for writing
  TYPE(ExceptionHandler_), PUBLIC :: e
    !! The exception handler for the object
    !! List of type bound procedures (methods) for the Base File Type object
  CONTAINS
    PRIVATE
    PROCEDURE(empty_abstract_iface), PUBLIC, DEFERRED, PASS( obj ) :: open
    PROCEDURE(empty_abstract_iface), PUBLIC, DEFERRED, PASS( obj ) :: close
    PROCEDURE(empty_abstract_iface), PUBLIC, DEFERRED, PASS( obj ) :: delete
    PROCEDURE, PUBLIC, PASS( obj ) :: setFilePath => setFilePath_file
    PROCEDURE, PUBLIC, PASS( obj ) :: setFileName => setFileName_file
    PROCEDURE, PUBLIC, PASS( obj ) :: setFileExt => setFileExt_file
    PROCEDURE, PUBLIC, PASS( obj ) :: getFilePath => getFilePath_file
    PROCEDURE, PUBLIC, PASS( obj ) :: getFileName => getFileName_file
    PROCEDURE, PUBLIC, PASS( obj ) :: getFileExt => getFileExt_file
    PROCEDURE, PUBLIC, PASS( obj ) :: getFileParts => getFileParts_file
    PROCEDURE, PUBLIC, PASS( obj ) :: setEOFstat => setEOFStat_file
    PROCEDURE, PUBLIC, PASS( obj ) :: setOpenStat => setOpenStat_file
    PROCEDURE, PUBLIC, PASS( obj ) :: setReadStat => setReadStat_file
    PROCEDURE, PUBLIC, PASS( obj ) :: setWriteStat => setWriteStat_file
    PROCEDURE, PUBLIC, PASS( obj ) :: isOpen => isOpen_file
    PROCEDURE, PUBLIC, PASS( obj ) :: isEOF => isEOF_file
    PROCEDURE, PUBLIC, PASS( obj ) :: isRead => isRead_file
    PROCEDURE, PUBLIC, PASS( obj ) :: isWrite => isWrite_file
    PROCEDURE, PUBLIC, PASS( Obj ) :: DeallocateBaseData => deallocateData_file
ENDTYPE AbstractFile_

PUBLIC :: AbstractFile_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE setFilePath_file( obj, path )
  CLASS( AbstractFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: path
END SUBROUTINE setFilePath_file
END INTERFACE

!----------------------------------------------------------------------------
!                                                                setFileName
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE setFileName_file( obj, fileName )
  CLASS( AbstractFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: fileName
END SUBROUTINE setFileName_file
END INTERFACE

!----------------------------------------------------------------------------
!                                                                setFileExt
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE setFileExt_file( obj, Ext )
  CLASS( AbstractFile_ ), INTENT( INOUT ) :: obj
  TYPE( String ), INTENT( IN ) :: Ext
END SUBROUTINE setFileExt_file
END INTERFACE

!----------------------------------------------------------------------------
!                                                               getFileParts
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE getFileParts_file( obj, path, fileName, ext )
  CLASS( AbstractFile_ ), INTENT( IN ) :: obj
  TYPE( String ), INTENT( OUT ) :: path
  TYPE( String ), INTENT( OUT ) :: fileName
  TYPE( String ), INTENT( OUT ) :: ext
END SUBROUTINE getFileParts_file
END INTERFACE

!----------------------------------------------------------------------------
!                                                                getFilePath
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION getFilePath_file( obj ) RESULT( path )
  CLASS( AbstractFile_ ), INTENT( IN ) :: obj
  TYPE( String ) :: path
END FUNCTION getFilePath_file
END INTERFACE

!----------------------------------------------------------------------------
!                                                                getFileName
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION getFileName_file( obj ) RESULT( fileName )
  CLASS( AbstractFile_ ), INTENT( IN ) :: obj
  TYPE( String ) :: fileName
END FUNCTION getFileName_file
END INTERFACE

!----------------------------------------------------------------------------
!                                                                getFileExt
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION getFileExt_file( obj ) RESULT( Ext )
  CLASS( AbstractFile_ ), INTENT( IN ) :: obj
  TYPE( String ) :: Ext
END FUNCTION getFileExt_file
END INTERFACE

!----------------------------------------------------------------------------
!                                                                    isOpen
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION isOpen_file( obj ) RESULT( ans )
  CLASS( AbstractFile_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ) :: ans
END FUNCTION isOpen_file
END INTERFACE

!----------------------------------------------------------------------------
!                                                                    isEOF
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION isEOF_file( obj ) RESULT( ans )
  CLASS( AbstractFile_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ) :: ans
END FUNCTION isEOF_file
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 isWrite
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION isWrite_file( obj ) RESULT( ans )
  CLASS( AbstractFile_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ) :: ans
END FUNCTION isWrite_file
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 isRead
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION isRead_file( obj ) RESULT( ans )
  CLASS( AbstractFile_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ) :: ans
END FUNCTION isRead_file
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 setEOFstat
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE setEOFstat_file( obj, stat )
  CLASS( AbstractFile_ ), INTENT( INOUT ) :: obj
  LOGICAL( LGT ), INTENT( IN ) :: stat
END SUBROUTINE setEOFstat_file
END INTERFACE

!----------------------------------------------------------------------------
!                                                               setOpenStat
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE setOpenStat_file( obj, stat )
  CLASS( AbstractFile_ ), INTENT( INOUT ) :: obj
  LOGICAL( LGT ), INTENT( IN ) :: stat
END SUBROUTINE setOpenStat_file
END INTERFACE

!----------------------------------------------------------------------------
!                                                               setReadStat
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE setReadStat_file( obj, stat )
  CLASS( AbstractFile_ ), INTENT( INOUT ) :: obj
  LOGICAL( LGT ), INTENT( IN ) :: stat
END SUBROUTINE setReadStat_file
END INTERFACE

!----------------------------------------------------------------------------
!                                                               setWriteStat
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE setWriteStat_file( obj, stat )
  CLASS( AbstractFile_ ), INTENT( INOUT ) :: obj
  LOGICAL( LGT ), INTENT( IN ) :: stat
END SUBROUTINE setWriteStat_file
END INTERFACE

!----------------------------------------------------------------------------
!                                                           DealalocateData
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE deallocateData_file( obj )
  CLASS( AbstractFile_ ), INTENT( INOUT ) :: obj
END SUBROUTINE deallocateData_file
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

ABSTRACT INTERFACE
  SUBROUTINE empty_abstract_iface(obj)
    IMPORT :: AbstractFile_
    CLASS( AbstractFile_ ), INTENT( INOUT ) :: obj
  END SUBROUTINE empty_abstract_iface
END INTERFACE

ENDMODULE AbstractFile_Class

