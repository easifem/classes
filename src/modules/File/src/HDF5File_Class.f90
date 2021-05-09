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

MODULE HDF5File_Class
USE GlobalData, ONLY: I4B, DFP, LGT
USE BaseType, ONLY : String
USE HDF5
USE ExceptionHandler_Class
USE AbstractFile_Class
IMPLICIT NONE
PRIVATE

PUBLIC :: HDF5Open, HDF5Close, HDF5Quiet
CHARACTER( LEN=* ), PARAMETER :: modName='HDF5File_Class'
INTEGER( I4B ), PARAMETER :: MAXSTRLEN=1024
INTEGER( I4B ), SAVE :: ierr=0
INTEGER( I4B ), SAVE :: nhdf5fileinuse=0
!! Variable for keeping track of the number of hdf5 files initialized
!! This variable will be used in logic to call the h5close_f(error)
!! which closes the interface.
LOGICAL( LGT ), SAVE :: libh5Open=.FALSE.
!! Variable to make sure that the hdf5 interface was opened, and thus
!! can then be closed.

!----------------------------------------------------------------------------
!                                                                  HDF5File_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	8 May 2021
! summary: 	HDF5 File
!
! Reading from HDF5 binary files. As implemented, there are three modes for
! accessing a file can be opened as:
! - 'read'
! - 'write'
! - 'new'.
! Read mode opens an existing HDF5 file and allows the client code to interrogate that file and extract data without permissions to alter the file.
! Write mode opens an existing HDF5 file and allows the client code to alter its contents.
! New mode overwrites any file by the same name and starts from scratch.

TYPE, EXTENDS( AbstractFile_ ) :: HDF5File_
  PRIVATE
  LOGICAL( LGT ) :: isInit = .FALSE.
    !! Initialization status
  LOGICAL( LGT ) :: hasCompression=.FALSE.
    !! Whether or not the file uses compression for writing
  INTEGER( I4B ),PRIVATE :: zlibOpt=-1
    !! Option for gzip compression -1 (no filter) or [0-9].
    !! these options are defined by HDF5.
  LOGICAL( LGT ),PRIVATE :: newstat=.FALSE.
    !! The 'new' status of a file
  TYPE( String ) :: fullname
    !! full name of the file
  INTEGER( I4B ),PRIVATE :: unitno=-1
    !! unit number of the file
  !TYPE(MPI_EnvType),POINTER  :: pe => NULL()
  LOGICAL( LGT ),PRIVATE :: overwriteStat=.FALSE.
    !! When .TRUE., file data can be overwritten
  INTEGER(HID_T) :: file_id=0
    !! File id assigned by the HDF5 library when file is opened
  CONTAINS
    PRIVATE
    PROCEDURE, PUBLIC, PASS( Obj ) :: open => hdf5_open
    PROCEDURE, PUBLIC, PASS( Obj ) :: close => hdf5_close
    PROCEDURE, PUBLIC, PASS( Obj ) :: delete => hdf5_delete
    PROCEDURE, PUBLIC, PASS( Obj ) :: initiate => hdf5_initiate
    PROCEDURE, PUBLIC, PASS( Obj ) :: clear => hdf5_clear
    PROCEDURE, PUBLIC, PASS( Obj ) :: setOverwriteStat => hdf5_setOverwriteStat
    PROCEDURE, PUBLIC, PASS( Obj ) :: getUnitNo => hdf5_getUnitNo
    PROCEDURE, PUBLIC, PASS( Obj ) :: isNew => hdf5_isNew
    PROCEDURE, PUBLIC, PASS( Obj ) :: setNewStat => hdf5_setNewStat
    PROCEDURE, PUBLIC, PASS( Obj ) :: ls => hdf5_ls
    PROCEDURE, PUBLIC, PASS( Obj ) :: mkdir => hdf5_mkdir
    PROCEDURE, PUBLIC, PASS( Obj ) :: mkalldir => hdf5_mkalldir
    PROCEDURE, PUBLIC, PASS( Obj ) :: ngrp => hdf5_ngrp
    PROCEDURE, PUBLIC, PASS( Obj ) :: isGroup => hdf5_isGroup
    PROCEDURE, PUBLIC, PASS( Obj ) :: pathExists => hdf5_pathExists
    PROCEDURE, PUBLIC, PASS( Obj ) :: createHardLink => hdf5_createHardLink
    PROCEDURE, PUBLIC, PASS( Obj ) :: getChunkSize => hdf5_getChunkSize
    PROCEDURE, PUBLIC, PASS( Obj ) :: isCompressed => hdf5_isCompressed
  END TYPE HDF5File_

PUBLIC :: HDF5File_

TYPE :: HDF5FilePointer_
  CLASS( HDF5File_ ), POINTER :: ptr => NULL()
END TYPE HDF5FilePointer_

PUBLIC :: HDF5FilePointer_

!----------------------------------------------------------------------------
!                                                                 Open
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	8 May 2021
! summary: Open HDF5 file
!
!# Introduction
! This routine implements the abstract open routine in the [[AbstractFile_]].
! It uses the HDF5 library interface that was initialized to open the file.

INTERFACE
MODULE SUBROUTINE hdf5_open( obj )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
END SUBROUTINE hdf5_open
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Close
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	8 May 2021
! summary: 	Close HDF5File
!
!# Introduction
!
! This routine implements the abstract @c fclose routine in the base file type.

INTERFACE
MODULE SUBROUTINE hdf5_close( obj )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
END SUBROUTINE hdf5_close
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Delete
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	8 May 2021
! summary: 	Delete HDF5 file
!
!# Introduction
! This routine implements the abstract @c fdelete routine in the base file
! type. Mimicking FileType_Fortran logical structure.  We do not clear the file after it is deleted.

INTERFACE
MODULE SUBROUTINE hdf5_delete( obj )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
END SUBROUTINE hdf5_delete
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	8 May 2021
! summary: 	Initializes an HDF5 file object.
!
!### Introduction
! This routine initializes an HDF5 file object by setting the objects
! attributes, initializing the HDF5 library interface and calling the @c open
! routine.
!
! Use setOverwriteStat to change the status to 'OVERWRITE'.
!
!@note
! Note that when overwriting datasets, new datasets must be the same type and shape as the existing one being overwritten; otherwise an exception will be thrown.
!@endnote

INTERFACE
MODULE SUBROUTINE hdf5_initiate(Obj, filename, mode, zlibOpt)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: Obj
    !! the object to be initialized
  CHARACTER( LEN = * ), INTENT( IN ) :: filename
    !! filename the relative path to the file on the filesystem
  CHARACTER( LEN = * ), INTENT( IN ) :: mode
    !! Access mode, which can be 'READ', 'WRITE', 'OVERWRITE', or 'NEW'
    !! 'READ', 'WRITE', and 'OVERWRITE' all require the file to exist.
    !! 'WRITE' gives write access but will fail if the user tries to overwrite
    !! 'OVERWRITE' will allow the user to overwrite existing data in the file.
    !! 'NEW' will create the file, overwriting any existing files with the
    !!  same name.
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: zlibOpt
    !! numeric option for GZIP compression [0-9] uses compression
END SUBROUTINE hdf5_initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 clear
!----------------------------------------------------------------------------

!> @brief Destructs an HDF5 file object.
!> @param thisHDF5File the HDF5 object to be destroyed
!> @param ldel logical on whether or not to delete or close the file.
!>
!> This routine releases resources used for interacting with the HSF5 file. It
!> closes or deletes the file and the HDF5 library interface. The structure
!> was taken from the Fortran file.
!>
INTERFACE
MODULE SUBROUTINE hdf5_clear(obj, Delete)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  LOGICAL( LGT ), INTENT( IN ), OPTIONAL :: Delete
END SUBROUTINE hdf5_clear
END INTERFACE

!----------------------------------------------------------------------------
!                                                          setOverwriteStat
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE hdf5_setOverwriteStat( obj, bool )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  LOGICAL( LGT ), INTENT( IN ) :: bool
END SUBROUTINE hdf5_setOverwriteStat
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 getUnitNo
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION hdf5_getUnitNo( obj ) RESULT( Ans )
  CLASS( HDF5File_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: ans
END FUNCTION hdf5_getUnitNo
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 isNew
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION hdf5_isNew( obj ) RESULT( Ans )
  CLASS( HDF5File_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ) :: ans
END FUNCTION hdf5_isNew
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 setNewStat
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE hdf5_setNewStat( obj, bool )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  LOGICAL( LGT ), INTENT( IN ) :: bool
END SUBROUTINE hdf5_setNewStat
END INTERFACE

!----------------------------------------------------------------------------
!                                                                       ls
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	8 may 2021
! summary: List the contents of a group
!
!### Introduction
!
! This routine is useful for discovering the contents of an HDF5 file. A path
! to a group in the file is provided in the subroutine call and the names of
! its constituents is stored in the objs list. If objs is passed in,
! it will be deallocated and reallocated to the proper size to store the
! names of all of the objects in the group.

INTERFACE
MODULE SUBROUTINE hdf5_ls( obj, path, objs )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: path
  TYPE( String ), ALLOCATABLE, INTENT( INOUT ) :: objs(:)
END SUBROUTINE hdf5_ls
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     mkdir
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	8 May 2021
! summary: Creates a new group in the HDF file.
!
!### Introduction
!
! This routine is used to create a new group in an HDF5 file. It can only be
! called if the file has write access

INTERFACE
MODULE RECURSIVE SUBROUTINE hdf5_mkdir( obj, path )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: path
END SUBROUTINE hdf5_mkdir
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 mkalldir
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	8 may 2021
! summary:
!
!### Introduction
! This routine is used to create a new group in an HDF5 file. It can only be
! called if the file has write access.

INTERFACE
MODULE SUBROUTINE hdf5_mkalldir( obj, path )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: path
END SUBROUTINE hdf5_mkalldir
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 ngrp
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	8 may 2021
! summary: 	This function returns how many objects are in the group path

INTERFACE
MODULE FUNCTION hdf5_ngrp( obj, path ) RESULT( Ans )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: path
  INTEGER( I4B ) :: Ans
END FUNCTION hdf5_ngrp
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 isGroup
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	8 may 2021
! summary: 	This function returns a logical corresponding to whether the group specified by path is a group or not.

INTERFACE
MODULE FUNCTION hdf5_isGroup( obj, path ) RESULT( Ans )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: path
  LOGICAL( LGT ) :: Ans
END FUNCTION hdf5_isGroup
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 pathExists
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	9 May 2021
! summary: 	Checks if Path exits
INTERFACE
MODULE FUNCTION hdf5_pathExists( obj, path ) RESULT( Ans )
  CLASS( HDF5File_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: path
  LOGICAL( LGT ) :: Ans
END FUNCTION hdf5_pathExists
END INTERFACE

!----------------------------------------------------------------------------
!                                                             CreateHardLink
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	9 May 2021
! summary: 	Create hard link

INTERFACE
MODULE SUBROUTINE hdf5_createHardLink( obj, source_path, link_path )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: source_path
  CHARACTER( LEN = * ), INTENT( IN ) :: link_path
END SUBROUTINE hdf5_createHardLink
END INTERFACE

!----------------------------------------------------------------------------
!                                                             getChunkSize
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	9 May 2021
! summary: 	Returns the chunk size of a dataset
!
!### Introduction
! If a dataset does not have chunking or does not exist, then cdims is
! returned unallocated.
!

INTERFACE
MODULE SUBROUTINE hdf5_getChunkSize( obj, path, cdims )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: path
  INTEGER( I4B ), ALLOCATABLE, INTENT( OUT ) :: cdims( : )
END SUBROUTINE hdf5_getChunkSize
END INTERFACE

!----------------------------------------------------------------------------
!                                                             isCompressed
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION hdf5_isCompressed( obj, path ) RESULT( Ans )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: path
  LOGICAL( LGT ) :: Ans
END FUNCTION hdf5_isCompressed
END INTERFACE

!----------------------------------------------------------------------------
!                                                                  HDF5Open
!----------------------------------------------------------------------------

CONTAINS

SUBROUTINE HDF5Open
  INTEGER( I4B ) :: herr
  herr=-1
  IF(.NOT.libh5Open) CALL H5open_f(herr)
  IF(herr == 0) libh5Open=.TRUE.
END SUBROUTINE HDF5Open

!----------------------------------------------------------------------------
!                                                                 HDF5Close
!----------------------------------------------------------------------------

SUBROUTINE HDF5Close
  INTEGER( I4B ) :: herr
  herr=-1
  IF(libh5Open) CALL H5close_f(herr)
  IF(herr == 0) libh5Open=.FALSE.
END SUBROUTINE HDF5Close

!----------------------------------------------------------------------------
!                                                                 HDF5Quiet
!----------------------------------------------------------------------------


!> authors: Vikas Sharma, Ph. D.
! date: 	8 May 2021
! summary: Enable/disable HDF5 exception writing

SUBROUTINE HDF5Quiet(quiet)
  LOGICAL, INTENT( IN ) :: quiet
  IF(quiet) THEN
    CALL h5eset_auto_f(0, ierr)
  ELSE
    CALL h5eset_auto_f(1, ierr)
  ENDIF
END SUBROUTINE HDF5Quiet

END MODULE HDF5File_Class