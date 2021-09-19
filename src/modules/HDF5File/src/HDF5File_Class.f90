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
USE GlobalData
USE BaseType
USE HDF5
USE H5LT
! USE H5Fortran
USE ExceptionHandler_Class
USE AbstractFile_Class
IMPLICIT NONE
PRIVATE
! PUBLIC :: HDF5Open, HDF5Close, HDF5Quiet
TYPE(ExceptionHandler_) :: e
CHARACTER( LEN=* ), PARAMETER :: modName='HDF5FILE_CLASS'
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
    PROCEDURE, PUBLIC, PASS( obj ) :: addSurrogate => hdf5_addSurrogate
    PROCEDURE, PUBLIC, PASS( Obj ) :: open => hdf5_open
    PROCEDURE, PUBLIC, PASS( Obj ) :: close => hdf5_close
    PROCEDURE, PUBLIC, PASS( Obj ) :: delete => hdf5_delete
    PROCEDURE, PUBLIC, PASS( Obj ) :: initiate => hdf5_initiate
    PROCEDURE, PUBLIC, PASS( Obj ) :: DeallocateData => hdf5_clear
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
    PROCEDURE, PASS( Obj ) :: &
      & hdf5_write_d0, hdf5_write_d1, hdf5_write_d2, &
      & hdf5_write_d3, hdf5_write_d4, hdf5_write_d5, &
      & hdf5_write_d6, hdf5_write_d7, &
      & hdf5_write_s0, hdf5_write_s1, hdf5_write_s2, &
      & hdf5_write_s3, hdf5_write_s4, hdf5_write_s5, &
      & hdf5_write_s6, hdf5_write_s7, &
      & hdf5_write_b0, hdf5_write_b1, hdf5_write_b2, &
      & hdf5_write_b3, &
      & hdf5_write_n0, hdf5_write_n1, hdf5_write_n2, &
      & hdf5_write_n3, hdf5_write_n4, hdf5_write_n5, &
      & hdf5_write_n6, hdf5_write_n7, &
      & hdf5_write_st0, hdf5_write_st1, hdf5_write_st1_helper, &
      & hdf5_write_st2, hdf5_write_st2_helper, &
      & hdf5_write_c1
    GENERIC, PUBLIC :: Write => &
      & hdf5_write_d0, hdf5_write_d1, hdf5_write_d2, &
      & hdf5_write_d3, hdf5_write_d4, hdf5_write_d5, &
      & hdf5_write_d6, hdf5_write_d7, &
      & hdf5_write_s0, hdf5_write_s1, hdf5_write_s2, &
      & hdf5_write_s3, hdf5_write_s4, hdf5_write_s5, &
      & hdf5_write_s6, hdf5_write_s7, &
      & hdf5_write_b0, hdf5_write_b1, hdf5_write_b2, &
      & hdf5_write_b3, &
      & hdf5_write_n0, hdf5_write_n1, hdf5_write_n2, &
      & hdf5_write_n3, hdf5_write_n4, hdf5_write_n5, &
      & hdf5_write_n6, hdf5_write_n7, &
      & hdf5_write_st0, hdf5_write_st1, hdf5_write_st1_helper, &
      & hdf5_write_st2, hdf5_write_st2_helper, &
      & hdf5_write_c1
    PROCEDURE, PASS( Obj ) :: &
      & hdf5_read_d0, hdf5_read_d1, hdf5_read_d2, &
      & hdf5_read_d3, hdf5_read_d4, hdf5_read_d5, &
      & hdf5_read_d6, hdf5_read_d7, &
      & hdf5_read_s0, hdf5_read_s1, hdf5_read_s2, &
      & hdf5_read_s3, hdf5_read_s4, hdf5_read_s5, &
      & hdf5_read_s6, hdf5_read_s7, &
      & hdf5_read_n0, hdf5_read_n1, hdf5_read_n2, &
      & hdf5_read_n3, hdf5_read_n4, hdf5_read_n5, &
      & hdf5_read_n6, hdf5_read_n7, &
      & hdf5_read_st0, hdf5_read_st0_helper, &
      & hdf5_read_st1, hdf5_read_st1_helper, &
      & hdf5_read_st2, hdf5_read_st2_helper, &
      & hdf5_read_c1, hdf5_read_b0, hdf5_read_b1, &
      & hdf5_read_b2, hdf5_read_b3

    GENERIC, PUBLIC :: Read => &
      & hdf5_read_d0, hdf5_read_d1, hdf5_read_d2, &
      & hdf5_read_d3, hdf5_read_d4, hdf5_read_d5, &
      & hdf5_read_d6, hdf5_read_d7, &
      & hdf5_read_s0, hdf5_read_s1, hdf5_read_s2, &
      & hdf5_read_s3, hdf5_read_s4, hdf5_read_s5, &
      & hdf5_read_s6, hdf5_read_s7, &
      & hdf5_read_n0, hdf5_read_n1, hdf5_read_n2, &
      & hdf5_read_n3, hdf5_read_n4, hdf5_read_n5, &
      & hdf5_read_n6, hdf5_read_n7, &
      & hdf5_read_st0, hdf5_read_st0_helper, &
      & hdf5_read_st1, hdf5_read_st1_helper, &
      & hdf5_read_st2, hdf5_read_st2_helper, &
      & hdf5_read_c1, hdf5_read_b0, hdf5_read_b1, &
      & hdf5_read_b2, hdf5_read_b3

    PROCEDURE, PASS( Obj ) :: &
      & hdf5_write_attribute_st0, hdf5_write_attribute_c0, &
      & hdf5_write_attribute_i0, hdf5_write_attribute_d0

    GENERIC, PUBLIC :: WriteAttribute => &
      & hdf5_write_attribute_st0, hdf5_write_attribute_c0, &
      & hdf5_write_attribute_i0, hdf5_write_attribute_d0

    PROCEDURE, PASS( Obj ) :: &
      & hdf5_read_attribute_st0, hdf5_read_attribute_c0, &
      & hdf5_read_attribute_i0, hdf5_read_attribute_d0

    GENERIC, PUBLIC :: ReadAttribute => &
      & hdf5_read_attribute_st0, hdf5_read_attribute_c0, &
      & hdf5_read_attribute_i0, hdf5_read_attribute_d0

    PROCEDURE, PUBLIC, PASS( Obj ) :: getDataShape
    PROCEDURE, PUBLIC, PASS( Obj ) :: getDataType

END TYPE HDF5File_

PUBLIC :: HDF5File_

TYPE :: HDF5FilePointer_
  CLASS( HDF5File_ ), POINTER :: ptr => NULL()
END TYPE HDF5FilePointer_

PUBLIC :: HDF5FilePointer_

!----------------------------------------------------------------------------
!                                                               addSurrogate
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE hdf5_addSurrogate( obj, UserObj )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  TYPE( ExceptionHandler_ ), INTENT( IN ) :: UserObj
END SUBROUTINE hdf5_addSurrogate
END INTERFACE

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

!> authors: Vikas Sharma, Ph. D.
! date: 	6 June 2021
! summary: Destructs an HDF5 file object
!
!# Introduction
!
! This routine releases resources used for interacting with the HDF5 file. It
! closes or deletes the file and the HDF5 library interface. The structure
! was taken from the Fortran file.
!

INTERFACE
MODULE SUBROUTINE hdf5_clear(obj, Delete)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  LOGICAL( LGT ), INTENT( IN ), OPTIONAL :: Delete
END SUBROUTINE hdf5_clear
END INTERFACE

!----------------------------------------------------------------------------
!                                                          setOverwriteStat
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 June 2021
! summary: 	Sets the value for the status of whether or not the file will be overwritable.

INTERFACE
MODULE SUBROUTINE hdf5_setOverwriteStat( obj, bool )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  LOGICAL( LGT ), INTENT( IN ) :: bool
END SUBROUTINE hdf5_setOverwriteStat
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 getUnitNo
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 June 2021
! summary: gets the unit number used by the HDF5 file

INTERFACE
MODULE PURE FUNCTION hdf5_getUnitNo( obj ) RESULT( Ans )
  CLASS( HDF5File_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: ans
END FUNCTION hdf5_getUnitNo
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 isNew
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 June 2021
! summary: Returns whether or not the HDF5 file is new.

INTERFACE
MODULE PURE FUNCTION hdf5_isNew( obj ) RESULT( Ans )
  CLASS( HDF5File_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ) :: ans
END FUNCTION hdf5_isNew
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 setNewStat
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	6 June 2021
! summary: Sets whether or not the HDF5 file is new.

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

INTERFACE
MODULE SUBROUTINE hdf5_getChunkSize( obj, path, cdims )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: path
  INTEGER( HSIZE_T ), ALLOCATABLE, INTENT( OUT ) :: cdims( : )
END SUBROUTINE hdf5_getChunkSize
END INTERFACE

!----------------------------------------------------------------------------
!                                                             isCompressed
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Returns whether an HDF5 File or data set has compression
!
! # Introduction
!
! - If the path is present, then whether or not the dataset is compressed is
! returned.
! - If the path is ommitted then the value of the hasCompression attribute is
! returned.
! - If a non-existent path is passed, a DBC error is thrown.

INTERFACE
MODULE FUNCTION hdf5_isCompressed( obj, path ) RESULT( Ans )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: path
    !! The path to a dataset in the file, which must exist
  LOGICAL( LGT ) :: Ans
END FUNCTION hdf5_isCompressed
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 PreWrite
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE preWrite( obj,rank,gdims,ldims,path,mem,dset_id,dspace_id,&
  & gspace_id,plist_id,error,cnt,offset )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  INTEGER, INTENT( IN ) :: rank
  INTEGER( HSIZE_T ), INTENT( IN ) :: gdims(:)
    !! global data space dimensions, i.e. shape
  INTEGER( HSIZE_T ), INTENT( IN ) :: ldims(:)
    !! local data space dimension
  CHARACTER( LEN=* ),INTENT( INOUT ) :: path
    !! path of data set
  INTEGER( HID_T ), INTENT( IN ) :: mem
    !!
  INTEGER(HID_T),INTENT(OUT) :: dset_id
    !! data set id
  INTEGER(HID_T),INTENT(OUT) :: dspace_id
    !! local dataspace id
  INTEGER(HID_T),INTENT(OUT) :: gspace_id
    !! global dataspace id
  INTEGER(HID_T),INTENT(OUT) :: plist_id
    !! dataset properties id
  INTEGER( I4B ),INTENT(OUT) :: error
    !! error flag
  INTEGER(HSIZE_T),INTENT(IN) :: cnt(:)
    !! count
  INTEGER(HSSIZE_T),INTENT(IN) :: offset(:)
END SUBROUTINE preWrite
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 ChunkSize
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	9 May 2021
! summary: Computes the optimal chunk size for a data set for writing with compression
!
!### Introduction
! Choosing the chunk size is EXTREMELY important to managing the memory
! overhead of the HDF5 library when using compression. A detailed discussion
! can be found on the HDF5 website:
!
! https://support.hdfgroup.org/HDF5/doc/Advanced/Chunking/index.html
!
! Here we choose a chunk size that is the smaller of 1 MB and the maximum size
! of the data set to be chunked to limit excessive memory overhead.
!
! This way of choosing the chunk size ONLY considers the memory overhead. It's
! still possible this could result in an increased execution time (via
! addtional I/O operations). In the future it may be worth revisiting to
! to optimize chunk sizes for both.

INTERFACE
MODULE SUBROUTINE compute_chunk_size(mem,gdims,cdims)
  INTEGER( HID_T ), INTENT( IN ) :: mem
  INTEGER( HSIZE_T ), INTENT( IN ) :: gdims(:)
  !! the global dimensions of the data set
  INTEGER( HSIZE_T ), INTENT( OUT ) :: cdims(:)
  !! the chunk dimensions to use
END SUBROUTINE compute_chunk_size
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 postwrite
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE postWrite(obj,error,dset_id,dspace_id,gspace_id,plist_id)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( INOUT ) :: error
  INTEGER( HID_T ), INTENT( INOUT ) :: dset_id
  INTEGER( HID_T ), INTENT( INOUT ) :: dspace_id
  INTEGER( HID_T ), INTENT( INOUT ) :: gspace_id
  INTEGER( HID_T ), INTENT( INOUT ) :: plist_id
END SUBROUTINE postWrite
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Write
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: Write double precision scalar data
!
!### Introduction
!
! This routine writes a Real64 datatype scalar to a dataset of name and path which is specified by `dsetname` by using the shape specified by `gdims_in`, if present.

INTERFACE
MODULE SUBROUTINE hdf5_write_d0( obj, dsetname, vals, gdims_in, cnt_in, &
  & offset_in )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
    !! HDF5 File object
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
    !! Dataset name and path
  REAL( Real64 ), INTENT( IN ) :: vals
    !! Value to be written
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: gdims_in( 1 )
    !! Global dimension; Shape of data to write with
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: cnt_in
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: offset_in
END SUBROUTINE hdf5_write_d0
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Write
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Write a rank-1 array of Real64 datatype to hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_write_d1( obj, dsetname, vals, gdims_in, cnt_in, &
  & offset_in )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
    !! hdf5 file object
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
    !! dataset name and path
  REAL( Real64 ), INTENT( IN ) :: vals( : )
    !! Rank-1 array of reals which will be written in hdf5 file
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: gdims_in( 1 )
    !! Shape of data to write with
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: cnt_in( 1 )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: offset_in( 1 )
END SUBROUTINE hdf5_write_d1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Write
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Write a rank-2 array of Real64 datatype to hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_write_d2( obj, dsetname, vals, gdims_in, cnt_in, &
  & offset_in )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
    !! hdf5 file object
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
    !! Dataset name and path
  REAL( Real64 ), INTENT( IN ) :: vals( :, : )
    !! Rank-2 array of Real64
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: gdims_in( 2 )
    !! Shape of data to write with
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: cnt_in( 2 )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: offset_in( 2 )
END SUBROUTINE hdf5_write_d2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Write
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Write a rank-3 array of Real64 datatype to hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_write_d3( obj, dsetname, vals, gdims_in, cnt_in, &
  & offset_in )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
    !! HDF5 file object
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
    !! Dataset name and path
  REAL( Real64 ), INTENT( IN ) :: vals( :, :, : )
    !! Rank 3 array of real64
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: gdims_in( 3 )
    !! Shape of data to write with
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: cnt_in( 3 )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: offset_in( 3 )
END SUBROUTINE hdf5_write_d3
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Write
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Write a rank-4 array of Real64 datatype to hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_write_d4( obj, dsetname, vals, gdims_in, cnt_in, &
  & offset_in )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
    !! HDF5 file object
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
    !! Dataset name and path
  REAL( Real64 ), INTENT( IN ) :: vals( :, :, :, : )
    !! Rank4 array of real64
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: gdims_in( 4 )
    !! Shape of data to write with
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: cnt_in( 4 )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: offset_in( 4 )
END SUBROUTINE hdf5_write_d4
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Write
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Write a rank-5 array of Real64 datatype to hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_write_d5( obj, dsetname, vals, gdims_in, cnt_in, &
  & offset_in )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
    !! HDF5 file object
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
    !! Dataset name and path
  REAL( Real64 ), INTENT( IN ) :: vals( :, :, :, :, : )
    !! Rank5 array of Real64
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: gdims_in( 5 )
    !! Shape of data to write with
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: cnt_in( 5 )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: offset_in( 5 )
END SUBROUTINE hdf5_write_d5
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Write
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Write a rank-6 array of Real64 datatype to hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_write_d6( obj, dsetname, vals, gdims_in, cnt_in, &
  & offset_in )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
    !! HDF5 file object
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
    !! Dataset name and path
  REAL( Real64 ), INTENT( IN ) :: vals( :, :, :, :, :, : )
    !! Rank6 array of datatype Real64
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: gdims_in( 6 )
    !! Shape of data to write with
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: cnt_in( 6 )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: offset_in( 6 )
END SUBROUTINE hdf5_write_d6
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Write
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Write a rank-7 array of Real64 datatype to hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_write_d7( obj, dsetname, vals, gdims_in, cnt_in, &
  & offset_in )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
    !! HDF5 file object
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
    !! Dataset name and path
  REAL( Real64 ), INTENT( IN ) :: vals( :, :, :, :, :, :, : )
    !! Rank7 array of real64
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: gdims_in( 7 )
    !! Shape of data to write with
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: cnt_in( 7 )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: offset_in( 7 )
END SUBROUTINE hdf5_write_d7
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Write
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE hdf5_write_s0( obj, dsetname, vals, gdims_in, cnt_in, &
  & offset_in )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
    !! HDF5 file object
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
    !! Dataset name and path
  REAL( Real32 ), INTENT( IN ) :: vals
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: gdims_in( 1 )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: cnt_in
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: offset_in
END SUBROUTINE hdf5_write_s0
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Write
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE hdf5_write_s1( obj, dsetname, vals, gdims_in, cnt_in, &
  & offset_in )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
  REAL( Real32 ), INTENT( IN ) :: vals( : )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: gdims_in( 1 )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: cnt_in( 1 )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: offset_in( 1 )
END SUBROUTINE hdf5_write_s1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Write
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE hdf5_write_s2( obj, dsetname, vals, gdims_in, cnt_in, &
  & offset_in )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
  REAL( Real32 ), INTENT( IN ) :: vals( :, : )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: gdims_in( 2 )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: cnt_in( 2 )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: offset_in( 2 )
END SUBROUTINE hdf5_write_s2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Write
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE hdf5_write_s3( obj, dsetname, vals, gdims_in, cnt_in, &
  & offset_in )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
  REAL( Real32 ), INTENT( IN ) :: vals( :, :, : )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: gdims_in( 3 )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: cnt_in( 3 )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: offset_in( 3 )
END SUBROUTINE hdf5_write_s3
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Write
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE hdf5_write_s4( obj, dsetname, vals, gdims_in, cnt_in, &
  & offset_in )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
  REAL( Real32 ), INTENT( IN ) :: vals( :, :, :, : )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: gdims_in( 4 )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: cnt_in( 4 )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: offset_in( 4 )
END SUBROUTINE hdf5_write_s4
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Write
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE hdf5_write_s5( obj, dsetname, vals, gdims_in, cnt_in, &
  & offset_in )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
  REAL( Real32 ), INTENT( IN ) :: vals( :, :, :, :, : )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: gdims_in( 5 )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: cnt_in( 5 )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: offset_in( 5 )
END SUBROUTINE hdf5_write_s5
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Write
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE hdf5_write_s6( obj, dsetname, vals, gdims_in, cnt_in, &
  & offset_in )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
  REAL( Real32 ), INTENT( IN ) :: vals( :, :, :, :, :, : )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: gdims_in( 6 )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: cnt_in( 6 )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: offset_in( 6 )
END SUBROUTINE hdf5_write_s6
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Write
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE hdf5_write_s7( obj, dsetname, vals, gdims_in, cnt_in, &
  & offset_in )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
  REAL( Real32 ), INTENT( IN ) :: vals( :, :, :, :, :, :, : )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: gdims_in( 7 )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: cnt_in( 7 )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: offset_in( 7 )
END SUBROUTINE hdf5_write_s7
END INTERFACE


!----------------------------------------------------------------------------
!                                                                      Write
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Writes logical scalar to hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_write_b0( obj, dsetname, vals, gdims_in, &
  & cnt_in, offset_in )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
    !! HDF5 file object
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
    !! Dataset name and path
  LOGICAL( LGT ), INTENT( IN ) :: vals
    !! Logical scalar
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: gdims_in( 1 )
    !! shape of data to write
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: cnt_in
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: offset_in
END SUBROUTINE hdf5_write_b0
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Write
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Writes rank-1 array logical scalar to hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_write_b1( obj, dsetname, vals, gdims_in, &
  & cnt_in, offset_in )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
    !! HDF5 file data
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
    !! Dataset name and path
  LOGICAL( LGT ), INTENT( IN ) :: vals( : )
    !! Rank-1 array of logical type
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: gdims_in( 1 )
    !! Shape of data to write
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: cnt_in( 1 )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: offset_in( 1 )
END SUBROUTINE hdf5_write_b1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Write
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Writes rank-4 array logical scalar to hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_write_b2( obj, dsetname, vals, gdims_in, &
  & cnt_in, offset_in )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
    !! HDF5 file object
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
    !! Dataset name and path
  LOGICAL( LGT ), INTENT( IN ) :: vals( :, : )
    !! Rank 2, array of logical data types
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: gdims_in( 2 )
    !! Shape of data to write
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: cnt_in( 2 )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: offset_in( 2 )
END SUBROUTINE hdf5_write_b2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Write
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Writes rank-3 array logical scalar to hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_write_b3( obj, dsetname, vals, gdims_in, &
  & cnt_in, offset_in )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
    !! HDF5 data type
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
    !! Dataset name and path
  LOGICAL( LGT ), INTENT( IN ) :: vals( :, :, : )
    !! Rank 2, array of logical data types
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: gdims_in( 3 )
    !! Shape of data to write
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: cnt_in( 3 )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: offset_in( 3 )
END SUBROUTINE hdf5_write_b3
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Write
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Writes scalar int32 datatype to hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_write_n0( obj, dsetname, vals, gdims_in, &
  & cnt_in, offset_in )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
    !! HDF5 data type
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
    !! Dataset name and path
  INTEGER( Int32 ), INTENT( IN ) :: vals
    !! Rank 0, array of int32 data types
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: gdims_in( 1 )
    !! Shape of data to write
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: cnt_in( 1 )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: offset_in( 1 )
END SUBROUTINE hdf5_write_n0
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Write
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Writes rank-1 array of int32 datatype to hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_write_n1( obj, dsetname, vals, gdims_in, &
  & cnt_in, offset_in )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
    !! HDF5 data type
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
    !! Dataset name and path
  INTEGER( Int32 ), INTENT( IN ) :: vals( : )
    !! Rank 0, array of int32 data types
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: gdims_in( 1 )
    !! Shape of data to write
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: cnt_in( 1 )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: offset_in( 1 )
END SUBROUTINE hdf5_write_n1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Write
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Writes rank-2 array of int32 datatype to hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_write_n2( obj, dsetname, vals, gdims_in, &
  & cnt_in, offset_in )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
    !! HDF5 data type
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
    !! Dataset name and path
  INTEGER( Int32 ), INTENT( IN ) :: vals( :, : )
    !! Rank 2, array of int32 data types
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: gdims_in( 2 )
    !! Shape of data to write
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: cnt_in( 2 )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: offset_in( 2 )
END SUBROUTINE hdf5_write_n2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Write
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Writes rank-3 array of int32 datatype to hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_write_n3( obj, dsetname, vals, gdims_in, &
  & cnt_in, offset_in )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
    !! HDF5 data type
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
    !! Dataset name and path
  INTEGER( Int32 ), INTENT( IN ) :: vals( :, :, : )
    !! Rank 3, array of int32 data types
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: gdims_in( 3 )
    !! Shape of data to write
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: cnt_in( 3 )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: offset_in( 3 )
END SUBROUTINE hdf5_write_n3
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Write
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Writes rank-4 array of int32 datatype to hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_write_n4( obj, dsetname, vals, gdims_in, &
  & cnt_in, offset_in )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
    !! HDF5 data type
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
    !! Dataset name and path
  INTEGER( Int32 ), INTENT( IN ) :: vals( :, :, :, : )
    !! Rank 4, array of int32 data types
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: gdims_in( 4 )
    !! Shape of data to write
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: cnt_in( 4 )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: offset_in( 4 )
END SUBROUTINE hdf5_write_n4
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Write
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Writes rank-5 array of int32 datatype to hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_write_n5( obj, dsetname, vals, gdims_in, &
  & cnt_in, offset_in )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
    !! HDF5 data type
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
    !! Dataset name and path
  INTEGER( Int32 ), INTENT( IN ) :: vals( :, :, :, :, : )
    !! Rank 5, array of int32 data types
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: gdims_in( 5 )
    !! Shape of data to write
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: cnt_in( 5 )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: offset_in( 5 )
END SUBROUTINE hdf5_write_n5
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Write
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Writes rank-6 array of int32 datatype to hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_write_n6( obj, dsetname, vals, gdims_in, &
  & cnt_in, offset_in )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
    !! HDF5 data type
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
    !! Dataset name and path
  INTEGER( Int32 ), INTENT( IN ) :: vals( :, :, :, :, :, : )
    !! Rank 6, array of int32 data types
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: gdims_in( 6 )
    !! Shape of data to write
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: cnt_in( 6 )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: offset_in( 6 )
END SUBROUTINE hdf5_write_n6
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Write
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Writes rank-7 array of int32 datatype to hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_write_n7( obj, dsetname, vals, gdims_in, &
  & cnt_in, offset_in )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
    !! HDF5 data type
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
    !! Dataset name and path
  INTEGER( Int32 ), INTENT( IN ) :: vals( :, :, :, :, :, :, : )
    !! Rank 7, array of int32 data types
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: gdims_in( 7 )
    !! Shape of data to write
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: cnt_in( 7 )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: offset_in( 7 )
END SUBROUTINE hdf5_write_n7
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Write
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Writes scalar int64 datatype to hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_write_st0( obj, dsetname, vals, gdims_in, &
  & cnt_in, offset_in )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
    !! HDF5 data type
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
    !! Dataset name and path
  TYPE(String), INTENT( IN ) :: vals
    !! Rank 0, array of int64 data types
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: gdims_in( 1 )
    !! Shape of data to write
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: cnt_in( 1 )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: offset_in( 1 )
END SUBROUTINE hdf5_write_st0
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Write
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Writes scalar int64 datatype to hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_write_st1_helper( obj, dsetname, vals, gdims_in, &
  & cnt_in, offset_in )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
    !! HDF5 data type
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
    !! Dataset name and path
  TYPE(String), INTENT( IN ) :: vals( : )
    !! Rank 0, array of int64 data types
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: gdims_in( 1 )
    !! Shape of data to write
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: cnt_in( 1 )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: offset_in( 1 )
END SUBROUTINE hdf5_write_st1_helper
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Write
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Writes scalar int64 datatype to hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_write_st1( obj, dsetname, vals, length_max, gdims_in, &
  & cnt_in, offset_in )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
    !! HDF5 data type
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
    !! Dataset name and path
  TYPE(String), INTENT( IN ) :: vals( : )
    !! Rank 0, array of int64 data types
  INTEGER( I4B ), INTENT( IN ) :: length_max
    !! The length of the longest stringType to be written
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: gdims_in( 1 )
    !! Shape of data to write
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: cnt_in( 1 )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: offset_in( 1 )
END SUBROUTINE hdf5_write_st1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Write
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Writes scalar int64 datatype to hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_write_st2_helper( obj, dsetname, vals, gdims_in, &
  & cnt_in, offset_in )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
    !! HDF5 data type
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
    !! Dataset name and path
  TYPE(String), INTENT( IN ) :: vals( :, : )
    !! Rank 0, array of int64 data types
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: gdims_in( 2 )
    !! Shape of data to write
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: cnt_in( 2 )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: offset_in( 2 )
END SUBROUTINE hdf5_write_st2_helper
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Write
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Writes scalar int64 datatype to hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_write_st2( obj, dsetname, vals, length_max, gdims_in, &
  & cnt_in, offset_in )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
    !! HDF5 data type
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
    !! Dataset name and path
  TYPE(String), INTENT( IN ) :: vals( :, : )
    !! Rank 0, array of int64 data types
  INTEGER( I4B ), INTENT( IN ) :: length_max
    !! The length of the longest stringType to be written
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: gdims_in( 2 )
    !! Shape of data to write
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: cnt_in( 2 )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: offset_in( 2 )
END SUBROUTINE hdf5_write_st2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Write
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Writes scalar int64 datatype to hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_write_st3_helper( obj, dsetname, vals, gdims_in, &
  & cnt_in, offset_in )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
    !! HDF5 data type
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
    !! Dataset name and path
  TYPE(String), INTENT( IN ) :: vals( :, :, : )
    !! Rank 0, array of int64 data types
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: gdims_in( 3 )
    !! Shape of data to write
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: cnt_in( 3 )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: offset_in( 3 )
END SUBROUTINE hdf5_write_st3_helper
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Write
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Writes scalar int64 datatype to hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_write_st3( obj, dsetname, vals, length_max, gdims_in, &
  & cnt_in, offset_in )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
    !! HDF5 data type
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
    !! Dataset name and path
  TYPE(String), INTENT( IN ) :: vals( :, :, : )
    !! Rank 0, array of int64 data types
  INTEGER( I4B ), INTENT( IN ) :: length_max
    !! The length of the longest stringType to be written
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: gdims_in( 3 )
    !! Shape of data to write
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: cnt_in( 3 )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: offset_in( 3 )
END SUBROUTINE hdf5_write_st3
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Write
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE hdf5_write_c1(obj,dsetname,vals,gdims_in,cnt_in,offset_in)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN=* ), INTENT( IN ) :: dsetname
  CHARACTER( LEN=* ), INTENT( IN ) :: vals
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: gdims_in
  INTEGER( I4B ), DIMENSION(1), INTENT(IN), OPTIONAL :: cnt_in
  INTEGER( I4B ), DIMENSION(1), INTENT(IN), OPTIONAL :: offset_in
END SUBROUTINE hdf5_write_c1
END INTERFACE

!----------------------------------------------------------------------------
!                                                             preRead
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE preRead( obj, path, rank, dset_id, dspace_id, &
  & dims, error )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN=* ), INTENT( IN ) :: path
  INTEGER( I4B ), INTENT( IN ) :: rank
  INTEGER( HID_T ), INTENT( INOUT ) :: dset_id
  INTEGER( HID_T ), INTENT( INOUT ) :: dspace_id
  INTEGER( HSIZE_T ), INTENT( INOUT ) :: dims(:)
  INTEGER( I4B ), INTENT( OUT ) :: error
END SUBROUTINE preRead
END INTERFACE

!----------------------------------------------------------------------------
!                                                               getDataShape
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	8 June 2021
! summary: 	the shape of the data array; size-0 implies a scalar dataset

INTERFACE
MODULE FUNCTION getDataShape( obj, dsetname ) RESULT( dataShape )
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN=* ), INTENT( IN ) :: dsetname
  INTEGER( I4B ), ALLOCATABLE :: dataShape( : )
END FUNCTION getDataShape
END INTERFACE

!----------------------------------------------------------------------------
!                                                               getDataType
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	8 June 2021
! summary: returns the shape of the dataset

INTERFACE
MODULE FUNCTION getDataType(obj,dsetname) RESULT(dataType)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN=* ), INTENT( IN ) :: dsetname
  CHARACTER( LEN=3 ) :: dataType
END FUNCTION getDataType
END INTERFACE

!----------------------------------------------------------------------------
!                                                            postRead
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	8 June 2021
! summary:

INTERFACE
MODULE SUBROUTINE postRead( obj, path, dset_id, dspace_id, error )
  CLASS( HDF5File_ ), INTENT( INOUT ) ::  obj
  CHARACTER( LEN=* ), INTENT( IN ) :: path
  INTEGER( HID_T ), INTENT( INOUT ) :: dset_id
  INTEGER( HID_T ), INTENT( INOUT ) :: dspace_id
  INTEGER( I4B ), INTENT( INOUT ) :: error
END SUBROUTINE postRead
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Read a scalar of real64 datatype from hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_read_d0(obj, dsetname, vals)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
  REAL( Real64 ), INTENT( INOUT ) :: vals
END SUBROUTINE hdf5_read_d0
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Read an array of rank-1 of real64 datatype from hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_read_d1(obj, dsetname, vals)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
  REAL( Real64 ), ALLOCATABLE, INTENT( INOUT ) :: vals( : )
END SUBROUTINE hdf5_read_d1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Read an array of rank-2 of real64 datatype from hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_read_d2(obj, dsetname, vals)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
  REAL( Real64 ), ALLOCATABLE, INTENT( INOUT ) :: vals( :, : )
END SUBROUTINE hdf5_read_d2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Read an array of rank-3 of real64 datatype from hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_read_d3(obj, dsetname, vals)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
  REAL( Real64 ), ALLOCATABLE, INTENT( INOUT ) :: vals( :, :, : )
END SUBROUTINE hdf5_read_d3
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Read an array of rank-4 of real64 datatype from hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_read_d4(obj, dsetname, vals)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
  REAL( Real64 ), ALLOCATABLE, INTENT( INOUT ) :: vals( :, :, :, : )
END SUBROUTINE hdf5_read_d4
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Read an array of rank-5 of real64 datatype from hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_read_d5(obj, dsetname, vals)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
  REAL( Real64 ), ALLOCATABLE, INTENT( INOUT ) :: vals( :, :, :, :, : )
END SUBROUTINE hdf5_read_d5
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Read an array of rank-6 of real64 datatype from hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_read_d6(obj, dsetname, vals)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
  REAL( Real64 ), ALLOCATABLE, INTENT( INOUT ) :: vals( :, :, :, :, :, : )
END SUBROUTINE hdf5_read_d6
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Read an array of rank-7 of real64 datatype from hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_read_d7(obj, dsetname, vals)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
  REAL( Real64 ), ALLOCATABLE, INTENT( INOUT ) :: vals( :, :, :, :, :, :, : )
END SUBROUTINE hdf5_read_d7
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Read a scalar of real64 datatype from hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_read_s0(obj, dsetname, vals)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
  REAL( Real32 ), INTENT( INOUT ) :: vals
END SUBROUTINE hdf5_read_s0
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Read an array of rank-1 of real64 datatype from hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_read_s1(obj, dsetname, vals)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
  REAL( Real32 ), ALLOCATABLE, INTENT( INOUT ) :: vals( : )
END SUBROUTINE hdf5_read_s1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Read an array of rank-2 of real64 datatype from hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_read_s2(obj, dsetname, vals)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
  REAL( Real32 ), ALLOCATABLE, INTENT( INOUT ) :: vals( :, : )
END SUBROUTINE hdf5_read_s2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Read an array of rank-3 of real64 datatype from hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_read_s3(obj, dsetname, vals)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
  REAL( Real32 ), ALLOCATABLE, INTENT( INOUT ) :: vals( :, :, : )
END SUBROUTINE hdf5_read_s3
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Read an array of rank-4 of real64 datatype from hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_read_s4(obj, dsetname, vals)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
  REAL( Real32 ), ALLOCATABLE, INTENT( INOUT ) :: vals( :, :, :, : )
END SUBROUTINE hdf5_read_s4
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Read an array of rank-5 of real64 datatype from hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_read_s5(obj, dsetname, vals)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
  REAL( Real32 ), ALLOCATABLE, INTENT( INOUT ) :: vals( :, :, :, :, : )
END SUBROUTINE hdf5_read_s5
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Read an array of rank-6 of real64 datatype from hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_read_s6(obj, dsetname, vals)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
  REAL( Real32 ), ALLOCATABLE, INTENT( INOUT ) :: vals( :, :, :, :, :, : )
END SUBROUTINE hdf5_read_s6
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Read an array of rank-7 of real64 datatype from hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_read_s7(obj, dsetname, vals)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
  REAL( Real32 ), ALLOCATABLE, INTENT( INOUT ) :: vals( :, :, :, :, :, :, : )
END SUBROUTINE hdf5_read_s7
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Read
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Read a scalar of real64 datatype from hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_read_n0(obj, dsetname, vals)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
  INTEGER( I4B ), INTENT( INOUT ) :: vals
END SUBROUTINE hdf5_read_n0
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Read an array of rank-1 of real64 datatype from hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_read_n1(obj, dsetname, vals)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: vals( : )
END SUBROUTINE hdf5_read_n1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Read an array of rank-2 of real64 datatype from hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_read_n2(obj, dsetname, vals)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: vals( :, : )
END SUBROUTINE hdf5_read_n2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Read an array of rank-3 of real64 datatype from hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_read_n3(obj, dsetname, vals)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: vals( :, :, : )
END SUBROUTINE hdf5_read_n3
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Read an array of rank-4 of real64 datatype from hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_read_n4(obj, dsetname, vals)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: vals( :, :, :, : )
END SUBROUTINE hdf5_read_n4
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Read an array of rank-5 of real64 datatype from hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_read_n5(obj, dsetname, vals)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: vals( :, :, :, :, : )
END SUBROUTINE hdf5_read_n5
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Read an array of rank-6 of real64 datatype from hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_read_n6(obj, dsetname, vals)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: vals( :, :, :, :, :, : )
END SUBROUTINE hdf5_read_n6
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	7 June 2021
! summary: 	Read an array of rank-7 of real64 datatype from hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_read_n7(obj, dsetname, vals)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: dsetname
  INTEGER( I4B ), ALLOCATABLE, INTENT( INOUT ) :: vals( :, :, :, :, :, :, : )
END SUBROUTINE hdf5_read_n7
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Read
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	8 June 2021
! summary: 	Read a string from dataset

INTERFACE
MODULE SUBROUTINE hdf5_read_st0_helper(obj,dsetname,vals)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN=* ), INTENT( IN ) :: dsetname
  TYPE( String ), INTENT( INOUT ) :: vals
END SUBROUTINE hdf5_read_st0_helper
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Read
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	8 June 2021
! summary: 	Read a string from dataset

INTERFACE
MODULE SUBROUTINE hdf5_read_st0(obj,dsetname,length_max,vals)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN=* ), INTENT( IN ) :: dsetname
  INTEGER( I4B ), INTENT( IN ) :: length_max
  TYPE( String ), INTENT( INOUT ) :: vals
END SUBROUTINE hdf5_read_st0
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Read
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	8 June 2021
! summary: 	Read a string from dataset

INTERFACE
MODULE SUBROUTINE hdf5_read_ca0(obj,dsetname,vals)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN=* ), INTENT( IN ) :: dsetname
  TYPE( String ), INTENT( INOUT ) :: vals
END SUBROUTINE hdf5_read_ca0
END INTERFACE

!----------------------------------------------------------------------------
!                                                                       Read
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	8 June 2021
! summary: 	Read a string from dataset

INTERFACE
MODULE SUBROUTINE hdf5_read_st1_helper(obj,dsetname,vals)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN=* ), INTENT( IN ) :: dsetname
  TYPE( String ), ALLOCATABLE, INTENT( INOUT ) :: vals(:)
END SUBROUTINE hdf5_read_st1_helper
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Read
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE hdf5_read_st1(obj,dsetname,length_max,vals)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN=* ), INTENT( IN ) :: dsetname
  INTEGER( I4B ), INTENT( IN ) :: length_max
  TYPE( String ), ALLOCATABLE, INTENT( INOUT ) :: vals(:)
END SUBROUTINE hdf5_read_st1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Read
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE hdf5_read_ca1(obj,dsetname,vals)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN=* ), INTENT( IN ) :: dsetname
  TYPE( String ), ALLOCATABLE, INTENT( INOUT ) :: vals(:)
END SUBROUTINE hdf5_read_ca1
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE hdf5_read_st2_helper(obj,dsetname,vals)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN=* ), INTENT( IN ) :: dsetname
  TYPE( String ), ALLOCATABLE, INTENT( INOUT ) :: vals(:,:)
END SUBROUTINE hdf5_read_st2_helper
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE hdf5_read_st2(obj,dsetname,length_max,vals)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN=* ), INTENT( IN ) :: dsetname
  INTEGER( I4B ), INTENT( IN ) :: length_max
  TYPE( String ), ALLOCATABLE, INTENT( INOUT ) :: vals(:,:)
END SUBROUTINE hdf5_read_st2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                       Read
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE hdf5_read_ca2(obj,dsetname,vals)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN=* ), INTENT( IN ) :: dsetname
  TYPE( String ), ALLOCATABLE, INTENT( INOUT ) :: vals(:,:)
END SUBROUTINE
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE hdf5_read_st3_helper(obj,dsetname,vals)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN=* ), INTENT( IN ) :: dsetname
  TYPE( String ), ALLOCATABLE, INTENT( INOUT ) :: vals(:,:,:)
END SUBROUTINE hdf5_read_st3_helper
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE hdf5_read_st3(obj,dsetname,length_max,vals)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN=* ), INTENT( IN ) :: dsetname
  INTEGER( I4B ), INTENT( IN ) :: length_max
  TYPE( String ), ALLOCATABLE, INTENT( INOUT ) :: vals(:,:,:)
END SUBROUTINE hdf5_read_st3
END INTERFACE

!----------------------------------------------------------------------------
!                                                                       Read
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE hdf5_read_ca3(obj,dsetname,vals)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN=* ), INTENT( IN ) :: dsetname
  TYPE( String ), ALLOCATABLE, INTENT( INOUT ) :: vals(:,:,:)
END SUBROUTINE hdf5_read_ca3
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Read
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	9 June 2021
! summary: 	Read string from hdf5 file

INTERFACE
MODULE SUBROUTINE  hdf5_read_c1(obj,dsetname,vals)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN=* ), INTENT( IN ) :: dsetname
  CHARACTER( LEN=* ), INTENT( INOUT ) :: vals
END SUBROUTINE hdf5_read_c1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Read
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	9 June 2021
! summary: 	Read Boolean from hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_read_b0(obj,dsetname,vals)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN=* ), INTENT( IN ) :: dsetname
  LOGICAL( LGT ), INTENT( INOUT ) :: vals
END SUBROUTINE hdf5_read_b0
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Read
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	9 June 2021
! summary: 	Read Boolean from hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_read_b1(obj,dsetname,vals)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN=* ), INTENT( IN ) :: dsetname
  LOGICAL( LGT ), ALLOCATABLE, INTENT( INOUT ) :: vals( : )
END SUBROUTINE hdf5_read_b1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Read
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	9 June 2021
! summary: 	Read Boolean from hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_read_b2(obj,dsetname,vals)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN=* ), INTENT( IN ) :: dsetname
  LOGICAL( LGT ), ALLOCATABLE, INTENT( INOUT ) :: vals(:,:)
END SUBROUTINE hdf5_read_b2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Read
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	9 June 2021
! summary: 	Read Boolean from hdf5 file

INTERFACE
MODULE SUBROUTINE hdf5_read_b3(obj,dsetname,vals)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN=* ), INTENT( IN ) :: dsetname
  LOGICAL( LGT ), ALLOCATABLE, INTENT( INOUT ) :: vals(:,:,:)
END SUBROUTINE hdf5_read_b3
END INTERFACE

!----------------------------------------------------------------------------
!                                                            WriteAttribute
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	9 June 2021
! summary:  Write attributes to a dataset

INTERFACE
MODULE SUBROUTINE hdf5_write_attribute_st0( obj,obj_name,attr_name,attr_val)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN=* ), INTENT( IN ) :: obj_name, attr_name
  TYPE( String ) :: attr_val
END SUBROUTINE hdf5_write_attribute_st0
END INTERFACE

!----------------------------------------------------------------------------
!                                                            WriteAttribute
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	9 June 2021
! summary: 	Write attributes to a dataset

INTERFACE
MODULE SUBROUTINE hdf5_write_attribute_c0(obj,obj_name,attr_name,attr_val)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN=* ), INTENT( IN ) :: obj_name, attr_name
  CHARACTER( LEN=* ) :: attr_val
END SUBROUTINE hdf5_write_attribute_c0
END INTERFACE

!----------------------------------------------------------------------------
!                                                            WriteAttribute
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	9 June 2021
! summary: 	Write attributes to a dataset

INTERFACE
MODULE SUBROUTINE hdf5_write_attribute_i0(obj,obj_name,attr_name,attr_val)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN=* ), INTENT( IN ) :: obj_name, attr_name
  INTEGER( I4B ), INTENT( IN ) :: attr_val
END SUBROUTINE hdf5_write_attribute_i0
END INTERFACE

!----------------------------------------------------------------------------
!                                                            WriteAttribute
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	9 June 2021
! summary: 	Write attributes to a dataset

INTERFACE
MODULE SUBROUTINE hdf5_write_attribute_d0(obj,obj_name,attr_name,attr_val)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN=* ), INTENT( IN ) :: obj_name, attr_name
  REAL( DFP ), INTENT( IN ) :: attr_val
END SUBROUTINE hdf5_write_attribute_d0
END INTERFACE


!----------------------------------------------------------------------------
!                                                             ReadAttribute
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	9 June 2021
! summary: 	Read attribute from a known dataset

INTERFACE
MODULE SUBROUTINE hdf5_read_attribute_st0(obj,obj_name,attr_name,attr_val)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN=* ), INTENT( IN ) :: obj_name, attr_name
  TYPE( String ), INTENT( INOUT ) :: attr_val
END SUBROUTINE hdf5_read_attribute_st0
END INTERFACE

!----------------------------------------------------------------------------
!                                                             ReadAttribute
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	9 June 2021
! summary: 	Read attribute from a known dataset

INTERFACE
MODULE SUBROUTINE hdf5_read_attribute_c0(obj,obj_name,attr_name,attr_val)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN=* ), INTENT( IN ) :: obj_name, attr_name
  CHARACTER( LEN = * ), INTENT( INOUT ) :: attr_val
END SUBROUTINE hdf5_read_attribute_c0
END INTERFACE

!----------------------------------------------------------------------------
!                                                             ReadAttribute
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	9 June 2021
! summary: 	Read attribute from a known dataset

INTERFACE
MODULE SUBROUTINE hdf5_read_attribute_i0(obj,obj_name,attr_name,attr_val)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN=* ), INTENT( IN ) :: obj_name, attr_name
  INTEGER( I4B ), INTENT( INOUT ) :: attr_val
END SUBROUTINE hdf5_read_attribute_i0
END INTERFACE

!----------------------------------------------------------------------------
!                                                             ReadAttribute
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	9 June 2021
! summary: 	Read attribute from a known dataset

INTERFACE
MODULE SUBROUTINE hdf5_read_attribute_d0(obj,obj_name,attr_name,attr_val)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN=* ), INTENT( IN ) :: obj_name, attr_name
  REAL( DFP ), INTENT( INOUT ) :: attr_val
END SUBROUTINE hdf5_read_attribute_d0
END INTERFACE

!----------------------------------------------------------------------------
!                                                                OpenObject
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	9 June 2021
! summary: 	sets up all attribute operations by checking links and opening object

INTERFACE
MODULE SUBROUTINE open_object(obj,obj_name,obj_id)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN=* ), INTENT( IN ) :: obj_name
  INTEGER( HID_T ), INTENT( OUT ) :: obj_id
END SUBROUTINE open_object
END INTERFACE

!----------------------------------------------------------------------------
!                                                                CloseObject
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	9 Jun 2021
! summary: 	Closes all group, dataset, datatype objects

INTERFACE
MODULE SUBROUTINE close_object(obj,obj_id)
  CHARACTER( LEN=* ), PARAMETER :: myName='close_object_HDF5FileType'
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  INTEGER( HID_T ), INTENT( IN ) :: obj_id
END SUBROUTINE close_object
END INTERFACE

!----------------------------------------------------------------------------
!                                                            CreateAttribute
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE createAttribute(obj,obj_id,attr_name,atype_id,&
  & dspace_id,attr_id)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN=* ), INTENT( IN ) :: attr_name
  INTEGER( HID_T ), INTENT( IN ) :: atype_id, dspace_id, obj_id
  INTEGER( HID_T ), INTENT( OUT ) :: attr_id
END SUBROUTINE createAttribute
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE open_attribute(obj,obj_id,attr_name,attr_id)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  CHARACTER(LEN=*), INTENT( IN ) :: attr_name
  INTEGER( HID_T ), INTENT( IN ) :: obj_id
  INTEGER( HID_T ), INTENT( OUT ) :: attr_id
END SUBROUTINE open_attribute
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE close_attribute(obj,attr_id)
  CLASS( HDF5File_ ), INTENT( INOUT ) :: obj
  INTEGER( HID_T ), INTENT( IN ) :: attr_id
END SUBROUTINE close_attribute
END INTERFACE

END MODULE HDF5File_Class