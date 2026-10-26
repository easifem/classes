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
! date: 2021-05-08
! summary: HDF5 file class
!
! This module defines HDF5File_, which reads and writes HDF5 files.

MODULE HDF5File_Class
USE GlobalData, ONLY: LGT, DFP, I4B
USE GlobalData, ONLY: REAL32, REAL64
USE GlobalData, ONLY: INT8, INT16, INT32, INT64
USE String_Class, ONLY: String
USE BaseType, ONLY: math => TypeMathOpt
USE HDF5, ONLY: HID_T, HSIZE_T, HSSIZE_T
! USE H5LT
! USE H5Fortran
USE ExceptionHandler_Class, ONLY: e
USE AbstractFile_Class, ONLY: AbstractFile_
IMPLICIT NONE

PRIVATE
PUBLIC :: HDF5File_
PUBLIC :: HDF5FilePointer_
PUBLIC :: HDF5ChunkPolicy_
! PUBLIC :: HDF5Open, HDF5Close, HDF5Quiet

INTEGER(I4B), PARAMETER :: MAXSTRLEN = 1024
INTEGER(I4B), SAVE :: nhdf5fileinuse = 0
!! Variable for keeping track of the number of hdf5 files initialized
!! This variable will be used in logic to call the h5close_f(error)
!! which closes the interface.
LOGICAL(LGT), SAVE :: libh5Open = math%no
!! Variable to make sure that the hdf5 interface was opened, and thus
!! can then be closed.

!----------------------------------------------------------------------------
!                                                           HDF5ChunkPolicy_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! summary: Layout and compression settings of the datasets of an HDF5 file
!
! HDF5ChunkPolicy_ controls how Write stores a dataset. It is passed to
! Initiate; the default values are used when it is absent.
!
! A dataset whose data has at least minBytes bytes is chunked and compressed
! when deflateLevel >= 0. Smaller datasets, and all scalars, are stored
! contiguously. The chunk has at most targetBytes bytes: the chunk starts as
! the whole dataset, and the last (slowest) dimension is reduced first, so a
! chunk holds whole columns whenever possible. With chunkMethod 1 (uniform,
! the default) the cut dimension is split into equal parts, so the last chunk
! is (almost) full; with chunkMethod 2 (max) the chunk is as large as
! possible, and the last chunk can be partly empty. When shuffle is true and
! the element has more than one byte, the shuffle filter is applied before
! the deflate filter; it usually improves the compression of numeric data.
!
! Components:
!
! - targetBytes: target size of a chunk in bytes (default 1 MiB)
! - minBytes: datasets with less data are not chunked or compressed
!   (default 64 KiB)
! - stepsPerChunk: number of calls of Append per chunk of an extendable
!   dataset (default 1); the chunk is then grown to at least minBytes and
!   limited to targetBytes, see AppendChunkSize
! - shuffle: apply the shuffle filter before deflate (default true)
! - deflateLevel: gzip compression level [0-9]; -1 means no compression
!   (default 1)
! - chunkMethod: 1 for uniform chunks (default), 2 for the largest chunk
!   that fits targetBytes; see compute_chunk_size
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE HDF5, ONLY: HSIZE_T
! USE HDF5File_Class, ONLY: HDF5ChunkPolicy_
! USE HDF5File_Class, ONLY: HDF5File_
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: filename = "ChunkPolicy_test_1.h5"
! INTEGER(I4B), PARAMETER :: n = 1000
! TYPE(HDF5File_) :: obj
! TYPE(HDF5ChunkPolicy_) :: policy
! REAL(DFP), ALLOCATABLE :: m(:, :), small(:)
! INTEGER(HSIZE_T), ALLOCATABLE :: cdims(:)
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! ALLOCATE (m(n, n), small(10))
! m = 1.0_DFP
! small = 2.0_DFP
!
! ! default policy: deflate level 1, chunks of about 1 MiB
! CALL obj%Initiate(filename=filename, mode="NEW")
! CALL obj%OPEN()
! CALL obj%WRITE(dsetname="/m", vals=m)
! CALL obj%WRITE(dsetname="/small", vals=small)
! CALL OK(obj%IsCompressed(path="/m"), "default: m is compressed")
! CALL OK(.NOT. obj%IsCompressed(path="/small"), &
!         "default: small is not compressed")
! CALL obj%GetChunkSize(path="/m", cdims=cdims)
! CALL OK(ALL(cdims .EQ. [1000_HSIZE_T, 125_HSIZE_T]), &
!         "default: uniform chunks of whole columns")
! CALL obj%DEALLOCATE()
!
! ! largest chunk that fits targetBytes
! policy%chunkMethod = 2
! CALL obj%Initiate(filename=filename, mode="NEW", chunkPolicy=policy)
! CALL obj%OPEN()
! CALL obj%WRITE(dsetname="/m", vals=m)
! CALL obj%GetChunkSize(path="/m", cdims=cdims)
! CALL OK(ALL(cdims .EQ. [1000_HSIZE_T, 131_HSIZE_T]), &
!         "chunkMethod=2: largest chunk")
! CALL obj%DEALLOCATE()
! policy%chunkMethod = 1
!
! ! no compression
! policy%deflateLevel = -1
! CALL obj%Initiate(filename=filename, mode="NEW", chunkPolicy=policy)
! CALL obj%OPEN()
! CALL obj%WRITE(dsetname="/m", vals=m)
! CALL OK(.NOT. obj%IsCompressed(), "deflateLevel=-1: no compression")
! CALL OK(.NOT. obj%IsCompressed(path="/m"), &
!         "deflateLevel=-1: m is not compressed")
! CALL obj%DEALLOCATE()
!
! ! smaller chunks
! policy%deflateLevel = 4
! policy%targetBytes = 80000
! CALL obj%Initiate(filename=filename, mode="NEW", chunkPolicy=policy)
! CALL obj%OPEN()
! CALL obj%WRITE(dsetname="/m", vals=m)
! CALL obj%GetChunkSize(path="/m", cdims=cdims)
! CALL OK(ALL(cdims .EQ. [1000_HSIZE_T, 10_HSIZE_T]), &
!         "targetBytes=80000: chunk 1000 x 10")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[compute_chunk_size]], [[obj_GetChunkSize]],
!   [[obj_IsCompressed]]

TYPE :: HDF5ChunkPolicy_
  INTEGER(INT64) :: targetBytes = 1048576_INT64
  !! target size of a chunk in bytes (about 1 MiB)
  INTEGER(INT64) :: minBytes = 65536_INT64
  !! datasets with less data are not chunked or compressed
  INTEGER(I4B) :: stepsPerChunk = 1
  !! calls of Append per chunk of an extendable dataset
  LOGICAL(LGT) :: shuffle = .TRUE.
  !! apply the shuffle filter before deflate
  INTEGER(I4B) :: deflateLevel = 1
  !! gzip compression level [0-9]; -1 means no compression
  INTEGER(I4B) :: chunkMethod = 1
  !! 1: uniform chunks (equal parts along the cut dimension, least padding)
  !! 2: largest chunk that fits targetBytes (last chunk can be partly empty)
END TYPE HDF5ChunkPolicy_

!----------------------------------------------------------------------------
!                                                                  HDF5File_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-05-08
! summary: HDF5 file
!
! HDF5File_ reads and writes HDF5 binary files. A file is used in four steps:
! Initiate (set the file name and the access mode), Open, the read and write
! calls, and Deallocate (close the file).
!
! The access modes are NEW, READ, WRITE and OVERWRITE; see Initiate. Data is
! stored in datasets which are addressed by a path such as
! "/group/subgroup/data". Groups are created by Mkdir and Mkalldir, and
! missing groups are created by Write.
!
! - Write writes data of rank 0 to 7 of Real64, Real32 and Int32, rank 0 to 3
!   of logical, rank 0 to 3 of String, and character variables.
! - Read reads the same data and allocates the array (logical up to rank 3,
!   String up to rank 3).
! - Read_ reads arrays without allocating them and returns the extent of the
!   data: rank 1 to 7 of Real64, Real32 and Int32, and rank 1 to 3 of logical
!   and String.
! - WriteAttribute and ReadAttribute handle attributes of groups and datasets.
!
! Components:
!
! - isInit: true if the object is initiated
! - hasCompression: true if datasets are written with compression, that is,
!   chunkPolicy%deflateLevel >= 0
! - chunkPolicy: layout and compression settings, see HDF5ChunkPolicy_
! - libverLow: lower bound of the file format version used when the file is
!   created: 110 (HDF5 1.10, default), 18, 112, 114 or 200; 0 uses the
!   default of the HDF5 library (1.8 format in HDF5 2.x)
! - compactBytes: datasets with less data are stored in their object
!   header (compact layout), unless they are chunked (default 4 KiB)
! - newstat: true if Open must create the file
! - fullname: full name of the file
! - unitno: Fortran unit number, used only to delete the file
! - overwriteStat: true if datasets can be overwritten
! - file_id: file id given by the HDF5 library
!
! See also: [[obj_Initiate]], [[obj_Open]], [[obj_Write_d1]], [[obj_Read_d1]],
!   [[obj_Read__d1]]

TYPE, EXTENDS(AbstractFile_) :: HDF5File_
  LOGICAL(LGT) :: isInit = math%no
  !! Initialization status
  LOGICAL(LGT) :: hasCompression = math%no
  !! Whether or not the file uses compression for writing
  TYPE(HDF5ChunkPolicy_), PRIVATE :: chunkPolicy
  !! layout and compression settings of the datasets
  INTEGER(I4B), PRIVATE :: libverLow = 110
  !! lower bound of the file format version of a new file:
  !! 0 (library default), 18, 110 (default), 112, 114 or 200
  INTEGER(INT64), PRIVATE :: compactBytes = 4096_INT64
  !! datasets with less data use the compact layout
  LOGICAL(LGT), PRIVATE :: newstat = math%no
  !! The 'new' status of a file
  TYPE(String) :: fullname
  !! full name of the file
  INTEGER(I4B), PRIVATE :: unitno = math%minus_one_i
  !! unit number of the file
  !TYPE(MPI_EnvType),POINTER  :: pe => NULL()
  LOGICAL(LGT), PRIVATE :: overwriteStat = math%no
  !! When .TRUE., file data can be overwritten
  INTEGER(HID_T) :: file_id = math%zero_i
  !! File id assigned by the HDF5 library when file is opened
CONTAINS
  !@ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: IsInitiated => obj_IsInitiated
  !! Returns true if the object is initiated
  PROCEDURE, PUBLIC, PASS(Obj) :: OPEN => obj_Open
  !! Open the file
  PROCEDURE, PUBLIC, PASS(Obj) :: CLOSE => obj_Close
  !! Close the file
  PROCEDURE, PUBLIC, PASS(Obj) :: Delete => obj_Delete
  !! Delete the file from disk
  PROCEDURE, PUBLIC, PASS(Obj) :: Initiate => obj_Initiate
  !! Initiate the file object
  PROCEDURE, PUBLIC, PASS(Obj) :: DEALLOCATE => obj_Deallocate
  !! Deallocate the file object
  PROCEDURE, PUBLIC, PASS(Obj) :: SetOverwriteStat => &
    obj_SetOverwriteStat
  !! Set whether datasets can be overwritten
  PROCEDURE, PUBLIC, PASS(Obj) :: GetUnitNo => obj_GetUnitNo
  !! Get the unit number
  PROCEDURE, PUBLIC, PASS(Obj) :: IsNew => obj_IsNew
  !! Returns true if the file is new
  PROCEDURE, PUBLIC, PASS(Obj) :: SetNewStat => obj_SetNewStat
  !! Set whether the file is new
  PROCEDURE, PUBLIC, PASS(Obj) :: Ls => obj_Ls
  !! List the contents of a group
  PROCEDURE, PUBLIC, PASS(Obj) :: Mkdir => obj_Mkdir
  !! Create a group
  PROCEDURE, PUBLIC, PASS(Obj) :: Mkalldir => obj_Mkalldir
  !! Create a group and its parents
  PROCEDURE, PUBLIC, PASS(Obj) :: Ngrp => obj_Ngrp
  !! Number of objects in a group
  PROCEDURE, PUBLIC, PASS(Obj) :: IsGroup => obj_IsGroup
  !! Returns true if a path is a group
  PROCEDURE, PUBLIC, PASS(Obj) :: PathExists => obj_PathExists
  !! Returns true if a path exists
  PROCEDURE, PUBLIC, PASS(Obj) :: CreateHardLink => obj_CreateHardLink
  !! Create a hard link
  PROCEDURE, PUBLIC, PASS(Obj) :: GetChunkSize => obj_GetChunkSize
  !! Get the chunk size of a dataset
  PROCEDURE, PUBLIC, PASS(Obj) :: IsCompressed => obj_IsCompressed
  !! Returns true if compression is used

  !@WriteMethods
  PROCEDURE, PRIVATE, PASS(Obj) :: &
    obj_Write_d0, obj_Write_d1, obj_Write_d2, &
    obj_Write_d3, obj_Write_d4, obj_Write_d5, &
    obj_Write_d6, obj_Write_d7, &
    obj_Write_s0, obj_Write_s1, obj_Write_s2, &
    obj_Write_s3, obj_Write_s4, obj_Write_s5, &
    obj_Write_s6, obj_Write_s7, &
    obj_Write_b0, obj_Write_b1, obj_Write_b2, &
    obj_Write_b3, &
    obj_Write_n0, obj_Write_n1, obj_Write_n2, &
    obj_Write_n3, obj_Write_n4, obj_Write_n5, &
    obj_Write_n6, obj_Write_n7, &
    obj_Write_st0, obj_Write_st1, obj_Write_st1_helper, &
    obj_Write_st2, obj_Write_st2_helper, &
    obj_Write_st3, obj_Write_st3_helper, &
    obj_Write_c1
  !! Specific procedures of WRITE
  GENERIC, PUBLIC :: WRITE => &
    obj_Write_d0, obj_Write_d1, obj_Write_d2, &
    obj_Write_d3, obj_Write_d4, obj_Write_d5, &
    obj_Write_d6, obj_Write_d7, &
    obj_Write_s0, obj_Write_s1, obj_Write_s2, &
    obj_Write_s3, obj_Write_s4, obj_Write_s5, &
    obj_Write_s6, obj_Write_s7, &
    obj_Write_b0, obj_Write_b1, obj_Write_b2, &
    obj_Write_b3, &
    obj_Write_n0, obj_Write_n1, obj_Write_n2, &
    obj_Write_n3, obj_Write_n4, obj_Write_n5, &
    obj_Write_n6, obj_Write_n7, &
    obj_Write_st0, obj_Write_st1, obj_Write_st1_helper, &
    obj_Write_st2, obj_Write_st2_helper, &
    obj_Write_st3, obj_Write_st3_helper, &
    obj_Write_c1
  !! Write data to a dataset

  !@ReadMethods
  PROCEDURE, PRIVATE, PASS(Obj) :: &
    obj_Read_d0, obj_Read_d1, obj_Read_d2, &
    obj_Read_d3, obj_Read_d4, obj_Read_d5, &
    obj_Read_d6, obj_Read_d7, &
    obj_Read_s0, obj_Read_s1, obj_Read_s2, &
    obj_Read_s3, obj_Read_s4, obj_Read_s5, &
    obj_Read_s6, obj_Read_s7, &
    obj_Read_n0, obj_Read_n1, obj_Read_n2, &
    obj_Read_n3, obj_Read_n4, obj_Read_n5, &
    obj_Read_n6, obj_Read_n7, &
    obj_Read_st0, obj_Read_st0_helper, &
    obj_Read_st1, obj_Read_st1_helper, &
    obj_Read_st2, obj_Read_st2_helper, &
    obj_Read_st3, obj_Read_st3_helper, &
    obj_Read_c1, obj_Read_b0, obj_Read_b1, &
    obj_Read_b2, obj_Read_b3
  !! Specific procedures of READ
  GENERIC, PUBLIC :: READ => &
    obj_Read_d0, obj_Read_d1, obj_Read_d2, &
    obj_Read_d3, obj_Read_d4, obj_Read_d5, &
    obj_Read_d6, obj_Read_d7, &
    obj_Read_s0, obj_Read_s1, obj_Read_s2, &
    obj_Read_s3, obj_Read_s4, obj_Read_s5, &
    obj_Read_s6, obj_Read_s7, &
    obj_Read_n0, obj_Read_n1, obj_Read_n2, &
    obj_Read_n3, obj_Read_n4, obj_Read_n5, &
    obj_Read_n6, obj_Read_n7, &
    obj_Read_st0, obj_Read_st0_helper, &
    obj_Read_st1, obj_Read_st1_helper, &
    obj_Read_st2, obj_Read_st2_helper, &
    obj_Read_st3, obj_Read_st3_helper, &
    obj_Read_c1, obj_Read_b0, obj_Read_b1, &
    obj_Read_b2, obj_Read_b3
  !! Read data from a dataset
  PROCEDURE, PRIVATE, PASS(Obj) :: &
    obj_Read__d1, obj_Read__d2, obj_Read__d3, &
    obj_Read__d4, obj_Read__d5, obj_Read__d6, &
    obj_Read__d7, &
    obj_Read__s1, obj_Read__s2, obj_Read__s3, &
    obj_Read__s4, obj_Read__s5, obj_Read__s6, &
    obj_Read__s7, &
    obj_Read__n1, obj_Read__n2, obj_Read__n3, &
    obj_Read__n4, obj_Read__n5, obj_Read__n6, &
    obj_Read__n7, &
    obj_Read__b1, obj_Read__b2, obj_Read__b3, &
    obj_Read__st1, obj_Read__st2, obj_Read__st3
  !! Specific procedures of READ_
  GENERIC, PUBLIC :: READ_ => &
    obj_Read__d1, obj_Read__d2, obj_Read__d3, &
    obj_Read__d4, obj_Read__d5, obj_Read__d6, &
    obj_Read__d7, &
    obj_Read__s1, obj_Read__s2, obj_Read__s3, &
    obj_Read__s4, obj_Read__s5, obj_Read__s6, &
    obj_Read__s7, &
    obj_Read__n1, obj_Read__n2, obj_Read__n3, &
    obj_Read__n4, obj_Read__n5, obj_Read__n6, &
    obj_Read__n7, &
    obj_Read__b1, obj_Read__b2, obj_Read__b3, &
    obj_Read__st1, obj_Read__st2, obj_Read__st3
  !! Read data without allocating the array

  !@WriteAttributeMethods
  PROCEDURE, PRIVATE, PASS(Obj) :: &
    obj_WriteAttribute_st0, obj_WriteAttribute_c0, &
    obj_WriteAttribute_i0, obj_WriteAttribute_d0, &
    obj_WriteAttribute_b0, obj_WriteAttribute_i1, &
    obj_WriteAttribute_d1, obj_WriteAttribute_b1, &
    obj_WriteAttribute_st1
  !! Specific procedures of WriteAttribute
  GENERIC, PUBLIC :: WriteAttribute => &
    obj_WriteAttribute_st0, obj_WriteAttribute_c0, &
    obj_WriteAttribute_i0, obj_WriteAttribute_d0, &
    obj_WriteAttribute_b0, obj_WriteAttribute_i1, &
    obj_WriteAttribute_d1, obj_WriteAttribute_b1, &
    obj_WriteAttribute_st1
  !! Write an attribute

  !@ReadAttributeMethods
  PROCEDURE, PRIVATE, PASS(Obj) :: &
    obj_ReadAttribute_st0, obj_ReadAttribute_c0, &
    obj_ReadAttribute_i0, obj_ReadAttribute_d0, &
    obj_ReadAttribute_b0, obj_ReadAttribute_i1, &
    obj_ReadAttribute_d1, obj_ReadAttribute_b1, &
    obj_ReadAttribute_st1
  !! Specific procedures of ReadAttribute
  GENERIC, PUBLIC :: ReadAttribute => &
    obj_ReadAttribute_st0, obj_ReadAttribute_c0, &
    obj_ReadAttribute_i0, obj_ReadAttribute_d0, &
    obj_ReadAttribute_b0, obj_ReadAttribute_i1, &
    obj_ReadAttribute_d1, obj_ReadAttribute_b1, &
    obj_ReadAttribute_st1
  !! Read an attribute

#ifdef USE_Real64
  PROCEDURE, PRIVATE, PASS(Obj) :: &
    obj_WriteAttribute_s0, obj_WriteAttribute_s1, &
    obj_ReadAttribute_s0, obj_ReadAttribute_s1
  !! Real32 attributes (only when DFP is Real64)
  GENERIC, PUBLIC :: WriteAttribute => &
    obj_WriteAttribute_s0, obj_WriteAttribute_s1
  GENERIC, PUBLIC :: ReadAttribute => &
    obj_ReadAttribute_s0, obj_ReadAttribute_s1
#endif

#ifndef USE_Int64
  PROCEDURE, PRIVATE, PASS(Obj) :: &
    obj_WriteAttribute_l0, obj_WriteAttribute_l1, &
    obj_ReadAttribute_l0, obj_ReadAttribute_l1
  !! Int64 attributes (only when I4B is Int32)
  GENERIC, PUBLIC :: WriteAttribute => &
    obj_WriteAttribute_l0, obj_WriteAttribute_l1
  GENERIC, PUBLIC :: ReadAttribute => &
    obj_ReadAttribute_l0, obj_ReadAttribute_l1
#endif

  PROCEDURE, PUBLIC, PASS(Obj) :: GetDataShape => obj_GetDataShape
  !! Get the shape of a dataset
  PROCEDURE, PUBLIC, PASS(Obj) :: GetDataType => obj_GetDataType
  !! Get the data type of a dataset
  PROCEDURE, PUBLIC, PASS(Obj) :: AttributeExists => obj_AttributeExists
  !! Returns true if an attribute exists
  PROCEDURE, PUBLIC, PASS(obj) :: GetExtent => obj_GetExtent
  !! Size of the last dimension of a dataset

  !@AppendMethods
  PROCEDURE, PRIVATE, PASS(obj) :: obj_append_d1, obj_append_d2, &
    obj_append_n1, obj_append_n2, obj_append_u1
  GENERIC, PUBLIC :: Append => obj_append_d1, obj_append_d2, obj_append_n1, &
    obj_append_n2, obj_append_u1
  !! Append values to an extendable dataset along the last dimension

  !@ReadSlabMethods
  PROCEDURE, PRIVATE, PASS(obj) :: &
    obj_ReadSlab_d1, obj_ReadSlab_d2, obj_ReadSlab_n1, &
    obj_ReadSlab_n2, obj_ReadSlab_u1
  GENERIC, PUBLIC :: ReadSlab => &
    obj_readslab_d1, obj_readslab_d2, obj_readslab_n1, &
    obj_readslab_n2, obj_readslab_u1

END TYPE HDF5File_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Pointer to an HDF5 file object
!
! Container of a pointer to HDF5File_, used to store files in arrays.

TYPE :: HDF5FilePointer_
  CLASS(HDF5File_), POINTER :: ptr => NULL()
END TYPE HDF5FilePointer_

!----------------------------------------------------------------------------
!                                              IsInitiated@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-05-08
! summary: Returns true if the HDF5 file object is initiated
!
! The object is initiated by Initiate and reset by Deallocate.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!
! Returns:
!   ans : LOGICAL(LGT) - true if obj is initiated
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE HDF5File_Class, ONLY: HDF5File_
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: filename = "IsInitiated_test_1.h5"
! TYPE(HDF5File_) :: obj
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! CALL OK(.NOT. obj%IsInitiated(), "not initiated")
! CALL obj%Initiate(filename=filename, mode="NEW")
! CALL OK(obj%IsInitiated(), "IsInitiated")
! CALL obj%DEALLOCATE()
! CALL OK(.NOT. obj%IsInitiated(), "not initiated after Deallocate")
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_Deallocate]]

INTERFACE
  MODULE FUNCTION obj_IsInitiated(obj) RESULT(ans)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    LOGICAL(LGT) :: ans
    !! true if obj is initiated
  END FUNCTION obj_IsInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Open
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-05-08
! update:
!   - 2026-10-26: the open status is set only on success
!   - 2026-10-26: a new file uses the format version libverLow
! summary: Open the HDF5 file
!
! Open the file which was set by Initiate. Open does nothing if obj is not
! initiated. The open status (and, in mode NEW, the new status) changes
! only when the file is created or opened; check IsOpen after Open.
!
! - mode NEW: the file is created; an existing file with the same name is
!   truncated. The new status is then set to false, so a second Open (after
!   Close) opens the file without truncating it. The file is created with
!   the format version bounds (libverLow, latest), see Initiate; HDF5 1.10
!   or newer is needed to read it with the default libverLow = 110.
! - mode WRITE or OVERWRITE: the file is opened with read-write access.
! - mode READ: the file is opened with read-only access.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE GlobalData, ONLY: DFP
! USE HDF5File_Class, ONLY: HDF5File_
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: filename = "Open_test_1.h5"
! TYPE(HDF5File_) :: obj
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! CALL obj%Initiate(filename=filename, mode="NEW")
! CALL obj%OPEN()
! CALL OK(obj%IsOpen(), "Open")
! CALL obj%WRITE(dsetname="/x", vals=1.0_DFP)
! CALL obj%CLOSE()
!
! CALL obj%OPEN()
! CALL OK(obj%IsOpen(), "Open again")
! CALL OK(obj%PathExists("/x"), "data is kept after Open again")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_Close]]

INTERFACE
  MODULE SUBROUTINE obj_Open(obj)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
  END SUBROUTINE obj_Open
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Close
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-05-08
! update:
!   - 2026-10-26: the open status is kept when H5Fclose fails
! summary: Close the HDF5 file
!
! Close the file. The object stays initiated, so the file can be opened again
! with Open. Close does nothing if the file is not open.
!
! The file is closed with the close degree H5F_CLOSE_SEMI, so H5Fclose fails
! while a dataset, group or attribute of the file is open. Then the file
! stays open and IsOpen stays true (an error is raised in DEBUG mode).
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE HDF5File_Class, ONLY: HDF5File_
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: filename = "Close_test_1.h5"
! TYPE(HDF5File_) :: obj
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! CALL obj%Initiate(filename=filename, mode="NEW")
! CALL obj%OPEN()
! CALL obj%CLOSE()
! CALL OK(.NOT. obj%IsOpen(), "Close")
! CALL OK(obj%IsInitiated(), "still initiated after Close")
!
! CALL obj%CLOSE()
! CALL OK(.NOT. obj%IsOpen(), "Close a closed file")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Open]], [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE obj_Close(obj)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
  END SUBROUTINE obj_Close
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Delete
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-05-08
! summary: Delete the HDF5 file from disk
!
! Delete the file by opening it with the Fortran OPEN statement on the unit
! number of obj (see GetUnitNo) and closing it with STATUS='DELETE'. Close the
! file before calling Delete.
!
! The object is not deallocated; its open status is set to false. Delete does
! nothing if obj is not initiated.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE GlobalData, ONLY: LGT
! USE HDF5File_Class, ONLY: HDF5File_
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: filename = "Delete_test_1.h5"
! TYPE(HDF5File_) :: obj
! LOGICAL(LGT) :: isok
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! CALL obj%Initiate(filename=filename, mode="NEW")
! CALL obj%OPEN()
! CALL obj%CLOSE()
! INQUIRE (FILE=filename, EXIST=isok)
! CALL OK(isok, "file exists")
!
! CALL obj%Delete()
! INQUIRE (FILE=filename, EXIST=isok)
! CALL OK(.NOT. isok, "Delete")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Deallocate]], [[obj_GetUnitNo]]

INTERFACE
  MODULE SUBROUTINE obj_Delete(obj)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
  END SUBROUTINE obj_Delete
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-05-08
! summary: Initiate an HDF5 file object
!
! Set the file name and the access mode, and initialize the HDF5 library.
! Initiate does not open the file; call Open after Initiate. The directory of
! filename is created if it does not exist.
!
! The access mode is not case sensitive:
!
! - NEW: read and write access. The file is created by Open; an existing file
!   with the same name is truncated. Datasets can be overwritten.
! - READ: read access. The file must exist.
! - WRITE: write access. The file must exist. Existing datasets can not be
!   overwritten unless SetOverwriteStat(.TRUE.) is called.
! - OVERWRITE (or READWRITE): read and write access. The file must exist.
!   Datasets can be overwritten.
!
! When a dataset is overwritten, the new data must have the same shape as the
! old data, since it is written in place (use Append for data that grows).
!
! chunkPolicy sets the layout and compression of the datasets, see
! HDF5ChunkPolicy_. If it is absent, the default values are used: datasets
! with at least 64 KiB of data are chunked (uniform chunks of at most 1 MiB)
! and written with the shuffle filter and gzip level 1. If zlibOpt is present,
! it replaces chunkPolicy%deflateLevel (-1 means no compression); it is kept
! for compatibility.
!
! libverLow is the lower bound of the file format version used when Open
! creates the file (mode NEW): 110 (HDF5 1.10, the default), 18, 112, 114 or
! 200; 0 does not set the bounds, so the default of the HDF5 library is used
! (the 1.8 format in HDF5 2.x, the earliest format in HDF5 1.x). A newer
! format has smaller groups and object headers, and better chunk indexes.
! Existing files (modes READ, WRITE, OVERWRITE) keep their format.
!
! compactBytes: a dataset with less data (default 4 KiB) is stored in its
! object header (compact layout), so it needs no separate data block and is
! read in one access. It must be at most 64000 bytes, since the object
! header is limited to 64 KiB. Chunked datasets (see chunkPolicy) are never
! compact.
!
! In DEBUG mode an error is raised if obj is already initiated, if the mode is
! unknown, or if the file does not exist in mode READ, WRITE or OVERWRITE.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   filename : CHARACTER(*) - path of the file
!   mode : CHARACTER(*) - NEW, READ, WRITE or OVERWRITE
!   zlibOpt : INTEGER(I4B), optional - gzip compression level [0-9];
!       replaces chunkPolicy%deflateLevel
!   chunkPolicy : TYPE(HDF5ChunkPolicy_), optional - layout and compression
!       settings; default values if absent
!   libverLow : INTEGER(I4B), optional - lower bound of the format version
!       of a new file; default 110
!   compactBytes : INTEGER(INT64), optional - limit of the compact layout
!       in bytes; default 4096
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE HDF5File_Class, ONLY: HDF5File_
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: filename = "Initiate_test_1.h5"
! TYPE(HDF5File_) :: obj
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! CALL obj%Initiate(filename=filename, mode="NEW")
! CALL OK(obj%IsInitiated(), "IsInitiated")
! CALL OK(obj%IsNew(), "IsNew")
! CALL OK(obj%IsRead(), "IsRead")
! CALL OK(obj%IsWrite(), "IsWrite")
! CALL OK(.NOT. obj%IsOpen(), "not open before Open")
!
! CALL obj%OPEN()
! CALL OK(obj%IsOpen(), "IsOpen")
! CALL OK(.NOT. obj%IsNew(), "not new after Open")
!
! CALL obj%DEALLOCATE()
! CALL OK(.NOT. obj%IsInitiated(), "Deallocate")
! END PROGRAM main
!```
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE GlobalData, ONLY: DFP
! USE HDF5File_Class, ONLY: HDF5File_
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: filename = "Initiate_test_2.h5"
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! TYPE(HDF5File_) :: obj
! REAL(DFP) :: aval
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! ! create the file
! CALL obj%Initiate(filename=filename, mode="NEW")
! CALL obj%OPEN()
! CALL obj%WRITE(dsetname="/a", vals=1.0_DFP)
! CALL obj%DEALLOCATE()
!
! CALL obj%Initiate(filename=filename, mode="READ")
! CALL OK(obj%IsRead() .AND. (.NOT. obj%IsWrite()), "mode READ")
! CALL obj%OPEN()
! CALL obj%READ(dsetname="/a", vals=aval)
! CALL OK(ABS(aval - 1.0_DFP) .LT. tol, "read in mode READ")
! CALL obj%DEALLOCATE()
!
! CALL obj%Initiate(filename=filename, mode="WRITE")
! CALL OK(obj%IsWrite() .AND. (.NOT. obj%IsRead()), "mode WRITE")
! CALL obj%OPEN()
! CALL obj%WRITE(dsetname="/b", vals=2.0_DFP)
! CALL OK(obj%PathExists("/b"), "write in mode WRITE")
! CALL obj%DEALLOCATE()
!
! CALL obj%Initiate(filename=filename, mode="OVERWRITE")
! CALL OK(obj%IsWrite() .AND. obj%IsRead(), "mode OVERWRITE")
! CALL obj%OPEN()
! CALL obj%WRITE(dsetname="/a", vals=3.0_DFP)
! CALL obj%READ(dsetname="/a", vals=aval)
! CALL OK(ABS(aval - 3.0_DFP) .LT. tol, "overwrite in mode OVERWRITE")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Open]], [[obj_Deallocate]], [[obj_SetOverwriteStat]],
!   [[obj_IsCompressed]]

INTERFACE
  MODULE SUBROUTINE obj_Initiate(Obj, filename, mode, zlibOpt, &
                                 chunkPolicy, libverLow, compactBytes)
    CLASS(HDF5File_), INTENT(INOUT) :: Obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: filename
    !! path of the file on the file system
    CHARACTER(LEN=*), INTENT(IN) :: mode
    !! access mode: 'NEW', 'READ', 'WRITE' or 'OVERWRITE'
    !! 'READ', 'WRITE' and 'OVERWRITE' need an existing file
    !! 'WRITE' does not allow overwriting datasets
    !! 'NEW' creates the file and truncates an existing one
    INTEGER(I4B), INTENT(IN), OPTIONAL :: zlibOpt
    !! gzip compression level [0-9], -1 for no compression;
    !! replaces chunkPolicy%deflateLevel
    TYPE(HDF5ChunkPolicy_), INTENT(IN), OPTIONAL :: chunkPolicy
    !! layout and compression settings; default values if absent
    INTEGER(I4B), INTENT(IN), OPTIONAL :: libverLow
    !! lower bound of the file format version of a new file:
    !! 0 (library default), 18, 110 (default), 112, 114 or 200
    INTEGER(INT64), INTENT(IN), OPTIONAL :: compactBytes
    !! datasets with less data use the compact layout; default 4096
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 clear
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-06
! update:
!   - 2026-10-26: close the file before deleting it
! summary: Deallocate the HDF5 file object
!
! Close the file (or delete it when delete is true) and reset obj to its
! default state. The HDF5 library is closed when no HDF5 file object is
! initiated any more. Deallocate does nothing if obj is not initiated.
!
! The file is always closed first, also when delete is true; then the file
! is deleted from disk.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   delete : LOGICAL(LGT), optional - if true, delete the file; default is
!       false
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE GlobalData, ONLY: LGT
! USE HDF5File_Class, ONLY: HDF5File_
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: filename = "Deallocate_test_1.h5"
! TYPE(HDF5File_) :: obj
! LOGICAL(LGT) :: isok
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! CALL obj%Initiate(filename=filename, mode="NEW")
! CALL obj%OPEN()
! CALL obj%DEALLOCATE()
! CALL OK(.NOT. obj%IsInitiated(), "Deallocate")
! CALL OK(.NOT. obj%IsOpen(), "file is closed")
! INQUIRE (FILE=filename, EXIST=isok)
! CALL OK(isok, "file is kept")
!
! ! the file is still open: Deallocate closes it before deleting it
! CALL obj%Initiate(filename=filename, mode="WRITE")
! CALL obj%OPEN()
! CALL obj%DEALLOCATE(delete=.TRUE.)
! INQUIRE (FILE=filename, EXIST=isok)
! CALL OK(.NOT. isok, "Deallocate with delete")
! END PROGRAM main
!```
!
! See also: [[obj_Close]], [[obj_Delete]]

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj, Delete)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: Delete
    !! if true, the file is deleted from disk; default is false
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                          setOverwriteStat
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-06
! summary: Set whether datasets in the file can be overwritten
!
! When the overwrite status is true, Write overwrites an existing dataset and
! WriteAttribute overwrites an existing attribute; otherwise writing an
! existing dataset gives an error. Initiate sets it to true for modes NEW and
! OVERWRITE, and to false for modes READ and WRITE.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   bool : LOGICAL(LGT) - true to allow overwriting
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE GlobalData, ONLY: DFP
! USE HDF5File_Class, ONLY: HDF5File_
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: filename = "SetOverwriteStat_test_1.h5"
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! TYPE(HDF5File_) :: obj
! REAL(DFP), ALLOCATABLE :: found(:)
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! CALL obj%Initiate(filename=filename, mode="NEW")
! CALL obj%OPEN()
! CALL obj%WRITE(dsetname="/a", vals=[1.0_DFP, 2.0_DFP])
! CALL obj%DEALLOCATE()
!
! CALL obj%Initiate(filename=filename, mode="WRITE")
! CALL obj%SetOverwriteStat(.TRUE.)
! CALL obj%OPEN()
! CALL obj%WRITE(dsetname="/a", vals=[3.0_DFP, 4.0_DFP])
! CALL obj%DEALLOCATE()
!
! CALL obj%Initiate(filename=filename, mode="READ")
! CALL obj%OPEN()
! CALL obj%READ(dsetname="/a", vals=found)
! CALL OK(ALL(ABS(found - [3.0_DFP, 4.0_DFP]) .LT. tol), &
!         "SetOverwriteStat")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]]

INTERFACE
  MODULE SUBROUTINE obj_SetOverwriteStat(obj, bool)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    LOGICAL(LGT), INTENT(IN) :: bool
    !! true to allow overwriting datasets and attributes
  END SUBROUTINE obj_SetOverwriteStat
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 getUnitNo
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-06
! summary: Get the unit number of the HDF5 file
!
! Initiate assigns a free unit number (99 or lower). It is only used by
! Delete, because the HDF5 library does not use Fortran units. The unit number
! is -1 if obj is not initiated.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!
! Returns:
!   ans : INTEGER(I4B) - unit number
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE HDF5File_Class, ONLY: HDF5File_
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: filename = "GetUnitNo_test_1.h5"
! TYPE(HDF5File_) :: obj
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! CALL OK(obj%GetUnitNo() .EQ. -1, "GetUnitNo before Initiate")
! CALL obj%Initiate(filename=filename, mode="NEW")
! CALL OK(obj%GetUnitNo() .GT. 0, "GetUnitNo after Initiate")
! CALL obj%DEALLOCATE()
! CALL OK(obj%GetUnitNo() .EQ. -1, "GetUnitNo after Deallocate")
! END PROGRAM main
!```
!
! See also: [[obj_Delete]]

INTERFACE
  MODULE PURE FUNCTION obj_GetUnitNo(obj) RESULT(ans)
    CLASS(HDF5File_), INTENT(IN) :: obj
    !! HDF5 file object
    INTEGER(I4B) :: ans
    !! unit number, -1 if not initiated
  END FUNCTION obj_GetUnitNo
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 isNew
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-06
! summary: Returns true if the HDF5 file is new
!
! A file initiated with mode NEW is new until Open creates it.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!
! Returns:
!   ans : LOGICAL(LGT) - true if the file is new
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE HDF5File_Class, ONLY: HDF5File_
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: filename = "IsNew_test_1.h5"
! TYPE(HDF5File_) :: obj
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! CALL obj%Initiate(filename=filename, mode="NEW")
! CALL OK(obj%IsNew(), "IsNew after Initiate")
! CALL obj%OPEN()
! CALL OK(.NOT. obj%IsNew(), "IsNew after Open")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_SetNewStat]]

INTERFACE
  MODULE PURE FUNCTION obj_IsNew(obj) RESULT(ans)
    CLASS(HDF5File_), INTENT(IN) :: obj
    !! HDF5 file object
    LOGICAL(LGT) :: ans
    !! true if the file is new
  END FUNCTION obj_IsNew
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 setNewStat
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-06
! summary: Set whether the HDF5 file is new
!
! When the new status is true, Open creates the file and truncates an existing
! file with the same name. Open sets it to false after creating the file.
! Initiate sets it to true for mode NEW.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   bool : LOGICAL(LGT) - new status
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE HDF5File_Class, ONLY: HDF5File_
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: filename = "SetNewStat_test_1.h5"
! TYPE(HDF5File_) :: obj
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! CALL obj%Initiate(filename=filename, mode="NEW")
! CALL obj%SetNewStat(.FALSE.)
! CALL OK(.NOT. obj%IsNew(), "SetNewStat false")
! CALL obj%SetNewStat(.TRUE.)
! CALL OK(obj%IsNew(), "SetNewStat true")
! CALL obj%OPEN()
! CALL OK(obj%IsOpen(), "Open creates the file")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_IsNew]], [[obj_Open]]

INTERFACE
  MODULE SUBROUTINE obj_SetNewStat(obj, bool)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    LOGICAL(LGT), INTENT(IN) :: bool
    !! true if the file is new
  END SUBROUTINE obj_SetNewStat
END INTERFACE

!----------------------------------------------------------------------------
!                                                                       ls
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-05-08
! summary: List the contents of a group
!
! Return the names of the objects (groups and datasets) in the group path, in
! increasing alphabetical order. objs is deallocated first; if path is not a
! group, objs is returned unallocated.
!
! In DEBUG mode an error is raised if obj is not initiated or the file is not
! open.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   path : CHARACTER(*) - path of the group
!   objs : TYPE(String), ALLOCATABLE - names of the objects
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: LGT
! USE HDF5File_Class, ONLY: HDF5File_
! USE String_Class, ONLY: String
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: filename = "Ls_test_1.h5"
! TYPE(HDF5File_) :: obj
! TYPE(String), ALLOCATABLE :: objs(:)
! LOGICAL(LGT) :: isok
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! CALL obj%Initiate(filename=filename, mode="NEW")
! CALL obj%OPEN()
!
! CALL obj%Mkdir("/b")
! CALL obj%Mkdir("/a")
! CALL obj%WRITE(dsetname="/c", vals=1.0_DFP)
!
! CALL obj%Ls(path="/", objs=objs)
! isok = SIZE(objs) .EQ. 3
! IF (isok) THEN
!   isok = (objs(1)%chars() .EQ. "a") .AND. &
!          (objs(2)%chars() .EQ. "b") .AND. &
!          (objs(3)%chars() .EQ. "c")
! END IF
! CALL OK(isok, "Ls('/')")
!
! CALL obj%Ls(path="/c", objs=objs)
! CALL OK(.NOT. ALLOCATED(objs), "Ls of a dataset")
!
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Ngrp]], [[obj_IsGroup]]

INTERFACE
  MODULE SUBROUTINE obj_Ls(obj, path, objs)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: path
    !! path of the group
    TYPE(String), ALLOCATABLE, INTENT(INOUT) :: objs(:)
    !! names of the objects in the group
  END SUBROUTINE obj_Ls
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     mkdir
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-05-08
! summary: Create a group in the HDF5 file
!
! Create the group path. Missing parent groups are created first. If the group
! exists and overwriting is allowed, nothing is done.
!
! In DEBUG mode an error is raised if obj is not initiated, the file is not
! open, or it does not have write access.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   path : CHARACTER(*) - path of the group
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE HDF5File_Class, ONLY: HDF5File_
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: filename = "Mkdir_test_1.h5"
! TYPE(HDF5File_) :: obj
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! CALL obj%Initiate(filename=filename, mode="NEW")
! CALL obj%OPEN()
!
! CALL obj%Mkdir("/grp1")
! CALL OK(obj%IsGroup("/grp1"), "Mkdir /grp1")
!
! CALL obj%Mkdir("/grp2/grp2a")
! CALL OK(obj%IsGroup("/grp2"), "parent group /grp2")
! CALL OK(obj%IsGroup("/grp2/grp2a"), "Mkdir /grp2/grp2a")
! CALL OK(obj%Ngrp("/") .EQ. 2, "Ngrp('/')")
! CALL OK(obj%Ngrp("/grp2") .EQ. 1, "Ngrp('/grp2')")
!
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Mkalldir]], [[obj_IsGroup]]

INTERFACE
  MODULE RECURSIVE SUBROUTINE obj_Mkdir(obj, path)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: path
    !! path of the group to create
  END SUBROUTINE obj_Mkdir
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 mkalldir
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-05-08
! summary: Create a group and all its parent groups
!
! Create every missing group in path. Groups which exist already are not
! changed.
!
! In DEBUG mode an error is raised if obj is not initiated, the file is not
! open, or it does not have write access.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   path : CHARACTER(*) - path of the group
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE HDF5File_Class, ONLY: HDF5File_
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: filename = "Mkalldir_test_1.h5"
! TYPE(HDF5File_) :: obj
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! CALL obj%Initiate(filename=filename, mode="NEW")
! CALL obj%OPEN()
!
! CALL obj%Mkalldir("/grp4/grp4a/grp4a1")
! CALL OK(obj%IsGroup("/grp4"), "/grp4")
! CALL OK(obj%IsGroup("/grp4/grp4a"), "/grp4/grp4a")
! CALL OK(obj%IsGroup("/grp4/grp4a/grp4a1"), "/grp4/grp4a/grp4a1")
!
! ! existing groups are left as they are
! CALL obj%Mkalldir("/grp4/grp4a/grp4a2")
! CALL OK(obj%Ngrp("/grp4/grp4a") .EQ. 2, "Ngrp('/grp4/grp4a')")
!
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Mkdir]]

INTERFACE
  MODULE SUBROUTINE obj_Mkalldir(obj, path)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: path
    !! path of the group to create
  END SUBROUTINE obj_Mkalldir
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 ngrp
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-05-08
! summary: Get the number of objects in a group
!
! Returns the number of objects (groups and datasets) in the group path. In
! DEBUG mode an error is raised if obj is not initiated or the file is not
! open.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   path : CHARACTER(*) - path of the group
!
! Returns:
!   ans : INTEGER(I4B) - number of objects in the group
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE GlobalData, ONLY: DFP
! USE HDF5File_Class, ONLY: HDF5File_
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: filename = "Ngrp_test_1.h5"
! TYPE(HDF5File_) :: obj
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! CALL obj%Initiate(filename=filename, mode="NEW")
! CALL obj%OPEN()
!
! CALL OK(obj%Ngrp("/") .EQ. 0, "Ngrp of an empty file")
! CALL obj%Mkdir("/g1")
! CALL obj%Mkdir("/g2")
! CALL obj%WRITE(dsetname="/d", vals=1.0_DFP)
! CALL obj%WRITE(dsetname="/g1/d", vals=1.0_DFP)
! CALL OK(obj%Ngrp("/") .EQ. 3, "Ngrp('/')")
! CALL OK(obj%Ngrp("/g1") .EQ. 1, "Ngrp('/g1')")
! CALL OK(obj%Ngrp("/g2") .EQ. 0, "Ngrp('/g2')")
!
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Ls]]

INTERFACE
  MODULE FUNCTION obj_Ngrp(obj, path) RESULT(Ans)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: path
    !! path of the group
    INTEGER(I4B) :: Ans
    !! number of objects in the group
  END FUNCTION obj_Ngrp
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 isGroup
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-05-08
! summary: Returns true if a path is a group
!
! Returns true if path exists and is a group. Returns false if path is a
! dataset, does not exist, or the file is not open.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   path : CHARACTER(*) - path in the file
!
! Returns:
!   ans : LOGICAL(LGT) - true if path is a group
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE GlobalData, ONLY: DFP
! USE HDF5File_Class, ONLY: HDF5File_
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: filename = "IsGroup_test_1.h5"
! TYPE(HDF5File_) :: obj
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! CALL obj%Initiate(filename=filename, mode="NEW")
! CALL obj%OPEN()
!
! CALL obj%Mkdir("/grp")
! CALL obj%WRITE(dsetname="/grp/data", vals=1.0_DFP)
! CALL OK(obj%IsGroup("/grp"), "group")
! CALL OK(.NOT. obj%IsGroup("/grp/data"), "dataset is not a group")
! CALL OK(.NOT. obj%IsGroup("/missing"), "missing path")
!
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_PathExists]]

INTERFACE
  MODULE FUNCTION obj_IsGroup(obj, path) RESULT(Ans)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: path
    !! path in the file
    LOGICAL(LGT) :: Ans
    !! true if path is a group
  END FUNCTION obj_IsGroup
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 pathExists
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-05-09
! update:
!   - 2026-10-26: the root group "/" exists; empty segments are skipped
! summary: Returns true if a path exists in the HDF5 file
!
! Every part of path is checked, so a path with a missing parent group does
! not exist. The path can be a group or a dataset. The root group "/" always
! exists, and empty segments (double slashes) are skipped. Returns false if
! the file is not open.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   path : CHARACTER(*) - path in the file
!
! Returns:
!   ans : LOGICAL(LGT) - true if path exists
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE GlobalData, ONLY: DFP
! USE HDF5File_Class, ONLY: HDF5File_
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: filename = "PathExists_test_1.h5"
! TYPE(HDF5File_) :: obj
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! CALL obj%Initiate(filename=filename, mode="NEW")
! CALL obj%OPEN()
!
! CALL obj%WRITE(dsetname="/grp1/grp2/data", vals=1.0_DFP)
! CALL OK(obj%PathExists("/grp1"), "group")
! CALL OK(obj%PathExists("/grp1/grp2/data"), "dataset")
! CALL OK(.NOT. obj%PathExists("/grp1/missing"), "missing")
! CALL OK(.NOT. obj%PathExists("/missing/data"), "missing parent")
!
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_IsGroup]]

INTERFACE
  MODULE FUNCTION obj_PathExists(obj, path) RESULT(Ans)
    CLASS(HDF5File_), INTENT(IN) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: path
    !! path in the file
    LOGICAL(LGT) :: Ans
    !! true if path exists
  END FUNCTION obj_PathExists
END INTERFACE

!----------------------------------------------------------------------------
!                                                             CreateHardLink
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-05-09
! summary: Create a hard link to an object
!
! Create link_path as a second name of the object at source_path. source_path
! must exist and link_path must not exist.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   source_path : CHARACTER(*) - path of the existing object
!   link_path : CHARACTER(*) - path of the new link
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE GlobalData, ONLY: DFP
! USE HDF5File_Class, ONLY: HDF5File_
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: filename = "CreateHardLink_test_1.h5"
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! TYPE(HDF5File_) :: obj
! REAL(DFP) :: aval
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! CALL obj%Initiate(filename=filename, mode="NEW")
! CALL obj%OPEN()
!
! CALL obj%WRITE(dsetname="/data", vals=5.0_DFP)
! CALL obj%CreateHardLink(source_path="/data", link_path="/link")
! CALL OK(obj%PathExists("/link"), "CreateHardLink")
! CALL obj%READ(dsetname="/link", vals=aval)
! CALL OK(ABS(aval - 5.0_DFP) .LT. tol, "read through the link")
!
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_PathExists]]

INTERFACE
  MODULE SUBROUTINE obj_CreateHardLink(obj, source_path, link_path)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: source_path
    !! path of the existing object
    CHARACTER(LEN=*), INTENT(IN) :: link_path
    !! path of the new link
  END SUBROUTINE obj_CreateHardLink
END INTERFACE

!----------------------------------------------------------------------------
!                                                             getChunkSize
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-05-09
! summary: Get the chunk size of a dataset
!
! Only compressed datasets are chunked (see Initiate). If the dataset is not
! chunked or does not exist, cdims is returned unallocated.
!
! In DEBUG mode an error is raised if obj is not initiated or the file is not
! open.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   path : CHARACTER(*) - path of the dataset
!   cdims : INTEGER(HSIZE_T), ALLOCATABLE - chunk dimensions
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: LGT
! USE HDF5, ONLY: HSIZE_T
! USE HDF5File_Class, ONLY: HDF5File_
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: filename = "GetChunkSize_test_1.h5"
! INTEGER, PARAMETER :: n = 200000
! TYPE(HDF5File_) :: obj
! REAL(DFP), ALLOCATABLE :: big(:)
! INTEGER(HSIZE_T), ALLOCATABLE :: cdims(:)
! LOGICAL(LGT) :: isok
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! ALLOCATE (big(n))
! big = 1.0_DFP
!
! CALL obj%Initiate(filename=filename, mode="NEW", zlibOpt=5)
! CALL obj%OPEN()
! CALL obj%WRITE(dsetname="/small", vals=[1.0_DFP, 2.0_DFP])
! CALL obj%WRITE(dsetname="/big", vals=big)
!
! CALL obj%GetChunkSize(path="/small", cdims=cdims)
! CALL OK(.NOT. ALLOCATED(cdims), "small dataset is not chunked")
!
! CALL obj%GetChunkSize(path="/big", cdims=cdims)
! isok = ALLOCATED(cdims)
! IF (isok) isok = (SIZE(cdims) .EQ. 1) .AND. (cdims(1) .LT. n)
! CALL OK(isok, "big dataset is chunked")
!
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_IsCompressed]]

INTERFACE
  MODULE SUBROUTINE obj_GetChunkSize(obj, path, cdims)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: path
    !! path of the dataset
    INTEGER(HSIZE_T), ALLOCATABLE, INTENT(OUT) :: cdims(:)
    !! chunk dimensions; unallocated if the dataset is not chunked
  END SUBROUTINE obj_GetChunkSize
END INTERFACE

!----------------------------------------------------------------------------
!                                                             isCompressed
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Returns true if the file or a dataset uses compression
!
! - If path is absent, returns true if datasets of the file are written
!   with compression, that is, chunkPolicy%deflateLevel >= 0 (the default).
! - If path is present, returns true if the dataset has a deflate, szip or
!   nbit filter. The dataset must exist.
!
! In DEBUG mode an error is raised if obj is not initiated or the file is not
! open.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   path : CHARACTER(*), optional - path of the dataset
!
! Returns:
!   ans : LOGICAL(LGT) - true if compression is used
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE HDF5File_Class, ONLY: HDF5File_
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: filename = "IsCompressed_test_1.h5"
! TYPE(HDF5File_) :: obj
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! CALL obj%Initiate(filename=filename, mode="NEW")
! CALL obj%OPEN()
! CALL OK(obj%IsCompressed(), "default chunk policy")
! CALL obj%DEALLOCATE()
!
! CALL obj%Initiate(filename=filename, mode="NEW", zlibOpt=5)
! CALL obj%OPEN()
! CALL OK(obj%IsCompressed(), "with zlibOpt=5")
! CALL obj%DEALLOCATE()
!
! CALL obj%Initiate(filename=filename, mode="NEW", zlibOpt=-1)
! CALL obj%OPEN()
! CALL OK(.NOT. obj%IsCompressed(), "with zlibOpt=-1")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_GetChunkSize]]

INTERFACE
  MODULE FUNCTION obj_IsCompressed(obj, path) RESULT(Ans)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: path
    !! path of a dataset in the file, which must exist
    LOGICAL(LGT) :: Ans
    !! true if compression is used
  END FUNCTION obj_IsCompressed
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 PreWrite
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! update:
!   - 2026-10-26: error keeps the first failure; ids not created are -1
!   - 2026-10-26: compact layout for datasets below compactBytes
!   - 2026-10-26: an existing dataset must have the shape of the new data
! summary: Prepare a dataset for writing
!
! Create the dataspaces and the dataset creation property list, create the
! missing parent groups of path, and create the dataset (or open it when it
! exists and overwriting is allowed). When the file uses compression and the
! dataset has at least chunkPolicy%minBytes of data, the dataset is chunked
! (see compute_chunk_size), the shuffle filter is set when
! chunkPolicy%shuffle is true and an element has more than one byte, and the
! deflate filter is set. Otherwise a dataset with less than compactBytes of
! data uses the compact layout (data in the object header), and a larger one
! is contiguous. This routine is used by all Write routines; postWrite closes
! what it opens.
!
! An existing dataset is written in place, so it must have the shape of the
! new data (gdims). If the shape differs, the dataset is closed, error is
! -1 and nothing is written; DEBUG mode raises an error that suggests
! Append for data that grows.
!
! error is 0 on success. Otherwise it is the status of the first call that
! failed, and the steps which need the dataset are skipped. The ids which are
! not created are set to -1, so postWrite closes only valid ids.
!
! In DEBUG mode an error is raised if obj is not initiated, the file is not
! open, or it does not have write access. An error is also raised when an
! existing dataset is smaller than the new data.
!
! See also: [[postWrite]], [[compute_chunk_size]]

INTERFACE
  MODULE SUBROUTINE preWrite(obj, rank, gdims, ldims, path, mem, dset_id, &
                             dspace_id, gspace_id, plist_id, error, &
                             cnt, offset)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    INTEGER, INTENT(IN) :: rank
    !! rank of the dataset
    INTEGER(HSIZE_T), INTENT(IN) :: gdims(:)
    !! global dataspace dimensions, i.e. shape
    INTEGER(HSIZE_T), INTENT(IN) :: ldims(:)
    !! local dataspace dimensions
    CHARACTER(*), INTENT(IN) :: path
    !! path of the dataset
    INTEGER(HID_T), INTENT(IN) :: mem
    !! HDF5 memory data type of the data
    INTEGER(HID_T), INTENT(OUT) :: dset_id
    !! dataset id
    INTEGER(HID_T), INTENT(OUT) :: dspace_id
    !! local dataspace id
    INTEGER(HID_T), INTENT(OUT) :: gspace_id
    !! global dataspace id
    INTEGER(HID_T), INTENT(OUT) :: plist_id
    !! property list id of the write operation
    INTEGER(I4B), INTENT(OUT) :: error
    !! error flag, zero on success
    INTEGER(HSIZE_T), INTENT(IN) :: cnt(:)
    !! count; not used at present
    INTEGER(HSSIZE_T), INTENT(IN) :: offset(:)
    !! offset; not used at present
  END SUBROUTINE preWrite
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 ChunkSize
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-08-08
! update:
!   - 2026-10-26: limit the total size of the chunk to targetBytes
!   - 2026-10-26: chunkMethod 1 (uniform) and 2 (max); policy argument
! summary: Compute the chunk size of a compressed dataset
!
! The chunk size controls the memory used by the HDF5 library for compression,
! see https://support.hdfgroup.org/HDF5/doc/Advanced/Chunking/index.html
!
! The chunk starts as the whole dataset. While it has more than
! policy%targetBytes / (size of an element) entries, the last dimension that
! is larger than 1 is reduced: to 1 if the other dimensions already hold the
! target, else to c = target / (product of the other dimensions). So the
! first (fastest) dimension is reduced last, and a chunk holds whole columns
! when a column is smaller than targetBytes. Every chunk dimension is at
! least 1.
!
! HDF5 stores whole chunks, so the last chunk along a dimension is padded
! when the dimension is not a multiple of the chunk size. With
! policy%chunkMethod = 2 (max) the cut dimension is c. With
! policy%chunkMethod = 1 (uniform, the default) the number of chunks along
! the cut dimension is kept, nch = CEILING(n / c), and the chunk size is
! CEILING(n / nch) <= c, so the chunks are equal and the padding is less
! than one entry per chunk.
!
! For example, with targetBytes = 1 MiB and Real64 data (131072 entries):
!
! | Dataset | uniform | max |
! | --- | --- | --- |
! | 1000 x 1000 | 1000 x 125 | 1000 x 131 |
! | 200000 x 200 | 100000 x 1 | 131072 x 1 |
!
! See also: [[preWrite]]

INTERFACE
  MODULE SUBROUTINE compute_chunk_size(mem, gdims, policy, cdims)
    INTEGER(HID_T), INTENT(IN) :: mem
    !! HDF5 data type of the dataset
    INTEGER(HSIZE_T), INTENT(IN) :: gdims(:)
    !! global dimensions of the dataset
    TYPE(HDF5ChunkPolicy_), INTENT(IN) :: policy
    !! chunk policy; targetBytes and chunkMethod are used
    INTEGER(HSIZE_T), INTENT(OUT) :: cdims(:)
    !! chunk dimensions to use
  END SUBROUTINE compute_chunk_size
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 postwrite
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! update:
!   - 2026-10-26: close only valid ids; error is not changed
! summary: Close the objects opened by preWrite
!
! Close the dataset, the dataspaces and the property list; ids equal to -1
! are skipped and the closed ids are set to -1. error keeps the status of the
! write. In DEBUG mode an error is raised if error is not zero, that is, if
! the write failed, or if an object can not be closed.
!
! See also: [[preWrite]]

INTERFACE
  MODULE SUBROUTINE postWrite(obj, error, dset_id, dspace_id, gspace_id, &
                              plist_id)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    INTEGER(I4B), INTENT(INOUT) :: error
    !! error flag of the write operation
    INTEGER(HID_T), INTENT(INOUT) :: dset_id
    !! dataset id
    INTEGER(HID_T), INTENT(INOUT) :: dspace_id
    !! local dataspace id
    INTEGER(HID_T), INTENT(INOUT) :: gspace_id
    !! global dataspace id
    INTEGER(HID_T), INTENT(INOUT) :: plist_id
    !! property list id
  END SUBROUTINE postWrite
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Write
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Write a scalar of Real64
!
! Write a scalar of Real64 to the dataset dsetname. Missing groups in dsetname
! are created. An existing dataset is overwritten when overwriting is allowed
! (see SetOverwriteStat); the new data must have the same shape as the old
! data, since it is written in place (use Append for data that grows).
!
! In DEBUG mode an error is raised if obj is not initiated, the file is not
! open, or it does not have write access.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : REAL(REAL64) - data to write
!   gdims_in, cnt_in, offset_in : INTEGER(I4B), optional - shape, count and
!       offset of the data
!
! See also: [[obj_Write_d1]], [[obj_Read_d0]]

INTERFACE
  MODULE SUBROUTINE obj_Write_d0(obj, dsetname, vals, gdims_in, cnt_in, &
                                 offset_in)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    REAL(REAL64), INTENT(IN) :: vals
    !! a scalar of Real64 to write
    INTEGER(I4B), INTENT(IN), OPTIONAL :: gdims_in(1)
    !! global shape of the dataset, default is the shape of vals
    INTEGER(I4B), INTENT(IN), OPTIONAL :: cnt_in
    !! number of entries to write; not used at present
    INTEGER(I4B), INTENT(IN), OPTIONAL :: offset_in
    !! offset of vals in the dataset; not used at present
  END SUBROUTINE obj_Write_d0
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Write
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Write a rank-1 array of Real64
!
! Write a rank-1 array of Real64 to the dataset dsetname. Missing groups in
! dsetname are created. An existing dataset is overwritten when overwriting is
! allowed (see SetOverwriteStat); the new data must have the same shape as the
! old data, since it is written in place (use Append for data that grows).
!
! In DEBUG mode an error is raised if obj is not initiated, the file is not
! open, or it does not have write access.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : REAL(REAL64) - data to write
!   gdims_in, cnt_in, offset_in : INTEGER(I4B), optional - shape, count and
!       offset of the data
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE GlobalData, ONLY: DFP
! USE HDF5File_Class, ONLY: HDF5File_
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: filename = "Write_test_1.h5"
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! TYPE(HDF5File_) :: obj
! REAL(DFP) :: s, v(5), m(3, 4), t(2, 3, 4)
! REAL(DFP) :: s1
! REAL(DFP), ALLOCATABLE :: v1(:), m1(:, :), t1(:, :, :)
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! s = 2.0_DFP
! CALL RANDOM_NUMBER(v)
! CALL RANDOM_NUMBER(m)
! CALL RANDOM_NUMBER(t)
!
! CALL obj%Initiate(filename=filename, mode="NEW")
! CALL obj%OPEN()
! CALL obj%WRITE(dsetname="/rank0/s", vals=s)
! CALL obj%WRITE(dsetname="/rank1/v", vals=v)
! CALL obj%WRITE(dsetname="/rank2/m", vals=m)
! CALL obj%WRITE(dsetname="/rank3/t", vals=t)
! CALL obj%DEALLOCATE()
!
! CALL obj%Initiate(filename=filename, mode="READ")
! CALL obj%OPEN()
! CALL obj%READ(dsetname="/rank0/s", vals=s1)
! CALL obj%READ(dsetname="/rank1/v", vals=v1)
! CALL obj%READ(dsetname="/rank2/m", vals=m1)
! CALL obj%READ(dsetname="/rank3/t", vals=t1)
! CALL obj%DEALLOCATE()
!
! CALL OK(ABS(s1 - s) .LT. tol, "rank 0")
! CALL OK(ALL(ABS(v1 - v) .LT. tol), "rank 1")
! CALL OK(ALL(ABS(m1 - m) .LT. tol), "rank 2")
! CALL OK(ALL(ABS(t1 - t) .LT. tol), "rank 3")
! END PROGRAM main
!```
!
! See also: [[obj_Read_d1]]

INTERFACE
  MODULE SUBROUTINE obj_Write_d1(obj, dsetname, vals, gdims_in, cnt_in, &
                                 offset_in)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    REAL(REAL64), INTENT(IN) :: vals(:)
    !! a rank-1 array of Real64 to write
    INTEGER(I4B), INTENT(IN), OPTIONAL :: gdims_in(1)
    !! global shape of the dataset, default is the shape of vals
    INTEGER(I4B), INTENT(IN), OPTIONAL :: cnt_in(1)
    !! number of entries to write; not used at present
    INTEGER(I4B), INTENT(IN), OPTIONAL :: offset_in(1)
    !! offset of vals in the dataset; not used at present
  END SUBROUTINE obj_Write_d1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Write
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Write a rank-2 array of Real64
!
! Write a rank-2 array of Real64 to the dataset dsetname. Missing groups in
! dsetname are created. An existing dataset is overwritten when overwriting is
! allowed (see SetOverwriteStat); the new data must have the same shape as the
! old data, since it is written in place (use Append for data that grows).
!
! In DEBUG mode an error is raised if obj is not initiated, the file is not
! open, or it does not have write access.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : REAL(REAL64) - data to write
!   gdims_in, cnt_in, offset_in : INTEGER(I4B), optional - shape, count and
!       offset of the data
!
! See also: [[obj_Write_d1]], [[obj_Read_d2]]

INTERFACE
  MODULE SUBROUTINE obj_Write_d2(obj, dsetname, vals, gdims_in, cnt_in, &
                                 offset_in)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    REAL(REAL64), INTENT(IN) :: vals(:, :)
    !! a rank-2 array of Real64 to write
    INTEGER(I4B), INTENT(IN), OPTIONAL :: gdims_in(2)
    !! global shape of the dataset, default is the shape of vals
    INTEGER(I4B), INTENT(IN), OPTIONAL :: cnt_in(2)
    !! number of entries to write; not used at present
    INTEGER(I4B), INTENT(IN), OPTIONAL :: offset_in(2)
    !! offset of vals in the dataset; not used at present
  END SUBROUTINE obj_Write_d2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Write
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Write a rank-3 array of Real64
!
! Write a rank-3 array of Real64 to the dataset dsetname. Missing groups in
! dsetname are created. An existing dataset is overwritten when overwriting is
! allowed (see SetOverwriteStat); the new data must have the same shape as the
! old data, since it is written in place (use Append for data that grows).
!
! In DEBUG mode an error is raised if obj is not initiated, the file is not
! open, or it does not have write access.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : REAL(REAL64) - data to write
!   gdims_in, cnt_in, offset_in : INTEGER(I4B), optional - shape, count and
!       offset of the data
!
! See also: [[obj_Write_d1]], [[obj_Read_d3]]

INTERFACE
  MODULE SUBROUTINE obj_Write_d3(obj, dsetname, vals, gdims_in, cnt_in, &
                                 offset_in)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    REAL(REAL64), INTENT(IN) :: vals(:, :, :)
    !! a rank-3 array of Real64 to write
    INTEGER(I4B), INTENT(IN), OPTIONAL :: gdims_in(3)
    !! global shape of the dataset, default is the shape of vals
    INTEGER(I4B), INTENT(IN), OPTIONAL :: cnt_in(3)
    !! number of entries to write; not used at present
    INTEGER(I4B), INTENT(IN), OPTIONAL :: offset_in(3)
    !! offset of vals in the dataset; not used at present
  END SUBROUTINE obj_Write_d3
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Write
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Write a rank-4 array of Real64
!
! Write a rank-4 array of Real64 to the dataset dsetname. Missing groups in
! dsetname are created. An existing dataset is overwritten when overwriting is
! allowed (see SetOverwriteStat); the new data must have the same shape as the
! old data, since it is written in place (use Append for data that grows).
!
! In DEBUG mode an error is raised if obj is not initiated, the file is not
! open, or it does not have write access.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : REAL(REAL64) - data to write
!   gdims_in, cnt_in, offset_in : INTEGER(I4B), optional - shape, count and
!       offset of the data
!
! See also: [[obj_Write_d1]], [[obj_Read_d4]]

INTERFACE
  MODULE SUBROUTINE obj_Write_d4(obj, dsetname, vals, gdims_in, cnt_in, &
                                 offset_in)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    REAL(REAL64), INTENT(IN) :: vals(:, :, :, :)
    !! a rank-4 array of Real64 to write
    INTEGER(I4B), INTENT(IN), OPTIONAL :: gdims_in(4)
    !! global shape of the dataset, default is the shape of vals
    INTEGER(I4B), INTENT(IN), OPTIONAL :: cnt_in(4)
    !! number of entries to write; not used at present
    INTEGER(I4B), INTENT(IN), OPTIONAL :: offset_in(4)
    !! offset of vals in the dataset; not used at present
  END SUBROUTINE obj_Write_d4
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Write
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Write a rank-5 array of Real64
!
! Write a rank-5 array of Real64 to the dataset dsetname. Missing groups in
! dsetname are created. An existing dataset is overwritten when overwriting is
! allowed (see SetOverwriteStat); the new data must have the same shape as the
! old data, since it is written in place (use Append for data that grows).
!
! In DEBUG mode an error is raised if obj is not initiated, the file is not
! open, or it does not have write access.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : REAL(REAL64) - data to write
!   gdims_in, cnt_in, offset_in : INTEGER(I4B), optional - shape, count and
!       offset of the data
!
! See also: [[obj_Write_d1]], [[obj_Read_d5]]

INTERFACE
  MODULE SUBROUTINE obj_Write_d5(obj, dsetname, vals, gdims_in, cnt_in, &
                                 offset_in)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    REAL(REAL64), INTENT(IN) :: vals(:, :, :, :, :)
    !! a rank-5 array of Real64 to write
    INTEGER(I4B), INTENT(IN), OPTIONAL :: gdims_in(5)
    !! global shape of the dataset, default is the shape of vals
    INTEGER(I4B), INTENT(IN), OPTIONAL :: cnt_in(5)
    !! number of entries to write; not used at present
    INTEGER(I4B), INTENT(IN), OPTIONAL :: offset_in(5)
    !! offset of vals in the dataset; not used at present
  END SUBROUTINE obj_Write_d5
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Write
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Write a rank-6 array of Real64
!
! Write a rank-6 array of Real64 to the dataset dsetname. Missing groups in
! dsetname are created. An existing dataset is overwritten when overwriting is
! allowed (see SetOverwriteStat); the new data must have the same shape as the
! old data, since it is written in place (use Append for data that grows).
!
! In DEBUG mode an error is raised if obj is not initiated, the file is not
! open, or it does not have write access.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : REAL(REAL64) - data to write
!   gdims_in, cnt_in, offset_in : INTEGER(I4B), optional - shape, count and
!       offset of the data
!
! See also: [[obj_Write_d1]], [[obj_Read_d6]]

INTERFACE
  MODULE SUBROUTINE obj_Write_d6(obj, dsetname, vals, gdims_in, cnt_in, &
                                 offset_in)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    REAL(REAL64), INTENT(IN) :: vals(:, :, :, :, :, :)
    !! a rank-6 array of Real64 to write
    INTEGER(I4B), INTENT(IN), OPTIONAL :: gdims_in(6)
    !! global shape of the dataset, default is the shape of vals
    INTEGER(I4B), INTENT(IN), OPTIONAL :: cnt_in(6)
    !! number of entries to write; not used at present
    INTEGER(I4B), INTENT(IN), OPTIONAL :: offset_in(6)
    !! offset of vals in the dataset; not used at present
  END SUBROUTINE obj_Write_d6
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Write
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Write a rank-7 array of Real64
!
! Write a rank-7 array of Real64 to the dataset dsetname. Missing groups in
! dsetname are created. An existing dataset is overwritten when overwriting is
! allowed (see SetOverwriteStat); the new data must have the same shape as the
! old data, since it is written in place (use Append for data that grows).
!
! In DEBUG mode an error is raised if obj is not initiated, the file is not
! open, or it does not have write access.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : REAL(REAL64) - data to write
!   gdims_in, cnt_in, offset_in : INTEGER(I4B), optional - shape, count and
!       offset of the data
!
! See also: [[obj_Write_d1]], [[obj_Read_d7]]

INTERFACE
  MODULE SUBROUTINE obj_Write_d7(obj, dsetname, vals, gdims_in, cnt_in, &
                                 offset_in)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    REAL(REAL64), INTENT(IN) :: vals(:, :, :, :, :, :, :)
    !! a rank-7 array of Real64 to write
    INTEGER(I4B), INTENT(IN), OPTIONAL :: gdims_in(7)
    !! global shape of the dataset, default is the shape of vals
    INTEGER(I4B), INTENT(IN), OPTIONAL :: cnt_in(7)
    !! number of entries to write; not used at present
    INTEGER(I4B), INTENT(IN), OPTIONAL :: offset_in(7)
    !! offset of vals in the dataset; not used at present
  END SUBROUTINE obj_Write_d7
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Write
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Write a scalar of Real32
!
! Write a scalar of Real32 to the dataset dsetname. Missing groups in dsetname
! are created. An existing dataset is overwritten when overwriting is allowed
! (see SetOverwriteStat); the new data must have the same shape as the old
! data, since it is written in place (use Append for data that grows).
!
! In DEBUG mode an error is raised if obj is not initiated, the file is not
! open, or it does not have write access.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : REAL(REAL32) - data to write
!   gdims_in, cnt_in, offset_in : INTEGER(I4B), optional - shape, count and
!       offset of the data
!
! See also: [[obj_Write_s1]], [[obj_Read_s0]]

INTERFACE
  MODULE SUBROUTINE obj_Write_s0(obj, dsetname, vals, gdims_in, cnt_in, &
                                 offset_in)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    REAL(REAL32), INTENT(IN) :: vals
    !! a scalar of Real32 to write
    INTEGER(I4B), INTENT(IN), OPTIONAL :: gdims_in(1)
    !! global shape of the dataset, default is the shape of vals
    INTEGER(I4B), INTENT(IN), OPTIONAL :: cnt_in
    !! number of entries to write; not used at present
    INTEGER(I4B), INTENT(IN), OPTIONAL :: offset_in
    !! offset of vals in the dataset; not used at present
  END SUBROUTINE obj_Write_s0
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Write
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Write a rank-1 array of Real32
!
! Write a rank-1 array of Real32 to the dataset dsetname. Missing groups in
! dsetname are created. An existing dataset is overwritten when overwriting is
! allowed (see SetOverwriteStat); the new data must have the same shape as the
! old data, since it is written in place (use Append for data that grows).
!
! In DEBUG mode an error is raised if obj is not initiated, the file is not
! open, or it does not have write access.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : REAL(REAL32) - data to write
!   gdims_in, cnt_in, offset_in : INTEGER(I4B), optional - shape, count and
!       offset of the data
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE GlobalData, ONLY: REAL32
! USE HDF5File_Class, ONLY: HDF5File_
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: filename = "Write_test_2.h5"
! REAL(REAL32), PARAMETER :: tol = 1.0E-6_REAL32
! TYPE(HDF5File_) :: obj
! REAL(REAL32) :: s, v(5), m(3, 4)
! REAL(REAL32) :: s1
! REAL(REAL32), ALLOCATABLE :: v1(:), m1(:, :)
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! s = 2.0_REAL32
! CALL RANDOM_NUMBER(v)
! CALL RANDOM_NUMBER(m)
!
! CALL obj%Initiate(filename=filename, mode="NEW")
! CALL obj%OPEN()
! CALL obj%WRITE(dsetname="/s", vals=s)
! CALL obj%WRITE(dsetname="/v", vals=v)
! CALL obj%WRITE(dsetname="/m", vals=m)
! CALL obj%DEALLOCATE()
!
! CALL obj%Initiate(filename=filename, mode="READ")
! CALL obj%OPEN()
! CALL obj%READ(dsetname="/s", vals=s1)
! CALL obj%READ(dsetname="/v", vals=v1)
! CALL obj%READ(dsetname="/m", vals=m1)
! CALL obj%DEALLOCATE()
!
! CALL OK(ABS(s1 - s) .LT. tol, "rank 0")
! CALL OK(ALL(ABS(v1 - v) .LT. tol), "rank 1")
! CALL OK(ALL(ABS(m1 - m) .LT. tol), "rank 2")
! END PROGRAM main
!```
!
! See also: [[obj_Read_s1]]

INTERFACE
  MODULE SUBROUTINE obj_Write_s1(obj, dsetname, vals, gdims_in, cnt_in, &
                                 offset_in)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    REAL(REAL32), INTENT(IN) :: vals(:)
    !! a rank-1 array of Real32 to write
    INTEGER(I4B), INTENT(IN), OPTIONAL :: gdims_in(1)
    !! global shape of the dataset, default is the shape of vals
    INTEGER(I4B), INTENT(IN), OPTIONAL :: cnt_in(1)
    !! number of entries to write; not used at present
    INTEGER(I4B), INTENT(IN), OPTIONAL :: offset_in(1)
    !! offset of vals in the dataset; not used at present
  END SUBROUTINE obj_Write_s1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Write
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Write a rank-2 array of Real32
!
! Write a rank-2 array of Real32 to the dataset dsetname. Missing groups in
! dsetname are created. An existing dataset is overwritten when overwriting is
! allowed (see SetOverwriteStat); the new data must have the same shape as the
! old data, since it is written in place (use Append for data that grows).
!
! In DEBUG mode an error is raised if obj is not initiated, the file is not
! open, or it does not have write access.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : REAL(REAL32) - data to write
!   gdims_in, cnt_in, offset_in : INTEGER(I4B), optional - shape, count and
!       offset of the data
!
! See also: [[obj_Write_s1]], [[obj_Read_s2]]

INTERFACE
  MODULE SUBROUTINE obj_Write_s2(obj, dsetname, vals, gdims_in, cnt_in, &
                                 offset_in)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    REAL(REAL32), INTENT(IN) :: vals(:, :)
    !! a rank-2 array of Real32 to write
    INTEGER(I4B), INTENT(IN), OPTIONAL :: gdims_in(2)
    !! global shape of the dataset, default is the shape of vals
    INTEGER(I4B), INTENT(IN), OPTIONAL :: cnt_in(2)
    !! number of entries to write; not used at present
    INTEGER(I4B), INTENT(IN), OPTIONAL :: offset_in(2)
    !! offset of vals in the dataset; not used at present
  END SUBROUTINE obj_Write_s2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Write
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Write a rank-3 array of Real32
!
! Write a rank-3 array of Real32 to the dataset dsetname. Missing groups in
! dsetname are created. An existing dataset is overwritten when overwriting is
! allowed (see SetOverwriteStat); the new data must have the same shape as the
! old data, since it is written in place (use Append for data that grows).
!
! In DEBUG mode an error is raised if obj is not initiated, the file is not
! open, or it does not have write access.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : REAL(REAL32) - data to write
!   gdims_in, cnt_in, offset_in : INTEGER(I4B), optional - shape, count and
!       offset of the data
!
! See also: [[obj_Write_s1]], [[obj_Read_s3]]

INTERFACE
  MODULE SUBROUTINE obj_Write_s3(obj, dsetname, vals, gdims_in, cnt_in, &
                                 offset_in)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    REAL(REAL32), INTENT(IN) :: vals(:, :, :)
    !! a rank-3 array of Real32 to write
    INTEGER(I4B), INTENT(IN), OPTIONAL :: gdims_in(3)
    !! global shape of the dataset, default is the shape of vals
    INTEGER(I4B), INTENT(IN), OPTIONAL :: cnt_in(3)
    !! number of entries to write; not used at present
    INTEGER(I4B), INTENT(IN), OPTIONAL :: offset_in(3)
    !! offset of vals in the dataset; not used at present
  END SUBROUTINE obj_Write_s3
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Write
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Write a rank-4 array of Real32
!
! Write a rank-4 array of Real32 to the dataset dsetname. Missing groups in
! dsetname are created. An existing dataset is overwritten when overwriting is
! allowed (see SetOverwriteStat); the new data must have the same shape as the
! old data, since it is written in place (use Append for data that grows).
!
! In DEBUG mode an error is raised if obj is not initiated, the file is not
! open, or it does not have write access.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : REAL(REAL32) - data to write
!   gdims_in, cnt_in, offset_in : INTEGER(I4B), optional - shape, count and
!       offset of the data
!
! See also: [[obj_Write_s1]], [[obj_Read_s4]]

INTERFACE
  MODULE SUBROUTINE obj_Write_s4(obj, dsetname, vals, gdims_in, cnt_in, &
                                 offset_in)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    REAL(REAL32), INTENT(IN) :: vals(:, :, :, :)
    !! a rank-4 array of Real32 to write
    INTEGER(I4B), INTENT(IN), OPTIONAL :: gdims_in(4)
    !! global shape of the dataset, default is the shape of vals
    INTEGER(I4B), INTENT(IN), OPTIONAL :: cnt_in(4)
    !! number of entries to write; not used at present
    INTEGER(I4B), INTENT(IN), OPTIONAL :: offset_in(4)
    !! offset of vals in the dataset; not used at present
  END SUBROUTINE obj_Write_s4
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Write
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Write a rank-5 array of Real32
!
! Write a rank-5 array of Real32 to the dataset dsetname. Missing groups in
! dsetname are created. An existing dataset is overwritten when overwriting is
! allowed (see SetOverwriteStat); the new data must have the same shape as the
! old data, since it is written in place (use Append for data that grows).
!
! In DEBUG mode an error is raised if obj is not initiated, the file is not
! open, or it does not have write access.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : REAL(REAL32) - data to write
!   gdims_in, cnt_in, offset_in : INTEGER(I4B), optional - shape, count and
!       offset of the data
!
! See also: [[obj_Write_s1]], [[obj_Read_s5]]

INTERFACE
  MODULE SUBROUTINE obj_Write_s5(obj, dsetname, vals, gdims_in, cnt_in, &
                                 offset_in)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    REAL(REAL32), INTENT(IN) :: vals(:, :, :, :, :)
    !! a rank-5 array of Real32 to write
    INTEGER(I4B), INTENT(IN), OPTIONAL :: gdims_in(5)
    !! global shape of the dataset, default is the shape of vals
    INTEGER(I4B), INTENT(IN), OPTIONAL :: cnt_in(5)
    !! number of entries to write; not used at present
    INTEGER(I4B), INTENT(IN), OPTIONAL :: offset_in(5)
    !! offset of vals in the dataset; not used at present
  END SUBROUTINE obj_Write_s5
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Write
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Write a rank-6 array of Real32
!
! Write a rank-6 array of Real32 to the dataset dsetname. Missing groups in
! dsetname are created. An existing dataset is overwritten when overwriting is
! allowed (see SetOverwriteStat); the new data must have the same shape as the
! old data, since it is written in place (use Append for data that grows).
!
! In DEBUG mode an error is raised if obj is not initiated, the file is not
! open, or it does not have write access.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : REAL(REAL32) - data to write
!   gdims_in, cnt_in, offset_in : INTEGER(I4B), optional - shape, count and
!       offset of the data
!
! See also: [[obj_Write_s1]], [[obj_Read_s6]]

INTERFACE
  MODULE SUBROUTINE obj_Write_s6(obj, dsetname, vals, gdims_in, cnt_in, &
                                 offset_in)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    REAL(REAL32), INTENT(IN) :: vals(:, :, :, :, :, :)
    !! a rank-6 array of Real32 to write
    INTEGER(I4B), INTENT(IN), OPTIONAL :: gdims_in(6)
    !! global shape of the dataset, default is the shape of vals
    INTEGER(I4B), INTENT(IN), OPTIONAL :: cnt_in(6)
    !! number of entries to write; not used at present
    INTEGER(I4B), INTENT(IN), OPTIONAL :: offset_in(6)
    !! offset of vals in the dataset; not used at present
  END SUBROUTINE obj_Write_s6
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Write
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Write a rank-7 array of Real32
!
! Write a rank-7 array of Real32 to the dataset dsetname. Missing groups in
! dsetname are created. An existing dataset is overwritten when overwriting is
! allowed (see SetOverwriteStat); the new data must have the same shape as the
! old data, since it is written in place (use Append for data that grows).
!
! In DEBUG mode an error is raised if obj is not initiated, the file is not
! open, or it does not have write access.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : REAL(REAL32) - data to write
!   gdims_in, cnt_in, offset_in : INTEGER(I4B), optional - shape, count and
!       offset of the data
!
! See also: [[obj_Write_s1]], [[obj_Read_s7]]

INTERFACE
  MODULE SUBROUTINE obj_Write_s7(obj, dsetname, vals, gdims_in, cnt_in, &
                                 offset_in)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    REAL(REAL32), INTENT(IN) :: vals(:, :, :, :, :, :, :)
    !! a rank-7 array of Real32 to write
    INTEGER(I4B), INTENT(IN), OPTIONAL :: gdims_in(7)
    !! global shape of the dataset, default is the shape of vals
    INTEGER(I4B), INTENT(IN), OPTIONAL :: cnt_in(7)
    !! number of entries to write; not used at present
    INTEGER(I4B), INTENT(IN), OPTIONAL :: offset_in(7)
    !! offset of vals in the dataset; not used at present
  END SUBROUTINE obj_Write_s7
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Write
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Write a scalar of logical
!
! Write a scalar of logical to the dataset dsetname. Missing groups in
! dsetname are created. An existing dataset is overwritten when overwriting is
! allowed (see SetOverwriteStat); the new data must have the same shape as the
! old data, since it is written in place (use Append for data that grows).
! Logical values are stored as the characters 'T' and 'F'.
!
! In DEBUG mode an error is raised if obj is not initiated, the file is not
! open, or it does not have write access.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : LOGICAL(LGT) - data to write
!   gdims_in, cnt_in, offset_in : INTEGER(I4B), optional - shape, count and
!       offset of the data
!
! See also: [[obj_Write_b1]], [[obj_Read_b0]]

INTERFACE
  MODULE SUBROUTINE obj_Write_b0(obj, dsetname, vals, gdims_in, &
                                 cnt_in, offset_in)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    LOGICAL(LGT), INTENT(IN) :: vals
    !! a scalar of logical to write
    INTEGER(I4B), INTENT(IN), OPTIONAL :: gdims_in(1)
    !! global shape of the dataset, default is the shape of vals
    INTEGER(I4B), INTENT(IN), OPTIONAL :: cnt_in
    !! number of entries to write; not used at present
    INTEGER(I4B), INTENT(IN), OPTIONAL :: offset_in
    !! offset of vals in the dataset; not used at present
  END SUBROUTINE obj_Write_b0
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Write
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Write a rank-1 array of logical
!
! Write a rank-1 array of logical to the dataset dsetname. Missing groups in
! dsetname are created. An existing dataset is overwritten when overwriting is
! allowed (see SetOverwriteStat); the new data must have the same shape as the
! old data, since it is written in place (use Append for data that grows).
! Logical values are stored as the characters 'T' and 'F'.
!
! In DEBUG mode an error is raised if obj is not initiated, the file is not
! open, or it does not have write access.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : LOGICAL(LGT) - data to write
!   gdims_in, cnt_in, offset_in : INTEGER(I4B), optional - shape, count and
!       offset of the data
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE GlobalData, ONLY: LGT
! USE HDF5File_Class, ONLY: HDF5File_
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: filename = "Write_test_4.h5"
! TYPE(HDF5File_) :: obj
! LOGICAL(LGT) :: s, v(3), m(2, 2)
! LOGICAL(LGT) :: s1
! LOGICAL(LGT), ALLOCATABLE :: v1(:), m1(:, :)
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! s = .TRUE.
! v = [.TRUE., .FALSE., .TRUE.]
! m = RESHAPE([.TRUE., .FALSE., .FALSE., .TRUE.], [2, 2])
!
! CALL obj%Initiate(filename=filename, mode="NEW")
! CALL obj%OPEN()
! CALL obj%WRITE(dsetname="/s", vals=s)
! CALL obj%WRITE(dsetname="/v", vals=v)
! CALL obj%WRITE(dsetname="/m", vals=m)
! CALL obj%DEALLOCATE()
!
! CALL obj%Initiate(filename=filename, mode="READ")
! CALL obj%OPEN()
! CALL obj%READ(dsetname="/s", vals=s1)
! CALL obj%READ(dsetname="/v", vals=v1)
! CALL obj%READ(dsetname="/m", vals=m1)
! CALL obj%DEALLOCATE()
!
! CALL OK(s1 .EQV. s, "rank 0")
! CALL OK(ALL(v1 .EQV. v), "rank 1")
! CALL OK(ALL(m1 .EQV. m), "rank 2")
! END PROGRAM main
!```
!
! See also: [[obj_Read_b1]]

INTERFACE
  MODULE SUBROUTINE obj_Write_b1(obj, dsetname, vals, gdims_in, &
                                 cnt_in, offset_in)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    LOGICAL(LGT), INTENT(IN) :: vals(:)
    !! a rank-1 array of logical to write
    INTEGER(I4B), INTENT(IN), OPTIONAL :: gdims_in(1)
    !! global shape of the dataset, default is the shape of vals
    INTEGER(I4B), INTENT(IN), OPTIONAL :: cnt_in(1)
    !! number of entries to write; not used at present
    INTEGER(I4B), INTENT(IN), OPTIONAL :: offset_in(1)
    !! offset of vals in the dataset; not used at present
  END SUBROUTINE obj_Write_b1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Write
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Write a rank-2 array of logical
!
! Write a rank-2 array of logical to the dataset dsetname. Missing groups in
! dsetname are created. An existing dataset is overwritten when overwriting is
! allowed (see SetOverwriteStat); the new data must have the same shape as the
! old data, since it is written in place (use Append for data that grows).
! Logical values are stored as the characters 'T' and 'F'.
!
! In DEBUG mode an error is raised if obj is not initiated, the file is not
! open, or it does not have write access.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : LOGICAL(LGT) - data to write
!   gdims_in, cnt_in, offset_in : INTEGER(I4B), optional - shape, count and
!       offset of the data
!
! See also: [[obj_Write_b1]], [[obj_Read_b2]]

INTERFACE
  MODULE SUBROUTINE obj_Write_b2(obj, dsetname, vals, gdims_in, &
                                 cnt_in, offset_in)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    LOGICAL(LGT), INTENT(IN) :: vals(:, :)
    !! a rank-2 array of logical to write
    INTEGER(I4B), INTENT(IN), OPTIONAL :: gdims_in(2)
    !! global shape of the dataset, default is the shape of vals
    INTEGER(I4B), INTENT(IN), OPTIONAL :: cnt_in(2)
    !! number of entries to write; not used at present
    INTEGER(I4B), INTENT(IN), OPTIONAL :: offset_in(2)
    !! offset of vals in the dataset; not used at present
  END SUBROUTINE obj_Write_b2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Write
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Write a rank-3 array of logical
!
! Write a rank-3 array of logical to the dataset dsetname. Missing groups in
! dsetname are created. An existing dataset is overwritten when overwriting is
! allowed (see SetOverwriteStat); the new data must have the same shape as the
! old data, since it is written in place (use Append for data that grows).
! Logical values are stored as the characters 'T' and 'F'.
!
! In DEBUG mode an error is raised if obj is not initiated, the file is not
! open, or it does not have write access.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : LOGICAL(LGT) - data to write
!   gdims_in, cnt_in, offset_in : INTEGER(I4B), optional - shape, count and
!       offset of the data
!
! See also: [[obj_Write_b1]], [[obj_Read_b3]]

INTERFACE
  MODULE SUBROUTINE obj_Write_b3(obj, dsetname, vals, gdims_in, &
                                 cnt_in, offset_in)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    LOGICAL(LGT), INTENT(IN) :: vals(:, :, :)
    !! a rank-3 array of logical to write
    INTEGER(I4B), INTENT(IN), OPTIONAL :: gdims_in(3)
    !! global shape of the dataset, default is the shape of vals
    INTEGER(I4B), INTENT(IN), OPTIONAL :: cnt_in(3)
    !! number of entries to write; not used at present
    INTEGER(I4B), INTENT(IN), OPTIONAL :: offset_in(3)
    !! offset of vals in the dataset; not used at present
  END SUBROUTINE obj_Write_b3
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Write
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Write a scalar of Int32
!
! Write a scalar of Int32 to the dataset dsetname. Missing groups in dsetname
! are created. An existing dataset is overwritten when overwriting is allowed
! (see SetOverwriteStat); the new data must have the same shape as the old
! data, since it is written in place (use Append for data that grows).
!
! In DEBUG mode an error is raised if obj is not initiated, the file is not
! open, or it does not have write access.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : INTEGER(INT32) - data to write
!   gdims_in, cnt_in, offset_in : INTEGER(I4B), optional - shape, count and
!       offset of the data
!
! See also: [[obj_Write_n1]], [[obj_Read_n0]]

INTERFACE
  MODULE SUBROUTINE obj_Write_n0(obj, dsetname, vals, gdims_in, &
                                 cnt_in, offset_in)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    INTEGER(INT32), INTENT(IN) :: vals
    !! a scalar of Int32 to write
    INTEGER(I4B), INTENT(IN), OPTIONAL :: gdims_in(1)
    !! global shape of the dataset, default is the shape of vals
    INTEGER(I4B), INTENT(IN), OPTIONAL :: cnt_in(1)
    !! number of entries to write; not used at present
    INTEGER(I4B), INTENT(IN), OPTIONAL :: offset_in(1)
    !! offset of vals in the dataset; not used at present
  END SUBROUTINE obj_Write_n0
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Write
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Write a rank-1 array of Int32
!
! Write a rank-1 array of Int32 to the dataset dsetname. Missing groups in
! dsetname are created. An existing dataset is overwritten when overwriting is
! allowed (see SetOverwriteStat); the new data must have the same shape as the
! old data, since it is written in place (use Append for data that grows).
!
! In DEBUG mode an error is raised if obj is not initiated, the file is not
! open, or it does not have write access.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : INTEGER(INT32) - data to write
!   gdims_in, cnt_in, offset_in : INTEGER(I4B), optional - shape, count and
!       offset of the data
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE GlobalData, ONLY: I4B
! USE HDF5File_Class, ONLY: HDF5File_
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: filename = "Write_test_3.h5"
! TYPE(HDF5File_) :: obj
! INTEGER(I4B) :: s, v(5), m(2, 3)
! INTEGER(I4B) :: s1
! INTEGER(I4B), ALLOCATABLE :: v1(:), m1(:, :)
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! s = 7
! v = [1, 2, 3, 4, 5]
! m = RESHAPE([1, 2, 3, 4, 5, 6], [2, 3])
!
! CALL obj%Initiate(filename=filename, mode="NEW")
! CALL obj%OPEN()
! CALL obj%WRITE(dsetname="/s", vals=s)
! CALL obj%WRITE(dsetname="/v", vals=v)
! CALL obj%WRITE(dsetname="/m", vals=m)
! CALL obj%DEALLOCATE()
!
! CALL obj%Initiate(filename=filename, mode="READ")
! CALL obj%OPEN()
! CALL obj%READ(dsetname="/s", vals=s1)
! CALL obj%READ(dsetname="/v", vals=v1)
! CALL obj%READ(dsetname="/m", vals=m1)
! CALL obj%DEALLOCATE()
!
! CALL OK(s1 .EQ. s, "rank 0")
! CALL OK(ALL(v1 .EQ. v), "rank 1")
! CALL OK(ALL(m1 .EQ. m), "rank 2")
! END PROGRAM main
!```
!
! See also: [[obj_Read_n1]]

INTERFACE
  MODULE SUBROUTINE obj_Write_n1(obj, dsetname, vals, gdims_in, &
                                 cnt_in, offset_in)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    INTEGER(INT32), INTENT(IN) :: vals(:)
    !! a rank-1 array of Int32 to write
    INTEGER(I4B), INTENT(IN), OPTIONAL :: gdims_in(1)
    !! global shape of the dataset, default is the shape of vals
    INTEGER(I4B), INTENT(IN), OPTIONAL :: cnt_in(1)
    !! number of entries to write; not used at present
    INTEGER(I4B), INTENT(IN), OPTIONAL :: offset_in(1)
    !! offset of vals in the dataset; not used at present
  END SUBROUTINE obj_Write_n1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Write
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Write a rank-2 array of Int32
!
! Write a rank-2 array of Int32 to the dataset dsetname. Missing groups in
! dsetname are created. An existing dataset is overwritten when overwriting is
! allowed (see SetOverwriteStat); the new data must have the same shape as the
! old data, since it is written in place (use Append for data that grows).
!
! In DEBUG mode an error is raised if obj is not initiated, the file is not
! open, or it does not have write access.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : INTEGER(INT32) - data to write
!   gdims_in, cnt_in, offset_in : INTEGER(I4B), optional - shape, count and
!       offset of the data
!
! See also: [[obj_Write_n1]], [[obj_Read_n2]]

INTERFACE
  MODULE SUBROUTINE obj_Write_n2(obj, dsetname, vals, gdims_in, &
                                 cnt_in, offset_in)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    INTEGER(INT32), INTENT(IN) :: vals(:, :)
    !! a rank-2 array of Int32 to write
    INTEGER(I4B), INTENT(IN), OPTIONAL :: gdims_in(2)
    !! global shape of the dataset, default is the shape of vals
    INTEGER(I4B), INTENT(IN), OPTIONAL :: cnt_in(2)
    !! number of entries to write; not used at present
    INTEGER(I4B), INTENT(IN), OPTIONAL :: offset_in(2)
    !! offset of vals in the dataset; not used at present
  END SUBROUTINE obj_Write_n2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Write
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Write a rank-3 array of Int32
!
! Write a rank-3 array of Int32 to the dataset dsetname. Missing groups in
! dsetname are created. An existing dataset is overwritten when overwriting is
! allowed (see SetOverwriteStat); the new data must have the same shape as the
! old data, since it is written in place (use Append for data that grows).
!
! In DEBUG mode an error is raised if obj is not initiated, the file is not
! open, or it does not have write access.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : INTEGER(INT32) - data to write
!   gdims_in, cnt_in, offset_in : INTEGER(I4B), optional - shape, count and
!       offset of the data
!
! See also: [[obj_Write_n1]], [[obj_Read_n3]]

INTERFACE
  MODULE SUBROUTINE obj_Write_n3(obj, dsetname, vals, gdims_in, &
                                 cnt_in, offset_in)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    INTEGER(INT32), INTENT(IN) :: vals(:, :, :)
    !! a rank-3 array of Int32 to write
    INTEGER(I4B), INTENT(IN), OPTIONAL :: gdims_in(3)
    !! global shape of the dataset, default is the shape of vals
    INTEGER(I4B), INTENT(IN), OPTIONAL :: cnt_in(3)
    !! number of entries to write; not used at present
    INTEGER(I4B), INTENT(IN), OPTIONAL :: offset_in(3)
    !! offset of vals in the dataset; not used at present
  END SUBROUTINE obj_Write_n3
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Write
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Write a rank-4 array of Int32
!
! Write a rank-4 array of Int32 to the dataset dsetname. Missing groups in
! dsetname are created. An existing dataset is overwritten when overwriting is
! allowed (see SetOverwriteStat); the new data must have the same shape as the
! old data, since it is written in place (use Append for data that grows).
!
! In DEBUG mode an error is raised if obj is not initiated, the file is not
! open, or it does not have write access.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : INTEGER(INT32) - data to write
!   gdims_in, cnt_in, offset_in : INTEGER(I4B), optional - shape, count and
!       offset of the data
!
! See also: [[obj_Write_n1]], [[obj_Read_n4]]

INTERFACE
  MODULE SUBROUTINE obj_Write_n4(obj, dsetname, vals, gdims_in, &
                                 cnt_in, offset_in)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    INTEGER(INT32), INTENT(IN) :: vals(:, :, :, :)
    !! a rank-4 array of Int32 to write
    INTEGER(I4B), INTENT(IN), OPTIONAL :: gdims_in(4)
    !! global shape of the dataset, default is the shape of vals
    INTEGER(I4B), INTENT(IN), OPTIONAL :: cnt_in(4)
    !! number of entries to write; not used at present
    INTEGER(I4B), INTENT(IN), OPTIONAL :: offset_in(4)
    !! offset of vals in the dataset; not used at present
  END SUBROUTINE obj_Write_n4
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Write
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Write a rank-5 array of Int32
!
! Write a rank-5 array of Int32 to the dataset dsetname. Missing groups in
! dsetname are created. An existing dataset is overwritten when overwriting is
! allowed (see SetOverwriteStat); the new data must have the same shape as the
! old data, since it is written in place (use Append for data that grows).
!
! In DEBUG mode an error is raised if obj is not initiated, the file is not
! open, or it does not have write access.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : INTEGER(INT32) - data to write
!   gdims_in, cnt_in, offset_in : INTEGER(I4B), optional - shape, count and
!       offset of the data
!
! See also: [[obj_Write_n1]], [[obj_Read_n5]]

INTERFACE
  MODULE SUBROUTINE obj_Write_n5(obj, dsetname, vals, gdims_in, &
                                 cnt_in, offset_in)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    INTEGER(INT32), INTENT(IN) :: vals(:, :, :, :, :)
    !! a rank-5 array of Int32 to write
    INTEGER(I4B), INTENT(IN), OPTIONAL :: gdims_in(5)
    !! global shape of the dataset, default is the shape of vals
    INTEGER(I4B), INTENT(IN), OPTIONAL :: cnt_in(5)
    !! number of entries to write; not used at present
    INTEGER(I4B), INTENT(IN), OPTIONAL :: offset_in(5)
    !! offset of vals in the dataset; not used at present
  END SUBROUTINE obj_Write_n5
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Write
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Write a rank-6 array of Int32
!
! Write a rank-6 array of Int32 to the dataset dsetname. Missing groups in
! dsetname are created. An existing dataset is overwritten when overwriting is
! allowed (see SetOverwriteStat); the new data must have the same shape as the
! old data, since it is written in place (use Append for data that grows).
!
! In DEBUG mode an error is raised if obj is not initiated, the file is not
! open, or it does not have write access.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : INTEGER(INT32) - data to write
!   gdims_in, cnt_in, offset_in : INTEGER(I4B), optional - shape, count and
!       offset of the data
!
! See also: [[obj_Write_n1]], [[obj_Read_n6]]

INTERFACE
  MODULE SUBROUTINE obj_Write_n6(obj, dsetname, vals, gdims_in, &
                                 cnt_in, offset_in)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    INTEGER(INT32), INTENT(IN) :: vals(:, :, :, :, :, :)
    !! a rank-6 array of Int32 to write
    INTEGER(I4B), INTENT(IN), OPTIONAL :: gdims_in(6)
    !! global shape of the dataset, default is the shape of vals
    INTEGER(I4B), INTENT(IN), OPTIONAL :: cnt_in(6)
    !! number of entries to write; not used at present
    INTEGER(I4B), INTENT(IN), OPTIONAL :: offset_in(6)
    !! offset of vals in the dataset; not used at present
  END SUBROUTINE obj_Write_n6
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Write
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Write a rank-7 array of Int32
!
! Write a rank-7 array of Int32 to the dataset dsetname. Missing groups in
! dsetname are created. An existing dataset is overwritten when overwriting is
! allowed (see SetOverwriteStat); the new data must have the same shape as the
! old data, since it is written in place (use Append for data that grows).
!
! In DEBUG mode an error is raised if obj is not initiated, the file is not
! open, or it does not have write access.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : INTEGER(INT32) - data to write
!   gdims_in, cnt_in, offset_in : INTEGER(I4B), optional - shape, count and
!       offset of the data
!
! See also: [[obj_Write_n1]], [[obj_Read_n7]]

INTERFACE
  MODULE SUBROUTINE obj_Write_n7(obj, dsetname, vals, gdims_in, &
                                 cnt_in, offset_in)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    INTEGER(INT32), INTENT(IN) :: vals(:, :, :, :, :, :, :)
    !! a rank-7 array of Int32 to write
    INTEGER(I4B), INTENT(IN), OPTIONAL :: gdims_in(7)
    !! global shape of the dataset, default is the shape of vals
    INTEGER(I4B), INTENT(IN), OPTIONAL :: cnt_in(7)
    !! number of entries to write; not used at present
    INTEGER(I4B), INTENT(IN), OPTIONAL :: offset_in(7)
    !! offset of vals in the dataset; not used at present
  END SUBROUTINE obj_Write_n7
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Write
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Write a scalar of String
!
! Write a scalar of String to the dataset dsetname. Missing groups in dsetname
! are created. An existing dataset is overwritten when overwriting is allowed
! (see SetOverwriteStat); the new data must have the same shape as the old
! data, since it is written in place (use Append for data that grows). The
! string is stored with length LEN_TRIM(vals), so trailing blanks are removed.
!
! In DEBUG mode an error is raised if obj is not initiated, the file is not
! open, or it does not have write access.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : TYPE(String) - data to write
!   gdims_in, cnt_in, offset_in : INTEGER(I4B), optional - shape, count and
!       offset of the data
!
! See also: [[obj_Write_st1_helper]], [[obj_Read_st0_helper]]

INTERFACE
  MODULE SUBROUTINE obj_Write_st0(obj, dsetname, vals, gdims_in, &
                                  cnt_in, offset_in)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    TYPE(String), INTENT(IN) :: vals
    !! a scalar of String to write
    INTEGER(I4B), INTENT(IN), OPTIONAL :: gdims_in(1)
    !! global shape of the dataset, default is the shape of vals
    INTEGER(I4B), INTENT(IN), OPTIONAL :: cnt_in(1)
    !! number of entries to write; not used at present
    INTEGER(I4B), INTENT(IN), OPTIONAL :: offset_in(1)
    !! offset of vals in the dataset; not used at present
  END SUBROUTINE obj_Write_st0
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Write
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Write a rank-1 array of String
!
! Write a rank-1 array of String to the dataset dsetname. Missing groups in
! dsetname are created. An existing dataset is overwritten when overwriting is
! allowed (see SetOverwriteStat); the new data must have the same shape as the
! old data, since it is written in place (use Append for data that grows).
! length_max is the length of the longest string in vals; the data is written
! by obj_Write_st1. cnt_in and offset_in are not passed on.
!
! In DEBUG mode an error is raised if obj is not initiated, the file is not
! open, or it does not have write access.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : TYPE(String) - data to write
!   gdims_in, cnt_in, offset_in : INTEGER(I4B), optional - shape, count and
!       offset of the data
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE GlobalData, ONLY: LGT
! USE HDF5File_Class, ONLY: HDF5File_
! USE String_Class, ONLY: String
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: filename = "Write_test_5.h5"
! TYPE(HDF5File_) :: obj
! TYPE(String) :: s, v(2), m(2, 2)
! TYPE(String) :: s1
! TYPE(String), ALLOCATABLE :: v1(:), m1(:, :)
! LOGICAL(LGT) :: isok
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! s = "name"
! v(1) = "hello"
! v(2) = "world"
! m(1, 1) = "a"
! m(2, 1) = "b"
! m(1, 2) = "c"
! m(2, 2) = "d"
!
! CALL obj%Initiate(filename=filename, mode="NEW")
! CALL obj%OPEN()
! CALL obj%WRITE(dsetname="/s", vals=s)
! CALL obj%WRITE(dsetname="/v", vals=v)
! CALL obj%WRITE(dsetname="/m", vals=m)
! CALL obj%DEALLOCATE()
!
! CALL obj%Initiate(filename=filename, mode="READ")
! CALL obj%OPEN()
! CALL obj%READ(dsetname="/s", vals=s1)
! CALL obj%READ(dsetname="/v", vals=v1)
! CALL obj%READ(dsetname="/m", vals=m1)
! CALL obj%DEALLOCATE()
!
! CALL OK(s1%chars() .EQ. "name", "scalar")
! isok = (v1(1)%chars() .EQ. "hello") .AND. (v1(2)%chars() .EQ. "world")
! CALL OK(isok, "rank 1")
! isok = (m1(1, 1)%chars() .EQ. "a") .AND. (m1(2, 1)%chars() .EQ. "b") &
!        .AND. (m1(1, 2)%chars() .EQ. "c") &
!        .AND. (m1(2, 2)%chars() .EQ. "d")
! CALL OK(isok, "rank 2")
! END PROGRAM main
!```
!
! See also: [[obj_Write_st1]], [[obj_Read_st1_helper]]

INTERFACE
  MODULE SUBROUTINE obj_Write_st1_helper(obj, dsetname, vals, gdims_in, &
                                         cnt_in, offset_in)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    TYPE(String), INTENT(IN) :: vals(:)
    !! a rank-1 array of String to write
    INTEGER(I4B), INTENT(IN), OPTIONAL :: gdims_in(1)
    !! global shape of the dataset, default is the shape of vals
    INTEGER(I4B), INTENT(IN), OPTIONAL :: cnt_in(1)
    !! number of entries to write; not used at present
    INTEGER(I4B), INTENT(IN), OPTIONAL :: offset_in(1)
    !! offset of vals in the dataset; not used at present
  END SUBROUTINE obj_Write_st1_helper
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Write
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Write a rank-1 array of String
!
! Write a rank-1 array of String to the dataset dsetname. Missing groups in
! dsetname are created. An existing dataset is overwritten when overwriting is
! allowed (see SetOverwriteStat); the new data must have the same shape as the
! old data, since it is written in place (use Append for data that grows).
! Every string is stored with the fixed length length_max.
!
! In DEBUG mode an error is raised if obj is not initiated, the file is not
! open, or it does not have write access.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : TYPE(String) - data to write
!   length_max : INTEGER(I4B) - length of the longest string
!   gdims_in, cnt_in, offset_in : INTEGER(I4B), optional - shape, count and
!       offset of the data
!
! See also: [[obj_Write_st1_helper]], [[obj_Write_st1_helper]],
!   [[obj_Read_st1_helper]]

INTERFACE
  MODULE SUBROUTINE obj_Write_st1(obj, dsetname, vals, length_max, &
                                  gdims_in, cnt_in, offset_in)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    TYPE(String), INTENT(IN) :: vals(:)
    !! a rank-1 array of String to write
    INTEGER(I4B), INTENT(IN) :: length_max
    !! length of the longest string in vals
    INTEGER(I4B), INTENT(IN), OPTIONAL :: gdims_in(1)
    !! global shape of the dataset, default is the shape of vals
    INTEGER(I4B), INTENT(IN), OPTIONAL :: cnt_in(1)
    !! number of entries to write; not used at present
    INTEGER(I4B), INTENT(IN), OPTIONAL :: offset_in(1)
    !! offset of vals in the dataset; not used at present
  END SUBROUTINE obj_Write_st1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Write
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Write a rank-2 array of String
!
! Write a rank-2 array of String to the dataset dsetname. Missing groups in
! dsetname are created. An existing dataset is overwritten when overwriting is
! allowed (see SetOverwriteStat); the new data must have the same shape as the
! old data, since it is written in place (use Append for data that grows).
! length_max is the length of the longest string in vals; the data is written
! by obj_Write_st2.
!
! In DEBUG mode an error is raised if obj is not initiated, the file is not
! open, or it does not have write access.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : TYPE(String) - data to write
!   gdims_in, cnt_in, offset_in : INTEGER(I4B), optional - shape, count and
!       offset of the data
!
! See also: [[obj_Write_st1_helper]], [[obj_Write_st2]],
!   [[obj_Read_st2_helper]]

INTERFACE
  MODULE SUBROUTINE obj_Write_st2_helper(obj, dsetname, vals, gdims_in, &
                                         cnt_in, offset_in)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    TYPE(String), INTENT(IN) :: vals(:, :)
    !! a rank-2 array of String to write
    INTEGER(I4B), INTENT(IN), OPTIONAL :: gdims_in(2)
    !! global shape of the dataset, default is the shape of vals
    INTEGER(I4B), INTENT(IN), OPTIONAL :: cnt_in(2)
    !! number of entries to write; not used at present
    INTEGER(I4B), INTENT(IN), OPTIONAL :: offset_in(2)
    !! offset of vals in the dataset; not used at present
  END SUBROUTINE obj_Write_st2_helper
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Write
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Write a rank-2 array of String
!
! Write a rank-2 array of String to the dataset dsetname. Missing groups in
! dsetname are created. An existing dataset is overwritten when overwriting is
! allowed (see SetOverwriteStat); the new data must have the same shape as the
! old data, since it is written in place (use Append for data that grows).
! Every string is stored with the fixed length length_max.
!
! In DEBUG mode an error is raised if obj is not initiated, the file is not
! open, or it does not have write access.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : TYPE(String) - data to write
!   length_max : INTEGER(I4B) - length of the longest string
!   gdims_in, cnt_in, offset_in : INTEGER(I4B), optional - shape, count and
!       offset of the data
!
! See also: [[obj_Write_st1_helper]], [[obj_Write_st2_helper]],
!   [[obj_Read_st2_helper]]

INTERFACE
  MODULE SUBROUTINE obj_Write_st2(obj, dsetname, vals, length_max, &
                                  gdims_in, cnt_in, offset_in)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    TYPE(String), INTENT(IN) :: vals(:, :)
    !! a rank-2 array of String to write
    INTEGER(I4B), INTENT(IN) :: length_max
    !! length of the longest string in vals
    INTEGER(I4B), INTENT(IN), OPTIONAL :: gdims_in(2)
    !! global shape of the dataset, default is the shape of vals
    INTEGER(I4B), INTENT(IN), OPTIONAL :: cnt_in(2)
    !! number of entries to write; not used at present
    INTEGER(I4B), INTENT(IN), OPTIONAL :: offset_in(2)
    !! offset of vals in the dataset; not used at present
  END SUBROUTINE obj_Write_st2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Write
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Write a rank-3 array of String
!
! Write a rank-3 array of String to the dataset dsetname. Missing groups in
! dsetname are created. An existing dataset is overwritten when overwriting is
! allowed (see SetOverwriteStat); the new data must have the same shape as the
! old data, since it is written in place (use Append for data that grows).
! length_max is the length of the longest string in vals; the data is written
! by obj_Write_st3. This routine is not bound to the WRITE generic.
!
! In DEBUG mode an error is raised if obj is not initiated, the file is not
! open, or it does not have write access.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : TYPE(String) - data to write
!   gdims_in, cnt_in, offset_in : INTEGER(I4B), optional - shape, count and
!       offset of the data
!
! See also: [[obj_Write_st1_helper]], [[obj_Write_st3]],
!   [[obj_Read_st3_helper]]

INTERFACE
  MODULE SUBROUTINE obj_Write_st3_helper(obj, dsetname, vals, gdims_in, &
                                         cnt_in, offset_in)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    TYPE(String), INTENT(IN) :: vals(:, :, :)
    !! a rank-3 array of String to write
    INTEGER(I4B), INTENT(IN), OPTIONAL :: gdims_in(3)
    !! global shape of the dataset, default is the shape of vals
    INTEGER(I4B), INTENT(IN), OPTIONAL :: cnt_in(3)
    !! number of entries to write; not used at present
    INTEGER(I4B), INTENT(IN), OPTIONAL :: offset_in(3)
    !! offset of vals in the dataset; not used at present
  END SUBROUTINE obj_Write_st3_helper
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Write
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Write a rank-3 array of String
!
! Write a rank-3 array of String to the dataset dsetname. Missing groups in
! dsetname are created. An existing dataset is overwritten when overwriting is
! allowed (see SetOverwriteStat); the new data must have the same shape as the
! old data, since it is written in place (use Append for data that grows).
! Every string is stored with the fixed length length_max. This routine is not
! bound to the WRITE generic.
!
! In DEBUG mode an error is raised if obj is not initiated, the file is not
! open, or it does not have write access.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : TYPE(String) - data to write
!   length_max : INTEGER(I4B) - length of the longest string
!   gdims_in, cnt_in, offset_in : INTEGER(I4B), optional - shape, count and
!       offset of the data
!
! See also: [[obj_Write_st1_helper]], [[obj_Write_st3_helper]],
!   [[obj_Read_st3_helper]]

INTERFACE
  MODULE SUBROUTINE obj_Write_st3(obj, dsetname, vals, length_max, &
                                  gdims_in, cnt_in, offset_in)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    TYPE(String), INTENT(IN) :: vals(:, :, :)
    !! a rank-3 array of String to write
    INTEGER(I4B), INTENT(IN) :: length_max
    !! length of the longest string in vals
    INTEGER(I4B), INTENT(IN), OPTIONAL :: gdims_in(3)
    !! global shape of the dataset, default is the shape of vals
    INTEGER(I4B), INTENT(IN), OPTIONAL :: cnt_in(3)
    !! number of entries to write; not used at present
    INTEGER(I4B), INTENT(IN), OPTIONAL :: offset_in(3)
    !! offset of vals in the dataset; not used at present
  END SUBROUTINE obj_Write_st3
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Write
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Write a character variable
!
! Write a character variable to the dataset dsetname. Missing groups in
! dsetname are created. An existing dataset is overwritten when overwriting is
! allowed (see SetOverwriteStat); the new data must have the same shape as the
! old data, since it is written in place (use Append for data that grows). The
! variable is stored as a rank-1 array of single characters. It can be read in
! a character variable or in a String.
!
! In DEBUG mode an error is raised if obj is not initiated, the file is not
! open, or it does not have write access.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : CHARACTER(*) - data to write
!   gdims_in, cnt_in, offset_in : INTEGER(I4B), optional - shape, count and
!       offset of the data
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE HDF5File_Class, ONLY: HDF5File_
! USE String_Class, ONLY: String
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: filename = "Write_test_6.h5"
! TYPE(HDF5File_) :: obj
! CHARACTER(LEN=11) :: cval
! TYPE(String) :: strval
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! CALL obj%Initiate(filename=filename, mode="NEW")
! CALL obj%OPEN()
! CALL obj%WRITE(dsetname="/greeting", vals="hello world")
! CALL obj%DEALLOCATE()
!
! CALL obj%Initiate(filename=filename, mode="READ")
! CALL obj%OPEN()
! CALL obj%READ(dsetname="/greeting", vals=cval)
! CALL obj%READ(dsetname="/greeting", vals=strval)
! CALL obj%DEALLOCATE()
!
! CALL OK(cval .EQ. "hello world", "read in character")
! CALL OK(strval%chars() .EQ. "hello world", "read in String")
! END PROGRAM main
!```
!
! See also: [[obj_Read_c1]], [[obj_Read_st0_helper]]

INTERFACE
  MODULE SUBROUTINE obj_Write_c1(obj, dsetname, vals, gdims_in, cnt_in, &
                                 offset_in)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    CHARACTER(LEN=*), INTENT(IN) :: vals
    !! character variable to write
    INTEGER(I4B), INTENT(IN), OPTIONAL :: gdims_in
    !! number of characters to write, default is LEN(vals)
    INTEGER(I4B), DIMENSION(1), INTENT(IN), OPTIONAL :: cnt_in
    !! number of entries to write; not used at present
    INTEGER(I4B), DIMENSION(1), INTENT(IN), OPTIONAL :: offset_in
    !! offset of vals in the dataset; not used at present
  END SUBROUTINE obj_Write_c1
END INTERFACE

!----------------------------------------------------------------------------
!                                                             preRead
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! update:
!   - 2026-10-26: error keeps the first failure; ids not opened are -1
! summary: Open a dataset for reading
!
! Open the dataset and get its dimensions. For rank 0, dims is set to 1. This
! routine is used by all Read routines; postRead closes what it opens.
!
! error is 0 on success and negative if a call fails; the later steps are
! then skipped. The ids which are not opened are set to -1.
!
! In DEBUG mode an error is raised if obj is not initiated, the file is not
! open or does not have read access, the dataset can not be opened, or the
! rank of the dataset is not rank.
!
! See also: [[postRead]]

INTERFACE
  MODULE SUBROUTINE preRead(obj, path, rank, dset_id, dspace_id, &
                            dims, error)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: path
    !! path of the dataset
    INTEGER(I4B), INTENT(IN) :: rank
    !! expected rank of the dataset
    INTEGER(HID_T), INTENT(INOUT) :: dset_id
    !! dataset id
    INTEGER(HID_T), INTENT(INOUT) :: dspace_id
    !! dataspace id
    INTEGER(HSIZE_T), INTENT(INOUT) :: dims(:)
    !! dimensions of the dataset
    INTEGER(I4B), INTENT(OUT) :: error
    !! error flag, negative on failure
  END SUBROUTINE preRead
END INTERFACE

!----------------------------------------------------------------------------
!                                                               GetDataShape
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-08
! summary: Get the shape of a dataset
!
! Returns the shape of the dataset dsetname. The shape of a scalar dataset has
! size zero.
!
! In DEBUG mode an error is raised if obj is not initiated, the file is not
! open or does not have read access, or the dataset can not be opened.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!
! Returns:
!   dataShape : INTEGER(I4B), ALLOCATABLE - shape of the dataset
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE HDF5File_Class, ONLY: HDF5File_
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: filename = "GetDataShape_test_1.h5"
! TYPE(HDF5File_) :: obj
! REAL(DFP) :: mat(3, 4)
! INTEGER(I4B), ALLOCATABLE :: dshape(:)
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! mat = 1.0_DFP
! CALL obj%Initiate(filename=filename, mode="NEW")
! CALL obj%OPEN()
! CALL obj%WRITE(dsetname="/mat", vals=mat)
! CALL obj%WRITE(dsetname="/scalar", vals=1.0_DFP)
!
! dshape = obj%GetDataShape("/mat")
! CALL OK(ALL(dshape .EQ. [3, 4]), "GetDataShape of a matrix")
! dshape = obj%GetDataShape("/scalar")
! CALL OK(SIZE(dshape) .EQ. 0, "GetDataShape of a scalar")
!
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetDataType]]

INTERFACE
  MODULE FUNCTION obj_GetDataShape(obj, dsetname) RESULT(dataShape)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    INTEGER(I4B), ALLOCATABLE :: dataShape(:)
    !! shape of the dataset; size zero for a scalar
  END FUNCTION obj_GetDataShape
END INTERFACE

!----------------------------------------------------------------------------
!                                                               GetDataType
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-08
! summary: Get the data type of a dataset
!
! Returns 'Rea' for a 32 or 64 bit real, 'Int' for a 32 or 64 bit integer,
! 'STR' for a string, and 'N/A' for a real or integer of another precision. In
! DEBUG mode an error is raised for other data types.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!
! Returns:
!   dataType : CHARACTER(3) - 'Rea', 'Int', 'STR' or 'N/A'
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE HDF5File_Class, ONLY: HDF5File_
! USE String_Class, ONLY: String
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: filename = "GetDataType_test_1.h5"
! TYPE(HDF5File_) :: obj
! TYPE(String) :: strval
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! strval = "hello"
! CALL obj%Initiate(filename=filename, mode="NEW")
! CALL obj%OPEN()
! CALL obj%WRITE(dsetname="/real", vals=1.0_DFP)
! CALL obj%WRITE(dsetname="/int", vals=1_I4B)
! CALL obj%WRITE(dsetname="/str", vals=strval)
!
! CALL OK(obj%GetDataType("/real") .EQ. "Rea", "real")
! CALL OK(obj%GetDataType("/int") .EQ. "Int", "integer")
! CALL OK(obj%GetDataType("/str") .EQ. "STR", "string")
!
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetDataShape]]

INTERFACE
  MODULE FUNCTION obj_GetDataType(obj, dsetname) RESULT(dataType)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    CHARACTER(LEN=3) :: dataType
    !! 'Rea', 'Int', 'STR' or 'N/A'
  END FUNCTION obj_GetDataType
END INTERFACE

!----------------------------------------------------------------------------
!                                                            postRead
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-08
! update:
!   - 2026-10-26: close only valid ids; error is not changed
! summary: Close the objects opened by preRead
!
! Close the dataset and the dataspace; ids equal to -1 are skipped. error
! keeps the status of the read. In DEBUG mode, if error is not zero, an error
! is raised; for a compressed dataset with large chunks a warning about
! memory use is raised first.
!
! See also: [[preRead]]

INTERFACE
  MODULE SUBROUTINE postRead(obj, path, dset_id, dspace_id, error)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: path
    !! path of the dataset
    INTEGER(HID_T), INTENT(INOUT) :: dset_id
    !! dataset id
    INTEGER(HID_T), INTENT(INOUT) :: dspace_id
    !! dataspace id
    INTEGER(I4B), INTENT(INOUT) :: error
    !! error flag of the read operation
  END SUBROUTINE postRead
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Read a scalar of Real64
!
! Read a scalar of Real64 from the dataset dsetname.
!
! In DEBUG mode an error is raised if the file is not open with read access,
! the dataset can not be opened, or its rank is not the rank of vals.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : REAL(REAL64) - data read from the dataset
!
! See also: [[obj_Read_d1]], [[obj_Write_d0]]

INTERFACE
  MODULE SUBROUTINE obj_Read_d0(obj, dsetname, vals)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    REAL(REAL64), INTENT(INOUT) :: vals
    !! a scalar of Real64 read from the dataset
  END SUBROUTINE obj_Read_d0
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Read a rank-1 array of Real64
!
! Read a rank-1 array of Real64 from the dataset dsetname. vals is allocated
! to the shape of the dataset; it is reallocated if it has another shape.
!
! In DEBUG mode an error is raised if the file is not open with read access,
! the dataset can not be opened, or its rank is not the rank of vals.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : REAL(REAL64), ALLOCATABLE - data read from the dataset
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE GlobalData, ONLY: DFP
! USE HDF5File_Class, ONLY: HDF5File_
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: filename = "Read_test_1.h5"
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! TYPE(HDF5File_) :: obj
! REAL(DFP) :: v(4), m(2, 3)
! REAL(DFP), ALLOCATABLE :: v1(:), m1(:, :)
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! v = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP]
! CALL RANDOM_NUMBER(m)
!
! CALL obj%Initiate(filename=filename, mode="NEW")
! CALL obj%OPEN()
! CALL obj%WRITE(dsetname="/v", vals=v)
! CALL obj%WRITE(dsetname="/m", vals=m)
! CALL obj%DEALLOCATE()
!
! ALLOCATE (v1(10), m1(2, 5))
! CALL obj%Initiate(filename=filename, mode="READ")
! CALL obj%OPEN()
! CALL obj%READ(dsetname="/v", vals=v1)
! CALL obj%READ(dsetname="/m", vals=m1)
! CALL obj%DEALLOCATE()
!
! CALL OK(SIZE(v1) .EQ. 4, "rank 1 reallocated")
! CALL OK(ALL(ABS(v1 - v) .LT. tol), "rank 1 values")
! CALL OK(ALL(SHAPE(m1) .EQ. [2, 3]), "rank 2 reallocated")
! CALL OK(ALL(ABS(m1 - m) .LT. tol), "rank 2 values")
! END PROGRAM main
!```
!
! See also: [[obj_Write_d1]], [[obj_Read__d1]]

INTERFACE
  MODULE SUBROUTINE obj_Read_d1(obj, dsetname, vals)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: vals(:)
    !! a rank-1 array of Real64 read from the dataset
  END SUBROUTINE obj_Read_d1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Read a rank-1 array of Real64 without allocating vals
!
! Read a rank-1 array of Real64 from the dataset dsetname into vals without
! allocating vals. The extent of the data is returned in tsize; the data is
! stored in vals(1:tsize), and the rest of vals is not changed.
!
! The extents are set to zero at the start. vals must be large enough to hold
! the dataset along every dimension; in DEBUG mode an error is raised
! otherwise. In release builds this is not checked.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : REAL(REAL64) - array to fill, SIZE(vals, i) >= extent i
!   tsize : INTEGER(I4B) - size of data written in vals
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE HDF5File_Class, ONLY: HDF5File_
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: filename = "Read__test_1.h5"
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! TYPE(HDF5File_) :: obj
! REAL(DFP) :: v(3), m(2, 3), t(2, 2, 2)
! REAL(DFP) :: vbuf(10), mbuf(5, 5), tbuf(3, 3, 3)
! INTEGER(I4B) :: tsize, nrow, ncol, dim1, dim2, dim3
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! CALL RANDOM_NUMBER(v)
! CALL RANDOM_NUMBER(m)
! CALL RANDOM_NUMBER(t)
!
! CALL obj%Initiate(filename=filename, mode="NEW")
! CALL obj%OPEN()
! CALL obj%WRITE(dsetname="/v", vals=v)
! CALL obj%WRITE(dsetname="/m", vals=m)
! CALL obj%WRITE(dsetname="/t", vals=t)
! CALL obj%DEALLOCATE()
!
! vbuf = -1.0_DFP
! mbuf = -1.0_DFP
! tbuf = -1.0_DFP
! CALL obj%Initiate(filename=filename, mode="READ")
! CALL obj%OPEN()
! CALL obj%READ_(dsetname="/v", vals=vbuf, tsize=tsize)
! CALL obj%READ_(dsetname="/m", vals=mbuf, nrow=nrow, ncol=ncol)
! CALL obj%READ_(dsetname="/t", vals=tbuf, dim1=dim1, dim2=dim2, &
!                dim3=dim3)
! CALL obj%DEALLOCATE()
!
! CALL OK(tsize .EQ. 3, "rank 1 tsize")
! CALL OK(ALL(ABS(vbuf(1:tsize) - v) .LT. tol), "rank 1 values")
! CALL OK(ALL(vbuf(tsize + 1:) .EQ. -1.0_DFP), "rank 1 rest")
!
! CALL OK((nrow .EQ. 2) .AND. (ncol .EQ. 3), "rank 2 nrow, ncol")
! CALL OK(ALL(ABS(mbuf(1:nrow, 1:ncol) - m) .LT. tol), "rank 2 values")
! CALL OK(ALL(mbuf(nrow + 1:, :) .EQ. -1.0_DFP), "rank 2 rest")
!
! CALL OK(ALL([dim1, dim2, dim3] .EQ. 2), "rank 3 dim1, dim2, dim3")
! CALL OK(ALL(ABS(tbuf(1:dim1, 1:dim2, 1:dim3) - t) .LT. tol), &
!         "rank 3 values")
! END PROGRAM main
!```
!
! See also: [[obj_Read_d1]]

INTERFACE
  MODULE SUBROUTINE obj_Read__d1(obj, dsetname, vals, tsize)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    REAL(REAL64), INTENT(INOUT) :: vals(:)
    !! vals will be filled with entries read from the dataset
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of data written in vals
  END SUBROUTINE obj_Read__d1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Read a rank-2 array of Real64
!
! Read a rank-2 array of Real64 from the dataset dsetname. vals is allocated
! to the shape of the dataset; it is reallocated if it has another shape.
!
! In DEBUG mode an error is raised if the file is not open with read access,
! the dataset can not be opened, or its rank is not the rank of vals.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : REAL(REAL64), ALLOCATABLE - data read from the dataset
!
! See also: [[obj_Read_d1]], [[obj_Write_d2]], [[obj_Read__d2]]

INTERFACE
  MODULE SUBROUTINE obj_Read_d2(obj, dsetname, vals)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: vals(:, :)
    !! a rank-2 array of Real64 read from the dataset
  END SUBROUTINE obj_Read_d2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Read a rank-2 array of Real64 without allocating vals
!
! Read a rank-2 array of Real64 from the dataset dsetname into vals without
! allocating vals. The extent of the data is returned in nrow, ncol; the data
! is stored in vals(1:nrow, 1:ncol), and the rest of vals is not changed.
!
! The extents are set to zero at the start. vals must be large enough to hold
! the dataset along every dimension; in DEBUG mode an error is raised
! otherwise. In release builds this is not checked.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : REAL(REAL64) - array to fill, SIZE(vals, i) >= extent i
!   nrow : INTEGER(I4B) - number of rows written in vals
!   ncol : INTEGER(I4B) - number of columns written in vals
!
! See also: [[obj_Read__d1]], [[obj_Read_d2]]

INTERFACE
  MODULE SUBROUTINE obj_Read__d2(obj, dsetname, vals, nrow, ncol)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    REAL(REAL64), INTENT(INOUT) :: vals(:, :)
    !! vals will be filled with entries read from the dataset
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! number of rows written in vals
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! number of columns written in vals
  END SUBROUTINE obj_Read__d2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Read a rank-3 array of Real64
!
! Read a rank-3 array of Real64 from the dataset dsetname. vals is allocated
! to the shape of the dataset; it is reallocated if it has another shape.
!
! In DEBUG mode an error is raised if the file is not open with read access,
! the dataset can not be opened, or its rank is not the rank of vals.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : REAL(REAL64), ALLOCATABLE - data read from the dataset
!
! See also: [[obj_Read_d1]], [[obj_Write_d3]], [[obj_Read__d3]]

INTERFACE
  MODULE SUBROUTINE obj_Read_d3(obj, dsetname, vals)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: vals(:, :, :)
    !! a rank-3 array of Real64 read from the dataset
  END SUBROUTINE obj_Read_d3
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Read a rank-3 array of Real64 without allocating vals
!
! Read a rank-3 array of Real64 from the dataset dsetname into vals without
! allocating vals. The extent of the data is returned in dim1, dim2, dim3; the
! data is stored in vals(1:dim1, ..., 1:dim3), and the rest of vals is not
! changed.
!
! The extents are set to zero at the start. vals must be large enough to hold
! the dataset along every dimension; in DEBUG mode an error is raised
! otherwise. In release builds this is not checked.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : REAL(REAL64) - array to fill, SIZE(vals, i) >= extent i
!   dim1, ..., dim3 : INTEGER(I4B) - extent of data along each dimension
!
! See also: [[obj_Read__d1]], [[obj_Read_d3]]

INTERFACE
  MODULE SUBROUTINE obj_Read__d3(obj, dsetname, vals, dim1, dim2, dim3)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    REAL(REAL64), INTENT(INOUT) :: vals(:, :, :)
    !! vals will be filled with entries read from the dataset
    INTEGER(I4B), INTENT(OUT) :: dim1
    !! extent of data written in vals along dimension 1
    INTEGER(I4B), INTENT(OUT) :: dim2
    !! extent of data written in vals along dimension 2
    INTEGER(I4B), INTENT(OUT) :: dim3
    !! extent of data written in vals along dimension 3
  END SUBROUTINE obj_Read__d3
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Read a rank-4 array of Real64
!
! Read a rank-4 array of Real64 from the dataset dsetname. vals is allocated
! to the shape of the dataset; it is reallocated if it has another shape.
!
! In DEBUG mode an error is raised if the file is not open with read access,
! the dataset can not be opened, or its rank is not the rank of vals.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : REAL(REAL64), ALLOCATABLE - data read from the dataset
!
! See also: [[obj_Read_d1]], [[obj_Write_d4]], [[obj_Read__d4]]

INTERFACE
  MODULE SUBROUTINE obj_Read_d4(obj, dsetname, vals)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: vals(:, :, :, :)
    !! a rank-4 array of Real64 read from the dataset
  END SUBROUTINE obj_Read_d4
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Read a rank-4 array of Real64 without allocating vals
!
! Read a rank-4 array of Real64 from the dataset dsetname into vals without
! allocating vals. The extent of the data is returned in dim1, dim2, dim3,
! dim4; the data is stored in vals(1:dim1, ..., 1:dim4), and the rest of vals
! is not changed.
!
! The extents are set to zero at the start. vals must be large enough to hold
! the dataset along every dimension; in DEBUG mode an error is raised
! otherwise. In release builds this is not checked.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : REAL(REAL64) - array to fill, SIZE(vals, i) >= extent i
!   dim1, ..., dim4 : INTEGER(I4B) - extent of data along each dimension
!
! See also: [[obj_Read__d1]], [[obj_Read_d4]]

INTERFACE
  MODULE SUBROUTINE obj_Read__d4(obj, dsetname, vals, dim1, dim2, dim3, &
                                 dim4)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    REAL(REAL64), INTENT(INOUT) :: vals(:, :, :, :)
    !! vals will be filled with entries read from the dataset
    INTEGER(I4B), INTENT(OUT) :: dim1
    !! extent of data written in vals along dimension 1
    INTEGER(I4B), INTENT(OUT) :: dim2
    !! extent of data written in vals along dimension 2
    INTEGER(I4B), INTENT(OUT) :: dim3
    !! extent of data written in vals along dimension 3
    INTEGER(I4B), INTENT(OUT) :: dim4
    !! extent of data written in vals along dimension 4
  END SUBROUTINE obj_Read__d4
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Read a rank-5 array of Real64
!
! Read a rank-5 array of Real64 from the dataset dsetname. vals is allocated
! to the shape of the dataset; it is reallocated if it has another shape.
!
! In DEBUG mode an error is raised if the file is not open with read access,
! the dataset can not be opened, or its rank is not the rank of vals.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : REAL(REAL64), ALLOCATABLE - data read from the dataset
!
! See also: [[obj_Read_d1]], [[obj_Write_d5]], [[obj_Read__d5]]

INTERFACE
  MODULE SUBROUTINE obj_Read_d5(obj, dsetname, vals)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: vals(:, :, :, :, :)
    !! a rank-5 array of Real64 read from the dataset
  END SUBROUTINE obj_Read_d5
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Read a rank-5 array of Real64 without allocating vals
!
! Read a rank-5 array of Real64 from the dataset dsetname into vals without
! allocating vals. The extent of the data is returned in dim1, dim2, dim3,
! dim4, dim5; the data is stored in vals(1:dim1, ..., 1:dim5), and the rest of
! vals is not changed.
!
! The extents are set to zero at the start. vals must be large enough to hold
! the dataset along every dimension; in DEBUG mode an error is raised
! otherwise. In release builds this is not checked.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : REAL(REAL64) - array to fill, SIZE(vals, i) >= extent i
!   dim1, ..., dim5 : INTEGER(I4B) - extent of data along each dimension
!
! See also: [[obj_Read__d1]], [[obj_Read_d5]]

INTERFACE
  MODULE SUBROUTINE obj_Read__d5(obj, dsetname, vals, dim1, dim2, dim3, &
                                 dim4, dim5)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    REAL(REAL64), INTENT(INOUT) :: vals(:, :, :, :, :)
    !! vals will be filled with entries read from the dataset
    INTEGER(I4B), INTENT(OUT) :: dim1
    !! extent of data written in vals along dimension 1
    INTEGER(I4B), INTENT(OUT) :: dim2
    !! extent of data written in vals along dimension 2
    INTEGER(I4B), INTENT(OUT) :: dim3
    !! extent of data written in vals along dimension 3
    INTEGER(I4B), INTENT(OUT) :: dim4
    !! extent of data written in vals along dimension 4
    INTEGER(I4B), INTENT(OUT) :: dim5
    !! extent of data written in vals along dimension 5
  END SUBROUTINE obj_Read__d5
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Read a rank-6 array of Real64
!
! Read a rank-6 array of Real64 from the dataset dsetname. vals is allocated
! to the shape of the dataset; it is reallocated if it has another shape.
!
! In DEBUG mode an error is raised if the file is not open with read access,
! the dataset can not be opened, or its rank is not the rank of vals.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : REAL(REAL64), ALLOCATABLE - data read from the dataset
!
! See also: [[obj_Read_d1]], [[obj_Write_d6]], [[obj_Read__d6]]

INTERFACE
  MODULE SUBROUTINE obj_Read_d6(obj, dsetname, vals)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: vals(:, :, :, :, :, :)
    !! a rank-6 array of Real64 read from the dataset
  END SUBROUTINE obj_Read_d6
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Read a rank-6 array of Real64 without allocating vals
!
! Read a rank-6 array of Real64 from the dataset dsetname into vals without
! allocating vals. The extent of the data is returned in dim1, dim2, dim3,
! dim4, dim5, dim6; the data is stored in vals(1:dim1, ..., 1:dim6), and the
! rest of vals is not changed.
!
! The extents are set to zero at the start. vals must be large enough to hold
! the dataset along every dimension; in DEBUG mode an error is raised
! otherwise. In release builds this is not checked.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : REAL(REAL64) - array to fill, SIZE(vals, i) >= extent i
!   dim1, ..., dim6 : INTEGER(I4B) - extent of data along each dimension
!
! See also: [[obj_Read__d1]], [[obj_Read_d6]]

INTERFACE
  MODULE SUBROUTINE obj_Read__d6(obj, dsetname, vals, dim1, dim2, dim3, &
                                 dim4, dim5, dim6)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    REAL(REAL64), INTENT(INOUT) :: vals(:, :, :, :, :, :)
    !! vals will be filled with entries read from the dataset
    INTEGER(I4B), INTENT(OUT) :: dim1
    !! extent of data written in vals along dimension 1
    INTEGER(I4B), INTENT(OUT) :: dim2
    !! extent of data written in vals along dimension 2
    INTEGER(I4B), INTENT(OUT) :: dim3
    !! extent of data written in vals along dimension 3
    INTEGER(I4B), INTENT(OUT) :: dim4
    !! extent of data written in vals along dimension 4
    INTEGER(I4B), INTENT(OUT) :: dim5
    !! extent of data written in vals along dimension 5
    INTEGER(I4B), INTENT(OUT) :: dim6
    !! extent of data written in vals along dimension 6
  END SUBROUTINE obj_Read__d6
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Read a rank-7 array of Real64
!
! Read a rank-7 array of Real64 from the dataset dsetname. vals is allocated
! to the shape of the dataset; it is reallocated if it has another shape.
!
! In DEBUG mode an error is raised if the file is not open with read access,
! the dataset can not be opened, or its rank is not the rank of vals.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : REAL(REAL64), ALLOCATABLE - data read from the dataset
!
! See also: [[obj_Read_d1]], [[obj_Write_d7]], [[obj_Read__d7]]

INTERFACE
  MODULE SUBROUTINE obj_Read_d7(obj, dsetname, vals)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    REAL(REAL64), ALLOCATABLE, INTENT(INOUT) :: vals(:, :, :, :, :, :, :)
    !! a rank-7 array of Real64 read from the dataset
  END SUBROUTINE obj_Read_d7
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Read a rank-7 array of Real64 without allocating vals
!
! Read a rank-7 array of Real64 from the dataset dsetname into vals without
! allocating vals. The extent of the data is returned in dim1, dim2, dim3,
! dim4, dim5, dim6, dim7; the data is stored in vals(1:dim1, ..., 1:dim7), and
! the rest of vals is not changed.
!
! The extents are set to zero at the start. vals must be large enough to hold
! the dataset along every dimension; in DEBUG mode an error is raised
! otherwise. In release builds this is not checked.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : REAL(REAL64) - array to fill, SIZE(vals, i) >= extent i
!   dim1, ..., dim7 : INTEGER(I4B) - extent of data along each dimension
!
! See also: [[obj_Read__d1]], [[obj_Read_d7]]

INTERFACE
  MODULE SUBROUTINE obj_Read__d7(obj, dsetname, vals, dim1, dim2, dim3, &
                                 dim4, dim5, dim6, dim7)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    REAL(REAL64), INTENT(INOUT) :: vals(:, :, :, :, :, :, :)
    !! vals will be filled with entries read from the dataset
    INTEGER(I4B), INTENT(OUT) :: dim1
    !! extent of data written in vals along dimension 1
    INTEGER(I4B), INTENT(OUT) :: dim2
    !! extent of data written in vals along dimension 2
    INTEGER(I4B), INTENT(OUT) :: dim3
    !! extent of data written in vals along dimension 3
    INTEGER(I4B), INTENT(OUT) :: dim4
    !! extent of data written in vals along dimension 4
    INTEGER(I4B), INTENT(OUT) :: dim5
    !! extent of data written in vals along dimension 5
    INTEGER(I4B), INTENT(OUT) :: dim6
    !! extent of data written in vals along dimension 6
    INTEGER(I4B), INTENT(OUT) :: dim7
    !! extent of data written in vals along dimension 7
  END SUBROUTINE obj_Read__d7
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Read a scalar of Real32
!
! Read a scalar of Real32 from the dataset dsetname.
!
! In DEBUG mode an error is raised if the file is not open with read access,
! the dataset can not be opened, or its rank is not the rank of vals.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : REAL(REAL32) - data read from the dataset
!
! See also: [[obj_Read_s1]], [[obj_Write_s0]]

INTERFACE
  MODULE SUBROUTINE obj_Read_s0(obj, dsetname, vals)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    REAL(REAL32), INTENT(INOUT) :: vals
    !! a scalar of Real32 read from the dataset
  END SUBROUTINE obj_Read_s0
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Read a rank-1 array of Real32
!
! Read a rank-1 array of Real32 from the dataset dsetname. vals is allocated
! to the shape of the dataset; it is reallocated if it has another shape.
!
! In DEBUG mode an error is raised if the file is not open with read access,
! the dataset can not be opened, or its rank is not the rank of vals.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : REAL(REAL32), ALLOCATABLE - data read from the dataset
!
! See also: [[obj_Write_s1]], [[obj_Write_s1]], [[obj_Read__s1]]

INTERFACE
  MODULE SUBROUTINE obj_Read_s1(obj, dsetname, vals)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: vals(:)
    !! a rank-1 array of Real32 read from the dataset
  END SUBROUTINE obj_Read_s1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Read a rank-1 array of Real32 without allocating vals
!
! Read a rank-1 array of Real32 from the dataset dsetname into vals without
! allocating vals. The extent of the data is returned in tsize; the data is
! stored in vals(1:tsize), and the rest of vals is not changed.
!
! The extents are set to zero at the start. vals must be large enough to hold
! the dataset along every dimension; in DEBUG mode an error is raised
! otherwise. In release builds this is not checked.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : REAL(REAL32) - array to fill, SIZE(vals, i) >= extent i
!   tsize : INTEGER(I4B) - size of data written in vals
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: REAL32
! USE HDF5File_Class, ONLY: HDF5File_
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: filename = "Read__test_2.h5"
! REAL(REAL32), PARAMETER :: tol = 1.0E-6_REAL32
! TYPE(HDF5File_) :: obj
! REAL(REAL32) :: rv(4), rbuf(10)
! INTEGER(I4B) :: iv(2, 3), ibuf(4, 4)
! INTEGER(I4B) :: tsize, nrow, ncol
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! CALL RANDOM_NUMBER(rv)
! iv = RESHAPE([1, 2, 3, 4, 5, 6], [2, 3])
!
! CALL obj%Initiate(filename=filename, mode="NEW")
! CALL obj%OPEN()
! CALL obj%WRITE(dsetname="/rv", vals=rv)
! CALL obj%WRITE(dsetname="/iv", vals=iv)
! CALL obj%DEALLOCATE()
!
! CALL obj%Initiate(filename=filename, mode="READ")
! CALL obj%OPEN()
! CALL obj%READ_(dsetname="/rv", vals=rbuf, tsize=tsize)
! CALL obj%READ_(dsetname="/iv", vals=ibuf, nrow=nrow, ncol=ncol)
! CALL obj%DEALLOCATE()
!
! CALL OK(tsize .EQ. 4, "Real32 tsize")
! CALL OK(ALL(ABS(rbuf(1:tsize) - rv) .LT. tol), "Real32 values")
! CALL OK((nrow .EQ. 2) .AND. (ncol .EQ. 3), "integer nrow, ncol")
! CALL OK(ALL(ibuf(1:nrow, 1:ncol) .EQ. iv), "integer values")
! END PROGRAM main
!```
!
! See also: [[obj_Read_s1]]

INTERFACE
  MODULE SUBROUTINE obj_Read__s1(obj, dsetname, vals, tsize)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    REAL(REAL32), INTENT(INOUT) :: vals(:)
    !! vals will be filled with entries read from the dataset
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of data written in vals
  END SUBROUTINE obj_Read__s1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Read a rank-2 array of Real32
!
! Read a rank-2 array of Real32 from the dataset dsetname. vals is allocated
! to the shape of the dataset; it is reallocated if it has another shape.
!
! In DEBUG mode an error is raised if the file is not open with read access,
! the dataset can not be opened, or its rank is not the rank of vals.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : REAL(REAL32), ALLOCATABLE - data read from the dataset
!
! See also: [[obj_Read_s1]], [[obj_Write_s2]], [[obj_Read__s2]]

INTERFACE
  MODULE SUBROUTINE obj_Read_s2(obj, dsetname, vals)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: vals(:, :)
    !! a rank-2 array of Real32 read from the dataset
  END SUBROUTINE obj_Read_s2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Read a rank-2 array of Real32 without allocating vals
!
! Read a rank-2 array of Real32 from the dataset dsetname into vals without
! allocating vals. The extent of the data is returned in nrow, ncol; the data
! is stored in vals(1:nrow, 1:ncol), and the rest of vals is not changed.
!
! The extents are set to zero at the start. vals must be large enough to hold
! the dataset along every dimension; in DEBUG mode an error is raised
! otherwise. In release builds this is not checked.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : REAL(REAL32) - array to fill, SIZE(vals, i) >= extent i
!   nrow : INTEGER(I4B) - number of rows written in vals
!   ncol : INTEGER(I4B) - number of columns written in vals
!
! See also: [[obj_Read__s1]], [[obj_Read_s2]]

INTERFACE
  MODULE SUBROUTINE obj_Read__s2(obj, dsetname, vals, nrow, ncol)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    REAL(REAL32), INTENT(INOUT) :: vals(:, :)
    !! vals will be filled with entries read from the dataset
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! number of rows written in vals
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! number of columns written in vals
  END SUBROUTINE obj_Read__s2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Read a rank-3 array of Real32
!
! Read a rank-3 array of Real32 from the dataset dsetname. vals is allocated
! to the shape of the dataset; it is reallocated if it has another shape.
!
! In DEBUG mode an error is raised if the file is not open with read access,
! the dataset can not be opened, or its rank is not the rank of vals.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : REAL(REAL32), ALLOCATABLE - data read from the dataset
!
! See also: [[obj_Read_s1]], [[obj_Write_s3]], [[obj_Read__s3]]

INTERFACE
  MODULE SUBROUTINE obj_Read_s3(obj, dsetname, vals)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: vals(:, :, :)
    !! a rank-3 array of Real32 read from the dataset
  END SUBROUTINE obj_Read_s3
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Read a rank-3 array of Real32 without allocating vals
!
! Read a rank-3 array of Real32 from the dataset dsetname into vals without
! allocating vals. The extent of the data is returned in dim1, dim2, dim3; the
! data is stored in vals(1:dim1, ..., 1:dim3), and the rest of vals is not
! changed.
!
! The extents are set to zero at the start. vals must be large enough to hold
! the dataset along every dimension; in DEBUG mode an error is raised
! otherwise. In release builds this is not checked.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : REAL(REAL32) - array to fill, SIZE(vals, i) >= extent i
!   dim1, ..., dim3 : INTEGER(I4B) - extent of data along each dimension
!
! See also: [[obj_Read__s1]], [[obj_Read_s3]]

INTERFACE
  MODULE SUBROUTINE obj_Read__s3(obj, dsetname, vals, dim1, dim2, dim3)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    REAL(REAL32), INTENT(INOUT) :: vals(:, :, :)
    !! vals will be filled with entries read from the dataset
    INTEGER(I4B), INTENT(OUT) :: dim1
    !! extent of data written in vals along dimension 1
    INTEGER(I4B), INTENT(OUT) :: dim2
    !! extent of data written in vals along dimension 2
    INTEGER(I4B), INTENT(OUT) :: dim3
    !! extent of data written in vals along dimension 3
  END SUBROUTINE obj_Read__s3
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Read a rank-4 array of Real32
!
! Read a rank-4 array of Real32 from the dataset dsetname. vals is allocated
! to the shape of the dataset; it is reallocated if it has another shape.
!
! In DEBUG mode an error is raised if the file is not open with read access,
! the dataset can not be opened, or its rank is not the rank of vals.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : REAL(REAL32), ALLOCATABLE - data read from the dataset
!
! See also: [[obj_Read_s1]], [[obj_Write_s4]], [[obj_Read__s4]]

INTERFACE
  MODULE SUBROUTINE obj_Read_s4(obj, dsetname, vals)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: vals(:, :, :, :)
    !! a rank-4 array of Real32 read from the dataset
  END SUBROUTINE obj_Read_s4
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Read a rank-4 array of Real32 without allocating vals
!
! Read a rank-4 array of Real32 from the dataset dsetname into vals without
! allocating vals. The extent of the data is returned in dim1, dim2, dim3,
! dim4; the data is stored in vals(1:dim1, ..., 1:dim4), and the rest of vals
! is not changed.
!
! The extents are set to zero at the start. vals must be large enough to hold
! the dataset along every dimension; in DEBUG mode an error is raised
! otherwise. In release builds this is not checked.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : REAL(REAL32) - array to fill, SIZE(vals, i) >= extent i
!   dim1, ..., dim4 : INTEGER(I4B) - extent of data along each dimension
!
! See also: [[obj_Read__s1]], [[obj_Read_s4]]

INTERFACE
  MODULE SUBROUTINE obj_Read__s4(obj, dsetname, vals, dim1, dim2, dim3, &
                                 dim4)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    REAL(REAL32), INTENT(INOUT) :: vals(:, :, :, :)
    !! vals will be filled with entries read from the dataset
    INTEGER(I4B), INTENT(OUT) :: dim1
    !! extent of data written in vals along dimension 1
    INTEGER(I4B), INTENT(OUT) :: dim2
    !! extent of data written in vals along dimension 2
    INTEGER(I4B), INTENT(OUT) :: dim3
    !! extent of data written in vals along dimension 3
    INTEGER(I4B), INTENT(OUT) :: dim4
    !! extent of data written in vals along dimension 4
  END SUBROUTINE obj_Read__s4
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Read a rank-5 array of Real32
!
! Read a rank-5 array of Real32 from the dataset dsetname. vals is allocated
! to the shape of the dataset; it is reallocated if it has another shape.
!
! In DEBUG mode an error is raised if the file is not open with read access,
! the dataset can not be opened, or its rank is not the rank of vals.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : REAL(REAL32), ALLOCATABLE - data read from the dataset
!
! See also: [[obj_Read_s1]], [[obj_Write_s5]], [[obj_Read__s5]]

INTERFACE
  MODULE SUBROUTINE obj_Read_s5(obj, dsetname, vals)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: vals(:, :, :, :, :)
    !! a rank-5 array of Real32 read from the dataset
  END SUBROUTINE obj_Read_s5
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Read a rank-5 array of Real32 without allocating vals
!
! Read a rank-5 array of Real32 from the dataset dsetname into vals without
! allocating vals. The extent of the data is returned in dim1, dim2, dim3,
! dim4, dim5; the data is stored in vals(1:dim1, ..., 1:dim5), and the rest of
! vals is not changed.
!
! The extents are set to zero at the start. vals must be large enough to hold
! the dataset along every dimension; in DEBUG mode an error is raised
! otherwise. In release builds this is not checked.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : REAL(REAL32) - array to fill, SIZE(vals, i) >= extent i
!   dim1, ..., dim5 : INTEGER(I4B) - extent of data along each dimension
!
! See also: [[obj_Read__s1]], [[obj_Read_s5]]

INTERFACE
  MODULE SUBROUTINE obj_Read__s5(obj, dsetname, vals, dim1, dim2, dim3, &
                                 dim4, dim5)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    REAL(REAL32), INTENT(INOUT) :: vals(:, :, :, :, :)
    !! vals will be filled with entries read from the dataset
    INTEGER(I4B), INTENT(OUT) :: dim1
    !! extent of data written in vals along dimension 1
    INTEGER(I4B), INTENT(OUT) :: dim2
    !! extent of data written in vals along dimension 2
    INTEGER(I4B), INTENT(OUT) :: dim3
    !! extent of data written in vals along dimension 3
    INTEGER(I4B), INTENT(OUT) :: dim4
    !! extent of data written in vals along dimension 4
    INTEGER(I4B), INTENT(OUT) :: dim5
    !! extent of data written in vals along dimension 5
  END SUBROUTINE obj_Read__s5
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Read a rank-6 array of Real32
!
! Read a rank-6 array of Real32 from the dataset dsetname. vals is allocated
! to the shape of the dataset; it is reallocated if it has another shape.
!
! In DEBUG mode an error is raised if the file is not open with read access,
! the dataset can not be opened, or its rank is not the rank of vals.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : REAL(REAL32), ALLOCATABLE - data read from the dataset
!
! See also: [[obj_Read_s1]], [[obj_Write_s6]], [[obj_Read__s6]]

INTERFACE
  MODULE SUBROUTINE obj_Read_s6(obj, dsetname, vals)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: vals(:, :, :, :, :, :)
    !! a rank-6 array of Real32 read from the dataset
  END SUBROUTINE obj_Read_s6
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Read a rank-6 array of Real32 without allocating vals
!
! Read a rank-6 array of Real32 from the dataset dsetname into vals without
! allocating vals. The extent of the data is returned in dim1, dim2, dim3,
! dim4, dim5, dim6; the data is stored in vals(1:dim1, ..., 1:dim6), and the
! rest of vals is not changed.
!
! The extents are set to zero at the start. vals must be large enough to hold
! the dataset along every dimension; in DEBUG mode an error is raised
! otherwise. In release builds this is not checked.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : REAL(REAL32) - array to fill, SIZE(vals, i) >= extent i
!   dim1, ..., dim6 : INTEGER(I4B) - extent of data along each dimension
!
! See also: [[obj_Read__s1]], [[obj_Read_s6]]

INTERFACE
  MODULE SUBROUTINE obj_Read__s6(obj, dsetname, vals, dim1, dim2, dim3, &
                                 dim4, dim5, dim6)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    REAL(REAL32), INTENT(INOUT) :: vals(:, :, :, :, :, :)
    !! vals will be filled with entries read from the dataset
    INTEGER(I4B), INTENT(OUT) :: dim1
    !! extent of data written in vals along dimension 1
    INTEGER(I4B), INTENT(OUT) :: dim2
    !! extent of data written in vals along dimension 2
    INTEGER(I4B), INTENT(OUT) :: dim3
    !! extent of data written in vals along dimension 3
    INTEGER(I4B), INTENT(OUT) :: dim4
    !! extent of data written in vals along dimension 4
    INTEGER(I4B), INTENT(OUT) :: dim5
    !! extent of data written in vals along dimension 5
    INTEGER(I4B), INTENT(OUT) :: dim6
    !! extent of data written in vals along dimension 6
  END SUBROUTINE obj_Read__s6
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Read a rank-7 array of Real32
!
! Read a rank-7 array of Real32 from the dataset dsetname. vals is allocated
! to the shape of the dataset; it is reallocated if it has another shape.
!
! In DEBUG mode an error is raised if the file is not open with read access,
! the dataset can not be opened, or its rank is not the rank of vals.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : REAL(REAL32), ALLOCATABLE - data read from the dataset
!
! See also: [[obj_Read_s1]], [[obj_Write_s7]], [[obj_Read__s7]]

INTERFACE
  MODULE SUBROUTINE obj_Read_s7(obj, dsetname, vals)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: vals(:, :, :, :, :, :, :)
    !! a rank-7 array of Real32 read from the dataset
  END SUBROUTINE obj_Read_s7
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Read a rank-7 array of Real32 without allocating vals
!
! Read a rank-7 array of Real32 from the dataset dsetname into vals without
! allocating vals. The extent of the data is returned in dim1, dim2, dim3,
! dim4, dim5, dim6, dim7; the data is stored in vals(1:dim1, ..., 1:dim7), and
! the rest of vals is not changed.
!
! The extents are set to zero at the start. vals must be large enough to hold
! the dataset along every dimension; in DEBUG mode an error is raised
! otherwise. In release builds this is not checked.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : REAL(REAL32) - array to fill, SIZE(vals, i) >= extent i
!   dim1, ..., dim7 : INTEGER(I4B) - extent of data along each dimension
!
! See also: [[obj_Read__s1]], [[obj_Read_s7]]

INTERFACE
  MODULE SUBROUTINE obj_Read__s7(obj, dsetname, vals, dim1, dim2, dim3, &
                                 dim4, dim5, dim6, dim7)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    REAL(REAL32), INTENT(INOUT) :: vals(:, :, :, :, :, :, :)
    !! vals will be filled with entries read from the dataset
    INTEGER(I4B), INTENT(OUT) :: dim1
    !! extent of data written in vals along dimension 1
    INTEGER(I4B), INTENT(OUT) :: dim2
    !! extent of data written in vals along dimension 2
    INTEGER(I4B), INTENT(OUT) :: dim3
    !! extent of data written in vals along dimension 3
    INTEGER(I4B), INTENT(OUT) :: dim4
    !! extent of data written in vals along dimension 4
    INTEGER(I4B), INTENT(OUT) :: dim5
    !! extent of data written in vals along dimension 5
    INTEGER(I4B), INTENT(OUT) :: dim6
    !! extent of data written in vals along dimension 6
    INTEGER(I4B), INTENT(OUT) :: dim7
    !! extent of data written in vals along dimension 7
  END SUBROUTINE obj_Read__s7
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Read
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Read a scalar of Int32
!
! Read a scalar of Int32 from the dataset dsetname.
!
! In DEBUG mode an error is raised if the file is not open with read access,
! the dataset can not be opened, or its rank is not the rank of vals.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : INTEGER(I4B) - data read from the dataset
!
! See also: [[obj_Read_n1]], [[obj_Write_n0]]

INTERFACE
  MODULE SUBROUTINE obj_Read_n0(obj, dsetname, vals)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    INTEGER(I4B), INTENT(INOUT) :: vals
    !! a scalar of Int32 read from the dataset
  END SUBROUTINE obj_Read_n0
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Read a rank-1 array of Int32
!
! Read a rank-1 array of Int32 from the dataset dsetname. vals is allocated to
! the shape of the dataset; it is reallocated if it has another shape.
!
! In DEBUG mode an error is raised if the file is not open with read access,
! the dataset can not be opened, or its rank is not the rank of vals.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : INTEGER(I4B), ALLOCATABLE - data read from the dataset
!
! See also: [[obj_Write_n1]], [[obj_Write_n1]], [[obj_Read__n1]]

INTERFACE
  MODULE SUBROUTINE obj_Read_n1(obj, dsetname, vals)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: vals(:)
    !! a rank-1 array of Int32 read from the dataset
  END SUBROUTINE obj_Read_n1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Read a rank-1 array of integer without allocating vals
!
! Read a rank-1 array of integer from the dataset dsetname into vals without
! allocating vals. The extent of the data is returned in tsize; the data is
! stored in vals(1:tsize), and the rest of vals is not changed.
!
! The extents are set to zero at the start. vals must be large enough to hold
! the dataset along every dimension; in DEBUG mode an error is raised
! otherwise. In release builds this is not checked.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : INTEGER(I4B) - array to fill, SIZE(vals, i) >= extent i
!   tsize : INTEGER(I4B) - size of data written in vals
!
! See also: [[obj_Read__s1]], [[obj_Read_n1]]

INTERFACE
  MODULE SUBROUTINE obj_Read__n1(obj, dsetname, vals, tsize)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    INTEGER(I4B), INTENT(INOUT) :: vals(:)
    !! vals will be filled with entries read from the dataset
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of data written in vals
  END SUBROUTINE obj_Read__n1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Read a rank-2 array of Int32
!
! Read a rank-2 array of Int32 from the dataset dsetname. vals is allocated to
! the shape of the dataset; it is reallocated if it has another shape.
!
! In DEBUG mode an error is raised if the file is not open with read access,
! the dataset can not be opened, or its rank is not the rank of vals.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : INTEGER(I4B), ALLOCATABLE - data read from the dataset
!
! See also: [[obj_Read_n1]], [[obj_Write_n2]], [[obj_Read__n2]]

INTERFACE
  MODULE SUBROUTINE obj_Read_n2(obj, dsetname, vals)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: vals(:, :)
    !! a rank-2 array of Int32 read from the dataset
  END SUBROUTINE obj_Read_n2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Read a rank-2 array of integer without allocating vals
!
! Read a rank-2 array of integer from the dataset dsetname into vals without
! allocating vals. The extent of the data is returned in nrow, ncol; the data
! is stored in vals(1:nrow, 1:ncol), and the rest of vals is not changed.
!
! The extents are set to zero at the start. vals must be large enough to hold
! the dataset along every dimension; in DEBUG mode an error is raised
! otherwise. In release builds this is not checked.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : INTEGER(I4B) - array to fill, SIZE(vals, i) >= extent i
!   nrow : INTEGER(I4B) - number of rows written in vals
!   ncol : INTEGER(I4B) - number of columns written in vals
!
! See also: [[obj_Read__s1]], [[obj_Read_n2]]

INTERFACE
  MODULE SUBROUTINE obj_Read__n2(obj, dsetname, vals, nrow, ncol)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    INTEGER(I4B), INTENT(INOUT) :: vals(:, :)
    !! vals will be filled with entries read from the dataset
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! number of rows written in vals
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! number of columns written in vals
  END SUBROUTINE obj_Read__n2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Read a rank-3 array of Int32
!
! Read a rank-3 array of Int32 from the dataset dsetname. vals is allocated to
! the shape of the dataset; it is reallocated if it has another shape.
!
! In DEBUG mode an error is raised if the file is not open with read access,
! the dataset can not be opened, or its rank is not the rank of vals.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : INTEGER(I4B), ALLOCATABLE - data read from the dataset
!
! See also: [[obj_Read_n1]], [[obj_Write_n3]], [[obj_Read__n3]]

INTERFACE
  MODULE SUBROUTINE obj_Read_n3(obj, dsetname, vals)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: vals(:, :, :)
    !! a rank-3 array of Int32 read from the dataset
  END SUBROUTINE obj_Read_n3
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Read a rank-3 array of integer without allocating vals
!
! Read a rank-3 array of integer from the dataset dsetname into vals without
! allocating vals. The extent of the data is returned in dim1, dim2, dim3; the
! data is stored in vals(1:dim1, ..., 1:dim3), and the rest of vals is not
! changed.
!
! The extents are set to zero at the start. vals must be large enough to hold
! the dataset along every dimension; in DEBUG mode an error is raised
! otherwise. In release builds this is not checked.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : INTEGER(I4B) - array to fill, SIZE(vals, i) >= extent i
!   dim1, ..., dim3 : INTEGER(I4B) - extent of data along each dimension
!
! See also: [[obj_Read__s1]], [[obj_Read_n3]]

INTERFACE
  MODULE SUBROUTINE obj_Read__n3(obj, dsetname, vals, dim1, dim2, dim3)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    INTEGER(I4B), INTENT(INOUT) :: vals(:, :, :)
    !! vals will be filled with entries read from the dataset
    INTEGER(I4B), INTENT(OUT) :: dim1
    !! extent of data written in vals along dimension 1
    INTEGER(I4B), INTENT(OUT) :: dim2
    !! extent of data written in vals along dimension 2
    INTEGER(I4B), INTENT(OUT) :: dim3
    !! extent of data written in vals along dimension 3
  END SUBROUTINE obj_Read__n3
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Read a rank-4 array of Int32
!
! Read a rank-4 array of Int32 from the dataset dsetname. vals is allocated to
! the shape of the dataset; it is reallocated if it has another shape.
!
! In DEBUG mode an error is raised if the file is not open with read access,
! the dataset can not be opened, or its rank is not the rank of vals.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : INTEGER(I4B), ALLOCATABLE - data read from the dataset
!
! See also: [[obj_Read_n1]], [[obj_Write_n4]], [[obj_Read__n4]]

INTERFACE
  MODULE SUBROUTINE obj_Read_n4(obj, dsetname, vals)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: vals(:, :, :, :)
    !! a rank-4 array of Int32 read from the dataset
  END SUBROUTINE obj_Read_n4
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Read a rank-4 array of integer without allocating vals
!
! Read a rank-4 array of integer from the dataset dsetname into vals without
! allocating vals. The extent of the data is returned in dim1, dim2, dim3,
! dim4; the data is stored in vals(1:dim1, ..., 1:dim4), and the rest of vals
! is not changed.
!
! The extents are set to zero at the start. vals must be large enough to hold
! the dataset along every dimension; in DEBUG mode an error is raised
! otherwise. In release builds this is not checked.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : INTEGER(I4B) - array to fill, SIZE(vals, i) >= extent i
!   dim1, ..., dim4 : INTEGER(I4B) - extent of data along each dimension
!
! See also: [[obj_Read__s1]], [[obj_Read_n4]]

INTERFACE
  MODULE SUBROUTINE obj_Read__n4(obj, dsetname, vals, dim1, dim2, dim3, &
                                 dim4)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    INTEGER(I4B), INTENT(INOUT) :: vals(:, :, :, :)
    !! vals will be filled with entries read from the dataset
    INTEGER(I4B), INTENT(OUT) :: dim1
    !! extent of data written in vals along dimension 1
    INTEGER(I4B), INTENT(OUT) :: dim2
    !! extent of data written in vals along dimension 2
    INTEGER(I4B), INTENT(OUT) :: dim3
    !! extent of data written in vals along dimension 3
    INTEGER(I4B), INTENT(OUT) :: dim4
    !! extent of data written in vals along dimension 4
  END SUBROUTINE obj_Read__n4
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Read a rank-5 array of Int32
!
! Read a rank-5 array of Int32 from the dataset dsetname. vals is allocated to
! the shape of the dataset; it is reallocated if it has another shape.
!
! In DEBUG mode an error is raised if the file is not open with read access,
! the dataset can not be opened, or its rank is not the rank of vals.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : INTEGER(I4B), ALLOCATABLE - data read from the dataset
!
! See also: [[obj_Read_n1]], [[obj_Write_n5]], [[obj_Read__n5]]

INTERFACE
  MODULE SUBROUTINE obj_Read_n5(obj, dsetname, vals)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: vals(:, :, :, :, :)
    !! a rank-5 array of Int32 read from the dataset
  END SUBROUTINE obj_Read_n5
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Read a rank-5 array of integer without allocating vals
!
! Read a rank-5 array of integer from the dataset dsetname into vals without
! allocating vals. The extent of the data is returned in dim1, dim2, dim3,
! dim4, dim5; the data is stored in vals(1:dim1, ..., 1:dim5), and the rest of
! vals is not changed.
!
! The extents are set to zero at the start. vals must be large enough to hold
! the dataset along every dimension; in DEBUG mode an error is raised
! otherwise. In release builds this is not checked.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : INTEGER(I4B) - array to fill, SIZE(vals, i) >= extent i
!   dim1, ..., dim5 : INTEGER(I4B) - extent of data along each dimension
!
! See also: [[obj_Read__s1]], [[obj_Read_n5]]

INTERFACE
  MODULE SUBROUTINE obj_Read__n5(obj, dsetname, vals, dim1, dim2, dim3, &
                                 dim4, dim5)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    INTEGER(I4B), INTENT(INOUT) :: vals(:, :, :, :, :)
    !! vals will be filled with entries read from the dataset
    INTEGER(I4B), INTENT(OUT) :: dim1
    !! extent of data written in vals along dimension 1
    INTEGER(I4B), INTENT(OUT) :: dim2
    !! extent of data written in vals along dimension 2
    INTEGER(I4B), INTENT(OUT) :: dim3
    !! extent of data written in vals along dimension 3
    INTEGER(I4B), INTENT(OUT) :: dim4
    !! extent of data written in vals along dimension 4
    INTEGER(I4B), INTENT(OUT) :: dim5
    !! extent of data written in vals along dimension 5
  END SUBROUTINE obj_Read__n5
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Read a rank-6 array of Int32
!
! Read a rank-6 array of Int32 from the dataset dsetname. vals is allocated to
! the shape of the dataset; it is reallocated if it has another shape.
!
! In DEBUG mode an error is raised if the file is not open with read access,
! the dataset can not be opened, or its rank is not the rank of vals.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : INTEGER(I4B), ALLOCATABLE - data read from the dataset
!
! See also: [[obj_Read_n1]], [[obj_Write_n6]], [[obj_Read__n6]]

INTERFACE
  MODULE SUBROUTINE obj_Read_n6(obj, dsetname, vals)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: vals(:, :, :, :, :, :)
    !! a rank-6 array of Int32 read from the dataset
  END SUBROUTINE obj_Read_n6
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Read a rank-6 array of integer without allocating vals
!
! Read a rank-6 array of integer from the dataset dsetname into vals without
! allocating vals. The extent of the data is returned in dim1, dim2, dim3,
! dim4, dim5, dim6; the data is stored in vals(1:dim1, ..., 1:dim6), and the
! rest of vals is not changed.
!
! The extents are set to zero at the start. vals must be large enough to hold
! the dataset along every dimension; in DEBUG mode an error is raised
! otherwise. In release builds this is not checked.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : INTEGER(I4B) - array to fill, SIZE(vals, i) >= extent i
!   dim1, ..., dim6 : INTEGER(I4B) - extent of data along each dimension
!
! See also: [[obj_Read__s1]], [[obj_Read_n6]]

INTERFACE
  MODULE SUBROUTINE obj_Read__n6(obj, dsetname, vals, dim1, dim2, dim3, &
                                 dim4, dim5, dim6)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    INTEGER(I4B), INTENT(INOUT) :: vals(:, :, :, :, :, :)
    !! vals will be filled with entries read from the dataset
    INTEGER(I4B), INTENT(OUT) :: dim1
    !! extent of data written in vals along dimension 1
    INTEGER(I4B), INTENT(OUT) :: dim2
    !! extent of data written in vals along dimension 2
    INTEGER(I4B), INTENT(OUT) :: dim3
    !! extent of data written in vals along dimension 3
    INTEGER(I4B), INTENT(OUT) :: dim4
    !! extent of data written in vals along dimension 4
    INTEGER(I4B), INTENT(OUT) :: dim5
    !! extent of data written in vals along dimension 5
    INTEGER(I4B), INTENT(OUT) :: dim6
    !! extent of data written in vals along dimension 6
  END SUBROUTINE obj_Read__n6
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-07
! summary: Read a rank-7 array of Int32
!
! Read a rank-7 array of Int32 from the dataset dsetname. vals is allocated to
! the shape of the dataset; it is reallocated if it has another shape.
!
! In DEBUG mode an error is raised if the file is not open with read access,
! the dataset can not be opened, or its rank is not the rank of vals.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : INTEGER(I4B), ALLOCATABLE - data read from the dataset
!
! See also: [[obj_Read_n1]], [[obj_Write_n7]], [[obj_Read__n7]]

INTERFACE
  MODULE SUBROUTINE obj_Read_n7(obj, dsetname, vals)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: vals(:, :, :, :, :, :, :)
    !! a rank-7 array of Int32 read from the dataset
  END SUBROUTINE obj_Read_n7
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Read a rank-7 array of integer without allocating vals
!
! Read a rank-7 array of integer from the dataset dsetname into vals without
! allocating vals. The extent of the data is returned in dim1, dim2, dim3,
! dim4, dim5, dim6, dim7; the data is stored in vals(1:dim1, ..., 1:dim7), and
! the rest of vals is not changed.
!
! The extents are set to zero at the start. vals must be large enough to hold
! the dataset along every dimension; in DEBUG mode an error is raised
! otherwise. In release builds this is not checked.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : INTEGER(I4B) - array to fill, SIZE(vals, i) >= extent i
!   dim1, ..., dim7 : INTEGER(I4B) - extent of data along each dimension
!
! See also: [[obj_Read__s1]], [[obj_Read_n7]]

INTERFACE
  MODULE SUBROUTINE obj_Read__n7(obj, dsetname, vals, dim1, dim2, dim3, &
                                 dim4, dim5, dim6, dim7)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    INTEGER(I4B), INTENT(INOUT) :: vals(:, :, :, :, :, :, :)
    !! vals will be filled with entries read from the dataset
    INTEGER(I4B), INTENT(OUT) :: dim1
    !! extent of data written in vals along dimension 1
    INTEGER(I4B), INTENT(OUT) :: dim2
    !! extent of data written in vals along dimension 2
    INTEGER(I4B), INTENT(OUT) :: dim3
    !! extent of data written in vals along dimension 3
    INTEGER(I4B), INTENT(OUT) :: dim4
    !! extent of data written in vals along dimension 4
    INTEGER(I4B), INTENT(OUT) :: dim5
    !! extent of data written in vals along dimension 5
    INTEGER(I4B), INTENT(OUT) :: dim6
    !! extent of data written in vals along dimension 6
    INTEGER(I4B), INTENT(OUT) :: dim7
    !! extent of data written in vals along dimension 7
  END SUBROUTINE obj_Read__n7
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Read
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-08
! summary: Read a scalar of String
!
! Read a scalar of String from the dataset dsetname. Strings can be stored in
! two ways, and this routine finds out which one is used:
!
! - as fixed-length strings, written by Write with String data; then
!   obj_Read_st0 is called with the length of the stored strings.
! - as an array of single characters, written by Write with a character
!   variable; then obj_Read_ca0 is called.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : TYPE(String) - data read from the dataset
!
! See also: [[obj_Read_st2_helper]], [[obj_Write_st0]]

INTERFACE
  MODULE SUBROUTINE obj_Read_st0_helper(obj, dsetname, vals)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    TYPE(String), INTENT(INOUT) :: vals
    !! a scalar of String read from the dataset
  END SUBROUTINE obj_Read_st0_helper
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Read
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-08
! summary: Read a scalar of String of fixed length
!
! Read a scalar of String stored as fixed-length strings of length length_max.
! C null characters are removed.
!
! In DEBUG mode an error is raised if the file is not open with read access,
! the dataset can not be opened, or its rank is not the rank of vals.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   length_max : INTEGER(I4B) - length of the stored strings
!   vals : TYPE(String) - data read from the dataset
!
! See also: [[obj_Read_st0_helper]]

INTERFACE
  MODULE SUBROUTINE obj_Read_st0(obj, dsetname, length_max, vals)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    INTEGER(I4B), INTENT(IN) :: length_max
    !! length of the stored strings
    TYPE(String), INTENT(INOUT) :: vals
    !! a scalar of String read from the dataset
  END SUBROUTINE obj_Read_st0
END INTERFACE

!----------------------------------------------------------------------------
!                                                                     Read
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-08
! summary: Read a scalar of String stored as single characters
!
! Read a rank-1 array of single characters into one String. This routine is
! used by obj_Read_st0_helper and is not bound to the READ generic.
!
! In DEBUG mode an error is raised if the file is not open with read access,
! the dataset can not be opened, or its rank is not the rank of vals plus one.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : TYPE(String) - data read from the dataset
!
! See also: [[obj_Read_st0_helper]]

INTERFACE
  MODULE SUBROUTINE obj_Read_ca0(obj, dsetname, vals)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    TYPE(String), INTENT(INOUT) :: vals
    !! a scalar of String read from the dataset
  END SUBROUTINE obj_Read_ca0
END INTERFACE

!----------------------------------------------------------------------------
!                                                                       Read
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-08
! summary: Read a rank-1 array of String
!
! Read a rank-1 array of String from the dataset dsetname. Strings can be
! stored in two ways, and this routine finds out which one is used:
!
! - as fixed-length strings, written by Write with String data; then
!   obj_Read_st1 is called with the length of the stored strings.
! - as an array of single characters, written by Write with a character
!   variable; then obj_Read_ca1 is called.
!
! vals is allocated to the shape of the dataset; it is reallocated if any
! dimension differs.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : TYPE(String), ALLOCATABLE - data read from the dataset
!
! See also: [[obj_Read_st2_helper]], [[obj_Write_st1_helper]],
!   [[obj_Read__st1]]

INTERFACE
  MODULE SUBROUTINE obj_Read_st1_helper(obj, dsetname, vals)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    TYPE(String), ALLOCATABLE, INTENT(INOUT) :: vals(:)
    !! a rank-1 array of String read from the dataset
  END SUBROUTINE obj_Read_st1_helper
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Read
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Read a rank-1 array of String of fixed length
!
! Read a rank-1 array of String stored as fixed-length strings of length
! length_max. C null characters are removed. vals is allocated to the shape of
! the dataset; it is reallocated if any dimension differs.
!
! In DEBUG mode an error is raised if the file is not open with read access,
! the dataset can not be opened, or its rank is not the rank of vals.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   length_max : INTEGER(I4B) - length of the stored strings
!   vals : TYPE(String), ALLOCATABLE - data read from the dataset
!
! See also: [[obj_Read_st1_helper]]

INTERFACE
  MODULE SUBROUTINE obj_Read_st1(obj, dsetname, length_max, vals)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    INTEGER(I4B), INTENT(IN) :: length_max
    !! length of the stored strings
    TYPE(String), ALLOCATABLE, INTENT(INOUT) :: vals(:)
    !! a rank-1 array of String read from the dataset
  END SUBROUTINE obj_Read_st1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Read
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Read a rank-1 array of String stored as single characters
!
! Read a rank-2 array of single characters into a rank-1 array of String; the
! first dimension holds the characters of each string. This routine is used by
! obj_Read_st1_helper and is not bound to the READ generic.
!
! In DEBUG mode an error is raised if the file is not open with read access,
! the dataset can not be opened, or its rank is not the rank of vals plus one.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : TYPE(String), ALLOCATABLE - data read from the dataset
!
! See also: [[obj_Read_st1_helper]]

INTERFACE
  MODULE SUBROUTINE obj_Read_ca1(obj, dsetname, vals)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    TYPE(String), ALLOCATABLE, INTENT(INOUT) :: vals(:)
    !! a rank-1 array of String read from the dataset
  END SUBROUTINE obj_Read_ca1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Read a rank-1 array of String without allocating vals
!
! Read a rank-1 array of String from the dataset dsetname into vals without
! allocating vals. The extent of the data is returned in tsize; the data is
! stored in vals(1:tsize), and the rest of vals is not changed. The data is
! read with obj_Read_st1_helper in a local array and copied to vals.
!
! The extents are set to zero at the start. vals must be large enough to hold
! the dataset along every dimension; in DEBUG mode an error is raised
! otherwise. In release builds this is not checked.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : TYPE(String) - array to fill, SIZE(vals, i) >= extent i
!   tsize : INTEGER(I4B) - size of data written in vals
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE HDF5File_Class, ONLY: HDF5File_
! USE String_Class, ONLY: String
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: filename = "Read__test_4.h5"
! TYPE(HDF5File_) :: obj
! TYPE(String) :: v(2), m(2, 2)
! TYPE(String) :: vbuf(5), mbuf(3, 3)
! INTEGER(I4B) :: tsize, nrow, ncol
! LOGICAL(LGT) :: isok
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! v(1) = "hello"
! v(2) = "world"
! m(1, 1) = "a"
! m(2, 1) = "b"
! m(1, 2) = "c"
! m(2, 2) = "d"
!
! CALL obj%Initiate(filename=filename, mode="NEW")
! CALL obj%OPEN()
! CALL obj%WRITE(dsetname="/v", vals=v)
! CALL obj%WRITE(dsetname="/m", vals=m)
! CALL obj%DEALLOCATE()
!
! CALL obj%Initiate(filename=filename, mode="READ")
! CALL obj%OPEN()
! CALL obj%READ_(dsetname="/v", vals=vbuf, tsize=tsize)
! CALL obj%READ_(dsetname="/m", vals=mbuf, nrow=nrow, ncol=ncol)
! CALL obj%DEALLOCATE()
!
! CALL OK(tsize .EQ. 2, "rank 1 tsize")
! isok = (vbuf(1)%chars() .EQ. "hello") .AND. &
!        (vbuf(2)%chars() .EQ. "world")
! CALL OK(isok, "rank 1 values")
! CALL OK((nrow .EQ. 2) .AND. (ncol .EQ. 2), "rank 2 nrow, ncol")
! isok = (mbuf(1, 1)%chars() .EQ. "a") .AND. &
!        (mbuf(2, 2)%chars() .EQ. "d")
! CALL OK(isok, "rank 2 values")
! END PROGRAM main
!```
!
! See also: [[obj_Read_st1_helper]]

INTERFACE
  MODULE SUBROUTINE obj_Read__st1(obj, dsetname, vals, tsize)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    TYPE(String), INTENT(INOUT) :: vals(:)
    !! vals will be filled with entries read from the dataset
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of data written in vals
  END SUBROUTINE obj_Read__st1
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Read a rank-2 array of String
!
! Read a rank-2 array of String from the dataset dsetname. Strings can be
! stored in two ways, and this routine finds out which one is used:
!
! - as fixed-length strings, written by Write with String data; then
!   obj_Read_st2 is called with the length of the stored strings.
! - as an array of single characters, written by Write with a character
!   variable; then obj_Read_ca2 is called.
!
! vals is allocated to the shape of the dataset; it is reallocated if any
! dimension differs.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : TYPE(String), ALLOCATABLE - data read from the dataset
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE GlobalData, ONLY: LGT
! USE HDF5File_Class, ONLY: HDF5File_
! USE String_Class, ONLY: String
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: filename = "Read_test_2.h5"
! TYPE(HDF5File_) :: obj
! TYPE(String) :: m(2, 3)
! TYPE(String), ALLOCATABLE :: m1(:, :)
! LOGICAL(LGT) :: isok
! INTEGER :: i, j
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! DO j = 1, 3
!   DO i = 1, 2
!     m(i, j) = "x"
!   END DO
! END DO
! m(2, 3) = "y"
!
! CALL obj%Initiate(filename=filename, mode="NEW")
! CALL obj%OPEN()
! CALL obj%WRITE(dsetname="/m", vals=m)
! CALL obj%DEALLOCATE()
!
! ! only the second dimension differs from the dataset
! ALLOCATE (m1(2, 5))
! CALL obj%Initiate(filename=filename, mode="READ")
! CALL obj%OPEN()
! CALL obj%READ(dsetname="/m", vals=m1)
! CALL obj%DEALLOCATE()
!
! CALL OK(ALL(SHAPE(m1) .EQ. [2, 3]), "reallocated")
! isok = (m1(1, 1)%chars() .EQ. "x") .AND. (m1(2, 3)%chars() .EQ. "y")
! CALL OK(isok, "values")
! END PROGRAM main
!```
!
! See also: [[obj_Write_st2_helper]], [[obj_Read__st2]]

INTERFACE
  MODULE SUBROUTINE obj_Read_st2_helper(obj, dsetname, vals)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    TYPE(String), ALLOCATABLE, INTENT(INOUT) :: vals(:, :)
    !! a rank-2 array of String read from the dataset
  END SUBROUTINE obj_Read_st2_helper
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Read a rank-2 array of String of fixed length
!
! Read a rank-2 array of String stored as fixed-length strings of length
! length_max. C null characters are removed. vals is allocated to the shape of
! the dataset; it is reallocated if any dimension differs.
!
! In DEBUG mode an error is raised if the file is not open with read access,
! the dataset can not be opened, or its rank is not the rank of vals.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   length_max : INTEGER(I4B) - length of the stored strings
!   vals : TYPE(String), ALLOCATABLE - data read from the dataset
!
! See also: [[obj_Read_st2_helper]]

INTERFACE
  MODULE SUBROUTINE obj_Read_st2(obj, dsetname, length_max, vals)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    INTEGER(I4B), INTENT(IN) :: length_max
    !! length of the stored strings
    TYPE(String), ALLOCATABLE, INTENT(INOUT) :: vals(:, :)
    !! a rank-2 array of String read from the dataset
  END SUBROUTINE obj_Read_st2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                       Read
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Read a rank-2 array of String stored as single characters
!
! Read a rank-3 array of single characters into a rank-2 array of String; the
! first dimension holds the characters of each string. This routine is used by
! obj_Read_st2_helper and is not bound to the READ generic.
!
! In DEBUG mode an error is raised if the file is not open with read access,
! the dataset can not be opened, or its rank is not the rank of vals plus one.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : TYPE(String), ALLOCATABLE - data read from the dataset
!
! See also: [[obj_Read_st2_helper]]

INTERFACE
  MODULE SUBROUTINE obj_Read_ca2(obj, dsetname, vals)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    TYPE(String), ALLOCATABLE, INTENT(INOUT) :: vals(:, :)
    !! a rank-2 array of String read from the dataset
  END SUBROUTINE obj_Read_ca2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Read a rank-2 array of String without allocating vals
!
! Read a rank-2 array of String from the dataset dsetname into vals without
! allocating vals. The extent of the data is returned in nrow, ncol; the data
! is stored in vals(1:nrow, 1:ncol), and the rest of vals is not changed. The
! data is read with obj_Read_st2_helper in a local array and copied to vals.
!
! The extents are set to zero at the start. vals must be large enough to hold
! the dataset along every dimension; in DEBUG mode an error is raised
! otherwise. In release builds this is not checked.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : TYPE(String) - array to fill, SIZE(vals, i) >= extent i
!   nrow : INTEGER(I4B) - number of rows written in vals
!   ncol : INTEGER(I4B) - number of columns written in vals
!
! See also: [[obj_Read__st1]], [[obj_Read_st2_helper]]

INTERFACE
  MODULE SUBROUTINE obj_Read__st2(obj, dsetname, vals, nrow, ncol)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    TYPE(String), INTENT(INOUT) :: vals(:, :)
    !! vals will be filled with entries read from the dataset
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! number of rows written in vals
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! number of columns written in vals
  END SUBROUTINE obj_Read__st2
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Read a rank-3 array of String
!
! Read a rank-3 array of String from the dataset dsetname. Strings can be
! stored in two ways, and this routine finds out which one is used:
!
! - as fixed-length strings, written by Write with String data; then
!   obj_Read_st3 is called with the length of the stored strings.
! - as an array of single characters, written by Write with a character
!   variable; then obj_Read_ca3 is called.
!
! vals is allocated to the shape of the dataset; it is reallocated if any
! dimension differs. This routine is not bound to the READ generic.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : TYPE(String), ALLOCATABLE - data read from the dataset
!
! See also: [[obj_Read_st2_helper]], [[obj_Write_st3_helper]],
!   [[obj_Read__st3]]

INTERFACE
  MODULE SUBROUTINE obj_Read_st3_helper(obj, dsetname, vals)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    TYPE(String), ALLOCATABLE, INTENT(INOUT) :: vals(:, :, :)
    !! a rank-3 array of String read from the dataset
  END SUBROUTINE obj_Read_st3_helper
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Read a rank-3 array of String of fixed length
!
! Read a rank-3 array of String stored as fixed-length strings of length
! length_max. C null characters are removed. vals is allocated to the shape of
! the dataset; it is reallocated if any dimension differs. This routine is not
! bound to the READ generic.
!
! In DEBUG mode an error is raised if the file is not open with read access,
! the dataset can not be opened, or its rank is not the rank of vals.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   length_max : INTEGER(I4B) - length of the stored strings
!   vals : TYPE(String), ALLOCATABLE - data read from the dataset
!
! See also: [[obj_Read_st3_helper]]

INTERFACE
  MODULE SUBROUTINE obj_Read_st3(obj, dsetname, length_max, vals)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    INTEGER(I4B), INTENT(IN) :: length_max
    !! length of the stored strings
    TYPE(String), ALLOCATABLE, INTENT(INOUT) :: vals(:, :, :)
    !! a rank-3 array of String read from the dataset
  END SUBROUTINE obj_Read_st3
END INTERFACE

!----------------------------------------------------------------------------
!                                                                       Read
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Read a rank-3 array of String stored as single characters
!
! Read a rank-4 array of single characters into a rank-3 array of String; the
! first dimension holds the characters of each string. This routine is used by
! obj_Read_st3_helper and is not bound to the READ generic.
!
! In DEBUG mode an error is raised if the file is not open with read access,
! the dataset can not be opened, or its rank is not the rank of vals plus one.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : TYPE(String), ALLOCATABLE - data read from the dataset
!
! See also: [[obj_Read_st3_helper]]

INTERFACE
  MODULE SUBROUTINE obj_Read_ca3(obj, dsetname, vals)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    TYPE(String), ALLOCATABLE, INTENT(INOUT) :: vals(:, :, :)
    !! a rank-3 array of String read from the dataset
  END SUBROUTINE obj_Read_ca3
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Read a rank-3 array of String without allocating vals
!
! Read a rank-3 array of String from the dataset dsetname into vals without
! allocating vals. The extent of the data is returned in dim1, dim2, dim3; the
! data is stored in vals(1:dim1, ..., 1:dim3), and the rest of vals is not
! changed. The data is read with obj_Read_st3_helper in a local array and
! copied to vals.
!
! The extents are set to zero at the start. vals must be large enough to hold
! the dataset along every dimension; in DEBUG mode an error is raised
! otherwise. In release builds this is not checked.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : TYPE(String) - array to fill, SIZE(vals, i) >= extent i
!   dim1, ..., dim3 : INTEGER(I4B) - extent of data along each dimension
!
! See also: [[obj_Read__st1]], [[obj_Read_st3_helper]]

INTERFACE
  MODULE SUBROUTINE obj_Read__st3(obj, dsetname, vals, dim1, dim2, dim3)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    TYPE(String), INTENT(INOUT) :: vals(:, :, :)
    !! vals will be filled with entries read from the dataset
    INTEGER(I4B), INTENT(OUT) :: dim1
    !! extent of data written in vals along dimension 1
    INTEGER(I4B), INTENT(OUT) :: dim2
    !! extent of data written in vals along dimension 2
    INTEGER(I4B), INTENT(OUT) :: dim3
    !! extent of data written in vals along dimension 3
  END SUBROUTINE obj_Read__st3
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Read
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-09
! summary: Read a character variable
!
! Read a dataset of single characters, written by Write with a character
! variable, into vals. vals is set to blanks first. LEN(vals) must not be
! smaller than the size of the dataset.
!
! In DEBUG mode an error is raised if the file is not open with read access,
! the dataset can not be opened, or its rank is not the rank one.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : CHARACTER(*) - data read from the dataset
!
! See also: [[obj_Write_c1]], [[obj_Read_st0_helper]]

INTERFACE
  MODULE SUBROUTINE obj_Read_c1(obj, dsetname, vals)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    CHARACTER(LEN=*), INTENT(INOUT) :: vals
    !! character variable read from the dataset
  END SUBROUTINE obj_Read_c1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Read
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-09
! summary: Read a scalar of logical
!
! Read a scalar of logical from the dataset dsetname. The characters 'T' and
! 'F' stored by Write are converted to logical values.
!
! In DEBUG mode an error is raised if the file is not open with read access,
! the dataset can not be opened, or its rank is not the rank of vals.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : LOGICAL(LGT) - data read from the dataset
!
! See also: [[obj_Read_b1]], [[obj_Write_b0]]

INTERFACE
  MODULE SUBROUTINE obj_Read_b0(obj, dsetname, vals)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    LOGICAL(LGT), INTENT(INOUT) :: vals
    !! a scalar of logical read from the dataset
  END SUBROUTINE obj_Read_b0
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Read
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-09
! summary: Read a rank-1 array of logical
!
! Read a rank-1 array of logical from the dataset dsetname. vals is allocated
! to the shape of the dataset; it is reallocated if it has another shape. The
! characters 'T' and 'F' stored by Write are converted to logical values.
!
! In DEBUG mode an error is raised if the file is not open with read access,
! the dataset can not be opened, or its rank is not the rank of vals.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : LOGICAL(LGT), ALLOCATABLE - data read from the dataset
!
! See also: [[obj_Write_b1]], [[obj_Write_b1]], [[obj_Read__b1]]

INTERFACE
  MODULE SUBROUTINE obj_Read_b1(obj, dsetname, vals)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    LOGICAL(LGT), ALLOCATABLE, INTENT(INOUT) :: vals(:)
    !! a rank-1 array of logical read from the dataset
  END SUBROUTINE obj_Read_b1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Read a rank-1 array of logical without allocating vals
!
! Read a rank-1 array of logical from the dataset dsetname into vals without
! allocating vals. The extent of the data is returned in tsize; the data is
! stored in vals(1:tsize), and the rest of vals is not changed. The data is
! read in a local character array and converted to logical values.
!
! The extents are set to zero at the start. vals must be large enough to hold
! the dataset along every dimension; in DEBUG mode an error is raised
! otherwise. In release builds this is not checked.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : LOGICAL(LGT) - array to fill, SIZE(vals, i) >= extent i
!   tsize : INTEGER(I4B) - size of data written in vals
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE HDF5File_Class, ONLY: HDF5File_
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: filename = "Read__test_3.h5"
! TYPE(HDF5File_) :: obj
! LOGICAL(LGT) :: v(3), m(2, 2), t(2, 1, 2)
! LOGICAL(LGT) :: vbuf(5), mbuf(3, 3), tbuf(2, 2, 2)
! INTEGER(I4B) :: tsize, nrow, ncol, dim1, dim2, dim3
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! v = [.TRUE., .FALSE., .TRUE.]
! m = RESHAPE([.TRUE., .FALSE., .FALSE., .TRUE.], [2, 2])
! t = RESHAPE([.TRUE., .FALSE., .FALSE., .TRUE.], [2, 1, 2])
!
! CALL obj%Initiate(filename=filename, mode="NEW")
! CALL obj%OPEN()
! CALL obj%WRITE(dsetname="/v", vals=v)
! CALL obj%WRITE(dsetname="/m", vals=m)
! CALL obj%WRITE(dsetname="/t", vals=t)
! CALL obj%DEALLOCATE()
!
! CALL obj%Initiate(filename=filename, mode="READ")
! CALL obj%OPEN()
! CALL obj%READ_(dsetname="/v", vals=vbuf, tsize=tsize)
! CALL obj%READ_(dsetname="/m", vals=mbuf, nrow=nrow, ncol=ncol)
! CALL obj%READ_(dsetname="/t", vals=tbuf, dim1=dim1, dim2=dim2, &
!                dim3=dim3)
! CALL obj%DEALLOCATE()
!
! CALL OK(tsize .EQ. 3, "rank 1 tsize")
! CALL OK(ALL(vbuf(1:tsize) .EQV. v), "rank 1 values")
! CALL OK((nrow .EQ. 2) .AND. (ncol .EQ. 2), "rank 2 nrow, ncol")
! CALL OK(ALL(mbuf(1:nrow, 1:ncol) .EQV. m), "rank 2 values")
! CALL OK(ALL([dim1, dim2, dim3] .EQ. [2, 1, 2]), "rank 3 extents")
! CALL OK(ALL(tbuf(1:dim1, 1:dim2, 1:dim3) .EQV. t), "rank 3 values")
! END PROGRAM main
!```
!
! See also: [[obj_Read_b1]]

INTERFACE
  MODULE SUBROUTINE obj_Read__b1(obj, dsetname, vals, tsize)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    LOGICAL(LGT), INTENT(INOUT) :: vals(:)
    !! vals will be filled with entries read from the dataset
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of data written in vals
  END SUBROUTINE obj_Read__b1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Read
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-09
! summary: Read a rank-2 array of logical
!
! Read a rank-2 array of logical from the dataset dsetname. vals is allocated
! to the shape of the dataset; it is reallocated if it has another shape. The
! characters 'T' and 'F' stored by Write are converted to logical values.
!
! In DEBUG mode an error is raised if the file is not open with read access,
! the dataset can not be opened, or its rank is not the rank of vals.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : LOGICAL(LGT), ALLOCATABLE - data read from the dataset
!
! See also: [[obj_Read_b1]], [[obj_Write_b2]], [[obj_Read__b2]]

INTERFACE
  MODULE SUBROUTINE obj_Read_b2(obj, dsetname, vals)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    LOGICAL(LGT), ALLOCATABLE, INTENT(INOUT) :: vals(:, :)
    !! a rank-2 array of logical read from the dataset
  END SUBROUTINE obj_Read_b2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Read a rank-2 array of logical without allocating vals
!
! Read a rank-2 array of logical from the dataset dsetname into vals without
! allocating vals. The extent of the data is returned in nrow, ncol; the data
! is stored in vals(1:nrow, 1:ncol), and the rest of vals is not changed. The
! data is read in a local character array and converted to logical values.
!
! The extents are set to zero at the start. vals must be large enough to hold
! the dataset along every dimension; in DEBUG mode an error is raised
! otherwise. In release builds this is not checked.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : LOGICAL(LGT) - array to fill, SIZE(vals, i) >= extent i
!   nrow : INTEGER(I4B) - number of rows written in vals
!   ncol : INTEGER(I4B) - number of columns written in vals
!
! See also: [[obj_Read__b1]], [[obj_Read_b2]]

INTERFACE
  MODULE SUBROUTINE obj_Read__b2(obj, dsetname, vals, nrow, ncol)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    LOGICAL(LGT), INTENT(INOUT) :: vals(:, :)
    !! vals will be filled with entries read from the dataset
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! number of rows written in vals
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! number of columns written in vals
  END SUBROUTINE obj_Read__b2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                      Read
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-09
! summary: Read a rank-3 array of logical
!
! Read a rank-3 array of logical. This routine is not implemented yet: in
! DEBUG mode it raises an error, otherwise it does nothing. Use obj_Read__b3
! instead.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : LOGICAL(LGT), ALLOCATABLE - data read from the dataset
!
! See also: [[obj_Read_b1]], [[obj_Write_b3]], [[obj_Read__b3]]

INTERFACE
  MODULE SUBROUTINE obj_Read_b3(obj, dsetname, vals)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    LOGICAL(LGT), ALLOCATABLE, INTENT(INOUT) :: vals(:, :, :)
    !! a rank-3 array of logical read from the dataset
  END SUBROUTINE obj_Read_b3
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Read_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Read a rank-3 array of logical without allocating vals
!
! Read a rank-3 array of logical from the dataset dsetname into vals without
! allocating vals. The extent of the data is returned in dim1, dim2, dim3; the
! data is stored in vals(1:dim1, ..., 1:dim3), and the rest of vals is not
! changed. The data is read in a local character array and converted to
! logical values.
!
! The extents are set to zero at the start. vals must be large enough to hold
! the dataset along every dimension; in DEBUG mode an error is raised
! otherwise. In release builds this is not checked.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   dsetname : CHARACTER(*) - dataset name with full path
!   vals : LOGICAL(LGT) - array to fill, SIZE(vals, i) >= extent i
!   dim1, ..., dim3 : INTEGER(I4B) - extent of data along each dimension
!
! See also: [[obj_Read__b1]], [[obj_Read_b3]]

INTERFACE
  MODULE SUBROUTINE obj_Read__b3(obj, dsetname, vals, dim1, dim2, dim3)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! dataset name with full path
    LOGICAL(LGT), INTENT(INOUT) :: vals(:, :, :)
    !! vals will be filled with entries read from the dataset
    INTEGER(I4B), INTENT(OUT) :: dim1
    !! extent of data written in vals along dimension 1
    INTEGER(I4B), INTENT(OUT) :: dim2
    !! extent of data written in vals along dimension 2
    INTEGER(I4B), INTENT(OUT) :: dim3
    !! extent of data written in vals along dimension 3
  END SUBROUTINE obj_Read__b3
END INTERFACE

!----------------------------------------------------------------------------
!                                                            WriteAttribute
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-09
! summary: Write a String attribute
!
! Write a String attribute attr_name to the object obj_name, which can be a
! group or a dataset. The object must exist. An empty String is allowed.
!
! An existing attribute is replaced when overwriting is allowed (modes NEW
! and OVERWRITE, or SetOverwriteStat(.TRUE.)): it is deleted and created
! again, so the new value can have another length or type. When
! overwriting is not allowed (mode WRITE) the attribute is not changed; in
! DEBUG mode an error is raised. The same rules hold for the character,
! integer and real attributes.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   obj_name : CHARACTER(*) - path of the group or dataset
!   attr_name : CHARACTER(*) - name of the attribute
!   attr_val : TYPE(String) - value of the attribute
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE HDF5File_Class, ONLY: HDF5File_
! USE String_Class, ONLY: String
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: filename = "WriteAttribute_test_1.h5"
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! TYPE(HDF5File_) :: obj
! TYPE(String) :: strval, found
! INTEGER(I4B) :: ival
! REAL(DFP) :: rval
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! strval = "meter"
! CALL obj%Initiate(filename=filename, mode="NEW")
! CALL obj%OPEN()
! CALL obj%WRITE(dsetname="/length", vals=2.0_DFP)
!
! CALL obj%WriteAttribute(obj_name="/length", attr_name="unit", &
!                         attr_val=strval)
! CALL obj%WriteAttribute(obj_name="/length", attr_name="note", &
!                         attr_val="measured")
! CALL obj%WriteAttribute(obj_name="/length", attr_name="count", &
!                         attr_val=3_I4B)
! CALL obj%WriteAttribute(obj_name="/length", attr_name="scale", &
!                         attr_val=0.5_DFP)
!
! CALL obj%ReadAttribute(obj_name="/length", attr_name="unit", &
!                        attr_val=found)
! CALL OK(found%chars() .EQ. "meter", "String attribute")
! CALL obj%ReadAttribute(obj_name="/length", attr_name="note", &
!                        attr_val=found)
! CALL OK(found%chars() .EQ. "measured", "character attribute")
! CALL obj%ReadAttribute(obj_name="/length", attr_name="count", &
!                        attr_val=ival)
! CALL OK(ival .EQ. 3, "integer attribute")
! CALL obj%ReadAttribute(obj_name="/length", attr_name="scale", &
!                        attr_val=rval)
! CALL OK(ABS(rval - 0.5_DFP) .LT. tol, "real attribute")
!
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_ReadAttribute_st0]]

INTERFACE
  MODULE SUBROUTINE obj_WriteAttribute_st0(obj, obj_name, attr_name, &
                                           attr_val)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: obj_name, attr_name
    !! path of the group or dataset, and
    !! name of the attribute
    TYPE(String), INTENT(IN) :: attr_val
    !! value of the attribute
  END SUBROUTINE obj_WriteAttribute_st0
END INTERFACE

!----------------------------------------------------------------------------
!                                                            WriteAttribute
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-09
! summary: Write a character attribute
!
! Write a character attribute attr_name to the object obj_name, which can be a
! group or a dataset. The object must exist. An existing attribute is
! overwritten when overwriting is allowed (see SetOverwriteStat). Trailing
! blanks are removed, and the value is stored as a string attribute.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   obj_name : CHARACTER(*) - path of the group or dataset
!   attr_name : CHARACTER(*) - name of the attribute
!   attr_val : CHARACTER(*) - value of the attribute
!
! See also: [[obj_WriteAttribute_st0]], [[obj_ReadAttribute_c0]]

INTERFACE
  MODULE SUBROUTINE obj_WriteAttribute_c0(obj, obj_name, attr_name, &
                                          attr_val)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: obj_name, attr_name
    !! path of the group or dataset, and
    !! name of the attribute
    CHARACTER(LEN=*), INTENT(IN) :: attr_val
    !! value of the attribute
  END SUBROUTINE obj_WriteAttribute_c0
END INTERFACE

!----------------------------------------------------------------------------
!                                                            WriteAttribute
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-09
! summary: Write an integer attribute
!
! Write an integer attribute attr_name to the object obj_name, which can be a
! group or a dataset. The object must exist. An existing attribute is
! overwritten when overwriting is allowed (see SetOverwriteStat).
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   obj_name : CHARACTER(*) - path of the group or dataset
!   attr_name : CHARACTER(*) - name of the attribute
!   attr_val : INTEGER(I4B) - value of the attribute
!
! See also: [[obj_WriteAttribute_st0]], [[obj_ReadAttribute_i0]]

INTERFACE
  MODULE SUBROUTINE obj_WriteAttribute_i0(obj, obj_name, attr_name, &
                                          attr_val)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: obj_name, attr_name
    !! path of the group or dataset, and
    !! name of the attribute
    INTEGER(I4B), INTENT(IN) :: attr_val
    !! value of the attribute
  END SUBROUTINE obj_WriteAttribute_i0
END INTERFACE

!----------------------------------------------------------------------------
!                                                            WriteAttribute
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-09
! summary: Write a real attribute
!
! Write a real attribute attr_name to the object obj_name, which can be a
! group or a dataset. The object must exist. An existing attribute is
! overwritten when overwriting is allowed (see SetOverwriteStat).
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   obj_name : CHARACTER(*) - path of the group or dataset
!   attr_name : CHARACTER(*) - name of the attribute
!   attr_val : REAL(DFP) - value of the attribute
!
! See also: [[obj_WriteAttribute_st0]], [[obj_ReadAttribute_d0]]

INTERFACE
  MODULE SUBROUTINE obj_WriteAttribute_d0(obj, obj_name, attr_name, &
                                          attr_val)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: obj_name, attr_name
    !! path of the group or dataset, and
    !! name of the attribute
    REAL(DFP), INTENT(IN) :: attr_val
    !! value of the attribute
  END SUBROUTINE obj_WriteAttribute_d0
END INTERFACE

!----------------------------------------------------------------------------
!                                                              WriteAttribute
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! summary: Write a logical attribute
!
! Write a logical attribute attr_name to the object obj_name, which can be a
! group or a dataset. The object must exist. An existing attribute is
! replaced when overwriting is allowed; see obj_WriteAttribute_st0.
! The value is stored as the character "T" or "F", as logical datasets.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   obj_name : CHARACTER(*) - path of the group or dataset
!   attr_name : CHARACTER(*) - name of the attribute
!   attr_val : LOGICAL(LGT) - value of the attribute
!
! See also: [[obj_WriteAttribute_i1]], [[obj_ReadAttribute_b0]]

INTERFACE
  MODULE SUBROUTINE obj_WriteAttribute_b0(obj, obj_name, attr_name, &
                                          attr_val)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: obj_name, attr_name
    !! path of the group or dataset, and
    !! name of the attribute
    LOGICAL(LGT), INTENT(IN) :: attr_val
    !! value of the attribute
  END SUBROUTINE obj_WriteAttribute_b0
END INTERFACE

!----------------------------------------------------------------------------
!                                                              WriteAttribute
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! summary: Write a rank-1 integer attribute
!
! Write a rank-1 integer attribute attr_name to the object obj_name, which
! can be a group or a dataset. The object must exist. An existing attribute is
! replaced when overwriting is allowed; see obj_WriteAttribute_st0.
! The HDF5 type follows the kind of attr_val (H5kind_to_type), so the
! data is stored correctly for every integer kind. This holds for the
! integer and real attributes of rank 0 and 1.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   obj_name : CHARACTER(*) - path of the group or dataset
!   attr_name : CHARACTER(*) - name of the attribute
!   attr_val : INTEGER(I4B) - values of the attribute, SIZE(attr_val)
!       entries
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE HDF5File_Class, ONLY: HDF5File_
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: filename = "WriteAttribute_test_4.h5"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(HDF5File_) :: obj
! INTEGER(I4B), ALLOCATABLE :: ivec(:)
! REAL(DFP), ALLOCATABLE :: rvec(:)
! LOGICAL(LGT) :: flag
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! CALL obj%Initiate(filename=filename, mode="NEW")
! CALL obj%OPEN()
! CALL obj%Mkdir("/mesh")
! CALL obj%WriteAttribute(obj_name="/mesh", attr_name="isActive", &
!                         attr_val=.TRUE.)
! CALL obj%WriteAttribute(obj_name="/mesh", attr_name="isBoundary", &
!                         attr_val=.FALSE.)
! CALL obj%WriteAttribute(obj_name="/mesh", attr_name="orders", &
!                         attr_val=[1_I4B, 2_I4B, 3_I4B])
! CALL obj%WriteAttribute(obj_name="/mesh", attr_name="boundingBox", &
!                         attr_val=[0.0_DFP, 1.0_DFP, 0.0_DFP, 2.0_DFP])
! CALL obj%DEALLOCATE()
!
! CALL obj%Initiate(filename=filename, mode="READ")
! CALL obj%OPEN()
! CALL obj%ReadAttribute(obj_name="/mesh", attr_name="isActive", &
!                        attr_val=flag)
! CALL OK(flag, "logical attribute: true")
! CALL obj%ReadAttribute(obj_name="/mesh", attr_name="isBoundary", &
!                        attr_val=flag)
! CALL OK(.NOT. flag, "logical attribute: false")
! CALL obj%ReadAttribute(obj_name="/mesh", attr_name="orders", &
!                        attr_val=ivec)
! CALL OK(SIZE(ivec) .EQ. 3, "integer vector: size")
! CALL OK(ALL(ivec .EQ. [1, 2, 3]), "integer vector: values")
! CALL obj%ReadAttribute(obj_name="/mesh", attr_name="boundingBox", &
!                        attr_val=rvec)
! CALL OK(SIZE(rvec) .EQ. 4, "real vector: size")
! CALL OK(ALL(ABS(rvec - [0.0_DFP, 1.0_DFP, 0.0_DFP, 2.0_DFP]) .LT. tol), &
!         "real vector: values")
! ! an integer vector read as a real vector is converted by HDF5
! CALL obj%ReadAttribute(obj_name="/mesh", attr_name="orders", &
!                        attr_val=rvec)
! CALL OK(ALL(ABS(rvec - [1.0_DFP, 2.0_DFP, 3.0_DFP]) .LT. tol), &
!         "integer vector read as real")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_WriteAttribute_d1]], [[obj_WriteAttribute_b0]],
!   [[obj_ReadAttribute_i1]]

INTERFACE
  MODULE SUBROUTINE obj_WriteAttribute_i1(obj, obj_name, attr_name, &
                                          attr_val)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: obj_name, attr_name
    !! path of the group or dataset, and
    !! name of the attribute
    INTEGER(I4B), INTENT(IN) :: attr_val(:)
    !! values of the attribute
  END SUBROUTINE obj_WriteAttribute_i1
END INTERFACE

!----------------------------------------------------------------------------
!                                                              WriteAttribute
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! summary: Write a rank-1 real attribute
!
! Write a rank-1 real attribute attr_name to the object obj_name, which can
! be a group or a dataset. The object must exist. An existing attribute is
! replaced when overwriting is allowed; see obj_WriteAttribute_st0.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   obj_name : CHARACTER(*) - path of the group or dataset
!   attr_name : CHARACTER(*) - name of the attribute
!   attr_val : REAL(DFP) - values of the attribute, SIZE(attr_val) entries
!
! See also: [[obj_WriteAttribute_i1]], [[obj_ReadAttribute_d1]]

INTERFACE
  MODULE SUBROUTINE obj_WriteAttribute_d1(obj, obj_name, attr_name, &
                                          attr_val)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: obj_name, attr_name
    !! path of the group or dataset, and
    !! name of the attribute
    REAL(DFP), INTENT(IN) :: attr_val(:)
    !! values of the attribute
  END SUBROUTINE obj_WriteAttribute_d1
END INTERFACE

!----------------------------------------------------------------------------
!                                                              WriteAttribute
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! summary: Write a rank-1 logical attribute
!
! Write a rank-1 logical attribute attr_name to the object obj_name, which can
! be a group or a dataset. The object must exist. An existing attribute is
! replaced when overwriting is allowed; see obj_WriteAttribute_st0. Each value
! is stored as the character "T" or "F".
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   obj_name : CHARACTER(*) - path of the group or dataset
!   attr_name : CHARACTER(*) - name of the attribute
!   attr_val : LOGICAL(LGT) - values of the attribute
!
! See also: [[obj_ReadAttribute_b1]], [[obj_WriteAttribute_b0]]

INTERFACE
  MODULE SUBROUTINE obj_WriteAttribute_b1(obj, obj_name, attr_name, &
                                          attr_val)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: obj_name, attr_name
    !! path of the group or dataset, and
    !! name of the attribute
    LOGICAL(LGT), INTENT(IN) :: attr_val(:)
    !! values of the attribute
  END SUBROUTINE obj_WriteAttribute_b1
END INTERFACE

!----------------------------------------------------------------------------
!                                                              WriteAttribute
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! summary: Write a rank-1 String attribute
!
! Write a rank-1 String attribute attr_name to the object obj_name, which can
! be a group or a dataset. The object must exist. An existing attribute is
! replaced when overwriting is allowed; see obj_WriteAttribute_st0. The
! strings are stored as fixed-length strings of the longest length (at least
! 1); shorter strings are padded with blanks, which ReadAttribute removes. So
! trailing blanks of a value are not kept. Empty strings are allowed.
!
! The example writes and reads all the vector attributes: String,
! logical, Real32 and Int64.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   obj_name : CHARACTER(*) - path of the group or dataset
!   attr_name : CHARACTER(*) - name of the attribute
!   attr_val : TYPE(String) - values of the attribute
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE GlobalData, ONLY: INT64
! USE GlobalData, ONLY: LGT
! USE GlobalData, ONLY: REAL32
! USE HDF5File_Class, ONLY: HDF5File_
! USE String_Class, ONLY: String
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: filename = "WriteAttribute_test_5.h5"
! TYPE(HDF5File_) :: obj
! TYPE(String) :: names(3)
! TYPE(String), ALLOCATABLE :: names1(:)
! LOGICAL(LGT), ALLOCATABLE :: flags(:)
! REAL(REAL32) :: r32
! REAL(REAL32), ALLOCATABLE :: r32v(:)
! INTEGER(INT64), ALLOCATABLE :: i64v(:)
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! names(1) = "steel"
! names(2) = "concrete"
! names(3) = ""
!
! CALL obj%Initiate(filename=filename, mode="NEW")
! CALL obj%OPEN()
! CALL obj%Mkdir("/model")
! CALL obj%WriteAttribute(obj_name="/model", attr_name="materials", &
!                         attr_val=names)
! CALL obj%WriteAttribute(obj_name="/model", attr_name="isFixed", &
!                         attr_val=[.TRUE., .FALSE., .TRUE.])
! CALL obj%WriteAttribute(obj_name="/model", attr_name="tol", &
!                         attr_val=1.0E-3_REAL32)
! CALL obj%WriteAttribute(obj_name="/model", attr_name="weights", &
!                         attr_val=[0.25_REAL32, 0.75_REAL32])
! CALL obj%WriteAttribute(obj_name="/model", attr_name="bigIds", &
!                         attr_val=[3000000000_INT64, 4000000000_INT64])
! CALL obj%DEALLOCATE()
!
! CALL obj%Initiate(filename=filename, mode="READ")
! CALL obj%OPEN()
! CALL obj%ReadAttribute(obj_name="/model", attr_name="materials", &
!                        attr_val=names1)
! CALL OK(SIZE(names1) .EQ. 3, "String vector: size")
! CALL OK(names1(1)%chars() .EQ. "steel" .AND. &
!         names1(2)%chars() .EQ. "concrete" .AND. &
!         names1(3)%LEN() .EQ. 0, "String vector: values")
! CALL obj%ReadAttribute(obj_name="/model", attr_name="isFixed", &
!                        attr_val=flags)
! CALL OK(SIZE(flags) .EQ. 3, "logical vector: size")
! CALL OK(flags(1) .AND. (.NOT. flags(2)) .AND. flags(3), &
!         "logical vector: values")
! CALL obj%ReadAttribute(obj_name="/model", attr_name="tol", attr_val=r32)
! CALL OK(ABS(r32 - 1.0E-3_REAL32) .LT. 1.0E-9_REAL32, "Real32 scalar")
! CALL obj%ReadAttribute(obj_name="/model", attr_name="weights", &
!                        attr_val=r32v)
! CALL OK(ALL(ABS(r32v - [0.25_REAL32, 0.75_REAL32]) .LT. 1.0E-6_REAL32), &
!         "Real32 vector")
! CALL obj%ReadAttribute(obj_name="/model", attr_name="bigIds", &
!                        attr_val=i64v)
! CALL OK(ALL(i64v .EQ. [3000000000_INT64, 4000000000_INT64]), &
!         "Int64 vector")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_ReadAttribute_st1]], [[obj_WriteAttribute_b1]]

INTERFACE
  MODULE SUBROUTINE obj_WriteAttribute_st1(obj, obj_name, attr_name, &
                                           attr_val)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: obj_name, attr_name
    !! path of the group or dataset, and
    !! name of the attribute
    TYPE(String), INTENT(IN) :: attr_val(:)
    !! values of the attribute
  END SUBROUTINE obj_WriteAttribute_st1
END INTERFACE

#ifdef USE_Real64
!----------------------------------------------------------------------------
!                                                              WriteAttribute
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! summary: Write a Real32 attribute
!
! Write a Real32 attribute attr_name to the object obj_name, which can be
! a group or a dataset. The object must exist. An existing attribute is
! replaced when overwriting is allowed; see obj_WriteAttribute_st0.
! Only available when DFP is Real64 (USE_Real64).
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   obj_name : CHARACTER(*) - path of the group or dataset
!   attr_name : CHARACTER(*) - name of the attribute
!   attr_val : REAL(REAL32) - value of the attribute
!
! See also: [[obj_ReadAttribute_s0]], [[obj_WriteAttribute_d0]]

INTERFACE
  MODULE SUBROUTINE obj_WriteAttribute_s0(obj, obj_name, attr_name, &
                                          attr_val)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: obj_name, attr_name
    !! path of the group or dataset, and
    !! name of the attribute
    REAL(REAL32), INTENT(IN) :: attr_val
    !! value of the attribute
  END SUBROUTINE obj_WriteAttribute_s0
END INTERFACE

!----------------------------------------------------------------------------
!                                                              WriteAttribute
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! summary: Write a rank-1 Real32 attribute
!
! Write a rank-1 Real32 attribute attr_name to the object obj_name, which can
! be a group or a dataset. The object must exist. An existing attribute is
! replaced when overwriting is allowed; see obj_WriteAttribute_st0.
! Only available when DFP is Real64 (USE_Real64).
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   obj_name : CHARACTER(*) - path of the group or dataset
!   attr_name : CHARACTER(*) - name of the attribute
!   attr_val : REAL(REAL32) - values of the attribute
!
! See also: [[obj_ReadAttribute_s1]], [[obj_WriteAttribute_d1]]

INTERFACE
  MODULE SUBROUTINE obj_WriteAttribute_s1(obj, obj_name, attr_name, &
                                          attr_val)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: obj_name, attr_name
    !! path of the group or dataset, and
    !! name of the attribute
    REAL(REAL32), INTENT(IN) :: attr_val(:)
    !! values of the attribute
  END SUBROUTINE obj_WriteAttribute_s1
END INTERFACE

#endif

#ifndef USE_Int64
!----------------------------------------------------------------------------
!                                                              WriteAttribute
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! summary: Write an Int64 attribute
!
! Write an Int64 attribute attr_name to the object obj_name, which can be
! a group or a dataset. The object must exist. An existing attribute is
! replaced when overwriting is allowed; see obj_WriteAttribute_st0.
! Only available when I4B is Int32 (USE_Int64 is not defined).
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   obj_name : CHARACTER(*) - path of the group or dataset
!   attr_name : CHARACTER(*) - name of the attribute
!   attr_val : INTEGER(INT64) - value of the attribute
!
! See also: [[obj_ReadAttribute_l0]], [[obj_WriteAttribute_i0]]

INTERFACE
  MODULE SUBROUTINE obj_WriteAttribute_l0(obj, obj_name, attr_name, &
                                          attr_val)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: obj_name, attr_name
    !! path of the group or dataset, and
    !! name of the attribute
    INTEGER(INT64), INTENT(IN) :: attr_val
    !! value of the attribute
  END SUBROUTINE obj_WriteAttribute_l0
END INTERFACE

!----------------------------------------------------------------------------
!                                                              WriteAttribute
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! summary: Write a rank-1 Int64 attribute
!
! Write a rank-1 Int64 attribute attr_name to the object obj_name, which can
! be a group or a dataset. The object must exist. An existing attribute is
! replaced when overwriting is allowed; see obj_WriteAttribute_st0.
! Only available when I4B is Int32 (USE_Int64 is not defined).
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   obj_name : CHARACTER(*) - path of the group or dataset
!   attr_name : CHARACTER(*) - name of the attribute
!   attr_val : INTEGER(INT64) - values of the attribute
!
! See also: [[obj_ReadAttribute_l1]], [[obj_WriteAttribute_i1]]

INTERFACE
  MODULE SUBROUTINE obj_WriteAttribute_l1(obj, obj_name, attr_name, &
                                          attr_val)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: obj_name, attr_name
    !! path of the group or dataset, and
    !! name of the attribute
    INTEGER(INT64), INTENT(IN) :: attr_val(:)
    !! values of the attribute
  END SUBROUTINE obj_WriteAttribute_l1
END INTERFACE

#endif

!----------------------------------------------------------------------------
!                                                           AttributeExists
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! summary: Returns true if an attribute exists
!
! Returns true if the object obj_name (a group or a dataset) exists and has
! the attribute attr_name. Returns false if the object or the attribute
! does not exist, or if the file is not initiated or not open; no error is
! raised. Use it before ReadAttribute to read optional attributes.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   obj_name : CHARACTER(*) - path of the group or dataset ("/" is the root)
!   attr_name : CHARACTER(*) - name of the attribute
!
! Returns:
!   ans : LOGICAL(LGT) - true if the attribute exists
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE GlobalData, ONLY: DFP
! USE HDF5File_Class, ONLY: HDF5File_
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: filename = "AttributeExists_test_1.h5"
! TYPE(HDF5File_) :: obj
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! CALL obj%Initiate(filename=filename, mode="NEW")
! CALL obj%OPEN()
! CALL obj%WRITE(dsetname="/mesh/x", vals=1.0_DFP)
! CALL obj%WriteAttribute(obj_name="/mesh/x", attr_name="unit", &
!                         attr_val="m")
! CALL obj%WriteAttribute(obj_name="/mesh", attr_name="nsd", attr_val=2)
!
! CALL OK(obj%AttributeExists("/mesh/x", "unit"), "attribute of a dataset")
! CALL OK(obj%AttributeExists("/mesh", "nsd"), "attribute of a group")
! CALL OK(.NOT. obj%AttributeExists("/mesh/x", "scale"), &
!         "missing attribute")
! CALL OK(.NOT. obj%AttributeExists("/missing", "unit"), "missing object")
! CALL OK(.NOT. obj%AttributeExists("/", "unit"), "root group")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_ReadAttribute_st0]], [[obj_WriteAttribute_st0]]

INTERFACE
  MODULE FUNCTION obj_AttributeExists(obj, obj_name, attr_name) RESULT(ans)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: obj_name
    !! path of the group or dataset
    CHARACTER(LEN=*), INTENT(IN) :: attr_name
    !! name of the attribute
    LOGICAL(LGT) :: ans
    !! true if the attribute exists
  END FUNCTION obj_AttributeExists
END INTERFACE

!----------------------------------------------------------------------------
!                                                             ReadAttribute
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-09
! summary: Read a String attribute
!
! Read a String attribute attr_name of the object obj_name, which can be a
! group or a dataset. The object and the attribute must exist.
!
! The example reads the scalar attributes (String, character, integer,
! real) and the rank-1 integer, real and Real32 attributes.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   obj_name : CHARACTER(*) - path of the group or dataset
!   attr_name : CHARACTER(*) - name of the attribute
!   attr_val : TYPE(String) - value of the attribute
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: REAL32
! USE HDF5File_Class, ONLY: HDF5File_
! USE String_Class, ONLY: String
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: filename = "ReadAttribute_test_1.h5"
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
! REAL(REAL32), PARAMETER :: tol32 = 1.0E-6_REAL32
! TYPE(HDF5File_) :: obj
! TYPE(String) :: strval
! CHARACTER(LEN=5) :: cval
! INTEGER(I4B) :: ival
! REAL(DFP) :: rval
! INTEGER(I4B) :: empty(0)
! INTEGER(I4B), ALLOCATABLE :: ivec(:)
! REAL(DFP), ALLOCATABLE :: rvec(:)
! REAL(REAL32), ALLOCATABLE :: svec(:)
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! strval = "meter"
! CALL obj%Initiate(filename=filename, mode="NEW")
! CALL obj%OPEN()
! CALL obj%WRITE(dsetname="/length", vals=2.0_DFP)
! CALL obj%WriteAttribute("/length", "unit", strval)
! CALL obj%WriteAttribute("/length", "count", 3_I4B)
! CALL obj%WriteAttribute("/length", "scale", 0.5_DFP)
! CALL obj%WriteAttribute("/length", "ids", [4_I4B, 5_I4B, 6_I4B])
! CALL obj%WriteAttribute("/length", "noIds", empty)
! CALL obj%WriteAttribute("/length", "limits", [-1.5_DFP, 1.5_DFP])
! CALL obj%WriteAttribute("/length", "weights", &
!                         [0.1_REAL32, 0.2_REAL32, 0.3_REAL32, 0.4_REAL32])
! CALL obj%DEALLOCATE()
!
! CALL obj%Initiate(filename=filename, mode="READ")
! CALL obj%OPEN()
! CALL obj%ReadAttribute(obj_name="/length", attr_name="unit", &
!                        attr_val=strval)
! CALL obj%ReadAttribute(obj_name="/length", attr_name="unit", &
!                        attr_val=cval)
! CALL obj%ReadAttribute(obj_name="/length", attr_name="count", &
!                        attr_val=ival)
! CALL obj%ReadAttribute(obj_name="/length", attr_name="scale", &
!                        attr_val=rval)
!
! CALL OK(strval%chars() .EQ. "meter", "read in String")
! CALL OK(cval .EQ. "meter", "read in character")
! CALL OK(ival .EQ. 3, "integer attribute")
! CALL OK(ABS(rval - 0.5_DFP) .LT. tol, "real attribute")
!
! ! rank-1 integer (_i1); ivec is allocated with another size first
! ALLOCATE (ivec(10))
! CALL obj%ReadAttribute(obj_name="/length", attr_name="ids", &
!                        attr_val=ivec)
! CALL OK(SIZE(ivec) .EQ. 3, "integer vector: size")
! CALL OK(ALL(ivec .EQ. [4, 5, 6]), "integer vector: values")
! CALL obj%ReadAttribute(obj_name="/length", attr_name="noIds", &
!                        attr_val=ivec)
! CALL OK(SIZE(ivec) .EQ. 0, "empty integer vector")
!
! ! rank-1 real (_d1)
! CALL obj%ReadAttribute(obj_name="/length", attr_name="limits", &
!                        attr_val=rvec)
! CALL OK(SIZE(rvec) .EQ. 2, "real vector: size")
! CALL OK(ALL(ABS(rvec - [-1.5_DFP, 1.5_DFP]) .LT. tol), &
!         "real vector: values")
! ! an integer vector read into a real vector is converted by HDF5
! CALL obj%ReadAttribute(obj_name="/length", attr_name="ids", &
!                        attr_val=rvec)
! CALL OK(ALL(ABS(rvec - [4.0_DFP, 5.0_DFP, 6.0_DFP]) .LT. tol), &
!         "integer vector read as real vector")
!
! ! rank-1 Real32 (_s1)
! CALL obj%ReadAttribute(obj_name="/length", attr_name="weights", &
!                        attr_val=svec)
! CALL OK(SIZE(svec) .EQ. 4, "Real32 vector: size")
! CALL OK(ALL(ABS(svec - [0.1_REAL32, 0.2_REAL32, 0.3_REAL32, &
!                         0.4_REAL32]) .LT. tol32), "Real32 vector: values")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_WriteAttribute_st0]]

INTERFACE
  MODULE SUBROUTINE obj_ReadAttribute_st0(obj, obj_name, attr_name, &
                                          attr_val)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: obj_name, attr_name
    !! path of the group or dataset, and
    !! name of the attribute
    TYPE(String), INTENT(INOUT) :: attr_val
    !! value of the attribute
  END SUBROUTINE obj_ReadAttribute_st0
END INTERFACE

!----------------------------------------------------------------------------
!                                                             ReadAttribute
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-09
! summary: Read a string attribute
!
! Read a string attribute attr_name of the object obj_name, which can be a
! group or a dataset. The object and the attribute must exist. The value is
! read as a String and assigned to attr_val, so it is truncated or padded with
! blanks to LEN(attr_val).
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   obj_name : CHARACTER(*) - path of the group or dataset
!   attr_name : CHARACTER(*) - name of the attribute
!   attr_val : CHARACTER(*) - value of the attribute
!
! See also: [[obj_ReadAttribute_st0]], [[obj_WriteAttribute_c0]]

INTERFACE
  MODULE SUBROUTINE obj_ReadAttribute_c0(obj, obj_name, attr_name, attr_val)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: obj_name, attr_name
    !! path of the group or dataset, and
    !! name of the attribute
    CHARACTER(LEN=*), INTENT(INOUT) :: attr_val
    !! value of the attribut
  END SUBROUTINE obj_ReadAttribute_c0
END INTERFACE

!----------------------------------------------------------------------------
!                                                             ReadAttribute
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-09
! summary: Read an integer attribute
!
! Read an integer attribute attr_name of the object obj_name, which can be a
! group or a dataset. The object and the attribute must exist.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   obj_name : CHARACTER(*) - path of the group or dataset
!   attr_name : CHARACTER(*) - name of the attribute
!   attr_val : INTEGER(I4B) - value of the attribute
!
! See also: [[obj_ReadAttribute_st0]], [[obj_WriteAttribute_i0]]

INTERFACE
  MODULE SUBROUTINE obj_ReadAttribute_i0(obj, obj_name, attr_name, attr_val)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: obj_name, attr_name
    !! path of the group or dataset, and
    !! name of the attribute
    INTEGER(I4B), INTENT(INOUT) :: attr_val
    !! value of the attribute
  END SUBROUTINE obj_ReadAttribute_i0
END INTERFACE

!----------------------------------------------------------------------------
!                                                             ReadAttribute
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-09
! summary: Read a real attribute
!
! Read a real attribute attr_name of the object obj_name, which can be a group
! or a dataset. The object and the attribute must exist.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   obj_name : CHARACTER(*) - path of the group or dataset
!   attr_name : CHARACTER(*) - name of the attribute
!   attr_val : REAL(DFP) - value of the attribute
!
! See also: [[obj_ReadAttribute_st0]], [[obj_WriteAttribute_d0]]

INTERFACE
  MODULE SUBROUTINE obj_ReadAttribute_d0(obj, obj_name, attr_name, attr_val)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: obj_name, attr_name
    !! path of the group or dataset, and
    !! name of the attribute
    REAL(DFP), INTENT(INOUT) :: attr_val
    !! value of the attribute
  END SUBROUTINE obj_ReadAttribute_d0
END INTERFACE

!----------------------------------------------------------------------------
!                                                               ReadAttribute
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! summary: Read a logical attribute
!
! Read a logical attribute attr_name of the object obj_name, which can be a
! group or a dataset. The object and the attribute must exist (see
! AttributeExists).
! attr_val is true if the stored character is "T".
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   obj_name : CHARACTER(*) - path of the group or dataset
!   attr_name : CHARACTER(*) - name of the attribute
!   attr_val : LOGICAL(LGT) - value of the attribute
!
! See also: [[obj_WriteAttribute_b0]], [[obj_ReadAttribute_i1]]

INTERFACE
  MODULE SUBROUTINE obj_ReadAttribute_b0(obj, obj_name, attr_name, attr_val)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: obj_name, attr_name
    !! path of the group or dataset, and
    !! name of the attribute
    LOGICAL(LGT), INTENT(INOUT) :: attr_val
    !! value of the attribute
  END SUBROUTINE obj_ReadAttribute_b0
END INTERFACE

!----------------------------------------------------------------------------
!                                                               ReadAttribute
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! summary: Read a rank-1 integer attribute
!
! Read a rank-1 integer attribute attr_name of the object obj_name, which
! can be a group or a dataset. The object and the attribute must exist (see
! AttributeExists).
! attr_val is allocated to the number of values of the attribute. HDF5
! converts the stored type to the kind of attr_val.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   obj_name : CHARACTER(*) - path of the group or dataset
!   attr_name : CHARACTER(*) - name of the attribute
!   attr_val : INTEGER(I4B), allocatable - values of the attribute
!
! See also: [[obj_WriteAttribute_i1]], [[obj_ReadAttribute_d1]]

INTERFACE
  MODULE SUBROUTINE obj_ReadAttribute_i1(obj, obj_name, attr_name, attr_val)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: obj_name, attr_name
    !! path of the group or dataset, and
    !! name of the attribute
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: attr_val(:)
    !! values of the attribute; allocated by the routine
  END SUBROUTINE obj_ReadAttribute_i1
END INTERFACE

!----------------------------------------------------------------------------
!                                                               ReadAttribute
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! summary: Read a rank-1 real attribute
!
! Read a rank-1 real attribute attr_name of the object obj_name, which can
! be a group or a dataset. The object and the attribute must exist (see
! AttributeExists).
! attr_val is allocated to the number of values of the attribute. HDF5
! converts the stored type, for example an integer attribute, to REAL(DFP).
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   obj_name : CHARACTER(*) - path of the group or dataset
!   attr_name : CHARACTER(*) - name of the attribute
!   attr_val : REAL(DFP), allocatable - values of the attribute
!
! See also: [[obj_WriteAttribute_d1]], [[obj_ReadAttribute_i1]]

INTERFACE
  MODULE SUBROUTINE obj_ReadAttribute_d1(obj, obj_name, attr_name, attr_val)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: obj_name
    !! path of the group or dataset, and
    CHARACTER(LEN=*), INTENT(IN) :: attr_name
    !! name of the attribute
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: attr_val(:)
    !! values of the attribute; allocated by the routine
  END SUBROUTINE obj_ReadAttribute_d1
END INTERFACE

!----------------------------------------------------------------------------
!                                                               ReadAttribute
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! summary: Read a rank-1 logical attribute
!
! Read a rank-1 logical attribute attr_name of the object obj_name, which can
! be a group or a dataset. The object and the attribute must exist (see
! AttributeExists). attr_val is allocated to the number of values of the
! attribute. A value is true if the stored character is "T".
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   obj_name : CHARACTER(*) - path of the group or dataset
!   attr_name : CHARACTER(*) - name of the attribute
!   attr_val : LOGICAL(LGT), allocatable - values of the attribute
!
! See also: [[obj_WriteAttribute_b1]]

INTERFACE
  MODULE SUBROUTINE obj_ReadAttribute_b1(obj, obj_name, attr_name, attr_val)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: obj_name, attr_name
    !! path of the group or dataset, and
    !! name of the attribute
    LOGICAL(LGT), ALLOCATABLE, INTENT(INOUT) :: attr_val(:)
    !! values of the attribute; allocated by the routine
  END SUBROUTINE obj_ReadAttribute_b1
END INTERFACE

!----------------------------------------------------------------------------
!                                                               ReadAttribute
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! summary: Read a rank-1 String attribute
!
! Read a rank-1 String attribute attr_name of the object obj_name, which can
! be a group or a dataset. The object and the attribute must exist (see
! AttributeExists). attr_val is allocated to the number of values of the
! attribute. The padding (blanks and NUL characters) is removed from each
! value.
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   obj_name : CHARACTER(*) - path of the group or dataset
!   attr_name : CHARACTER(*) - name of the attribute
!   attr_val : TYPE(String), allocatable - values of the attribute
!
! See also: [[obj_WriteAttribute_st1]]

INTERFACE
  MODULE SUBROUTINE obj_ReadAttribute_st1(obj, obj_name, attr_name, &
                                          attr_val)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: obj_name, attr_name
    !! path of the group or dataset, and
    !! name of the attribute
    TYPE(String), ALLOCATABLE, INTENT(INOUT) :: attr_val(:)
    !! values of the attribute; allocated by the routine
  END SUBROUTINE obj_ReadAttribute_st1
END INTERFACE

#ifdef USE_Real64
!----------------------------------------------------------------------------
!                                                               ReadAttribute
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! summary: Read a Real32 attribute
!
! Read a Real32 attribute attr_name of the object obj_name, which can be a
! group or a dataset. The object and the attribute must exist (see
! AttributeExists).
! Only available when DFP is Real64 (USE_Real64).
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   obj_name : CHARACTER(*) - path of the group or dataset
!   attr_name : CHARACTER(*) - name of the attribute
!   attr_val : REAL(REAL32) - value of the attribute
!
! See also: [[obj_WriteAttribute_s0]]

INTERFACE
  MODULE SUBROUTINE obj_ReadAttribute_s0(obj, obj_name, attr_name, attr_val)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: obj_name, attr_name
    !! path of the group or dataset, and
    !! name of the attribute
    REAL(REAL32), INTENT(INOUT) :: attr_val
    !! value of the attribute
  END SUBROUTINE obj_ReadAttribute_s0
END INTERFACE

!----------------------------------------------------------------------------
!                                                               ReadAttribute
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! summary: Read a rank-1 Real32 attribute
!
! Read a rank-1 Real32 attribute attr_name of the object obj_name, which can
! be a group or a dataset. The object and the attribute must exist (see
! AttributeExists). attr_val is allocated to the number of values of the
! attribute.
! Only available when DFP is Real64 (USE_Real64).
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   obj_name : CHARACTER(*) - path of the group or dataset
!   attr_name : CHARACTER(*) - name of the attribute
!   attr_val : REAL(REAL32), allocatable - values of the attribute
!
! See also: [[obj_WriteAttribute_s1]]

INTERFACE
  MODULE SUBROUTINE obj_ReadAttribute_s1(obj, obj_name, attr_name, attr_val)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: obj_name, attr_name
    !! path of the group or dataset, and
    !! name of the attribute
    REAL(REAL32), ALLOCATABLE, INTENT(INOUT) :: attr_val(:)
    !! values of the attribute; allocated by the routine
  END SUBROUTINE obj_ReadAttribute_s1
END INTERFACE

#endif

#ifndef USE_Int64
!----------------------------------------------------------------------------
!                                                               ReadAttribute
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! summary: Read an Int64 attribute
!
! Read an Int64 attribute attr_name of the object obj_name, which can be a
! group or a dataset. The object and the attribute must exist (see
! AttributeExists).
! Only available when I4B is Int32 (USE_Int64 is not defined).
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   obj_name : CHARACTER(*) - path of the group or dataset
!   attr_name : CHARACTER(*) - name of the attribute
!   attr_val : INTEGER(INT64) - value of the attribute
!
! See also: [[obj_WriteAttribute_l0]]

INTERFACE
  MODULE SUBROUTINE obj_ReadAttribute_l0(obj, obj_name, attr_name, attr_val)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: obj_name, attr_name
    !! path of the group or dataset, and
    !! name of the attribute
    INTEGER(INT64), INTENT(INOUT) :: attr_val
    !! value of the attribute
  END SUBROUTINE obj_ReadAttribute_l0
END INTERFACE

!----------------------------------------------------------------------------
!                                                               ReadAttribute
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! summary: Read a rank-1 Int64 attribute
!
! Read a rank-1 Int64 attribute attr_name of the object obj_name, which can be
! a group or a dataset. The object and the attribute must exist (see
! AttributeExists). attr_val is allocated to the number of values of the
! attribute.
! Only available when I4B is Int32 (USE_Int64 is not defined).
!
! Arguments:
!   obj : CLASS(HDF5File_) - HDF5 file object
!   obj_name : CHARACTER(*) - path of the group or dataset
!   attr_name : CHARACTER(*) - name of the attribute
!   attr_val : INTEGER(INT64), allocatable - values of the attribute
!
! See also: [[obj_WriteAttribute_l1]]

INTERFACE
  MODULE SUBROUTINE obj_ReadAttribute_l1(obj, obj_name, attr_name, attr_val)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: obj_name, attr_name
    !! path of the group or dataset, and
    !! name of the attribute
    INTEGER(INT64), ALLOCATABLE, INTENT(INOUT) :: attr_val(:)
    !! values of the attribute; allocated by the routine
  END SUBROUTINE obj_ReadAttribute_l1
END INTERFACE

#endif

!----------------------------------------------------------------------------
!                                                                OpenObject
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-09
! summary: Open a group or dataset for attribute operations
!
! In DEBUG mode an error is raised if obj_name does not exist or can not be
! opened.
!
! See also: [[close_object]]

INTERFACE
  MODULE SUBROUTINE open_object(obj, obj_name, obj_id)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: obj_name
    !! path of the group or dataset
    INTEGER(HID_T), INTENT(OUT) :: obj_id
    !! id of the opened object
  END SUBROUTINE open_object
END INTERFACE

!----------------------------------------------------------------------------
!                                                                CloseObject
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-09
! summary: Close an object opened by open_object
!
! In DEBUG mode an error is raised if the object can not be closed.
!
! See also: [[open_object]]

INTERFACE
  MODULE SUBROUTINE close_object(obj, obj_id)
    CHARACTER(LEN=*), PARAMETER :: myName = 'close_object_HDF5FileType'
    !! name of the routine used in messages
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    INTEGER(HID_T), INTENT(IN) :: obj_id
    !! id of the object
  END SUBROUTINE close_object
END INTERFACE

!----------------------------------------------------------------------------
!                                                            CreateAttribute
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! update:
!   - 2026-10-26: replace an existing attribute (delete and create)
! summary: Create an attribute, replacing an existing one
!
! If the attribute exists and overwriting is allowed, it is deleted and
! created again with atype_id and dspace_id, so its type and size can
! change. If it exists and overwriting is not allowed, attr_id is -1 and
! nothing is written (DEBUG mode raises an error). attr_id is also -1 if
! the attribute can not be created; the callers write and close it only
! when attr_id > 0.
!
! See also: [[open_attribute]], [[close_attribute]]

INTERFACE
  MODULE SUBROUTINE createAttribute(obj, obj_id, attr_name, atype_id, &
                                    dspace_id, attr_id)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: attr_name
    !! name of the attribute
    INTEGER(HID_T), INTENT(IN) :: atype_id, dspace_id, obj_id
    !! data type of the attribute, dataspace of the
    !! attribute, and id of the object
    INTEGER(HID_T), INTENT(OUT) :: attr_id
    !! id of the attribute
  END SUBROUTINE createAttribute
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Open an existing attribute
!
! In DEBUG mode an error is raised if the attribute does not exist or can not
! be opened.
!
! See also: [[createAttribute]], [[close_attribute]]

INTERFACE
  MODULE SUBROUTINE open_attribute(obj, obj_id, attr_name, attr_id)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    CHARACTER(LEN=*), INTENT(IN) :: attr_name
    !! name of the attribute
    INTEGER(HID_T), INTENT(IN) :: obj_id
    !! id of the object
    INTEGER(HID_T), INTENT(OUT) :: attr_id
    !! id of the attribute
  END SUBROUTINE open_attribute
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Close an attribute
!
! In DEBUG mode an error is raised if the attribute can not be closed.
!
! See also: [[open_attribute]]

INTERFACE
  MODULE SUBROUTINE close_attribute(obj, attr_id)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! HDF5 file object
    INTEGER(HID_T), INTENT(IN) :: attr_id
    !! id of the attribute
  END SUBROUTINE close_attribute
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Append@AppendMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! update:
!   - 2026-10-26: chunk shape and filters from the chunk policy
! summary: Append a rank-1 real array to an extendable dataset
!
! If the dataset does not exist, then an extendable (chunked) dataset
! is created. The size of the last (Fortran) dimension is unlimited,
! and the other dimensions are fixed by the first call.
! The dataset is extended by the size of the last dimension of vals,
! and vals is written at the end of the dataset.
! The chunk shape follows the chunk policy of the file (see Initiate and
! HDF5ChunkPolicy_): a chunk holds chunkPolicy%stepsPerChunk calls of
! Append, at least chunkPolicy%minBytes and at most
! chunkPolicy%targetBytes of data. When compression is on, the shuffle and
! deflate filters of the policy are used.
!
! The parent groups are created if they do not exist.
! It is not possible to append to a dataset created by Write, because
! such a dataset is not extendable.
!
! Arguments:
!   obj : CLASS(HDF5File_) - opened hdf5 file with write access
!   dsetname : CHARACTER(*) - path of dataset
!   vals : REAL(DFP) - values to append, SIZE(vals)
!   offset : INTEGER(I4B) - (optional) the starting index (zero based)
!     of vals in the dataset, i.e., size of the dataset before append
!
! Formula:
!
! Let \(n_0\) be the size of the last dimension before append, and
! \(n\) the size of the last dimension of vals, then
!
! $$ \text{dset}(:, n_0+1:n_0+n) = \text{vals}(:, 1:n),
!    \quad \text{offset} = n_0 $$
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE HDF5, ONLY: HSIZE_T
! USE HDF5File_Class, ONLY: HDF5ChunkPolicy_
! USE HDF5File_Class, ONLY: HDF5File_
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: filename = "Append_test_1.h5"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(HDF5File_) :: obj
! TYPE(HDF5ChunkPolicy_) :: policy
! REAL(DFP), ALLOCATABLE :: vals(:), xij(:, :), field(:, :)
! INTEGER(HSIZE_T), ALLOCATABLE :: cdims(:)
! INTEGER(I4B) :: offset, istep
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! CALL obj%Initiate(filename=filename, mode="NEW")
! CALL obj%OPEN()
!
! ! rank-1: one value per call, then two values
! CALL obj%Append("/steps/values", [0.0_DFP], offset)
! CALL OK(offset .EQ. 0, "Append: offset of first call")
! CALL obj%Append("/steps/values", [0.1_DFP, 0.2_DFP], offset)
! CALL OK(offset .EQ. 1, "Append: offset of second call")
! CALL OK(obj%GetExtent("/steps/values") .EQ. 3, "Append: extent")
! CALL obj%ReadSlab("/steps/values", 1, 2, vals)
! CALL OK(ALL(ABS(vals - [0.1_DFP, 0.2_DFP]) .LT. tol), "ReadSlab rank-1")
!
! ! the chunk has at least minBytes (64 KiB): 8192 Real64 values
! CALL obj%GetChunkSize(path="/steps/values", cdims=cdims)
! CALL OK(ALL(cdims .EQ. [8192_HSIZE_T]), "chunk of rank-1 appends")
! CALL OK(obj%IsCompressed(path="/steps/values"), "compressed")
!
! ! rank-2: points (3, nPoints), appended along nPoints
! CALL obj%Append("/points", RESHAPE([0.0_DFP, 0.0_DFP, 0.0_DFP, &
!                 1.0_DFP, 0.0_DFP, 0.0_DFP], [3, 2]))
! CALL obj%Append("/points", RESHAPE([0.0_DFP, 1.0_DFP, 0.0_DFP], &
!                 [3, 1]), offset)
! CALL OK(offset .EQ. 2, "Append rank-2: offset")
! CALL OK(obj%GetExtent("/points") .EQ. 3, "Append rank-2: extent")
! CALL obj%ReadSlab("/points", 2, 1, xij)
! CALL OK(ALL(SHAPE(xij) .EQ. [3, 1]), "ReadSlab rank-2: shape")
! CALL OK(ABS(xij(2, 1) - 1.0_DFP) .LT. tol, "ReadSlab rank-2: value")
! CALL obj%GetChunkSize(path="/points", cdims=cdims)
! CALL OK(ALL(cdims .EQ. [3_HSIZE_T, 2731_HSIZE_T]), "chunk of points")
! CALL obj%DEALLOCATE()
!
! ! a field of 3000 values per time step, 4 steps per chunk
! policy%stepsPerChunk = 4
! CALL obj%Initiate(filename=filename, mode="NEW", chunkPolicy=policy)
! CALL obj%OPEN()
! ALLOCATE (field(3000, 1))
! DO istep = 1, 5
!   field = REAL(istep, DFP)
!   CALL obj%Append("/field", field)
! END DO
! CALL OK(obj%GetExtent("/field") .EQ. 5, "time steps: extent")
! CALL obj%GetChunkSize(path="/field", cdims=cdims)
! CALL OK(ALL(cdims .EQ. [3000_HSIZE_T, 4_HSIZE_T]), &
!         "stepsPerChunk=4: chunk of 4 steps")
! CALL obj%ReadSlab("/field", 4, 1, xij)
! CALL OK(ALL(ABS(xij - 5.0_DFP) .LT. tol), "time steps: last step")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_readslab_d1]], [[obj_GetExtent]]

INTERFACE
  MODULE SUBROUTINE obj_append_d1(obj, dsetname, vals, offset)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! hdf5 file
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! path of dataset
    REAL(DFP), TARGET, CONTIGUOUS, INTENT(IN) :: vals(:)
    !! values to append
    INTEGER(I4B), INTENT(OUT), OPTIONAL :: offset
    !! zero based starting index of vals in the dataset
  END SUBROUTINE obj_append_d1
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Append@AppendMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Append a rank-2 real array to an extendable dataset
!
! vals(:, 1:n) is appended along the second (last) dimension. The size
! of the first dimension should be the same in all calls.
!
! Arguments:
!   obj : CLASS(HDF5File_) - opened hdf5 file with write access
!   dsetname : CHARACTER(*) - path of dataset
!   vals : REAL(DFP) - values to append, SIZE(vals, 1) x SIZE(vals, 2)
!   offset : INTEGER(I4B) - (optional) zero based starting column
!
! See also: [[obj_append_d1]]

INTERFACE
  MODULE SUBROUTINE obj_append_d2(obj, dsetname, vals, offset)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! hdf5 file
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! path of dataset
    REAL(DFP), TARGET, CONTIGUOUS, INTENT(IN) :: vals(:, :)
    !! values to append
    INTEGER(I4B), INTENT(OUT), OPTIONAL :: offset
    !! zero based starting column of vals in the dataset
  END SUBROUTINE obj_append_d2
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Append@AppendMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Append a rank-1 integer array to an extendable dataset
!
! Arguments:
!   obj : CLASS(HDF5File_) - opened hdf5 file with write access
!   dsetname : CHARACTER(*) - path of dataset
!   vals : INTEGER(I4B) - values to append, SIZE(vals)
!   offset : INTEGER(I4B) - (optional) zero based starting index
!
! See also: [[obj_append_d1]]

INTERFACE
  MODULE SUBROUTINE obj_append_n1(obj, dsetname, vals, offset)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! hdf5 file
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! path of dataset
    INTEGER(I4B), TARGET, CONTIGUOUS, INTENT(IN) :: vals(:)
    !! values to append
    INTEGER(I4B), INTENT(OUT), OPTIONAL :: offset
    !! zero based starting index of vals in the dataset
  END SUBROUTINE obj_append_n1
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Append@AppendMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Append a rank-2 integer array to an extendable dataset
!
! Arguments:
!   obj : CLASS(HDF5File_) - opened hdf5 file with write access
!   dsetname : CHARACTER(*) - path of dataset
!   vals : INTEGER(I4B) - values to append, SIZE(vals, 1) x SIZE(vals, 2)
!   offset : INTEGER(I4B) - (optional) zero based starting column
!
! See also: [[obj_append_d1]]

INTERFACE
  MODULE SUBROUTINE obj_append_n2(obj, dsetname, vals, offset)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! hdf5 file
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! path of dataset
    INTEGER(I4B), TARGET, CONTIGUOUS, INTENT(IN) :: vals(:, :)
    !! values to append
    INTEGER(I4B), INTENT(OUT), OPTIONAL :: offset
    !! zero based starting column of vals in the dataset
  END SUBROUTINE obj_append_n2
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Append@AppendMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Append a rank-1 INT8 array as unsigned 8-bit integers
!
! The values are stored as unsigned 8-bit integers (H5T_STD_U8LE),
! which is the data type of cell types in VTK. Only values in
! [0, 127] are converted exactly.
!
! Arguments:
!   obj : CLASS(HDF5File_) - opened hdf5 file with write access
!   dsetname : CHARACTER(*) - path of dataset
!   vals : INTEGER(INT8) - values to append, SIZE(vals)
!   offset : INTEGER(I4B) - (optional) zero based starting index
!
! See also: [[obj_append_d1]], [[obj_readslab_u1]]

INTERFACE
  MODULE SUBROUTINE obj_append_u1(obj, dsetname, vals, offset)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! hdf5 file
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! path of dataset
    INTEGER(INT8), TARGET, CONTIGUOUS, INTENT(IN) :: vals(:)
    !! values to append
    INTEGER(I4B), INTENT(OUT), OPTIONAL :: offset
    !! zero based starting index of vals in the dataset
  END SUBROUTINE obj_append_u1
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ReadSlab@AppendMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Read a slab of a rank-1 real dataset
!
! icount entries are read starting from the zero based index istart.
! vals is reallocated to SIZE icount. The dataset can be extendable
! or not.
!
! Arguments:
!   obj : CLASS(HDF5File_) - opened hdf5 file with read access
!   dsetname : CHARACTER(*) - path of dataset
!   istart : INTEGER(I4B) - zero based starting index
!   icount : INTEGER(I4B) - number of entries to read
!   vals : REAL(DFP) - values (allocatable), SIZE(vals) = icount
!
! Formula:
!
! $$ \text{vals}(:, 1:n) = \text{dset}(:, s+1:s+n),
!    \quad s = \text{istart}, n = \text{icount} $$
!
! See also: [[obj_append_d1]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_readslab_d1(obj, dsetname, istart, icount, vals)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! hdf5 file
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! path of dataset
    INTEGER(I4B), INTENT(IN) :: istart
    !! zero based starting index
    INTEGER(I4B), INTENT(IN) :: icount
    !! number of entries
    REAL(DFP), ALLOCATABLE, TARGET, INTENT(INOUT) :: vals(:)
    !! values read
  END SUBROUTINE obj_readslab_d1
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ReadSlab@AppendMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Read a slab of a rank-2 real dataset along the last dimension
!
! Columns istart+1 to istart+icount are read. The size of the first
! dimension is taken from the dataset.
!
! Arguments:
!   obj : CLASS(HDF5File_) - opened hdf5 file with read access
!   dsetname : CHARACTER(*) - path of dataset
!   istart : INTEGER(I4B) - zero based starting column
!   icount : INTEGER(I4B) - number of columns to read
!   vals : REAL(DFP) - values (allocatable), SIZE(vals, 2) = icount
!
! See also: [[obj_readslab_d1]]

INTERFACE
  MODULE SUBROUTINE obj_readslab_d2(obj, dsetname, istart, icount, vals)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! hdf5 file
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! path of dataset
    INTEGER(I4B), INTENT(IN) :: istart
    !! zero based starting column
    INTEGER(I4B), INTENT(IN) :: icount
    !! number of columns
    REAL(DFP), ALLOCATABLE, TARGET, INTENT(INOUT) :: vals(:, :)
    !! values read
  END SUBROUTINE obj_readslab_d2
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ReadSlab@AppendMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Read a slab of a rank-1 integer dataset
!
! Arguments:
!   obj : CLASS(HDF5File_) - opened hdf5 file with read access
!   dsetname : CHARACTER(*) - path of dataset
!   istart : INTEGER(I4B) - zero based starting index
!   icount : INTEGER(I4B) - number of entries to read
!   vals : INTEGER(I4B) - values (allocatable), SIZE(vals) = icount
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: INT8
! USE HDF5File_Class, ONLY: HDF5File_
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: filename = "ReadSlab_test_1.h5"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(HDF5File_) :: obj
! INTEGER(I4B), ALLOCATABLE :: ivec(:), conn(:, :)
! INTEGER(INT8), ALLOCATABLE :: types(:)
! REAL(DFP) :: m(3, 4)
! REAL(DFP), ALLOCATABLE :: mslab(:, :)
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! m = RESHAPE([1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP, &
!             7.0_DFP, 8.0_DFP, 9.0_DFP, 10.0_DFP, 11.0_DFP, 12.0_DFP], &
!             [3, 4])
!
! CALL obj%Initiate(filename=filename, mode="NEW")
! CALL obj%OPEN()
! CALL obj%Append("/ids", [10_I4B, 20_I4B, 30_I4B])
! CALL obj%Append("/ids", [40_I4B, 50_I4B])
! CALL obj%Append("/cells", RESHAPE([1_I4B, 2_I4B, 3_I4B, &
!                                    2_I4B, 3_I4B, 4_I4B], [3, 2]))
! CALL obj%Append("/types", [5_INT8, 5_INT8, 9_INT8])
! CALL obj%WRITE(dsetname="/m", vals=m)
! CALL obj%DEALLOCATE()
!
! CALL obj%Initiate(filename=filename, mode="READ")
! CALL obj%OPEN()
!
! ! rank-1 integer; ivec is allocated with another size first
! ALLOCATE (ivec(10))
! CALL obj%ReadSlab("/ids", 2, 3, ivec)
! CALL OK(SIZE(ivec) .EQ. 3, "integer rank-1: size")
! CALL OK(ALL(ivec .EQ. [30, 40, 50]), "integer rank-1: values")
!
! ! rank-2 integer: the first dimension is read whole
! CALL obj%ReadSlab("/cells", 1, 1, conn)
! CALL OK(ALL(SHAPE(conn) .EQ. [3, 1]), "integer rank-2: shape")
! CALL OK(ALL(conn(:, 1) .EQ. [2, 3, 4]), "integer rank-2: values")
!
! ! unsigned 8-bit integers
! CALL obj%ReadSlab("/types", 0, 3, types)
! CALL OK(ALL(types .EQ. [5_INT8, 5_INT8, 9_INT8]), "8-bit integers")
!
! ! a slab of a dataset written by Write (not extendable)
! CALL obj%ReadSlab("/m", 1, 2, mslab)
! CALL OK(ALL(SHAPE(mslab) .EQ. [3, 2]), "slab of Write data: shape")
! CALL OK(ALL(ABS(mslab - m(:, 2:3)) .LT. tol), "slab of Write data")
! CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_readslab_d1]], [[obj_append_n1]]

INTERFACE
  MODULE SUBROUTINE obj_readslab_n1(obj, dsetname, istart, icount, vals)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! hdf5 file
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! path of dataset
    INTEGER(I4B), INTENT(IN) :: istart
    !! zero based starting index
    INTEGER(I4B), INTENT(IN) :: icount
    !! number of entries
    INTEGER(I4B), ALLOCATABLE, TARGET, INTENT(INOUT) :: vals(:)
    !! values read
  END SUBROUTINE obj_readslab_n1
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ReadSlab@AppendMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Read a slab of a rank-2 integer dataset along the last dimension
!
! Arguments:
!   obj : CLASS(HDF5File_) - opened hdf5 file with read access
!   dsetname : CHARACTER(*) - path of dataset
!   istart : INTEGER(I4B) - zero based starting column
!   icount : INTEGER(I4B) - number of columns to read
!   vals : INTEGER(I4B) - values (allocatable), SIZE(vals, 2) = icount
!
! See also: [[obj_readslab_d1]]

INTERFACE
  MODULE SUBROUTINE obj_readslab_n2(obj, dsetname, istart, icount, vals)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! hdf5 file
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! path of dataset
    INTEGER(I4B), INTENT(IN) :: istart
    !! zero based starting column
    INTEGER(I4B), INTENT(IN) :: icount
    !! number of columns
    INTEGER(I4B), ALLOCATABLE, TARGET, INTENT(INOUT) :: vals(:, :)
    !! values read
  END SUBROUTINE obj_readslab_n2
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ReadSlab@AppendMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Read a slab of a rank-1 8-bit integer dataset
!
! Arguments:
!   obj : CLASS(HDF5File_) - opened hdf5 file with read access
!   dsetname : CHARACTER(*) - path of dataset
!   istart : INTEGER(I4B) - zero based starting index
!   icount : INTEGER(I4B) - number of entries to read
!   vals : INTEGER(INT8) - values (allocatable), SIZE(vals) = icount
!
! See also: [[obj_append_u1]]

INTERFACE
  MODULE SUBROUTINE obj_readslab_u1(obj, dsetname, istart, icount, vals)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! hdf5 file
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! path of dataset
    INTEGER(I4B), INTENT(IN) :: istart
    !! zero based starting index
    INTEGER(I4B), INTENT(IN) :: icount
    !! number of entries
    INTEGER(INT8), ALLOCATABLE, TARGET, INTENT(INOUT) :: vals(:)
    !! values read
  END SUBROUTINE obj_readslab_u1
END INTERFACE

!----------------------------------------------------------------------------
!                                                    GetExtent@AppendMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Size of the last (Fortran) dimension of a dataset
!
! Returns zero if the dataset does not exist. This is the number of
! entries of an extendable dataset written by Append.
!
! Arguments:
!   obj : CLASS(HDF5File_) - opened hdf5 file
!   dsetname : CHARACTER(*) - path of dataset
!
! Returns:
!   ans : INTEGER(I4B) - size of the last dimension
!
! See also: [[obj_append_d1]] (contains an example)

INTERFACE
  MODULE FUNCTION obj_GetExtent(obj, dsetname) RESULT(ans)
    CLASS(HDF5File_), INTENT(INOUT) :: obj
    !! hdf5 file
    CHARACTER(LEN=*), INTENT(IN) :: dsetname
    !! path of dataset
    INTEGER(I4B) :: ans
    !! size of the last dimension
  END FUNCTION obj_GetExtent
END INTERFACE

END MODULE HDF5File_Class
