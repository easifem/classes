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

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Append to extendable datasets and read slabs of datasets
!
! The extendable datasets are chunked, and the size of the last
! (Fortran) dimension is unlimited. In C order (h5py, VTK) this is the
! first dimension, i.e., rows are appended.

SUBMODULE(HDF5File_Class) AppendMethods
USE ISO_C_BINDING, ONLY: C_PTR
USE ISO_C_BINDING, ONLY: C_LOC
USE ISO_C_BINDING, ONLY: C_NULL_PTR
USE HDF5, ONLY: SIZE_T
USE HDF5, ONLY: H5DCLOSE_F
USE HDF5, ONLY: H5DCREATE_F
USE HDF5, ONLY: H5DGET_SPACE_F
USE HDF5, ONLY: H5DOPEN_F
USE HDF5, ONLY: H5DREAD_F
USE HDF5, ONLY: H5DSET_EXTENT_F
USE HDF5, ONLY: H5DWRITE_F
USE HDF5, ONLY: H5PCLOSE_F
USE HDF5, ONLY: H5PCREATE_F
USE HDF5, ONLY: H5PSET_CHUNK_F
USE HDF5, ONLY: H5PSET_DEFLATE_F
USE HDF5, ONLY: H5P_DATASET_CREATE_F
USE HDF5, ONLY: H5SCLOSE_F
USE HDF5, ONLY: H5SCREATE_SIMPLE_F
USE HDF5, ONLY: H5SGET_SIMPLE_EXTENT_DIMS_F
USE HDF5, ONLY: H5SGET_SIMPLE_EXTENT_NDIMS_F
USE HDF5, ONLY: H5SSELECT_HYPERSLAB_F
USE HDF5, ONLY: H5S_SELECT_SET_F
USE HDF5, ONLY: H5S_UNLIMITED_F
USE HDF5, ONLY: H5TGET_SIZE_F
USE HDF5, ONLY: H5T_NATIVE_DOUBLE
USE HDF5, ONLY: H5T_NATIVE_INTEGER
USE HDF5, ONLY: H5T_NATIVE_INTEGER_1
USE HDF5, ONLY: H5T_STD_U8LE
USE ReallocateUtility, ONLY: Reallocate
IMPLICIT NONE

CHARACTER(*), PARAMETER :: modName = "HDF5File_Class@AppendMethods.F90"

INTEGER(HSIZE_T), PARAMETER :: MIN_CHUNK = 1024
!! minimum chunk size along the extendable dimension
INTEGER(SIZE_T), PARAMETER :: CHUNK_BYTES = 1048576
!! maximum size of a chunk in bytes (1MB)

CONTAINS

!----------------------------------------------------------------------------
!                                                                    Append
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_append_d1
INTEGER(HSIZE_T) :: ldims(1)
INTEGER(I4B) :: offset0

ldims = SHAPE(vals, KIND=HSIZE_T)
IF (SIZE(vals) .GT. 0) THEN
  CALL AppendCore(obj, dsetname, ldims, H5T_NATIVE_DOUBLE, &
                  H5T_NATIVE_DOUBLE, C_LOC(vals(1)), offset0)
ELSE
  CALL AppendCore(obj, dsetname, ldims, H5T_NATIVE_DOUBLE, &
                  H5T_NATIVE_DOUBLE, C_NULL_PTR, offset0)
END IF
IF (PRESENT(offset)) offset = offset0
END PROCEDURE hdf5_append_d1

!----------------------------------------------------------------------------
!                                                                    Append
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_append_d2
INTEGER(HSIZE_T) :: ldims(2)
INTEGER(I4B) :: offset0

ldims = SHAPE(vals, KIND=HSIZE_T)
IF (SIZE(vals) .GT. 0) THEN
  CALL AppendCore(obj, dsetname, ldims, H5T_NATIVE_DOUBLE, &
                  H5T_NATIVE_DOUBLE, C_LOC(vals(1, 1)), offset0)
ELSE
  CALL AppendCore(obj, dsetname, ldims, H5T_NATIVE_DOUBLE, &
                  H5T_NATIVE_DOUBLE, C_NULL_PTR, offset0)
END IF
IF (PRESENT(offset)) offset = offset0
END PROCEDURE hdf5_append_d2

!----------------------------------------------------------------------------
!                                                                    Append
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_append_n1
INTEGER(HSIZE_T) :: ldims(1)
INTEGER(I4B) :: offset0

ldims = SHAPE(vals, KIND=HSIZE_T)
IF (SIZE(vals) .GT. 0) THEN
  CALL AppendCore(obj, dsetname, ldims, H5T_NATIVE_INTEGER, &
                  H5T_NATIVE_INTEGER, C_LOC(vals(1)), offset0)
ELSE
  CALL AppendCore(obj, dsetname, ldims, H5T_NATIVE_INTEGER, &
                  H5T_NATIVE_INTEGER, C_NULL_PTR, offset0)
END IF
IF (PRESENT(offset)) offset = offset0
END PROCEDURE hdf5_append_n1

!----------------------------------------------------------------------------
!                                                                    Append
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_append_n2
INTEGER(HSIZE_T) :: ldims(2)
INTEGER(I4B) :: offset0

ldims = SHAPE(vals, KIND=HSIZE_T)
IF (SIZE(vals) .GT. 0) THEN
  CALL AppendCore(obj, dsetname, ldims, H5T_NATIVE_INTEGER, &
                  H5T_NATIVE_INTEGER, C_LOC(vals(1, 1)), offset0)
ELSE
  CALL AppendCore(obj, dsetname, ldims, H5T_NATIVE_INTEGER, &
                  H5T_NATIVE_INTEGER, C_NULL_PTR, offset0)
END IF
IF (PRESENT(offset)) offset = offset0
END PROCEDURE hdf5_append_n2

!----------------------------------------------------------------------------
!                                                                    Append
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_append_u1
INTEGER(HSIZE_T) :: ldims(1)
INTEGER(I4B) :: offset0

ldims = SHAPE(vals, KIND=HSIZE_T)
IF (SIZE(vals) .GT. 0) THEN
  CALL AppendCore(obj, dsetname, ldims, H5T_STD_U8LE, &
                  H5T_NATIVE_INTEGER_1, C_LOC(vals(1)), offset0)
ELSE
  CALL AppendCore(obj, dsetname, ldims, H5T_STD_U8LE, &
                  H5T_NATIVE_INTEGER_1, C_NULL_PTR, offset0)
END IF
IF (PRESENT(offset)) offset = offset0
END PROCEDURE hdf5_append_u1

!----------------------------------------------------------------------------
!                                                                  ReadSlab
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_readslab_d1
INTEGER(HSIZE_T) :: dims(1)

dims(1) = INT(icount, HSIZE_T)
CALL Reallocate(vals, icount)
IF (icount .LE. 0) RETURN
CALL ReadSlabCore(obj, dsetname, dims, istart, H5T_NATIVE_DOUBLE, &
                  C_LOC(vals(1)))
END PROCEDURE hdf5_readslab_d1

!----------------------------------------------------------------------------
!                                                                  ReadSlab
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_readslab_d2
INTEGER(HSIZE_T) :: dims(2)

dims = GetDims(obj, dsetname, 2)
dims(2) = INT(icount, HSIZE_T)
CALL Reallocate(vals, INT(dims(1), I4B), icount)
IF (SIZE(vals) .LE. 0) RETURN
CALL ReadSlabCore(obj, dsetname, dims, istart, H5T_NATIVE_DOUBLE, &
                  C_LOC(vals(1, 1)))
END PROCEDURE hdf5_readslab_d2

!----------------------------------------------------------------------------
!                                                                  ReadSlab
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_readslab_n1
INTEGER(HSIZE_T) :: dims(1)

dims(1) = INT(icount, HSIZE_T)
CALL Reallocate(vals, icount)
IF (icount .LE. 0) RETURN
CALL ReadSlabCore(obj, dsetname, dims, istart, H5T_NATIVE_INTEGER, &
                  C_LOC(vals(1)))
END PROCEDURE hdf5_readslab_n1

!----------------------------------------------------------------------------
!                                                                  ReadSlab
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_readslab_n2
INTEGER(HSIZE_T) :: dims(2)

dims = GetDims(obj, dsetname, 2)
dims(2) = INT(icount, HSIZE_T)
CALL Reallocate(vals, INT(dims(1), I4B), icount)
IF (SIZE(vals) .LE. 0) RETURN
CALL ReadSlabCore(obj, dsetname, dims, istart, H5T_NATIVE_INTEGER, &
                  C_LOC(vals(1, 1)))
END PROCEDURE hdf5_readslab_n2

!----------------------------------------------------------------------------
!                                                                  ReadSlab
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_readslab_u1
INTEGER(HSIZE_T) :: dims(1)

dims(1) = INT(icount, HSIZE_T)
IF (ALLOCATED(vals)) DEALLOCATE (vals)
ALLOCATE (vals(icount))
IF (icount .LE. 0) RETURN
CALL ReadSlabCore(obj, dsetname, dims, istart, H5T_NATIVE_INTEGER_1, &
                  C_LOC(vals(1)))
END PROCEDURE hdf5_readslab_u1

!----------------------------------------------------------------------------
!                                                                 GetExtent
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_GetExtent
INTEGER(HID_T) :: dset_id, space_id
INTEGER(HSIZE_T) :: dims(7), maxdims(7)
INTEGER(I4B) :: ndims, error
LOGICAL :: isok

ans = 0
isok = obj%pathExists(dsetname)
IF (.NOT. isok) RETURN

CALL H5DOPEN_F(obj%file_id, dsetname, dset_id, error)
CALL H5DGET_SPACE_F(dset_id, space_id, error)
CALL H5SGET_SIMPLE_EXTENT_NDIMS_F(space_id, ndims, error)
IF (ndims .GT. 0) THEN
  CALL H5SGET_SIMPLE_EXTENT_DIMS_F(space_id, dims(1:ndims), &
                                   maxdims(1:ndims), error)
  ans = INT(dims(ndims), I4B)
ELSE
  ans = 1
END IF
CALL H5SCLOSE_F(space_id, error)
CALL H5DCLOSE_F(dset_id, error)
END PROCEDURE hdf5_GetExtent

!----------------------------------------------------------------------------
!                                                                AppendCore
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Append data to an extendable dataset (creates if needed)
!
! ldims is the shape of the data (Fortran order). The data is appended
! along the last dimension. filetype is the data type in the file and
! memtype is the data type in memory (buf).

SUBROUTINE AppendCore(obj, path, ldims, filetype, memtype, buf, offset)
  CLASS(HDF5File_), INTENT(INOUT) :: obj
  CHARACTER(*), INTENT(IN) :: path
  INTEGER(HSIZE_T), INTENT(IN) :: ldims(:)
  INTEGER(HID_T), INTENT(IN) :: filetype, memtype
  TYPE(C_PTR), INTENT(IN) :: buf
  INTEGER(I4B), INTENT(OUT) :: offset

  CHARACTER(*), PARAMETER :: myName = "AppendCore()"
  INTEGER(HID_T) :: dset_id, fspace_id, mspace_id, dcpl_id
  INTEGER(HSIZE_T) :: curdims(SIZE(ldims)), maxdims(SIZE(ldims)), &
                      newdims(SIZE(ldims)), chunk(SIZE(ldims)), &
                      start(SIZE(ldims))
  INTEGER(SIZE_T) :: bsize
  INTEGER(I4B) :: rank, error, lastslash
  LOGICAL :: isok

  rank = SIZE(ldims)
  offset = 0

  IF (.NOT. obj%IsWrite()) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[ERROR] :: file does not have write access.')
    RETURN
  END IF

  isok = obj%pathExists(path)

  IF (.NOT. isok) THEN
    ! Create the parent groups
    lastslash = INDEX(path, '/', .TRUE.)
    IF (lastslash .GT. 1) THEN
      IF (.NOT. obj%pathExists(path(1:lastslash - 1))) &
        CALL obj%mkdir(path(1:lastslash - 1))
    END IF

    ! Create an empty extendable dataset
    curdims = ldims
    curdims(rank) = 0
    maxdims = ldims
    maxdims(rank) = H5S_UNLIMITED_F

    IF (ANY(ldims(1:rank - 1) .EQ. 0)) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
                        '[ERROR] :: Only the last dimension can be '// &
                        'zero in the first append of '//path)
      RETURN
    END IF

    CALL H5SCREATE_SIMPLE_F(rank, curdims, fspace_id, error, maxdims)
    CALL H5PCREATE_F(H5P_DATASET_CREATE_F, dcpl_id, error)

    CALL H5TGET_SIZE_F(filetype, bsize, error)
    chunk = ldims
    chunk(rank) = MAX(INT(CHUNK_BYTES / bsize, HSIZE_T) / &
                      MAX(PRODUCT(ldims(1:rank - 1)), 1_HSIZE_T), &
                      1_HSIZE_T)
    chunk(rank) = MIN(chunk(rank), MAX(ldims(rank), MIN_CHUNK))
    CALL H5PSET_CHUNK_F(dcpl_id, rank, chunk, error)

    IF (obj%hasCompression) &
      CALL H5PSET_DEFLATE_F(dcpl_id, obj%zlibOpt, error)

    CALL H5DCREATE_F(obj%file_id, path, filetype, fspace_id, dset_id, &
                     error, dcpl_id=dcpl_id)
    CALL H5PCLOSE_F(dcpl_id, error)
    CALL H5SCLOSE_F(fspace_id, error)

    IF (error .NE. 0) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
                        '[ERROR] :: Could not create dataset '//path)
      RETURN
    END IF

  ELSE
    CALL H5DOPEN_F(obj%file_id, path, dset_id, error)
    CALL H5DGET_SPACE_F(dset_id, fspace_id, error)
    CALL H5SGET_SIMPLE_EXTENT_DIMS_F(fspace_id, curdims, maxdims, error)
    CALL H5SCLOSE_F(fspace_id, error)

    isok = error .EQ. rank
    IF (isok) isok = ALL(curdims(1:rank - 1) .EQ. ldims(1:rank - 1))
    IF (.NOT. isok) THEN
      CALL H5DCLOSE_F(dset_id, error)
      CALL e%RaiseError(modName//'::'//myName//' - '// &
                        '[ERROR] :: Rank or shape of data does not '// &
                        'match the existing dataset '//path)
      RETURN
    END IF
  END IF

  offset = INT(curdims(rank), I4B)

  IF (ldims(rank) .GT. 0) THEN
    newdims = curdims
    newdims(rank) = curdims(rank) + ldims(rank)
    CALL H5DSET_EXTENT_F(dset_id, newdims, error)

    IF (error .NE. 0) THEN
      CALL H5DCLOSE_F(dset_id, error)
      CALL e%RaiseError(modName//'::'//myName//' - '// &
                        '[ERROR] :: Could not extend dataset '//path// &
                        ', it should be created by Append.')
      RETURN
    END IF

    CALL H5DGET_SPACE_F(dset_id, fspace_id, error)
    start = 0
    start(rank) = curdims(rank)
    CALL H5SSELECT_HYPERSLAB_F(fspace_id, H5S_SELECT_SET_F, start, &
                               ldims, error)
    CALL H5SCREATE_SIMPLE_F(rank, ldims, mspace_id, error)
    CALL H5DWRITE_F(dset_id, memtype, buf, error, mspace_id, fspace_id)

    IF (error .NE. 0) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
                        '[ERROR] :: Could not write to dataset '//path)
    END IF

    CALL H5SCLOSE_F(mspace_id, error)
    CALL H5SCLOSE_F(fspace_id, error)
  END IF

  CALL H5DCLOSE_F(dset_id, error)
END SUBROUTINE AppendCore

!----------------------------------------------------------------------------
!                                                              ReadSlabCore
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Read dims(:) entries starting at istart along last dimension

SUBROUTINE ReadSlabCore(obj, path, dims, istart, memtype, buf)
  CLASS(HDF5File_), INTENT(INOUT) :: obj
  CHARACTER(*), INTENT(IN) :: path
  INTEGER(HSIZE_T), INTENT(IN) :: dims(:)
  INTEGER(I4B), INTENT(IN) :: istart
  INTEGER(HID_T), INTENT(IN) :: memtype
  TYPE(C_PTR), INTENT(IN) :: buf

  CHARACTER(*), PARAMETER :: myName = "ReadSlabCore()"
  INTEGER(HID_T) :: dset_id, fspace_id, mspace_id
  INTEGER(HSIZE_T) :: curdims(SIZE(dims)), maxdims(SIZE(dims)), &
                      start(SIZE(dims))
  INTEGER(I4B) :: rank, error
  LOGICAL :: isok

  rank = SIZE(dims)

  isok = obj%pathExists(path)
  IF (.NOT. isok) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[ERROR] :: Dataset not found: '//path)
    RETURN
  END IF

  CALL H5DOPEN_F(obj%file_id, path, dset_id, error)
  CALL H5DGET_SPACE_F(dset_id, fspace_id, error)
  CALL H5SGET_SIMPLE_EXTENT_DIMS_F(fspace_id, curdims, maxdims, error)

  isok = error .EQ. rank
  IF (isok) isok = INT(istart, HSIZE_T) + dims(rank) .LE. curdims(rank)
  IF (isok) isok = ALL(curdims(1:rank - 1) .EQ. dims(1:rank - 1))
  IF (.NOT. isok) THEN
    CALL H5SCLOSE_F(fspace_id, error)
    CALL H5DCLOSE_F(dset_id, error)
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[ERROR] :: Requested slab is out of bounds or '// &
                      'rank does not match: '//path)
    RETURN
  END IF

  start = 0
  start(rank) = INT(istart, HSIZE_T)
  CALL H5SSELECT_HYPERSLAB_F(fspace_id, H5S_SELECT_SET_F, start, dims, &
                             error)
  CALL H5SCREATE_SIMPLE_F(rank, dims, mspace_id, error)
  CALL H5DREAD_F(dset_id, memtype, buf, error, mspace_id, fspace_id)

  IF (error .NE. 0) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[ERROR] :: Could not read dataset '//path)
  END IF

  CALL H5SCLOSE_F(mspace_id, error)
  CALL H5SCLOSE_F(fspace_id, error)
  CALL H5DCLOSE_F(dset_id, error)
END SUBROUTINE ReadSlabCore

!----------------------------------------------------------------------------
!                                                                   GetDims
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! summary: Dimensions of a dataset of given rank (Fortran order)

FUNCTION GetDims(obj, path, rank) RESULT(ans)
  CLASS(HDF5File_), INTENT(INOUT) :: obj
  CHARACTER(*), INTENT(IN) :: path
  INTEGER(I4B), INTENT(IN) :: rank
  INTEGER(HSIZE_T) :: ans(rank)

  CHARACTER(*), PARAMETER :: myName = "GetDims()"
  INTEGER(HID_T) :: dset_id, space_id
  INTEGER(HSIZE_T) :: maxdims(rank)
  INTEGER(I4B) :: error, ndims
  LOGICAL :: isok

  ans = 0
  isok = obj%pathExists(path)
  IF (.NOT. isok) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[ERROR] :: Dataset not found: '//path)
    RETURN
  END IF

  CALL H5DOPEN_F(obj%file_id, path, dset_id, error)
  CALL H5DGET_SPACE_F(dset_id, space_id, error)
  CALL H5SGET_SIMPLE_EXTENT_NDIMS_F(space_id, ndims, error)
  IF (ndims .EQ. rank) THEN
    CALL H5SGET_SIMPLE_EXTENT_DIMS_F(space_id, ans, maxdims, error)
  ELSE
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[ERROR] :: Rank of dataset does not match: '//path)
  END IF
  CALL H5SCLOSE_F(space_id, error)
  CALL H5DCLOSE_F(dset_id, error)
END FUNCTION GetDims

END SUBMODULE AppendMethods
