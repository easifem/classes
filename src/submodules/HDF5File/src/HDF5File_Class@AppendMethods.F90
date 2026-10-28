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
! update:
!   - 2026-10-26: chunk shape and filters from the chunk policy; HDF5
!     types from the kind of the data; error checks only in DEBUG mode
!   - 2026-10-28: ReadSlab_, read a slab into a buffer
! summary: Append to extendable datasets and read slabs of datasets
!
! The extendable datasets are chunked, and the size of the last
! (Fortran) dimension is unlimited. In C order (h5py, VTK) this is the
! first dimension, i.e., rows are Appended.

SUBMODULE(HDF5File_Class) AppendMethods
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
USE HDF5, ONLY: H5PSET_SHUFFLE_F
USE HDF5, ONLY: H5P_DATASET_CREATE_F
USE HDF5, ONLY: H5SCLOSE_F
USE HDF5, ONLY: H5SCREATE_SIMPLE_F
USE HDF5, ONLY: H5SGET_SIMPLE_EXTENT_DIMS_F
USE HDF5, ONLY: H5SGET_SIMPLE_EXTENT_NDIMS_F
USE HDF5, ONLY: H5SSELECT_HYPERSLAB_F
USE HDF5, ONLY: H5S_SELECT_SET_F
USE HDF5, ONLY: H5S_UNLIMITED_F
USE HDF5, ONLY: H5TGET_SIZE_F
USE HDF5, ONLY: H5KIND_TO_TYPE
USE HDF5, ONLY: H5_INTEGER_KIND
USE HDF5, ONLY: H5_REAL_KIND
USE HDF5, ONLY: H5T_STD_U8LE
USE ISO_C_BINDING, ONLY: C_PTR
USE ISO_C_BINDING, ONLY: C_LOC
USE ISO_C_BINDING, ONLY: C_NULL_PTR
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "HDF5File_Class@AppendMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                    Append
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Append_d1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Append_d1()"
#endif
INTEGER(HSIZE_T) :: ldims(1)
INTEGER(HID_T) :: mem
INTEGER(I4B) :: offset0
TYPE(C_PTR) :: buf

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ldims = SHAPE(vals, KIND=HSIZE_T)
mem = H5KIND_TO_TYPE(KIND(vals), H5_REAL_KIND)
buf = C_NULL_PTR
IF (SIZE(vals) .GT. 0) buf = C_LOC(vals(1))
CALL AppendCore(obj, dsetname, ldims, mem, mem, buf, offset0)
IF (PRESENT(offset)) offset = offset0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Append_d1

!----------------------------------------------------------------------------
!                                                                    Append
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Append_d2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Append_d2()"
#endif
INTEGER(HSIZE_T) :: ldims(2)
INTEGER(HID_T) :: mem
INTEGER(I4B) :: offset0
TYPE(C_PTR) :: buf

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ldims = SHAPE(vals, KIND=HSIZE_T)
mem = H5KIND_TO_TYPE(KIND(vals), H5_REAL_KIND)
buf = C_NULL_PTR
IF (SIZE(vals) .GT. 0) buf = C_LOC(vals(1, 1))
CALL AppendCore(obj, dsetname, ldims, mem, mem, buf, offset0)
IF (PRESENT(offset)) offset = offset0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Append_d2

!----------------------------------------------------------------------------
!                                                                    Append
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Append_n1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Append_n1()"
#endif
INTEGER(HSIZE_T) :: ldims(1)
INTEGER(HID_T) :: mem
INTEGER(I4B) :: offset0
TYPE(C_PTR) :: buf

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ldims = SHAPE(vals, KIND=HSIZE_T)
mem = H5KIND_TO_TYPE(KIND(vals), H5_INTEGER_KIND)
buf = C_NULL_PTR
IF (SIZE(vals) .GT. 0) buf = C_LOC(vals(1))
CALL AppendCore(obj, dsetname, ldims, mem, mem, buf, offset0)
IF (PRESENT(offset)) offset = offset0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Append_n1

!----------------------------------------------------------------------------
!                                                                    Append
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Append_n2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Append_n2()"
#endif
INTEGER(HSIZE_T) :: ldims(2)
INTEGER(HID_T) :: mem
INTEGER(I4B) :: offset0
TYPE(C_PTR) :: buf

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ldims = SHAPE(vals, KIND=HSIZE_T)
mem = H5KIND_TO_TYPE(KIND(vals), H5_INTEGER_KIND)
buf = C_NULL_PTR
IF (SIZE(vals) .GT. 0) buf = C_LOC(vals(1, 1))
CALL AppendCore(obj, dsetname, ldims, mem, mem, buf, offset0)
IF (PRESENT(offset)) offset = offset0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Append_n2

!----------------------------------------------------------------------------
!                                                                    Append
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Append_u1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Append_u1()"
#endif
INTEGER(HSIZE_T) :: ldims(1)
INTEGER(HID_T) :: mem
INTEGER(I4B) :: offset0
TYPE(C_PTR) :: buf

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! The file type is unsigned 8-bit (for example VTK cell types); HDF5
! converts from the signed INT8 in memory, so negative values become 0
ldims = SHAPE(vals, KIND=HSIZE_T)
mem = H5KIND_TO_TYPE(KIND(vals), H5_INTEGER_KIND)
buf = C_NULL_PTR
IF (SIZE(vals) .GT. 0) buf = C_LOC(vals(1))
CALL AppendCore(obj, dsetname, ldims, H5T_STD_U8LE, mem, buf, offset0)
IF (PRESENT(offset)) offset = offset0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Append_u1

!----------------------------------------------------------------------------
!                                                                  ReadSlab
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ReadSlab_d1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ReadSlab_d1()"
#endif
INTEGER(HID_T) :: dset_id, fspace_id
INTEGER(HSIZE_T) :: cnt(1)
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL SlabOpen(obj, dsetname, istart, icount, dset_id, fspace_id, cnt, &
              isok)

IF (ALLOCATED(vals)) THEN
  IF (SIZE(vals) .NE. icount) DEALLOCATE (vals)
END IF
IF (.NOT. ALLOCATED(vals)) ALLOCATE (vals(MAX(icount, 0)))

IF (isok .AND. icount .GT. 0) THEN
  CALL SlabRead(dset_id, fspace_id, istart, cnt, &
                H5KIND_TO_TYPE(KIND(vals), H5_REAL_KIND), C_LOC(vals(1)))
END IF
CALL SlabClose(dset_id, fspace_id)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ReadSlab_d1

!----------------------------------------------------------------------------
!                                                                  ReadSlab
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ReadSlab_d2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ReadSlab_d2()"
#endif
INTEGER(HID_T) :: dset_id, fspace_id
INTEGER(HSIZE_T) :: cnt(2)
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! The first dimension is read whole; cnt(1) is its size in the file
CALL SlabOpen(obj, dsetname, istart, icount, dset_id, fspace_id, cnt, &
              isok)

IF (ALLOCATED(vals)) THEN
  IF (ANY(SHAPE(vals) .NE. cnt)) DEALLOCATE (vals)
END IF
IF (.NOT. ALLOCATED(vals)) ALLOCATE (vals(cnt(1), cnt(2)))

IF (isok .AND. SIZE(vals) .GT. 0) THEN
  CALL SlabRead(dset_id, fspace_id, istart, cnt, &
                H5KIND_TO_TYPE(KIND(vals), H5_REAL_KIND), &
                C_LOC(vals(1, 1)))
END IF
CALL SlabClose(dset_id, fspace_id)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ReadSlab_d2

!----------------------------------------------------------------------------
!                                                                  ReadSlab
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ReadSlab_n1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ReadSlab_n1()"
#endif
INTEGER(HID_T) :: dset_id, fspace_id
INTEGER(HSIZE_T) :: cnt(1)
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL SlabOpen(obj, dsetname, istart, icount, dset_id, fspace_id, cnt, &
              isok)

IF (ALLOCATED(vals)) THEN
  IF (SIZE(vals) .NE. icount) DEALLOCATE (vals)
END IF
IF (.NOT. ALLOCATED(vals)) ALLOCATE (vals(MAX(icount, 0)))

IF (isok .AND. icount .GT. 0) THEN
  CALL SlabRead(dset_id, fspace_id, istart, cnt, &
                H5KIND_TO_TYPE(KIND(vals), H5_INTEGER_KIND), &
                C_LOC(vals(1)))
END IF
CALL SlabClose(dset_id, fspace_id)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ReadSlab_n1

!----------------------------------------------------------------------------
!                                                                  ReadSlab
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ReadSlab_n2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ReadSlab_n2()"
#endif
INTEGER(HID_T) :: dset_id, fspace_id
INTEGER(HSIZE_T) :: cnt(2)
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! The first dimension is read whole; cnt(1) is its size in the file
CALL SlabOpen(obj, dsetname, istart, icount, dset_id, fspace_id, cnt, &
              isok)

IF (ALLOCATED(vals)) THEN
  IF (ANY(SHAPE(vals) .NE. cnt)) DEALLOCATE (vals)
END IF
IF (.NOT. ALLOCATED(vals)) ALLOCATE (vals(cnt(1), cnt(2)))

IF (isok .AND. SIZE(vals) .GT. 0) THEN
  CALL SlabRead(dset_id, fspace_id, istart, cnt, &
                H5KIND_TO_TYPE(KIND(vals), H5_INTEGER_KIND), &
                C_LOC(vals(1, 1)))
END IF
CALL SlabClose(dset_id, fspace_id)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ReadSlab_n2

!----------------------------------------------------------------------------
!                                                                  ReadSlab
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ReadSlab_u1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ReadSlab_u1()"
#endif
INTEGER(HID_T) :: dset_id, fspace_id
INTEGER(HSIZE_T) :: cnt(1)
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL SlabOpen(obj, dsetname, istart, icount, dset_id, fspace_id, cnt, &
              isok)

IF (ALLOCATED(vals)) THEN
  IF (SIZE(vals) .NE. icount) DEALLOCATE (vals)
END IF
IF (.NOT. ALLOCATED(vals)) ALLOCATE (vals(MAX(icount, 0)))

IF (isok .AND. icount .GT. 0) THEN
  CALL SlabRead(dset_id, fspace_id, istart, cnt, &
                H5KIND_TO_TYPE(KIND(vals), H5_INTEGER_KIND), &
                C_LOC(vals(1)))
END IF
CALL SlabClose(dset_id, fspace_id)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ReadSlab_u1

!----------------------------------------------------------------------------
!                                                                   ReadSlab_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ReadSlab_d1_
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ReadSlab_d1_()"
#endif
INTEGER(HID_T) :: dset_id, fspace_id
INTEGER(HSIZE_T) :: cnt(1)
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL SlabOpen(obj, dsetname, istart, icount, dset_id, fspace_id, cnt, &
              isok)
tsize = INT(cnt(1), I4B)

#ifdef DEBUG_VER
CALL AssertError3(tsize, SIZE(vals), myName, &
                  "icount should not be larger than SIZE(vals)")
#endif

IF (isok .AND. tsize .GT. 0) THEN
  CALL SlabRead(dset_id, fspace_id, istart, cnt, &
                H5KIND_TO_TYPE(KIND(vals), H5_REAL_KIND), C_LOC(vals(1)))
END IF
CALL SlabClose(dset_id, fspace_id)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ReadSlab_d1_

!----------------------------------------------------------------------------
!                                                                   ReadSlab_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ReadSlab_d2_
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ReadSlab_d2_()"
#endif
INTEGER(HID_T) :: dset_id, fspace_id
INTEGER(HSIZE_T) :: cnt(2)
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! The first dimension is read whole; cnt(1) is its size in the file
CALL SlabOpen(obj, dsetname, istart, icount, dset_id, fspace_id, cnt, &
              isok)
nrow = INT(cnt(1), I4B)
ncol = INT(cnt(2), I4B)

#ifdef DEBUG_VER
CALL AssertError3(nrow, SIZE(vals, 1), myName, &
                  "rows of the dataset and SIZE(vals, 1)")
CALL AssertError3(ncol, SIZE(vals, 2), myName, &
                  "icount and SIZE(vals, 2)")
#endif

! The slab is placed in vals(1:nrow, 1:ncol) of the whole buffer
IF (isok .AND. nrow * ncol .GT. 0) THEN
  CALL SlabRead(dset_id, fspace_id, istart, cnt, &
                H5KIND_TO_TYPE(KIND(vals), H5_REAL_KIND), &
                C_LOC(vals(1, 1)), SHAPE(vals, KIND=HSIZE_T))
END IF
CALL SlabClose(dset_id, fspace_id)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ReadSlab_d2_

!----------------------------------------------------------------------------
!                                                                   ReadSlab_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ReadSlab_n1_
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ReadSlab_n1_()"
#endif
INTEGER(HID_T) :: dset_id, fspace_id
INTEGER(HSIZE_T) :: cnt(1)
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL SlabOpen(obj, dsetname, istart, icount, dset_id, fspace_id, cnt, &
              isok)
tsize = INT(cnt(1), I4B)

#ifdef DEBUG_VER
CALL AssertError3(tsize, SIZE(vals), myName, &
                  "icount should not be larger than SIZE(vals)")
#endif

IF (isok .AND. tsize .GT. 0) THEN
  CALL SlabRead(dset_id, fspace_id, istart, cnt, &
                H5KIND_TO_TYPE(KIND(vals), H5_INTEGER_KIND), C_LOC(vals(1)))
END IF
CALL SlabClose(dset_id, fspace_id)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ReadSlab_n1_

!----------------------------------------------------------------------------
!                                                                   ReadSlab_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ReadSlab_n2_
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ReadSlab_n2_()"
#endif
INTEGER(HID_T) :: dset_id, fspace_id
INTEGER(HSIZE_T) :: cnt(2)
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! The first dimension is read whole; cnt(1) is its size in the file
CALL SlabOpen(obj, dsetname, istart, icount, dset_id, fspace_id, cnt, &
              isok)
nrow = INT(cnt(1), I4B)
ncol = INT(cnt(2), I4B)

#ifdef DEBUG_VER
CALL AssertError3(nrow, SIZE(vals, 1), myName, &
                  "rows of the dataset and SIZE(vals, 1)")
CALL AssertError3(ncol, SIZE(vals, 2), myName, &
                  "icount and SIZE(vals, 2)")
#endif

! The slab is placed in vals(1:nrow, 1:ncol) of the whole buffer
IF (isok .AND. nrow * ncol .GT. 0) THEN
  CALL SlabRead(dset_id, fspace_id, istart, cnt, &
                H5KIND_TO_TYPE(KIND(vals), H5_INTEGER_KIND), &
                C_LOC(vals(1, 1)), SHAPE(vals, KIND=HSIZE_T))
END IF
CALL SlabClose(dset_id, fspace_id)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ReadSlab_n2_

!----------------------------------------------------------------------------
!                                                                   ReadSlab_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ReadSlab_u1_
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ReadSlab_u1_()"
#endif
INTEGER(HID_T) :: dset_id, fspace_id
INTEGER(HSIZE_T) :: cnt(1)
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL SlabOpen(obj, dsetname, istart, icount, dset_id, fspace_id, cnt, &
              isok)
tsize = INT(cnt(1), I4B)

#ifdef DEBUG_VER
CALL AssertError3(tsize, SIZE(vals), myName, &
                  "icount should not be larger than SIZE(vals)")
#endif

IF (isok .AND. tsize .GT. 0) THEN
  CALL SlabRead(dset_id, fspace_id, istart, cnt, &
                H5KIND_TO_TYPE(KIND(vals), H5_INTEGER_KIND), C_LOC(vals(1)))
END IF
CALL SlabClose(dset_id, fspace_id)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ReadSlab_u1_

!----------------------------------------------------------------------------
!                                                                 GetExtent
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetExtent
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetExtent()"
#endif
INTEGER(HID_T) :: dset_id, space_id
INTEGER(HSIZE_T) :: dims(7), maxdims(7)
INTEGER(I4B) :: ndims, ERROR
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = 0
isok = obj%PathExists(dsetname)

IF (isok) THEN
  CALL H5DOPEN_F(obj%file_id, dsetname, dset_id, ERROR)
  CALL H5DGET_SPACE_F(dset_id, space_id, ERROR)
  CALL H5SGET_SIMPLE_EXTENT_NDIMS_F(space_id, ndims, ERROR)
  IF (ndims .GT. 0) THEN
    CALL H5SGET_SIMPLE_EXTENT_DIMS_F(space_id, dims(1:ndims), &
                                     maxdims(1:ndims), ERROR)
    ans = INT(dims(ndims), I4B)
  ELSE
    ans = 1
  END IF
  CALL H5SCLOSE_F(space_id, ERROR)
  CALL H5DCLOSE_F(dset_id, ERROR)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetExtent

!----------------------------------------------------------------------------
!                                                                AppendCore
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-10-24
! update:
!   - 2026-10-26: chunk shape and filters from obj%chunkPolicy; error
!     checks only in DEBUG mode
! summary: Append data to an extendable dataset (creates if needed)
!
! ldims is the shape of the data (Fortran order). The data is Appended
! along the last dimension. filetype is the data type in the file and
! memtype is the data type in memory (buf).
!
! When the dataset is created, its chunk shape is computed from
! obj%chunkPolicy, see AppendChunkSize. When compression is on, the
! shuffle filter (chunkPolicy%shuffle, elements larger than 1 byte) and
! the deflate filter (chunkPolicy%deflateLevel) are set.
!
! If a step fails (no write access, wrong rank or shape, the dataset can
! not be created or extended), nothing more is done and offset is the
! size before the call; DEBUG mode raises an error.

SUBROUTINE AppendCore(obj, path, ldims, filetype, memtype, buf, offset)
  CLASS(HDF5File_), INTENT(INOUT) :: obj
  !! HDF5 file object
  CHARACTER(*), INTENT(IN) :: path
  !! path of the dataset
  INTEGER(HSIZE_T), INTENT(IN) :: ldims(:)
  !! shape of the data to Append (Fortran order)
  INTEGER(HID_T), INTENT(IN) :: filetype, memtype
  !! data type in the file and in memory
  TYPE(C_PTR), INTENT(IN) :: buf
  !! address of the data
  INTEGER(I4B), INTENT(OUT) :: offset
  !! size of the last dimension before the call

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "AppendCore()"
#endif
  INTEGER(HID_T) :: dset_id, fspace_id, mspace_id, dcpl_id
  INTEGER(HSIZE_T) :: curdims(SIZE(ldims)), maxdims(SIZE(ldims)), &
                      newdims(SIZE(ldims)), chunk(SIZE(ldims)), &
                      start(SIZE(ldims))
  INTEGER(SIZE_T) :: bsize
  INTEGER(I4B) :: RANK, ERROR, ierr, lastslash, ndims
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  RANK = SIZE(ldims)
  offset = 0

  isok = obj%IsWrite()

#ifdef DEBUG_VER
  CALL AssertError1(isok, myName, &
                    'file does not have write access.')
#endif

  IF (.NOT. isok) RETURN

  IF (.NOT. obj%PathExists(path)) THEN

    ! The dimensions other than the last one are fixed by the first call
    isok = .NOT. ANY(ldims(1:RANK - 1) .EQ. 0)

#ifdef DEBUG_VER
    CALL AssertError1(isok, myName, &
                      'Only the last dimension can be zero in the '// &
                      'first Append of '//path)
#endif

    IF (.NOT. isok) RETURN

    ! Create the parent groups
    lastslash = INDEX(path, '/', .TRUE.)
    IF (lastslash .GT. 1) THEN
      IF (.NOT. obj%PathExists(path(1:lastslash - 1))) THEN
        CALL obj%Mkdir(path(1:lastslash - 1))
      END IF
    END IF

    ! Create an empty extendable dataset
    curdims = ldims
    curdims(RANK) = 0
    maxdims = ldims
    maxdims(RANK) = H5S_UNLIMITED_F

    CALL H5TGET_SIZE_F(filetype, bsize, ERROR)
    CALL AppendChunkSize(obj%chunkPolicy, ldims, INT(bsize, INT64), &
                         filetype, chunk)

    CALL H5SCREATE_SIMPLE_F(RANK, curdims, fspace_id, ERROR, maxdims)
    CALL H5PCREATE_F(H5P_DATASET_CREATE_F, dcpl_id, ERROR)
    CALL H5PSET_CHUNK_F(dcpl_id, RANK, chunk, ERROR)

    IF (obj%hasCompression) THEN
      ! Shuffle the bytes before deflate; useless for 1-byte elements
      IF (obj%chunkPolicy%shuffle .AND. bsize .GT. 1) THEN
        CALL H5PSET_SHUFFLE_F(dcpl_id, ERROR)
      END IF
      CALL H5PSET_DEFLATE_F(dcpl_id, obj%chunkPolicy%deflateLevel, ERROR)
    END IF

    CALL H5DCREATE_F(obj%file_id, path, filetype, fspace_id, dset_id, &
                     ERROR, dcpl_id=dcpl_id)
    CALL H5PCLOSE_F(dcpl_id, ierr)
    CALL H5SCLOSE_F(fspace_id, ierr)
    isok = ERROR .EQ. 0

#ifdef DEBUG_VER
    CALL AssertError1(isok, myName, &
                      'Could not create dataset '//path)
#endif

    IF (.NOT. isok) RETURN

  ELSE
    CALL H5DOPEN_F(obj%file_id, path, dset_id, ERROR)
    CALL H5DGET_SPACE_F(dset_id, fspace_id, ERROR)
    CALL H5SGET_SIMPLE_EXTENT_NDIMS_F(fspace_id, ndims, ERROR)
    isok = ndims .EQ. RANK
    IF (isok) THEN
      CALL H5SGET_SIMPLE_EXTENT_DIMS_F(fspace_id, curdims, maxdims, ERROR)
      isok = ALL(curdims(1:RANK - 1) .EQ. ldims(1:RANK - 1))
    END IF
    CALL H5SCLOSE_F(fspace_id, ERROR)

#ifdef DEBUG_VER
    CALL AssertError1(isok, myName, &
                      'Rank or shape of data does not match the '// &
                      'existing dataset '//path)
#endif

    IF (.NOT. isok) THEN
      CALL H5DCLOSE_F(dset_id, ERROR)
      RETURN
    END IF
  END IF

  offset = INT(curdims(RANK), I4B)

  IF (ldims(RANK) .GT. 0) THEN
    newdims = curdims
    newdims(RANK) = curdims(RANK) + ldims(RANK)
    CALL H5DSET_EXTENT_F(dset_id, newdims, ERROR)
    isok = ERROR .EQ. 0

#ifdef DEBUG_VER
    CALL AssertError1(isok, myName, &
                      'Could not extend dataset '//path// &
                      ', it should be created by Append.')
#endif

    IF (isok) THEN
      CALL H5DGET_SPACE_F(dset_id, fspace_id, ERROR)
      start = 0
      start(RANK) = curdims(RANK)
      CALL H5SSELECT_HYPERSLAB_F(fspace_id, H5S_SELECT_SET_F, start, &
                                 ldims, ERROR)
      CALL H5SCREATE_SIMPLE_F(RANK, ldims, mspace_id, ERROR)
      CALL H5DWRITE_F(dset_id, memtype, buf, ERROR, mspace_id, fspace_id)

#ifdef DEBUG_VER
      CALL AssertError1(ERROR .EQ. 0, myName, &
                        'Could not write to dataset '//path)
#endif

      CALL H5SCLOSE_F(mspace_id, ierr)
      CALL H5SCLOSE_F(fspace_id, ierr)
    END IF
  END IF

  CALL H5DCLOSE_F(dset_id, ERROR)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE AppendCore

!----------------------------------------------------------------------------
!                                                           AppendChunkSize
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! summary: Chunk shape of an extendable dataset from the chunk policy
!
! A step is one call of Append, with the shape ldims. The chunk is
! computed in three steps:
!
! 1. It holds policy%stepsPerChunk steps: the last dimension is
!    MAX(ldims(rank), 1) * stepsPerChunk.
! 2. If it has less than policy%minBytes bytes, its last dimension is
!    increased until it has policy%minBytes bytes; so Appending a few
!    values per call does not create tiny chunks.
! 3. It is limited to policy%targetBytes with compute_chunk_size (the
!    same rule and policy%chunkMethod as for Write).
!
! For example, with the default policy (minBytes 64 KiB, targetBytes
! 1 MiB, stepsPerChunk 1) and Real64 data: one value per call gives a
! chunk of 8192 values; one column of 3 values per call gives 3 x 2731;
! a field of 1000 values per call gives 1000 x 9.

SUBROUTINE AppendChunkSize(policy, ldims, bsize, filetype, chunk)
  TYPE(HDF5ChunkPolicy_), INTENT(IN) :: policy
  !! chunk policy of the file
  INTEGER(HSIZE_T), INTENT(IN) :: ldims(:)
  !! shape of one step (Fortran order)
  INTEGER(INT64), INTENT(IN) :: bsize
  !! size of an element in bytes
  INTEGER(HID_T), INTENT(IN) :: filetype
  !! data type in the file
  INTEGER(HSIZE_T), INTENT(OUT) :: chunk(:)
  !! chunk shape

  INTEGER(HSIZE_T) :: step(SIZE(ldims)), nother, nlast
  INTEGER(I4B) :: RANK

  RANK = SIZE(ldims)
  step = MAX(ldims, 1_HSIZE_T)
  step(RANK) = step(RANK) * INT(MAX(policy%stepsPerChunk, 1), HSIZE_T)

  ! At least minBytes bytes
  nother = MAX(PRODUCT(step(1:RANK - 1)), 1_HSIZE_T)
  IF (INT(MAX(bsize, 1_INT64), HSIZE_T) * nother * step(RANK) .LT. &
      INT(policy%minBytes, HSIZE_T)) THEN
    nlast = (INT(policy%minBytes, HSIZE_T) + &
             INT(MAX(bsize, 1_INT64), HSIZE_T) * nother - 1) / &
            (INT(MAX(bsize, 1_INT64), HSIZE_T) * nother)
    step(RANK) = MAX(step(RANK), nlast)
  END IF

  ! At most targetBytes bytes
  CALL compute_chunk_size(filetype, step, policy, chunk)
END SUBROUTINE AppendChunkSize

!----------------------------------------------------------------------------
!                                                                  SlabOpen
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! summary: Open a dataset and check a slab along the last dimension
!
! Open the dataset path and its dataspace, and return the count of the
! slab: the whole dataset along the first rank-1 dimensions, and icount
! along the last one. isok is false (and DEBUG mode raises an error) if
! the dataset does not exist, has another rank than SIZE(cnt), or the
! slab istart:istart+icount-1 (zero based) is out of bounds. The ids are
! -1 if they are not opened; close them with SlabClose.

SUBROUTINE SlabOpen(obj, path, istart, icount, dset_id, fspace_id, cnt, &
                    isok)
  CLASS(HDF5File_), INTENT(INOUT) :: obj
  !! HDF5 file object
  CHARACTER(*), INTENT(IN) :: path
  !! path of the dataset
  INTEGER(I4B), INTENT(IN) :: istart
  !! zero based start along the last dimension
  INTEGER(I4B), INTENT(IN) :: icount
  !! number of entries along the last dimension
  INTEGER(HID_T), INTENT(OUT) :: dset_id, fspace_id
  !! dataset and dataspace ids, -1 if not opened
  INTEGER(HSIZE_T), INTENT(OUT) :: cnt(:)
  !! count of the slab
  LOGICAL(LGT), INTENT(OUT) :: isok
  !! true if the slab can be read

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "SlabOpen()"
#endif
  INTEGER(HSIZE_T) :: curdims(SIZE(cnt)), maxdims(SIZE(cnt))
  INTEGER(I4B) :: RANK, ERROR, ndims

  RANK = SIZE(cnt)
  dset_id = -1
  fspace_id = -1
  cnt = 0
  cnt(RANK) = INT(MAX(icount, 0), HSIZE_T)

  isok = obj%PathExists(path)

#ifdef DEBUG_VER
  CALL AssertError1(isok, myName, 'Dataset not found: '//path)
#endif

  IF (.NOT. isok) RETURN

  CALL H5DOPEN_F(obj%file_id, path, dset_id, ERROR)
  CALL H5DGET_SPACE_F(dset_id, fspace_id, ERROR)
  CALL H5SGET_SIMPLE_EXTENT_NDIMS_F(fspace_id, ndims, ERROR)
  isok = ndims .EQ. RANK
  IF (isok) THEN
    CALL H5SGET_SIMPLE_EXTENT_DIMS_F(fspace_id, curdims, maxdims, ERROR)
    cnt(1:RANK - 1) = curdims(1:RANK - 1)
    isok = istart .GE. 0 .AND. &
           INT(istart, HSIZE_T) + cnt(RANK) .LE. curdims(RANK)
  END IF

#ifdef DEBUG_VER
  CALL AssertError1(isok, myName, &
                    'Requested slab is out of bounds or rank does not '// &
                    'match: '//path)
#endif
END SUBROUTINE SlabOpen

!----------------------------------------------------------------------------
!                                                                  SlabRead
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! update:
!   - 2026-10-28: optional mdims, shape of a buffer larger than the slab
! summary: Read a slab of an open dataset into buf
!
! If mdims is present, buf is a buffer of shape mdims, and the slab is
! placed in its first cnt entries of each dimension (buf(1:cnt(1),
! 1:cnt(2), ...)). Otherwise buf has the shape cnt.

SUBROUTINE SlabRead(dset_id, fspace_id, istart, cnt, memtype, buf, mdims)
  INTEGER(HID_T), INTENT(IN) :: dset_id, fspace_id
  !! dataset and dataspace ids from SlabOpen
  INTEGER(I4B), INTENT(IN) :: istart
  !! zero based start along the last dimension
  INTEGER(HSIZE_T), INTENT(IN) :: cnt(:)
  !! count of the slab from SlabOpen
  INTEGER(HID_T), INTENT(IN) :: memtype
  !! data type in memory
  TYPE(C_PTR), INTENT(IN) :: buf
  !! address of the data, PRODUCT(cnt) entries (PRODUCT(mdims) entries
  !! if mdims is present)
  INTEGER(HSIZE_T), INTENT(IN), OPTIONAL :: mdims(:)
  !! shape of buf, mdims >= cnt; default is cnt

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "SlabRead()"
#endif
  INTEGER(HID_T) :: mspace_id
  INTEGER(HSIZE_T) :: start(SIZE(cnt)), mstart(SIZE(cnt))
  INTEGER(I4B) :: RANK, ERROR

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  RANK = SIZE(cnt)
  start = 0
  start(RANK) = INT(istart, HSIZE_T)
  CALL H5SSELECT_HYPERSLAB_F(fspace_id, H5S_SELECT_SET_F, start, cnt, &
                             ERROR)

  IF (PRESENT(mdims)) THEN
    CALL H5SCREATE_SIMPLE_F(RANK, mdims, mspace_id, ERROR)
    mstart = 0
    CALL H5SSELECT_HYPERSLAB_F(mspace_id, H5S_SELECT_SET_F, mstart, cnt, &
                               ERROR)
  ELSE
    CALL H5SCREATE_SIMPLE_F(RANK, cnt, mspace_id, ERROR)
  END IF

  CALL H5DREAD_F(dset_id, memtype, buf, ERROR, mspace_id, fspace_id)

#ifdef DEBUG_VER
  CALL AssertError1(ERROR .EQ. 0, myName, 'Could not read the slab.')
#endif

  CALL H5SCLOSE_F(mspace_id, ERROR)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE SlabRead

!----------------------------------------------------------------------------
!                                                                 SlabClose
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! summary: Close the ids opened by SlabOpen

SUBROUTINE SlabClose(dset_id, fspace_id)
  INTEGER(HID_T), INTENT(INOUT) :: dset_id, fspace_id
  !! dataset and dataspace ids; set to -1
  INTEGER(I4B) :: ERROR

  IF (fspace_id .GT. 0) CALL H5SCLOSE_F(fspace_id, ERROR)
  IF (dset_id .GT. 0) CALL H5DCLOSE_F(dset_id, ERROR)
  fspace_id = -1
  dset_id = -1
END SUBROUTINE SlabClose

!----------------------------------------------------------------------------
!                                                             Include errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE AppendMethods
