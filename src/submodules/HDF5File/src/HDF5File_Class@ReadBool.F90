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

SUBMODULE(HDF5File_Class) ReadBool
USE HDF5, ONLY: SIZE_T
USE HDF5, ONLY: H5DREAD_F
USE HDF5, ONLY: H5DGET_TYPE_F
USE HDF5, ONLY: H5TCLOSE_F
USE HDF5, ONLY: H5TGET_CLASS_F
USE HDF5, ONLY: H5TGET_SIZE_F
USE HDF5, ONLY: H5T_STRING_F
USE HDF5, ONLY: H5KIND_TO_TYPE
USE HDF5, ONLY: H5_INTEGER_KIND
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "HDF5File_Class@ReadBool.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                        Read
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Read_b0
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Read_b0()"
#endif
  INTEGER(INT8) :: bvals
  CHARACTER(LEN(dsetname) + 1) :: path
  INTEGER(HSIZE_T), DIMENSION(1) :: dims
  INTEGER(I4B), PARAMETER :: rank = 0
  INTEGER(I4B) :: error, ierr
  INTEGER(HID_T) :: mem, dtype, dspace_id, dset_id
  LOGICAL(LGT) :: isChar

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  path = dsetname
  CALL preRead(obj, path, rank, dset_id, dspace_id, dims, error)
  IF (error .GE. 0) THEN

    ! Read the raw 8-bit values: the enum {FALSE, TRUE}, an integer, or
    ! the characters 'T'/'F' of older files
    CALL H5DGET_TYPE_F(dset_id, dtype, ierr)
    CALL BoolMemType(dtype, mem, isChar)
    CALL H5DREAD_F(dset_id, mem, bvals, dims, error)
    CALL H5TCLOSE_F(dtype, ierr)
    vals = BoolValue(bvals, isChar)
  END IF
  CALL postRead(obj, path, dset_id, dspace_id, error)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_Read_b0

!----------------------------------------------------------------------------
!                                                                        Read
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Read_b1
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Read_b1()"
#endif
  INTEGER(INT8), ALLOCATABLE :: bvals(:)
  CHARACTER(LEN(dsetname) + 1) :: path
  INTEGER(HSIZE_T), DIMENSION(1) :: dims
  INTEGER(I4B), PARAMETER :: rank = 1
  INTEGER(I4B) :: error, ierr
  INTEGER(HID_T) :: mem, dtype, dspace_id, dset_id
  LOGICAL(LGT) :: isChar

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  path = dsetname
  CALL preRead(obj, path, rank, dset_id, dspace_id, dims, error)
  IF (error .GE. 0) THEN
    IF (ALLOCATED(vals)) THEN
      IF (ANY(SHAPE(vals) .NE. dims)) DEALLOCATE(vals)
    END IF
    IF (.NOT. ALLOCATED(vals)) ALLOCATE(vals(dims(1)))
    ALLOCATE(bvals(dims(1)))

    ! Read the raw 8-bit values: the enum {FALSE, TRUE}, an integer, or
    ! the characters 'T'/'F' of older files
    CALL H5DGET_TYPE_F(dset_id, dtype, ierr)
    CALL BoolMemType(dtype, mem, isChar)
    CALL H5DREAD_F(dset_id, mem, bvals, dims, error)
    CALL H5TCLOSE_F(dtype, ierr)
    vals = BoolValue(bvals, isChar)
  END IF
  CALL postRead(obj, path, dset_id, dspace_id, error)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_Read_b1

!----------------------------------------------------------------------------
!                                                                       Read_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Read__b1
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Read__b1()"
#endif
  INTEGER(INT8), ALLOCATABLE :: bvals(:)
  CHARACTER(LEN(dsetname) + 1) :: path
  INTEGER(HSIZE_T), DIMENSION(1) :: dims
  INTEGER(I4B), PARAMETER :: rank = 1
  INTEGER(I4B) :: error, ierr
  INTEGER(HID_T) :: mem, dtype, dspace_id, dset_id
  LOGICAL(LGT) :: isChar

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  tsize = math%zero_i
  path = dsetname
  CALL preRead(obj, path, rank, dset_id, dspace_id, dims, error)
  IF (error .GE. 0) THEN
#ifdef DEBUG_VER
    CALL AssertError3(INT(dims(1), I4B), SIZE(vals, 1), myName, &
                      'a=dims(1) from hdf5File, b=SIZE(vals, 1)')
#endif

    tsize = INT(dims(1), I4B)
    ALLOCATE(bvals(dims(1)))

    ! Read the raw 8-bit values: the enum {FALSE, TRUE}, an integer, or
    ! the characters 'T'/'F' of older files
    CALL H5DGET_TYPE_F(dset_id, dtype, ierr)
    CALL BoolMemType(dtype, mem, isChar)
    CALL H5DREAD_F(dset_id, mem, bvals, dims, error)
    CALL H5TCLOSE_F(dtype, ierr)
    vals(1:tsize) = BoolValue(bvals, isChar)
  END IF
  CALL postRead(obj, path, dset_id, dspace_id, error)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_Read__b1

!----------------------------------------------------------------------------
!                                                                        Read
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Read_b2
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Read_b2()"
#endif
  INTEGER(INT8), ALLOCATABLE :: bvals(:, :)
  CHARACTER(LEN(dsetname) + 1) :: path
  INTEGER(HSIZE_T), DIMENSION(2) :: dims
  INTEGER(I4B), PARAMETER :: rank = 2
  INTEGER(I4B) :: error, ierr
  INTEGER(HID_T) :: mem, dtype, dspace_id, dset_id
  LOGICAL(LGT) :: isChar

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  path = dsetname
  CALL preRead(obj, path, rank, dset_id, dspace_id, dims, error)
  IF (error .GE. 0) THEN
    IF (ALLOCATED(vals)) THEN
      IF (ANY(SHAPE(vals) .NE. dims)) DEALLOCATE(vals)
    END IF
    IF (.NOT. ALLOCATED(vals)) ALLOCATE(vals(dims(1), dims(2)))
    ALLOCATE(bvals(dims(1), dims(2)))

    ! Read the raw 8-bit values: the enum {FALSE, TRUE}, an integer, or
    ! the characters 'T'/'F' of older files
    CALL H5DGET_TYPE_F(dset_id, dtype, ierr)
    CALL BoolMemType(dtype, mem, isChar)
    CALL H5DREAD_F(dset_id, mem, bvals, dims, error)
    CALL H5TCLOSE_F(dtype, ierr)
    vals = BoolValue(bvals, isChar)
  END IF
  CALL postRead(obj, path, dset_id, dspace_id, error)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_Read_b2

!----------------------------------------------------------------------------
!                                                                       Read_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Read__b2
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Read__b2()"
#endif
  INTEGER(INT8), ALLOCATABLE :: bvals(:, :)
  CHARACTER(LEN(dsetname) + 1) :: path
  INTEGER(HSIZE_T), DIMENSION(2) :: dims
  INTEGER(I4B), PARAMETER :: rank = 2
  INTEGER(I4B) :: error, ierr
  INTEGER(HID_T) :: mem, dtype, dspace_id, dset_id
  LOGICAL(LGT) :: isChar

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  nrow = math%zero_i
  ncol = math%zero_i
  path = dsetname
  CALL preRead(obj, path, rank, dset_id, dspace_id, dims, error)
  IF (error .GE. 0) THEN
#ifdef DEBUG_VER
    CALL AssertError3(INT(dims(1), I4B), SIZE(vals, 1), myName, &
                      'a=dims(1) from hdf5File, b=SIZE(vals, 1)')
    CALL AssertError3(INT(dims(2), I4B), SIZE(vals, 2), myName, &
                      'a=dims(2) from hdf5File, b=SIZE(vals, 2)')
#endif

    nrow = INT(dims(1), I4B)
    ncol = INT(dims(2), I4B)
    ALLOCATE(bvals(dims(1), dims(2)))

    ! Read the raw 8-bit values: the enum {FALSE, TRUE}, an integer, or
    ! the characters 'T'/'F' of older files
    CALL H5DGET_TYPE_F(dset_id, dtype, ierr)
    CALL BoolMemType(dtype, mem, isChar)
    CALL H5DREAD_F(dset_id, mem, bvals, dims, error)
    CALL H5TCLOSE_F(dtype, ierr)
    vals(1:nrow, 1:ncol) = BoolValue(bvals, isChar)
  END IF
  CALL postRead(obj, path, dset_id, dspace_id, error)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_Read__b2

!----------------------------------------------------------------------------
!                                                                        Read
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Read_b3
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Read_b3()"
#endif
  INTEGER(INT8), ALLOCATABLE :: bvals(:, :, :)
  CHARACTER(LEN(dsetname) + 1) :: path
  INTEGER(HSIZE_T), DIMENSION(3) :: dims
  INTEGER(I4B), PARAMETER :: rank = 3
  INTEGER(I4B) :: error, ierr
  INTEGER(HID_T) :: mem, dtype, dspace_id, dset_id
  LOGICAL(LGT) :: isChar

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  path = dsetname
  CALL preRead(obj, path, rank, dset_id, dspace_id, dims, error)
  IF (error .GE. 0) THEN
    IF (ALLOCATED(vals)) THEN
      IF (ANY(SHAPE(vals) .NE. dims)) DEALLOCATE(vals)
    END IF
    IF (.NOT. ALLOCATED(vals)) ALLOCATE(vals(dims(1), dims(2), dims(3)))
    ALLOCATE(bvals(dims(1), dims(2), dims(3)))

    ! Read the raw 8-bit values: the enum {FALSE, TRUE}, an integer, or
    ! the characters 'T'/'F' of older files
    CALL H5DGET_TYPE_F(dset_id, dtype, ierr)
    CALL BoolMemType(dtype, mem, isChar)
    CALL H5DREAD_F(dset_id, mem, bvals, dims, error)
    CALL H5TCLOSE_F(dtype, ierr)
    vals = BoolValue(bvals, isChar)
  END IF
  CALL postRead(obj, path, dset_id, dspace_id, error)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_Read_b3

!----------------------------------------------------------------------------
!                                                                       Read_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Read__b3
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Read__b3()"
#endif
  INTEGER(INT8), ALLOCATABLE :: bvals(:, :, :)
  CHARACTER(LEN(dsetname) + 1) :: path
  INTEGER(HSIZE_T), DIMENSION(3) :: dims
  INTEGER(I4B), PARAMETER :: rank = 3
  INTEGER(I4B) :: error, ierr
  INTEGER(HID_T) :: mem, dtype, dspace_id, dset_id
  LOGICAL(LGT) :: isChar

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  dim1 = math%zero_i
  dim2 = math%zero_i
  dim3 = math%zero_i
  path = dsetname
  CALL preRead(obj, path, rank, dset_id, dspace_id, dims, error)
  IF (error .GE. 0) THEN
#ifdef DEBUG_VER
    CALL AssertError3(INT(dims(1), I4B), SIZE(vals, 1), myName, &
                      'a=dims(1) from hdf5File, b=SIZE(vals, 1)')
    CALL AssertError3(INT(dims(2), I4B), SIZE(vals, 2), myName, &
                      'a=dims(2) from hdf5File, b=SIZE(vals, 2)')
    CALL AssertError3(INT(dims(3), I4B), SIZE(vals, 3), myName, &
                      'a=dims(3) from hdf5File, b=SIZE(vals, 3)')
#endif

    dim1 = INT(dims(1), I4B)
    dim2 = INT(dims(2), I4B)
    dim3 = INT(dims(3), I4B)
    ALLOCATE(bvals(dims(1), dims(2), dims(3)))

    ! Read the raw 8-bit values: the enum {FALSE, TRUE}, an integer, or
    ! the characters 'T'/'F' of older files
    CALL H5DGET_TYPE_F(dset_id, dtype, ierr)
    CALL BoolMemType(dtype, mem, isChar)
    CALL H5DREAD_F(dset_id, mem, bvals, dims, error)
    CALL H5TCLOSE_F(dtype, ierr)
    vals(1:dim1, 1:dim2, 1:dim3) = BoolValue(bvals, isChar)
  END IF
  CALL postRead(obj, path, dset_id, dspace_id, error)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_Read__b3

!----------------------------------------------------------------------------
!                                                                 BoolMemType
!----------------------------------------------------------------------------

MODULE PROCEDURE BoolMemType
  INTEGER(SIZE_T) :: tsize
  INTEGER(I4B) :: tclass, error

  CALL H5TGET_CLASS_F(dtype, tclass, error)
  CALL H5TGET_SIZE_F(dtype, tsize, error)
  isChar = tclass .EQ. H5T_STRING_F

  IF (tsize .EQ. 1) THEN
    ! The enum {FALSE, TRUE}, an 8-bit integer, or a character: the bytes
    ! are read as they are
    memtype = dtype
  ELSE
    ! An integer of another size: HDF5 converts it to an 8-bit integer
    memtype = H5KIND_TO_TYPE(INT8, H5_INTEGER_KIND)
  END IF
END PROCEDURE BoolMemType

!----------------------------------------------------------------------------
!                                                                   BoolValue
!----------------------------------------------------------------------------

MODULE PROCEDURE BoolValue
  IF (isChar) THEN
    ans = INT(b, I4B) .EQ. ICHAR('T')
  ELSE
    ans = b .NE. 0_INT8
  END IF
END PROCEDURE BoolValue

!----------------------------------------------------------------------------
!                                                               Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ReadBool
