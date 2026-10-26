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

SUBMODULE(HDF5File_Class) WriteString
USE HDF5, ONLY: H5DWRITE_F, H5TCLOSE_F, H5TCOPY_F, H5TSET_SIZE_F, &
  H5TSET_STRPAD_F, H5T_NATIVE_CHARACTER
USE HDF5, ONLY: SIZE_T
USE ISO_C_BINDING, ONLY: C_NULL_CHAR
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "HDF5File_Class@WriteString.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                     Write
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Write_st0
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Write_st0()"
#endif
  CHARACTER(:), ALLOCATABLE :: valss
  TYPE(String) :: path
  INTEGER(HSIZE_T), DIMENSION(1) :: ldims, gdims, offset, cnt
  INTEGER(I4B), PARAMETER :: rank = 0
  INTEGER(HID_T) :: mem, dspace_id, dset_id, gspace_id, plist_id
  INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  path = dsetname
  valss = vals%chars()
  ! A type of size 0 is not allowed: an empty String is written as one NUL
  ! character, which Read removes
  IF (vals%LEN_TRIM() .EQ. 0) valss = C_NULL_CHAR
! stash offset
  offset(1) = 0
  IF (PRESENT(offset_in)) offset = offset_in

! Determine the dimensions for the dataspace
  ldims = 1

! Store the dimensions from global if present
  IF (PRESENT(gdims_in)) THEN
    gdims = gdims_in
  ELSE
    gdims = ldims
  END IF
  cnt = gdims
  IF (PRESENT(cnt_in)) cnt = cnt_in

  CALL H5TCOPY_F(H5T_NATIVE_CHARACTER, mem, ierr)
  CALL H5TSET_STRPAD_F(mem, 0, ierr)
  CALL H5TSET_SIZE_F(mem, INT(MAX(vals%LEN_TRIM(), 1), SIZE_T), ierr)
  CALL preWrite(obj, rank, gdims, ldims, path%Chars(), mem, &
                dset_id, dspace_id, gspace_id, plist_id, ierr, &
                cnt, offset)
  IF (ierr == 0) CALL H5DWRITE_F(dset_id, mem, valss, gdims, ierr, &
                                 dspace_id, gspace_id, plist_id)
  CALL postWrite(obj, ierr, dset_id, dspace_id, gspace_id, plist_id)
  CALL H5TCLOSE_F(mem, ierr)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_Write_st0

!----------------------------------------------------------------------------
!                                                                     Write
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Write_st1
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Write_st1()"
#endif
  CHARACTER(LEN=:), ALLOCATABLE :: valss(:)
  TYPE(String) :: path
  INTEGER(I4B) :: j, ierr
  INTEGER(HSIZE_T), DIMENSION(1) :: ldims, gdims, offset, cnt
  INTEGER(I4B), PARAMETER :: rank = 1
  INTEGER(HID_T) :: mem, dspace_id, dset_id, gspace_id, plist_id

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  path = dsetname
! Fill character array (allocatable, so it is not on the stack)
  ALLOCATE(CHARACTER(LEN=MAX(length_max, 1)) :: valss(SIZE(vals)))
  DO j = 1, SIZE(vals, DIM=1)
    valss(j) = vals(j)%chars()
  END DO

! stash offset
  offset(1) = 0
  IF (PRESENT(offset_in)) offset = offset_in

! Determine the dimensions for the dataspace
  ldims = SHAPE(vals)

! Store the dimensions from global if present
  IF (PRESENT(gdims_in)) THEN
    gdims = gdims_in
  ELSE
    gdims = ldims
  END IF
  cnt = gdims
  IF (PRESENT(cnt_in)) cnt = cnt_in

  CALL H5TCOPY_F(H5T_NATIVE_CHARACTER, mem, ierr)
  CALL H5TSET_STRPAD_F(mem, 0, ierr)
  ! The string length is at least 1 (a type of size 0 is not allowed)
  CALL H5TSET_SIZE_F(mem, INT(MAX(length_max, 1), SIZE_T), ierr)
  CALL preWrite(obj, rank, gdims, ldims, path%Chars(), mem, dset_id, &
                dspace_id, gspace_id, plist_id, ierr, cnt, offset)
  IF (ierr == 0) CALL H5DWRITE_F(dset_id, mem, valss, gdims, ierr, &
                                 dspace_id, gspace_id, plist_id)

  CALL postWrite(obj, ierr, dset_id, dspace_id, gspace_id, plist_id)
  CALL H5TCLOSE_F(mem, ierr)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_Write_st1

!----------------------------------------------------------------------------
!                                                                     Write
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Write_st1_helper
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Write_st1_helper()"
#endif
  INTEGER(I4B) :: length_max, i, local_gdims(1)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  length_max = 0
  DO i = 1, SIZE(vals)
    length_max = MAX(vals(i)%LEN_TRIM(), length_max)
  END DO
  local_gdims(1:) = SHAPE(vals)
  IF (PRESENT(gdims_in)) local_gdims(1) = gdims_in(1)
  CALL obj_Write_st1(obj, dsetname, vals, length_max, local_gdims)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_Write_st1_helper

!----------------------------------------------------------------------------
!                                                                     Write
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Write_st2_helper
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Write_st2_helper()"
#endif
  INTEGER(I4B) :: length_max, i, j, local_gdims(1)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  length_max = 0
  DO j = 1, SIZE(vals, 1)
    DO i = 1, SIZE(vals, 2)
      length_max = MAX(vals(j, i)%LEN_TRIM(), length_max)
    END DO
  END DO

  IF (PRESENT(gdims_in)) THEN
    CALL obj_Write_st2(obj, dsetname, vals, length_max, gdims_in)
  ELSE
    CALL obj_Write_st2(obj, dsetname, vals, length_max)
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_Write_st2_helper

!----------------------------------------------------------------------------
!                                                                 Write
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Write_st2
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Write_st2()"
#endif
  CHARACTER(LEN=:), ALLOCATABLE :: valss(:, :)
  TYPE(String) :: path
  INTEGER(I4B) :: j, k, ierr
  INTEGER(HSIZE_T), DIMENSION(2) :: gdims, ldims, offset, cnt
  INTEGER(I4B), PARAMETER :: rank = 2
  INTEGER(HID_T) :: mem, dspace_id, dset_id, gspace_id, plist_id

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  path = dsetname
  ! Allocatable, so the copy is not on the stack
  ALLOCATE(CHARACTER(LEN=MAX(length_max, 1)) :: &
           valss(SIZE(vals, 1), SIZE(vals, 2)))
  DO k = 1, SIZE(vals, 2)
    DO j = 1, SIZE(vals, 1)
      valss(j, k) = vals(j, k)%chars()
    END DO
  END DO

! stash offset
  offset(1) = LBOUND(vals, 1) - 1
  offset(2) = LBOUND(vals, 2) - 1
  IF (PRESENT(offset_in)) offset = offset_in

! Determine the dimensions for the dataspace
  ldims = SHAPE(vals)

! Store the dimensions from global if present
  IF (PRESENT(gdims_in)) THEN
    gdims = gdims_in
  ELSE
    gdims = ldims
  END IF
  cnt = gdims
  IF (PRESENT(cnt_in)) cnt = cnt_in

  CALL h5tcopy_f(H5T_NATIVE_CHARACTER, mem, ierr)
  CALL h5tset_strpad_f(mem, 0, ierr)
  ! The string length is at least 1 (a type of size 0 is not allowed)
  CALL h5tset_size_f(mem, INT(MAX(length_max, 1), SIZE_T), ierr)
  CALL preWrite(obj, rank, gdims, ldims, path%Chars(), mem, dset_id, &
                dspace_id, gspace_id, plist_id, ierr, cnt, offset)
  IF (ierr == 0) CALL H5DWRITE_F(dset_id, mem, valss, gdims, ierr, &
                                 dspace_id, gspace_id, plist_id)

  CALL postWrite(obj, ierr, dset_id, dspace_id, gspace_id, plist_id)
  CALL H5TCLOSE_F(mem, ierr)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_Write_st2

!----------------------------------------------------------------------------
!                                                                     Write
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Write_st3_helper
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Write_st3_helper()"
#endif
  INTEGER(I4B) :: length_max, i, j, k

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  length_max = 0
  DO k = 1, SIZE(vals, 3)
    DO j = 1, SIZE(vals, 2)
      DO i = 1, SIZE(vals, 1)
        length_max = MAX(vals(i, j, k)%LEN_TRIM(), length_max)
      END DO
    END DO
  END DO

  IF (PRESENT(gdims_in)) THEN
    CALL obj_Write_st3(obj, dsetname, vals, length_max, gdims_in)
  ELSE
    CALL obj_Write_st3(obj, dsetname, vals, length_max)
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif

END PROCEDURE obj_Write_st3_helper

!----------------------------------------------------------------------------
!                                                                 Write
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Write_st3
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Write_st3()"
#endif
  CHARACTER(LEN=:), ALLOCATABLE :: valss(:, :, :)
  TYPE(String) :: path
  INTEGER(I4B) :: i, j, k, ierr
  INTEGER(HSIZE_T), DIMENSION(3) :: gdims, ldims, offset, cnt
  INTEGER(I4B), PARAMETER :: rank = 3
  INTEGER(HID_T) :: mem, dspace_id, dset_id, gspace_id, plist_id

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  path = dsetname
  ! Allocatable, so the copy is not on the stack
  ALLOCATE(CHARACTER(LEN=MAX(length_max, 1)) :: &
           valss(SIZE(vals, 1), SIZE(vals, 2), SIZE(vals, 3)))
  DO i = 1, SIZE(vals, 3)
    DO k = 1, SIZE(vals, 2)
      DO j = 1, SIZE(vals, 1)
        valss(j, k, i) = vals(j, k, i)%chars()
      END DO
    END DO
  END DO

! stash offset
  offset(1) = LBOUND(vals, 1) - 1
  offset(2) = LBOUND(vals, 2) - 1
  offset(3) = LBOUND(vals, 3) - 1
  IF (PRESENT(offset_in)) offset = offset_in

! Determine the dimensions for the dataspace
  ldims = SHAPE(vals)

! Store the dimensions from global if present
  IF (PRESENT(gdims_in)) THEN
    gdims = gdims_in
  ELSE
    gdims = ldims
  END IF
  cnt = gdims
  IF (PRESENT(cnt_in)) cnt = cnt_in
  CALL H5TCOPY_F(H5T_NATIVE_CHARACTER, mem, ierr)
  CALL H5TSET_STRPAD_F(mem, 0, ierr)
  ! The string length is at least 1 (a type of size 0 is not allowed)
  CALL H5TSET_SIZE_F(mem, INT(MAX(length_max, 1), SIZE_T), ierr)

  CALL preWrite(obj, rank, gdims, ldims, path%Chars(), mem, dset_id, &
                dspace_id, gspace_id, plist_id, ierr, cnt, offset)

  IF (ierr == 0) CALL H5DWRITE_F(dset_id, mem, valss, gdims, ierr, &
                                 dspace_id, gspace_id, plist_id)

  CALL postWrite(obj, ierr, dset_id, dspace_id, gspace_id, plist_id)
  CALL H5TCLOSE_F(mem, ierr)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_Write_st3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Write_c1
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Write_c1()"
#endif
  TYPE(String) :: path
  INTEGER(HSIZE_T), DIMENSION(1) :: ldims, offset, gdims, cnt
  INTEGER(I4B), PARAMETER :: rank = 1
  INTEGER(HID_T) :: mem, dspace_id, dset_id, gspace_id, plist_id
  INTEGER(I4B) :: error

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

! stash offset
  offset(1) = 1
  IF (PRESENT(offset_in)) offset = offset_in
  path = dsetname
! Determine the dimensions for the dataspace
  ldims(1) = LEN(vals)
! Store the dimensions from global if present
  IF (PRESENT(gdims_in)) THEN
    gdims(1) = gdims_in
  ELSE
    gdims(1) = ldims(1)
  END IF
  cnt = gdims
  IF (PRESENT(cnt_in)) cnt = cnt_in
  mem = H5T_NATIVE_CHARACTER
  CALL preWrite(obj, rank, gdims, ldims, path%Chars(), mem, dset_id, &
                dspace_id, gspace_id, plist_id, error, cnt, offset)

  IF (error == 0) CALL H5DWRITE_F(dset_id, mem, vals, gdims, error, &
                                  dspace_id, gspace_id, plist_id)
  CALL postWrite(obj, error, dset_id, dspace_id, gspace_id, plist_id)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_Write_c1

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE WriteString
