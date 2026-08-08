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

SUBMODULE(HDF5File_Class) ReadString
USE HDF5, ONLY: SIZE_T, H5DCLOSE_F, H5DGET_SPACE_F, H5DGET_TYPE_F, &
                H5DOPEN_F, H5DREAD_F, H5SCLOSE_F, &
                H5SGET_SIMPLE_EXTENT_NDIMS_F, &
                H5TGET_SIZE_F, H5T_NATIVE_CHARACTER
USE ISO_C_BINDING, ONLY: C_NULL_CHAR

IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "HDF5File_Class@ReadString.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_st0_helper
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_read_st0_helper()"
#endif
INTEGER(I4B), PARAMETER :: rank = 0
TYPE(String) :: path
INTEGER(I4B) :: ndims
INTEGER(HID_T) :: mem
INTEGER(HID_T) :: dspace_id, dset_id
INTEGER(SIZE_T) :: max_size
INTEGER(I4B) :: error

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

mem = -1
dspace_id = -1
dset_id = -1
path = dsetname

CALL H5DOPEN_F(obj%file_id, path%Chars(), dset_id, error)
CALL H5DGET_TYPE_F(dset_id, mem, error)
CALL H5TGET_SIZE_F(mem, max_size, error)
CALL H5DGET_SPACE_F(dset_id, dspace_id, error)
CALL H5SGET_SIMPLE_EXTENT_NDIMS_F(dspace_id, ndims, error)

IF ((ndims .EQ. rank + 1) .AND. (max_size .EQ. 1)) THEN
  CALL hdf5_read_ca0(obj, dsetname, vals)
ELSE
  CALL hdf5_read_st0(obj, dsetname, INT(max_size, I4B), vals)
END IF

CALL H5SCLOSE_F(dspace_id, error)
CALL H5DCLOSE_F(dset_id, error)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE hdf5_read_st0_helper

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_st0
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_read_st0()"
#endif
CHARACTER(LEN=length_max), ALLOCATABLE :: valsc(:)
TYPE(String) :: path
INTEGER(HSIZE_T), DIMENSION(1) :: dims
INTEGER(I4B), PARAMETER :: rank = 0
INTEGER(HID_T) :: mem
INTEGER(HID_T) :: dspace_id, dset_id
INTEGER(I4B) :: error

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! Allocate surrogate data
path = dsetname
CALL preRead(obj, path%Chars(), rank, dset_id, dspace_id, dims, error)

IF (error >= 0) THEN
  ALLOCATE (valsc(dims(1)))
  ! Read the dataset
  CALL H5DGET_TYPE_F(dset_id, mem, error)
  CALL H5DREAD_F(dset_id, mem, valsc, dims, error)

  IF (length_max == 1) THEN
    CALL convert_char_array_to_str(valsc, vals)
  ELSE
    vals = valsc(1) (1:length_max)
  END IF
  vals = vals%Replace(C_NULL_CHAR, '')
END IF

CALL postRead(obj, path%Chars(), dset_id, dspace_id, error)
IF (ALLOCATED(valsc)) DEALLOCATE (valsc)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE hdf5_read_st0

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_ca0
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_read_st0_helper()"
#endif
INTEGER(I4B), PARAMETER :: rank = 1
CHARACTER(LEN=1), ALLOCATABLE :: valsc(:)
INTEGER(I4B) :: i, error
TYPE(String) :: path
INTEGER(HSIZE_T), DIMENSION(1) :: dims
INTEGER(HID_T) :: mem
INTEGER(HID_T) :: dspace_id, dset_id

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

path = dsetname
CALL preRead(obj, path%Chars(), rank, dset_id, dspace_id, dims, error)

IF (error >= 0) THEN
  ALLOCATE (valsc(dims(1)))
  CALL H5DGET_TYPE_F(dset_id, mem, error)
  CALL H5DREAD_F(dset_id, mem, valsc, dims, error)
  ! Allocate space if needed, make sure it is the right size
  vals = ''
  ! Convert to StringType
  DO i = 1, SIZE(valsc)
    vals = vals//valsc(i)
  END DO
END IF

CALL postRead(obj, path%Chars(), dset_id, dspace_id, error)
IF (ALLOCATED(valsc)) DEALLOCATE (valsc)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE hdf5_read_ca0

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE convert_char_array_to_str(char_array, s)
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "convert_char_array_to_str()"
#endif
  CHARACTER(LEN=1), INTENT(IN) :: char_array(:)
  TYPE(String), INTENT(INOUT) :: s
  CHARACTER(LEN=SIZE(char_array)) :: c
  INTEGER(I4B) :: i

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  DO i = 1, LEN(c)
    c(i:i) = char_array(i)
  END DO
  s = c

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE convert_char_array_to_str

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_st1_helper
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_read_st1_helper()"
#endif
TYPE(String) :: path
INTEGER(I4B), PARAMETER :: rank = 1
INTEGER(I4B) :: ndims, error
INTEGER(HID_T) :: mem
INTEGER(HID_T) :: dspace_id, dset_id
INTEGER(SIZE_T) :: max_size

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

mem = -1
dspace_id = -1
dset_id = -1
path = dsetname
CALL H5DOPEN_F(obj%file_id, path%Chars(), dset_id, error)
CALL H5DGET_TYPE_F(dset_id, mem, error)
CALL H5TGET_SIZE_F(mem, max_size, error)
CALL H5DGET_SPACE_F(dset_id, dspace_id, error)
CALL H5SGET_SIMPLE_EXTENT_NDIMS_F(dspace_id, ndims, error)
IF ((ndims .EQ. rank + 1) .AND. (max_size .EQ. 1)) THEN
  CALL hdf5_read_ca1(obj, dsetname, vals)
ELSE
  CALL hdf5_read_st1(obj, dsetname, INT(max_size, I4B), vals)
END IF
CALL H5SCLOSE_F(dspace_id, error)
CALL H5DCLOSE_F(dset_id, error)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE hdf5_read_st1_helper

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_st1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_read_st1()"
#endif
CHARACTER(LEN=length_max), ALLOCATABLE :: valsc(:)
INTEGER(I4B) :: i, error
TYPE(String) :: path
INTEGER(HSIZE_T), DIMENSION(1) :: dims
INTEGER(I4B), PARAMETER :: rank = 1
INTEGER(HID_T) :: mem
INTEGER(HID_T) :: dspace_id, dset_id

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

path = dsetname
CALL preRead(obj, path%Chars(), rank, dset_id, dspace_id, dims, error)

IF (error >= 0) THEN
  ALLOCATE (valsc(dims(1)))
  CALL H5DGET_TYPE_F(dset_id, mem, error)
  CALL H5DREAD_F(dset_id, mem, valsc, dims, error)
  IF (ALLOCATED(vals)) THEN
    IF (SIZE(vals) .NE. dims(1)) THEN
      DEALLOCATE (vals)
      ALLOCATE (vals(dims(1)))
    END IF
  ELSE
    ALLOCATE (vals(dims(1)))
  END IF
  DO i = 1, SIZE(vals)
    vals(i) = valsc(i) (1:length_max)
  END DO

  !Find replace C_NULL_CHARs from HDF5
  DO i = 1, SIZE(vals)
    vals(i) = vals(i)%replace(C_NULL_CHAR, '')
  END DO
END IF

CALL postRead(obj, path%Chars(), dset_id, dspace_id, error)
IF (ALLOCATED(valsc)) DEALLOCATE (valsc)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE hdf5_read_st1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_ca1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_read_ca1()"
#endif
CHARACTER(LEN=1), ALLOCATABLE :: valsc(:, :)
INTEGER(I4B) :: i, j, error
TYPE(String) :: path
INTEGER(HSIZE_T), DIMENSION(2) :: dims
INTEGER(I4B), PARAMETER :: rank = 2
INTEGER(HID_T) :: mem
INTEGER(HID_T) :: dspace_id, dset_id

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

path = dsetname
! Allocate character array to size
CALL preRead(obj, path%Chars(), rank, dset_id, dspace_id, dims, error)

IF (error >= 0) THEN
  ALLOCATE (valsc(dims(1), dims(2)))
  CALL H5DGET_TYPE_F(dset_id, mem, error)
  CALL H5DREAD_F(dset_id, mem, valsc, dims, error)
  ! Allocate space if needed, make sure it is the right size
  IF (ALLOCATED(vals)) THEN
    IF (SIZE(vals) /= dims(2)) THEN
      DEALLOCATE (vals)
      ALLOCATE (vals(dims(2)))
    END IF
  ELSE
    ALLOCATE (vals(dims(2)))
  END IF
  ! Convert to StringType
  DO i = 1, SIZE(vals)
    vals(i) = ''
    DO j = 1, SIZE(valsc(:, i))
      vals(i) = vals(i)//valsc(j, i)
    END DO
  END DO
END IF
CALL postRead(obj, path%Chars(), dset_id, dspace_id, error)
IF (ALLOCATED(valsc)) DEALLOCATE (valsc)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE hdf5_read_ca1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_st2_helper
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_read_st2_helper()"
#endif
TYPE(String) :: path
INTEGER(I4B), PARAMETER :: rank = 2
INTEGER(I4B) :: ndims, error
INTEGER(HID_T) :: mem
INTEGER(HID_T) :: dspace_id, dset_id
INTEGER(SIZE_T) :: max_size

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

mem = -1
dspace_id = -1
dset_id = -1
path = dsetname
CALL H5DOPEN_F(obj%file_id, path%Chars(), dset_id, error)
CALL H5DGET_TYPE_F(dset_id, mem, error)
CALL H5TGET_SIZE_F(mem, max_size, error)
CALL H5DGET_SPACE_F(dset_id, dspace_id, error)
CALL H5SGET_SIMPLE_EXTENT_NDIMS_F(dspace_id, ndims, error)
IF ((ndims == rank + 1) .AND. (max_size == 1)) THEN
  CALL hdf5_read_ca2(obj, dsetname, vals)
ELSE
  CALL hdf5_read_st2(obj, dsetname, INT(max_size, I4B), vals)
END IF
CALL H5SCLOSE_F(dspace_id, error)
CALL H5DCLOSE_F(dset_id, error)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE hdf5_read_st2_helper

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_st2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_read_st2()"
#endif
CHARACTER(LEN=length_max), ALLOCATABLE :: valsc(:, :)
INTEGER(I4B) :: i, j, error
TYPE(String) :: path
INTEGER(HSIZE_T), DIMENSION(2) :: dims
INTEGER(I4B), PARAMETER :: rank = 2
INTEGER(HID_T) :: mem
INTEGER(HID_T) :: dspace_id, dset_id

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

path = dsetname
CALL preRead(obj, path%Chars(), rank, dset_id, dspace_id, dims, error)

IF (error >= 0) THEN
  ALLOCATE (valsc(dims(1), dims(2)))
  CALL H5DGET_TYPE_F(dset_id, mem, error)
  CALL H5DREAD_F(dset_id, mem, valsc, dims, error)
  IF (ALLOCATED(vals)) THEN
    IF (ALL(SHAPE(vals) /= (/dims(1), dims(2)/))) THEN
      DEALLOCATE (vals)
      ALLOCATE (vals(dims(1), dims(2)))
    END IF
  ELSE
    ALLOCATE (vals(dims(1), dims(2)))
  END IF
  DO i = 1, SIZE(vals, 1)
    DO j = 1, SIZE(vals, 2)
      vals(i, j) = valsc(i, j) (1:length_max)
    END DO
  END DO
  DO j = 1, SIZE(vals, DIM=2)
    DO i = 1, SIZE(vals, DIM=1)
      vals(i, j) = vals(i, j)%replace(C_NULL_CHAR, '')
    END DO
  END DO
END IF

CALL postRead(obj, path%Chars(), dset_id, dspace_id, error)
IF (ALLOCATED(valsc)) DEALLOCATE (valsc)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE hdf5_read_st2

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_ca2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_read_ca2()"
#endif
CHARACTER(LEN=1), ALLOCATABLE :: valsc(:, :, :)
INTEGER(I4B) :: i, j, k, error
TYPE(String) :: path
INTEGER(HSIZE_T), DIMENSION(3) :: dims
INTEGER(I4B), PARAMETER :: rank = 3
INTEGER(HID_T) :: mem
INTEGER(HID_T) :: dspace_id, dset_id

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

path = dsetname
! Allocate character array to size
CALL preRead(obj, path%Chars(), rank, dset_id, dspace_id, dims, error)

IF (error >= 0) THEN
  ALLOCATE (valsc(dims(1), dims(2), dims(3)))
  CALL H5DGET_TYPE_F(dset_id, mem, error)
  CALL H5DREAD_F(dset_id, mem, valsc, dims, error)
  ! Allocate space if needed, make sure it is the right size
  IF (ALLOCATED(vals)) THEN
    IF (ALL(SHAPE(vals) /= (/dims(2), dims(3)/))) THEN
      DEALLOCATE (vals)
      ALLOCATE (vals(dims(2), dims(3)))
    END IF
  ELSE
    ALLOCATE (vals(dims(2), dims(3)))
  END IF
  ! Convert to StringType
  DO i = 1, SIZE(vals, 1)
    DO j = 1, SIZE(vals, 2)
      vals(i, j) = ''
      DO k = 1, SIZE(valsc(:, i, j))
        vals(i, j) = vals(i, j)//valsc(k, i, j)
      END DO
    END DO
  END DO
END IF
CALL postRead(obj, path%Chars(), dset_id, dspace_id, error)
IF (ALLOCATED(valsc)) DEALLOCATE (valsc)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE hdf5_read_ca2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_st3_helper
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_read_st3_helper()"
#endif
TYPE(String) :: path
INTEGER(I4B), PARAMETER :: rank = 3
INTEGER(I4B) :: ndims, error
INTEGER(HID_T) :: mem
INTEGER(HID_T) :: dspace_id, dset_id
INTEGER(SIZE_T) :: max_size

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

mem = -1
dspace_id = -1
dset_id = -1
path = dsetname
CALL H5DOPEN_F(obj%file_id, path%Chars(), dset_id, error)
CALL H5DGET_TYPE_F(dset_id, mem, error)
CALL H5TGET_SIZE_F(mem, max_size, error)
CALL H5DGET_SPACE_F(dset_id, dspace_id, error)
CALL H5SGET_SIMPLE_EXTENT_NDIMS_F(dspace_id, ndims, error)
IF ((ndims == rank + 1) .AND. (max_size == 1)) THEN
  CALL hdf5_read_ca3(obj, dsetname, vals)
ELSE
  CALL hdf5_read_st3(obj, dsetname, INT(max_size, I4B), vals)
END IF
CALL H5SCLOSE_F(dspace_id, error)
CALL H5DCLOSE_F(dset_id, error)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE hdf5_read_st3_helper

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_st3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_read_st3()"
#endif
CHARACTER(LEN=length_max), ALLOCATABLE :: valsc(:, :, :)
INTEGER(I4B) :: i, j, k, error
TYPE(String) :: path
INTEGER(HSIZE_T), DIMENSION(3) :: dims
INTEGER(I4B), PARAMETER :: rank = 3
INTEGER(HID_T) :: mem
INTEGER(HID_T) :: dspace_id, dset_id

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

path = dsetname
! Allocate character array to size
CALL preRead(obj, path%Chars(), rank, dset_id, dspace_id, dims, error)
IF (error >= 0) THEN
  ALLOCATE (valsc(dims(1), dims(2), dims(3)))
  CALL H5DGET_TYPE_F(dset_id, mem, error)
  CALL H5DREAD_F(dset_id, mem, valsc, dims, error)
  IF (ALLOCATED(vals)) THEN
    IF (ALL(SHAPE(vals) /= (/dims(1), dims(2), dims(3)/))) THEN
      DEALLOCATE (vals)
      ALLOCATE (vals(dims(1), dims(2), dims(3)))
    END IF
  ELSE
    ALLOCATE (vals(dims(1), dims(2), dims(3)))
  END IF
  DO i = 1, SIZE(vals, 1)
    DO j = 1, SIZE(vals, 2)
      DO k = 1, SIZE(vals, 3)
        vals(i, j, k) = valsc(i, j, k) (1:length_max)
      END DO
    END DO
  END DO
  !Find replace C_NULL_CHARs from HDF5
  DO k = 1, SIZE(vals, DIM=3)
    DO j = 1, SIZE(vals, DIM=2)
      DO i = 1, SIZE(vals, DIM=1)
        vals(i, j, k) = vals(i, j, k)%replace(C_NULL_CHAR, '')
      END DO
    END DO
  END DO
END IF
CALL postRead(obj, path%Chars(), dset_id, dspace_id, error)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE hdf5_read_st3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_ca3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_read_ca3()"
#endif
CHARACTER(LEN=1), ALLOCATABLE :: valsc(:, :, :, :)
INTEGER(I4B) :: i, j, k, m, error
TYPE(String) :: path
INTEGER(HSIZE_T), DIMENSION(4) :: dims
INTEGER(I4B), PARAMETER :: rank = 4
INTEGER(HID_T) :: mem
INTEGER(HID_T) :: dspace_id, dset_id

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

path = dsetname
! Allocate character array to size
CALL preRead(obj, path%Chars(), rank, dset_id, dspace_id, dims, error)

IF (error >= 0) THEN
  ALLOCATE (valsc(dims(1), dims(2), dims(3), dims(4)))
  CALL H5DGET_TYPE_F(dset_id, mem, error)
  CALL H5DREAD_F(dset_id, mem, valsc, dims, error)
  ! Allocate space if needed, make sure it is the right size
  IF (ALLOCATED(vals)) THEN
    IF (ALL(SHAPE(vals) /= (/dims(2), dims(3), dims(4)/))) THEN
      DEALLOCATE (vals)
      ALLOCATE (vals(dims(2), dims(3), dims(4)))
    END IF
  ELSE
    ALLOCATE (vals(dims(2), dims(3), dims(4)))
  END IF
  ! Convert to StringType
  DO i = 1, SIZE(vals, 1)
    DO j = 1, SIZE(vals, 2)
      DO m = 1, SIZE(vals, 3)
        vals(i, j, m) = ''
        DO k = 1, SIZE(valsc(:, i, j, m))
          vals(i, j, m) = vals(i, j, m)//valsc(k, i, j, m)
        END DO
      END DO
    END DO
  END DO
END IF
CALL postRead(obj, path%Chars(), dset_id, dspace_id, error)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE hdf5_read_ca3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_c1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_read_c1()"
#endif
CHARACTER, ALLOCATABLE :: valsc(:)
TYPE(String) :: path
INTEGER(I4B) :: i, error
INTEGER(HSIZE_T), DIMENSION(1) :: dims
INTEGER(I4B), PARAMETER :: rank = 1
INTEGER(HID_T) :: mem
INTEGER(HID_T) :: dspace_id, dset_id

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

path = dsetname
! Allocate surrogate data
CALL preRead(obj, path%Chars(), rank, dset_id, dspace_id, dims, error)
IF (error >= 0) THEN
  ALLOCATE (valsc(dims(1)))
  ! Read the dataset
  mem = H5T_NATIVE_CHARACTER
  CALL H5DREAD_F(dset_id, mem, valsc, dims, error)

  ! Convert from surrogate character array to boolean array
  vals(:) = " "
  DO i = 1, SIZE(valsc)
    vals(i:i) = valsc(i)
  END DO
END IF
CALL postRead(obj, path%Chars(), dset_id, dspace_id, error)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE hdf5_read_c1

!----------------------------------------------------------------------------
!                                                             Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ReadString
