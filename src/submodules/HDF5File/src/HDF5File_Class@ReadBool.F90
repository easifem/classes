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
USE HDF5, ONLY: H5T_NATIVE_CHARACTER, H5DREAD_F
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "HDF5File_Class@ReadBool.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                      Read
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Read_b0
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Read_b0()"
#endif
  CHARACTER(1) :: valsc
  CHARACTER(LEN(dsetname) + 1) :: path
  INTEGER(HSIZE_T), DIMENSION(1) :: dims
  INTEGER(I4B), PARAMETER :: rank = 0
  INTEGER(I4B) :: error
  INTEGER(HID_T) :: mem, dspace_id, dset_id

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[START] ')
#endif

  path = dsetname
! Read the dataset
  mem = H5T_NATIVE_CHARACTER
  CALL preRead(obj, path, rank, dset_id, dspace_id, dims, error)
  IF (error .GE. 0) THEN
    CALL H5DREAD_F(dset_id, mem, valsc, dims, error)
    ! Convert to logical from character
    IF (valsc .EQ. 'F') THEN
      vals = .FALSE.
    ELSE
      vals = .TRUE.
    END IF
  END IF
  CALL postRead(obj, path, dset_id, dspace_id, error)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[END] ')
#endif
END PROCEDURE obj_Read_b0

!----------------------------------------------------------------------------
!                                                                      Read
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Read_b1
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Read_b1()"
#endif
  CHARACTER, ALLOCATABLE :: valsc(:)
  CHARACTER(LEN(dsetname) + 1) :: path
  INTEGER(I4B) :: i, error
  INTEGER(HSIZE_T), DIMENSION(1) :: dims
  INTEGER(I4B), PARAMETER :: rank = 1
  INTEGER(HID_T) :: mem, dspace_id, dset_id

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[START] ')
#endif

  path = dsetname
! Allocate space in data if needed, make sure it is the right size
  CALL preRead(obj, path, rank, dset_id, dspace_id, dims, error)

  IF (error .GE. 0) THEN
    IF (ALLOCATED(vals)) THEN
      IF (ANY(SHAPE(vals) /= dims)) THEN
        DEALLOCATE(vals)
        ALLOCATE(vals(dims(1)))
      END IF
    ELSE
      ALLOCATE(vals(dims(1)))
    END IF
    ALLOCATE(valsc(dims(1)))
    mem = H5T_NATIVE_CHARACTER
    CALL H5DREAD_F(dset_id, mem, valsc, dims, error)
    vals = .FALSE.
    DO CONCURRENT(i=1:SIZE(vals), valsc(i) == 'T')
      vals(i) = .TRUE.
    END DO
    DEALLOCATE(valsc)
  END IF

  CALL postRead(obj, path, dset_id, dspace_id, error)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[END] ')
#endif
END PROCEDURE obj_Read_b1

!----------------------------------------------------------------------------
!                                                                      Read_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Read__b1
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Read__b1()"
#endif
  CHARACTER(1), ALLOCATABLE :: valsc(:)
  CHARACTER(LEN(dsetname) + 1) :: path
  INTEGER(I4B), PARAMETER :: rank = 1
  INTEGER(HSIZE_T), DIMENSION(rank) :: dims
  INTEGER(HID_T) :: mem, dspace_id, dset_id
  INTEGER(I4B) :: ii, error

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[START] ')
#endif

  tsize = math%zero_i

  path = dsetname
  CALL preRead(obj, path, rank, dset_id, dspace_id, dims, error)
  IF (error .GE. 0) THEN

#ifdef DEBUG_VER
    CALL AssertError3(INT(dims(1), I4B), SIZE(vals), myName, &
      'a=dims(1) from hdf5File, b=SIZE(vals)')
#endif

    tsize = INT(dims(1), I4B)
    ALLOCATE(valsc(tsize))
    mem = H5T_NATIVE_CHARACTER
    CALL H5DREAD_F(dset_id, mem, valsc, dims, error)
    ! Convert from surrogate character array to boolean array
    DO ii = 1, tsize
      vals(ii) = valsc(ii) .EQ. 'T'
    END DO
    DEALLOCATE(valsc)
  END IF
  CALL postRead(obj, path, dset_id, dspace_id, error)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[END] ')
#endif
END PROCEDURE obj_Read__b1

!----------------------------------------------------------------------------
!                                                                      Read
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Read_b2
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Read_b2()"
#endif
  CHARACTER(1), ALLOCATABLE :: valsc(:, :)
  CHARACTER(LEN(dsetname) + 1) :: path
  INTEGER(I4B) :: i, j, error
  INTEGER(HSIZE_T), DIMENSION(2) :: dims
  INTEGER(I4B), PARAMETER :: rank = 2
  INTEGER(HID_T) :: mem, dspace_id, dset_id

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[START] ')
#endif

  path = dsetname
  CALL preRead(obj, path, rank, dset_id, dspace_id, dims, error)

  IF (error >= 0) THEN
    IF (ALLOCATED(vals)) THEN
      IF (ANY(SHAPE(vals) /= dims)) THEN
        DEALLOCATE(vals)
        ALLOCATE(vals(dims(1), dims(2)))
      END IF
    ELSE
      ALLOCATE(vals(dims(1), dims(2)))
    END IF
    ALLOCATE(valsc(dims(1), dims(2)))

    ! Read the dataset
    mem = H5T_NATIVE_CHARACTER
    CALL H5DREAD_F(dset_id, mem, valsc, dims, error)
    ! Convert from surrogate character array to boolean array
    vals = .FALSE.
    DO CONCURRENT(i=1:SIZE(vals, DIM=1), j=1:SIZE(vals, DIM=2), &
      valsc(i, j) == 'T')
      vals(i, j) = .TRUE.
    END DO

    DEALLOCATE(valsc)
  END IF
  CALL postRead(obj, path, dset_id, dspace_id, error)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[END] ')
#endif
END PROCEDURE obj_Read_b2

!----------------------------------------------------------------------------
!                                                                      Read_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Read__b2
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Read__b2()"
#endif
  CHARACTER(1), ALLOCATABLE :: valsc(:, :)
  CHARACTER(LEN(dsetname) + 1) :: path
  INTEGER(I4B), PARAMETER :: rank = 2
  INTEGER(HSIZE_T), DIMENSION(rank) :: dims
  INTEGER(HID_T) :: mem, dspace_id, dset_id
  INTEGER(I4B) :: ii, jj, error

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
    ALLOCATE(valsc(nrow, ncol))
    mem = H5T_NATIVE_CHARACTER
    CALL H5DREAD_F(dset_id, mem, valsc, dims, error)
    ! Convert from surrogate character array to boolean array
    DO jj = 1, ncol
      DO ii = 1, nrow
        vals(ii, jj) = valsc(ii, jj) .EQ. 'T'
      END DO
    END DO
    DEALLOCATE(valsc)
  END IF
  CALL postRead(obj, path, dset_id, dspace_id, error)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[END] ')
#endif
END PROCEDURE obj_Read__b2

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Read_b3
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Read_b3()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[START] ')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseError(modName // '::' // myName // ' - ' // &
    '[WIP ERROR] :: This routine is under development')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[END] ')
#endif
END PROCEDURE obj_Read_b3

!----------------------------------------------------------------------------
!                                                                      Read_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Read__b3
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Read__b3()"
#endif
  CHARACTER(1), ALLOCATABLE :: valsc(:, :, :)
  CHARACTER(LEN(dsetname) + 1) :: path
  INTEGER(I4B), PARAMETER :: rank = 3
  INTEGER(HSIZE_T), DIMENSION(rank) :: dims
  INTEGER(HID_T) :: mem, dspace_id, dset_id
  INTEGER(I4B) :: ii, jj, kk, error

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
    ALLOCATE(valsc(dim1, dim2, dim3))
    mem = H5T_NATIVE_CHARACTER
    CALL H5DREAD_F(dset_id, mem, valsc, dims, error)
    ! Convert from surrogate character array to boolean array
    DO kk = 1, dim3
      DO jj = 1, dim2
        DO ii = 1, dim1
          vals(ii, jj, kk) = valsc(ii, jj, kk) .EQ. 'T'
        END DO
      END DO
    END DO
    DEALLOCATE(valsc)
  END IF
  CALL postRead(obj, path, dset_id, dspace_id, error)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
    '[END] ')
#endif
END PROCEDURE obj_Read__b3

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ReadBool
