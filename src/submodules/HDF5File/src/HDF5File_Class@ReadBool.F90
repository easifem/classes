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

MODULE PROCEDURE hdf5_read_b0
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_read_b0()"
#endif
CHARACTER(1) :: valsc
CHARACTER(LEN(dsetname) + 1) :: path
INTEGER(HSIZE_T), DIMENSION(1) :: dims
INTEGER(I4B), PARAMETER :: rank = 0
INTEGER(I4B) :: error
INTEGER(HID_T) :: mem, dspace_id, dset_id

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
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
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE hdf5_read_b0

!----------------------------------------------------------------------------
!                                                                      Read
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_b1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_read_b1()"
#endif
CHARACTER, ALLOCATABLE :: valsc(:)
CHARACTER(LEN(dsetname) + 1) :: path
INTEGER(I4B) :: i, error
INTEGER(HSIZE_T), DIMENSION(1) :: dims
INTEGER(I4B), PARAMETER :: rank = 1
INTEGER(HID_T) :: mem, dspace_id, dset_id

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

path = dsetname
! Allocate space in data if needed, make sure it is the right size
CALL preRead(obj, path, rank, dset_id, dspace_id, dims, error)

IF (error .GE. 0) THEN
  IF (ALLOCATED(vals)) THEN
    IF (ANY(SHAPE(vals) /= dims)) THEN
      DEALLOCATE (vals)
      ALLOCATE (vals(dims(1)))
    END IF
  ELSE
    ALLOCATE (vals(dims(1)))
  END IF
  ALLOCATE (valsc(dims(1)))
  mem = H5T_NATIVE_CHARACTER
  CALL H5DREAD_F(dset_id, mem, valsc, dims, error)
  vals = .FALSE.
  DO CONCURRENT(i=1:SIZE(vals), valsc(i) == 'T')
    vals(i) = .TRUE.
  END DO
  DEALLOCATE (valsc)
END IF

CALL postRead(obj, path, dset_id, dspace_id, error)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE hdf5_read_b1

!----------------------------------------------------------------------------
!                                                                      Read
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_b2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_read_b2()"
#endif
CHARACTER(1), ALLOCATABLE :: valsc(:, :)
CHARACTER(LEN(dsetname) + 1) :: path
INTEGER(I4B) :: i, j, error
INTEGER(HSIZE_T), DIMENSION(2) :: dims
INTEGER(I4B), PARAMETER :: rank = 2
INTEGER(HID_T) :: mem, dspace_id, dset_id

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

path = dsetname
CALL preRead(obj, path, rank, dset_id, dspace_id, dims, error)

IF (error >= 0) THEN
  IF (ALLOCATED(vals)) THEN
    IF (ANY(SHAPE(vals) /= dims)) THEN
      DEALLOCATE (vals)
      ALLOCATE (vals(dims(1), dims(2)))
    END IF
  ELSE
    ALLOCATE (vals(dims(1), dims(2)))
  END IF
  ALLOCATE (valsc(dims(1), dims(2)))

  ! Read the dataset
  mem = H5T_NATIVE_CHARACTER
  CALL H5DREAD_F(dset_id, mem, valsc, dims, error)
  ! Convert from surrogate character array to boolean array
  vals = .FALSE.
  DO CONCURRENT(i=1:SIZE(vals, DIM=1), j=1:SIZE(vals, DIM=2), &
                valsc(i, j) == 'T')
    vals(i, j) = .TRUE.
  END DO

  DEALLOCATE (valsc)
END IF
CALL postRead(obj, path, dset_id, dspace_id, error)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE hdf5_read_b2

!----------------------------------------------------------------------------
!                                                                 Read
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_b3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_read_b3()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE hdf5_read_b3

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ReadBool
