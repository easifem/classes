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

SUBMODULE(HDF5File_Class) ReadReal32
USE HDF5, ONLY: H5T_NATIVE_REAL, H5DREAD_F
USE ReallocateUtility, ONLY: Reallocate
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "HDF5File_Class@ReadReal32.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                      Read
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_s0
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_read_s0()"
#endif
TYPE(String) :: path
INTEGER(HSIZE_T), DIMENSION(1) :: dims
INTEGER(I4B), PARAMETER :: rank = 0
INTEGER(HID_T) :: mem
INTEGER(HID_T) :: dspace_id, dset_id
INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! Read the dataset
path = dsetname
CALL preRead(obj, path%Chars(), rank, dset_id, dspace_id, dims, ierr)
mem = H5T_NATIVE_REAL
IF (ierr >= 0) &
  CALL H5DREAD_F(dset_id, mem, vals, dims, ierr)
CALL postRead(obj, path%Chars(), dset_id, dspace_id, ierr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE hdf5_read_s0

!----------------------------------------------------------------------------
!                                                                       Read
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_s1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_read_s1()"
#endif
INTEGER(I4B), PARAMETER :: rank = 1
TYPE(String) :: path
INTEGER(HSIZE_T), DIMENSION(rank) :: dims
INTEGER(HID_T) :: mem
INTEGER(HID_T) :: dspace_id, dset_id
INTEGER(I4B) :: error

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! Allocate space if needed, make sure it is the right size
path = dsetname
CALL preRead(obj, path%Chars(), rank, dset_id, dspace_id, dims, error)
IF (error >= 0) THEN
  CALL Reallocate(vals, INT(dims, I4B))
  mem = H5T_NATIVE_REAL
  CALL H5DREAD_F(dset_id, mem, vals, dims, error)
END IF
CALL postRead(obj, path%Chars(), dset_id, dspace_id, error)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE hdf5_read_s1

!----------------------------------------------------------------------------
!                                                                       Read
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_s2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_read_s2()"
#endif
INTEGER(I4B), PARAMETER :: rank = 2
TYPE(String) :: path
INTEGER(HSIZE_T), DIMENSION(rank) :: dims
INTEGER(HID_T) :: mem
INTEGER(HID_T) :: dspace_id, dset_id
INTEGER(I4B) :: error

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

path = dsetname
CALL preRead(obj, path%Chars(), rank, dset_id, dspace_id, dims, error)
IF (error >= 0) THEN
  CALL Reallocate(vals, INT(dims, I4B))
  mem = H5T_NATIVE_REAL
  CALL H5DREAD_F(dset_id, mem, vals, dims, error)
END IF
CALL postRead(obj, path%Chars(), dset_id, dspace_id, error)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE hdf5_read_s2

!----------------------------------------------------------------------------
!                                                                      Read
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_s3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_read_s3()"
#endif
INTEGER(I4B), PARAMETER :: rank = 3
TYPE(String) :: path
INTEGER(HSIZE_T), DIMENSION(rank) :: dims
INTEGER(HID_T) :: mem
INTEGER(HID_T) :: dspace_id, dset_id
INTEGER(I4B) :: error

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

path = dsetname
CALL preRead(obj, path%Chars(), rank, dset_id, dspace_id, dims, error)
IF (error >= 0) THEN
  CALL Reallocate(vals, INT(dims, I4B))
  mem = H5T_NATIVE_REAL
  CALL H5DREAD_F(dset_id, mem, vals, dims, error)
END IF
CALL postRead(obj, path%Chars(), dset_id, dspace_id, error)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE hdf5_read_s3

!----------------------------------------------------------------------------
!                                                                       Read
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_s4
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_read_s4()"
#endif
INTEGER(I4B), PARAMETER :: rank = 4
TYPE(String) :: path
INTEGER(HSIZE_T), DIMENSION(rank) :: dims
INTEGER(HID_T) :: mem
INTEGER(HID_T) :: dspace_id, dset_id
INTEGER(I4B) :: error

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

path = dsetname
CALL preRead(obj, path%Chars(), rank, dset_id, dspace_id, dims, error)
IF (error >= 0) THEN
  CALL Reallocate(vals, INT(dims, I4B))
  mem = H5T_NATIVE_REAL
  CALL H5DREAD_F(dset_id, mem, vals, dims, error)
END IF
CALL postRead(obj, path%Chars(), dset_id, dspace_id, error)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE hdf5_read_s4

!----------------------------------------------------------------------------
!                                                                       Read
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_s5
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_read_s5()"
#endif
INTEGER(I4B), PARAMETER :: rank = 5
TYPE(String) :: path
INTEGER(HSIZE_T), DIMENSION(rank) :: dims
INTEGER(HID_T) :: mem
INTEGER(HID_T) :: dspace_id, dset_id
INTEGER(I4B) :: error

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

path = dsetname
CALL preRead(obj, path%Chars(), rank, dset_id, dspace_id, dims, error)
IF (error >= 0) THEN
  CALL Reallocate(vals, INT(dims, I4B))
  mem = H5T_NATIVE_REAL
  CALL H5DREAD_F(dset_id, mem, vals, dims, error)
END IF
CALL postRead(obj, path%Chars(), dset_id, dspace_id, error)
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE hdf5_read_s5

!----------------------------------------------------------------------------
!                                                                      Read
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_s6
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_read_s6()"
#endif
INTEGER(I4B), PARAMETER :: rank = 6
TYPE(String) :: path
INTEGER(HSIZE_T), DIMENSION(rank) :: dims
INTEGER(HID_T) :: mem
INTEGER(HID_T) :: dspace_id, dset_id
INTEGER(I4B) :: error

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

path = dsetname
CALL preRead(obj, path%Chars(), rank, dset_id, dspace_id, dims, error)
IF (error >= 0) THEN
  CALL Reallocate(vals, INT(dims, I4B))
  mem = H5T_NATIVE_REAL
  CALL H5DREAD_F(dset_id, mem, vals, dims, error)
END IF
CALL postRead(obj, path%Chars(), dset_id, dspace_id, error)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE hdf5_read_s6

!----------------------------------------------------------------------------
!                                                                      Read
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_s7
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_read_s7()"
#endif
INTEGER(I4B), PARAMETER :: rank = 7
TYPE(String) :: path
INTEGER(HSIZE_T), DIMENSION(rank) :: dims
INTEGER(HID_T) :: mem
INTEGER(HID_T) :: dspace_id, dset_id
INTEGER(I4B) :: error

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

path = dsetname
CALL preRead(obj, path%Chars(), rank, dset_id, dspace_id, dims, error)
IF (error >= 0) THEN
  CALL Reallocate(vals, INT(dims, I4B))
  mem = H5T_NATIVE_REAL
  CALL H5DREAD_F(dset_id, mem, vals, dims, error)
END IF
CALL postRead(obj, path%Chars(), dset_id, dspace_id, error)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE hdf5_read_s7

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ReadReal32
