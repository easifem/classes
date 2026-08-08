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

SUBMODULE(HDF5File_Class) WriteInt32
USE HDF5, ONLY: H5T_NATIVE_INTEGER, H5DWRITE_F
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "HDF5File_Class@WriteInt32.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                 Write
!----------------------------------------------------------------------------

#ifdef mem_type
#undef mem_type
#endif
#define mem_type H5T_NATIVE_INTEGER

MODULE PROCEDURE hdf5_write_n0
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_write_n0()"
#endif
#define rank 0
#include "./include/write.F90"
#undef rank
END PROCEDURE hdf5_write_n0

!----------------------------------------------------------------------------
!                                                                 Write
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_write_n1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_write_n1()"
#endif
#define rank 1
#include "./include/write.F90"
#undef rank
END PROCEDURE hdf5_write_n1

!----------------------------------------------------------------------------
!                                                                 Write
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_write_n2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_write_n2()"
#endif
#define rank 2
#include "./include/write.F90"
#undef rank
END PROCEDURE hdf5_write_n2

!----------------------------------------------------------------------------
!                                                                 Write
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_write_n3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_write_n3()"
#endif
#define rank 3
#include "./include/write.F90"
#undef rank
END PROCEDURE hdf5_write_n3

!----------------------------------------------------------------------------
!                                                                 Write
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_write_n4
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_write_n4()"
#endif
#define rank 4
#include "./include/write.F90"
#undef rank
END PROCEDURE hdf5_write_n4

!----------------------------------------------------------------------------
!                                                                 Write
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_write_n5
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_write_n5()"
#endif
#define rank 5
#include "./include/write.F90"
#undef rank
END PROCEDURE hdf5_write_n5

!----------------------------------------------------------------------------
!                                                                 Write
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_write_n6
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_write_n6()"
#endif
#define rank 6
#include "./include/write.F90"
#undef rank
END PROCEDURE hdf5_write_n6

!----------------------------------------------------------------------------
!                                                                 Write
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_write_n7
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_write_n7()"
#endif
#define rank 7
#include "./include/write.F90"
#undef rank
END PROCEDURE hdf5_write_n7

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE WriteInt32
