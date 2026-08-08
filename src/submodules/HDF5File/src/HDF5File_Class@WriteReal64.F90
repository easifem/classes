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

SUBMODULE(HDF5File_Class) WriteReal64
USE HDF5, ONLY: H5T_NATIVE_DOUBLE, H5DWRITE_F
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "HDF5File_Class@WriteReal64.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                 Write
!----------------------------------------------------------------------------

#ifdef mem_type
#undef mem_type
#endif
#define mem_type H5T_NATIVE_DOUBLE

MODULE PROCEDURE hdf5_write_d0
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_write_d0()"
#endif
#define rank 0
#include "./include/write.F90"
#undef rank
END PROCEDURE hdf5_write_d0

!----------------------------------------------------------------------------
!                                                                  Write
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_write_d1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_write_d1()"
#endif
#define rank 1
#include "./include/write.F90"
#undef rank
END PROCEDURE hdf5_write_d1

!----------------------------------------------------------------------------
!                                                                 Write
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_write_d2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_write_d2()"
#endif
#define rank 2
#include "./include/write.F90"
#undef rank
END PROCEDURE hdf5_write_d2

!----------------------------------------------------------------------------
!                                                                 Write
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_write_d3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_write_d3()"
#endif
#define rank 3
#include "./include/write.F90"
#undef rank
END PROCEDURE hdf5_write_d3

!----------------------------------------------------------------------------
!                                                                 Write
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_write_d4
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_write_d4()"
#endif
#define rank 4
#include "./include/write.F90"
#undef rank
END PROCEDURE hdf5_write_d4

!----------------------------------------------------------------------------
!                                                                 Write
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_write_d5
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_write_d5()"
#endif
#define rank 5
#include "./include/write.F90"
#undef rank
END PROCEDURE hdf5_write_d5

!----------------------------------------------------------------------------
!                                                                 Write
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_write_d6
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_write_d6()"
#endif
#define rank 6
#include "./include/write.F90"
#undef rank
END PROCEDURE hdf5_write_d6

!----------------------------------------------------------------------------
!                                                                 Write
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_write_d7
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_write_d7()"
#endif
#define rank 7
#include "./include/write.F90"
#undef rank
END PROCEDURE hdf5_write_d7

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE WriteReal64
