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

SUBMODULE(HDF5File_Class) WriteBool
USE HDF5, ONLY: H5T_NATIVE_CHARACTER, H5DWRITE_F
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "HDF5File_Class@WriteBool.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                      Write
!----------------------------------------------------------------------------

#ifdef mem_type
#undef mem_type
#endif
#define mem_type H5T_NATIVE_CHARACTER
#define mem_type_bool

MODULE PROCEDURE hdf5_write_b0
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_write_b0()"
#endif
#define rank 0
#include "./include/write.F90"
#undef rank
END PROCEDURE hdf5_write_b0

!----------------------------------------------------------------------------
!                                                                      Write
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_write_b1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_write_b1()"
#endif
#define rank 1
#include "./include/write.F90"
#undef rank
END PROCEDURE hdf5_write_b1

!----------------------------------------------------------------------------
!                                                                      Write
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_write_b2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_write_b2()"
#endif
#define rank 2
#include "./include/write.F90"
#undef rank
END PROCEDURE hdf5_write_b2

!----------------------------------------------------------------------------
!                                                                     Write
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_write_b3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_write_b3()"
#endif
#define rank 3
#include "./include/write.F90"
#undef rank
END PROCEDURE hdf5_write_b3

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE WriteBool
