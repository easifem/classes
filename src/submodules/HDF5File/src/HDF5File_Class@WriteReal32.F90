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

SUBMODULE(HDF5File_Class) WriteReal32
USE HDF5, ONLY: H5T_NATIVE_REAL, H5DWRITE_F
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "HDF5File_Class@WriteReal32.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                 Write
!----------------------------------------------------------------------------
#ifdef mem_type
#undef mem_type
#endif
#define mem_type H5T_NATIVE_REAL

MODULE PROCEDURE obj_Write_s0
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Write_s0()"
#endif
#define DATA_RANK 0
#include "./include/write.F90"
#undef DATA_RANK
END PROCEDURE obj_Write_s0

!----------------------------------------------------------------------------
!                                                                  Write
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Write_s1
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Write_s1()"
#endif
#define DATA_RANK 1
#include "./include/write.F90"
#undef DATA_RANK
END PROCEDURE obj_Write_s1

!----------------------------------------------------------------------------
!                                                                 Write
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Write_s2
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Write_s2()"
#endif
#define DATA_RANK 2
#include "./include/write.F90"
#undef DATA_RANK
END PROCEDURE obj_Write_s2

!----------------------------------------------------------------------------
!                                                                 Write
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Write_s3
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Write_s3()"
#endif
#define DATA_RANK 3
#include "./include/write.F90"
#undef DATA_RANK
END PROCEDURE obj_Write_s3

!----------------------------------------------------------------------------
!                                                                 Write
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Write_s4
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Write_s4()"
#endif
#define DATA_RANK 4
#include "./include/write.F90"
#undef DATA_RANK
END PROCEDURE obj_Write_s4

!----------------------------------------------------------------------------
!                                                                 Write
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Write_s5
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Write_s5()"
#endif
#define DATA_RANK 5
#include "./include/write.F90"
#undef DATA_RANK
END PROCEDURE obj_Write_s5

!----------------------------------------------------------------------------
!                                                                 Write
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Write_s6
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Write_s6()"
#endif
#define DATA_RANK 6
#include "./include/write.F90"
#undef DATA_RANK
END PROCEDURE obj_Write_s6

!----------------------------------------------------------------------------
!                                                                 Write
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Write_s7
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Write_s7()"
#endif
#define DATA_RANK 7
#include "./include/write.F90"
#undef DATA_RANK
END PROCEDURE obj_Write_s7

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE WriteReal32
