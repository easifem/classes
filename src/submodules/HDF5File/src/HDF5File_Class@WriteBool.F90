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
USE HDF5, ONLY: H5DWRITE_F
USE HDF5, ONLY: H5KIND_TO_TYPE
USE HDF5, ONLY: H5_INTEGER_KIND
USE HDF5, ONLY: H5TCLOSE_F
USE HDF5, ONLY: H5TENUM_CREATE_F
USE HDF5, ONLY: H5TENUM_INSERT_F
USE ISO_C_BINDING, ONLY: C_LOC
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
#define mem_type_bool

MODULE PROCEDURE obj_Write_b0
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Write_b0()"
#endif
#define DATA_RANK 0
#include "./include/write.F90"
#undef DATA_RANK
END PROCEDURE obj_Write_b0

!----------------------------------------------------------------------------
!                                                                      Write
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Write_b1
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Write_b1()"
#endif
#define DATA_RANK 1
#include "./include/write.F90"
#undef DATA_RANK
END PROCEDURE obj_Write_b1

!----------------------------------------------------------------------------
!                                                                      Write
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Write_b2
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Write_b2()"
#endif
#define DATA_RANK 2
#include "./include/write.F90"
#undef DATA_RANK
END PROCEDURE obj_Write_b2

!----------------------------------------------------------------------------
!                                                                     Write
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Write_b3
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Write_b3()"
#endif
#define DATA_RANK 3
#include "./include/write.F90"
#undef DATA_RANK
END PROCEDURE obj_Write_b3

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------
!                                                               BoolEnumType
!----------------------------------------------------------------------------

MODULE PROCEDURE BoolEnumType
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "BoolEnumType()"
#endif
  INTEGER(INT8), TARGET :: v
  INTEGER(I4B) :: error

  ! The enum {FALSE=0, TRUE=1} on an 8-bit integer, as written by h5py
  CALL H5TENUM_CREATE_F(H5KIND_TO_TYPE(INT8, H5_INTEGER_KIND), enum_id, &
                        error)
  v = 0_INT8
  CALL H5TENUM_INSERT_F(enum_id, "FALSE", C_LOC(v), error)
  v = 1_INT8
  CALL H5TENUM_INSERT_F(enum_id, "TRUE", C_LOC(v), error)

#ifdef DEBUG_VER
  CALL AssertError1(error .EQ. 0, myName, &
                    'Could not create the enum type of logical data.')
#endif
END PROCEDURE BoolEnumType

!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE WriteBool
