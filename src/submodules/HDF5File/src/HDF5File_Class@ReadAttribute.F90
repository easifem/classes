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
! along with obj program.  If not, see <https: //www.gnu.org/licenses/>
!

SUBMODULE(HDF5File_Class) ReadAttribute
USE HDF5, ONLY: SIZE_T, H5AGET_TYPE_F, H5AREAD_F, H5T_NATIVE_INTEGER, &
                H5T_NATIVE_DOUBLE, H5AREAD_F, H5AGET_STORAGE_SIZE_F

USE ISO_C_BINDING, ONLY: C_CHAR
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "HDF5File_Class@ReadAttribute.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                              ReadAttribute
!----------------------------------------------------------------------------

SUBROUTINE hdf5_read_attribute_st0_helper(attr_id, length_max, attr_val)
  TYPE(String), INTENT(INOUT) :: attr_val
  INTEGER(SIZE_T), INTENT(IN) :: length_max
  INTEGER(HID_T), INTENT(IN) :: attr_id
  ! Define internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "hdf5_read_attribute_st0_helper()"
#endif
  INTEGER(HID_T) :: atype_id
  INTEGER(HSIZE_T), DIMENSION(1) :: dims
  CHARACTER(LEN=length_max, KIND=C_CHAR), TARGET :: buf
  INTEGER(I4B) :: error

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  dims(1) = 1
  CALL H5AGET_TYPE_F(attr_id, atype_id, error)
  CALL H5AREAD_F(attr_id, atype_id, buf, dims, error)
  attr_val = buf

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE hdf5_read_attribute_st0_helper

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_attribute_st0
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_read_attribute_st0()"
#endif
INTEGER(HID_T) :: attr_id, obj_id
INTEGER(SIZE_T) :: max_size
INTEGER(I4B) :: error

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

!Prepare the File and object for the attribute
CALL open_object(obj, obj_name, obj_id)
CALL open_attribute(obj, obj_id, attr_name, attr_id)
CALL H5AGET_STORAGE_SIZE_F(attr_id, max_size, error)
CALL HDF5_READ_ATTRIBUTE_ST0_HELPER(attr_id, max_size, attr_val)
CALL close_attribute(obj, attr_id)
CALL close_object(obj, obj_id)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE hdf5_read_attribute_st0

!----------------------------------------------------------------------------
!                                                             ReadAttribute
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_attribute_c0
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_read_attribute_c0()"
#endif
TYPE(String) :: str_val

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%ReadAttribute(obj_name, attr_name, str_val)
attr_val = str_val%Chars()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE hdf5_read_attribute_c0

!----------------------------------------------------------------------------
!                                                             ReadAttribute
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_attribute_i0
#ifdef DEBUG_VER
CHARACTER(LEN=*), PARAMETER :: myName = 'hdf5_read_attribute_i0()'
#endif
INTEGER(HID_T) :: attr_id, obj_id
INTEGER(HSIZE_T), DIMENSION(1) :: dims
INTEGER(I4B) :: error

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

dims(1) = 1
!Prepare the File and object for the attribute
CALL open_object(obj, obj_name, obj_id)
CALL open_attribute(obj, obj_id, attr_name, attr_id)
CALL H5AREAD_F(attr_id, H5T_NATIVE_INTEGER, attr_val, dims, error)

#ifdef DEBUG_VER
CALL AssertError1(error .EQ. 0, myName, &
                  'Failed to read attribute.')
#endif

CALL close_attribute(obj, attr_id)
CALL close_object(obj, obj_id)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE hdf5_read_attribute_i0

!----------------------------------------------------------------------------
!                                                             ReadAttribute
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_attribute_d0
#ifdef DEBUG_VER
CHARACTER(LEN=*), PARAMETER :: myName = 'hdf5_read_attribute_d0()'
#endif
INTEGER(HID_T) :: attr_id, obj_id
INTEGER(HSIZE_T), DIMENSION(1) :: dims
INTEGER(I4B) :: error

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

dims(1) = 1
!Prepare the File and object for the attribute
CALL open_object(obj, obj_name, obj_id)
CALL open_attribute(obj, obj_id, attr_name, attr_id)
CALL H5AREAD_F(attr_id, H5T_NATIVE_DOUBLE, attr_val, dims, error)

#ifdef DEBUG_VER
CALL AssertError1(error .EQ. 0, myName, &
                  'Failed to read attribute.')
#endif

CALL close_attribute(obj, attr_id)
CALL close_object(obj, obj_id)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE hdf5_read_attribute_d0

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ReadAttribute
