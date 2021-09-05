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

SUBMODULE( HDF5File_Class ) ReadAttribute
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                              ReadAttribute
!----------------------------------------------------------------------------

SUBROUTINE hdf5_read_attribute_st0_helper(attr_id,length_max,attr_val)
  TYPE( String ), INTENT( INOUT ) :: attr_val
  INTEGER( SIZE_T ), INTENT( IN ) :: length_max
  INTEGER( HID_T ), INTENT( IN ) :: attr_id
  ! Internal variables
  INTEGER( HID_T ) :: atype_id
  INTEGER( HSIZE_T ), DIMENSION( 1 ) :: dims
  CHARACTER( LEN=length_max,KIND=C_CHAR ), TARGET :: buf
  INTEGER( I4B ) :: error
  !
  dims(1)=1
  CALL h5aget_type_f(attr_id,atype_id,error)
  CALL h5aread_f(attr_id,atype_id,buf,dims,error)
  attr_val=buf
END SUBROUTINE hdf5_read_attribute_st0_helper

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_attribute_st0
  INTEGER( HID_T ) :: attr_id, obj_id
  INTEGER( SIZE_T ):: max_size
  INTEGER( I4B ) :: error
  !Prepare the File and object for the attribute
  CALL open_object(obj,obj_name,obj_id)
  CALL open_attribute(obj,obj_id,attr_name,attr_id)
  CALL h5aget_storage_size_f(attr_id,max_size,error)
  CALL hdf5_read_attribute_st0_helper(attr_id,max_size,attr_val)
  CALL close_attribute(obj,attr_id)
  CALL close_object(obj,obj_id)
END PROCEDURE hdf5_read_attribute_st0

!----------------------------------------------------------------------------
!                                                             ReadAttribute
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_attribute_c0
  TYPE( String ) :: str_val
  CALL obj%readAttribute(obj_name,attr_name,str_val)
  attr_val=str_val%chars()
END PROCEDURE hdf5_read_attribute_c0

!----------------------------------------------------------------------------
!                                                             ReadAttribute
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_attribute_i0
  CHARACTER( LEN=* ),PARAMETER :: myName='read_attribute_i0_HDF5FileType'
  INTEGER( HID_T ) :: attr_id, obj_id
  INTEGER( HSIZE_T ), DIMENSION( 1 ) :: dims
  INTEGER( I4B ) :: error

  dims(1)=1
  !Prepare the File and object for the attribute
  CALL open_object(obj,obj_name,obj_id)
  CALL open_attribute(obj,obj_id,attr_name,attr_id)
  CALL h5aread_f(attr_id,H5T_NATIVE_INTEGER,attr_val,dims,error)
  IF(error .NE. 0) THEN
    CALL obj%e%raiseError(modName//'::'//myName// &
      & ' - Failed to read attribute.')
    RETURN
  ENDIF
  CALL close_attribute(obj,attr_id)
  CALL close_object(obj,obj_id)
END PROCEDURE hdf5_read_attribute_i0

!----------------------------------------------------------------------------
!                                                             ReadAttribute
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_read_attribute_d0
  CHARACTER( LEN=* ), PARAMETER :: myName='read_attribute_d0_HDF5FileType'
  INTEGER( HID_T ) :: attr_id, obj_id
  INTEGER( HSIZE_T ),DIMENSION(1) :: dims
  INTEGER( I4B ) :: error

  dims(1)=1
  !Prepare the File and object for the attribute
  CALL open_object(obj,obj_name,obj_id)
  CALL open_attribute(obj,obj_id,attr_name,attr_id)
  CALL h5aread_f(attr_id,H5T_NATIVE_DOUBLE,attr_val,dims,error)
  IF(error .NE. 0) THEN
    CALL obj%e%raiseError(modName//'::'//myName// &
      & ' - Failed to read attribute.')
    RETURN
  ENDIF
  CALL close_attribute(obj,attr_id)
  CALL close_object(obj,obj_id)
END PROCEDURE hdf5_read_attribute_d0

END SUBMODULE ReadAttribute