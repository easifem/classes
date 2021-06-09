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

SUBMODULE( HDF5File_Class ) WriteAttribute
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 OpenObject
!----------------------------------------------------------------------------

MODULE PROCEDURE open_object
  ! Internal variables
  CHARACTER( LEN=LEN(obj_name)+1 ) :: path
  LOGICAL( LGT ) :: dset_exists
  CHARACTER( LEN=* ), PARAMETER :: myName='open_object_HDF5File_Class'


  path=obj_name
  !Check for expected links between object, and File
  CALL h5lexists_f(obj%file_id,path,dset_exists,ierr)
  IF( .NOT. dset_exists ) THEN
    CALL obj%e%raiseError(modName//'::'//myName// &
    ' - Incorrect path to object.')
    RETURN
  ENDIF

  !Open the object
  CALL h5Oopen_f(obj%file_id, path, obj_id, ierr)
  IF(ierr .NE. 0) THEN
    CALL obj%e%raiseError(modName//'::'//myName// &
    ' - Failed to open object.')
    RETURN
  ENDIF
END PROCEDURE open_object

!----------------------------------------------------------------------------
!                                                                 CloseObject
!----------------------------------------------------------------------------

MODULE PROCEDURE close_object
  CHARACTER( LEN=* ), PARAMETER :: myName='close_object_HDF5FileType'
  CALL h5Oclose_f(obj_id,ierr)
  IF (ierr .NE. 0) THEN
    CALL obj%e%raiseError(modName//'::'//myName// &
    ' - Failed to close objectt.')
    RETURN
  ENDIF
END PROCEDURE close_object

!----------------------------------------------------------------------------
!                                                            CreateAttribute
!----------------------------------------------------------------------------

MODULE PROCEDURE createAttribute
  ! Internal variables
  LOGICAL( LGT ) :: attr_exists
  INTEGER( I4B ) :: error
  CHARACTER( LEN = * ), PARAMETER :: myName='createAttribute'

  CALL h5aexists_f(obj_id,attr_name,attr_exists,error)
  !Create and write to the attribute within the dataspce
  IF(obj%overwriteStat .AND. attr_exists) THEN
    ! Open the attribute
    CALL h5aopen_f(obj_id,attr_name,attr_id,error)
    IF(error .NE. 0) CALL obj%e%raiseError(modName//'::'//myName// &
      & ' - Unable to open attribute.')
  ELSE
    ! Create the attribute
    CALL h5acreate_f(obj_id,attr_name,atype_id,dspace_id,attr_id,error)
    IF(error .NE. 0) CALL obj%e%raiseError(modName//'::'//myName// &
      & ' - Unable to create attribute.')
  ENDIF
END PROCEDURE createAttribute

!----------------------------------------------------------------------------
!                                                              OpenAttribute
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	9 June 2021
! summary: 	Sets up the attribute wrting general operation by checking existance

MODULE PROCEDURE open_attribute
  LOGICAL( LGT ) :: attr_exists
  INTEGER( I4B ) :: error
  CHARACTER( LEN=* ), PARAMETER :: myName='open_attribute_rHDF5FileType'

  !Check that the named attribute exists
  CALL h5aexists_f(obj_id,attr_name,attr_exists,error)
  IF (.NOT. attr_exists) THEN
    CALL obj%e%raiseError(modName//'::'//myName// &
    ' - Attribute does not exist for object.')
    RETURN
  ENDIF

  !Open the Attribute
  CALL h5aopen_f(obj_id,attr_name,attr_id,error)
  IF(error /= 0) THEN
    CALL obj%e%raiseError(modName//'::'//myName// &
    ' - Failed to open attribute.')
    RETURN
  ENDIF
END PROCEDURE open_attribute

!----------------------------------------------------------------------------
!                                                            closeAttribute
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	9 June 2021
! summary: closes all attribute operations by closing attribute

MODULE PROCEDURE close_attribute
  CHARACTER( LEN=* ), PARAMETER :: &
    & myName='close_attribute_HDF5FileType'
  INTEGER( I4B ) :: error

  CALL h5aclose_f(attr_id,error)
  IF (error .NE. 0) THEN
    CALL obj%e%raiseError(modName//'::'//myName// &
      & ' - Failed to close objectt.')
    RETURN
  ENDIF
END PROCEDURE close_attribute

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_write_attribute_st0
  INTEGER( I4B ) :: num_dims, error
  INTEGER( HID_T ) :: atype_id, attr_id, dspace_id, obj_id
  INTEGER( HSIZE_T ), DIMENSION(1) :: dims
  INTEGER( SIZE_T ) :: attr_len
  CHARACTER( LEN=:,KIND=C_CHAR ), ALLOCATABLE :: valss
  num_dims=1
  dims(1)=1
  valss=attr_val%chars()
  attr_len=INT(LEN(valss),I4B)
  !Prepare the File and object for the attribute
  CALL open_object(obj,obj_name,obj_id)
  !Create the data space for memory type and size
  CALL h5screate_simple_f(num_dims,dims,dspace_id,error)
  CALL h5tcopy_f(H5T_NATIVE_CHARACTER,atype_id,error)
  CALL h5tset_size_f(atype_id,attr_len,error)
  CALL createAttribute(obj,obj_id,attr_name,atype_id,dspace_id,attr_id)
  CALL h5awrite_f(attr_id,atype_id,TRIM(valss),dims,error)
  !Close datatype opened by h5tcopy_f
  CALL h5tclose_f(atype_id,error)
  !Close dataspace, attribute and object
  CALL h5sclose_f(dspace_id,error)
  CALL close_attribute(obj,attr_id)
  CALL close_object(obj,obj_id)
END PROCEDURE hdf5_write_attribute_st0

!----------------------------------------------------------------------------
!                                                            WriteAttribute
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_write_attribute_c0
  TYPE( String ) :: str_val
  str_val = TRIM(attr_val)
  CALL obj%writeAttribute(obj_name,attr_name,str_val)
END PROCEDURE hdf5_write_attribute_c0

!----------------------------------------------------------------------------
!                                                            WriteAttribute
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_write_attribute_i0
  INTEGER( I4B ) :: num_dims, error
  INTEGER( HID_T ) :: attr_id, dspace_id, obj_id
  INTEGER( HSIZE_T ), DIMENSION( 1 ) :: dims
  num_dims=1
  dims(1)=1
  !Prepare the File and object for the attribute
  CALL open_object(obj,obj_name,obj_id)
  !Create the data space for memory type and size
  CALL h5screate_simple_f(num_dims,dims,dspace_id,error)
  !Create and write to the attribute within the dataspce
  CALL createAttribute(obj,obj_id,attr_name,H5T_NATIVE_INTEGER,&
    & dspace_id,attr_id)
  CALL h5awrite_f(attr_id,H5T_NATIVE_INTEGER,attr_val,dims,error)
  CALL h5sclose_f(dspace_id,error)
  CALL close_attribute(obj,attr_id)
  CALL close_object(obj,obj_id)
END PROCEDURE hdf5_write_attribute_i0

!----------------------------------------------------------------------------
!                                                            WriteAttribute
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_write_attribute_d0
  INTEGER( I4B ) :: num_dims,error
  INTEGER( HID_T ) :: attr_id, dspace_id, obj_id
  INTEGER( HSIZE_T ), DIMENSION( 1 ) :: dims
  num_dims=1
  dims(1)=1
  !Prepare the File and object for the attribute
  CALL open_object(obj,obj_name,obj_id)
  !Create the data space for memory type and size
  CALL h5screate_simple_f(num_dims,dims,dspace_id,error)
  !Create and write to the attribute within the dataspce
  CALL createAttribute(obj,obj_id,attr_name,H5T_NATIVE_DOUBLE,&
  dspace_id,attr_id)
  CALL h5awrite_f(attr_id,H5T_NATIVE_DOUBLE,attr_val,dims,error)
  CALL h5sclose_f(dspace_id,error)
  CALL close_attribute(obj,attr_id)
  CALL close_object(obj,obj_id)
END PROCEDURE hdf5_write_attribute_d0

END SUBMODULE WriteAttribute