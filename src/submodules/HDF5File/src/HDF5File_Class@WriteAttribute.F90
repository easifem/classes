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

SUBMODULE(HDF5File_Class) WriteAttribute
USE ISO_C_BINDING, ONLY: C_CHAR
USE HDF5, ONLY: SIZE_T, H5ACREATE_F, H5AOPEN_F, H5AWRITE_F, H5LEXISTS_F, &
                H5OCLOSE_F, H5OOPEN_F, H5SCLOSE_F, H5SCREATE_SIMPLE_F, &
                H5TCLOSE_F, H5TCOPY_F, H5T_NATIVE_CHARACTER, &
                H5T_NATIVE_DOUBLE, H5ACLOSE_F, H5AEXISTS_F, &
                H5SCREATE_SIMPLE_F, H5TSET_SIZE_F, &
                H5T_NATIVE_INTEGER
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "HDF5File_Class@WriteAttribute.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                 OpenObject
!----------------------------------------------------------------------------

MODULE PROCEDURE open_object
#ifdef DEBUG_VER
CHARACTER(LEN=*), PARAMETER :: myName = 'open_object_HDF5File_Class()'
#endif
TYPE(String) :: path
LOGICAL(LGT) :: dset_exists
INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

path = obj_name
!Check for expected links between object, and File
CALL H5LEXISTS_F(obj%file_id, path%Chars(), dset_exists, ierr)
#ifdef DEBUG_VER
CALL AssertError1(dset_exists, myName, &
                  'Incorrect path to object.')
#endif

!Open the object
CALL H5OOPEN_F(obj%file_id, path%Chars(), obj_id, ierr)

#ifdef DEBUG_VER
CALL AssertError1(ierr .EQ. 0, myName, &
                  'Failed to open object.')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE open_object

!----------------------------------------------------------------------------
!                                                                 CloseObject
!----------------------------------------------------------------------------

MODULE PROCEDURE close_object
#ifdef DEBUG_VER
CHARACTER(LEN=*), PARAMETER :: myName = 'close_object()'
#endif
INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL H5OCLOSE_F(obj_id, ierr)

#ifdef DEBUG_VER
CALL AssertError1(ierr .EQ. 0, myName, &
                  'Failed to close object.')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE close_object

!----------------------------------------------------------------------------
!                                                            CreateAttribute
!----------------------------------------------------------------------------

MODULE PROCEDURE createAttribute
LOGICAL(LGT) :: attr_exists
INTEGER(I4B) :: error
#ifdef DEBUG_VER
CHARACTER(LEN=*), PARAMETER :: myName = 'createAttribute()'
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL H5AEXISTS_F(obj_id, attr_name, attr_exists, error)
!Create and write to the attribute within the dataspce
IF (obj%overwriteStat .AND. attr_exists) THEN
  ! Open the attribute
  CALL H5AOPEN_F(obj_id, attr_name, attr_id, error)

#ifdef DEBUG_VER
  CALL AssertError1(error .EQ. 0, myName, &
                    'Unable to open attribute.')
#endif

ELSE
  ! Create the attribute
  CALL H5ACREATE_F(obj_id, attr_name, atype_id, dspace_id, attr_id, error)

#ifdef DEBUG_VER
  CALL AssertError1(error .EQ. 0, myName, &
                    'Unable to create attribute.')
#endif
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE createAttribute

!----------------------------------------------------------------------------
!                                                              OpenAttribute
!----------------------------------------------------------------------------

MODULE PROCEDURE open_attribute
LOGICAL(LGT) :: attr_exists
INTEGER(I4B) :: error
#ifdef DEBUG_VER
CHARACTER(LEN=*), PARAMETER :: myName = 'open_attribute()'
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

!Check that the named attribute exists
CALL H5AEXISTS_F(obj_id, attr_name, attr_exists, error)

#ifdef DEBUG_VER
CALL AssertError1(attr_exists, myName, &
                  'Attribute does not exist for object.')
#endif

!Open the Attribute
CALL H5AOPEN_F(obj_id, attr_name, attr_id, error)

#ifdef DEBUG_VER
CALL AssertError1(error .EQ. 0, myName, &
                  'Failed to open attribute.')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE open_attribute

!----------------------------------------------------------------------------
!                                                            closeAttribute
!----------------------------------------------------------------------------

MODULE PROCEDURE close_attribute
#ifdef DEBUG_VER
CHARACTER(LEN=*), PARAMETER :: myName = 'close_attribute()'
#endif
INTEGER(I4B) :: error

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL H5ACLOSE_F(attr_id, error)

#ifdef DEBUG_VER
CALL AssertError1(error .EQ. 0, myName, &
                  'Failed to close objectt.')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE close_attribute

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_write_attribute_st0
#ifdef DEBUG_VER
CHARACTER(LEN=*), PARAMETER :: myName = 'hdf5_write_attribute_st0()'
#endif
INTEGER(I4B) :: num_dims, error
INTEGER(HID_T) :: atype_id, attr_id, dspace_id, obj_id
INTEGER(HSIZE_T), DIMENSION(1) :: dims
INTEGER(SIZE_T) :: attr_len
CHARACTER(LEN=:, KIND=C_CHAR), ALLOCATABLE :: valss

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

num_dims = 1
dims(1) = 1
valss = attr_val%chars()
attr_len = INT(LEN(valss), I4B)
!Prepare the File and object for the attribute
CALL open_object(obj, obj_name, obj_id)
!Create the data space for memory type and size
CALL H5SCREATE_SIMPLE_F(num_dims, dims, dspace_id, error)
CALL H5TCOPY_F(H5T_NATIVE_CHARACTER, atype_id, error)
CALL H5TSET_SIZE_F(atype_id, attr_len, error)
CALL createAttribute(obj, obj_id, attr_name, atype_id, dspace_id, attr_id)
CALL H5AWRITE_F(attr_id, atype_id, TRIM(valss), dims, error)
!Close datatype opened by h5tcopy_f
CALL H5TCLOSE_F(atype_id, error)
!Close dataspace, attribute and object
CALL H5SCLOSE_F(dspace_id, error)
CALL close_attribute(obj, attr_id)
CALL close_object(obj, obj_id)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE hdf5_write_attribute_st0

!----------------------------------------------------------------------------
!                                                            WriteAttribute
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_write_attribute_c0
#ifdef DEBUG_VER
CHARACTER(LEN=*), PARAMETER :: myName = 'hdf5_write_attribute_c0()'
#endif
TYPE(String) :: str_val

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

str_val = TRIM(attr_val)
CALL obj%writeAttribute(obj_name, attr_name, str_val)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE hdf5_write_attribute_c0

!----------------------------------------------------------------------------
!                                                            WriteAttribute
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_write_attribute_i0
#ifdef DEBUG_VER
CHARACTER(LEN=*), PARAMETER :: myName = 'hdf5_write_attribute_i0()'
#endif
INTEGER(I4B) :: num_dims, error
INTEGER(HID_T) :: attr_id, dspace_id, obj_id
INTEGER(HSIZE_T), DIMENSION(1) :: dims

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

num_dims = 1
dims(1) = 1
!Prepare the File and object for the attribute
CALL open_object(obj, obj_name, obj_id)
!Create the data space for memory type and size
CALL H5SCREATE_SIMPLE_F(num_dims, dims, dspace_id, error)
!Create and write to the attribute within the dataspce
CALL createAttribute(obj, obj_id, attr_name, H5T_NATIVE_INTEGER, &
                     dspace_id, attr_id)
CALL H5AWRITE_F(attr_id, H5T_NATIVE_INTEGER, attr_val, dims, error)
CALL H5SCLOSE_F(dspace_id, error)
CALL close_attribute(obj, attr_id)
CALL close_object(obj, obj_id)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE hdf5_write_attribute_i0

!----------------------------------------------------------------------------
!                                                            WriteAttribute
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_write_attribute_d0
#ifdef DEBUG_VER
CHARACTER(LEN=*), PARAMETER :: myName = 'hdf5_write_attribute_d0()'
#endif
INTEGER(I4B) :: num_dims, error
INTEGER(HID_T) :: attr_id, dspace_id, obj_id
INTEGER(HSIZE_T), DIMENSION(1) :: dims

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

num_dims = 1
dims(1) = 1
!Prepare the File and object for the attribute
CALL open_object(obj, obj_name, obj_id)
!Create the data space for memory type and size
CALL H5SCREATE_SIMPLE_F(num_dims, dims, dspace_id, error)
!Create and write to the attribute within the dataspce
CALL createAttribute(obj, obj_id, attr_name, H5T_NATIVE_DOUBLE, &
                     dspace_id, attr_id)
CALL H5AWRITE_F(attr_id, H5T_NATIVE_DOUBLE, attr_val, dims, error)
CALL H5SCLOSE_F(dspace_id, error)
CALL close_attribute(obj, attr_id)
CALL close_object(obj, obj_id)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE hdf5_write_attribute_d0

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE WriteAttribute
