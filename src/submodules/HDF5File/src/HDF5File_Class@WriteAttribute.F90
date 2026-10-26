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
  H5ACLOSE_F, H5AEXISTS_F, H5TSET_SIZE_F
USE HDF5, ONLY: H5ADELETE_F
USE HDF5, ONLY: H5KIND_TO_TYPE
USE HDF5, ONLY: H5_INTEGER_KIND
USE HDF5, ONLY: H5_REAL_KIND
USE ISO_C_BINDING, ONLY: C_NULL_CHAR
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
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
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
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
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
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  CALL H5OCLOSE_F(obj_id, ierr)

#ifdef DEBUG_VER
  CALL AssertError1(ierr .EQ. 0, myName, &
                    'Failed to close object.')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
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
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ! attr_id is -1 if the attribute is not created
  attr_id = -1

  CALL H5AEXISTS_F(obj_id, attr_name, attr_exists, error)

  IF (attr_exists) THEN
    ! An existing attribute is replaced only when overwriting is allowed
    IF (.NOT. obj%overwriteStat) THEN
#ifdef DEBUG_VER
      CALL AssertError1(math%no, myName, &
                        'Attribute ' // attr_name // &
                        ' exists and overwriting is ' // &
                        'not allowed (mode WRITE); call ' // &
                        'SetOverwriteStat(.TRUE.).')
      CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                              '[END] ')
#endif
      RETURN
    END IF

    ! Delete it, so that it is created with the new type and size
    CALL H5ADELETE_F(obj_id, attr_name, error)

#ifdef DEBUG_VER
    CALL AssertError1(error .EQ. 0, myName, &
                      'Unable to delete the old attribute ' // attr_name)
#endif
  END IF

  CALL H5ACREATE_F(obj_id, attr_name, atype_id, dspace_id, attr_id, error)
  IF (error .NE. 0) attr_id = -1

#ifdef DEBUG_VER
  CALL AssertError1(error .EQ. 0, myName, &
                    'Unable to create attribute.')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
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
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
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
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
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
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  CALL H5ACLOSE_F(attr_id, error)

#ifdef DEBUG_VER
  CALL AssertError1(error .EQ. 0, myName, &
                    'Failed to close objectt.')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE close_attribute

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteAttribute_st0
#ifdef DEBUG_VER
  CHARACTER(LEN=*), PARAMETER :: myName = 'obj_WriteAttribute_st0()'
#endif
  INTEGER(I4B) :: num_dims, error
  INTEGER(HID_T) :: atype_id, attr_id, dspace_id, obj_id
  INTEGER(HSIZE_T), DIMENSION(1) :: dims
  INTEGER(SIZE_T) :: attr_len
  CHARACTER(LEN=:, KIND=C_CHAR), ALLOCATABLE :: valss

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  num_dims = 1
  dims(1) = 1
  ! The buffer and the type have the same length; an empty String is
  ! written as one NUL character (ReadAttribute removes it), since a type
  ! of size 0 is not allowed
  valss = attr_val%chars()
  IF (LEN(valss) .EQ. 0) valss = C_NULL_CHAR
  attr_len = INT(LEN(valss), SIZE_T)
!Prepare the File and object for the attribute
  CALL open_object(obj, obj_name, obj_id)
!Create the data space for memory type and size
  CALL H5SCREATE_SIMPLE_F(num_dims, dims, dspace_id, error)
  CALL H5TCOPY_F(H5T_NATIVE_CHARACTER, atype_id, error)
  CALL H5TSET_SIZE_F(atype_id, attr_len, error)
  CALL createAttribute(obj, obj_id, attr_name, atype_id, dspace_id, attr_id)
  IF (attr_id .GT. 0) THEN
    CALL H5AWRITE_F(attr_id, atype_id, valss, dims, error)
    CALL close_attribute(obj, attr_id)
  END IF
!Close datatype opened by h5tcopy_f
  CALL H5TCLOSE_F(atype_id, error)
!Close dataspace and object
  CALL H5SCLOSE_F(dspace_id, error)
  CALL close_object(obj, obj_id)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_WriteAttribute_st0

!----------------------------------------------------------------------------
!                                                            WriteAttribute
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteAttribute_c0
#ifdef DEBUG_VER
  CHARACTER(LEN=*), PARAMETER :: myName = 'obj_WriteAttribute_c0()'
#endif
  TYPE(String) :: str_val

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  str_val = TRIM(attr_val)
  CALL obj%WriteAttribute(obj_name, attr_name, str_val)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_WriteAttribute_c0

!----------------------------------------------------------------------------
!                                                            WriteAttribute
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteAttribute_i0
#ifdef DEBUG_VER
  CHARACTER(LEN=*), PARAMETER :: myName = 'obj_WriteAttribute_i0()'
#endif
  INTEGER(I4B) :: error
  INTEGER(HID_T) :: attr_id, dspace_id, obj_id, mem
  INTEGER(HSIZE_T), DIMENSION(1) :: dims

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ! The HDF5 type follows the kind of attr_val
  dims(1) = 1
  mem = H5KIND_TO_TYPE(KIND(attr_val), H5_INTEGER_KIND)

  CALL open_object(obj, obj_name, obj_id)
  CALL H5SCREATE_SIMPLE_F(1, dims, dspace_id, error)
  CALL createAttribute(obj, obj_id, attr_name, mem, dspace_id, attr_id)

  IF (attr_id .GT. 0) THEN
    IF (dims(1) .GT. 0) THEN
      CALL H5AWRITE_F(attr_id, mem, attr_val, dims, error)
    END IF

#ifdef DEBUG_VER
    CALL AssertError1(error .EQ. 0, myName, &
                      'Failed to write attribute.')
#endif

    CALL close_attribute(obj, attr_id)
  END IF

  CALL H5SCLOSE_F(dspace_id, error)
  CALL close_object(obj, obj_id)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_WriteAttribute_i0

!----------------------------------------------------------------------------
!                                                            WriteAttribute
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteAttribute_d0
#ifdef DEBUG_VER
  CHARACTER(LEN=*), PARAMETER :: myName = 'obj_WriteAttribute_d0()'
#endif
  INTEGER(I4B) :: error
  INTEGER(HID_T) :: attr_id, dspace_id, obj_id, mem
  INTEGER(HSIZE_T), DIMENSION(1) :: dims

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ! The HDF5 type follows the kind of attr_val
  dims(1) = 1
  mem = H5KIND_TO_TYPE(KIND(attr_val), H5_REAL_KIND)

  CALL open_object(obj, obj_name, obj_id)
  CALL H5SCREATE_SIMPLE_F(1, dims, dspace_id, error)
  CALL createAttribute(obj, obj_id, attr_name, mem, dspace_id, attr_id)

  IF (attr_id .GT. 0) THEN
    IF (dims(1) .GT. 0) THEN
      CALL H5AWRITE_F(attr_id, mem, attr_val, dims, error)
    END IF

#ifdef DEBUG_VER
    CALL AssertError1(error .EQ. 0, myName, &
                      'Failed to write attribute.')
#endif

    CALL close_attribute(obj, attr_id)
  END IF

  CALL H5SCLOSE_F(dspace_id, error)
  CALL close_object(obj, obj_id)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_WriteAttribute_d0

!----------------------------------------------------------------------------
!                                                              WriteAttribute
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteAttribute_b0
#ifdef DEBUG_VER
  CHARACTER(LEN=*), PARAMETER :: myName = 'obj_WriteAttribute_b0()'
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ! A logical is stored as the character "T" or "F", as in Write
  IF (attr_val) THEN
    CALL obj%WriteAttribute(obj_name, attr_name, "T")
  ELSE
    CALL obj%WriteAttribute(obj_name, attr_name, "F")
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_WriteAttribute_b0

!----------------------------------------------------------------------------
!                                                              WriteAttribute
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteAttribute_i1
#ifdef DEBUG_VER
  CHARACTER(LEN=*), PARAMETER :: myName = 'obj_WriteAttribute_i1()'
#endif
  INTEGER(I4B) :: error
  INTEGER(HID_T) :: attr_id, dspace_id, obj_id, mem
  INTEGER(HSIZE_T), DIMENSION(1) :: dims

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ! The HDF5 type follows the kind of attr_val
  dims(1) = SIZE(attr_val)
  mem = H5KIND_TO_TYPE(KIND(attr_val), H5_INTEGER_KIND)

  CALL open_object(obj, obj_name, obj_id)
  CALL H5SCREATE_SIMPLE_F(1, dims, dspace_id, error)
  CALL createAttribute(obj, obj_id, attr_name, mem, dspace_id, attr_id)

  IF (attr_id .GT. 0) THEN
    IF (dims(1) .GT. 0) THEN
      CALL H5AWRITE_F(attr_id, mem, attr_val, dims, error)
    END IF

#ifdef DEBUG_VER
    CALL AssertError1(error .EQ. 0, myName, &
                      'Failed to write attribute.')
#endif

    CALL close_attribute(obj, attr_id)
  END IF

  CALL H5SCLOSE_F(dspace_id, error)
  CALL close_object(obj, obj_id)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_WriteAttribute_i1

!----------------------------------------------------------------------------
!                                                              WriteAttribute
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteAttribute_d1
#ifdef DEBUG_VER
  CHARACTER(LEN=*), PARAMETER :: myName = 'obj_WriteAttribute_d1()'
#endif
  INTEGER(I4B) :: error
  INTEGER(HID_T) :: attr_id, dspace_id, obj_id, mem
  INTEGER(HSIZE_T), DIMENSION(1) :: dims

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ! The HDF5 type follows the kind of attr_val
  dims(1) = SIZE(attr_val)
  mem = H5KIND_TO_TYPE(KIND(attr_val), H5_REAL_KIND)

  CALL open_object(obj, obj_name, obj_id)
  CALL H5SCREATE_SIMPLE_F(1, dims, dspace_id, error)
  CALL createAttribute(obj, obj_id, attr_name, mem, dspace_id, attr_id)

  IF (attr_id .GT. 0) THEN
    IF (dims(1) .GT. 0) THEN
      CALL H5AWRITE_F(attr_id, mem, attr_val, dims, error)
    END IF

#ifdef DEBUG_VER
    CALL AssertError1(error .EQ. 0, myName, &
                      'Failed to write attribute.')
#endif

    CALL close_attribute(obj, attr_id)
  END IF

  CALL H5SCLOSE_F(dspace_id, error)
  CALL close_object(obj, obj_id)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_WriteAttribute_d1

!----------------------------------------------------------------------------
!                                                              WriteAttribute
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteAttribute_b1
#ifdef DEBUG_VER
  CHARACTER(LEN=*), PARAMETER :: myName = 'obj_WriteAttribute_b1()'
#endif
  INTEGER(I4B) :: error, ii
  INTEGER(HID_T) :: attr_id, dspace_id, obj_id
  INTEGER(HSIZE_T), DIMENSION(1) :: dims
  CHARACTER(LEN=1) :: buf(SIZE(attr_val))

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ! A logical is stored as the character "T" or "F", as in Write
  DO ii = 1, SIZE(attr_val)
    buf(ii) = 'F'
    IF (attr_val(ii)) buf(ii) = 'T'
  END DO
  dims(1) = SIZE(attr_val)

  CALL open_object(obj, obj_name, obj_id)
  CALL H5SCREATE_SIMPLE_F(1, dims, dspace_id, error)
  CALL createAttribute(obj, obj_id, attr_name, H5T_NATIVE_CHARACTER, &
                       dspace_id, attr_id)

  IF (attr_id .GT. 0) THEN
    IF (dims(1) .GT. 0) THEN
      CALL H5AWRITE_F(attr_id, H5T_NATIVE_CHARACTER, buf, dims, error)
    END IF

#ifdef DEBUG_VER
    CALL AssertError1(error .EQ. 0, myName, &
                      'Failed to write attribute.')
#endif

    CALL close_attribute(obj, attr_id)
  END IF

  CALL H5SCLOSE_F(dspace_id, error)
  CALL close_object(obj, obj_id)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_WriteAttribute_b1

!----------------------------------------------------------------------------
!                                                              WriteAttribute
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteAttribute_st1
#ifdef DEBUG_VER
  CHARACTER(LEN=*), PARAMETER :: myName = 'obj_WriteAttribute_st1()'
#endif
  INTEGER(I4B) :: error, ii, lmax
  INTEGER(HID_T) :: atype_id, attr_id, dspace_id, obj_id
  INTEGER(HSIZE_T), DIMENSION(1) :: dims
  CHARACTER(LEN=:), ALLOCATABLE :: buf(:)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ! Fixed-length strings of the longest length (at least 1, since a type
  ! of size 0 is not allowed); shorter strings are padded with blanks
  lmax = 1
  DO ii = 1, SIZE(attr_val)
    lmax = MAX(lmax, attr_val(ii)%LEN())
  END DO
  ALLOCATE(CHARACTER(LEN=lmax) :: buf(SIZE(attr_val)))
  DO ii = 1, SIZE(attr_val)
    buf(ii) = attr_val(ii)%chars()
  END DO
  dims(1) = SIZE(attr_val)

  CALL open_object(obj, obj_name, obj_id)
  CALL H5SCREATE_SIMPLE_F(1, dims, dspace_id, error)
  CALL H5TCOPY_F(H5T_NATIVE_CHARACTER, atype_id, error)
  CALL H5TSET_SIZE_F(atype_id, INT(lmax, SIZE_T), error)
  CALL createAttribute(obj, obj_id, attr_name, atype_id, dspace_id, attr_id)

  IF (attr_id .GT. 0) THEN
    IF (dims(1) .GT. 0) THEN
      CALL H5AWRITE_F(attr_id, atype_id, buf, dims, error)
    END IF

#ifdef DEBUG_VER
    CALL AssertError1(error .EQ. 0, myName, &
                      'Failed to write attribute.')
#endif

    CALL close_attribute(obj, attr_id)
  END IF

  CALL H5TCLOSE_F(atype_id, error)
  CALL H5SCLOSE_F(dspace_id, error)
  CALL close_object(obj, obj_id)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_WriteAttribute_st1

!----------------------------------------------------------------------------
!                                                              WriteAttribute
!----------------------------------------------------------------------------

#ifdef USE_Real64
MODULE PROCEDURE obj_WriteAttribute_s0
#ifdef DEBUG_VER
  CHARACTER(LEN=*), PARAMETER :: myName = 'obj_WriteAttribute_s0()'
#endif
  INTEGER(I4B) :: error
  INTEGER(HID_T) :: attr_id, dspace_id, obj_id, mem
  INTEGER(HSIZE_T), DIMENSION(1) :: dims

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ! The HDF5 type follows the kind of attr_val
  dims(1) = 1
  mem = H5KIND_TO_TYPE(KIND(attr_val), H5_REAL_KIND)

  CALL open_object(obj, obj_name, obj_id)
  CALL H5SCREATE_SIMPLE_F(1, dims, dspace_id, error)
  CALL createAttribute(obj, obj_id, attr_name, mem, dspace_id, attr_id)

  IF (attr_id .GT. 0) THEN
    IF (dims(1) .GT. 0) THEN
      CALL H5AWRITE_F(attr_id, mem, attr_val, dims, error)
    END IF

#ifdef DEBUG_VER
    CALL AssertError1(error .EQ. 0, myName, &
                      'Failed to write attribute.')
#endif

    CALL close_attribute(obj, attr_id)
  END IF

  CALL H5SCLOSE_F(dspace_id, error)
  CALL close_object(obj, obj_id)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_WriteAttribute_s0

!----------------------------------------------------------------------------
!                                                              WriteAttribute
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteAttribute_s1
#ifdef DEBUG_VER
  CHARACTER(LEN=*), PARAMETER :: myName = 'obj_WriteAttribute_s1()'
#endif
  INTEGER(I4B) :: error
  INTEGER(HID_T) :: attr_id, dspace_id, obj_id, mem
  INTEGER(HSIZE_T), DIMENSION(1) :: dims

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ! The HDF5 type follows the kind of attr_val
  dims(1) = SIZE(attr_val)
  mem = H5KIND_TO_TYPE(KIND(attr_val), H5_REAL_KIND)

  CALL open_object(obj, obj_name, obj_id)
  CALL H5SCREATE_SIMPLE_F(1, dims, dspace_id, error)
  CALL createAttribute(obj, obj_id, attr_name, mem, dspace_id, attr_id)

  IF (attr_id .GT. 0) THEN
    IF (dims(1) .GT. 0) THEN
      CALL H5AWRITE_F(attr_id, mem, attr_val, dims, error)
    END IF

#ifdef DEBUG_VER
    CALL AssertError1(error .EQ. 0, myName, &
                      'Failed to write attribute.')
#endif

    CALL close_attribute(obj, attr_id)
  END IF

  CALL H5SCLOSE_F(dspace_id, error)
  CALL close_object(obj, obj_id)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_WriteAttribute_s1
#endif

!----------------------------------------------------------------------------
!                                                              WriteAttribute
!----------------------------------------------------------------------------

#ifndef USE_Int64
MODULE PROCEDURE obj_WriteAttribute_l0
#ifdef DEBUG_VER
  CHARACTER(LEN=*), PARAMETER :: myName = 'obj_WriteAttribute_l0()'
#endif
  INTEGER(I4B) :: error
  INTEGER(HID_T) :: attr_id, dspace_id, obj_id, mem
  INTEGER(HSIZE_T), DIMENSION(1) :: dims

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ! The HDF5 type follows the kind of attr_val
  dims(1) = 1
  mem = H5KIND_TO_TYPE(KIND(attr_val), H5_INTEGER_KIND)

  CALL open_object(obj, obj_name, obj_id)
  CALL H5SCREATE_SIMPLE_F(1, dims, dspace_id, error)
  CALL createAttribute(obj, obj_id, attr_name, mem, dspace_id, attr_id)

  IF (attr_id .GT. 0) THEN
    IF (dims(1) .GT. 0) THEN
      CALL H5AWRITE_F(attr_id, mem, attr_val, dims, error)
    END IF

#ifdef DEBUG_VER
    CALL AssertError1(error .EQ. 0, myName, &
                      'Failed to write attribute.')
#endif

    CALL close_attribute(obj, attr_id)
  END IF

  CALL H5SCLOSE_F(dspace_id, error)
  CALL close_object(obj, obj_id)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_WriteAttribute_l0

!----------------------------------------------------------------------------
!                                                              WriteAttribute
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_WriteAttribute_l1
#ifdef DEBUG_VER
  CHARACTER(LEN=*), PARAMETER :: myName = 'obj_WriteAttribute_l1()'
#endif
  INTEGER(I4B) :: error
  INTEGER(HID_T) :: attr_id, dspace_id, obj_id, mem
  INTEGER(HSIZE_T), DIMENSION(1) :: dims

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ! The HDF5 type follows the kind of attr_val
  dims(1) = SIZE(attr_val)
  mem = H5KIND_TO_TYPE(KIND(attr_val), H5_INTEGER_KIND)

  CALL open_object(obj, obj_name, obj_id)
  CALL H5SCREATE_SIMPLE_F(1, dims, dspace_id, error)
  CALL createAttribute(obj, obj_id, attr_name, mem, dspace_id, attr_id)

  IF (attr_id .GT. 0) THEN
    IF (dims(1) .GT. 0) THEN
      CALL H5AWRITE_F(attr_id, mem, attr_val, dims, error)
    END IF

#ifdef DEBUG_VER
    CALL AssertError1(error .EQ. 0, myName, &
                      'Failed to write attribute.')
#endif

    CALL close_attribute(obj, attr_id)
  END IF

  CALL H5SCLOSE_F(dspace_id, error)
  CALL close_object(obj, obj_id)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_WriteAttribute_l1
#endif

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE WriteAttribute
