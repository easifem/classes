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
USE HDF5, ONLY: SIZE_T
USE HDF5, ONLY: H5AGET_TYPE_F
USE HDF5, ONLY: H5AREAD_F
USE HDF5, ONLY: H5AGET_STORAGE_SIZE_F
USE HDF5, ONLY: H5TCLOSE_F
USE ISO_C_BINDING, ONLY: C_CHAR
USE ISO_C_BINDING, ONLY: C_NULL_CHAR
USE HDF5, ONLY: H5AEXISTS_BY_NAME_F
USE HDF5, ONLY: H5KIND_TO_TYPE
USE HDF5, ONLY: H5_INTEGER_KIND
USE HDF5, ONLY: H5_REAL_KIND
USE HDF5, ONLY: H5AGET_SPACE_F
USE HDF5, ONLY: H5SCLOSE_F
USE HDF5, ONLY: H5SGET_SIMPLE_EXTENT_NPOINTS_F
USE HDF5, ONLY: H5TGET_SIZE_F
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "HDF5File_Class@ReadAttribute.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                              ReadAttribute
!----------------------------------------------------------------------------

SUBROUTINE obj_ReadAttribute_st0_helper(attr_id, length_max, attr_val)
  TYPE(String), INTENT(INOUT) :: attr_val
  INTEGER(SIZE_T), INTENT(IN) :: length_max
  INTEGER(HID_T), INTENT(IN) :: attr_id
  ! Define internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_ReadAttribute_st0_helper()"
#endif
  INTEGER(HID_T) :: atype_id
  INTEGER(HSIZE_T), DIMENSION(1) :: dims
  CHARACTER(LEN=length_max, KIND=C_CHAR), TARGET :: buf
  INTEGER(I4B) :: error

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  dims(1) = 1
  CALL H5AGET_TYPE_F(attr_id, atype_id, error)
  CALL H5AREAD_F(attr_id, atype_id, buf, dims, error)
  attr_val = buf
  ! An empty String is stored as one NUL character
  attr_val = attr_val%Replace(C_NULL_CHAR, '')

  CALL H5TCLOSE_F(atype_id, error)
#ifdef DEBUG_VER
  CALL AssertError1(error .EQ. 0, myName, 'Failed to close datatype.')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END SUBROUTINE obj_ReadAttribute_st0_helper

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ReadAttribute_st0
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_ReadAttribute_st0()"
#endif
  INTEGER(HID_T) :: attr_id, obj_id
  INTEGER(SIZE_T) :: max_size
  INTEGER(I4B) :: error

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

!Prepare the File and object for the attribute
  CALL open_object(obj, obj_name, obj_id)
  CALL open_attribute(obj, obj_id, attr_name, attr_id)
  CALL H5AGET_STORAGE_SIZE_F(attr_id, max_size, error)
  CALL obj_ReadAttribute_st0_helper(attr_id, max_size, attr_val)
  CALL close_attribute(obj, attr_id)
  CALL close_object(obj, obj_id)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_ReadAttribute_st0

!----------------------------------------------------------------------------
!                                                           AttributeExists
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AttributeExists
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_AttributeExists()"
#endif
  LOGICAL :: isok
  INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ans = math%no

  isok = obj%isInit .AND. obj%IsOpen()

  ! The object must exist; the root group "/" always exists
  IF (isok .AND. TRIM(obj_name) .NE. "/") isok = obj%PathExists(obj_name)

  IF (isok) THEN
    CALL H5AEXISTS_BY_NAME_F(obj%file_id, obj_name, attr_name, isok, ierr)
    ans = isok .AND. (ierr .EQ. 0)
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_AttributeExists

!----------------------------------------------------------------------------
!                                                             ReadAttribute
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ReadAttribute_c0
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_ReadAttribute_c0()"
#endif
  TYPE(String) :: str_val

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  CALL obj%ReadAttribute(obj_name, attr_name, str_val)
  attr_val = str_val%Chars()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_ReadAttribute_c0

!----------------------------------------------------------------------------
!                                                             ReadAttribute
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ReadAttribute_i0
#ifdef DEBUG_VER
  CHARACTER(LEN=*), PARAMETER :: myName = 'obj_ReadAttribute_i0()'
#endif
  INTEGER(HID_T) :: attr_id, obj_id, mem
  INTEGER(HSIZE_T), DIMENSION(1) :: dims
  INTEGER(I4B) :: error

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  error = 0
  CALL open_object(obj, obj_name, obj_id)
  CALL open_attribute(obj, obj_id, attr_name, attr_id)

  ! A scalar is read only from an attribute with one value
  dims(1) = GetAttributeSize(attr_id)

#ifdef DEBUG_VER
  CALL AssertError1(dims(1) .EQ. 1, myName, &
                    'The attribute ' // attr_name // ' is not a scalar.')
#endif

  IF (dims(1) .NE. 1) dims(1) = 0

  ! The HDF5 type follows the kind of attr_val; HDF5 converts the stored
  ! type to it
  mem = H5KIND_TO_TYPE(KIND(attr_val), H5_INTEGER_KIND)
  IF (dims(1) .GT. 0) CALL H5AREAD_F(attr_id, mem, attr_val, dims, error)

#ifdef DEBUG_VER
  CALL AssertError1(error .EQ. 0, myName, &
                    'Failed to read attribute.')
#endif

  CALL close_attribute(obj, attr_id)
  CALL close_object(obj, obj_id)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_ReadAttribute_i0

!----------------------------------------------------------------------------
!                                                             ReadAttribute
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ReadAttribute_d0
#ifdef DEBUG_VER
  CHARACTER(LEN=*), PARAMETER :: myName = 'obj_ReadAttribute_d0()'
#endif
  INTEGER(HID_T) :: attr_id, obj_id, mem
  INTEGER(HSIZE_T), DIMENSION(1) :: dims
  INTEGER(I4B) :: error

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  error = 0
  CALL open_object(obj, obj_name, obj_id)
  CALL open_attribute(obj, obj_id, attr_name, attr_id)

  ! A scalar is read only from an attribute with one value
  dims(1) = GetAttributeSize(attr_id)

#ifdef DEBUG_VER
  CALL AssertError1(dims(1) .EQ. 1, myName, &
                    'The attribute ' // attr_name // ' is not a scalar.')
#endif

  IF (dims(1) .NE. 1) dims(1) = 0

  ! The HDF5 type follows the kind of attr_val; HDF5 converts the stored
  ! type to it
  mem = H5KIND_TO_TYPE(KIND(attr_val), H5_REAL_KIND)
  IF (dims(1) .GT. 0) CALL H5AREAD_F(attr_id, mem, attr_val, dims, error)

#ifdef DEBUG_VER
  CALL AssertError1(error .EQ. 0, myName, &
                    'Failed to read attribute.')
#endif

  CALL close_attribute(obj, attr_id)
  CALL close_object(obj, obj_id)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_ReadAttribute_d0

!----------------------------------------------------------------------------
!                                                               ReadAttribute
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ReadAttribute_b0
#ifdef DEBUG_VER
  CHARACTER(LEN=*), PARAMETER :: myName = 'obj_ReadAttribute_b0()'
#endif
  INTEGER(HID_T) :: attr_id, obj_id, atype_id, mem
  INTEGER(HSIZE_T), DIMENSION(1) :: dims
  INTEGER(I4B) :: error
  LOGICAL(LGT) :: isChar
  INTEGER(INT8) :: bvals

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  error = 0
  CALL open_object(obj, obj_name, obj_id)
  CALL open_attribute(obj, obj_id, attr_name, attr_id)

  ! A scalar is read only from an attribute with one value
  dims(1) = GetAttributeSize(attr_id)

#ifdef DEBUG_VER
  CALL AssertError1(dims(1) .EQ. 1, myName, &
                    'The attribute ' // attr_name // ' is not a scalar.')
#endif

  ! Read the raw 8-bit values: the enum {FALSE, TRUE}, an integer, or
  ! the characters "T"/"F" of older files
  CALL H5AGET_TYPE_F(attr_id, atype_id, error)
  CALL BoolMemType(atype_id, mem, isChar)
  IF (dims(1) .EQ. 1) CALL H5AREAD_F(attr_id, mem, bvals, dims, error)

#ifdef DEBUG_VER
  CALL AssertError1(error .EQ. 0, myName, &
                    'Failed to read attribute.')
#endif

  CALL H5TCLOSE_F(atype_id, error)
  CALL close_attribute(obj, attr_id)
  CALL close_object(obj, obj_id)

  IF (dims(1) .EQ. 1) attr_val = BoolValue(bvals, isChar)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_ReadAttribute_b0

!----------------------------------------------------------------------------
!                                                               ReadAttribute
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ReadAttribute_i1
#ifdef DEBUG_VER
  CHARACTER(LEN=*), PARAMETER :: myName = 'obj_ReadAttribute_i1()'
#endif
  INTEGER(HID_T) :: attr_id, obj_id, mem
  INTEGER(HSIZE_T), DIMENSION(1) :: dims
  INTEGER(I4B) :: error

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  error = 0
  CALL open_object(obj, obj_name, obj_id)
  CALL open_attribute(obj, obj_id, attr_name, attr_id)

  dims(1) = GetAttributeSize(attr_id)
  IF (ALLOCATED(attr_val)) THEN
    IF (SIZE(attr_val) .NE. dims(1)) DEALLOCATE(attr_val)
  END IF
  IF (.NOT. ALLOCATED(attr_val)) ALLOCATE(attr_val(dims(1)))

  ! The HDF5 type follows the kind of attr_val; HDF5 converts the stored
  ! type to it
  mem = H5KIND_TO_TYPE(KIND(attr_val), H5_INTEGER_KIND)
  IF (dims(1) .GT. 0) CALL H5AREAD_F(attr_id, mem, attr_val, dims, error)

#ifdef DEBUG_VER
  CALL AssertError1(error .EQ. 0, myName, &
                    'Failed to read attribute.')
#endif

  CALL close_attribute(obj, attr_id)
  CALL close_object(obj, obj_id)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_ReadAttribute_i1

!----------------------------------------------------------------------------
!                                                               ReadAttribute
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ReadAttribute_d1
#ifdef DEBUG_VER
  CHARACTER(LEN=*), PARAMETER :: myName = 'obj_ReadAttribute_d1()'
#endif
  INTEGER(HID_T) :: attr_id, obj_id, mem
  INTEGER(HSIZE_T), DIMENSION(1) :: dims
  INTEGER(I4B) :: error

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  error = 0
  CALL open_object(obj, obj_name, obj_id)
  CALL open_attribute(obj, obj_id, attr_name, attr_id)

  dims(1) = GetAttributeSize(attr_id)
  IF (ALLOCATED(attr_val)) THEN
    IF (SIZE(attr_val) .NE. dims(1)) DEALLOCATE(attr_val)
  END IF
  IF (.NOT. ALLOCATED(attr_val)) ALLOCATE(attr_val(dims(1)))

  ! The HDF5 type follows the kind of attr_val; HDF5 converts the stored
  ! type to it
  mem = H5KIND_TO_TYPE(KIND(attr_val), H5_REAL_KIND)
  IF (dims(1) .GT. 0) CALL H5AREAD_F(attr_id, mem, attr_val, dims, error)

#ifdef DEBUG_VER
  CALL AssertError1(error .EQ. 0, myName, &
                    'Failed to read attribute.')
#endif

  CALL close_attribute(obj, attr_id)
  CALL close_object(obj, obj_id)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_ReadAttribute_d1

!----------------------------------------------------------------------------
!                                                               ReadAttribute
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ReadAttribute_b1
#ifdef DEBUG_VER
  CHARACTER(LEN=*), PARAMETER :: myName = 'obj_ReadAttribute_b1()'
#endif
  INTEGER(HID_T) :: attr_id, obj_id, atype_id, mem
  INTEGER(HSIZE_T), DIMENSION(1) :: dims
  INTEGER(I4B) :: error
  LOGICAL(LGT) :: isChar
  INTEGER(INT8), ALLOCATABLE :: bvals(:)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  error = 0
  CALL open_object(obj, obj_name, obj_id)
  CALL open_attribute(obj, obj_id, attr_name, attr_id)

  dims(1) = GetAttributeSize(attr_id)
  ALLOCATE(bvals(dims(1)))

  ! Read the raw 8-bit values: the enum {FALSE, TRUE}, an integer, or
  ! the characters "T"/"F" of older files
  CALL H5AGET_TYPE_F(attr_id, atype_id, error)
  CALL BoolMemType(atype_id, mem, isChar)
  IF (dims(1) .GT. 0) CALL H5AREAD_F(attr_id, mem, bvals, dims, error)

#ifdef DEBUG_VER
  CALL AssertError1(error .EQ. 0, myName, &
                    'Failed to read attribute.')
#endif

  CALL H5TCLOSE_F(atype_id, error)
  CALL close_attribute(obj, attr_id)
  CALL close_object(obj, obj_id)

  IF (ALLOCATED(attr_val)) THEN
    IF (SIZE(attr_val) .NE. dims(1)) DEALLOCATE(attr_val)
  END IF
  IF (.NOT. ALLOCATED(attr_val)) ALLOCATE(attr_val(dims(1)))
  attr_val = BoolValue(bvals, isChar)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_ReadAttribute_b1

!----------------------------------------------------------------------------
!                                                               ReadAttribute
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ReadAttribute_st1
#ifdef DEBUG_VER
  CHARACTER(LEN=*), PARAMETER :: myName = 'obj_ReadAttribute_st1()'
#endif
  INTEGER(HID_T) :: attr_id, obj_id, atype_id
  INTEGER(HSIZE_T), DIMENSION(1) :: dims
  INTEGER(SIZE_T) :: lsize
  INTEGER(I4B) :: error, ii
  CHARACTER(LEN=:), ALLOCATABLE :: buf(:)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  error = 0
  CALL open_object(obj, obj_name, obj_id)
  CALL open_attribute(obj, obj_id, attr_name, attr_id)

  ! Number of strings and length of a string
  dims(1) = GetAttributeSize(attr_id)
  CALL H5AGET_TYPE_F(attr_id, atype_id, error)
  CALL H5TGET_SIZE_F(atype_id, lsize, error)
  ALLOCATE(CHARACTER(LEN=lsize) :: buf(dims(1)))

  IF (dims(1) .GT. 0) CALL H5AREAD_F(attr_id, atype_id, buf, dims, error)

#ifdef DEBUG_VER
  CALL AssertError1(error .EQ. 0, myName, &
                    'Failed to read attribute.')
#endif

  CALL H5TCLOSE_F(atype_id, error)
  CALL close_attribute(obj, attr_id)
  CALL close_object(obj, obj_id)

  ! The padding (blanks or NUL characters) is removed
  IF (ALLOCATED(attr_val)) THEN
    IF (SIZE(attr_val) .NE. dims(1)) DEALLOCATE(attr_val)
  END IF
  IF (.NOT. ALLOCATED(attr_val)) ALLOCATE(attr_val(dims(1)))
  DO ii = 1, SIZE(attr_val)
    attr_val(ii) = TRIM(buf(ii))
    attr_val(ii) = attr_val(ii)%Replace(C_NULL_CHAR, '')
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_ReadAttribute_st1

!----------------------------------------------------------------------------
!                                                               ReadAttribute
!----------------------------------------------------------------------------

#ifdef USE_Real64
MODULE PROCEDURE obj_ReadAttribute_s0
#ifdef DEBUG_VER
  CHARACTER(LEN=*), PARAMETER :: myName = 'obj_ReadAttribute_s0()'
#endif
  INTEGER(HID_T) :: attr_id, obj_id, mem
  INTEGER(HSIZE_T), DIMENSION(1) :: dims
  INTEGER(I4B) :: error

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  error = 0
  CALL open_object(obj, obj_name, obj_id)
  CALL open_attribute(obj, obj_id, attr_name, attr_id)

  ! A scalar is read only from an attribute with one value
  dims(1) = GetAttributeSize(attr_id)

#ifdef DEBUG_VER
  CALL AssertError1(dims(1) .EQ. 1, myName, &
                    'The attribute ' // attr_name // ' is not a scalar.')
#endif

  IF (dims(1) .NE. 1) dims(1) = 0

  ! The HDF5 type follows the kind of attr_val; HDF5 converts the stored
  ! type to it
  mem = H5KIND_TO_TYPE(KIND(attr_val), H5_REAL_KIND)
  IF (dims(1) .GT. 0) CALL H5AREAD_F(attr_id, mem, attr_val, dims, error)

#ifdef DEBUG_VER
  CALL AssertError1(error .EQ. 0, myName, &
                    'Failed to read attribute.')
#endif

  CALL close_attribute(obj, attr_id)
  CALL close_object(obj, obj_id)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_ReadAttribute_s0

!----------------------------------------------------------------------------
!                                                               ReadAttribute
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ReadAttribute_s1
#ifdef DEBUG_VER
  CHARACTER(LEN=*), PARAMETER :: myName = 'obj_ReadAttribute_s1()'
#endif
  INTEGER(HID_T) :: attr_id, obj_id, mem
  INTEGER(HSIZE_T), DIMENSION(1) :: dims
  INTEGER(I4B) :: error

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  error = 0
  CALL open_object(obj, obj_name, obj_id)
  CALL open_attribute(obj, obj_id, attr_name, attr_id)

  dims(1) = GetAttributeSize(attr_id)
  IF (ALLOCATED(attr_val)) THEN
    IF (SIZE(attr_val) .NE. dims(1)) DEALLOCATE(attr_val)
  END IF
  IF (.NOT. ALLOCATED(attr_val)) ALLOCATE(attr_val(dims(1)))

  ! The HDF5 type follows the kind of attr_val; HDF5 converts the stored
  ! type to it
  mem = H5KIND_TO_TYPE(KIND(attr_val), H5_REAL_KIND)
  IF (dims(1) .GT. 0) CALL H5AREAD_F(attr_id, mem, attr_val, dims, error)

#ifdef DEBUG_VER
  CALL AssertError1(error .EQ. 0, myName, &
                    'Failed to read attribute.')
#endif

  CALL close_attribute(obj, attr_id)
  CALL close_object(obj, obj_id)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_ReadAttribute_s1
#endif

!----------------------------------------------------------------------------
!                                                               ReadAttribute
!----------------------------------------------------------------------------

#ifndef USE_Int64
MODULE PROCEDURE obj_ReadAttribute_l0
#ifdef DEBUG_VER
  CHARACTER(LEN=*), PARAMETER :: myName = 'obj_ReadAttribute_l0()'
#endif
  INTEGER(HID_T) :: attr_id, obj_id, mem
  INTEGER(HSIZE_T), DIMENSION(1) :: dims
  INTEGER(I4B) :: error

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  error = 0
  CALL open_object(obj, obj_name, obj_id)
  CALL open_attribute(obj, obj_id, attr_name, attr_id)

  ! A scalar is read only from an attribute with one value
  dims(1) = GetAttributeSize(attr_id)

#ifdef DEBUG_VER
  CALL AssertError1(dims(1) .EQ. 1, myName, &
                    'The attribute ' // attr_name // ' is not a scalar.')
#endif

  IF (dims(1) .NE. 1) dims(1) = 0

  ! The HDF5 type follows the kind of attr_val; HDF5 converts the stored
  ! type to it
  mem = H5KIND_TO_TYPE(KIND(attr_val), H5_INTEGER_KIND)
  IF (dims(1) .GT. 0) CALL H5AREAD_F(attr_id, mem, attr_val, dims, error)

#ifdef DEBUG_VER
  CALL AssertError1(error .EQ. 0, myName, &
                    'Failed to read attribute.')
#endif

  CALL close_attribute(obj, attr_id)
  CALL close_object(obj, obj_id)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_ReadAttribute_l0

!----------------------------------------------------------------------------
!                                                               ReadAttribute
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ReadAttribute_l1
#ifdef DEBUG_VER
  CHARACTER(LEN=*), PARAMETER :: myName = 'obj_ReadAttribute_l1()'
#endif
  INTEGER(HID_T) :: attr_id, obj_id, mem
  INTEGER(HSIZE_T), DIMENSION(1) :: dims
  INTEGER(I4B) :: error

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  error = 0
  CALL open_object(obj, obj_name, obj_id)
  CALL open_attribute(obj, obj_id, attr_name, attr_id)

  dims(1) = GetAttributeSize(attr_id)
  IF (ALLOCATED(attr_val)) THEN
    IF (SIZE(attr_val) .NE. dims(1)) DEALLOCATE(attr_val)
  END IF
  IF (.NOT. ALLOCATED(attr_val)) ALLOCATE(attr_val(dims(1)))

  ! The HDF5 type follows the kind of attr_val; HDF5 converts the stored
  ! type to it
  mem = H5KIND_TO_TYPE(KIND(attr_val), H5_INTEGER_KIND)
  IF (dims(1) .GT. 0) CALL H5AREAD_F(attr_id, mem, attr_val, dims, error)

#ifdef DEBUG_VER
  CALL AssertError1(error .EQ. 0, myName, &
                    'Failed to read attribute.')
#endif

  CALL close_attribute(obj, attr_id)
  CALL close_object(obj, obj_id)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_ReadAttribute_l1
#endif

!----------------------------------------------------------------------------
!                                                            GetAttributeSize
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! summary: Number of values of an attribute

FUNCTION GetAttributeSize(attr_id) RESULT(ans)
  INTEGER(HID_T), INTENT(IN) :: attr_id
  !! id of the attribute
  INTEGER(HSIZE_T) :: ans
  !! number of values; 1 for a scalar
  INTEGER(HID_T) :: space_id
  INTEGER(I4B) :: error

  CALL H5AGET_SPACE_F(attr_id, space_id, error)
  CALL H5SGET_SIMPLE_EXTENT_NPOINTS_F(space_id, ans, error)
  CALL H5SCLOSE_F(space_id, error)
  IF (error .NE. 0) ans = 0
END FUNCTION GetAttributeSize

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ReadAttribute
