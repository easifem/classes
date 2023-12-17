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

SUBMODULE(ScalarFieldLis_Class) GetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             GetSingle
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSingle
#include "lisf.h"
INTEGER(I4B) :: ierr
IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  CALL lis_vector_get_value(obj%lis_ptr, 1, VALUE, ierr)
  CALL CHKERR(ierr)
ELSE
  CALL lis_vector_get_value(obj%lis_ptr, indx, VALUE, ierr)
  CALL CHKERR(ierr)
END IF
END PROCEDURE obj_GetSingle

!----------------------------------------------------------------------------
!                                                               GetPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_getPointer
CHARACTER(*), PARAMETER :: myName = "obj_getPointer"
CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'This method is not available for ScalarFieldLis_')
END PROCEDURE obj_getPointer

!----------------------------------------------------------------------------
!                                                                   get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_get1
CHARACTER(*), PARAMETER :: myName = "obj_get1"
INTEGER(I4B) :: localNode
INTEGER(I4B) :: tSize
INTEGER(I4B) :: ierr

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either ScalarField object is not initiated'// &
  & " or, lis_ptr is not available")
END IF

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN

  CALL lis_vector_get_value(obj%lis_ptr, 1, VALUE, ierr)
  CALL CHKERR(ierr)

ELSE

  localNode = obj%domain%getLocalNodeNumber(globalNode)
  tSize = obj%SIZE()

  IF (localNode .EQ. 0_I4B .OR. localNode .GT. tSize) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'localNode is either 0 or greater than size of '// &
      & " scalarFieldLis_::obj")
  END IF

  CALL lis_vector_get_value(obj%lis_ptr, localNode, VALUE, ierr)
  CALL CHKERR(ierr)

END IF

END PROCEDURE obj_get1

!----------------------------------------------------------------------------
!                                                                   get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_get2
#include "lisf.h"
CHARACTER(*), PARAMETER :: myName = "obj_get1"
INTEGER(I4B) :: ierr
INTEGER(I4B) :: tSize
INTEGER(I4B) :: ii

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either ScalarField object is not initiated'// &
  & " or, lis_ptr is not available")
END IF

tSize = obj%SIZE()
CALL Reallocate(VALUE, tSize)

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN

  CALL lis_vector_get_value(obj%lis_ptr, 1, VALUE(1), ierr)
  CALL CHKERR(ierr)
  DO ii = 2, tSize
    VALUE(ii) = VALUE(1)
  END DO

ELSE

  CALL lis_vector_gather(obj%lis_ptr, VALUE, ierr)
  CALL CHKERR(ierr)

END IF

END PROCEDURE obj_get2

!----------------------------------------------------------------------------
!                                                                   get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_get3
#include "lisf.h"
CHARACTER(*), PARAMETER :: myName = "obj_get3"
INTEGER(I4B) :: ierr
INTEGER(I4B) :: localNode(SIZE(globalNode))
INTEGER(I4B) :: tSize
INTEGER(I4B) :: n
INTEGER(I4B) :: ii

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either ScalarField object is not initiated'// &
  & " or, lis_ptr is not available")
END IF

tSize = obj%SIZE()
n = SIZE(globalNode)
CALL Reallocate(VALUE, n)

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN

  CALL lis_vector_get_value(obj%lis_ptr, 1, VALUE(1), ierr)
  CALL CHKERR(ierr)
  DO ii = 2, n
    VALUE(ii) = VALUE(1)
  END DO

ELSE

  localNode = obj%domain%getLocalNodeNumber(globalNode)

  DO ii = 1, n
    CALL lis_vector_get_value( &
    & obj%lis_ptr, &
    & localNode(ii), &
    & VALUE(ii), &
    & ierr)
    CALL CHKERR(ierr)
  END DO

END IF

END PROCEDURE obj_get3

!----------------------------------------------------------------------------
!                                                                   get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_get4
INTEGER(I4B) :: globalNode(INT(1 + (iend - istart) / stride)), ii, jj
jj = 0
DO ii = istart, iend, stride
  jj = jj + 1
  globalNode(jj) = ii
END DO
CALL obj%Get(globalNode=globalNode, VALUE=VALUE)
END PROCEDURE obj_get4

!----------------------------------------------------------------------------
!                                                                       get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_get5
REAL(DFP), ALLOCATABLE :: value0(:)
CALL obj%Get(VALUE=value0, globalNode=globalNode)
VALUE = NodalVariable( &
  & value0, &
  & TypeFEVariableScalar, &
  & TypeFEVariableSpace)
IF (ALLOCATED(value0)) DEALLOCATE (value0)
END PROCEDURE obj_get5

!----------------------------------------------------------------------------
!                                                                       get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_get6
#include "lisf.h"
CHARACTER(*), PARAMETER :: myName = "obj_get6"
INTEGER(I4B) :: ierr
INTEGER(I4B) :: size1, size2
REAL(DFP), POINTER :: realvec(:)

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either ScalarField_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

IF (.NOT. VALUE%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either ScalarFieldLis_::value is not initiated'// &
  & "")
END IF

size1 = obj%SIZE()
size2 = VALUE%SIZE()

IF (size1 .NE. size2) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'Size of obj and value are not same')
END IF

SELECT TYPE (VALUE)
TYPE is (ScalarField_)

  realvec => NULL()
  realvec => VALUE%getPointer()
  CALL lis_vector_gather(obj%lis_ptr, realvec, ierr)
  CALL CHKERR(ierr)
  realvec => NULL()

TYPE is (ScalarFieldLis_)

  CALL lis_vector_copy(obj%lis_ptr, VALUE%lis_ptr, ierr)
  CALL CHKERR(ierr)

CLASS DEFAULT
  CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'No case found for type of value [ScalarField_, ScalarFieldLis_] '// &
  & ' are supported')
END SELECT
END PROCEDURE obj_get6

END SUBMODULE GetMethods
