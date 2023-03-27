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

SUBMODULE(ScalarFieldLis_Class) SetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 SetSingle
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_setSingle
#include "lisf.h"
INTEGER(I4B) :: i, ierr
REAL(DFP) :: value0

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  i = 1
ELSE
  i = indx
END IF

value0 = INPUT(option=scale, default=1.0_DFP) * VALUE

IF (PRESENT(addContribution)) THEN
  CALL lis_vector_set_value( &
    & LIS_ADD_VALUE, &
    & i, &
    & value0, &
    & obj%lis_ptr, &
    & ierr &
    & )
  CALL CHKERR(ierr)
ELSE
  CALL lis_vector_set_value( &
    & LIS_INS_VALUE, &
    & i, &
    & value0, &
    & obj%lis_ptr, &
    & ierr &
    & )
  CALL CHKERR(ierr)
END IF

END PROCEDURE sField_setSingle

!----------------------------------------------------------------------------
!                                                               SetMultiple
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_setMultiple
#include "lisf.h"
INTEGER(I4B) :: i(SIZE(indx)), ierr, n
REAL(DFP) :: value0(SIZE(VALUE))

i = indx
n = SIZE(indx)

value0 = INPUT(option=scale, default=1.0_DFP) * VALUE

IF (PRESENT(addContribution)) THEN
  CALL lis_vector_set_values( &
    & LIS_ADD_VALUE, &
    & n, &
    & i, &
    & value0, &
    & obj%lis_ptr, &
    & ierr &
    & )
  CALL CHKERR(ierr)
ELSE
  CALL lis_vector_set_values( &
    & LIS_INS_VALUE, &
    & n, &
    & i, &
    & value0, &
    & obj%lis_ptr, &
    & ierr &
    & )
  CALL CHKERR(ierr)
END IF
END PROCEDURE sField_setMultiple

!----------------------------------------------------------------------------
!                                                                     SetAll
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_setAll
#include "lisf.h"
INTEGER(I4B) :: ierr, ii, n
REAL(DFP) :: value0

value0 = INPUT(option=scale, default=1.0_DFP) * VALUE

IF (PRESENT(addContribution)) THEN
  n = obj%SIZE()
  DO ii = 1, n
    CALL lis_vector_set_value( &
      & LIS_ADD_VALUE, &
      & ii, &
      & value0, &
      & obj%lis_ptr, &
      & ierr &
      & )
    CALL CHKERR(ierr)
  END DO
ELSE
  CALL lis_vector_set_all( &
    & value0, &
    & obj%lis_ptr, &
    & ierr &
    & )
  CALL CHKERR(ierr)
END IF
END PROCEDURE sField_setAll

!----------------------------------------------------------------------------
!                                                                   set
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_set1
CHARACTER(*), PARAMETER :: myName = "sField_set1"
INTEGER(I4B) :: localNode
INTEGER(I4B) :: ierr

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either ScalarField object is not initiated'// &
  & " or, lis_ptr is not available")
END IF

localNode = obj%domain%getLocalNodeNumber(globalNode)

IF (localNode .NE. 0) THEN
  CALL obj%setSingle( &
    & indx=localNode, &
    & VALUE=VALUE, &
    & scale=scale, &
    & addContribution=addContribution)
END IF

END PROCEDURE sField_set1

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_set2
CHARACTER(*), PARAMETER :: myName = "sField_set2"
INTEGER(I4B) :: ierr

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either ScalarField object is not initiated'// &
  & " or, lis_ptr is not available")
END IF

CALL obj%setAll(&
  & VALUE=VALUE, &
  & scale=scale, &
  & addContribution=addContribution &
  & )

END PROCEDURE sField_set2

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_set3
#include "lisf.h"
CHARACTER(*), PARAMETER :: myName = "sField_set3"
INTEGER(I4B) :: ii
INTEGER(I4B) :: ierr
INTEGER(I4B) :: tsize
REAL(DFP) :: scale0
REAL(DFP) :: value0

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either ScalarField object is not initiated'// &
  & " or, lis_ptr is not available")
END IF

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'This routine should not be called for constant field type.')
END IF

tsize = obj%SIZE()

IF (tSize .NE. SIZE(VALUE)) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Size of value is not equal to size of scalarfield')
END IF

IF (PRESENT(addContribution)) THEN
  scale0 = input(option=scale, default=1.0_DFP)
  DO ii = 1, tSize
    value0 = VALUE(ii) * scale0
    CALL lis_vector_set_value( &
      & LIS_ADD_VALUE, &
      & ii, &
      & value0, &
      & obj%lis_ptr, &
      & ierr &
      & )
    CALL CHKERR(ierr)
  END DO
ELSE
  DO ii = 1, tSize
    value0 = VALUE(ii)
    CALL lis_vector_set_value( &
      & LIS_INS_VALUE, &
      & ii, &
      & value0, &
      & obj%lis_ptr, &
      & ierr &
      & )
    CALL CHKERR(ierr)
  END DO
END IF

END PROCEDURE sField_set3

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_set4
#include "lisf.h"
CHARACTER(*), PARAMETER :: myName = "sField_set4"
INTEGER(I4B) :: localNode(SIZE(globalNode))
REAL(DFP) :: value0(SIZE(globalNode))
INTEGER(I4B) :: ierr
INTEGER(I4B) :: tsize

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either ScalarField object is not initiated'// &
  & " or, lis_ptr is not available")
END IF

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'This routine should not be called for constant field type.')
END IF

localNode = obj%domain%getLocalNodeNumber(globalNode)

tsize = obj%SIZE()

IF (ANY(localNode .GT. tSize)) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'Some of the globalNode are out of bound')
END IF

value0 = VALUE

CALL obj%SetMultiple(&
  & indx=localNode, &
  & VALUE=VALUE0, &
  & scale=scale, &
  & addContribution=addContribution &
  & )

END PROCEDURE sField_set4

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_set5
CHARACTER(*), PARAMETER :: myName = "sField_set5"
INTEGER(I4B) :: localNode(SIZE(globalNode))
INTEGER(I4B) :: ierr
INTEGER(I4B) :: tsize

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either ScalarField object is not initiated'// &
  & " or, lis_ptr is not available")
END IF

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'This routine should not be called for constant field type.')
END IF

localNode = obj%domain%getLocalNodeNumber(globalNode)

tsize = obj%SIZE()

IF (ANY(localNode .GT. tSize)) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'Some of the globalNode are out of bound')
END IF

CALL obj%SetMultiple(&
  & indx=localNode, &
  & VALUE=VALUE, &
  & scale=scale, &
  & addContribution=addContribution &
  & )

END PROCEDURE sField_set5

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_set6
CHARACTER(*), PARAMETER :: myName = "sField_set6"
INTEGER(I4B) :: globalNode(INT(1 + (iend - istart) / stride)), ii, jj

jj = 0
DO ii = istart, iend, stride
  jj = jj + 1
  globalNode(jj) = ii
END DO
CALL obj%set(globalNode=globalNode, VALUE=VALUE, scale=scale, &
  & addContribution=addContribution)
END PROCEDURE sField_set6

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_set7
CHARACTER(*), PARAMETER :: myName = "sField_set7"
INTEGER(I4B) :: globalNode(INT(1 + (iend - istart) / stride)), ii, jj

jj = 0
DO ii = istart, iend, stride
  jj = jj + 1
  globalNode(jj) = ii
END DO
CALL obj%set(globalNode=globalNode, VALUE=VALUE, scale=scale, &
  & addContribution=addContribution)
END PROCEDURE sField_set7

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_set8
CHARACTER(*), PARAMETER :: myName = "sField_set8"
INTEGER(I4B) :: ierr
REAL(DFP), POINTER :: realvec(:)

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either ScalarFieldLis_::obj is not initiated'// &
  & " or, lis_ptr is not available")
END IF

IF (.NOT. obj2%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either ScalarField_::obj2 is not initiated'// &
  & "")
END IF

SELECT TYPE (obj2)
TYPE is (ScalarField_)
  realvec => NULL()
  realvec => obj2%getPointer()
  CALL obj%set(VALUE=realvec)
  realvec => NULL()

TYPE is (ScalarFieldLis_)
  CALL lis_vector_is_null(obj2%lis_ptr, ierr)

  IF (ierr .EQ. LIS_TRUE) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'Either ScalarFieldLis_::obj%lis_ptr'// &
    & " is not available")
  END IF

  IF (obj%SIZE() .NE. obj2%SIZE()) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'Size of obj and obj2 are not same')
  END IF

  CALL lis_vector_copy(obj2%lis_ptr, obj%lis_ptr, ierr)
  CALL CHKERR(ierr)

CLASS DEFAULT
  CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'Unknown type of ScalarField_::obj2')
END SELECT

END PROCEDURE sField_set8

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_set9
CHARACTER(*), PARAMETER :: myName = "sField_set9"

SELECT CASE (VALUE%vartype)
CASE (Constant)
  CALL obj%Set( &
  & VALUE=GET(VALUE, TypeFEVariableScalar, TypeFEVariableConstant), &
  & globalNode=globalNode, &
  & scale=scale, &
  & addContribution=addContribution)
CASE (Space)
  CALL obj%Set( &
    & VALUE=GET(VALUE, TypeFEVariableScalar, TypeFEVariableSpace), &
    & globalNode=globalNode, &
    & scale=scale, &
    & addContribution=addContribution)
CASE DEFAULT
  CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'No case found for Value%vartype, only [Constant and Space is allowed]')
END SELECT
END PROCEDURE sField_set9

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_set10
! obj = obj + scale * obj2
#include "lisf.h"
CHARACTER(*), PARAMETER :: myName = "sField_set10"
INTEGER(I4B) :: ierr
REAL(DFP), POINTER :: realvec(:)

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either ScalarFieldLis_::obj is not initiated'// &
  & " or, lis_ptr is not available")
END IF

IF (.NOT. obj2%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either ScalarField_::obj2 is not initiated'// &
  & "")
END IF

SELECT TYPE (obj2)
TYPE is (ScalarField_)
  realvec => NULL()
  realvec => obj2%getPointer()
  CALL obj%set(VALUE=realvec, scale=scale, addContribution=addContribution)
  realvec => NULL()

TYPE is (ScalarFieldLis_)
  CALL lis_vector_is_null(obj2%lis_ptr, ierr)

  IF (ierr .EQ. LIS_TRUE) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'Either ScalarFieldLis_::obj%lis_ptr'// &
    & " is not available")
  END IF

  IF (obj%SIZE() .NE. obj2%SIZE()) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'Size of obj and obj2 are not same')
  END IF

  CALL lis_vector_axpy(scale, obj2%lis_ptr, obj%lis_ptr, ierr)
  CALL CHKERR(ierr)

CLASS DEFAULT
  CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'Unknown type of ScalarField_::obj2')
END SELECT

END PROCEDURE sField_set10

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods
