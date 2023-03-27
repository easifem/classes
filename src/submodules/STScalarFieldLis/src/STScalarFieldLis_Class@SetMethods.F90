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

SUBMODULE(STScalarFieldLis_Class) SetMethods
USE BaseMethod
USE ScalarFieldLis_Class
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 SetSingle
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_setSingle
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

END PROCEDURE stsField_setSingle

!----------------------------------------------------------------------------
!                                                               SetMultiple
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_setMultiple
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
END PROCEDURE stsField_setMultiple

!----------------------------------------------------------------------------
!                                                                     SetAll
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_setAll
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
END PROCEDURE stsField_setAll

!----------------------------------------------------------------------------
!                                                                   set
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_set1
CHARACTER(*), PARAMETER :: myName = "stsField_set1"
INTEGER(I4B) :: localNode
INTEGER(I4B) :: tsize
INTEGER(I4B) :: indx(obj%timeCompo)
INTEGER(I4B) :: ierr

CALL lis_vector_is_null(obj%lis_ptr, ierr)
CALL CHKERR(ierr)
IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either STScalarFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

IF (SIZE(VALUE) .NE. obj%timeCompo) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Size of value should be equal to obj%timeCompo')
END IF

localNode = obj%domain%getLocalNodeNumber(globalNode)

IF (localNode .EQ. 0_I4B) THEN
  CALL e%raiseError(modName//'::'//myName//" - " &
    & //'globalNode :: '//TRIM(str(globalNode, .TRUE.)) &
    & //" is out of bound for the domain.")
END IF

indx = GetIndex(obj=obj%dof, nodenum=localNode)

CALL obj%SetMultiple(&
  & indx=indx, &
  & VALUE=VALUE, &
  & scale=scale, &
  & addContribution=addContribution)

END PROCEDURE stsField_set1

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_set2
CHARACTER(*), PARAMETER :: myName = "stsField_set2"
INTEGER(I4B) :: ii
INTEGER(I4B) :: tsize
INTEGER(I4B) :: indx(obj%timeCompo)
INTEGER(I4B) :: ierr

CALL lis_vector_is_null(obj%lis_ptr, ierr)
CALL CHKERR(ierr)
IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either ScalarField object is not initiated'// &
  & " or, lis_ptr is not available")
END IF

IF (SIZE(VALUE) .NE. obj%timeCompo) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'size(value) should be same as obj%timeCompo')
END IF

tsize = obj%domain%getTotalNodes()

DO ii = 1, tsize
  indx = GetIndex(obj=obj%dof, nodenum=ii)
  CALL obj%SetMultiple(&
    & indx=indx, &
    & VALUE=VALUE, &
    & scale=scale, &
    & addContribution=addContribution)
END DO

END PROCEDURE stsField_set2

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_set3
CHARACTER(*), PARAMETER :: myName = "stsField_set3"
INTEGER(I4B) :: indx
INTEGER(I4B) :: tsize
INTEGER(I4B) :: ii
INTEGER(I4B) :: ierr

CALL lis_vector_is_null(obj%lis_ptr, ierr)
CALL CHKERR(ierr)
IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either STScalarFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

IF (timeCompo .GT. obj%timeCompo) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'given timeCompo should be less than or equal to obj%timeCompo')
END IF

tsize = obj%domain%getTotalNodes()

DO ii = 1, tsize
  indx = GetNodeLoc(obj=obj%dof, nodenum=ii, idof=timeCompo)
  CALL obj%SetSingle(&
    & indx=indx, &
    & VALUE=VALUE, &
    & scale=scale, &
    & addContribution=addContribution)
END DO

END PROCEDURE stsField_set3

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_set4
CHARACTER(*), PARAMETER :: myName = "stsField_set4"
INTEGER(I4B) :: ii, tnodes, aa, jj
INTEGER(I4B) :: ierr

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either STScalarFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'This subroutine is not callable for constant STScalar field')
END IF

tnodes = obj%domain%getTotalNodes()

IF ( &
  &      SIZE(VALUE, 1) .NE. obj%timeCompo &
  & .OR. SIZE(VALUE, 2) .NE. tnodes) THEN

  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'The shape of value should be [ ' &
    & //tostring(obj%timeCompo) &
    & //', ' &
    & //tostring(tnodes) &
    & //' ]')

END IF

aa = 0
DO jj = 1, tnodes
  DO ii = 1, obj%timeCompo
    aa = aa + 1
    CALL obj%SetSingle(&
      & indx=aa, &
      & VALUE=VALUE(ii, jj), &
      & scale=scale, &
      & addContribution=addContribution)
  END DO
END DO

END PROCEDURE stsField_set4

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_set5
CHARACTER(*), PARAMETER :: myName = "stsField_set5"
INTEGER(I4B) :: ii
INTEGER(I4B) :: indx
INTEGER(I4B) :: ierr
INTEGER(I4B) :: tsize

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either STScalarFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

IF (timeCompo .GT. obj%timeCompo) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'given timeCompo should be less than or equal to obj%timeCompo')

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'This subroutine is not callable for constant STScalar field')

tsize = SIZE(VALUE)

IF (tsize .NE. obj%domain%getTotalNodes()) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Size of value should be equal to the total number of nodes')

DO ii = 1, tsize
  indx = GetNodeLoc(obj=obj%dof, nodenum=ii, idof=timeCompo)
  CALL obj%SetSingle(&
    & indx=indx, &
    & VALUE=VALUE(ii), &
    & scale=scale, &
    & addContribution=addContribution)
END DO

END PROCEDURE stsField_set5

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_set6
REAL(DFP), POINTER :: vecPointer(:)
CHARACTER(*), PARAMETER :: myName = "stsField_set5"
INTEGER(I4B) :: ierr
INTEGER(I4B) :: tsize1
INTEGER(I4B) :: tsize
INTEGER(I4B) :: ii
INTEGER(I4B) :: jj
INTEGER(I4B) :: indx
REAL(DFP) :: avar

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE &
  & .OR. .NOT. VALUE%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either STScalarFieldLis_::object is not initiated'// &
  & ', or, ScalarField::value is not initiated'// &
  & ", or, obj%lis_ptr is not available")
END IF

IF (timeCompo .GT. obj%timeCompo) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'given timeCompo should be less than or equal to obj%timeCompo')

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'This subroutine is not callable for constant STScalar field')

tsize = obj%domain%getTotalNodes()
tsize1 = VALUE%domain%getTotalNodes()

IF (tsize .NE. tsize1) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Size of value should be equal to the total number of nodes')
END IF

SELECT TYPE (VALUE)
TYPE is (ScalarField_)

  IF (VALUE%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
    vecPointer => VALUE%getPointer()
    CALL obj%set(VALUE=vecPointer(1), timeCompo=timeCompo, &
      & scale=scale, addContribution=addContribution)
    vecPointer => NULL()
  ELSE
    vecPointer => VALUE%getPointer()
    CALL obj%set(VALUE=vecPointer, timeCompo=timeCompo, &
      & scale=scale, addContribution=addContribution)
    vecPointer => NULL()
  END IF

TYPE is (ScalarFieldLis_)

  CALL lis_vector_is_null(VALUE%lis_ptr, ierr)

  IF (ierr .EQ. LIS_TRUE) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
    & "ScalarFieldLis_::value%lis_ptr is not available")
  END IF

  IF (VALUE%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN

    CALL VALUE%get(VALUE=avar, globalNode=1)
    CALL obj%set( &
      & VALUE=avar, &
      & timeCompo=timeCompo, &
      & scale=scale, &
      & addContribution=addContribution)

  ELSE

    DO ii = 1, tsize
      jj = obj%domain%getGlobalNodeNumber(localNode=ii)

      IF (jj .GT. 0) THEN
        CALL VALUE%get(VALUE=avar, globalNode=jj)

        ! CALL Display(avar, "avar = ")
        indx = GetNodeLoc(obj=obj%dof, nodenum=ii, idof=timeCompo)

        CALL obj%setSingle( &
          & VALUE=avar, &
          & indx=indx, &
          & scale=scale, &
          & addContribution=addContribution)

      END IF

    END DO

  END IF

END SELECT

END PROCEDURE stsField_set6

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_set7
REAL(DFP) :: val(SIZE(VALUE), SIZE(globalNode))
INTEGER(I4B) :: ii
DO ii = 1, SIZE(globalNode)
  val(:, ii) = VALUE(:)
END DO
CALL obj%set( &
  & VALUE=val, &
  & globalNode=globalNode, &
  & scale=scale, &
  & addContribution=addContribution)
END PROCEDURE stsField_set7

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_set8
CHARACTER(*), PARAMETER :: myName = "stsField_set8"
INTEGER(I4B) :: localNode(SIZE(globalNode))
REAL(DFP) :: val(SIZE(VALUE))
INTEGER(I4B) :: indx(SIZE(VALUE))
INTEGER(I4B) :: ierr

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either STScalarFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'This routine should not be called for constant STScalar field')

IF (SIZE(VALUE, 1) .NE. obj%timeCompo .OR. &
  & SIZE(VALUE, 2) .NE. SIZE(globalNode)) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'SIZE( value, 1 ) not equal to timeCompo'// &
  & 'or SIZE( value, 2 ) not equal to'// &
  & ' the SIZE(globalNode)')
END IF

localNode = obj%domain%getLocalNodeNumber(globalNode)

IF (ANY(localNode .EQ. 0_I4B)) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Some of the globalNode are out of bound')
END IF

val = RESHAPE(VALUE, [SIZE(VALUE)])
indx = GetNodeLoc( &
& obj=obj%dof, &
& nodenum=localNode, &
& ivar=1_I4B, &
& spaceCompo=1_I4B, &
& timeCompo=arange(1, obj%timeCompo))

CALL obj%SetMultiple(&
& VALUE=val, &
& indx=indx, &
& scale=scale, &
& addContribution=addContribution)

END PROCEDURE stsField_set8

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_set9
CHARACTER(*), PARAMETER :: myName = "stsField_set9"
INTEGER(I4B) :: localNode(SIZE(globalNode))
INTEGER(I4B) :: indx(SIZE(globalNode))
INTEGER(I4B) :: ierr

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either STScalarFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

IF (timeCompo .GT. obj%timeCompo) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'given timeCompo should be less than or equal to obj%timeCompo')

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'This subroutine is not callable for constant STScalar field')

IF (SIZE(VALUE) .NE. SIZE(globalNode)) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Size of value should be equal to size of globalNode')

localNode = obj%domain%getLocalNodeNumber(globalNode)

IF (ANY(localNode .EQ. 0_I4B)) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Some of the global node num are out of bound')

indx = GetNodeLoc(&
& obj=obj%dof, &
& ivar=1, &
& spaceCompo=1, &
& timeCompo=timeCompo, &
& nodenum=localNode &
& )

CALL obj%SetMultiple(&
  & indx=indx, &
  & VALUE=VALUE, &
  & scale=scale, &
  & addContribution=addContribution)

END PROCEDURE stsField_set9

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_set10
CHARACTER(*), PARAMETER :: myName = "stsField_set10"
INTEGER(I4B) :: indx
INTEGER(I4B) :: localNode
INTEGER(I4B) :: ierr

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either STScalarFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

IF (timeCompo .GT. obj%timeCompo) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'given timeCompo should be less than or equal to obj%timeCompo')

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'This subroutine is not callable for constant STScalar field')

localNode = obj%domain%getLocalNodeNumber(globalNode)

IF (localNode .EQ. 0_I4B) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'The given global node num are out of bound')

indx = getNodeLoc( &
  & obj=obj%dof, &
  & ivar=1, &
  & spaceCompo=1, &
  & timeCompo=timeCompo, &
  & nodenum=localNode)

CALL obj%setSingle(&
  & indx=indx, &
  & VALUE=VALUE, &
  & scale=scale, &
  & addContribution=addContribution)

END PROCEDURE stsField_set10

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_set11
CHARACTER(*), PARAMETER :: myName = "stsField_set11"
INTEGER(I4B) :: globalNode(INT(1 + (iend - istart) / stride)), ii, jj
jj = 0
DO ii = istart, iend, stride
  jj = jj + 1
  globalNode(jj) = ii
END DO
CALL obj%set(globalNode=globalNode, VALUE=VALUE, &
  & scale=scale, addContribution=addContribution)
END PROCEDURE stsField_set11

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_set12
CHARACTER(*), PARAMETER :: myName = "stsField_set12"
INTEGER(I4B) :: globalNode(INT(1 + (iend - istart) / stride)), ii, jj
jj = 0
DO ii = istart, iend, stride
  jj = jj + 1
  globalNode(jj) = ii
END DO
CALL obj%set( &
  & globalNode=globalNode, &
  & VALUE=VALUE, &
  & scale=scale, &
  & addContribution=addContribution)
END PROCEDURE stsField_set12

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_set13
CHARACTER(*), PARAMETER :: myName = "stsField_set13"

IF (.NOT. obj%isInitiated) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Scalar field object is not initiated')

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'This routine should not be called for constant STScalar field')

SELECT CASE (VALUE%vartype)
CASE (SpaceTime)

  CALL obj%set( &
    & VALUE=GET(VALUE, TypeFEVariableScalar, TypeFEVariableSpaceTime), &
    & globalNode=globalNode, &
    & scale=scale, &
    & addContribution=addContribution)

CASE DEFAULT

  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'No case found for Value%vartype')

END SELECT
END PROCEDURE stsField_set13

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_set14
CHARACTER(*), PARAMETER :: myName = "stsField_set14"
INTEGER(I4B) :: ierr

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either STScalarFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

CALL obj%setAll(VALUE=VALUE, scale=scale, addContribution=addContribution)

END PROCEDURE stsField_set14

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods
