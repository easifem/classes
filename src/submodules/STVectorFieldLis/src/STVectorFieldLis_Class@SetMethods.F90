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

SUBMODULE(STVectorFieldLis_Class) SetMethods
USE BaseMethod
USE Field
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 SetSingle
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_setSingle
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

END PROCEDURE stvField_setSingle

!----------------------------------------------------------------------------
!                                                               SetMultiple
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_setMultiple
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
END PROCEDURE stvField_setMultiple

!----------------------------------------------------------------------------
!                                                                     SetAll
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_setAll
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
END PROCEDURE stvField_setAll

!----------------------------------------------------------------------------
!                                                                   set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set1
CHARACTER(*), PARAMETER :: myName = "stvField_set1"
INTEGER(I4B) :: localNode
INTEGER(I4B) :: ii
INTEGER(I4B) :: indx(obj%spaceCompo)
INTEGER(I4B) :: sindx(obj%spaceCompo)
INTEGER(I4B) :: ierr
LOGICAL(LGT) :: bools(2)

CALL lis_vector_is_null(obj%lis_ptr, ierr)
CALL CHKERR(ierr)
IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either STVectorFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

bools = SHAPE(VALUE) .NE. [obj%spaceCompo, obj%timeCompo]

IF (ANY(bools)) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'The shape of value is not compatible, it should be equal'// &
  & ' to [obj%spaceCompo, obj%timeCompo]')
END IF

localNode = obj%domain%getLocalNodeNumber(globalNode)

IF (localNode .EQ. 0_I4B) THEN
  CALL e%raiseError(modName//'::'//myName//" - " &
    & //'globalNode :: '//tostring(globalNode) &
    & //" is out of bound for the domain.")
END IF

sindx = arange(1_I4B, obj%spaceCompo, 1_I4B)
DO ii = 1, obj%timeCompo
  indx = GetNodeLoc( &
    & obj=obj%dof, &
    & nodenum=localNode, &
    & ivar=1_I4B, &
    & spaceCompo=sindx, &
    & timeCompo=ii)
  CALL obj%SetMultiple(&
    & indx=indx, &
    & VALUE=VALUE(:, ii), &
    & scale=scale, &
    & addContribution=addContribution)
END DO

END PROCEDURE stvField_set1

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set2
CHARACTER(*), PARAMETER :: myName = "stvField_set2"
LOGICAL(LGT) :: bools(2)
INTEGER(I4B) :: localNode
INTEGER(I4B) :: ii
INTEGER(I4B) :: jj
INTEGER(I4B) :: tNodes
INTEGER(I4B) :: indx(obj%spaceCompo)
INTEGER(I4B) :: sindx(obj%spaceCompo)
INTEGER(I4B) :: ierr

CALL lis_vector_is_null(obj%lis_ptr, ierr)
CALL CHKERR(ierr)
IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either STVectorFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

bools = SHAPE(VALUE) .NE. [obj%spaceCompo, obj%timeCompo]

IF (ANY(bools)) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'The shape of value is not compatible, it should be equal'// &
  & ' to [obj%spaceCompo, obj%timeCompo]')
END IF

sindx = arange(1_I4B, obj%spaceCompo, 1_I4B)
tNodes = obj%domain%getTotalNodes()

DO jj = 1, tNodes
  DO ii = 1, obj%timeCompo
    indx = GetNodeLoc( &
      & obj=obj%dof, &
      & nodenum=jj, &
      & ivar=1_I4B, &
      & spaceCompo=sindx, &
      & timeCompo=ii)
    CALL obj%SetMultiple(&
      & indx=indx, &
      & VALUE=VALUE(:, ii), &
      & scale=scale, &
      & addContribution=addContribution)
  END DO
END DO

END PROCEDURE stvField_set2

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set3
CHARACTER(*), PARAMETER :: myName = "stvField_set3"
LOGICAL(LGT) :: bools(2)
INTEGER(I4B) :: tNodes
INTEGER(I4B) :: indx
INTEGER(I4B) :: ii
INTEGER(I4B) :: ierr

CALL lis_vector_is_null(obj%lis_ptr, ierr)
CALL CHKERR(ierr)
IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either STVectorFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

bools = [spaceCompo, timeCompo] .GT. [obj%spaceCompo, obj%timeCompo]

IF (ANY(bools)) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'given spaceCompo and timeCompo should be less than or equal'// &
  & ' to obj%spaceCompo and obj%timeCompo')
END IF

tNodes = obj%domain%getTotalNodes()

DO ii = 1, tNodes
  indx = GetNodeLoc( &
    & obj=obj%dof, &
    & nodenum=ii, &
    & ivar=1_I4B, &
    & spaceCompo=spaceCompo, &
    & timeCompo=timeCompo)

  CALL obj%SetSingle(&
    & indx=indx, &
    & VALUE=VALUE, &
    & scale=scale, &
    & addContribution=addContribution)
END DO

END PROCEDURE stvField_set3

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set4
CHARACTER(*), PARAMETER :: myName = "stvField_set4"
INTEGER(I4B) :: ierr
LOGICAL(LGT) :: bools(3)
INTEGER(I4B) :: localNode
INTEGER(I4B) :: ii
INTEGER(I4B) :: jj
INTEGER(I4B) :: tNodes
INTEGER(I4B) :: indx(obj%spaceCompo)
INTEGER(I4B) :: sindx(obj%spaceCompo)

CALL lis_vector_is_null(obj%lis_ptr, ierr)
CALL CHKERR(ierr)
IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either STVectorFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

tnodes = obj%domain%getTotalNodes()

bools = SHAPE(VALUE) .NE. [obj%spaceCompo, obj%timeCompo, tnodes]

IF (ANY(bools)) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'The shape of value is not compatible, it should be equal'// &
  & ' to [obj%spaceCompo, obj%timeCompo, tnodes]')
END IF

sindx = arange(1_I4B, obj%spaceCompo, 1_I4B)

DO jj = 1, tNodes
  DO ii = 1, obj%timeCompo
    indx = GetNodeLoc( &
      & obj=obj%dof, &
      & nodenum=jj, &
      & ivar=1_I4B, &
      & spaceCompo=sindx, &
      & timeCompo=ii)
    CALL obj%SetMultiple(&
      & indx=indx, &
      & VALUE=VALUE(:, ii, jj), &
      & scale=scale, &
      & addContribution=addContribution)
  END DO
END DO
END PROCEDURE stvField_set4

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set5
CHARACTER(*), PARAMETER :: myName = "stvField_set5"
LOGICAL(LGT) :: bools
INTEGER(I4B) :: ii
INTEGER(I4B) :: tNodes
INTEGER(I4B) :: indx
INTEGER(I4B) :: ierr

CALL lis_vector_is_null(obj%lis_ptr, ierr)
CALL CHKERR(ierr)
IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either STVectorFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

tnodes = obj%domain%getTotalNodes()
bools = SIZE(VALUE) .NE. tnodes

IF (bools) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'The size of value is not compatible, it should be equal'// &
  & ' to tnodes')
END IF

DO ii = 1, tNodes
  indx = GetNodeLoc( &
    & obj=obj%dof, &
    & nodenum=ii, &
    & ivar=1_I4B, &
    & spaceCompo=spaceCompo, &
    & timeCompo=timeCompo)
  CALL obj%SetSingle(&
    & indx=indx, &
    & VALUE=VALUE(ii), &
    & scale=scale, &
    & addContribution=addContribution)
END DO
END PROCEDURE stvField_set5

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set6
CHARACTER(*), PARAMETER :: myName = "stvField_set6"
LOGICAL(LGT) :: bools(2)
INTEGER(I4B) :: ii
INTEGER(I4B) :: jj
INTEGER(I4B) :: indx
INTEGER(I4B) :: tNodes
INTEGER(I4B) :: ierr
REAL(DFP) :: aval

CALL lis_vector_is_null(obj%lis_ptr, ierr)
CALL CHKERR(ierr)
IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either STVectorFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

bools = [spaceCompo, timeCompo] .GT. [obj%spaceCompo, obj%timeCompo]
IF (ANY(bools)) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'given spaceCompo and timeCompo should be less than or equal'// &
  & ' to obj%spaceCompo and obj%timeCompo')
END IF

tNodes = obj%domain%getTotalNodes()

IF (tNodes .NE. VALUE%domain%getTotalNodes()) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'tNodes in obj and value should be equal.')
END IF

SELECT TYPE (VALUE)
CLASS IS (ScalarField_)
  DO ii = 1, tNodes
    jj = obj%domain%getGlobalNodeNumber(localNode=ii)
    CALL VALUE%get(VALUE=aval, globalNode=jj)
    indx = GetNodeLoc(&
      & obj=obj%dof, &
      & nodenum=ii, &
      & ivar=1_I4B, &
      & spaceCompo=spaceCompo, &
      & timeCompo=timeCompo)
    CALL obj%SetSingle( &
      & VALUE=aval, &
      & indx=indx, &
      & scale=scale, &
      & addContribution=addContribution)
  END DO
CLASS default
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'No case found for the type of AbstractNodeField_')
END SELECT

END PROCEDURE stvField_set6

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set7
REAL(DFP) :: val(SIZE(VALUE, 1), SIZE(VALUE, 2), SIZE(globalNode))
INTEGER(I4B) :: ii
DO ii = 1, SIZE(globalNode)
  val(:, :, ii) = VALUE(:, :)
END DO
CALL obj%set(VALUE=val, globalNode=globalNode, scale=scale, &
  & addContribution=addContribution)
END PROCEDURE stvField_set7

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set8
CHARACTER(*), PARAMETER :: myName = "stvField_set8"
INTEGER(I4B) :: ierr
LOGICAL(LGT) :: bools(3)
INTEGER(I4B) :: localNode(SIZE(globalNode))
INTEGER(I4B) :: ii
INTEGER(I4B) :: jj
INTEGER(I4B) :: indx(obj%spaceCompo)
INTEGER(I4B) :: sindx(obj%spaceCompo)

CALL lis_vector_is_null(obj%lis_ptr, ierr)
CALL CHKERR(ierr)
IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either STVectorFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

bools = SHAPE(VALUE) .NE. [obj%spaceCompo, obj%timeCompo, &
  & SIZE(globalNode)]

IF (ANY(bools)) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Incompatible shape and size of value')
END IF

localNode = obj%domain%getLocalNodeNumber(globalNode)

IF (ANY(localNode .EQ. 0)) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Some of the globalNode are out of bound')
END IF

sindx = arange(1_I4B, obj%spaceCompo, 1_I4B)

DO jj = 1, SIZE(VALUE, 3)
  DO ii = 1, obj%timeCompo
    indx = GetNodeLoc( &
      & obj=obj%dof, &
      & nodenum=localNode(jj), &
      & ivar=1_I4B, &
      & spaceCompo=sindx, &
      & timeCompo=ii)

    CALL obj%SetMultiple(&
      & indx=indx, &
      & VALUE=VALUE(:, ii, jj), &
      & scale=scale, &
      & addContribution=addContribution)
  END DO
END DO

END PROCEDURE stvField_set8

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set9
CHARACTER(*), PARAMETER :: myName = "stvField_set8"
INTEGER(I4B) :: ierr
LOGICAL(LGT) :: bools(3)
INTEGER(I4B) :: localNode(SIZE(globalNode))
INTEGER(I4B) :: indx(SIZE(globalNode))

CALL lis_vector_is_null(obj%lis_ptr, ierr)
CALL CHKERR(ierr)
IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either STVectorFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

bools(1:2) = [spaceCompo, timeCompo] .GT. [obj%spaceCompo, obj%timeCompo]

IF (ANY(bools)) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'given spaceCompo and timeCompo should be less than or equal'// &
  & ' to obj%spaceCompo and obj%timeCompo')
END IF

IF (SIZE(VALUE) .NE. SIZE(globalNode)) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Size of value should be equal to size of globalNode')
END IF

localNode = obj%domain%getLocalNodeNumber(globalNode)

IF (ANY(localNode .EQ. 0_I4B)) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Some of the global node num are out of bound')
END IF

indx = GetNodeLoc( &
  & obj=obj%dof, &
  & nodenum=localNode, &
  & ivar=1_I4B, &
  & spaceCompo=spaceCompo, &
  & timeCompo=timeCompo)

CALL obj%SetMultiple(&
  & indx=indx, &
  & VALUE=VALUE, &
  & scale=scale, &
  & addContribution=addContribution)

END PROCEDURE stvField_set9

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set10
CHARACTER(*), PARAMETER :: myName = "stvField_set8"
INTEGER(I4B) :: ierr
LOGICAL(LGT) :: bools(2)
INTEGER(I4B) :: localNode
INTEGER(I4B) :: indx

CALL lis_vector_is_null(obj%lis_ptr, ierr)
CALL CHKERR(ierr)
IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either STVectorFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

bools(1:2) = [spaceCompo, timeCompo] .GT. [obj%spaceCompo, obj%timeCompo]

IF (ANY(bools)) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'given spaceCompo and timeCompo should be less than or equal'// &
  & ' to obj%spaceCompo and obj%timeCompo')
END IF

localNode = obj%domain%getLocalNodeNumber(globalNode)

IF ((localNode .EQ. 0_I4B)) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Some of the global node num are out of bound')
END IF

indx = GetNodeLoc( &
  & obj=obj%dof, &
  & nodenum=localNode, &
  & ivar=1_I4B, &
  & spaceCompo=spaceCompo, &
  & timeCompo=timeCompo)

CALL obj%SetSingle(&
  & indx=indx, &
  & VALUE=VALUE, &
  & scale=scale, &
  & addContribution=addContribution)

END PROCEDURE stvField_set10

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set11
CHARACTER(*), PARAMETER :: myName = "stvField_set11"
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
END PROCEDURE stvField_set11

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set12
CHARACTER(*), PARAMETER :: myName = "stvField_set12"
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
END PROCEDURE stvField_set12

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set13
CHARACTER(*), PARAMETER :: myName = "stvField_set13"

SELECT CASE (VALUE%vartype)
CASE (SpaceTime)
  CALL obj%set( &
    & VALUE=GET(VALUE, TypeFEVariableVector, &
    & TypeFEVariableSpaceTime), &
    & globalNode=globalNode, &
    & scale=scale, &
    & addContribution=addContribution)
CASE DEFAULT
  CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'No case found for Value%vartype only SpaceTime allowed')
END SELECT
END PROCEDURE stvField_set13

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set14
CHARACTER(*), PARAMETER :: myName = "stvField_set14"
INTEGER(I4B) :: ierr

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either STVectorFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

CALL obj%setAll(VALUE=VALUE, scale=scale, addContribution=addContribution)

END PROCEDURE stvField_set14

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods
