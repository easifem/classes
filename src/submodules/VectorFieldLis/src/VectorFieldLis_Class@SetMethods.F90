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

SUBMODULE(VectorFieldLis_Class) SetMethods
USE BaseMethod
USE ScalarFieldLis_Class
USE ScalarField_Class
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 SetSingle
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSingle
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

END PROCEDURE obj_SetSingle

!----------------------------------------------------------------------------
!                                                               SetMultiple
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMultiple
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
END PROCEDURE obj_SetMultiple

!----------------------------------------------------------------------------
!                                                                     SetAll
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetAll
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
END PROCEDURE obj_SetAll

!----------------------------------------------------------------------------
!                                                                   set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set1
CHARACTER(*), PARAMETER :: myName = "obj_Set1"
INTEGER(I4B) :: localNode
INTEGER(I4B) :: indx(obj%spaceCompo)
INTEGER(I4B) :: ierr

CALL lis_vector_is_null(obj%lis_ptr, ierr)
CALL CHKERR(ierr)
IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either VectorFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

IF (SIZE(VALUE) .NE. obj%spaceCompo) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Size of value should be equal to obj%spaceCompo')
END IF

localNode = obj%domain%getLocalNodeNumber(globalNode)

IF (localNode .EQ. 0_I4B) THEN
  CALL e%raiseError(modName//'::'//myName//" - " &
    & //'globalNode :: '//tostring(globalNode) &
    & //" is out of bound for the domain.")
END IF

indx = GetIndex(obj=obj%dof, nodenum=localNode)

CALL obj%SetMultiple(&
  & indx=indx, &
  & VALUE=VALUE, &
  & scale=scale, &
  & addContribution=addContribution)

END PROCEDURE obj_Set1

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set2
REAL(DFP), POINTER :: vecPointer(:)
CHARACTER(*), PARAMETER :: myName = "obj_Set2"
INTEGER(I4B) :: ii
INTEGER(I4B) :: tsize
INTEGER(I4B) :: indx(obj%spaceCompo)
INTEGER(I4B) :: ierr

CALL lis_vector_is_null(obj%lis_ptr, ierr)
CALL CHKERR(ierr)
IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either VectorFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

IF (SIZE(VALUE) .NE. obj%spaceCompo) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'size(value) should be same as obj%spaceCompo')
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

END PROCEDURE obj_Set2

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set3
CHARACTER(*), PARAMETER :: myName = "obj_Set3"
INTEGER(I4B) :: indx
INTEGER(I4B) :: tsize
INTEGER(I4B) :: ii
INTEGER(I4B) :: ierr

CALL lis_vector_is_null(obj%lis_ptr, ierr)
CALL CHKERR(ierr)
IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either VectorFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

IF (spaceCompo .GT. obj%spaceCompo) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'given spaceCompo should be less than or equal to obj%spaceCompo')
END IF

tsize = obj%domain%getTotalNodes()

DO ii = 1, tsize
  indx = GetNodeLoc(obj=obj%dof, nodenum=ii, idof=spaceCompo)
  CALL obj%SetSingle(&
    & indx=indx, &
    & VALUE=VALUE, &
    & scale=scale, &
    & addContribution=addContribution)
END DO

END PROCEDURE obj_Set3

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set4
CHARACTER(*), PARAMETER :: myName = "obj_Set4"
INTEGER(I4B) :: ii, tnodes, aa, jj
INTEGER(I4B) :: ierr

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'Either ScalarField object is not initiated'// &
    & " or, lis_ptr is not available")
END IF

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'This subroutine is not callable for constant STScalar field')
END IF

tnodes = obj%domain%getTotalNodes()

IF ( &
  &      SIZE(VALUE, 1) .NE. obj%spaceCompo &
  & .OR. SIZE(VALUE, 2) .NE. tnodes) THEN

  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'The shape of value should be [ ' &
    & //tostring(obj%spaceCompo) &
    & //', ' &
    & //tostring(tnodes) &
    & //' ]')

END IF

aa = 0
DO jj = 1, tnodes
  DO ii = 1, obj%spaceCompo
    aa = aa + 1
    CALL obj%SetSingle(&
      & indx=aa, &
      & VALUE=VALUE(ii, jj), &
      & scale=scale, &
      & addContribution=addContribution)
  END DO
END DO

END PROCEDURE obj_Set4

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set5
CHARACTER(*), PARAMETER :: myName = "obj_Set5"
INTEGER(I4B) :: ii
INTEGER(I4B) :: indx
INTEGER(I4B) :: ierr
INTEGER(I4B) :: tsize

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either VectorFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

IF (spaceCompo .GT. obj%spaceCompo) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'given spaceCompo should be less than or equal to obj%spaceCompo')

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'This subroutine is not callable for constant STScalar field')

tsize = SIZE(VALUE)

IF (tsize .NE. obj%domain%getTotalNodes()) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Size of value should be equal to the total number of nodes')

DO ii = 1, tsize
  indx = GetNodeLoc(obj=obj%dof, nodenum=ii, idof=spaceCompo)
  CALL obj%SetSingle(&
    & indx=indx, &
    & VALUE=VALUE(ii), &
    & scale=scale, &
    & addContribution=addContribution)
END DO

END PROCEDURE obj_Set5

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set6
REAL(DFP), POINTER :: vecPointer(:)
CHARACTER(*), PARAMETER :: myName = "obj_Set5"
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
  & 'Either VectorFieldLis_::object is not initiated'// &
  & ', or, ScalarField::value is not initiated'// &
  & ", or, obj%lis_ptr is not available")
END IF

IF (spaceCompo .GT. obj%spaceCompo) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'given spaceCompo should be less than or equal to obj%spaceCompo')

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
    CALL obj%set(VALUE=vecPointer(1), spaceCompo=spaceCompo, &
      & scale=scale, addContribution=addContribution)
    vecPointer => NULL()
  ELSE
    vecPointer => VALUE%getPointer()
    CALL obj%set(VALUE=vecPointer, spaceCompo=spaceCompo, &
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
      & spaceCompo=spaceCompo, &
      & scale=scale, &
      & addContribution=addContribution)

  ELSE

    DO ii = 1, tsize
      jj = obj%domain%getGlobalNodeNumber(localNode=ii)

      IF (jj .GT. 0) THEN
        CALL VALUE%get(VALUE=avar, globalNode=jj)

        ! CALL Display(avar, "avar = ")
        indx = GetNodeLoc(obj=obj%dof, nodenum=ii, idof=spaceCompo)

        CALL obj%setSingle( &
          & VALUE=avar, &
          & indx=indx, &
          & scale=scale, &
          & addContribution=addContribution)

      END IF

    END DO

  END IF

CLASS DEFAULT
  CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'No case found for the type of value')

END SELECT

END PROCEDURE obj_Set6

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set7
REAL(DFP) :: val(SIZE(VALUE), SIZE(globalNode))
INTEGER(I4B) :: ii
DO ii = 1, SIZE(globalNode)
  val(:, ii) = VALUE(:)
END DO
CALL obj%set(VALUE=val, globalNode=globalNode, scale=scale, &
  & addContribution=addContribution)
END PROCEDURE obj_Set7

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set8
CHARACTER(*), PARAMETER :: myName = "obj_Set8"
INTEGER(I4B) :: localNode(SIZE(globalNode))
REAL(DFP) :: val(SIZE(VALUE))
INTEGER(I4B) :: indx(SIZE(VALUE))
INTEGER(I4B) :: ierr

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either VectorFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'This routine should not be called for constant STScalar field')

IF (SIZE(VALUE, 1) .NE. obj%spaceCompo .OR. &
  & SIZE(VALUE, 2) .NE. SIZE(globalNode)) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'SIZE( value, 1 ) not equal to spaceCompo'// &
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
& timeCompo=1_I4B, &
& spaceCompo=arange(1, obj%spaceCompo))

CALL obj%SetMultiple(&
& VALUE=val, &
& indx=indx, &
& scale=scale, &
& addContribution=addContribution)

END PROCEDURE obj_Set8

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set9
CHARACTER(*), PARAMETER :: myName = "obj_Set9"
INTEGER(I4B) :: localNode(SIZE(globalNode))
INTEGER(I4B) :: indx(SIZE(globalNode))
INTEGER(I4B) :: ierr

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either VectorFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

IF (spaceCompo .GT. obj%spaceCompo) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'given spaceCompo should be less than or equal to obj%spaceCompo')

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
& timeCompo=1, &
& spaceCompo=spaceCompo, &
& nodenum=localNode &
& )

CALL obj%SetMultiple(&
  & indx=indx, &
  & VALUE=VALUE, &
  & scale=scale, &
  & addContribution=addContribution)

END PROCEDURE obj_Set9

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set10
CHARACTER(*), PARAMETER :: myName = "obj_Set10"
INTEGER(I4B) :: indx
INTEGER(I4B) :: localNode
INTEGER(I4B) :: ierr

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either VectorFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

IF (spaceCompo .GT. obj%spaceCompo) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'given spaceCompo should be less than or equal to obj%spaceCompo')

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
  & timeCompo=1, &
  & spaceCompo=spaceCompo, &
  & nodenum=localNode)

CALL obj%setSingle(&
  & indx=indx, &
  & VALUE=VALUE, &
  & scale=scale, &
  & addContribution=addContribution)

END PROCEDURE obj_Set10

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set11
CHARACTER(*), PARAMETER :: myName = "obj_Set11"
INTEGER(I4B) :: globalNode(INT(1 + (iend - istart) / stride)), ii, jj
jj = 0
DO ii = istart, iend, stride
  jj = jj + 1
  globalNode(jj) = ii
END DO
CALL obj%set(globalNode=globalNode, VALUE=VALUE, scale=scale, &
  & addContribution=addContribution)
END PROCEDURE obj_Set11

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set12
CHARACTER(*), PARAMETER :: myName = "obj_Set12"
INTEGER(I4B) :: globalNode(INT(1 + (iend - istart) / stride)), ii, jj
jj = 0
DO ii = istart, iend, stride
  jj = jj + 1
  globalNode(jj) = ii
END DO
CALL obj%set(globalNode=globalNode, VALUE=VALUE, scale=scale, &
  & addContribution=addContribution)
END PROCEDURE obj_Set12

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set13
CHARACTER(*), PARAMETER :: myName = "obj_Set13"

IF (.NOT. obj%isInitiated) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Scalar field object is not initiated')

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'This routine should not be called for constant vector field')

SELECT CASE (VALUE%vartype)
CASE (Constant)
  CALL obj%Set( &
    & VALUE=GET(VALUE, TypeFEVariableVector, TypeFEVariableConstant), &
    & globalNode=globalNode, &
    & scale=scale, &
    & addContribution=addContribution)
CASE (Space)
  CALL obj%Set( &
    & VALUE=GET(VALUE, TypeFEVariableVector, TypeFEVariableSpace), &
    & globalNode=globalNode, &
    & scale=scale, &
    & addContribution=addContribution)
END SELECT
END PROCEDURE obj_Set13

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set14
CHARACTER(*), PARAMETER :: myName = "obj_Set14"
INTEGER(I4B) :: ierr

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either VectorFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

CALL obj%setAll(VALUE=VALUE, scale=scale, addContribution=addContribution)

END PROCEDURE obj_Set14

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set16
CHARACTER(*), PARAMETER :: myName = "obj_Set16()"
INTEGER(I4B) :: ierr
REAL(DFP), POINTER :: realvec(:)
LOGICAL(LGT) :: problem

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

#ifdef DEBUG_VER
CALL lis_vector_is_null(obj%lis_ptr, ierr)
problem = .NOT. obj%isInitiated .OR. (ierr .EQ. LIS_TRUE)
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & '[INTERNAL ERROR] :: Either VectorFieldLis_::obj is not initiated'// &
  & " or, lis_ptr is not available")
  RETURN
END IF

problem = .NOT. VALUE%isInitiated
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: Either VectorField_::value is not initiated')
  RETURN
END IF
#endif

SELECT TYPE (VALUE)
TYPE is (VectorField_)
  realvec => NULL()
  realvec => VALUE%GetPointer()
  CALL obj%Set(VALUE=realvec)
  realvec => NULL()

TYPE is (VectorFieldLis_)

#ifdef DEBUG_VER
  CALL lis_vector_is_null(VALUE%lis_ptr, ierr)
  problem = ierr .EQ. LIS_TRUE
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: Either VectorFieldLis_::obj%lis_ptr'// &
    & " is not available")
  END IF

  problem = obj%SIZE() .NE. VALUE%SIZE()
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: Size of obj and value are not same')
    RETURN
  END IF
#endif

  CALL lis_vector_copy(VALUE%lis_ptr, obj%lis_ptr, ierr)
  CALL CHKERR(ierr)

CLASS DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: Unknown type of VectorField_::value')
  RETURN
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_Set16

END SUBMODULE SetMethods
