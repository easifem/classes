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

SUBMODULE(STScalarField_Class) SetMethods
USE BaseMethod
USE ScalarField_Class, ONLY: ScalarField_
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   set
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_set1
CHARACTER(*), PARAMETER :: myName = "stsField_set1"
INTEGER(I4B) :: localNode
REAL(DFP) :: areal
LOGICAL(LGT) :: abool
areal = Input(option=scale, default=1.0_DFP)
abool = Input(option=addContribution, default=.FALSE.)

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'STScalarField_::obj is not initiated')
  RETURN
END IF

IF (SIZE(VALUE) .NE. obj%timeCompo) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Size of value should be equal to obj%timeCompo')
  RETURN
END IF

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  IF (abool) THEN
    CALL add( &
      & obj=obj%realVec, &
      & dofobj=obj%dof, &
      & nodenum=[1], &
      & VALUE=VALUE, &
      & conversion=[NONE], &
      & scale=areal)
  ELSE
    CALL set( &
      & obj=obj%realVec, &
      & dofobj=obj%dof, &
      & nodenum=[1], &
      & VALUE=VALUE, &
      & conversion=[NONE])
  END IF
  RETURN
END IF

localNode = obj%domain%getLocalNodeNumber(globalNode)

IF (obj%tSize .GE. localNode) THEN
  IF (abool) THEN
    CALL add( &
      & obj=obj%realVec, &
      & dofobj=obj%dof, &
      & nodenum=[localNode], &
      & VALUE=VALUE, &
      & conversion=[NONE], &
      & scale=areal)
  ELSE
    CALL set( &
      & obj=obj%realVec, &
      & dofobj=obj%dof, &
      & nodenum=[localNode], &
      & VALUE=VALUE, &
      & conversion=[NONE])
  END IF
  RETURN
END IF

CALL e%raiseError(modName//'::'//myName//" - " &
& //'globalNode :: '//TRIM(str(globalNode, .TRUE.)) &
& //" is out of bound for the domain.")

END PROCEDURE stsField_set1

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_set2
REAL(DFP), POINTER :: vecPointer(:)
CHARACTER(*), PARAMETER :: myName = "stsField_set2"
INTEGER(I4B) :: idof
REAL(DFP) :: areal
LOGICAL(LGT) :: abool
areal = Input(option=scale, default=1.0_DFP)
abool = Input(option=addContribution, default=.FALSE.)

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'STScalarField_::obj is not initiated')
  RETURN
END IF

IF (SIZE(VALUE) .NE. obj%timeCompo) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'size(value) should be same as obj%timeCompo')
  RETURN
END IF

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  IF (abool) THEN
    CALL add( &
      & obj=obj%realVec, &
      & dofobj=obj%dof, &
      & nodenum=[1], &
      & VALUE=VALUE, &
      & conversion=[NONE], &
      & scale=areal)
  ELSE
    CALL set( &
      & obj=obj%realVec, &
      & dofobj=obj%dof, &
      & nodenum=[1], &
      & VALUE=VALUE, &
      & conversion=[NONE])
  END IF
  RETURN
END IF

vecPointer => NULL()

IF (abool) THEN
  DO idof = 1, obj%timeCompo
    vecPointer => getPointer(obj%realVec, obj%dof, idof)
    vecPointer = vecPointer + scale * VALUE(idof)
  END DO
ELSE
  DO idof = 1, obj%timeCompo
    vecPointer => getPointer(obj%realVec, obj%dof, idof)
    vecPointer = VALUE(idof)
  END DO
END IF

vecPointer => NULL()

END PROCEDURE stsField_set2

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_set3
REAL(DFP), POINTER :: vecPointer(:)
CHARACTER(*), PARAMETER :: myName = "stsField_set3"
INTEGER(I4B) :: idof
REAL(DFP) :: areal
LOGICAL(LGT) :: abool
areal = Input(option=scale, default=1.0_DFP)
abool = Input(option=addContribution, default=.FALSE.)

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'STScalar field object is not initiated')
  RETURN
END IF

IF (timeCompo .GT. obj%timeCompo) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
 & 'given timeCompo should be less than or equal to obj%timeCompo')
  RETURN
END IF

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  IF (abool) THEN
    CALL add( &
      & obj=obj%realVec, &
      & dofobj=obj%dof, &
      & nodenum=[1], &
      & VALUE=[VALUE], &
      & idof=timeCompo, &
      & scale=areal)
  ELSE
    CALL set( &
      & obj=obj%realVec, &
      & dofobj=obj%dof, &
      & nodenum=[1], &
      & VALUE=[VALUE], &
      & idof=timeCompo)
  END IF
  RETURN
END IF

vecPointer => getPointer(obj%realVec, obj%dof, timeCompo)
IF (abool) THEN
  vecPointer = vecPointer + areal * VALUE
ELSE
  vecPointer = VALUE
END IF
vecPointer => NULL()

END PROCEDURE stsField_set3

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_set4
CHARACTER(*), PARAMETER :: myName = "stsField_set4"
INTEGER(I4B) :: ii, tnodes, aa, jj
REAL(DFP) :: areal
LOGICAL(LGT) :: abool
areal = Input(option=scale, default=1.0_DFP)
abool = Input(option=addContribution, default=.FALSE.)

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'STScalar field object is not initiated')
  RETURN
END IF

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
 & 'This subroutine is not callable for constant STScalar field')
  RETURN
END IF

tnodes = obj%domain%getTotalNodes()

IF (SIZE(VALUE, 1) .NE. obj%timeCompo &
  & .OR. SIZE(VALUE, 2) .NE. tnodes) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'The shape of value should be [ ' &
    & //tostring(obj%timeCompo) &
    & //', ' &
    & //tostring(tnodes) &
    & //' ]')
  RETURN
END IF

aa = 0

IF (abool) THEN
  DO jj = 1, tnodes
    DO ii = 1, obj%timeCompo
      aa = aa + 1
      obj%realVec%val(aa) = obj%realVec%val(aa) + areal * VALUE(ii, jj)
    END DO
  END DO
ELSE
  DO jj = 1, tnodes
    DO ii = 1, obj%timeCompo
      aa = aa + 1
      obj%realVec%val(aa) = VALUE(ii, jj)
    END DO
  END DO
END IF

END PROCEDURE stsField_set4

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_set5
REAL(DFP), POINTER :: vecPointer(:)
CHARACTER(*), PARAMETER :: myName = "stsField_set5"
INTEGER(I4B) :: idof
REAL(DFP) :: areal
LOGICAL(LGT) :: abool
areal = Input(option=scale, default=1.0_DFP)
abool = Input(option=addContribution, default=.FALSE.)

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
 & 'STScalar field object is not initiated')
  RETURN
END IF

IF (timeCompo .GT. obj%timeCompo) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
 & 'given timeCompo should be less than or equal to obj%timeCompo')
  RETURN
END IF

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'This subroutine is not callable for constant STScalar field')
  RETURN
END IF

IF (SIZE(VALUE) .NE. obj%domain%getTotalNodes()) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
 & 'Size of value should be equal to the total number of nodes')
  RETURN
END IF

vecPointer => getPointer(obj%realVec, obj%dof, timeCompo)
IF (abool) THEN
  vecPointer = vecPointer + areal * VALUE
ELSE
  vecPointer = VALUE
END IF

vecPointer => NULL()

END PROCEDURE stsField_set5

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_set6
REAL(DFP), POINTER :: vecPointer(:)
CHARACTER(*), PARAMETER :: myName = "stsField_set5"
INTEGER(I4B) :: idof
REAL(DFP) :: areal
LOGICAL(LGT) :: abool
areal = Input(option=scale, default=1.0_DFP)
abool = Input(option=addContribution, default=.FALSE.)

IF (.NOT. obj%isInitiated .OR. .NOT. VALUE%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
 & 'STScalar field object is not initiated')
  RETURN
END IF

IF (timeCompo .GT. obj%timeCompo) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'given timeCompo should be less than or equal to obj%timeCompo')
  RETURN
END IF

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
 & 'This subroutine is not callable for constant STScalar field')
  RETURN
END IF

SELECT TYPE (VALUE)
TYPE IS (ScalarField_)
  IF (VALUE%domain%getTotalNodes() .NE. obj%domain%getTotalNodes()) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
        & 'Size of value should be equal to the total number of nodes')
    RETURN
  END IF

  IF (VALUE%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
    vecPointer => getPointer(VALUE%realVec, VALUE%dof, 1)
    CALL obj%set(VALUE=vecPointer(1), timeCompo=timeCompo, &
      & scale=areal, addContribution=abool)
    vecPointer => NULL()
    RETURN
  END IF

  vecPointer => getPointer(obj%realVec, obj%dof, timeCompo)
  IF (abool) THEN
    vecPointer = vecPointer + areal * get(VALUE%realVec, 1.0_DFP)
  ELSE
    vecPointer = get(VALUE%realVec, 1.0_DFP)
  END IF
  vecPointer => NULL()

CLASS DEFAULT
  CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'No case found for the type of value')
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
REAL(DFP) :: areal
LOGICAL(LGT) :: abool
areal = Input(option=scale, default=1.0_DFP)
abool = Input(option=addContribution, default=.FALSE.)

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
 & 'Scalar field object is not initiated')
  RETURN
END IF

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
 & 'This routine should not be called for constant STScalar field')
  RETURN
END IF

IF (SIZE(VALUE, 1) .NE. obj%timeCompo .OR. &
    & SIZE(VALUE, 2) .NE. SIZE(globalNode)) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'SIZE( value, 1 ) not equal timeCompo or SIZE( value, 2 ) ' &
      & //'not equal to the SIZE(globalNode)')
  RETURN
END IF

localNode = obj%domain%getLocalNodeNumber(globalNode)

IF (ANY(localNode .GT. obj%tSize)) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
 & 'Some of the globalNode are out of bound')
  RETURN
END IF

val = RESHAPE(VALUE, [SIZE(VALUE)])

IF (abool) THEN
  CALL add( &
    & obj=obj%realVec, &
    & dofobj=obj%dof, &
    & nodenum=localNode, &
    & VALUE=val, &
    & conversion=[NONE], &
    & scale=areal)
ELSE
  CALL set( &
    & obj=obj%realVec, &
    & dofobj=obj%dof, &
    & nodenum=localNode, &
    & VALUE=val, &
    & conversion=[NONE])
END IF

END PROCEDURE stsField_set8

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_set9
REAL(DFP), POINTER :: vecPointer(:)
CHARACTER(*), PARAMETER :: myName = "stsField_set9"
INTEGER(I4B) :: idof
INTEGER(I4B) :: localNode(SIZE(globalNode))
REAL(DFP) :: areal
LOGICAL(LGT) :: abool
areal = Input(option=scale, default=1.0_DFP)
abool = Input(option=addContribution, default=.FALSE.)

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
 & 'STScalar field object is not initiated')
  RETURN
END IF

IF (timeCompo .GT. obj%timeCompo) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
 & 'given timeCompo should be less than or equal to obj%timeCompo')
  RETURN
END IF

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
 & 'This subroutine is not callable for constant STScalar field')
  RETURN
END IF

IF (SIZE(VALUE) .NE. SIZE(globalNode)) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
 & 'Size of value should be equal to size of globalNode')
  RETURN
END IF

localNode = obj%domain%getLocalNodeNumber(globalNode)

IF (ANY(localNode .GT. obj%domain%getTotalNodes())) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
 & 'Some of the global node num are out of bound')
  RETURN
END IF

vecPointer => getPointer(obj%realVec, obj%dof, timeCompo)
IF (abool) THEN
  vecPointer(localNode) = vecPointer(localNode) + areal * VALUE
ELSE
  vecPointer(localNode) = VALUE
END IF
vecPointer => NULL()

END PROCEDURE stsField_set9

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_set10
REAL(DFP), POINTER :: vecPointer(:)
CHARACTER(*), PARAMETER :: myName = "stsField_set9"
INTEGER(I4B) :: idof
INTEGER(I4B) :: localNode
REAL(DFP) :: areal
LOGICAL(LGT) :: abool
areal = Input(option=scale, default=1.0_DFP)
abool = Input(option=addContribution, default=.FALSE.)

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
 & 'STScalar field object is not initiated')
  RETURN
END IF

IF (timeCompo .GT. obj%timeCompo) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
 & 'given timeCompo should be less than or equal to obj%timeCompo')
  RETURN
END IF

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
 & 'This subroutine is not callable for constant STScalar field')
  RETURN
END IF

localNode = obj%domain%getLocalNodeNumber(globalNode)

IF (localNode .GT. obj%domain%getTotalNodes()) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
 & 'The given global node num are out of bound')
  RETURN
END IF

vecPointer => getPointer(obj%realVec, obj%dof, timeCompo)
IF (abool) THEN
  vecPointer(localNode) = vecPointer(localNode) + areal * VALUE
ELSE
  vecPointer(localNode) = VALUE
END IF
vecPointer => NULL()

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
CALL obj%set(globalNode=globalNode, VALUE=VALUE, &
  & scale=scale, addContribution=addContribution)
END PROCEDURE stsField_set12

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_set13
CHARACTER(*), PARAMETER :: myName = "stsField_set13"
INTEGER(I4B) :: localNode(SIZE(globalNode))

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
 & 'Scalar field object is not initiated')
  RETURN
END IF

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
 & 'This routine should not be called for constant STScalar field')
  RETURN
END IF

IF (SIZE(VALUE, 1) .NE. obj%timeCompo .OR. &
  & SIZE(VALUE, 2) .NE. SIZE(globalNode)) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
      & 'SIZE( value, 1 ) not equal timeCompo or SIZE( value, 2 ) ' &
      & //'not equal to the SIZE(globalNode)')
  RETURN
END IF

localNode = obj%domain%getLocalNodeNumber(globalNode)

IF (ANY(localNode .GT. obj%tSize)) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
 & 'Some of the globalNode are out of bound')
  RETURN
END IF

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
IF (PRESENT(addContribution)) THEN
  CALL Add(obj=obj%realvec, VALUE=VALUE, scale=scale)
ELSE
  CALL Set(obj=obj%realvec, VALUE=VALUE)
END IF
END PROCEDURE stsField_set14

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_set15
CHARACTER(*), PARAMETER :: myName = "stsField_set15"
INTEGER(I4B) :: tsize
INTEGER(I4B) :: tsize_value
INTEGER(I4B) :: ii
INTEGER(I4B) :: indx1
INTEGER(I4B) :: indx2
REAL(DFP) :: avar

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'STScalarField_::obj is not initiated')
  RETURN
END IF

IF (.NOT. VALUE%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'AbstractNodeField_ ::value is not initiated')
  RETURN
END IF

tsize = obj%dof.tNodes. [ivar, idof]
tsize_value = VALUE%dof.tNodes. [ivar_value, idof_value]
IF (tsize .NE. tsize_value) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'tSize of obj(ivar, idof) is equal to value(ivar_value, idof_value)')
  RETURN
END IF

DO ii = 1, tsize
  indx1 = GetNodeLoc(&
    & obj=VALUE%dof, &
    & nodenum=ii, &
    & ivar=ivar_value, &
    & idof=idof_value)
  CALL VALUE%GetSingle(VALUE=avar, indx=indx1)
  indx2 = GetNodeLoc(&
    & obj=obj%dof, &
    & nodenum=ii, &
    & ivar=ivar, &
    & idof=idof)
  CALL obj%SetSingle(VALUE=avar, indx=indx2, scale=scale, &
    & addContribution=addContribution)
END DO

END PROCEDURE stsField_set15

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods
