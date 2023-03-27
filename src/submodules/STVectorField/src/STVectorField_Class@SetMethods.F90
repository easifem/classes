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

SUBMODULE(STVectorField_Class) SetMethods
USE BaseMethod
USE Field
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set1
CHARACTER(*), PARAMETER :: myName = "stvField_set1"
INTEGER(I4B) :: localNode

IF (.NOT. obj%isInitiated) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'STVectorField_::obj is not initiated')

IF (ANY(SHAPE(VALUE) .NE. [obj%spaceCompo, obj%timeCompo])) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'The shape of value is not compatible, it should be equal'// &
  & ' to [obj%spaceCompo, obj%timeCompo]')

localNode = obj%domain%getLocalNodeNumber(globalNode)

IF (localNode .EQ. 0) THEN
  CALL e%raiseError(modName//'::'//myName//" - " &
    & //'globalNode :: '//tostring(globalNode) &
    & //" is out of bound for the domain.")
END IF

IF (PRESENT(addContribution)) THEN

  CALL add( &
    & obj=obj%realVec, &
    & dofobj=obj%dof, &
    & nodenum=[localNode], &
    & VALUE=RESHAPE(VALUE, [SIZE(VALUE)]), &
    & conversion=[NONE], &
    & scale=scale)

ELSE

  CALL set( &
    & obj=obj%realVec, &
    & dofobj=obj%dof, &
    & nodenum=[localNode], &
    & VALUE=RESHAPE(VALUE, [SIZE(VALUE)]), &
    & conversion=[NONE])

END IF

END PROCEDURE stvField_set1

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set2
REAL(DFP), POINTER :: vecPointer(:)
CHARACTER(*), PARAMETER :: myName = "stvField_set2"
INTEGER(I4B) :: ii, aa, idof

IF (.NOT. obj%isInitiated) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'STVectorField_::obj is not initiated')

IF (ANY(SHAPE(VALUE) .NE. [obj%spaceCompo, obj%timeCompo])) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'The shape of value is not compatible, it should be equal'// &
  & ' to [obj%spaceCompo, obj%timeCompo]')

idof = 0

IF (PRESENT(addContribution)) THEN

  DO aa = 1, obj%timeCompo
    DO ii = 1, obj%spaceCompo
      idof = idof + 1
      vecPointer => getPointer(obj%realVec, obj%dof, idof)
      vecPointer = vecPointer + scale * VALUE(ii, aa)
    END DO
  END DO

ELSE

  DO aa = 1, obj%timeCompo
    DO ii = 1, obj%spaceCompo
      idof = idof + 1
      vecPointer => getPointer(obj%realVec, obj%dof, idof)
      vecPointer = VALUE(ii, aa)
    END DO
  END DO

END IF

vecPointer => NULL()
END PROCEDURE stvField_set2

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set3
REAL(DFP), POINTER :: vecPointer(:)
CHARACTER(*), PARAMETER :: myName = "stvField_set3"
INTEGER(I4B) :: idof

IF (.NOT. obj%isInitiated) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'STVectorField_::obj is not initiated')

IF (ANY([spaceCompo, timeCompo] .GT. [obj%spaceCompo, obj%timeCompo])) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'given spaceCompo and timeCompo should be less than or equal'// &
  & ' to obj%spaceCompo and obj%timeCompo')

idof = (timeCompo - 1) * obj%spaceCompo + spaceCompo
vecPointer => getPointer(obj%realVec, obj%dof, idof)

IF (PRESENT(addContribution)) THEN
  vecPointer = vecPointer + scale * VALUE
ELSE
  vecPointer = VALUE
END IF

vecPointer => NULL()
END PROCEDURE stvField_set3

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set4
CHARACTER(*), PARAMETER :: myName = "stvField_set4"
INTEGER(I4B) :: ii, tnodes, aa, jj
REAL(DFP), ALLOCATABLE :: vec(:)

IF (.NOT. obj%isInitiated) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'STVectorField_::obj is not initiated')

tnodes = obj%domain%getTotalNodes()

IF (ANY(SHAPE(VALUE) .NE. [obj%spaceCompo, obj%timeCompo, tNodes])) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'The shape of value is not compatible')

vec = RESHAPE(VALUE, [SIZE(VALUE)])

IF (PRESENT(addContribution)) THEN
  CALL Add(obj=obj%realVec, VALUE=vec, scale=scale)
ELSE
  CALL Set(obj=obj%realVec, VALUE=vec)
END IF

IF (ALLOCATED(vec)) DEALLOCATE (vec)
END PROCEDURE stvField_set4

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set5
REAL(DFP), POINTER :: vecPointer(:)
CHARACTER(*), PARAMETER :: myName = "stvField_set5"
INTEGER(I4B) :: idof

IF (.NOT. obj%isInitiated) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'STVectorField_::obj is not initiated')

IF (ANY([spaceCompo, timeCompo] .GT. [obj%spaceCompo, obj%timeCompo])) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'given spaceCompo and timeCompo should be less than or equal'// &
  & ' to obj%spaceCompo and obj%timeCompo')
!
IF (SIZE(VALUE) .NE. obj%domain%getTotalNodes()) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Size of value should be equal to the total number of nodes')

idof = (timeCompo - 1) * obj%spaceCompo + spaceCompo
vecPointer => getPointer(obj%realVec, obj%dof, idof)

IF (PRESENT(addContribution)) THEN
  vecPointer = vecPointer + scale * VALUE
ELSE
  vecPointer = VALUE
END IF

vecPointer => NULL()
END PROCEDURE stvField_set5

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set6
REAL(DFP), POINTER :: vecPointer(:)
REAL(DFP) :: vec(1)
CHARACTER(*), PARAMETER :: myName = "stvField_set6"
INTEGER(I4B) :: idof

IF (.NOT. obj%isInitiated .OR. .NOT. VALUE%isInitiated) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'STVectorField_::obj is not initiated')

IF (ANY([spaceCompo, timeCompo] .GT. [obj%spaceCompo, obj%timeCompo])) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'given spaceCompo and timeCompo should be less than or equal'// &
  & ' to obj%spaceCompo and obj%timeCompo')

IF (VALUE%domain%getTotalNodes() .NE. obj%domain%getTotalNodes()) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Size of value should be equal to the total number of nodes')

SELECT TYPE (VALUE)
TYPE is (ScalarField_)
  IF (VALUE%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN

    idof = (timeCompo - 1) * obj%spaceCompo + spaceCompo
    vecPointer => getPointer(obj%realVec, obj%dof, idof)
    vec = get(obj=VALUE%realVec, nodenum=[1], datatype=1.0_DFP)
    IF (PRESENT(addContribution)) THEN
      vecPointer = vecPointer + scale * vec(1)
    ELSE
      vecPointer = vec(1)
    END IF

  ELSE

    idof = (timeCompo - 1) * obj%spaceCompo + spaceCompo
    vecPointer => getPointer(obj%realVec, obj%dof, idof)
    IF (PRESENT(addContribution)) THEN
      vecPointer = vecPointer + scale * get(VALUE%realVec, 1.0_DFP)
    ELSE
      vecPointer = get(VALUE%realVec, 1.0_DFP)
    END IF

  END IF

  vecPointer => NULL()
CLASS DEFAULT
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'No case found for the type of VALUE')
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
INTEGER(I4B) :: localNode(SIZE(globalNode))
REAL(DFP) :: val(SIZE(VALUE))

IF (.NOT. obj%isInitiated) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Scalar field object is not initiated')

IF (ANY(SHAPE(VALUE) .NE. [obj%spaceCompo, obj%timeCompo, &
  & SIZE(globalNode)])) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Incompatible shape and size of value')

localNode = obj%domain%getLocalNodeNumber(globalNode)

IF (ANY(localNode .EQ. 0)) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Some of the globalNode are out of bound')

val = RESHAPE(VALUE, [SIZE(VALUE)])

IF (PRESENT(addContribution)) THEN
  CALL add( &
    & obj=obj%realVec, &
    & dofobj=obj%dof, &
    & nodenum=localNode, &
    & VALUE=val, &
    & conversion=[NONE], &
    & scale=scale)
ELSE
  CALL set( &
    & obj=obj%realVec, &
    & dofobj=obj%dof, &
    & nodenum=localNode, &
    & VALUE=val, &
    & conversion=[NONE])
END IF

END PROCEDURE stvField_set8

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set9
REAL(DFP), POINTER :: vecPointer(:)
CHARACTER(*), PARAMETER :: myName = "stvField_set9"
INTEGER(I4B) :: idof
INTEGER(I4B) :: localNode(SIZE(globalNode))

IF (.NOT. obj%isInitiated) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'STVectorField_::obj is not initiated')

IF (ANY([spaceCompo, timeCompo] .GT. [obj%spaceCompo, obj%timeCompo])) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'given spaceCompo and timeCompo should be less than or equal'// &
  & ' to obj%spaceCompo and obj%timeCompo')

IF (SIZE(VALUE) .NE. SIZE(globalNode)) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Size of value should be equal to size of globalNode')

localNode = obj%domain%getLocalNodeNumber(globalNode)

IF (ANY(localNode .GT. obj%domain%getTotalNodes())) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Some of the global node num are out of bound')

idof = (timeCompo - 1) * obj%spaceCompo + spaceCompo
vecPointer => getPointer(obj%realVec, obj%dof, idof)

IF (PRESENT(addContribution)) THEN
  vecPointer(localNode) = vecPointer(localNode) + scale * VALUE
ELSE
  vecPointer(localNode) = VALUE
END IF

vecPointer => NULL()
END PROCEDURE stvField_set9

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set10
REAL(DFP), POINTER :: vecPointer(:)
CHARACTER(*), PARAMETER :: myName = "stvField_set9"
INTEGER(I4B) :: idof
INTEGER(I4B) :: localNode

IF (.NOT. obj%isInitiated) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'STVectorField_::obj is not initiated')

IF (ANY([spaceCompo, timeCompo] .GT. [obj%spaceCompo, obj%timeCompo])) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'given spaceCompo and timeCompo should be less than or equal'// &
  & ' to obj%spaceCompo and obj%timeCompo')

localNode = obj%domain%getLocalNodeNumber(globalNode)

IF (localNode .GT. obj%domain%getTotalNodes()) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'The given global node num are out of bound')

idof = (timeCompo - 1) * obj%spaceCompo + spaceCompo
vecPointer => getPointer(obj%realVec, obj%dof, idof)

IF (PRESENT(addContribution)) THEN
  vecPointer(localNode) = vecPointer(localNode) + scale * VALUE
ELSE
  vecPointer(localNode) = VALUE
END IF

vecPointer => NULL()
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

IF (.NOT. obj%isInitiated) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'STVectorField_::obj is not initiated')

IF (PRESENT(addContribution)) THEN
  CALL Add(obj=obj%realvec, VALUE=VALUE, scale=scale)
ELSE
  CALL Set(obj=obj%realvec, VALUE=VALUE)
END IF
END PROCEDURE stvField_set14

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods
