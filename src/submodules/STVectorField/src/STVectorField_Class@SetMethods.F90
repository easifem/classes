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
CHARACTER(*), PARAMETER :: myName = "stvField_set1()"
INTEGER(I4B) :: localNode
REAL(DFP) :: areal
LOGICAL(LGT) :: abool

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

areal = Input(option=scale, default=1.0_DFP)
abool = Input(option=addContribution, default=.FALSE.)

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'STVectorField_::obj is not initiated')
  RETURN
END IF

IF (ANY(SHAPE(VALUE) .NE. [obj%spaceCompo, obj%timeCompo])) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
   & 'The shape of value is not compatible, it should be equal'// &
   & ' to [obj%spaceCompo, obj%timeCompo]')
  RETURN
END IF

localNode = obj%domain%getLocalNodeNumber(globalNode)

IF (localNode .EQ. 0) THEN
  CALL e%RaiseError(modName//'::'//myName//" - " &
    & //'globalNode :: '//tostring(globalNode) &
    & //" is out of bound for the domain.")
  RETURN
END IF

IF (abool) THEN

  CALL add( &
    & obj=obj%realVec, &
    & dofobj=obj%dof, &
    & nodenum=[localNode], &
    & VALUE=RESHAPE(VALUE, [SIZE(VALUE)]), &
    & conversion=[NONE], &
    & scale=areal)

ELSE

  CALL set( &
    & obj=obj%realVec, &
    & dofobj=obj%dof, &
    & nodenum=[localNode], &
    & VALUE=RESHAPE(VALUE, [SIZE(VALUE)]), &
    & conversion=[NONE])

END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE stvField_set1

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set2
REAL(DFP), POINTER :: vecPointer(:)
CHARACTER(*), PARAMETER :: myName = "stvField_set2"
INTEGER(I4B) :: ii, aa, idof
REAL(DFP) :: areal
LOGICAL(LGT) :: abool

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

areal = Input(option=scale, default=1.0_DFP)
abool = Input(option=addContribution, default=.FALSE.)

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'STVectorField_::obj is not initiated')
  RETURN
END IF

IF (ANY(SHAPE(VALUE) .NE. [obj%spaceCompo, obj%timeCompo])) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'The shape of value is not compatible, it should be equal'// &
    & ' to [obj%spaceCompo, obj%timeCompo]')
  RETURN
END IF

idof = 0

IF (abool) THEN

  DO aa = 1, obj%timeCompo
    DO ii = 1, obj%spaceCompo
      idof = idof + 1
      vecPointer => getPointer(obj%realVec, obj%dof, idof)
      vecPointer = vecPointer + areal * VALUE(ii, aa)
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

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE stvField_set2

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set3
REAL(DFP), POINTER :: vecPointer(:)
CHARACTER(*), PARAMETER :: myName = "stvField_set3"
INTEGER(I4B) :: idof
REAL(DFP) :: areal
LOGICAL(LGT) :: abool

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

areal = Input(option=scale, default=1.0_DFP)
abool = Input(option=addContribution, default=.FALSE.)

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'STVectorField_::obj is not initiated')
  RETURN
END IF

IF (ANY([spaceCompo, timeCompo]  &
  & .GT. [obj%spaceCompo, obj%timeCompo])) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'given spaceCompo and timeCompo should be less than or equal'// &
    & ' to obj%spaceCompo and obj%timeCompo')
  RETURN
END IF

idof = (timeCompo - 1) * obj%spaceCompo + spaceCompo
vecPointer => getPointer(obj%realVec, obj%dof, idof)

IF (abool) THEN
  vecPointer = vecPointer + areal * VALUE
ELSE
  vecPointer = VALUE
END IF

vecPointer => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE stvField_set3

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set4
CHARACTER(*), PARAMETER :: myName = "stvField_set4"
INTEGER(I4B) :: tnodes
REAL(DFP), ALLOCATABLE :: vec(:)
REAL(DFP) :: areal
LOGICAL(LGT) :: abool

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

areal = Input(option=scale, default=1.0_DFP)
abool = Input(option=addContribution, default=.FALSE.)

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'STVectorField_::obj is not initiated')
  RETURN
END IF

tnodes = obj%domain%getTotalNodes()

IF (ANY(SHAPE(VALUE)  &
  & .NE. [obj%spaceCompo, obj%timeCompo, tNodes])) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'The shape of value is not compatible')
  RETURN
END IF

vec = RESHAPE(VALUE, [SIZE(VALUE)])

IF (abool) THEN
  CALL Add(obj=obj%realVec, VALUE=vec, scale=areal)
ELSE
  CALL Set(obj=obj%realVec, VALUE=vec)
END IF

IF (ALLOCATED(vec)) DEALLOCATE (vec)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE stvField_set4

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set5
REAL(DFP), POINTER :: vecPointer(:)
CHARACTER(*), PARAMETER :: myName = "stvField_set5"
INTEGER(I4B) :: idof
REAL(DFP) :: areal
LOGICAL(LGT) :: abool

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

areal = Input(option=scale, default=1.0_DFP)
abool = Input(option=addContribution, default=.FALSE.)

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'STVectorField_::obj is not initiated')
  RETURN
END IF

IF (ANY([spaceCompo, timeCompo]  &
  & .GT. [obj%spaceCompo, obj%timeCompo])) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'given spaceCompo and timeCompo should be less than or equal'// &
    & ' to obj%spaceCompo and obj%timeCompo')
  RETURN
END IF
!
IF (SIZE(VALUE) .NE. obj%domain%getTotalNodes()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'Size of value should be equal to the total number of nodes')
  RETURN
END IF

idof = (timeCompo - 1) * obj%spaceCompo + spaceCompo
vecPointer => getPointer(obj%realVec, obj%dof, idof)

IF (abool) THEN
  vecPointer = vecPointer + areal * VALUE
ELSE
  vecPointer = VALUE
END IF

vecPointer => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER
END PROCEDURE stvField_set5

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set6
REAL(DFP), POINTER :: vecPointer(:)
REAL(DFP) :: vec(1)
CHARACTER(*), PARAMETER :: myName = "stvField_set6"
INTEGER(I4B) :: idof
REAL(DFP) :: areal
LOGICAL(LGT) :: abool

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

areal = Input(option=scale, default=1.0_DFP)
abool = Input(option=addContribution, default=.FALSE.)

IF (.NOT. obj%isInitiated .OR. .NOT. VALUE%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'STVectorField_::obj is not initiated')
  RETURN
END IF

IF (ANY([spaceCompo, timeCompo]  &
  & .GT. [obj%spaceCompo, obj%timeCompo])) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'given spaceCompo and timeCompo should be less than or equal'// &
    & ' to obj%spaceCompo and obj%timeCompo')
  RETURN
END IF

IF (VALUE%domain%getTotalNodes() .NE. obj%domain%getTotalNodes()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'Size of value should be equal to the total number of nodes')
  RETURN
END IF

SELECT TYPE (VALUE)
TYPE is (ScalarField_)
  IF (VALUE%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
    idof = (timeCompo - 1) * obj%spaceCompo + spaceCompo
    vecPointer => getPointer(obj%realVec, obj%dof, idof)
    vec = get(obj=VALUE%realVec, nodenum=[1], datatype=1.0_DFP)
    IF (abool) THEN
      vecPointer = vecPointer + areal * vec(1)
    ELSE
      vecPointer = vec(1)
    END IF
    vecPointer => NULL()
    RETURN
  END IF

  idof = (timeCompo - 1) * obj%spaceCompo + spaceCompo
  vecPointer => getPointer(obj%realVec, obj%dof, idof)
  IF (abool) THEN
    vecPointer = vecPointer + areal * get(VALUE%realVec, 1.0_DFP)
  ELSE
    vecPointer = get(VALUE%realVec, 1.0_DFP)
  END IF
  vecPointer => NULL()
  RETURN
CLASS DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & 'No case found for the type of VALUE')
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

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
REAL(DFP) :: areal
LOGICAL(LGT) :: abool

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

areal = Input(option=scale, default=1.0_DFP)
abool = Input(option=addContribution, default=.FALSE.)

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'Scalar field object is not initiated')
  RETURN
END IF

IF (ANY(SHAPE(VALUE) .NE. [obj%spaceCompo, obj%timeCompo, &
  & SIZE(globalNode)])) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'Incompatible shape and size of value')
  RETURN
END IF

localNode = obj%domain%getLocalNodeNumber(globalNode)

IF (ANY(localNode .EQ. 0)) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
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

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE stvField_set8

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set9
REAL(DFP), POINTER :: vecPointer(:)
CHARACTER(*), PARAMETER :: myName = "stvField_set9"
INTEGER(I4B) :: idof
INTEGER(I4B) :: localNode(SIZE(globalNode))
REAL(DFP) :: areal
LOGICAL(LGT) :: abool

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

areal = Input(option=scale, default=1.0_DFP)
abool = Input(option=addContribution, default=.FALSE.)

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'STVectorField_::obj is not initiated')
  RETURN
END IF

IF (ANY([spaceCompo, timeCompo] .GT. [obj%spaceCompo, obj%timeCompo])) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'given spaceCompo and timeCompo should be less than or equal'// &
    & ' to obj%spaceCompo and obj%timeCompo')
  RETURN
END IF

IF (SIZE(VALUE) .NE. SIZE(globalNode)) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'Size of value should be equal to size of globalNode')
  RETURN
END IF

localNode = obj%domain%getLocalNodeNumber(globalNode)

IF (ANY(localNode .GT. obj%domain%getTotalNodes())) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'Some of the global node num are out of bound')
  RETURN
END IF

idof = (timeCompo - 1) * obj%spaceCompo + spaceCompo
vecPointer => getPointer(obj%realVec, obj%dof, idof)

IF (abool) THEN
  vecPointer(localNode) = vecPointer(localNode) + areal * VALUE
ELSE
  vecPointer(localNode) = VALUE
END IF

vecPointer => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER
END PROCEDURE stvField_set9

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set10
REAL(DFP), POINTER :: vecPointer(:)
CHARACTER(*), PARAMETER :: myName = "stvField_set9"
INTEGER(I4B) :: idof
INTEGER(I4B) :: localNode
REAL(DFP) :: areal
LOGICAL(LGT) :: abool

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

areal = Input(option=scale, default=1.0_DFP)
abool = Input(option=addContribution, default=.FALSE.)

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'STVectorField_::obj is not initiated')
  RETURN
END IF

IF (ANY([spaceCompo, timeCompo] .GT. [obj%spaceCompo, obj%timeCompo])) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'given spaceCompo and timeCompo should be less than or equal'// &
    & ' to obj%spaceCompo and obj%timeCompo')
  RETURN
END IF

localNode = obj%domain%getLocalNodeNumber(globalNode)

IF (localNode .GT. obj%domain%getTotalNodes()) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'The given global node num are out of bound')
  RETURN
END IF

idof = (timeCompo - 1) * obj%spaceCompo + spaceCompo
vecPointer => getPointer(obj%realVec, obj%dof, idof)

IF (abool) THEN
  vecPointer(localNode) = vecPointer(localNode) + areal * VALUE
ELSE
  vecPointer(localNode) = VALUE
END IF

vecPointer => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE stvField_set10

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set11
CHARACTER(*), PARAMETER :: myName = "stvField_set11"
INTEGER(I4B) :: globalNode(INT(1 + (iend - istart) / stride)), ii, jj

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

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

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE stvField_set11

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set12
CHARACTER(*), PARAMETER :: myName = "stvField_set12()"
INTEGER(I4B) :: globalNode(INT(1 + (iend - istart) / stride)), ii, jj

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

jj = 0
DO ii = istart, iend, stride
  jj = jj + 1
  globalNode(jj) = ii
END DO
CALL obj%set(globalNode=globalNode, VALUE=VALUE, scale=scale,  &
  & addContribution=addContribution)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE stvField_set12

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set13
CHARACTER(*), PARAMETER :: myName = "stvField_set13()"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

SELECT CASE (VALUE%vartype)
CASE (SpaceTime)
  CALL obj%Set(VALUE=GET(VALUE, TypeFEVariableVector, &
    & TypeFEVariableSpaceTime), &
    & globalNode=globalNode, &
    & scale=scale, &
    & addContribution=addContribution)
CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: No case found for Value%vartype '//  &
    & ' only SpaceTime allowed')
  RETURN
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER
END PROCEDURE stvField_set13

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set14
CHARACTER(*), PARAMETER :: myName = "stvField_set14"
REAL(DFP) :: areal
LOGICAL(LGT) :: abool
areal = Input(option=scale, default=1.0_DFP)
abool = Input(option=addContribution, default=.FALSE.)

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'STVectorField_::obj is not initiated')
  RETURN
END IF

IF (abool) THEN
  CALL Add(obj=obj%realvec, VALUE=VALUE, scale=areal)
ELSE
  CALL Set(obj=obj%realvec, VALUE=VALUE)
END IF
END PROCEDURE stvField_set14

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set15
CHARACTER(*), PARAMETER :: myName = "stvField_set15"
INTEGER(I4B) :: tsize
INTEGER(I4B) :: tsize_value
INTEGER(I4B) :: ii
INTEGER(I4B) :: indx1
INTEGER(I4B) :: indx2
REAL(DFP) :: avar

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'STVectorField_::obj is not initiated')
END IF

IF (.NOT. VALUE%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'AbstractNodeField_ ::value is not initiated')
END IF

tsize = obj%dof.tNodes. [ivar, idof]
tsize_value = VALUE%dof.tNodes. [ivar_value, idof_value]
IF (tsize .NE. tsize_value) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & 'tSize of obj(ivar, idof) is equal to value(ivar_value, idof_value)')
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

END PROCEDURE stvField_set15

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set16
CHARACTER(*), PARAMETER :: myName = "stvField_set16()"
INTEGER(I4B) :: ii, aint, bint
LOGICAL(LGT) :: problem

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: STVectorField_::obj is not initiated')
  RETURN
END IF

aint = SIZE(spaceCompo)
problem = (aint .GT. obj%spaceCompo) .OR. (timeCompo .GT. obj%timeCompo)
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: Size of spaceCompo and given timeCompo'// &
    & ' should be less than or equal'// &
    & ' to obj%spaceCompo and obj%timeCompo')
  RETURN
END IF

bint = obj%domain%GetTotalNodes()
problem = (SIZE(VALUE, 1) .NE. aint) .OR. (SIZE(VALUE, 2) .NE. bint)
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: The shape of value is not compatible')
  RETURN
END IF

DO ii = 1, aint
  CALL obj%Set(VALUE=VALUE(ii, :), spaceCompo=spaceCompo(ii),  &
    & timeCompo=timeCompo, addContribution=addContribution, scale=scale)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE stvField_set16

!----------------------------------------------------------------------------
!                                                                    set
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_set17
CHARACTER(*), PARAMETER :: myName = "stvField_set17()"
INTEGER(I4B) :: ii, aint, bint
LOGICAL(LGT) :: problem

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: STVectorField_::obj is not initiated')
  RETURN
END IF

bint = SIZE(timeCompo)
problem = (spaceCompo .GT. obj%spaceCompo) .OR. (bint .GT. obj%timeCompo)

IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: given spaceCompo and size of timeCompo'// &
    & ' should be less than or equal'// &
    & ' to obj%spaceCompo and obj%timeCompo')
  RETURN
END IF

aint = obj%domain%GetTotalNodes()
problem = (SIZE(VALUE, 1) .NE. obj%timeCompo) .OR. (SIZE(VALUE, 2) .NE. aint)

IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: The shape of value is not compatible')
  RETURN
END IF

DO ii = 1, bint
  CALL obj%Set(VALUE=VALUE(ii, :), spaceCompo=spaceCompo,  &
    & timeCompo=timeCompo(ii), addContribution=addContribution,  &
    & scale=scale)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

END PROCEDURE stvField_set17

END SUBMODULE SetMethods
