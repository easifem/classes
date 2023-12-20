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

SUBMODULE(VectorField_Class) SetMethods
USE BaseMethod
USE ScalarField_Class
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set1
CHARACTER(*), PARAMETER :: myName = "obj_Set1"
INTEGER(I4B) :: localNode

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'VectorField_::obj is not initiated')
END IF

IF (SIZE(VALUE) .NE. obj%spaceCompo) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Size of value should be equal to obj%spaceCompo')
END IF

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN

  IF (PRESENT(addContribution)) THEN
    CALL add( &
      & obj=obj%realVec, &
      & dofobj=obj%dof, &
      & nodenum=[1], &
      & VALUE=VALUE, &
      & conversion=[NONE], &
      & scale=scale)
  ELSE
    CALL set( &
      & obj=obj%realVec, &
      & dofobj=obj%dof, &
      & nodenum=[1], &
      & VALUE=VALUE, &
      & conversion=[NONE])
  END IF

ELSE

  localNode = obj%domain%getLocalNodeNumber(globalNode)

  IF (localNode .EQ. 0_I4B) THEN
    CALL e%raiseError(modName//'::'//myName//" - " &
      & //'globalNode :: '//tostring(globalNode) &
      & //" is out of bound for the domain.")
  END IF

  IF (PRESENT(addContribution)) THEN

    CALL add( &
      & obj=obj%realVec, &
      & dofobj=obj%dof, &
      & nodenum=[localNode], &
      & VALUE=VALUE, &
      & conversion=[NONE], &
      & scale=scale)

  ELSE

    CALL set( &
      & obj=obj%realVec, &
      & dofobj=obj%dof, &
      & nodenum=[localNode], &
      & VALUE=VALUE, &
      & conversion=[NONE])

  END IF

END IF

END PROCEDURE obj_Set1

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set2
REAL(DFP), POINTER :: vecPointer(:)
CHARACTER(*), PARAMETER :: myName = "obj_Set2"
INTEGER(I4B) :: idof

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Vector field object is not initiated')
END IF

IF (SIZE(VALUE) .NE. obj%spaceCompo) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'size(value) should be same as obj%spaceCompo')
END IF

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  IF (PRESENT(addContribution)) THEN
    CALL add( &
      & obj=obj%realVec, &
      & dofobj=obj%dof, &
      & nodenum=[1], &
      & VALUE=VALUE, &
      & conversion=[NONE], &
      & scale=scale)
  ELSE
    CALL set( &
      & obj=obj%realVec, &
      & dofobj=obj%dof, &
      & nodenum=[1], &
      & VALUE=VALUE, &
      & conversion=[NONE])
  END IF
ELSE
  vecPointer => NULL()
  IF (PRESENT(addContribution)) THEN
    DO idof = 1, obj%spaceCompo
      vecPointer => getPointer(obj%realVec, obj%dof, idof)
      vecPointer = vecPointer + scale * VALUE(idof)
    END DO
  ELSE
    DO idof = 1, obj%spaceCompo
      vecPointer => getPointer(obj%realVec, obj%dof, idof)
      vecPointer = VALUE(idof)
    END DO
  END IF
  vecPointer => NULL()
END IF
END PROCEDURE obj_Set2

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set3
REAL(DFP), POINTER :: vecPointer(:)
CHARACTER(*), PARAMETER :: myName = "obj_Set3"
INTEGER(I4B) :: idof

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Vector field object is not initiated')
END IF

IF (spaceCompo .GT. obj%spaceCompo) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'given spaceCompo should be less than or equal to obj%spaceCompo')
END IF

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  CALL set(obj%realVec, obj%dof, [1], [VALUE], spaceCompo)
ELSE
  vecPointer => getPointer(obj%realVec, obj%dof, spaceCompo)
  vecPointer = VALUE
  vecPointer => NULL()
END IF

END PROCEDURE obj_Set3

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set4
CHARACTER(*), PARAMETER :: myName = "obj_Set4"
INTEGER(I4B) :: ii, tnodes, aa, jj

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
   & 'Vector field object is not initiated')
END IF

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
   & 'This subroutine is not callable for constant vector field')
END IF

tnodes = obj%domain%getTotalNodes()

IF (SIZE(VALUE, 2) .NE. tnodes .OR. SIZE(VALUE, 1) .NE. obj%spaceCompo) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'The shape of value should be [ ' &
    & //tostring(obj%spaceCompo) &
    & //', ' &
    & //tostring(tnodes) &
    & //' ]')
END IF

aa = 0
IF (PRESENT(addContribution)) THEN
  DO ii = 1, tnodes
    DO jj = 1, obj%spaceCompo
      aa = aa + 1
      obj%realVec%val(aa) = obj%realVec%val(aa) + scale * VALUE(jj, ii)
    END DO
  END DO
ELSE
  DO ii = 1, tnodes
    DO jj = 1, obj%spaceCompo
      aa = aa + 1
      obj%realVec%val(aa) = VALUE(jj, ii)
    END DO
  END DO
END IF

END PROCEDURE obj_Set4

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set5
REAL(DFP), POINTER :: vecPointer(:)
CHARACTER(*), PARAMETER :: myName = "obj_Set5"

IF (.NOT. obj%isInitiated) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'VectorField_::obj is not initiated')

IF (spaceCompo .GT. obj%spaceCompo) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'given spaceCompo should be less than or equal to obj%spaceCompo')

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'This subroutine is not callable for constant vector field')

IF (SIZE(VALUE) .NE. obj%domain%getTotalNodes()) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Size of value should be equal to the total number of nodes')

vecPointer => getPointer(obj%realVec, obj%dof, spaceCompo)

IF (PRESENT(addContribution)) THEN
  vecPointer = vecPointer + scale * VALUE
ELSE
  vecPointer = VALUE
END IF

vecPointer => NULL()
END PROCEDURE obj_Set5

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set6
REAL(DFP), POINTER :: vecPointer(:)
CHARACTER(*), PARAMETER :: myName = "obj_Set5"

IF (.NOT. obj%isInitiated .OR. .NOT. VALUE%isInitiated) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Vector field object is not initiated')

IF (spaceCompo .GT. obj%spaceCompo) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'given spaceCompo should be less than or equal to obj%spaceCompo')

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'This subroutine is not callable for constant vector field')

SELECT TYPE (VALUE)
TYPE IS (ScalarField_)
  IF (VALUE%domain%getTotalNodes() .NE. obj%domain%getTotalNodes()) &
    & CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'Size of value should be equal to the total number of nodes')

  IF (VALUE%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
    vecPointer => getPointer(VALUE%realVec, VALUE%dof, 1)
    CALL obj%set(VALUE=vecPointer(1), spaceCompo=spaceCompo, &
      & scale=scale, addContribution=addContribution)
  ELSE
    vecPointer => getPointer(obj%realVec, obj%dof, spaceCompo)
    IF (PRESENT(addContribution)) THEN
      vecPointer = vecPointer + get(VALUE%realVec, 1.0_DFP)
    ELSE
      vecPointer = get(VALUE%realVec, 1.0_DFP)
    END IF
    vecPointer => NULL()
  END IF
CLASS DEFAULT
  CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'No case found for the type of value.')
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

IF (.NOT. obj%isInitiated) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Scalar field object is not initiated')

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'This routine should not be called for constant vector field')

IF (SIZE(VALUE, 1) .NE. obj%spaceCompo .OR. &
  & SIZE(VALUE, 2) .NE. SIZE(globalNode)) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'SIZE( value, 1 ) not equal spaceCompo or SIZE( value, 2 ) not &
  & equal to the SIZE(globalNode)')

localNode = obj%domain%getLocalNodeNumber(globalNode)

IF (ANY(localNode .GT. obj%tSize)) &
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
END PROCEDURE obj_Set8

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set9
REAL(DFP), POINTER :: vecPointer(:)
CHARACTER(*), PARAMETER :: myName = "obj_Set9"
INTEGER(I4B) :: localNode(SIZE(globalNode))

IF (.NOT. obj%isInitiated) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Vector field object is not initiated')

IF (.NOT. ASSOCIATED(obj%domain)) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'obj%domain is not associated')

IF (spaceCompo .GT. obj%spaceCompo) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'given spaceCompo should be less than or equal to obj%spaceCompo')

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'This subroutine is not callable for constant vector field')

IF (SIZE(VALUE) .NE. SIZE(globalNode)) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Size of value should be equal to size of globalNode')

localNode = obj%domain%getLocalNodeNumber(globalNode)

IF (ANY(localNode .GT. obj%domain%getTotalNodes())) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Some of the global node num are out of bound')

vecPointer => getPointer(obj%realVec, obj%dof, spaceCompo)

IF (PRESENT(addContribution)) THEN
  vecPointer(localNode) = vecPointer(localNode) + scale * VALUE
ELSE
  vecPointer(localNode) = VALUE
END IF

vecPointer => NULL()
END PROCEDURE obj_Set9

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set10
REAL(DFP), POINTER :: vecPointer(:)
CHARACTER(*), PARAMETER :: myName = "obj_Set9"
INTEGER(I4B) :: localNode

IF (.NOT. obj%isInitiated) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Vector field object is not initiated')

IF (spaceCompo .GT. obj%spaceCompo) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'given spaceCompo should be less than or equal to obj%spaceCompo')

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'This subroutine is not callable for constant vector field')

localNode = obj%domain%getLocalNodeNumber(globalNode)

IF (localNode .GT. obj%domain%getTotalNodes()) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'The given global node num are out of bound')

vecPointer => getPointer(obj%realVec, obj%dof, spaceCompo)

IF (PRESENT(addContribution)) THEN
  vecPointer(localNode) = vecPointer(localNode) + scale * VALUE
ELSE
  vecPointer(localNode) = VALUE
END IF

vecPointer => NULL()
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

IF (SIZE(VALUE, 1) .NE. obj%spaceCompo .OR. &
  & SIZE(VALUE, 2) .NE. SIZE(globalNode)) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'SIZE( value, 1 ) not equal spaceCompo or SIZE( value, 2 ) not &
  & equal to the SIZE(globalNode)')

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
IF (PRESENT(addContribution)) THEN
  CALL Add(obj=obj%realvec, VALUE=VALUE, scale=scale)
ELSE
  CALL Set(obj=obj%realvec, VALUE=VALUE)
END IF
END PROCEDURE obj_Set14

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set15
CHARACTER(*), PARAMETER :: myName = "obj_Set15"
INTEGER(I4B) :: tsize
INTEGER(I4B) :: tsize_value
INTEGER(I4B) :: ii
INTEGER(I4B) :: indx1
INTEGER(I4B) :: indx2
REAL(DFP) :: avar

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'VectorField_::obj is not initiated')
END IF

IF (.NOT. VALUE%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'AbstractNodeField_ ::value is not initiated')
END IF

tsize = obj%dof.tNodes. [ivar, idof]
tsize_value = VALUE%dof.tNodes. [ivar_value, idof_value]
IF (tsize .NE. tsize_value) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
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

END PROCEDURE obj_Set15

!----------------------------------------------------------------------------
!                                                                     Set16
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set16
CHARACTER(*), PARAMETER :: myName = "obj_Set16()"
#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

CALL Set(obj=obj%realVec, VALUE=VALUE%realVec)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif
END PROCEDURE obj_Set16

!----------------------------------------------------------------------------
!                                                                      Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetByFunction
CHARACTER(*), PARAMETER :: myName = "obj_SetByFunction()"
LOGICAL(LGT) :: istimes, problem
INTEGER(I4B) :: ttime, returnType, nsd, tnodes, ii, globalNode(1)
REAL(DFP) :: args(4), xij(3, 1)
REAL(DFP), ALLOCATABLE :: VALUE(:)
INTEGER(I4B), PARAMETER :: needed_returnType = Vector
CLASS(Domain_), POINTER :: dom

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

istimes = PRESENT(times)
problem = .FALSE.

args = 0.0_DFP
IF (istimes) THEN
  ttime = SIZE(times)
  args(4) = times(1)
  problem = ttime .NE. 1_I4B
END IF

IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: times size should be 1.')
  RETURN
END IF

returnType = func%GetReturnType()
problem = returnType .NE. needed_returnType

IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: Return type of function is not correct.')
  RETURN
END IF

dom => NULL()
dom => obj%domain
problem = .NOT. ASSOCIATED(dom)
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: domain is not ASSOCIATED.')
  RETURN
END IF

nsd = dom%GetNSD()
tnodes = dom%GetTotalNodes()

DO ii = 1, tnodes
  globalNode = ii
  CALL dom%GetNodeCoord(globalNode=globalNode, nodeCoord=xij(1:nsd, 1:1))
  args(1:nsd) = xij(1:nsd, 1)
  CALL func%Get(val=VALUE, args=args)
  CALL obj%Set(globalNode=globalNode(1), VALUE=VALUE)
END DO

IF (ALLOCATED(VALUE)) DEALLOCATE (VALUE)
NULLIFY (dom)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif
END PROCEDURE obj_SetByFunction

END SUBMODULE SetMethods
