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

SUBMODULE(BlockNodeField_Class) SetMethods
USE BaseMethod
USE Field
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set1
CHARACTER(*), PARAMETER :: myName = "obj_set1"

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'BlockNodeField_::obj is not initiated')
END IF

IF (PRESENT(addContribution)) THEN
  CALL add( &
    & obj%realVec, &
    & VALUE=VALUE, &
    & scale=INPUT(option=scale, default=1.0_DFP) &
    & )
ELSE
  CALL set(obj%realVec, VALUE=VALUE)
END IF
END PROCEDURE obj_set1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set2
CHARACTER(*), PARAMETER :: myName = "obj_set2"
INTEGER(I4B) :: tsize

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'BlockNodeField_::obj is not initiated')
END IF

tsize = obj%SIZE()

IF (tsize .NE. SIZE(VALUE)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'Size of obj should be same as size of value')
END IF

IF (PRESENT(addContribution)) THEN
  CALL add( &
    & obj%realVec, &
    & VALUE=VALUE, &
    & scale=INPUT(option=scale, default=1.0_DFP) &
    & )
ELSE
  CALL set(obj%realVec, VALUE=VALUE)
END IF
END PROCEDURE obj_set2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set3
CHARACTER(*), PARAMETER :: myName = "obj_set3"
INTEGER(I4B) :: localNode(1)

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'BlockNodeField_::obj is not initiated')
END IF

localNode(1) = obj%domains(ivar)%ptr%GetLocalNodeNumber(globalNode)

IF (localNode(1) .EQ. 0_I4B) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'globalNode is out of bound')
END IF

IF (PRESENT(addContribution)) THEN
  CALL Add( &
    & obj=obj%realVec, &
    & dofobj=obj%dof, &
    & nodenum=localNode, &
    & VALUE=[VALUE], &
    & scale=INPUT(option=scale, default=1.0_DFP), &
    & ivar=ivar, &
    & idof=idof)
ELSE
  CALL Set( &
    & obj=obj%realVec, &
    & dofobj=obj%dof, &
    & nodenum=localNode, &
    & VALUE=[VALUE], &
    & ivar=ivar, &
    & idof=idof)
END IF
END PROCEDURE obj_set3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set4
CHARACTER(*), PARAMETER :: myName = "obj_set4"
INTEGER(I4B) :: localNode(SIZE(globalNode))

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'BlockNodeField_::obj is not initiated')
END IF

localNode = obj%domains(ivar)%ptr%GetLocalNodeNumber(globalNode)

IF (ANY(localNode .EQ. 0_I4B)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'Some globalNodes are out of bound')
END IF

IF (PRESENT(addContribution)) THEN
  CALL add( &
    & obj=obj%realVec, &
    & nodenum=getIndex( &
      & obj=obj%dof, &
      & nodenum=localNode, &
      & ivar=ivar), &
    & VALUE=VALUE, &
    & scale=INPUT(option=scale, default=1.0_DFP) &
    & )
ELSE
  CALL set( &
    & obj=obj%realVec, &
    & nodenum=getIndex( &
      & obj=obj%dof, &
      & nodenum=localNode, &
      & ivar=ivar), &
    & VALUE=VALUE)
END IF
END PROCEDURE obj_set4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set5
CHARACTER(*), PARAMETER :: myName = "obj_set5"
INTEGER(I4B) :: localNode(SIZE(globalNode))

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'BlockNodeField_::obj is not initiated')
END IF

localNode = obj%domains(ivar)%ptr%GetLocalNodeNumber(globalNode)

IF (ANY(localNode .EQ. 0_I4B)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'Some globalNodes are out of bound')
END IF

! IF (SIZE(VALUE) .NE. SIZE(globalNode)) THEN
!   CALL e%raiseError(modName//'::'//myName//' - '// &
!     & 'The size of value not same as the size of globalNode.')
! END IF

IF (PRESENT(addContribution)) THEN
  CALL add( &
    & obj=obj%realVec, &
    & nodenum=getIndex( &
      & obj=obj%dof, &
      & nodenum=localNode, &
      & ivar=ivar), &
    & VALUE=VALUE, &
    & scale=INPUT(option=scale, default=1.0_DFP) &
    & )
ELSE
  CALL set( &
    & obj=obj%realVec, &
    & nodenum=getIndex( &
      & obj=obj%dof, &
      & nodenum=localNode, &
      & ivar=ivar), &
    & VALUE=VALUE)
END IF
END PROCEDURE obj_set5

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set6
CHARACTER(*), PARAMETER :: myName = "obj_set6"
INTEGER(I4B) :: localNode(SIZE(globalNode))

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'BlockNodeField_::obj is not initiated')
END IF

localNode = obj%domains(ivar)%ptr%GetLocalNodeNumber(globalNode)

IF (ANY(localNode .EQ. 0_I4B)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'Some globalNodes are out of bound')
END IF

IF (PRESENT(addContribution)) THEN
  CALL add( &
    & obj=obj%realVec, &
    & dofobj=obj%dof, &
    & nodenum=localNode, &
    & VALUE=VALUE, &
    & scale=INPUT(option=scale, default=1.0_DFP), &
    & ivar=ivar, &
    & idof=idof)
ELSE
  CALL Set( &
    & obj=obj%realVec, &
    & dofobj=obj%dof, &
    & nodenum=localNode, &
    & VALUE=VALUE, &
    & ivar=ivar, &
    & idof=idof)
END IF
END PROCEDURE obj_set6

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set7
CHARACTER(*), PARAMETER :: myName = "obj_set7"
INTEGER(I4B) :: localNode(SIZE(globalNode))

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'BlockNodeField_::obj is not initiated')
END IF

localNode = obj%domains(ivar)%ptr%GetLocalNodeNumber(globalNode)

IF (ANY(localNode .EQ. 0_I4B)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'Some globalNodes are out of bound')
END IF

IF (SIZE(VALUE) .NE. SIZE(globalNode)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'The size of value not same as the size of globalNode.')
END IF

IF (PRESENT(addContribution)) THEN
  CALL add( &
    & obj=obj%realVec, &
    & dofobj=obj%dof, &
    & nodenum=localNode, &
    & VALUE=VALUE, &
    & scale=INPUT(option=scale, default=1.0_DFP), &
    & ivar=ivar, &
    & idof=idof)
ELSE
  CALL Set( &
    & obj=obj%realVec, &
    & dofobj=obj%dof, &
    & nodenum=localNode, &
    & VALUE=VALUE, &
    & ivar=ivar, &
    & idof=idof)
END IF
END PROCEDURE obj_set7

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set8
CHARACTER(*), PARAMETER :: myName = "obj_set8"
INTEGER(I4B) :: localNode(SIZE(globalNode))

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'BlockNodeField_::obj is not initiated')
END IF

localNode = obj%domains(ivar)%ptr%GetLocalNodeNumber(globalNode)

IF (ANY(localNode .EQ. 0_I4B)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'Some globalNodes are out of bound')
END IF

IF (SIZE(VALUE) .NE. SIZE(globalNode)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'The size of value not same as the size of globalNode.')
END IF

IF (PRESENT(addContribution)) THEN
  CALL add( &
    & obj=obj%realVec, &
    & dofobj=obj%dof, &
    & nodenum=localNode, &
    & VALUE=VALUE, &
    & scale=INPUT(option=scale, default=1.0_DFP), &
    & ivar=ivar, &
    & spaceCompo=spaceCompo, &
    & timeCompo=timeCompo &
    & )
ELSE
  CALL Set( &
    & obj=obj%realVec, &
    & dofobj=obj%dof, &
    & nodenum=localNode, &
    & VALUE=VALUE, &
    & ivar=ivar, &
    & spaceCompo=spaceCompo, &
    & timeCompo=timeCompo &
    & )
END IF
END PROCEDURE obj_set8

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set9
CHARACTER(*), PARAMETER :: myName = "obj_set9"
INTEGER(I4B) :: localNode(SIZE(globalNode))

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'BlockNodeField_::obj is not initiated')
END IF

localNode = obj%domains(ivar)%ptr%GetLocalNodeNumber(globalNode)

IF (ANY(localNode .EQ. 0_I4B)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'Some globalNodes are out of bound')
END IF

IF (PRESENT(addContribution)) THEN
  CALL add( &
    & obj=obj%realVec, &
    & dofobj=obj%dof, &
    & nodenum=localNode, &
    & VALUE=VALUE, &
    & scale=INPUT(option=scale, default=1.0_DFP), &
    & ivar=ivar, &
    & spaceCompo=spaceCompo, &
    & timeCompo=timeCompo &
    & )
ELSE
  CALL Set( &
    & obj=obj%realVec, &
    & dofobj=obj%dof, &
    & nodenum=localNode, &
    & VALUE=VALUE, &
    & ivar=ivar, &
    & spaceCompo=spaceCompo, &
    & timeCompo=timeCompo &
    & )
END IF
END PROCEDURE obj_set9

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set10
CHARACTER(*), PARAMETER :: myName = "obj_set10"
INTEGER(I4B) :: localNode(SIZE(globalNode))

CALL e%raiseError(modName//'::'//myName//' - '// &
& 'This routine does not perform well, there is a bug, see test-11')

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'BlockNodeField_::obj is not initiated')
END IF

localNode = obj%domains(ivar)%ptr%GetLocalNodeNumber(globalNode)

IF (ANY(localNode .EQ. 0_I4B)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'Some globalNodes are out of bound')
END IF

IF (SIZE(VALUE) .NE. SIZE(globalNode) * SIZE(timeCompo)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'The size of value not same as the size of globalNode '// &
    & ' times the size of timeCompo.')
END IF

IF (PRESENT(addContribution)) THEN
  CALL add( &
    & obj=obj%realVec, &
    & dofobj=obj%dof, &
    & nodenum=localNode, &
    & VALUE=VALUE, &
    & scale=INPUT(option=scale, default=1.0_DFP), &
    & ivar=ivar, &
    & spaceCompo=spaceCompo, &
    & timeCompo=timeCompo &
    & )
ELSE
  CALL Set( &
    & obj=obj%realVec, &
    & dofobj=obj%dof, &
    & nodenum=localNode, &
    & VALUE=VALUE, &
    & ivar=ivar, &
    & spaceCompo=spaceCompo, &
    & timeCompo=timeCompo &
    & )
END IF
END PROCEDURE obj_set10

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set11
CHARACTER(*), PARAMETER :: myName = "obj_set11"
INTEGER(I4B) :: localNode(SIZE(globalNode))

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'BlockNodeField_::obj is not initiated')
END IF

localNode = obj%domains(ivar)%ptr%GetLocalNodeNumber(globalNode)

IF (ANY(localNode .EQ. 0_I4B)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'Some globalNodes are out of bound')
END IF

IF (PRESENT(addContribution)) THEN
  CALL add( &
    & obj=obj%realVec, &
    & dofobj=obj%dof, &
    & nodenum=localNode, &
    & VALUE=VALUE, &
    & scale=INPUT(option=scale, default=1.0_DFP), &
    & ivar=ivar, &
    & spaceCompo=spaceCompo, &
    & timeCompo=timeCompo &
    & )
ELSE
  CALL Set( &
    & obj=obj%realVec, &
    & dofobj=obj%dof, &
    & nodenum=localNode, &
    & VALUE=VALUE, &
    & ivar=ivar, &
    & spaceCompo=spaceCompo, &
    & timeCompo=timeCompo &
    & )
END IF
END PROCEDURE obj_set11

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set12
CHARACTER(*), PARAMETER :: myName = "obj_set12"
INTEGER(I4B) :: localNode(SIZE(globalNode))

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'BlockNodeField_::obj is not initiated')
END IF

localNode = obj%domains(ivar)%ptr%GetLocalNodeNumber(globalNode)

IF (ANY(localNode .EQ. 0_I4B)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'Some globalNodes are out of bound')
END IF

IF (SIZE(VALUE) .NE. SIZE(globalNode)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'The size of value not same as the size of globalNode'// &
    & ' times the size of spaceCompo.')
END IF

IF (PRESENT(addContribution)) THEN
  CALL add( &
    & obj=obj%realVec, &
    & dofobj=obj%dof, &
    & nodenum=localNode, &
    & VALUE=VALUE, &
    & scale=INPUT(option=scale, default=1.0_DFP), &
    & ivar=ivar, &
    & spaceCompo=spaceCompo, &
    & timeCompo=timeCompo &
    & )
ELSE
  CALL Set( &
    & obj=obj%realVec, &
    & dofobj=obj%dof, &
    & nodenum=localNode, &
    & VALUE=VALUE, &
    & ivar=ivar, &
    & spaceCompo=spaceCompo, &
    & timeCompo=timeCompo &
    & )
END IF
END PROCEDURE obj_set12

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set13
CHARACTER(*), PARAMETER :: myName = "obj_set13"
INTEGER(I4B) :: localNode(SIZE(globalNode))

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'BlockNodeField_::obj is not initiated')
END IF

localNode = obj%domains(ivar)%ptr%GetLocalNodeNumber(globalNode)

IF (ANY(localNode .EQ. 0_I4B)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'Some globalNodes are out of bound')
END IF

IF (PRESENT(addContribution)) THEN
  CALL add( &
    & obj=obj%realVec, &
    & dofobj=obj%dof, &
    & nodenum=localNode, &
    & VALUE=VALUE, &
    & scale=INPUT(option=scale, default=1.0_DFP), &
    & ivar=ivar, &
    & spaceCompo=spaceCompo, &
    & timeCompo=timeCompo &
    & )
ELSE
  CALL Set( &
    & obj=obj%realVec, &
    & dofobj=obj%dof, &
    & nodenum=localNode, &
    & VALUE=VALUE, &
    & ivar=ivar, &
    & spaceCompo=spaceCompo, &
    & timeCompo=timeCompo &
    & )
END IF
END PROCEDURE obj_set13

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set14
CHARACTER(*), PARAMETER :: myName = "obj_set14"
INTEGER(I4B) :: localNode

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'BlockNodeField_::obj is not initiated')
END IF

localNode = obj%domains(ivar)%ptr%GetLocalNodeNumber(globalNode)

IF ((localNode .EQ. 0_I4B)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'GlobalNode is out of bound')
END IF

IF (PRESENT(addContribution)) THEN
  CALL add( &
    & obj=obj%realVec, &
    & dofobj=obj%dof, &
    & nodenum=localNode, &
    & VALUE=VALUE, &
    & scale=INPUT(option=scale, default=1.0_DFP), &
    & ivar=ivar, &
    & spaceCompo=spaceCompo, &
    & timeCompo=timeCompo &
    & )
ELSE
  CALL Set( &
    & obj=obj%realVec, &
    & dofobj=obj%dof, &
    & nodenum=localNode, &
    & VALUE=VALUE, &
    & ivar=ivar, &
    & spaceCompo=spaceCompo, &
    & timeCompo=timeCompo &
    & )
END IF
END PROCEDURE obj_set14

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set15
CHARACTER(*), PARAMETER :: myName = "obj_set15"
INTEGER(I4B) :: localNode

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'BlockNodeField_::obj is not initiated')
END IF

localNode = obj%domains(ivar)%ptr%GetLocalNodeNumber(globalNode)

IF ((localNode .EQ. 0_I4B)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'GlobalNode is out of bound')
END IF

IF (PRESENT(addContribution)) THEN
  CALL add( &
    & obj=obj%realVec, &
    & dofobj=obj%dof, &
    & nodenum=localNode, &
    & VALUE=VALUE, &
    & scale=INPUT(option=scale, default=1.0_DFP), &
    & ivar=ivar, &
    & spaceCompo=spaceCompo, &
    & timeCompo=timeCompo &
    & )
ELSE
  CALL Set( &
    & obj=obj%realVec, &
    & dofobj=obj%dof, &
    & nodenum=localNode, &
    & VALUE=VALUE, &
    & ivar=ivar, &
    & spaceCompo=spaceCompo, &
    & timeCompo=timeCompo &
    & )
END IF
END PROCEDURE obj_set15

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set16
CHARACTER(*), PARAMETER :: myName = "obj_set16"
INTEGER(I4B) :: localNode

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'BlockNodeField_::obj is not initiated')
END IF

localNode = obj%domains(ivar)%ptr%GetLocalNodeNumber(globalNode)

IF ((localNode .EQ. 0_I4B)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'GlobalNode is out of bound')
END IF

IF (PRESENT(addContribution)) THEN
  CALL add( &
    & obj=obj%realVec, &
    & dofobj=obj%dof, &
    & nodenum=localNode, &
    & VALUE=VALUE, &
    & scale=INPUT(option=scale, default=1.0_DFP), &
    & ivar=ivar, &
    & spaceCompo=spaceCompo, &
    & timeCompo=timeCompo &
    & )
ELSE
  CALL Set( &
    & obj=obj%realVec, &
    & dofobj=obj%dof, &
    & nodenum=localNode, &
    & VALUE=VALUE, &
    & ivar=ivar, &
    & spaceCompo=spaceCompo, &
    & timeCompo=timeCompo &
    & )
END IF
END PROCEDURE obj_set16

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set17
CHARACTER(*), PARAMETER :: myName = "obj_set17()"
LOGICAL(LGT) :: problem

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

problem = .NOT. obj%isInitiated

#ifdef DEBUG_VER
IF (problem) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & '[INTERNAL ERROR] :: BlockNodeField_::obj is not initiated')
END IF

problem = .NOT. VALUE%isInitiated
IF (problem) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & '[INTERNAL ERROR] :: BlockNodeField_::value is not initiated')
END IF
#endif

IF (PRESENT(addContribution)) THEN
  CALL AXPY(X=VALUE%realvec, Y=obj%realvec, A=scale)
ELSE
  CALL COPY(Y=obj%realVec, X=VALUE%realVec)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif
END PROCEDURE obj_set17

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_assign
CALL set(obj%realVec, VALUE=VALUE)
END PROCEDURE obj_assign

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_set18
CHARACTER(*), PARAMETER :: myName = "obj_set18"
INTEGER(I4B) :: tsize
INTEGER(I4B) :: tsize_value
INTEGER(I4B) :: ii
INTEGER(I4B) :: indx1
INTEGER(I4B) :: indx2
REAL(DFP) :: avar

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'BlockNodeField_::obj is not initiated')
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

END PROCEDURE obj_set18

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods
