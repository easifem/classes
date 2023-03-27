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
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_set1
CHARACTER(*), PARAMETER :: myName = "bnField_set1"

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
END PROCEDURE bnField_set1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_set2
CHARACTER(*), PARAMETER :: myName = "bnField_set2"
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
END PROCEDURE bnField_set2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_set3
CHARACTER(*), PARAMETER :: myName = "bnField_set3"
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
END PROCEDURE bnField_set3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_set4
CHARACTER(*), PARAMETER :: myName = "bnField_set4"
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
END PROCEDURE bnField_set4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_set5
CHARACTER(*), PARAMETER :: myName = "bnField_set5"
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
END PROCEDURE bnField_set5

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_set6
CHARACTER(*), PARAMETER :: myName = "bnField_set6"
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
END PROCEDURE bnField_set6

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_set7
CHARACTER(*), PARAMETER :: myName = "bnField_set7"
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
END PROCEDURE bnField_set7

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_set8
CHARACTER(*), PARAMETER :: myName = "bnField_set8"
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
END PROCEDURE bnField_set8

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_set9
CHARACTER(*), PARAMETER :: myName = "bnField_set9"
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
END PROCEDURE bnField_set9

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_set10
CHARACTER(*), PARAMETER :: myName = "bnField_set10"
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
END PROCEDURE bnField_set10

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_set11
CHARACTER(*), PARAMETER :: myName = "bnField_set11"
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
END PROCEDURE bnField_set11

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_set12
CHARACTER(*), PARAMETER :: myName = "bnField_set12"
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
END PROCEDURE bnField_set12

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_set13
CHARACTER(*), PARAMETER :: myName = "bnField_set13"
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
END PROCEDURE bnField_set13

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_set14
CHARACTER(*), PARAMETER :: myName = "bnField_set14"
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
END PROCEDURE bnField_set14

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_set15
CHARACTER(*), PARAMETER :: myName = "bnField_set15"
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
END PROCEDURE bnField_set15

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_set16
CHARACTER(*), PARAMETER :: myName = "bnField_set16"
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
END PROCEDURE bnField_set16

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_set17
IF (PRESENT(addContribution)) THEN
  CALL AXPY(X=obj2%realvec, Y=obj%realvec, A=scale)
ELSE
  obj%realVec = obj2%realVec
END IF
END PROCEDURE bnField_set17

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_assign
CALL set(obj%realVec, VALUE=VALUE)
END PROCEDURE bnField_assign

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods
