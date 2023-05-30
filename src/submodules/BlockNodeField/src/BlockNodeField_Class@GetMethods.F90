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

SUBMODULE(BlockNodeField_Class) GetMethods
USE BaseMethod
USE Field
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_get1
CHARACTER(*), PARAMETER :: myName = "bnField_get1"
IF (.NOT. obj%isInitiated) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'BlockNodeField_::obj is not initiated')
VALUE = Get( &
  & obj=obj%realVec, &
  & dofobj=obj%dof, &
  & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & idof=idof)
END PROCEDURE bnField_get1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_get2
CHARACTER(*), PARAMETER :: myName = "bnField_get2"
IF (.NOT. obj%isInitiated) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'BlockNodeField_::obj is not initiated')
VALUE = Get(obj=obj%realVec)
END PROCEDURE bnField_get2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_get3
CHARACTER(*), PARAMETER :: myName = "bnField_get3"
IF (.NOT. obj%isInitiated) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'BlockNodeField_::obj is not initiated')

VALUE = Get( &
  & obj=obj%realVec, &
  & dofobj=obj%dof, &
  & nodenum=obj%domains(ivar)%ptr%getLocalNodeNumber(globalNode), &
  & ivar=ivar, &
  & idof=idof)
END PROCEDURE bnField_get3

!----------------------------------------------------------------------------
!                                                                   get
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_get4
INTEGER(I4B) :: globalNode(INT(1 + (iend - istart) / stride)), ii, jj
jj = 0
DO ii = istart, iend, stride
  jj = jj + 1
  globalNode(jj) = ii
END DO
CALL obj%get( &
  & globalNode=globalNode, &
  & VALUE=VALUE, &
  & ivar=ivar, &
  & idof=idof)
END PROCEDURE bnField_get4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_get5
REAL(DFP), ALLOCATABLE :: value0(:)
CALL obj%get(VALUE=value0, globalNode=globalNode, ivar=ivar, idof=idof)
VALUE = NodalVariable( &
  & value0, &
  & TypeFEVariableScalar, &
  & TypeFEVariableSpace)
DEALLOCATE (value0)
END PROCEDURE bnField_get5

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_get6
CHARACTER(*), PARAMETER :: myName = "bnField_get6"
INTEGER(I4B) :: timeCompo
INTEGER(I4B) :: spaceCompo
INTEGER(I4B) :: ierr
INTEGER(I4B) :: case_id
REAL(DFP), ALLOCATABLE :: m3a(:, :, :)
REAL(DFP), ALLOCATABLE :: m3b(:, :, :)
REAL(DFP), ALLOCATABLE :: value0(:)
INTEGER(I4B) :: tdof
INTEGER(I4B) :: ii

tdof = obj%dof.tdof.ivar
case_id = SIZE(globalNode)
CALL Reallocate(m3a, case_id, tdof, 1)

DO ii = 1, tdof
  CALL obj%get(VALUE=value0, globalNode=globalNode, &
    & ivar=ivar, idof=ii)
  m3a(:, ii, 1) = value0
END DO
value0 = RESHAPE(m3a, [case_id * tdof])
DEALLOCATE (m3a)

case_id = 0_I4B
timeCompo = obj%dof.TimeComponents.ivar
spaceCompo = obj%dof.SpaceComponents.ivar

IF ((spaceCompo .GT. 1)) THEN
  IF (timeCompo .GT. 1) THEN
    case_id = 1
  ELSE
    case_id = 2
  END IF
ELSE
  IF (timeCompo .GT. 1) THEN
    case_id = 3
  ELSE
    case_id = 4
  END IF
END IF

SELECT CASE (case_id)
CASE (1)
  ! vector space-time
  m3b = RESHAPE(value0, [SIZE(globalNode), spaceCompo, timeCompo])
  ! Here m3b is in (J, i, a) format, but we need (i,J,a) format
  CALL SWAP(a=m3a, b=m3b, i1=2, i2=1, i3=3)
  VALUE = NodalVariable(m3a, TypeFEVariableVector, TypeFEVariableSpaceTime)
CASE (2)
  ! vector space
  VALUE = NodalVariable( &
    & TRANSPOSE(RESHAPE(value0, [SIZE(globalNode), spaceCompo])), &
    & TypeFEVariableVector, TypeFEVariableSpace)
CASE (3)
  ! scalar space-time
  VALUE = NodalVariable(  &
    & RESHAPE(value0, [SIZE(globalNode), timeCompo]),  &
    & TypeFEVariableScalar, &
    & TypeFEVariableSpaceTime)
CASE (4)
  ! scalar space
  VALUE = NodalVariable(value0, TypeFEVariableScalar, TypeFEVariableSpace)
CASE DEFAULT
  CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'No case found for given arguments')
END SELECT

IF (ALLOCATED(value0)) DEALLOCATE (value0)
IF (ALLOCATED(m3a)) DEALLOCATE (m3a)
IF (ALLOCATED(m3b)) DEALLOCATE (m3b)

END PROCEDURE bnField_get6

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_get7
INTEGER(I4B) :: localNode(SIZE(globalNode)), idof
CHARACTER(*), PARAMETER :: myName = "bnField_get7"
IF (.NOT. obj%isInitiated) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'BlockNodeField_::obj is not initiated')
localNode = obj%domains(ivar)%ptr%getLocalNodeNumber(globalNode)
IF (ANY(localNode .EQ. 0_I4B)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'Some of globalNodes are out of bound')
END IF
idof = GetIDOF(spaceCompo=spaceCompo, timeCompo=timeCompo, &
  & tSpaceCompo=obj%dof.SpaceComponents.ivar)
VALUE = Get(obj=obj%realVec, dofobj=obj%dof, &
  & nodenum=localNode, ivar=ivar, idof=idof)
END PROCEDURE bnField_get7

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_get8
REAL(DFP), ALLOCATABLE :: value0(:)
CALL obj%get(VALUE=value0, globalNode=globalNode, ivar=ivar, &
  & spaceCompo=spaceCompo, timeCompo=timeCompo)
VALUE = NodalVariable( &
  & value0, &
  & TypeFEVariableScalar, &
  & TypeFEVariableSpace)
DEALLOCATE (value0)
END PROCEDURE bnField_get8

!----------------------------------------------------------------------------
!                                                                 Get
!----------------------------------------------------------------------------

MODULE PROCEDURE bnField_get9
CHARACTER(*), PARAMETER :: myName = "bnField_get9"
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
    & obj=obj%dof, &
    & nodenum=ii, &
    & ivar=ivar, &
    & idof=idof)
  CALL obj%GetSingle(VALUE=avar, indx=indx1)
  indx2 = GetNodeLoc(&
    & obj=VALUE%dof, &
    & nodenum=ii, &
    & ivar=ivar_value, &
    & idof=idof_value)
  CALL VALUE%SetSingle(VALUE=avar, indx=indx2)
END DO

END PROCEDURE bnField_get9

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetMethods
