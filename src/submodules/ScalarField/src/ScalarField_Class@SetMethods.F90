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

SUBMODULE(ScalarField_Class) SetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   set
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_set1
CHARACTER(*), PARAMETER :: myName = "sField_set1"
INTEGER(I4B) :: localNode
REAL(DFP) :: areal
LOGICAL(LGT) :: abool
areal = Input(option=scale, default=1.0_DFP)
abool = Input(option=addContribution, default=.FALSE.)

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: Scalar field object is not initiated')
  RETURN
END IF

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  IF (abool) THEN
    CALL add(obj%realVec, nodenum=[1], VALUE=[VALUE], scale=areal)
    RETURN
  END IF

  CALL set(obj%realVec, nodenum=[1], VALUE=[VALUE])
  RETURN
END IF

localNode = obj%domain%GetLocalNodeNumber(globalNode)

IF (localNode .NE. 0) THEN
  IF (abool) THEN
    CALL add(obj%realVec, nodenum=[localNode], VALUE=[VALUE], scale=areal)
    RETURN
  END IF

  CALL set(obj%realVec, nodenum=[localNode], VALUE=[VALUE])
  RETURN
END IF

END PROCEDURE sField_set1

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_set2
CHARACTER(*), PARAMETER :: myName = "sField_set2"
REAL(DFP) :: areal
LOGICAL(LGT) :: abool
areal = Input(option=scale, default=1.0_DFP)
abool = Input(option=addContribution, default=.FALSE.)

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: Scalar field object is not initiated')
  RETURN
END IF

IF (abool) THEN
  CALL add(obj%realVec, VALUE=VALUE, scale=areal)
ELSE
  CALL set(obj%realVec, VALUE=VALUE)
END IF

END PROCEDURE sField_set2

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_set3
CHARACTER(*), PARAMETER :: myName = "sField_set3"
REAL(DFP) :: areal
LOGICAL(LGT) :: abool
areal = Input(option=scale, default=1.0_DFP)
abool = Input(option=addContribution, default=.FALSE.)

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & 'Scalar field object is not initiated')
END IF

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: This routine should not be '//  &
    & 'called for constant field type.')
  RETURN
END IF

IF (obj%tSize .NE. SIZE(VALUE)) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & '[INTERNAL ERROR] :: Size of value ('//tostring(SIZE(VALUE))//  &
  & ') is not equal to size of scalarfield ('//  &
  & tostring(obj%tSize)//')')
  RETURN
END IF

IF (abool) THEN
  CALL add(obj%realVec, VALUE=VALUE, scale=areal)
ELSE
  CALL set(obj%realVec, VALUE=VALUE)
END IF

END PROCEDURE sField_set3

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_set4
CHARACTER(*), PARAMETER :: myName = "sField_set4"
INTEGER(I4B) :: localNode(SIZE(globalNode))
REAL(DFP) :: areal
LOGICAL(LGT) :: abool
areal = Input(option=scale, default=1.0_DFP)
abool = Input(option=addContribution, default=.FALSE.)

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'Scalar field object is not initiated')
  RETURN
END IF

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'This routine should not be called for constant field type.')
  RETURN
END IF

localNode = obj%domain%GetLocalNodeNumber(globalNode)
IF (ANY(localNode .GT. obj%tSize)) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'Some of the globalNode are out of bound')
  RETURN
END IF

IF (abool) THEN
  CALL add(obj%realVec, nodenum=localNode, VALUE=VALUE, scale=areal)
ELSE
  CALL set(obj%realVec, nodenum=localNode, VALUE=VALUE)
END IF
END PROCEDURE sField_set4

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_set5
CHARACTER(*), PARAMETER :: myName = "sField_set5"
INTEGER(I4B) :: localNode(SIZE(globalNode))
REAL(DFP) :: areal
LOGICAL(LGT) :: abool
areal = Input(option=scale, default=1.0_DFP)
abool = Input(option=addContribution, default=.FALSE.)

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'Scalar field object is not initiated')
  RETURN
END IF

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'This routine should not be called for constant field type.')
  RETURN
END IF

localNode = obj%domain%GetLocalNodeNumber(globalNode)

IF (ANY(localNode .GT. obj%tSize)) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'Some of the globalNode are out of bound')
  RETURN
END IF

IF (abool) THEN
  CALL add( &
    & obj=obj%realVec, &
    & nodenum=localNode, &
    & VALUE=VALUE, &
    & scale=areal)
ELSE
  CALL set(obj%realVec, nodenum=localNode, VALUE=VALUE)
END IF

END PROCEDURE sField_set5

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_set6
CHARACTER(*), PARAMETER :: myName = "sField_set6"
INTEGER(I4B) :: globalNode(INT(1 + (iend - istart) / stride)), ii, jj

jj = 0
DO ii = istart, iend, stride
  jj = jj + 1
  globalNode(jj) = ii
END DO
CALL obj%set(globalNode=globalNode, VALUE=VALUE, scale=scale, &
  & addContribution=addContribution)
END PROCEDURE sField_set6

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_set7
CHARACTER(*), PARAMETER :: myName = "sField_set7"
INTEGER(I4B) :: globalNode(INT(1 + (iend - istart) / stride)), ii, jj

jj = 0
DO ii = istart, iend, stride
  jj = jj + 1
  globalNode(jj) = ii
END DO
CALL obj%set(globalNode=globalNode, VALUE=VALUE, scale=scale, &
  & addContribution=addContribution)
END PROCEDURE sField_set7

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_set8
CALL set(obj%realVec, VALUE=obj2%realVec)
END PROCEDURE sField_set8

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_set9
CHARACTER(*), PARAMETER :: myName = "sField_set9"

SELECT CASE (VALUE%vartype)
CASE (Constant)
  CALL obj%Set( &
  & VALUE=GET(VALUE, TypeFEVariableScalar, TypeFEVariableConstant), &
  & globalNode=globalNode, &
  & scale=scale, &
  & addContribution=addContribution)
CASE (Space)
  CALL obj%Set( &
    & VALUE=GET(VALUE, TypeFEVariableScalar, TypeFEVariableSpace), &
    & globalNode=globalNode, &
    & scale=scale, &
    & addContribution=addContribution)
CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
  & 'No case found for Value%vartype, only [Constant and Space is allowed]')
END SELECT
END PROCEDURE sField_set9

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_set10
CALL add(obj%realVec, VALUE=obj2%realVec, scale=scale)
END PROCEDURE sField_set10

!----------------------------------------------------------------------------
!                                                                      Set
!----------------------------------------------------------------------------

MODULE PROCEDURE sField_set11
CHARACTER(*), PARAMETER :: myName = "sField_set11"
INTEGER(I4B) :: tsize
INTEGER(I4B) :: tsize_value
INTEGER(I4B) :: ii
INTEGER(I4B) :: indx1
INTEGER(I4B) :: indx2
REAL(DFP) :: avar

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & 'ScalarNodeField_::obj is not initiated')
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

END PROCEDURE sField_set11

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods
