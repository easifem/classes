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

SUBMODULE(STScalarField_Class) GetMethods
USE BaseMethod
USE ScalarField_Class, ONLY: ScalarField_
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_Get1
CHARACTER(*), PARAMETER :: myName = "stsField_Get1"
LOGICAL(LGT) :: bool1, bool2

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'STScalarField_::obj is not initiated')
END IF

bool1 = PRESENT(globalNode)
bool2 = PRESENT(timeCompo)

IF (bool1 .AND. bool2) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'Both globalNode and timeCompo cannot be present')
END IF

IF (bool1) THEN
  SELECT CASE (obj%fieldType)
  CASE (FIELD_TYPE_CONSTANT)
    CALL GetValue( &
      & obj=obj%realvec, &
      & dofobj=obj%dof, &
      & idof=arange(1, obj%timeCompo), &
      & VALUE=VALUE, &
      & nodenum=[1])
  CASE (FIELD_TYPE_NORMAL)
    CALL GetValue( &
      & obj=obj%realvec, &
      & dofobj=obj%dof, &
      & idof=arange(1, obj%timeCompo), &
      & VALUE=VALUE, &
      & nodenum=obj%domain%GetLocalNodeNumber([globalnode]))
  END SELECT
END IF

IF (bool2) THEN
  CALL GetValue( &
    & obj=obj%realvec, &
    & dofobj=obj%dof, &
    & ivar=1, &
    & idof=timeCompo, &
    & VALUE=VALUE)
END IF

END PROCEDURE stsField_Get1

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_Get2
CHARACTER(*), PARAMETER :: myName = "stsField_Get2"

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'STScalarField_::obj is not initiated')
END IF

CALL GetValue( &
  & obj=obj%realvec, &
  & dofobj=obj%dof, &
  & idof=arange(1, obj%timeCompo), &
  & VALUE=VALUE)
END PROCEDURE stsField_Get2

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_Get3
REAL(DFP), ALLOCATABLE :: v(:)
CALL GetValue( &
  & obj=obj%realvec, &
  & dofobj=obj%dof, &
  & idof=arange(1, obj%timeCompo), &
  & VALUE=v, &
  & nodenum=obj%domain%GetLocalNodeNumber(globalnode))
VALUE = RESHAPE(v, [obj%timeCompo, SIZE(globalnode)])
DEALLOCATE (v)
END PROCEDURE stsField_Get3

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_Get4
CALL GetValue( &
  & obj=obj%realvec, &
  & dofobj=obj%dof, &
  & ivar=1, &
  & idof=timeCompo, &
  & VALUE=VALUE, &
  & nodenum=obj%domain%GetLocalNodeNumber(globalnode))
END PROCEDURE stsField_Get4

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_Get5
CALL GetValue( &
  & obj=obj%realvec, &
  & dofobj=obj%dof, &
  & ivar=1, &
  & idof=timeCompo, &
  & VALUE=VALUE, &
  & nodenum=obj%domain%GetLocalNodeNumber(globalnode))
END PROCEDURE stsField_Get5

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_Get6
INTEGER(I4B) :: globalnode(INT(1 + (iend - istart) / stride)), ii, jj
jj = 0
DO ii = istart, iend, stride
  jj = jj + 1
  globalnode(jj) = ii
END DO
CALL obj%Get(globalnode=globalnode, VALUE=VALUE)
END PROCEDURE stsField_Get6

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_Get7
INTEGER(I4B) :: globalnode(INT(1 + (iend - istart) / stride)), ii, jj
jj = 0
DO ii = istart, iend, stride
  jj = jj + 1
  globalnode(jj) = ii
END DO
CALL obj%Get(globalnode=globalnode, VALUE=VALUE, timeCompo=timeCompo)
END PROCEDURE stsField_Get7

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_Get8
REAL(DFP), ALLOCATABLE :: v(:)
CALL GetValue( &
  & obj=obj%realvec, &
  & dofobj=obj%dof, &
  & VALUE=v, &
  & idof=arange(1, obj%timeCompo), &
  & nodenum=obj%domain%GetLocalNodeNumber(globalnode))
VALUE = NodalVariable( &
  & RESHAPE(v, [obj%timeCompo, SIZE(globalnode)]), &
  & TypeFEVariableScalar, &
  & TypeFEVariableSpaceTime)
DEALLOCATE (v)
END PROCEDURE stsField_Get8

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_Get9
CHARACTER(*), PARAMETER :: myName = "stsField_Get9"
INTEGER(I4B) :: n
n = (obj%dof.timecomponents.1)
IF (timecompo .GT. n) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'This routine is not callable as &
  & (obj%dof .timecomponents. 1)='//tostring(n)// &
  & ' is lesser than '// &
  & ' timecompo='//tostring(timecompo))
SELECT TYPE (VALUE)
TYPE IS (ScalarField_)
  CALL GetValue( &
    & obj=obj%realvec, &
    & dofobj=obj%dof, &
    & VALUE=VALUE%realvec, &
    & idof=timecompo)
CLASS DEFAULT
  CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'No case found for type of value')
END SELECT
END PROCEDURE stsField_Get9

!----------------------------------------------------------------------------
!                                                     GetPointerOfComponent
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_GetPointerOfComponent
CHARACTER(*), PARAMETER :: myName = "stsField_GetPointerOfComponent"
IF (timeCompo .GT. obj%timeCompo) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'given timeCompo should be less than or equal to obj%timeCompo')
ans => GetPointer(obj=obj%realvec, dofobj=obj%dof, idof=timeCompo)
END PROCEDURE stsField_GetPointerOfComponent

!----------------------------------------------------------------------------
!                                                                     Get
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_Get10
CHARACTER(*), PARAMETER :: myName = "stsField_Get10"
INTEGER(I4B) :: tsize
INTEGER(I4B) :: tsize_value
INTEGER(I4B) :: ii
INTEGER(I4B) :: indx1
INTEGER(I4B) :: indx2
REAL(DFP) :: avar

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'STScalarField_::obj is not initiated')
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

END PROCEDURE stsField_Get10

!----------------------------------------------------------------------------
!                                                              GetTimeCompo
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_GetTimeCompo
ans = obj%timeCompo
END PROCEDURE stsField_GetTimeCompo

!----------------------------------------------------------------------------
!                                                           GetFEVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_GetFEVariable
CALL obj%Get(globalNode=globalNode, VALUE=VALUE)
END PROCEDURE stsField_GetFEVariable

END SUBMODULE GetMethods
