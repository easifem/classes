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

SUBMODULE(VectorField_Class) GetMethods
USE BaseMethod
USE ScalarField_Class
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get1
INTEGER(I4B) :: localNode
CHARACTER(*), PARAMETER :: myName = "obj_Get1"

IF (.NOT. obj%isInitiated) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'VectorField_::obj is not initiated')

IF (PRESENT(globalNode) .AND. PRESENT(spaceCompo)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'spaceCompo and globalNode both cannot be present')
END IF

IF (PRESENT(globalNode)) THEN
  SELECT CASE (obj%fieldType)
  CASE (FIELD_TYPE_CONSTANT)
    CALL GetValue( &
      & obj=obj%realvec, &
      & dofobj=obj%dof, &
      & idof=arange(1, obj%spaceCompo), &
      & VALUE=VALUE, &
      & nodenum=[1])
  CASE (FIELD_TYPE_NORMAL)
    CALL GetValue( &
      & obj=obj%realvec, &
      & dofobj=obj%dof, &
      & idof=arange(1, obj%spaceCompo), &
      & VALUE=VALUE, &
      & nodenum=obj%domain%GetLocalNodeNumber([globalNode]))
  END SELECT
END IF

IF (PRESENT(spaceCompo)) THEN
  CALL GetValue( &
    & obj=obj%realvec, &
    & dofobj=obj%dof, &
    & idof=spaceCompo, &
    & VALUE=VALUE)
END IF

END PROCEDURE obj_Get1

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get2
CALL GetValue( &
  & obj=obj%realvec, &
  & dofobj=obj%dof, &
  & idof=arange(1, obj%spaceCompo), &
  & VALUE=VALUE, &
  & force3D=force3D)
END PROCEDURE obj_Get2

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get3
REAL(DFP), ALLOCATABLE :: v(:)

CALL GetValue( &
  & obj=obj%realvec, &
  & dofobj=obj%dof, &
  & idof=arange(1, obj%spaceCompo), &
  & VALUE=v, &
  & nodenum=obj%domain%GetLocalNodeNumber(globalNode))

IF (PRESENT(force3D)) THEN
  CALL Reallocate(VALUE, 3, SIZE(globalNode))
  VALUE(1:obj%spaceCompo, :) = &
    & RESHAPE(v, [obj%spaceCompo, SIZE(globalNode)])
ELSE
  VALUE = RESHAPE(v, [obj%spaceCompo, SIZE(globalNode)])
END IF

DEALLOCATE (v)
END PROCEDURE obj_Get3

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get4
CALL GetValue( &
  & obj=obj%realvec, &
  & dofobj=obj%dof, &
  & ivar=1, &
  & idof=spaceCompo, &
  & VALUE=VALUE, &
  & nodenum=obj%domain%GetLocalNodeNumber(globalNode))
END PROCEDURE obj_Get4

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get5
CALL GetValue( &
  & obj=obj%realvec, &
  & dofobj=obj%dof, &
  & ivar=1, &
  & idof=spaceCompo, &
  & VALUE=VALUE, &
  & nodenum=obj%domain%GetLocalNodeNumber(globalNode))
END PROCEDURE obj_Get5

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get6
INTEGER(I4B) :: globalNode(INT(1 + (iend - istart) / stride)), ii, jj
jj = 0
DO ii = istart, iend, stride
  jj = jj + 1
  globalNode(jj) = ii
END DO
CALL obj%Get(globalNode=globalNode, VALUE=VALUE)
END PROCEDURE obj_Get6

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get7
INTEGER(I4B) :: globalNode(INT(1 + (iend - istart) / stride)), ii, jj
jj = 0
DO ii = istart, iend, stride
  jj = jj + 1
  globalNode(jj) = ii
END DO
CALL obj%Get(globalNode=globalNode, VALUE=VALUE, spaceCompo=spaceCompo)
END PROCEDURE obj_Get7

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get8
REAL(DFP), ALLOCATABLE :: v(:)
CALL GetValue( &
  & obj=obj%realvec, &
  & dofobj=obj%dof, &
  & idof=arange(1, obj%spaceCompo), &
  & VALUE=v, &
  & nodenum=obj%domain%GetLocalNodeNumber(globalNode))
VALUE = NodalVariable( &
  & RESHAPE(v, [obj%spaceCompo, SIZE(globalNode)]), &
  & TypeFEVariableVector, TypeFEVariableSpace)
DEALLOCATE (v)
END PROCEDURE obj_Get8

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get9
CHARACTER(*), PARAMETER :: myName = "obj_Get9"
INTEGER(I4B) :: n
n = obj%spaceCompo

IF (spacecompo .GT. n) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'This routine is not callable as'// &
    & ' (obj%dof .tspacecomponents. 1)='//tostring(n)// &
    & ' is lesser than '// &
    & ' spacecompo='//tostring(spacecompo))
END IF

SELECT TYPE (VALUE)
TYPE IS (ScalarField_)
  CALL GetValue( &
    & obj=obj%realvec, &
    & dofobj=obj%dof, &
    & VALUE=VALUE%realvec, &
    & idof=spacecompo)
CLASS DEFAULT
  CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'No case found for the type of value')
END SELECT
END PROCEDURE obj_Get9

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get10
CALL GetValue( &
  & obj=obj%realvec, &
  & VALUE=VALUE%realvec)
END PROCEDURE obj_Get10

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get11
CHARACTER(*), PARAMETER :: myName = "obj_Get11"
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

END PROCEDURE obj_Get11

!----------------------------------------------------------------------------
!                                                           GetFEVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFEVariable
CALL obj%Get(VALUE=VALUE, globalNode=globalNode)
END PROCEDURE obj_GetFEVariable

!----------------------------------------------------------------------------
!                                                     GetPointerOfComponent
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPointerOfComponent
CHARACTER(*), PARAMETER :: myName = "obj_GetPointerOfComponent"
IF (spaceCompo .GT. obj%spaceCompo) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'given spaceCompo should be less than or equal to obj%spaceCompo')
ans => GetPointer( &
  & obj=obj%realvec, &
  & dofobj=obj%dof, &
  & idof=spaceCompo)
END PROCEDURE obj_GetPointerOfComponent

!----------------------------------------------------------------------------
!                                                                  GetPrefix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPrefix
ans = myprefix
END PROCEDURE obj_GetPrefix

END SUBMODULE GetMethods
