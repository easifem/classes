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
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_get1
INTEGER(I4B) :: localNode
CHARACTER(*), PARAMETER :: myName = "vField_get1"

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
    CALL getValue( &
      & obj=obj%realvec, &
      & dofobj=obj%dof, &
      & idof=arange(1, obj%spaceCompo), &
      & VALUE=VALUE, &
      & nodenum=[1])
  CASE (FIELD_TYPE_NORMAL)
    CALL getValue( &
      & obj=obj%realvec, &
      & dofobj=obj%dof, &
      & idof=arange(1, obj%spaceCompo), &
      & VALUE=VALUE, &
      & nodenum=obj%domain%getLocalNodeNumber([globalNode]))
  END SELECT
END IF

IF (PRESENT(spaceCompo)) THEN
  CALL getValue( &
    & obj=obj%realvec, &
    & dofobj=obj%dof, &
    & idof=spaceCompo, &
    & VALUE=VALUE)
END IF

END PROCEDURE vField_get1

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_get2
CALL getValue( &
  & obj=obj%realvec, &
  & dofobj=obj%dof, &
  & idof=arange(1, obj%spaceCompo), &
  & VALUE=VALUE, &
  & force3D=force3D)
END PROCEDURE vField_get2

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_get3
REAL(DFP), ALLOCATABLE :: v(:)

CALL getValue( &
  & obj=obj%realvec, &
  & dofobj=obj%dof, &
  & idof=arange(1, obj%spaceCompo), &
  & VALUE=v, &
  & nodenum=obj%domain%getLocalNodeNumber(globalNode))

IF (PRESENT(force3D)) THEN
  CALL Reallocate(VALUE, 3, SIZE(globalNode))
  VALUE(1:obj%spaceCompo, :) = &
    & RESHAPE(v, [obj%spaceCompo, SIZE(globalNode)])
ELSE
  VALUE = RESHAPE(v, [obj%spaceCompo, SIZE(globalNode)])
END IF

DEALLOCATE (v)
END PROCEDURE vField_get3

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_get4
CALL getValue( &
  & obj=obj%realvec, &
  & dofobj=obj%dof, &
  & ivar=1, &
  & idof=spaceCompo, &
  & VALUE=VALUE, &
  & nodenum=obj%domain%getLocalNodeNumber(globalNode))
END PROCEDURE vField_get4

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_get5
CALL getValue( &
  & obj=obj%realvec, &
  & dofobj=obj%dof, &
  & ivar=1, &
  & idof=spaceCompo, &
  & VALUE=VALUE, &
  & nodenum=obj%domain%getLocalNodeNumber(globalNode))
END PROCEDURE vField_get5

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_get6
INTEGER(I4B) :: globalNode(INT(1 + (iend - istart) / stride)), ii, jj
jj = 0
DO ii = istart, iend, stride
  jj = jj + 1
  globalNode(jj) = ii
END DO
CALL obj%get(globalNode=globalNode, VALUE=VALUE)
END PROCEDURE vField_get6

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_get7
INTEGER(I4B) :: globalNode(INT(1 + (iend - istart) / stride)), ii, jj
jj = 0
DO ii = istart, iend, stride
  jj = jj + 1
  globalNode(jj) = ii
END DO
CALL obj%get(globalNode=globalNode, VALUE=VALUE, spaceCompo=spaceCompo)
END PROCEDURE vField_get7

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_get8
REAL(DFP), ALLOCATABLE :: v(:)
CALL getValue( &
  & obj=obj%realvec, &
  & dofobj=obj%dof, &
  & idof=arange(1, obj%spaceCompo), &
  & VALUE=v, &
  & nodenum=obj%domain%getLocalNodeNumber(globalNode))
VALUE = NodalVariable( &
  & RESHAPE(v, [obj%spaceCompo, SIZE(globalNode)]), &
  & TypeFEVariableVector, TypeFEVariableSpace)
DEALLOCATE (v)
END PROCEDURE vField_get8

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_get9
CHARACTER(*), PARAMETER :: myName = "vField_get9"
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
END PROCEDURE vField_get9

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_get10
CALL GetValue( &
  & obj=obj%realvec, &
  & VALUE=VALUE%realvec)
END PROCEDURE vField_get10

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_get11
CHARACTER(*), PARAMETER :: myName = "vField_get11"
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

END PROCEDURE vField_get11

!----------------------------------------------------------------------------
!                                                     getPointerOfComponent
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_getPointerOfComponent
CHARACTER(*), PARAMETER :: myName = "vField_getPointerOfComponent"
IF (spaceCompo .GT. obj%spaceCompo) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'given spaceCompo should be less than or equal to obj%spaceCompo')
ans => getPointer( &
  & obj=obj%realvec, &
  & dofobj=obj%dof, &
  & idof=spaceCompo)
END PROCEDURE vField_getPointerOfComponent

END SUBMODULE GetMethods
