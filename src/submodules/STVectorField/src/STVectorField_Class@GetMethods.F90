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

SUBMODULE(STVectorField_Class) GetMethods
USE BaseMethod
USE Field
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_Get1
IF (PRESENT(globalnode)) THEN
  SELECT CASE (obj%fieldType)
  CASE (FIELD_TYPE_CONSTANT)
    CALL GetValue( &
      & obj=obj%realvec, &
      & dofobj=obj%dof, &
      & idof=GetIDOF(obj=obj%dof, ivar=1), &
      & VALUE=VALUE, &
      & storageFMT=NODES_FMT, &
      & nodenum=[1])
    RETURN
  CASE (FIELD_TYPE_NORMAL)
    CALL GetValue( &
      & obj=obj%realvec, &
      & dofobj=obj%dof, &
      & idof=GetIDOF(obj=obj%dof, ivar=1), &
      & VALUE=VALUE, &
      & storageFMT=NODES_FMT, &
      & nodenum=obj%domain%GetLocalNodeNumber([globalnode]))
    RETURN
  END SELECT
END IF

IF (PRESENT(spacecompo) .AND. PRESENT(timecompo)) THEN
  CALL GetValue( &
    & obj=obj%realvec, &
    & dofobj=obj%dof, &
    & ivar=1, &
    & spacecompo=spacecompo, &
    & timecompo=timecompo, &
    & VALUE=VALUE)
  RETURN
END IF

IF (PRESENT(spacecompo)) THEN
  CALL GetValue( &
    & obj=obj%realvec, &
    & dofobj=obj%dof, &
    & idof=GetIDOF( &
    & obj=obj%dof, &
    & ivar=1, &
    & spacecompo=spacecompo, &
    & timecompo=arange(1, obj%timecompo)), &
    & storageFMT=NODES_FMT, &
    & VALUE=VALUE)
  RETURN
END IF

IF (PRESENT(timecompo)) THEN
  CALL GetValue( &
    & obj=obj%realvec, &
    & dofobj=obj%dof, &
    & idof=GetIDOF( &
    & obj=obj%dof, &
    & ivar=1, &
    & timecompo=timecompo, &
    & spacecompo=arange(1, obj%spacecompo)), &
    & storageFMT=NODES_FMT, &
    & VALUE=VALUE)
  RETURN
END IF

END PROCEDURE stvField_Get1

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_Get2
VALUE = RESHAPE( &
  & Get( &
  & obj=obj%realVec, &
  & datatype=1.0_DFP), &
  & [obj%spaceCompo, obj%timeCompo, obj%domain%GetTotalNodes()])
END PROCEDURE stvField_Get2

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_Get3
REAL(DFP), ALLOCATABLE :: v(:)
CALL GetValue( &
  & obj=obj%realVec, &
  & dofobj=obj%dof, &
  & idof=GetIDOF(obj=obj%dof, ivar=1), &
  & VALUE=v, &
  & storageFMT=NODES_FMT, &
  & nodenum=obj%domain%GetLocalNodeNumber(globalNode))
VALUE = RESHAPE(v, [obj%spaceCompo, obj%timeCompo, SIZE(globalNode)])
DEALLOCATE (v)
END PROCEDURE stvField_Get3

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_Get4
CALL GetValue( &
  & obj=obj%realVec, &
  & dofobj=obj%dof, &
  & ivar=1, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo, &
  & VALUE=VALUE, &
  & nodenum=obj%domain%GetLocalNodeNumber(globalNode))
END PROCEDURE stvField_Get4

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_Get5
CALL GetValue( &
  & obj=obj%realVec, &
  & dofobj=obj%dof, &
  & ivar=1, &
  & idof=GetIDOF( &
  & obj=obj%dof, &
  & ivar=1, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo), &
  & VALUE=VALUE, &
  & nodenum=obj%domain%GetLocalNodeNumber(globalNode))
END PROCEDURE stvField_Get5

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_Get6
INTEGER(I4B) :: globalNode(INT(1 + (iend - istart) / stride)), ii, jj
jj = 0
DO ii = istart, iend, stride
  jj = jj + 1
  globalNode(jj) = ii
END DO
CALL obj%Get(globalNode=globalNode, VALUE=VALUE)
END PROCEDURE stvField_Get6

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_Get7
INTEGER(I4B) :: globalNode(INT(1 + (iend - istart) / stride)), ii, jj
jj = 0
DO ii = istart, iend, stride
  jj = jj + 1
  globalNode(jj) = ii
END DO
CALL obj%Get( &
  & globalNode=globalNode, &
  & VALUE=VALUE, &
  & spaceCompo=spaceCompo, &
  & timeCompo=timeCompo)
END PROCEDURE stvField_Get7

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_Get8
REAL(DFP), ALLOCATABLE :: val(:, :, :)
CALL obj%Get(VALUE=val, globalNode=[globalNode])
VALUE = val(:, :, 1)
DEALLOCATE (val)
END PROCEDURE stvField_Get8

!----------------------------------------------------------------------------
!                                                                      Get
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_Get9
REAL(DFP), ALLOCATABLE :: m3a(:, :, :), m3b(:, :, :)
CALL obj%Get(VALUE=m3b, globalNode=globalNode)

! Here m3b is in (i, a, J) format,
! so we have to swap the dimensions to (i,J,a)
! We will call swap method from Utility.
CALL SWAP(a=m3a, b=m3b, i1=1, i2=3, i3=2)
VALUE = NodalVariable(m3a, TypeFEVariableVector, &
  & TypeFEVariableSpacetime)

DEALLOCATE (m3a, m3b)
END PROCEDURE stvField_Get9

!----------------------------------------------------------------------------
!                                                                 Get
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_Get10
CHARACTER(*), PARAMETER :: myName = "stvField_Get10"
INTEGER(I4B) :: case_id
INTEGER(I4B) :: tNodes
INTEGER(I4B) :: ii
INTEGER(I4B) :: jj
INTEGER(I4B) :: kk
INTEGER(I4B) :: globalnode
REAL(DFP) :: avar

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either VectorFieldLis_::obj is not initiated'// &
  & "")
END IF

IF (.NOT. VALUE%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either AbstractNodeField_::value is not initiated'// &
  & "")
END IF

tNodes = obj%domain%GetTotalNodes()

IF (tNodes .NE. VALUE%domain%GetTotalNodes()) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'tNodes of STVectorFieldLis_::obj and'// &
    & ' tNodes of AbstractNodeField_::value are not same')
END IF

IF (PRESENT(spaceCompo) .AND. PRESENT(timeCompo)) THEN
  case_id = 1
  IF (spaceCompo .GT. obj%spaceCompo) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'spaceCompo is greater than obj%spaceCompo')
  END IF

  IF (timeCompo .GT. obj%timeCompo) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'timeCompo is greater than obj%timeCompo')
  END IF

ELSEIF (PRESENT(spaceCompo) .AND. .NOT. PRESENT(timeCompo)) THEN
  case_id = 2
  IF (spaceCompo .GT. obj%spaceCompo) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'spaceCompo is greater than obj%spaceCompo')
  END IF

ELSEIF (.NOT. PRESENT(spaceCompo) .AND. PRESENT(timeCompo)) THEN
  case_id = 3
  IF (timeCompo .GT. obj%timeCompo) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'timeCompo is greater than obj%timeCompo')
  END IF

ELSEIF (.NOT. PRESENT(spaceCompo) .AND. .NOT. PRESENT(timeCompo)) THEN
  case_id = 4
END IF

SELECT CASE (case_id)
! spaceCompo and timeCompo are present
CASE (1)
  SELECT TYPE (VALUE)
  CLASS IS (ScalarField_)
    DO ii = 1, tNodes
      globalnode = obj%domain%GetGlobalNodeNumber(localNode=ii)
      CALL obj%Get(VALUE=avar, globalnode=globalnode, &
        & spaceCompo=spaceCompo, timeCompo=timeCompo)
      CALL VALUE%set(VALUE=avar, globalnode=globalnode)
    END DO

  CLASS IS (STScalarField_)
    DO ii = 1, tNodes
      globalnode = obj%domain%GetGlobalNodeNumber(localNode=ii)
      CALL obj%Get(VALUE=avar, globalnode=globalnode, &
        & spaceCompo=spaceCompo, timeCompo=timeCompo)
      CALL VALUE%set(VALUE=avar, globalnode=globalnode, timeCompo=timeCompo)
    END DO

  CLASS IS (VectorField_)
    DO ii = 1, tNodes
      globalnode = obj%domain%GetGlobalNodeNumber(localNode=ii)
      CALL obj%Get(VALUE=avar, globalnode=globalnode, &
        & spaceCompo=spaceCompo, timeCompo=timeCompo)
      CALL VALUE%set(VALUE=avar, globalnode=globalnode, spaceCompo=spaceCompo)
    END DO

  CLASS IS (STVectorField_)
    DO ii = 1, tNodes
      globalnode = obj%domain%GetGlobalNodeNumber(localNode=ii)
      CALL obj%Get(VALUE=avar, globalnode=globalnode, &
        & spaceCompo=spaceCompo, timeCompo=timeCompo)
      CALL VALUE%set(VALUE=avar, globalnode=globalnode, &
        & spaceCompo=spaceCompo, timeCompo=timeCompo)
    END DO

  CLASS DEFAULT
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'No case found for type of value; case_id=1')

  END SELECT

! spaceCompo is present
CASE (2)
  SELECT TYPE (VALUE)

  CLASS IS (STScalarField_)
    DO ii = 1, tNodes
      globalnode = obj%domain%GetGlobalNodeNumber(localNode=ii)
      DO jj = 1, obj%timeCompo
        CALL obj%Get(VALUE=avar, globalnode=globalnode, &
          & spaceCompo=spaceCompo, timeCompo=jj)
        CALL VALUE%set(VALUE=avar, globalnode=globalnode, timeCompo=jj)
      END DO
    END DO

  CLASS IS (STVectorField_)
    DO ii = 1, tNodes
      globalnode = obj%domain%GetGlobalNodeNumber(localNode=ii)
      DO jj = 1, obj%timeCompo
        CALL obj%Get(VALUE=avar, globalnode=globalnode, &
          & spaceCompo=spaceCompo, timeCompo=jj)
        CALL VALUE%set(VALUE=avar, globalnode=globalnode, &
          & spaceCompo=spaceCompo, timeCompo=jj)
      END DO
    END DO

  CLASS DEFAULT
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'No case found for type of value; case_id=2')

  END SELECT

! timeCompo is present
CASE (3)
  SELECT TYPE (VALUE)

  CLASS IS (VectorField_)
    DO ii = 1, tNodes
      globalnode = obj%domain%GetGlobalNodeNumber(localNode=ii)
      DO jj = 1, obj%spaceCompo
        CALL obj%Get(VALUE=avar, globalnode=globalnode, &
          & spaceCompo=jj, timeCompo=timeCompo)
        CALL VALUE%set(VALUE=avar, globalnode=globalnode, spaceCompo=jj)
      END DO
    END DO

  CLASS IS (STVectorField_)
    DO ii = 1, tNodes
      globalnode = obj%domain%GetGlobalNodeNumber(localNode=ii)
      DO jj = 1, obj%spaceCompo
        CALL obj%Get(VALUE=avar, globalnode=globalnode, &
          & spaceCompo=jj, timeCompo=timeCompo)
        CALL VALUE%set(VALUE=avar, globalnode=globalnode, &
          & spaceCompo=jj, timeCompo=timeCompo)
      END DO
    END DO

  CLASS DEFAULT
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'No case found for type of value; case_id=3')

  END SELECT

! spaceCompo and timeCompo are not present
CASE (4)
  SELECT TYPE (VALUE)

  CLASS IS (STVectorField_)
    DO ii = 1, tNodes
      globalnode = obj%domain%GetGlobalNodeNumber(localNode=ii)
      DO jj = 1, obj%timeCompo
        DO kk = 1, obj%spaceCompo
          CALL obj%Get(VALUE=avar, globalnode=globalnode, &
            & spaceCompo=kk, timeCompo=jj)
          CALL VALUE%set(VALUE=avar, globalnode=globalnode, &
            & spaceCompo=kk, timeCompo=jj)
        END DO
      END DO
    END DO

  CLASS DEFAULT
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'No case found for type of value; case_id=4')

  END SELECT

CASE default
  CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'No case found for given arguments')
END SELECT

END PROCEDURE stvField_Get10

!----------------------------------------------------------------------------
!                                                                 Get
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_Get11
CHARACTER(*), PARAMETER :: myName = "stvField_Get11"
INTEGER(I4B) :: tsize
INTEGER(I4B) :: tsize_value
INTEGER(I4B) :: ii
INTEGER(I4B) :: indx1
INTEGER(I4B) :: indx2
REAL(DFP) :: avar

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'STVectorField_::obj is not initiated')
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

END PROCEDURE stvField_Get11

!----------------------------------------------------------------------------
!                                                     GetPointerOfComponent
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_GetPointerOfComponent
CHARACTER(*), PARAMETER :: myName = "stvField_GetPointerOfComponent"

IF (spaceCompo .GT. obj%spaceCompo .OR. timeCompo .GT. obj%timeCompo) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'given spaceCompo or timeCompo should be less than'// &
    & ' or equal to obj%spaceCompo or obj%timeCompo')
END IF

ans => GetPointer( &
  & obj=obj%realVec, &
  & dofobj=obj%dof, &
  & idof=GetIDOF( &
  & obj=obj%dof, &
  & ivar=1, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo))

END PROCEDURE stvField_GetPointerOfComponent

!----------------------------------------------------------------------------
!                                                              GetFEVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_GetFEVariable
CALL obj%Get(VALUE=VALUE, globalNode=globalNode)
END PROCEDURE stvField_GetFEVariable

!----------------------------------------------------------------------------
!                                                                  GetPrefix
!----------------------------------------------------------------------------

MODULE PROCEDURE stvField_GetPrefix
ans = myprefix
END PROCEDURE stvField_GetPrefix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetMethods
