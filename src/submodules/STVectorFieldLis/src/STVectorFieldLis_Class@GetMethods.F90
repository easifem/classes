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

SUBMODULE(STVectorFieldLis_Class) GetMethods
USE BaseMethod
USE Field
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 GetSingle
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSingle
#include "lisf.h"
INTEGER(I4B) :: ierr
IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  CALL lis_vector_get_value(obj%lis_ptr, 1, VALUE, ierr)
  CALL CHKERR(ierr)
ELSE
  CALL lis_vector_get_value(obj%lis_ptr, indx, VALUE, ierr)
  CALL CHKERR(ierr)
END IF
END PROCEDURE obj_GetSingle

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_get1
CHARACTER(*), PARAMETER :: myName = "obj_get1"
INTEGER(I4B) :: case_id
INTEGER(I4B) :: localNode
INTEGER(I4B) :: indx
INTEGER(I4B) :: tNodes
INTEGER(I4B) :: ii
INTEGER(I4B) :: jj
INTEGER(I4B) :: kk
INTEGER(I4B) :: ierr

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either VectorFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

IF (PRESENT(globalnode)) THEN
  case_id = 1
ELSE IF (PRESENT(spaceCompo) .AND. PRESENT(timeCompo)) THEN
  case_id = 2
ELSE IF (PRESENT(spaceCompo) .AND. .NOT. PRESENT(timeCompo)) THEN
  case_id = 3
ELSE IF (PRESENT(timeCompo) .AND. .NOT. PRESENT(spaceCompo)) THEN
  case_id = 4
END IF

SELECT CASE (case_id)
! globalnode
CASE (1)

  CALL Reallocate(VALUE, obj%spaceCompo * obj%timeCompo)

  SELECT CASE (obj%fieldType)
  CASE (FIELD_TYPE_CONSTANT)
    CALL lis_vector_get_values(obj%lis_ptr, 1, SIZE(VALUE, kind=I4B), &
      & VALUE, ierr)
    CALL CHKERR(ierr)

  CASE (FIELD_TYPE_NORMAL)
    localNode = obj%domain%getLocalNodeNumber(globalNode=globalNode)
    indx = GetNodeLoc(&
      & obj=obj%dof, &
      & nodenum=localNode, &
      & ivar=1, &
      & timeCompo=1, &
      & spaceCompo=1 &
      & )
    CALL lis_vector_get_values(obj%lis_ptr, indx, SIZE(VALUE, kind=I4B), &
      & VALUE, ierr)
    CALL CHKERR(ierr)

  END SELECT

! all nodal values of space-timeCompo
CASE (2)
  tNodes = obj%domain%getTotalNodes()
  CALL Reallocate(VALUE, tNodes)
  DO ii = 1, tNodes
    indx = GetNodeLoc(&
      & obj=obj%dof, &
      & nodenum=ii, &
      & ivar=1, &
      & timeCompo=timeCompo, &
      & spaceCompo=spaceCompo &
      & )
    CALL lis_vector_get_value( &
      & obj%lis_ptr, indx, &
      & VALUE(ii), ierr)
    CALL CHKERR(ierr)
  END DO

! all nodal values of spaceCompo
CASE (3)

  tNodes = obj%domain%getTotalNodes()
  CALL Reallocate(VALUE, tNodes * obj%timeCompo)
  kk = 0
  DO ii = 1, tNodes
    DO jj = 1, obj%timeCompo
      indx = GetNodeLoc(&
        & obj=obj%dof, &
        & nodenum=ii, &
        & ivar=1, &
        & timeCompo=jj, &
        & spaceCompo=spaceCompo &
        & )
      kk = kk + 1
      CALL lis_vector_get_value( &
        & obj%lis_ptr, indx, &
        & VALUE(kk), ierr)
      CALL CHKERR(ierr)
    END DO
  END DO

! all nodal values of timeCompoCompo
CASE (4)
  tNodes = obj%domain%getTotalNodes()
  CALL Reallocate(VALUE, tNodes * obj%spaceCompo)
  kk = 0
  DO ii = 1, tNodes
    DO jj = 1, obj%spaceCompo
      indx = GetNodeLoc(&
        & obj=obj%dof, &
        & nodenum=ii, &
        & ivar=1, &
        & timeCompo=timeCompo, &
        & spaceCompo=jj &
        & )
      kk = kk + 1
      CALL lis_vector_get_value( &
        & obj%lis_ptr, indx, &
        & VALUE(kk), ierr)
      CALL CHKERR(ierr)
    END DO
  END DO

CASE DEFAULT
  CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'No case found for given arguments'// &
  & ' Either globalnode should be present'// &
  & ' spaceCompo and timeCompo both should be present'// &
  & ' spaceCompo or timeCompo should be present')
END SELECT

END PROCEDURE obj_get1

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_get2
CHARACTER(*), PARAMETER :: myName = "obj_get2"
INTEGER(I4B) :: tNodes
INTEGER(I4B) :: ierr

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either VectorFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

tNodes = obj%domain%getTotalNodes()
CALL reallocate(VALUE, obj%spaceCompo, obj%timeCompo, tNodes)

CALL lis_vector_get_values( &
& obj%lis_ptr, &
& 1_I4B, &
& SIZE(VALUE, kind=I4B), &
& VALUE, ierr)
CALL CHKERR(ierr)

END PROCEDURE obj_get2

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_get3
CHARACTER(*), PARAMETER :: myName = "obj_get3"
INTEGER(I4B) :: ierr
INTEGER(I4B) :: localNode(SIZE(globalnode))
INTEGER(I4B) :: ii
INTEGER(I4B) :: jj
INTEGER(I4B) :: kk
INTEGER(I4B) :: tsize
INTEGER(I4B) :: indx

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either VectorFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

localNode = obj%domain%getLocalNodeNumber(globalnode=globalnode)

IF (ANY(localNode .EQ. 0_I4B)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'Some globalnode are out of bound')
END IF

tsize = SIZE(globalnode)

CALL Reallocate(VALUE, obj%spaceCompo, obj%timeCompo, tsize)

DO ii = 1, tsize
  DO jj = 1, obj%timeCompo
    DO kk = 1, obj%spaceCompo
      indx = GetNodeLoc(&
        & obj=obj%dof, &
        & nodenum=localNode(ii), &
        & ivar=1, &
        & timeCompo=jj, &
        & spaceCompo=kk &
        & )
      CALL lis_vector_get_value( &
        & obj%lis_ptr, indx, &
        & VALUE(kk, jj, ii), ierr)
      CALL CHKERR(ierr)
    END DO
  END DO
END DO

END PROCEDURE obj_get3

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_get4
CHARACTER(*), PARAMETER :: myName = "obj_get4"
INTEGER(I4B) :: ierr
INTEGER(I4B) :: localNode(SIZE(globalnode))
INTEGER(I4B) :: ii
INTEGER(I4B) :: tsize
INTEGER(I4B) :: indx

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either VectorFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

IF (spaceCompo .GT. obj%spaceCompo) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'spaceCompo is greater than obj%spaceCompo')
END IF

IF (timeCompo .GT. obj%timeCompo) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'timeCompo is greater than obj%timeCompo')
END IF

localNode = obj%domain%getLocalNodeNumber(globalnode=globalnode)

IF (ANY(localNode .EQ. 0_I4B)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'Some globalnode are out of bound')
END IF

tsize = SIZE(globalnode)

CALL Reallocate(VALUE, tsize)

DO ii = 1, tsize
  indx = GetNodeLoc(&
    & obj=obj%dof, &
    & nodenum=localNode(ii), &
    & ivar=1, &
    & timeCompo=timeCompo, &
    & spaceCompo=spaceCompo &
    & )
  CALL lis_vector_get_value( &
    & obj%lis_ptr, indx, &
    & VALUE(ii), ierr)
  CALL CHKERR(ierr)
END DO

END PROCEDURE obj_get4

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_get5
CHARACTER(*), PARAMETER :: myName = "obj_get5"
INTEGER(I4B) :: ierr
INTEGER(I4B) :: localNode
INTEGER(I4B) :: ii
INTEGER(I4B) :: indx

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either VectorFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

IF (spaceCompo .GT. obj%spaceCompo) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'spaceCompo is greater than obj%spaceCompo')
END IF

IF (timeCompo .GT. obj%timeCompo) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'timeCompo is greater than obj%timeCompo')
END IF

localNode = obj%domain%getLocalNodeNumber(globalnode=globalnode)

IF ((localNode .EQ. 0_I4B)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'Globalnode is out of bound')
END IF

indx = GetNodeLoc(&
  & obj=obj%dof, &
  & nodenum=localNode, &
  & ivar=1, &
  & timeCompo=timeCompo, &
  & spaceCompo=spaceCompo &
  & )
CALL lis_vector_get_value( &
  & obj%lis_ptr, indx, &
  & VALUE, ierr)
CALL CHKERR(ierr)

END PROCEDURE obj_get5

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_get6
INTEGER(I4B) :: globalNode(INT(1 + (iend - istart) / stride)), ii, jj
jj = 0
DO ii = istart, iend, stride
  jj = jj + 1
  globalNode(jj) = ii
END DO
CALL obj%get(globalNode=globalNode, VALUE=VALUE)
END PROCEDURE obj_get6

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_get7
INTEGER(I4B) :: globalNode(INT(1 + (iend - istart) / stride)), ii, jj
jj = 0
DO ii = istart, iend, stride
  jj = jj + 1
  globalNode(jj) = ii
END DO
CALL obj%get( &
  & globalNode=globalNode, &
  & VALUE=VALUE, &
  & spaceCompo=spaceCompo, &
  & timeCompo=timeCompo)
END PROCEDURE obj_get7

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_get8
CHARACTER(*), PARAMETER :: myName = "obj_get8"
INTEGER(I4B) :: ierr
INTEGER(I4B) :: localNode
INTEGER(I4B) :: jj
INTEGER(I4B) :: kk
INTEGER(I4B) :: tsize
INTEGER(I4B) :: indx

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either VectorFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

localNode = obj%domain%getLocalNodeNumber(globalnode=globalnode)

IF ((localNode .EQ. 0_I4B)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'Globalnode is out of bound')
END IF

CALL Reallocate(VALUE, obj%spaceCompo, obj%timeCompo)

DO jj = 1, obj%timeCompo
  DO kk = 1, obj%spaceCompo
    indx = GetNodeLoc(&
      & obj=obj%dof, &
      & nodenum=localNode, &
      & ivar=1, &
      & timeCompo=jj, &
      & spaceCompo=kk &
      & )
    CALL lis_vector_get_value( &
      & obj%lis_ptr, indx, &
      & VALUE(kk, jj), ierr)
    CALL CHKERR(ierr)
  END DO
END DO

END PROCEDURE obj_get8

!----------------------------------------------------------------------------
!                                                                      get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_get9
REAL(DFP), ALLOCATABLE :: m3a(:, :, :), m3b(:, :, :)
CALL obj%get(VALUE=m3b, globalNode=globalNode)

! Here m3b is in (i, a, J) format,
! so we have to swap the dimensions to (i,J,a)
! We will call swap method from Utility.
CALL SWAP(a=m3a, b=m3b, i1=1, i2=3, i3=2)
VALUE = NodalVariable(m3a, TypeFEVariableVector, &
  & TypeFEVariableSpacetime)

DEALLOCATE (m3a, m3b)
END PROCEDURE obj_get9

!----------------------------------------------------------------------------
!                                                                 Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_get10
CHARACTER(*), PARAMETER :: myName = "obj_get10"
INTEGER(I4B) :: case_id
INTEGER(I4B) :: ierr
INTEGER(I4B) :: tNodes
INTEGER(I4B) :: ii
INTEGER(I4B) :: jj
INTEGER(I4B) :: kk
INTEGER(I4B) :: globalnode
REAL(DFP) :: avar

CALL lis_vector_is_null(obj%lis_ptr, ierr)
IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either VectorFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

IF (.NOT. VALUE%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either AbstractNodeField_::value is not initiated'// &
  & "")
END IF

tNodes = obj%domain%getTotalNodes()

IF (tNodes .NE. VALUE%domain%getTotalNodes()) THEN
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
      globalnode = obj%domain%getGlobalNodeNumber(localNode=ii)
      CALL obj%get(VALUE=avar, globalnode=globalnode, &
        & spaceCompo=spaceCompo, timeCompo=timeCompo)
      CALL VALUE%set(VALUE=avar, globalnode=globalnode)
    END DO

  CLASS IS (STScalarField_)
    DO ii = 1, tNodes
      globalnode = obj%domain%getGlobalNodeNumber(localNode=ii)
      CALL obj%get(VALUE=avar, globalnode=globalnode, &
        & spaceCompo=spaceCompo, timeCompo=timeCompo)
      CALL VALUE%set(VALUE=avar, globalnode=globalnode, timeCompo=timeCompo)
    END DO

  CLASS IS (VectorField_)
    DO ii = 1, tNodes
      globalnode = obj%domain%getGlobalNodeNumber(localNode=ii)
      CALL obj%get(VALUE=avar, globalnode=globalnode, &
        & spaceCompo=spaceCompo, timeCompo=timeCompo)
      CALL VALUE%set(VALUE=avar, globalnode=globalnode, spaceCompo=spaceCompo)
    END DO

  CLASS IS (STVectorField_)
    DO ii = 1, tNodes
      globalnode = obj%domain%getGlobalNodeNumber(localNode=ii)
      CALL obj%get(VALUE=avar, globalnode=globalnode, &
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
      globalnode = obj%domain%getGlobalNodeNumber(localNode=ii)
      DO jj = 1, obj%timeCompo
        CALL obj%get(VALUE=avar, globalnode=globalnode, &
          & spaceCompo=spaceCompo, timeCompo=jj)
        CALL VALUE%set(VALUE=avar, globalnode=globalnode, timeCompo=jj)
      END DO
    END DO

  CLASS IS (STVectorField_)
    DO ii = 1, tNodes
      globalnode = obj%domain%getGlobalNodeNumber(localNode=ii)
      DO jj = 1, obj%timeCompo
        CALL obj%get(VALUE=avar, globalnode=globalnode, &
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
      globalnode = obj%domain%getGlobalNodeNumber(localNode=ii)
      DO jj = 1, obj%spaceCompo
        CALL obj%get(VALUE=avar, globalnode=globalnode, &
          & spaceCompo=jj, timeCompo=timeCompo)
        CALL VALUE%set(VALUE=avar, globalnode=globalnode, spaceCompo=jj)
      END DO
    END DO

  CLASS IS (STVectorField_)
    DO ii = 1, tNodes
      globalnode = obj%domain%getGlobalNodeNumber(localNode=ii)
      DO jj = 1, obj%spaceCompo
        CALL obj%get(VALUE=avar, globalnode=globalnode, &
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
      globalnode = obj%domain%getGlobalNodeNumber(localNode=ii)
      DO jj = 1, obj%timeCompo
        DO kk = 1, obj%spaceCompo
          CALL obj%get(VALUE=avar, globalnode=globalnode, &
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

END PROCEDURE obj_get10

!----------------------------------------------------------------------------
!                                                     getPointerOfComponent
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_getPointer
CHARACTER(*), PARAMETER :: myName = "obj_getPointer"
CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'This method is not available for STVectorFieldLis_')
END PROCEDURE obj_getPointer

!----------------------------------------------------------------------------
!                                                     getPointerOfComponent
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_getPointerOfComponent
CHARACTER(*), PARAMETER :: myName = "obj_getPointerOfComponent"
CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'This method is not available for STVectorFieldLis_')
END PROCEDURE obj_getPointerOfComponent

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetMethods
