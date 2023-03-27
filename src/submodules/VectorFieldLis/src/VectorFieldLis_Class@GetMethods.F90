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

SUBMODULE(VectorFieldLis_Class) GetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_get1
#include "lisf.h"
CHARACTER(*), PARAMETER :: myName = "vField_get1"
LOGICAL(LGT) :: bool1, bool2
INTEGER(I4B) :: ierr
INTEGER(I4B) :: ii
INTEGER(I4B) :: localNode
INTEGER(I4B) :: indx(obj%spaceCompo)

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either VectorFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

bool1 = PRESENT(globalNode)
bool2 = PRESENT(spaceCompo)

IF (bool1 .AND. bool2) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'Both globalNode and spaceCompo cannot be present')
END IF

IF (.NOT. bool1 .AND. .NOT. bool2) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'Either globalNode and spaceCompo should be present')
END IF

! globalnode present
IF (bool1) THEN
  SELECT CASE (obj%fieldType)
  CASE (FIELD_TYPE_CONSTANT)

    CALL Reallocate(VALUE, obj%spaceCompo)
    CALL lis_vector_get_values(obj%lis_ptr, 1, obj%spaceCompo, &
      & VALUE, ierr)
    CALL CHKERR(ierr)

  CASE (FIELD_TYPE_NORMAL)

    localNode = obj%domain%getLocalNodeNumber(globalNode=globalNode)
    CALL Reallocate(VALUE, obj%spaceCompo)
    indx = GetNodeLoc(&
      & obj=obj%dof, &
      & nodenum=localNode, &
      & ivar=1, &
      & timeCompo=1, &
      & spaceCompo=arange(1_I4B, obj%spaceCompo) &
      & )

    DO ii = 1, obj%spaceCompo
      CALL lis_vector_get_value( &
        & obj%lis_ptr, indx(ii), &
        & VALUE(ii), ierr)
      CALL CHKERR(ierr)
    END DO

  END SELECT
END IF

!> get all values of spaceCompo
IF (bool2) THEN

  ii = obj%domain%getTotalNodes()
  CALL Reallocate(VALUE, ii)

  DO ii = 1, SIZE(VALUE)

    indx(1) = getNodeLoc(&
      & obj=obj%dof, &
      & nodenum=ii, &
      & idof=spaceCompo)

    CALL lis_vector_get_value( &
      & obj%lis_ptr, &
      & indx(1), &
      & VALUE(ii), &
      & ierr)

    CALL CHKERR(ierr)

  END DO

END IF

END PROCEDURE vField_get1

!----------------------------------------------------------------------------
!                                                                       get
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_get2
CHARACTER(*), PARAMETER :: myName = "vField_get2"
INTEGER(I4B) :: ierr
INTEGER(I4B) :: ii
INTEGER(I4B) :: jj
INTEGER(I4B) :: indx

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'Either VectorFieldLis_::obj is not initiated'// &
    & " or, obj%lis_ptr is not available")
END IF

!> get all values of spaceCompo
ii = obj%domain%getTotalNodes()

IF (PRESENT(force3D)) THEN
  CALL Reallocate(VALUE, 3_I4B, ii)
ELSE
  CALL Reallocate(VALUE, obj%spaceCompo, ii)
END IF

indx = 0
DO jj = 1, SIZE(VALUE, 2)

  DO ii = 1, obj%spaceCompo
    indx = indx + 1

    CALL lis_vector_get_value( &
      & obj%lis_ptr, &
      & indx, &
      & VALUE(ii, jj), &
      & ierr)

    CALL CHKERR(ierr)

  END DO
END DO

END PROCEDURE vField_get2

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_get3
CHARACTER(*), PARAMETER :: myName = "vField_get3"
INTEGER(I4B) :: ierr
INTEGER(I4B) :: ii
INTEGER(I4B) :: jj
INTEGER(I4B) :: localNode(SIZE(globalNode))
INTEGER(I4B) :: indx(obj%spaceCompo * SIZE(globalNode))
REAL(DFP) :: val(obj%spaceCompo * SIZE(globalNode))

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'Either VectorFieldLis_::obj is not initiated'// &
    & " or, obj%lis_ptr is not available")
END IF

localNode = obj%domain%getLocalNodeNumber(globalNode)
ii = obj%domain%getTotalNodes()

IF (ANY(localNode .EQ. 0_I4B)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'localNode is either 0 or greater than size of '// &
    & " VectorFieldLis_::obj")
END IF

!> get all values of spaceCompo
IF (PRESENT(force3D)) THEN
  CALL Reallocate(VALUE, 3_I4B, SIZE(globalNode))
ELSE
  CALL Reallocate(VALUE, obj%spaceCompo, SIZE(globalNode))
END IF

indx = getNodeLoc(&
  & obj=obj%dof, &
  & nodenum=localNode, &
  & ivar=1, &
  & timeCompo=1, &
  & spaceCompo=arange(1, obj%spaceCompo) &
  & )

DO ii = 1, SIZE(indx)

  CALL lis_vector_get_value( &
    & obj%lis_ptr, &
    & indx(ii), &
    & val(ii), &
    & ierr)

  CALL CHKERR(ierr)

END DO

VALUE(1:obj%spaceCompo, :) = &
& RESHAPE(val, [obj%spaceCompo, SIZE(globalnode)])

END PROCEDURE vField_get3

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_get4
CHARACTER(*), PARAMETER :: myName = "vField_get2"
INTEGER(I4B) :: ierr
INTEGER(I4B) :: ii
INTEGER(I4B) :: jj
INTEGER(I4B) :: localNode(SIZE(globalNode))
INTEGER(I4B) :: indx(SIZE(globalNode))

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'Either VectorFieldLis_::obj is not initiated'// &
    & " or, obj%lis_ptr is not available")
END IF

localNode = obj%domain%getLocalNodeNumber(globalNode)

IF (ANY(localNode .EQ. 0_I4B)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'localNode is either 0 or greater than size of '// &
    & " VectorFieldLis_::obj")
END IF

!> get all values of spaceCompo
CALL Reallocate(VALUE, SIZE(globalNode))

indx = getNodeLoc(&
  & obj=obj%dof, &
  & nodenum=localNode, &
  & ivar=1, &
  & timeCompo=1, &
  & spaceCompo=spaceCompo &
  & )

DO ii = 1, SIZE(indx)

  CALL lis_vector_get_value( &
    & obj%lis_ptr, &
    & indx(ii), &
    & VALUE(ii), &
    & ierr)

  CALL CHKERR(ierr)

END DO

END PROCEDURE vField_get4

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_get5
CHARACTER(*), PARAMETER :: myName = "vField_get2"
INTEGER(I4B) :: ii
INTEGER(I4B) :: localNode
INTEGER(I4B) :: indx
INTEGER(I4B) :: ierr

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'Either VectorFieldLis_::obj is not initiated'// &
    & " or, obj%lis_ptr is not available")
END IF

localNode = obj%domain%getLocalNodeNumber(globalNode)

IF ((localNode .EQ. 0_I4B)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'localNode is either 0 or greater than size of '// &
    & " VectorFieldLis_::obj")
END IF

indx = getNodeLoc(&
  & obj=obj%dof, &
  & nodenum=localNode, &
  & ivar=1, &
  & timeCompo=1, &
  & spaceCompo=spaceCompo &
  & )

CALL lis_vector_get_value( &
  & obj%lis_ptr, &
  & indx, &
  & VALUE, &
  & ierr)

CALL CHKERR(ierr)

END PROCEDURE vField_get5

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_get6
INTEGER(I4B) :: globalnode(INT(1 + (iend - istart) / stride)), ii, jj
jj = 0
DO ii = istart, iend, stride
  jj = jj + 1
  globalnode(jj) = ii
END DO
CALL obj%get(globalnode=globalnode, VALUE=VALUE)
END PROCEDURE vField_get6

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_get7
INTEGER(I4B) :: globalnode(INT(1 + (iend - istart) / stride)), ii, jj
jj = 0
DO ii = istart, iend, stride
  jj = jj + 1
  globalnode(jj) = ii
END DO
CALL obj%get(globalnode=globalnode, VALUE=VALUE, spaceCompo=spaceCompo)
END PROCEDURE vField_get7

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_get8
REAL(DFP), ALLOCATABLE :: val(:, :)
CALL obj%get(globalNode=globalNode, VALUE=val)
VALUE = NodalVariable( &
  & val, &
  & TypeFEVariableScalar, &
  & TypeFEVariableSpaceTime)
DEALLOCATE (val)
END PROCEDURE vField_get8

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_get9
CHARACTER(*), PARAMETER :: myName = "vField_get9"
REAL(DFP) :: aval
INTEGER(I4B) :: ii
INTEGER(I4B) :: jj
INTEGER(I4B) :: tsize

tsize = obj%domain%getTotalNodes()

DO ii = 1, tsize
  jj = obj%domain%getGlobalNodeNumber(ii)
  CALL obj%get(VALUE=aval, globalNode=jj, spaceCompo=spaceCompo)
  CALL VALUE%set(VALUE=aval, globalNode=jj)
END DO

END PROCEDURE vField_get9

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_get10
CHARACTER(*), PARAMETER :: myName = "vField_get10"
INTEGER(I4B) :: ii
INTEGER(I4B) :: jj
INTEGER(I4B) :: tsize
REAL(DFP), ALLOCATABLE :: aval(:)

tsize = obj%domain%getTotalNodes()

DO ii = 1, tsize
  jj = obj%domain%getGlobalNodeNumber(ii)
  CALL obj%get(VALUE=aval, globalNode=jj)
  CALL VALUE%set(VALUE=aval, globalNode=jj)
END DO

END PROCEDURE vField_get10

!----------------------------------------------------------------------------
!                                                     getPointerOfComponent
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_getPointerOfComponent
CHARACTER(*), PARAMETER :: myName = "vField_getPointerOfComponent"
CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'This method is not available for VectorFieldLis_')
END PROCEDURE vField_getPointerOfComponent

!----------------------------------------------------------------------------
!                                                                getPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE vField_getPointer
CHARACTER(*), PARAMETER :: myName = "vField_getPointer"
CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'This method is not available for VectorFieldLis_')
END PROCEDURE vField_getPointer

END SUBMODULE GetMethods
