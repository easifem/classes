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

SUBMODULE(STScalarFieldLis_Class) GetMethods
USE BaseMethod
USE ScalarFieldLis_Class
USE ScalarField_Class
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
LOGICAL(LGT) :: bool1, bool2
INTEGER(I4B) :: ierr
INTEGER(I4B) :: ii
INTEGER(I4B) :: localNode
INTEGER(I4B) :: indx(obj%timeCompo)

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either ScalarField object is not initiated'// &
  & " or, lis_ptr is not available")
END IF

bool1 = PRESENT(globalNode)
bool2 = PRESENT(timeCompo)

IF (bool1 .AND. bool2) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'Both globalNode and timeCompo cannot be present')
END IF

IF (.NOT. bool1 .AND. .NOT. bool2) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'Either globalNode and timeCompo should be present')
END IF

! globalnode present
IF (bool1) THEN
  SELECT CASE (obj%fieldType)
  CASE (FIELD_TYPE_CONSTANT)

    CALL Reallocate(VALUE, obj%timeCompo)
    CALL lis_vector_get_values(obj%lis_ptr, 1, obj%timeCompo, &
      & VALUE, ierr)
    CALL CHKERR(ierr)

  CASE (FIELD_TYPE_NORMAL)

    localNode = obj%domain%getLocalNodeNumber(globalNode=globalNode)
    CALL Reallocate(VALUE, obj%timeCompo)
    indx = GetNodeLoc(&
      & obj=obj%dof, &
      & nodenum=localNode, &
      & ivar=1, &
      & spaceCompo=1, &
      & timeCompo=arange(1_I4B, obj%timeCompo) &
      & )

    DO ii = 1, obj%timeCompo
      CALL lis_vector_get_value( &
        & obj%lis_ptr, indx(ii), &
        & VALUE(ii), ierr)
      CALL CHKERR(ierr)
    END DO

  END SELECT
END IF

!> get all values of timeCompo
IF (bool2) THEN

  ii = obj%domain%getTotalNodes()
  CALL Reallocate(VALUE, ii)

  DO ii = 1, SIZE(VALUE)

    indx(1) = getNodeLoc(&
      & obj=obj%dof, &
      & nodenum=ii, &
      & idof=timeCompo)

    CALL lis_vector_get_value( &
      & obj%lis_ptr, &
      & indx(1), &
      & VALUE(ii), &
      & ierr)

    CALL CHKERR(ierr)

  END DO

END IF

END PROCEDURE obj_get1

!----------------------------------------------------------------------------
!                                                                       get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_get2
#include "lisf.h"
CHARACTER(*), PARAMETER :: myName = "obj_get2"
INTEGER(I4B) :: ierr
INTEGER(I4B) :: ii
INTEGER(I4B) :: jj
INTEGER(I4B) :: indx

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'Either STScalarFieldLis_::obj is not initiated'// &
    & " or, obj%lis_ptr is not available")
END IF

!> get all values of timeCompo
ii = obj%domain%getTotalNodes()
CALL Reallocate(VALUE, obj%timeCompo, ii)

indx = 0
DO jj = 1, SIZE(VALUE, 2)

  DO ii = 1, obj%timeCompo
    indx = indx + 1

    CALL lis_vector_get_value( &
      & obj%lis_ptr, &
      & indx, &
      & VALUE(ii, jj), &
      & ierr)

    CALL CHKERR(ierr)

  END DO
END DO

END PROCEDURE obj_get2

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_get3
#include "lisf.h"
CHARACTER(*), PARAMETER :: myName = "obj_get3"
INTEGER(I4B) :: ierr
INTEGER(I4B) :: ii
INTEGER(I4B) :: jj
INTEGER(I4B) :: localNode(SIZE(globalNode))
INTEGER(I4B) :: indx(obj%timeCompo * SIZE(globalNode))
REAL(DFP) :: val(obj%timeCompo * SIZE(globalNode))

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'Either STScalarFieldLis_::obj is not initiated'// &
    & " or, obj%lis_ptr is not available")
END IF

localNode = obj%domain%getLocalNodeNumber(globalNode)
ii = obj%domain%getTotalNodes()

IF (ANY(localNode .EQ. 0_I4B)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'localNode is either 0 or greater than size of '// &
    & " STScalarFieldLis_::obj")
END IF

!> get all values of timeCompo
CALL Reallocate(VALUE, obj%timeCompo, SIZE(globalNode))

indx = getNodeLoc(&
  & obj=obj%dof, &
  & nodenum=localNode, &
  & ivar=1, &
  & spaceCompo=1, &
  & timeCompo=arange(1, obj%timeCompo) &
  & )

DO ii = 1, SIZE(indx)

  CALL lis_vector_get_value( &
    & obj%lis_ptr, &
    & indx(ii), &
    & val(ii), &
    & ierr)

  CALL CHKERR(ierr)

END DO

VALUE = RESHAPE(val, [obj%timeCompo, SIZE(globalnode)])
END PROCEDURE obj_get3

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_get4
#include "lisf.h"
CHARACTER(*), PARAMETER :: myName = "obj_get4"
INTEGER(I4B) :: ierr
INTEGER(I4B) :: ii
INTEGER(I4B) :: jj
INTEGER(I4B) :: localNode(SIZE(globalNode))
INTEGER(I4B) :: indx(SIZE(globalNode))

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'Either STScalarFieldLis_::obj is not initiated'// &
    & " or, obj%lis_ptr is not available")
END IF

localNode = obj%domain%getLocalNodeNumber(globalNode)
ii = obj%domain%getTotalNodes()

IF (ANY(localNode .EQ. 0_I4B) .OR. ANY(localNode .GT. ii)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'localNode is either 0 or greater than size of '// &
    & " STScalarFieldLis_::obj")
END IF

!> get all values of timeCompo
CALL Reallocate(VALUE, SIZE(globalNode))

indx = getNodeLoc(&
  & obj=obj%dof, &
  & nodenum=localNode, &
  & ivar=1, &
  & spaceCompo=1, &
  & timeCompo=timeCompo &
  & )

DO ii = 1, SIZE(indx)

  CALL lis_vector_get_value( &
    & obj%lis_ptr, &
    & indx(ii), &
    & VALUE(ii), &
    & ierr)

  CALL CHKERR(ierr)

END DO

END PROCEDURE obj_get4

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_get5
#include "lisf.h"
CHARACTER(*), PARAMETER :: myName = "obj_get5"
INTEGER(I4B) :: ii
INTEGER(I4B) :: localNode
INTEGER(I4B) :: indx
INTEGER(I4B) :: ierr

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'Either STScalarFieldLis_::obj is not initiated'// &
    & " or, obj%lis_ptr is not available")
END IF

localNode = obj%domain%getLocalNodeNumber(globalNode)
ii = obj%domain%getTotalNodes()

IF ((localNode .EQ. 0_I4B) .OR. (localNode .GT. ii)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'localNode is either 0 or greater than size of '// &
    & " STScalarFieldLis_::obj")
END IF

indx = getNodeLoc(&
  & obj=obj%dof, &
  & nodenum=localNode, &
  & ivar=1, &
  & spaceCompo=1, &
  & timeCompo=timeCompo &
  & )

CALL lis_vector_get_value( &
  & obj%lis_ptr, &
  & indx, &
  & VALUE, &
  & ierr)

CALL CHKERR(ierr)

END PROCEDURE obj_get5

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_get6
INTEGER(I4B) :: globalnode(INT(1 + (iend - istart) / stride)), ii, jj
jj = 0
DO ii = istart, iend, stride
  jj = jj + 1
  globalnode(jj) = ii
END DO
CALL obj%get(globalnode=globalnode, VALUE=VALUE)
END PROCEDURE obj_get6

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_get7
INTEGER(I4B) :: globalnode(INT(1 + (iend - istart) / stride)), ii, jj
jj = 0
DO ii = istart, iend, stride
  jj = jj + 1
  globalnode(jj) = ii
END DO
CALL obj%get(globalnode=globalnode, VALUE=VALUE, timeCompo=timeCompo)
END PROCEDURE obj_get7

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_get8
REAL(DFP), ALLOCATABLE :: val(:, :)
CALL obj%get(globalNode=globalNode, VALUE=val)
VALUE = NodalVariable( &
  & val, &
  & TypeFEVariableScalar, &
  & TypeFEVariableSpaceTime)
DEALLOCATE (val)
END PROCEDURE obj_get8

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_get9
CHARACTER(*), PARAMETER :: myName = "obj_get9"
REAL(DFP) :: aval
INTEGER(I4B) :: ii
INTEGER(I4B) :: jj
INTEGER(I4B) :: tsize

SELECT TYPE (VALUE)
CLASS IS (ScalarField_)
  tsize = obj%domain%getTotalNodes()
  DO ii = 1, tsize
    jj = obj%domain%getGlobalNodeNumber(ii)
    CALL obj%get(VALUE=aval, globalNode=jj, timeCompo=timeCompo)
    CALL VALUE%set(VALUE=aval, globalNode=jj)
  END DO
CLASS DEFAULT
  CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'No case found for the type of value')
END SELECT

END PROCEDURE obj_get9

!----------------------------------------------------------------------------
!                                                     getPointerOfComponent
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_getPointerOfComponent
CHARACTER(*), PARAMETER :: myName = "obj_getPointerOfComponent"
CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'This method is not available for STScalarFieldLis_')
END PROCEDURE obj_getPointerOfComponent

!----------------------------------------------------------------------------
!                                                                getPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_getPointer
CHARACTER(*), PARAMETER :: myName = "obj_getPointer"
CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'This method is not available for STScalarFieldLis_')
END PROCEDURE obj_getPointer

END SUBMODULE GetMethods
