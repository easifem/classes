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

SUBMODULE(BlockNodeFieldLis_Class) GetMethods
USE BaseMethod
USE Field
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             GetSingle
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
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get1
CHARACTER(*), PARAMETER :: myName = "obj_Get1"
INTEGER(I4B) :: ierr
INTEGER(I4B) :: localNode
INTEGER(I4B) :: indx

CALL lis_vector_is_null(obj%lis_ptr, ierr)
CALL CHKERR(ierr)
IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either BlockNodeFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

localNode = obj%domains(ivar)%ptr%GetLocalNodeNumber(globalNode)
IF (localNode .EQ. 0_I4B) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'globalNode is out of bound')
END IF

indx = GetNodeLoc(&
  & obj=obj%dof, &
  & ivar=ivar, &
  & idof=idof, &
  & nodenum=localNode)

CALL obj%GetSingle(indx=indx, VALUE=VALUE)

END PROCEDURE obj_Get1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get2
CHARACTER(*), PARAMETER :: myName = "obj_Get2"
INTEGER(I4B) :: ierr
INTEGER(I4B) :: tsize

CALL lis_vector_is_null(obj%lis_ptr, ierr)
CALL CHKERR(ierr)
IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either BlockNodeFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF
tsize = obj%SIZE()
CALL reallocate(VALUE, tsize)
CALL lis_vector_gather(obj%lis_ptr, VALUE, ierr)
CALL CHKERR(ierr)
END PROCEDURE obj_Get2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get3
CHARACTER(*), PARAMETER :: myName = "obj_Get3"
INTEGER(I4B) :: ierr
INTEGER(I4B) :: tsize
INTEGER(I4B) :: localNode(SIZE(globalNode))
INTEGER(I4B) :: indx(SIZE(globalNode))
INTEGER(I4B) :: ii

CALL lis_vector_is_null(obj%lis_ptr, ierr)
CALL CHKERR(ierr)
IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either BlockNodeFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

tsize = SIZE(globalNode)
CALL reallocate(VALUE, tsize)

localNode = obj%domains(ivar)%ptr%GetLocalNodeNumber(globalNode)

IF (ANY(localNode .EQ. 0_I4B)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'Some of globalNodes are out of bound')
END IF

indx = GetNodeLoc(&
  & obj=obj%dof, ivar=ivar, idof=idof, nodenum=localNode)

DO ii = 1, tsize
  CALL obj%GetSingle(indx=indx(ii), VALUE=VALUE(ii))
END DO

END PROCEDURE obj_Get3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get7
CHARACTER(*), PARAMETER :: myName = "obj_Get7"
INTEGER(I4B) :: ierr
INTEGER(I4B) :: tsize
INTEGER(I4B) :: localNode(SIZE(globalNode))
INTEGER(I4B) :: indx(SIZE(globalNode))
INTEGER(I4B) :: ii

CALL lis_vector_is_null(obj%lis_ptr, ierr)
CALL CHKERR(ierr)
IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either BlockNodeFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

tsize = SIZE(globalNode)
CALL reallocate(VALUE, tsize)

localNode = obj%domains(ivar)%ptr%GetLocalNodeNumber(globalNode)

IF (ANY(localNode .EQ. 0_I4B)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'Some of globalNodes are out of bound')
END IF

indx = GetNodeLoc(&
  & obj=obj%dof, ivar=ivar, spaceCompo=spaceCompo, &
  & timeCompo=timeCompo, nodenum=localNode)

DO ii = 1, tsize
  CALL obj%GetSingle(indx=indx(ii), VALUE=VALUE(ii))
END DO

END PROCEDURE obj_Get7

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetMethods
