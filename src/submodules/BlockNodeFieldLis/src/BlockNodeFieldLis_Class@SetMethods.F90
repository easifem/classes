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

SUBMODULE(BlockNodeFieldLis_Class) SetMethods
USE BaseMethod
USE Field
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 SetSingle
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSingle
#include "lisf.h"
INTEGER(I4B) :: i, ierr
REAL(DFP) :: value0

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  i = 1
ELSE
  i = indx
END IF

value0 = INPUT(option=scale, default=1.0_DFP) * VALUE

IF (PRESENT(addContribution)) THEN
  CALL lis_vector_set_value( &
    & LIS_ADD_VALUE, &
    & i, &
    & value0, &
    & obj%lis_ptr, &
    & ierr &
    & )
  CALL CHKERR(ierr)
ELSE
  CALL lis_vector_set_value( &
    & LIS_INS_VALUE, &
    & i, &
    & value0, &
    & obj%lis_ptr, &
    & ierr &
    & )
  CALL CHKERR(ierr)
END IF

END PROCEDURE obj_SetSingle

!----------------------------------------------------------------------------
!                                                               SetMultiple
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMultiple
INTEGER(I4B) :: ierr, n
REAL(DFP) :: value0(SIZE(VALUE))

n = SIZE(indx)
value0 = INPUT(option=scale, default=1.0_DFP) * VALUE

IF (PRESENT(addContribution)) THEN
  CALL lis_vector_set_values( &
    & LIS_ADD_VALUE, &
    & n, &
    & indx, &
    & value0, &
    & obj%lis_ptr, &
    & ierr &
    & )
  CALL CHKERR(ierr)
ELSE
  CALL lis_vector_set_values( &
    & LIS_INS_VALUE, &
    & n, &
    & indx, &
    & value0, &
    & obj%lis_ptr, &
    & ierr &
    & )
  CALL CHKERR(ierr)
END IF
END PROCEDURE obj_SetMultiple

!----------------------------------------------------------------------------
!                                                                     SetAll
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetAll
INTEGER(I4B) :: ierr, ii, n
REAL(DFP) :: value0

value0 = INPUT(option=scale, default=1.0_DFP) * VALUE

IF (PRESENT(addContribution)) THEN
  n = obj%SIZE()
  DO ii = 1, n
    CALL lis_vector_set_value( &
      & LIS_ADD_VALUE, &
      & ii, &
      & value0, &
      & obj%lis_ptr, &
      & ierr &
      & )
    CALL CHKERR(ierr)
  END DO
ELSE
  CALL lis_vector_set_all( &
    & value0, &
    & obj%lis_ptr, &
    & ierr &
    & )
  CALL CHKERR(ierr)
END IF
END PROCEDURE obj_SetAll

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set1
CHARACTER(*), PARAMETER :: myName = "obj_Set1"
INTEGER(I4B) :: ierr
CALL lis_vector_is_null(obj%lis_ptr, ierr)
CALL CHKERR(ierr)
IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either BlockNodeFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF
CALL obj%setAll(VALUE=VALUE, scale=scale, addContribution=addContribution)
END PROCEDURE obj_Set1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set2
CHARACTER(*), PARAMETER :: myName = "obj_Set2"
INTEGER(I4B) :: tsize
INTEGER(I4B) :: ierr
CALL lis_vector_is_null(obj%lis_ptr, ierr)
CALL CHKERR(ierr)
IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either BlockNodeFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

tsize = obj%SIZE()

IF (tsize .NE. SIZE(VALUE)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'Size of obj should be same as size of value')
END IF

CALL lis_vector_scatter(VALUE, obj%lis_ptr, ierr)
CALL CHKERR(ierr)

END PROCEDURE obj_Set2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set3
CHARACTER(*), PARAMETER :: myName = "obj_Set3"
INTEGER(I4B) :: localNode
INTEGER(I4B) :: ierr
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
  & nodenum=localNode, &
  & ivar=ivar, &
  & idof=idof)

CALL obj%setSingle(indx=indx, VALUE=VALUE, scale=scale, &
  & addContribution=addContribution)

END PROCEDURE obj_Set3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set4
CHARACTER(*), PARAMETER :: myName = "obj_Set4"
INTEGER(I4B) :: localNode(SIZE(globalNode))
INTEGER(I4B) :: ierr
INTEGER(I4B), ALLOCATABLE :: indx(:)
INTEGER(I4B) :: ii

CALL lis_vector_is_null(obj%lis_ptr, ierr)
CALL CHKERR(ierr)
IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either BlockNodeFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

localNode = obj%domains(ivar)%ptr%GetLocalNodeNumber(globalNode)

IF (ANY(localNode .EQ. 0_I4B)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'Some globalNodes are out of bound')
END IF

indx = GetIndex(&
  & obj=obj%dof, &
  & ivar=ivar, &
  & nodenum=localNode)

DO ii = 1, SIZE(indx)
  CALL obj%setSingle(VALUE=VALUE, indx=indx(ii), &
    & scale=scale, addContribution=addContribution)
END DO

DEALLOCATE (indx)
END PROCEDURE obj_Set4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set5
CHARACTER(*), PARAMETER :: myName = "obj_Set5"
INTEGER(I4B) :: localNode(SIZE(globalNode))
INTEGER(I4B) :: tsize
INTEGER(I4B), ALLOCATABLE :: indx(:)

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'BlockNodeField_::obj is not initiated')
END IF

localNode = obj%domains(ivar)%ptr%GetLocalNodeNumber(globalNode)

IF (ANY(localNode .EQ. 0_I4B)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'Some globalNodes are out of bound')
END IF

tsize = (obj%dof.tdof.ivar) * SIZE(globalNode)

IF (SIZE(VALUE) .NE. tsize) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'The size of value not same as the size of globalNode '// &
    & ' times the total degree of freedom in ivar')
END IF

indx = GetIndex(&
  & obj=obj%dof, &
  & nodenum=localNode, &
  & ivar=ivar)

CALL obj%SetMultiple(&
  & VALUE=VALUE, &
  & indx=indx, &
  & scale=scale, &
  & addContribution=addContribution)

DEALLOCATE (indx)

END PROCEDURE obj_Set5

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set6
CHARACTER(*), PARAMETER :: myName = "obj_Set6"
INTEGER(I4B) :: localNode(SIZE(globalNode))
INTEGER(I4B) :: ierr
INTEGER(I4B) :: indx(SIZE(globalNode))
REAL(DFP) :: value0(SIZE(globalNode))

CALL lis_vector_is_null(obj%lis_ptr, ierr)
CALL CHKERR(ierr)
IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either BlockNodeFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

localNode = obj%domains(ivar)%ptr%GetLocalNodeNumber(globalNode)

IF (ANY(localNode .EQ. 0_I4B)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'Some globalNodes are out of bound')
END IF

indx = GetNodeLoc(&
  & obj=obj%dof, &
  & ivar=ivar, &
  & idof=idof, &
  & nodenum=localNode)

value0 = VALUE

CALL obj%SetMultiple(&
  & VALUE=value0, &
  & indx=indx, &
  & scale=scale, &
  & addContribution=addContribution)

END PROCEDURE obj_Set6

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set7
CHARACTER(*), PARAMETER :: myName = "obj_Set6"
INTEGER(I4B) :: localNode(SIZE(globalNode))
INTEGER(I4B) :: ierr
INTEGER(I4B) :: indx(SIZE(globalNode))

CALL lis_vector_is_null(obj%lis_ptr, ierr)
CALL CHKERR(ierr)
IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either BlockNodeFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

localNode = obj%domains(ivar)%ptr%GetLocalNodeNumber(globalNode)

IF (ANY(localNode .EQ. 0_I4B)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'Some globalNodes are out of bound')
END IF

IF (SIZE(VALUE) .NE. SIZE(globalNode)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'Size of value is not equal to size of globalNode.')
END IF

indx = GetNodeLoc(&
  & obj=obj%dof, &
  & ivar=ivar, &
  & idof=idof, &
  & nodenum=localNode)

CALL obj%SetMultiple(&
  & VALUE=VALUE, &
  & indx=indx, &
  & scale=scale, &
  & addContribution=addContribution)
END PROCEDURE obj_Set7

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set8
CHARACTER(*), PARAMETER :: myName = "obj_Set6"
INTEGER(I4B) :: localNode(SIZE(globalNode))
INTEGER(I4B) :: ierr
INTEGER(I4B) :: indx(SIZE(globalNode))

CALL lis_vector_is_null(obj%lis_ptr, ierr)
CALL CHKERR(ierr)
IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either BlockNodeFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

localNode = obj%domains(ivar)%ptr%GetLocalNodeNumber(globalNode)

IF (ANY(localNode .EQ. 0_I4B)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'Some globalNodes are out of bound')
END IF

IF (SIZE(VALUE) .NE. SIZE(globalNode)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'Size of value is not equal to size of globalNode.')
END IF

indx = GetNodeLoc(&
  & obj=obj%dof, &
  & ivar=ivar, &
  & spaceCompo=spaceCompo, &
  & timeCompo=timeCompo, &
  & nodenum=localNode)

CALL obj%SetMultiple(&
  & VALUE=VALUE, &
  & indx=indx, &
  & scale=scale, &
  & addContribution=addContribution)
END PROCEDURE obj_Set8

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set9
CHARACTER(*), PARAMETER :: myName = "obj_Set9"
INTEGER(I4B) :: localNode(SIZE(globalNode))
INTEGER(I4B) :: ierr
INTEGER(I4B) :: indx(SIZE(globalNode))
REAL(DFP) :: value0(SIZE(globalNode))

CALL lis_vector_is_null(obj%lis_ptr, ierr)
CALL CHKERR(ierr)
IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either BlockNodeFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

localNode = obj%domains(ivar)%ptr%GetLocalNodeNumber(globalNode)

IF (ANY(localNode .EQ. 0_I4B)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'Some globalNodes are out of bound')
END IF

indx = GetNodeLoc(&
  & obj=obj%dof, &
  & ivar=ivar, &
  & spaceCompo=spaceCompo, &
  & timeCompo=timeCompo, &
  & nodenum=localNode)

value0 = VALUE

CALL obj%SetMultiple(&
  & VALUE=value0, &
  & indx=indx, &
  & scale=scale, &
  & addContribution=addContribution)
END PROCEDURE obj_Set9

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set10
CHARACTER(*), PARAMETER :: myName = "obj_Set10"
INTEGER(I4B) :: localNode(SIZE(globalNode))
INTEGER(I4B) :: ierr
INTEGER(I4B) :: indx(SIZE(globalNode))
INTEGER(I4B) :: tsize
INTEGER(I4B) :: nn
INTEGER(I4B) :: ii
INTEGER(I4B) :: jj
INTEGER(I4B) :: kk

CALL lis_vector_is_null(obj%lis_ptr, ierr)
CALL CHKERR(ierr)
IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either BlockNodeFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

localNode = obj%domains(ivar)%ptr%GetLocalNodeNumber(globalNode)

IF (ANY(localNode .EQ. 0_I4B)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'Some globalNodes are out of bound')
END IF

tsize = SIZE(globalNode)
nn = SIZE(timeCompo)

IF (SIZE(VALUE) .NE. tsize * nn) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'The size of value not same as the size of globalNode '// &
    & ' times the size of timeCompo.')
END IF

kk = 0
DO jj = 1, nn
  indx = GetNodeLoc(&
    & obj=obj%dof, &
    & nodenum=localNode, &
    & ivar=ivar, &
    & spaceCompo=spaceCompo, &
    & timeCompo=timeCompo(jj))

  DO ii = 1, tsize
    kk = kk + 1
    CALL obj%SetSingle(&
      & indx=indx(ii), &
      & VALUE=VALUE(kk), &
      & scale=scale, &
      & addContribution=addContribution)
  END DO
END DO

END PROCEDURE obj_Set10

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set11
CHARACTER(*), PARAMETER :: myName = "obj_Set11"
INTEGER(I4B) :: localNode(SIZE(globalNode))
INTEGER(I4B) :: ierr
INTEGER(I4B) :: indx(SIZE(globalNode) * SIZE(timeCompo))
REAL(DFP) :: value0(SIZE(globalNode) * SIZE(timeCompo))

CALL lis_vector_is_null(obj%lis_ptr, ierr)
CALL CHKERR(ierr)
IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either BlockNodeFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

localNode = obj%domains(ivar)%ptr%GetLocalNodeNumber(globalNode)

IF (ANY(localNode .EQ. 0_I4B)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'Some globalNodes are out of bound')
END IF

value0 = VALUE

indx = GetNodeLoc(&
  & obj=obj%dof, &
  & nodenum=localNode, &
  & ivar=ivar, &
  & spaceCompo=spaceCompo, &
  & timeCompo=timeCompo)

CALL obj%SetMultiple(&
  & indx=indx, &
  & VALUE=value0, &
  & scale=scale, &
  & addContribution=addContribution)

END PROCEDURE obj_Set11

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set12
CHARACTER(*), PARAMETER :: myName = "obj_Set12"
INTEGER(I4B) :: localNode(SIZE(globalNode))
INTEGER(I4B) :: ierr
INTEGER(I4B) :: indx(SIZE(globalNode))
INTEGER(I4B) :: tsize
INTEGER(I4B) :: nn
INTEGER(I4B) :: ii
INTEGER(I4B) :: jj
INTEGER(I4B) :: kk

CALL lis_vector_is_null(obj%lis_ptr, ierr)
CALL CHKERR(ierr)
IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either BlockNodeFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

localNode = obj%domains(ivar)%ptr%GetLocalNodeNumber(globalNode)

IF (ANY(localNode .EQ. 0_I4B)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'Some globalNodes are out of bound')
END IF

tsize = SIZE(globalNode)
nn = SIZE(spaceCompo)

IF (SIZE(VALUE) .NE. tsize * nn) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'The size of value not same as the size of globalNode '// &
    & ' times the size of spaceCompo.')
END IF

kk = 0
DO jj = 1, nn
  indx = GetNodeLoc(&
    & obj=obj%dof, &
    & nodenum=localNode, &
    & ivar=ivar, &
    & spaceCompo=spaceCompo(jj), &
    & timeCompo=timeCompo)

  DO ii = 1, tsize
    kk = kk + 1
    CALL obj%SetSingle(&
      & indx=indx(ii), &
      & VALUE=VALUE(kk), &
      & scale=scale, &
      & addContribution=addContribution)
  END DO
END DO
END PROCEDURE obj_Set12

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set13
CHARACTER(*), PARAMETER :: myName = "obj_Set13"
INTEGER(I4B) :: localNode(SIZE(globalNode))
INTEGER(I4B) :: ierr
INTEGER(I4B) :: indx(SIZE(globalNode) * SIZE(spaceCompo))
REAL(DFP) :: value0(SIZE(globalNode) * SIZE(spaceCompo))

CALL lis_vector_is_null(obj%lis_ptr, ierr)
CALL CHKERR(ierr)
IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either BlockNodeFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

localNode = obj%domains(ivar)%ptr%GetLocalNodeNumber(globalNode)

IF (ANY(localNode .EQ. 0_I4B)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'Some globalNodes are out of bound')
END IF

value0 = VALUE

indx = GetNodeLoc(&
  & obj=obj%dof, &
  & nodenum=localNode, &
  & ivar=ivar, &
  & spaceCompo=spaceCompo, &
  & timeCompo=timeCompo)

CALL obj%SetMultiple(&
  & indx=indx, &
  & VALUE=value0, &
  & scale=scale, &
  & addContribution=addContribution)

END PROCEDURE obj_Set13

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set14
CHARACTER(*), PARAMETER :: myName = "obj_Set14"
INTEGER(I4B) :: localNode
INTEGER(I4B) :: ierr
INTEGER(I4B) :: indx

CALL lis_vector_is_null(obj%lis_ptr, ierr)
CALL CHKERR(ierr)
IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either BlockNodeFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

localNode = obj%domains(ivar)%ptr%GetLocalNodeNumber(globalNode)

IF ((localNode .EQ. 0_I4B)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'GlobalNode is out of bound')
END IF

indx = GetNodeLoc(&
  & obj=obj%dof, &
  & nodenum=localNode, &
  & ivar=ivar, &
  & spaceCompo=spaceCompo, &
  & timeCompo=timeCompo)

CALL obj%setSingle(&
  & indx=indx, &
  & VALUE=VALUE, &
  & scale=scale, &
  & addContribution=addContribution)

END PROCEDURE obj_Set14

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set15
CHARACTER(*), PARAMETER :: myName = "obj_Set15"
INTEGER(I4B) :: ierr
INTEGER(I4B) :: indx(SIZE(timeCompo))
REAL(DFP) :: value0(SIZE(timeCompo))
INTEGER(I4B) :: localNode

CALL lis_vector_is_null(obj%lis_ptr, ierr)
CALL CHKERR(ierr)
IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either BlockNodeFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

localNode = obj%domains(ivar)%ptr%GetLocalNodeNumber(globalNode)

IF ((localNode .EQ. 0_I4B)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'GlobalNode is out of bound')
END IF

value0 = VALUE
indx = GetNodeLoc(&
  & obj=obj%dof, &
  & nodenum=localNode,  &
  & ivar=ivar, &
  & spaceCompo=spaceCompo, &
  & timeCompo=timeCompo)

CALL obj%SetMultiple(&
  & VALUE=value0, &
  & indx=indx, &
  & scale=scale, &
  & addContribution=addContribution)

END PROCEDURE obj_Set15

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set16
CHARACTER(*), PARAMETER :: myName = "obj_Set16"
INTEGER(I4B) :: ierr
INTEGER(I4B) :: indx(SIZE(spaceCompo))
REAL(DFP) :: value0(SIZE(spaceCompo))
INTEGER(I4B) :: localNode

CALL lis_vector_is_null(obj%lis_ptr, ierr)
CALL CHKERR(ierr)
IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either BlockNodeFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

localNode = obj%domains(ivar)%ptr%GetLocalNodeNumber(globalNode)

IF ((localNode .EQ. 0_I4B)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'GlobalNode is out of bound')
END IF

value0 = VALUE
indx = GetNodeLoc(&
  & obj=obj%dof, &
  & nodenum=localNode,  &
  & ivar=ivar, &
  & spaceCompo=spaceCompo, &
  & timeCompo=timeCompo)

CALL obj%SetMultiple(&
  & VALUE=value0, &
  & indx=indx, &
  & scale=scale, &
  & addContribution=addContribution)

END PROCEDURE obj_Set16

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set17
CHARACTER(*), PARAMETER :: myName = "obj_Set17()"
INTEGER(I4B) :: ierr
INTEGER(I4B) :: tsize
INTEGER(I4B) :: tsize2
REAL(DFP), POINTER :: realvec(:)

CALL lis_vector_is_null(obj%lis_ptr, ierr)
CALL CHKERR(ierr)
IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either BlockNodeFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

IF (.NOT. VALUE%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'BlockNodeField_::value is not initiated')
END IF

tsize = obj%SIZE()
tsize2 = VALUE%SIZE()

IF (tsize .NE. tsize2) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'Size of obj not same as size of value')
END IF

SELECT TYPE (VALUE)
TYPE IS (BlockNodeField_)
  realvec => VALUE%getPointer()
  CALL obj%set(VALUE=realvec, scale=scale, addContribution=addContribution)
  NULLIFY (realvec)
TYPE IS (BlockNodeFieldLis_)
  CALL lis_vector_is_null(VALUE%lis_ptr, ierr)
  CALL CHKERR(ierr)
  IF (ierr .EQ. LIS_TRUE) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
    & " value%lis_ptr is not available")
  END IF
  IF (PRESENT(addContribution)) THEN
    CALL lis_vector_axpy(scale, VALUE%lis_ptr, obj%lis_ptr, ierr)
    CALL CHKERR(ierr)
  ELSE
    CALL lis_vector_copy(VALUE%lis_ptr, obj%lis_ptr, ierr)
    CALL CHKERR(ierr)
  END IF
CLASS DEFAULT
END SELECT

END PROCEDURE obj_Set17

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_assign
CALL set(obj%realVec, VALUE=VALUE)
END PROCEDURE obj_assign

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods
