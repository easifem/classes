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

MODULE PROCEDURE obj_Get1
CHARACTER(*), PARAMETER :: myName = "obj_Get1()"
INTEGER(I4B) :: localNode, indx, tNodes, ii, jj, kk, ierr
CHARACTER(3) :: mycase
LOGICAL(LGT) :: isnode, isspace, istime

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

#ifdef DEBUG_VER
CALL lis_vector_is_null(obj%lis_ptr, ierr)
IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & '[INTERNAL ERROR] :: Either VectorFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
  RETURN
END IF
#endif

isnode = PRESENT(globalNode)
isspace = PRESENT(spacecompo)
istime = PRESENT(timecompo)

mycase = "NNN"
IF (isnode) THEN
  localNode = obj%domain%GetLocalNodeNumber(globalnode)
  mycase(1:1) = "Y"
END IF
IF (isspace) mycase(2:2) = "Y"
IF (istime) mycase(3:3) = "Y"

SELECT CASE (mycase)

CASE ("YYY")
  ! node space time
  CALL Reallocate(VALUE, 1)
  indx = GetNodeLoc(&
    & obj=obj%dof, &
    & nodenum=localNode, &
    & ivar=1, &
    & timecompo=timecompo, &
    & spacecompo=spacecompo &
    & )
  CALL lis_vector_get_value(obj%lis_ptr, indx, VALUE(1), ierr)

#ifdef DEBUG_VER
  CALL CHKERR(ierr)
#endif

CASE ("YNN")
  ! node | no space | no time
  CALL Reallocate(VALUE, obj%spacecompo * obj%timecompo)

  kk = 0
  DO jj = 1, obj%timecompo
    DO ii = 1, obj%spacecompo
      kk = kk + 1
      indx = GetNodeLoc(&
        & obj=obj%dof, &
        & nodenum=localNode, &
        & ivar=1, &
        & timecompo=jj, &
        & spacecompo=ii &
        & )
      CALL lis_vector_get_value(obj%lis_ptr, indx, VALUE(kk), ierr)

#ifdef DEBUG_VER
      CALL CHKERR(ierr)
#endif

    END DO
  END DO

CASE ("YYN")
  ! node | space | no time
  CALL Reallocate(VALUE, obj%timecompo)

  DO ii = 1, obj%timecompo
    indx = GetNodeLoc(&
      & obj=obj%dof, &
      & nodenum=localNode, &
      & ivar=1, &
      & timecompo=ii, &
      & spacecompo=spacecompo &
      & )
    CALL lis_vector_get_value(obj%lis_ptr, indx, VALUE(ii), ierr)

#ifdef DEBUG_VER
    CALL CHKERR(ierr)
#endif

  END DO

CASE ("YNY")
  ! node | no space | time
  CALL Reallocate(VALUE, obj%spacecompo)

  DO ii = 1, obj%spacecompo
    indx = GetNodeLoc(&
      & obj=obj%dof, &
      & nodenum=localNode, &
      & ivar=1, &
      & timecompo=obj%timecompo, &
      & spacecompo=ii &
      & )
    CALL lis_vector_get_value(obj%lis_ptr, indx, VALUE(ii), ierr)

#ifdef DEBUG_VER
    CALL CHKERR(ierr)
#endif

  END DO

CASE ("NYY")
  tNodes = obj%domain%GetTotalNodes()
  CALL Reallocate(VALUE, tNodes)
  DO ii = 1, tNodes
    indx = GetNodeLoc(&
      & obj=obj%dof, &
      & nodenum=ii, &
      & ivar=1, &
      & timecompo=timecompo, &
      & spacecompo=spacecompo &
      & )
    CALL lis_vector_get_value(obj%lis_ptr, indx, VALUE(ii), ierr)

#ifdef DEBUG_VER
    CALL CHKERR(ierr)
#endif

  END DO

CASE ("NYN")

  tNodes = obj%domain%GetTotalNodes()
  CALL Reallocate(VALUE, tNodes * obj%timecompo)
  kk = 0
  DO ii = 1, tNodes
    DO jj = 1, obj%timecompo
      indx = GetNodeLoc(&
        & obj=obj%dof, &
        & nodenum=ii, &
        & ivar=1, &
        & timecompo=jj, &
        & spacecompo=spacecompo &
        & )
      kk = kk + 1
      CALL lis_vector_get_value(obj%lis_ptr, indx, VALUE(kk), ierr)

#ifdef DEBUG_VER
      CALL CHKERR(ierr)
#endif

    END DO
  END DO

CASE ("NNY")
  tNodes = obj%domain%GetTotalNodes()
  CALL Reallocate(VALUE, tNodes * obj%spacecompo)
  kk = 0
  DO ii = 1, tNodes
    DO jj = 1, obj%spacecompo
      indx = GetNodeLoc(&
        & obj=obj%dof, &
        & nodenum=ii, &
        & ivar=1, &
        & timecompo=timecompo, &
        & spacecompo=jj &
        & )
      kk = kk + 1
      CALL lis_vector_get_value(obj%lis_ptr, indx, VALUE(kk), ierr)

#ifdef DEBUG_VER
      CALL CHKERR(ierr)
#endif

    END DO
  END DO

CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: No case found.')
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_Get1

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get2
CHARACTER(*), PARAMETER :: myName = "obj_Get2"
INTEGER(I4B) :: tNodes
INTEGER(I4B) :: ierr

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either VectorFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

tNodes = obj%domain%GetTotalNodes()
CALL reallocate(VALUE, obj%spacecompo, obj%timecompo, tNodes)

CALL lis_vector_get_values( &
& obj%lis_ptr, &
& 1_I4B, &
& SIZE(VALUE, kind=I4B), &
& VALUE, ierr)
CALL CHKERR(ierr)

END PROCEDURE obj_Get2

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get3
CHARACTER(*), PARAMETER :: myName = "obj_Get3"
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

localNode = obj%domain%GetLocalNodeNumber(globalnode=globalnode)

IF (ANY(localNode .EQ. 0_I4B)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'Some globalnode are out of bound')
END IF

tsize = SIZE(globalnode)

CALL Reallocate(VALUE, obj%spacecompo, obj%timecompo, tsize)

DO ii = 1, tsize
  DO jj = 1, obj%timecompo
    DO kk = 1, obj%spacecompo
      indx = GetNodeLoc(&
        & obj=obj%dof, &
        & nodenum=localNode(ii), &
        & ivar=1, &
        & timecompo=jj, &
        & spacecompo=kk &
        & )
      CALL lis_vector_get_value( &
        & obj%lis_ptr, indx, &
        & VALUE(kk, jj, ii), ierr)
      CALL CHKERR(ierr)
    END DO
  END DO
END DO

END PROCEDURE obj_Get3

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get4
CHARACTER(*), PARAMETER :: myName = "obj_Get4"
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

IF (spacecompo .GT. obj%spacecompo) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'spacecompo is greater than obj%spacecompo')
END IF

IF (timecompo .GT. obj%timecompo) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'timecompo is greater than obj%timecompo')
END IF

localNode = obj%domain%GetLocalNodeNumber(globalnode=globalnode)

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
    & timecompo=timecompo, &
    & spacecompo=spacecompo &
    & )
  CALL lis_vector_get_value( &
    & obj%lis_ptr, indx, &
    & VALUE(ii), ierr)
  CALL CHKERR(ierr)
END DO

END PROCEDURE obj_Get4

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get5
CHARACTER(*), PARAMETER :: myName = "obj_Get5()"
INTEGER(I4B) :: ierr
INTEGER(I4B) :: localNode
INTEGER(I4B) :: indx

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

#ifdef DEBUG_VER
CALL lis_vector_is_null(obj%lis_ptr, ierr)
IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either VectorFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

IF (spacecompo .GT. obj%spacecompo) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'spacecompo is greater than obj%spacecompo')
END IF

IF (timecompo .GT. obj%timecompo) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'timecompo is greater than obj%timecompo')
END IF
#endif

localNode = obj%domain%GetLocalNodeNumber(globalnode=globalnode)

#ifdef DEBUG_VER
IF ((localNode .EQ. 0_I4B)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'Globalnode is out of bound')
END IF
#endif

indx = GetNodeLoc(&
  & obj=obj%dof, &
  & nodenum=localNode, &
  & ivar=1, &
  & timecompo=timecompo, &
  & spacecompo=spacecompo &
  & )
CALL lis_vector_get_value(obj%lis_ptr, indx, VALUE, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_Get5

!----------------------------------------------------------------------------
!                                                                        get
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
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get7
INTEGER(I4B) :: globalNode(INT(1 + (iend - istart) / stride)), ii, jj
jj = 0
DO ii = istart, iend, stride
  jj = jj + 1
  globalNode(jj) = ii
END DO
CALL obj%Get( &
  & globalNode=globalNode, &
  & VALUE=VALUE, &
  & spacecompo=spacecompo, &
  & timecompo=timecompo)
END PROCEDURE obj_Get7

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get8
CHARACTER(*), PARAMETER :: myName = "obj_Get8()"
INTEGER(I4B) :: ierr, localNode, jj, kk, indx

#ifdef DEBUG_VER
CALL lis_vector_is_null(obj%lis_ptr, ierr)
IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Either VectorFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF
#endif

localNode = obj%domain%GetLocalNodeNumber(globalnode=globalnode)

#ifdef DEBUG_VER
IF ((localNode .EQ. 0_I4B)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'Globalnode is out of bound')
END IF
#endif

CALL Reallocate(VALUE, obj%spacecompo, obj%timecompo)

DO jj = 1, obj%timecompo
  DO kk = 1, obj%spacecompo
    indx = GetNodeLoc(&
      & obj=obj%dof, &
      & nodenum=localNode, &
      & ivar=1, &
      & timecompo=jj, &
      & spacecompo=kk &
      & )
    CALL lis_vector_get_value(obj%lis_ptr, indx, VALUE(kk, jj), ierr)

#ifdef DEBUG_VER
    CALL CHKERR(ierr)
#endif

  END DO
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_Get8

!----------------------------------------------------------------------------
!                                                                      get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get9
REAL(DFP), ALLOCATABLE :: m3a(:, :, :), m3b(:, :, :)
CALL obj%Get(VALUE=m3b, globalNode=globalNode)

! Here m3b is in (i, a, J) format,
! so we have to swap the dimensions to (i,J,a)
! We will call swap method from Utility.
CALL SWAP(a=m3a, b=m3b, i1=1, i2=3, i3=2)
VALUE = NodalVariable(m3a, TypeFEVariableVector, &
  & TypeFEVariableSpacetime)

DEALLOCATE (m3a, m3b)
END PROCEDURE obj_Get9

!----------------------------------------------------------------------------
!                                                                 Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get10
CHARACTER(*), PARAMETER :: myName = "obj_Get10()"
INTEGER(I4B) :: case_id, ierr, tNodes, ii, jj, kk, globalnode
REAL(DFP) :: avar

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

#ifdef DEBUG_VER
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
#endif

tNodes = obj%domain%GetTotalNodes()

#ifdef DEBUG_VER
IF (tNodes .NE. VALUE%domain%GetTotalNodes()) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'tNodes of STVectorFieldLis_::obj and'// &
    & ' tNodes of AbstractNodeField_::value are not same')
END IF
#endif

IF (PRESENT(spacecompo) .AND. PRESENT(timecompo)) THEN
  case_id = 1

#ifdef DEBUG_VER
  IF (spacecompo .GT. obj%spacecompo) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'spacecompo is greater than obj%spacecompo')
  END IF
  IF (timecompo .GT. obj%timecompo) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'timecompo is greater than obj%timecompo')
  END IF
#endif

ELSEIF (PRESENT(spacecompo) .AND. .NOT. PRESENT(timecompo)) THEN
  case_id = 2

#ifdef DEBUG_VER
  IF (spacecompo .GT. obj%spacecompo) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'spacecompo is greater than obj%spacecompo')
  END IF
#endif

ELSEIF (.NOT. PRESENT(spacecompo) .AND. PRESENT(timecompo)) THEN
  case_id = 3

#ifdef DEBUG_VER
  IF (timecompo .GT. obj%timecompo) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'timecompo is greater than obj%timecompo')
  END IF
#endif

ELSEIF (.NOT. PRESENT(spacecompo) .AND. .NOT. PRESENT(timecompo)) THEN
  case_id = 4
END IF

SELECT CASE (case_id)
! spacecompo and timecompo are present
CASE (1)
  SELECT TYPE (VALUE)
  CLASS IS (ScalarField_)
    DO ii = 1, tNodes
      globalnode = obj%domain%GetGlobalNodeNumber(localNode=ii)
      CALL obj%Get(VALUE=avar, globalnode=globalnode, &
        & spacecompo=spacecompo, timecompo=timecompo)
      CALL VALUE%set(VALUE=avar, globalnode=globalnode)
    END DO

  CLASS IS (STScalarField_)
    DO ii = 1, tNodes
      globalnode = obj%domain%GetGlobalNodeNumber(localNode=ii)
      CALL obj%Get(VALUE=avar, globalnode=globalnode, &
        & spacecompo=spacecompo, timecompo=timecompo)
      CALL VALUE%set(VALUE=avar, globalnode=globalnode, timecompo=timecompo)
    END DO

  CLASS IS (VectorField_)
    DO ii = 1, tNodes
      globalnode = obj%domain%GetGlobalNodeNumber(localNode=ii)
      CALL obj%Get(VALUE=avar, globalnode=globalnode, &
        & spacecompo=spacecompo, timecompo=timecompo)
      CALL VALUE%set(VALUE=avar, globalnode=globalnode, spacecompo=spacecompo)
    END DO

  CLASS IS (STVectorField_)
    DO ii = 1, tNodes
      globalnode = obj%domain%GetGlobalNodeNumber(localNode=ii)
      CALL obj%Get(VALUE=avar, globalnode=globalnode, &
        & spacecompo=spacecompo, timecompo=timecompo)
      CALL VALUE%set(VALUE=avar, globalnode=globalnode, &
        & spacecompo=spacecompo, timecompo=timecompo)
    END DO

  CLASS DEFAULT
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'No case found for type of value; case_id=1')
  END SELECT

! spacecompo is present
CASE (2)
  SELECT TYPE (VALUE)

  CLASS IS (STScalarField_)
    DO ii = 1, tNodes
      globalnode = obj%domain%GetGlobalNodeNumber(localNode=ii)
      DO jj = 1, obj%timecompo
        CALL obj%Get(VALUE=avar, globalnode=globalnode, &
          & spacecompo=spacecompo, timecompo=jj)
        CALL VALUE%set(VALUE=avar, globalnode=globalnode, timecompo=jj)
      END DO
    END DO

  CLASS IS (STVectorField_)
    DO ii = 1, tNodes
      globalnode = obj%domain%GetGlobalNodeNumber(localNode=ii)
      DO jj = 1, obj%timecompo
        CALL obj%Get(VALUE=avar, globalnode=globalnode, &
          & spacecompo=spacecompo, timecompo=jj)
        CALL VALUE%set(VALUE=avar, globalnode=globalnode, &
          & spacecompo=spacecompo, timecompo=jj)
      END DO
    END DO

  CLASS DEFAULT
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'No case found for type of value; case_id=2')

  END SELECT

! timecompo is present
CASE (3)
  SELECT TYPE (VALUE)

  CLASS IS (VectorField_)
    DO ii = 1, tNodes
      globalnode = obj%domain%GetGlobalNodeNumber(localNode=ii)
      DO jj = 1, obj%spacecompo
        CALL obj%Get(VALUE=avar, globalnode=globalnode, &
          & spacecompo=jj, timecompo=timecompo)
        CALL VALUE%set(VALUE=avar, globalnode=globalnode, spacecompo=jj)
      END DO
    END DO

  CLASS IS (STVectorField_)
    DO ii = 1, tNodes
      globalnode = obj%domain%GetGlobalNodeNumber(localNode=ii)
      DO jj = 1, obj%spacecompo
        CALL obj%Get(VALUE=avar, globalnode=globalnode, &
          & spacecompo=jj, timecompo=timecompo)
        CALL VALUE%set(VALUE=avar, globalnode=globalnode, &
          & spacecompo=jj, timecompo=timecompo)
      END DO
    END DO

  CLASS DEFAULT
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'No case found for type of value; case_id=3')

  END SELECT

! spacecompo and timecompo are not present
CASE (4)
  SELECT TYPE (VALUE)

  CLASS IS (STVectorField_)
    DO ii = 1, tNodes
      globalnode = obj%domain%GetGlobalNodeNumber(localNode=ii)
      DO jj = 1, obj%timecompo
        DO kk = 1, obj%spacecompo
          CALL obj%Get(VALUE=avar, globalnode=globalnode, &
            & spacecompo=kk, timecompo=jj)
          CALL VALUE%set(VALUE=avar, globalnode=globalnode, &
            & spacecompo=kk, timecompo=jj)
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

END PROCEDURE obj_Get10

!----------------------------------------------------------------------------
!                                                     getPointerOfComponent
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPointer
CHARACTER(*), PARAMETER :: myName = "obj_GetPointer"
CALL e%raiseError(modName//'::'//myName//' - '// &
  & '[Internal ERROR] :: This method is not available for STVectorFieldLis_')
END PROCEDURE obj_GetPointer

!----------------------------------------------------------------------------
!                                                     getPointerOfComponent
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPointerOfComponent
CHARACTER(*), PARAMETER :: myName = "obj_GetPointerOfComponent"
CALL e%raiseError(modName//'::'//myName//' - '// &
  & '[INTERNAL ERROR] :: This method is not available for STVectorFieldLis_')
END PROCEDURE obj_GetPointerOfComponent

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetMethods
