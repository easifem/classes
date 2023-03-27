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
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_get1
CHARACTER(*), PARAMETER :: myName = "stsField_get1"
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
    CALL getValue( &
      & obj=obj%realvec, &
      & dofobj=obj%dof, &
      & idof=arange(1, obj%timeCompo), &
      & VALUE=VALUE, &
      & nodenum=[1])
  CASE (FIELD_TYPE_NORMAL)
    CALL getValue( &
      & obj=obj%realvec, &
      & dofobj=obj%dof, &
      & idof=arange(1, obj%timeCompo), &
      & VALUE=VALUE, &
      & nodenum=obj%domain%getLocalNodeNumber([globalnode]))
  END SELECT
END IF

IF (bool2) THEN
  CALL getValue( &
    & obj=obj%realvec, &
    & dofobj=obj%dof, &
    & ivar=1, &
    & idof=timeCompo, &
    & VALUE=VALUE)
END IF

END PROCEDURE stsField_get1

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_get2
CHARACTER(*), PARAMETER :: myName = "stsField_get2"

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'STScalarField_::obj is not initiated')
END IF

CALL getValue( &
  & obj=obj%realvec, &
  & dofobj=obj%dof, &
  & idof=arange(1, obj%timeCompo), &
  & VALUE=VALUE)
END PROCEDURE stsField_get2

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_get3
REAL(DFP), ALLOCATABLE :: v(:)
CALL getValue( &
  & obj=obj%realvec, &
  & dofobj=obj%dof, &
  & idof=arange(1, obj%timeCompo), &
  & VALUE=v, &
  & nodenum=obj%domain%getLocalNodeNumber(globalnode))
VALUE = RESHAPE(v, [obj%timeCompo, SIZE(globalnode)])
DEALLOCATE (v)
END PROCEDURE stsField_get3

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_get4
CALL getValue( &
  & obj=obj%realvec, &
  & dofobj=obj%dof, &
  & ivar=1, &
  & idof=timeCompo, &
  & VALUE=VALUE, &
  & nodenum=obj%domain%getLocalNodeNumber(globalnode))
END PROCEDURE stsField_get4

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_get5
CALL getValue( &
  & obj=obj%realvec, &
  & dofobj=obj%dof, &
  & ivar=1, &
  & idof=timeCompo, &
  & VALUE=VALUE, &
  & nodenum=obj%domain%getLocalNodeNumber(globalnode))
END PROCEDURE stsField_get5

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_get6
INTEGER(I4B) :: globalnode(INT(1 + (iend - istart) / stride)), ii, jj
jj = 0
DO ii = istart, iend, stride
  jj = jj + 1
  globalnode(jj) = ii
END DO
CALL obj%get(globalnode=globalnode, VALUE=VALUE)
END PROCEDURE stsField_get6

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_get7
INTEGER(I4B) :: globalnode(INT(1 + (iend - istart) / stride)), ii, jj
jj = 0
DO ii = istart, iend, stride
  jj = jj + 1
  globalnode(jj) = ii
END DO
CALL obj%get(globalnode=globalnode, VALUE=VALUE, timeCompo=timeCompo)
END PROCEDURE stsField_get7

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_get8
REAL(DFP), ALLOCATABLE :: v(:)
CALL getValue( &
  & obj=obj%realvec, &
  & dofobj=obj%dof, &
  & VALUE=v, &
  & idof=arange(1, obj%timeCompo), &
  & nodenum=obj%domain%getLocalNodeNumber(globalnode))
VALUE = NodalVariable( &
  & RESHAPE(v, [obj%timeCompo, SIZE(globalnode)]), &
  & TypeFEVariableScalar, &
  & TypeFEVariableSpaceTime)
DEALLOCATE (v)
END PROCEDURE stsField_get8

!----------------------------------------------------------------------------
!                                                                        get
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_get9
CHARACTER(*), PARAMETER :: myName = "stsField_get9"
INTEGER(I4B) :: n
n = (obj%dof.timecomponents.1)
IF (timecompo .GT. n) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'This routine is not callable as &
  & (obj%dof .timecomponents. 1)='//tostring(n)// &
  & ' is lesser than '// &
  & ' timecompo='//tostring(timecompo))
CALL GetValue( &
  & obj=obj%realvec, &
  & dofobj=obj%dof, &
  & VALUE=VALUE%realvec, &
  & idof=timecompo)
END PROCEDURE stsField_get9

!----------------------------------------------------------------------------
!                                                     getPointerOfComponent
!----------------------------------------------------------------------------

MODULE PROCEDURE stsField_getPointerOfComponent
CHARACTER(*), PARAMETER :: myName = "stsField_getPointerOfComponent"
IF (timeCompo .GT. obj%timeCompo) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'given timeCompo should be less than or equal to obj%timeCompo')
ans => getPointer(obj=obj%realvec, dofobj=obj%dof, idof=timeCompo)
END PROCEDURE stsField_getPointerOfComponent

END SUBMODULE GetMethods
