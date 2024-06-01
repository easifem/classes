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

SUBMODULE(ScalarField_Class) GetMethods
USE RealVector_Method, ONLY: GetValue_, Get, GetValue

USE ArangeUtility, ONLY: Arange

USE FEVariable_Method, ONLY: NodalVariable

USE BaseType, ONLY: TypeFEVariableScalar, &
                    TypeFEVariableSpace

USE DOF_Method, ONLY: GetNodeLoc, &
                      OPERATOR(.tNodes.)

USE AbstractField_Class, ONLY: TypeField

USE ReallocateUtility, ONLY: Reallocate

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get1
CHARACTER(*), PARAMETER :: myName = "obj_Get1()"

IF (obj%fieldType .EQ. TypeField%constant) THEN
  VALUE = Get(obj=obj%realVec, nodenum=1, dataType=1.0_DFP)
  RETURN
END IF

#include "./localNodeError.inc"

VALUE = Get(obj=obj%realVec, nodenum=globalNode, dataType=1.0_DFP)

END PROCEDURE obj_Get1

!----------------------------------------------------------------------------
!                                                                   Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get2

IF (obj%fieldType .EQ. TypeField%constant) THEN
  tsize = obj%tSize
  VALUE = Get(obj=obj%realVec, nodenum=1, dataType=1.0_DFP)
  RETURN
END IF

CALL GetValue_(obj=obj%realvec, dofobj=obj%dof, VALUE=VALUE, idof=1, &
               tsize=tsize)

END PROCEDURE obj_Get2

!----------------------------------------------------------------------------
!                                                                   Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get3
CHARACTER(*), PARAMETER :: myName = "obj_Get3()"

#include "./localNodeError.inc"

CALL GetValue_(obj=obj%realVec, nodenum=globalNode, VALUE=VALUE, &
               tsize=tsize, dofobj=obj%dof, ivar=1_I4B, idof=1_I4B)

END PROCEDURE obj_Get3

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get4
CHARACTER(*), PARAMETER :: myName = "obj_Get4()"

#include "./localNodeError.inc"

VALUE = NodalVariable( &
        Get( &
        obj=obj%realVec, &
        nodenum=globalNode, &
        dataType=1.0_DFP), &
        TypeFEVariableScalar, &
        TypeFEVariableSpace)

END PROCEDURE obj_Get4

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get5
CALL GetValue_(obj=obj%realVec, VALUE=VALUE%realVec)
END PROCEDURE obj_Get5

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get6
CHARACTER(*), PARAMETER :: myName = "obj_Get6()"
INTEGER(I4B) :: tsize
INTEGER(I4B) :: tsize_value
INTEGER(I4B) :: ii
INTEGER(I4B) :: indx1
INTEGER(I4B) :: indx2
REAL(DFP) :: avar

#ifdef DEBUG_VER

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    '[INTERNAL ERROR] :: ScalarField_::obj is not initiated')
  RETURN
END IF

IF (.NOT. VALUE%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
            '[INTERNAL ERROR] :: AbstractNodeField_ ::value is not initiated')
  RETURN
END IF

#endif

tsize = obj%dof.tNodes. [ivar, idof]

#ifdef DEBUG_VER

tsize_value = VALUE%dof.tNodes. [ivar_value, idof_value]

IF (tsize .NE. tsize_value) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: size mismatch between obj and VALUE')
  RETURN
END IF

#endif

DO ii = 1, tsize
  indx1 = GetNodeLoc(obj=obj%dof, nodenum=ii, ivar=ivar, idof=idof)

  CALL obj%GetSingle(VALUE=avar, indx=indx1)

  indx2 = GetNodeLoc(obj=VALUE%dof, nodenum=ii, ivar=ivar_value, &
                     idof=idof_value)

  CALL VALUE%SetSingle(VALUE=avar, indx=indx2)
END DO

END PROCEDURE obj_Get6

!----------------------------------------------------------------------------
!                                                              GetFeVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFEVariable
CALL obj%Get(VALUE=VALUE, globalNode=globalNode, islocal=islocal)
END PROCEDURE obj_GetFEVariable

!----------------------------------------------------------------------------
!                                                                 GetPrefix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPrefix
ans = myprefix
END PROCEDURE obj_GetPrefix

END SUBMODULE GetMethods
