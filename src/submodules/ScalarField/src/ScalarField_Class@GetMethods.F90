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

USE ScalarField_Class, ONLY: ScalarField_

USE STScalarField_Class, ONLY: STScalarField_

USE VectorField_Class, ONLY: VectorField_

! USE STVectorField_Class, only: STVectorField_

USE ScalarFieldLis_Class, ONLY: ScalarFieldLis_

USE STScalarFieldLis_Class, ONLY: STScalarFieldLis_

USE VectorFieldLis_Class, ONLY: VectorFieldLis_

! USE STVectorFieldLis_Class, only: STVectorFieldLis_

USE ArangeUtility, ONLY: Arange

USE FEVariable_Method, ONLY: NodalVariable

USE BaseType, ONLY: TypeFEVariableScalar, &
                    TypeFEVariableSpace

USE DOF_Method, ONLY: GetNodeLoc, &
                      OPERATOR(.tNodes.), &
                      GetIDOF

USE FieldOpt_Class, ONLY: TypeField => TypeFieldOpt

USE ReallocateUtility, ONLY: Reallocate

USE Display_Method, ONLY: Tostring

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get1
CHARACTER(*), PARAMETER :: myName = "obj_Get1()"
#include "./localNodeError.inc"
CALL obj%GetSingle(VALUE=VALUE, indx=globalNode)
END PROCEDURE obj_Get1

!----------------------------------------------------------------------------
!                                                                   Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get2
INTEGER(I4B) :: s(3)
s = GetNodeLoc(obj=obj%dof, idof=1)
CALL obj%GetMultiple(VALUE=VALUE, istart=s(1), iend=s(2), stride=s(3), &
                     tsize=tsize)
END PROCEDURE obj_Get2

!----------------------------------------------------------------------------
!                                                                   Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get3
CHARACTER(*), PARAMETER :: myName = "obj_Get3()"
#include "./localNodeError.inc"
CALL obj%GetMultiple(VALUE=VALUE, indx=globalNode, tsize=tsize)
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
CALL obj%Get(ivar=1, idof=1, VALUE=VALUE, ivar_value=1, idof_value=1)
END PROCEDURE obj_Get5

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get6
CHARACTER(*), PARAMETER :: myName = "obj_Get6()"
INTEGER(I4B) :: tsize
INTEGER(I4B) :: s(3), p(3)
REAL(DFP), POINTER :: realvec(:)

#ifdef DEBUG_VER

CALL AssertError1(obj%isInitiated, myName, &
                  'ScalarFieldLis_::obj is not initiated')

CALL AssertError1(VALUE%isInitiated, myName, &
                  'AbstractNodeField::value is not initiated')
#endif

SELECT TYPE (VALUE)

TYPE IS (ScalarField_)

  s = GetNodeLoc(obj=obj%dof, idof=1)
  realvec => VALUE%GetPointer()
  CALL obj%GetMultiple(VALUE=realvec, istart=s(1), iend=s(2), stride=s(3), &
                       tsize=tsize)
  realvec => NULL()

TYPE IS (STScalarField_)

  s = GetNodeLoc(obj=obj%dof, idof=1)
  p = GetNodeLoc(obj=VALUE%dof, idof=GetIDOF(obj=VALUE%dof, ivar=1_I4B, &
                                             idof=idof_value))
  tsize = obj%dof.tNodes.1

  realvec => VALUE%GetPointer()
  CALL obj%GetMultiple(VALUE=realvec, istart=s(1), iend=s(2), stride=s(3), &
                       tsize=tsize, istart_value=p(1), iend_value=p(2), &
                       stride_value=p(3))
  realvec => NULL()

TYPE IS (VectorField_)

  s = GetNodeLoc(obj=obj%dof, idof=1)
  p = GetNodeLoc(obj=VALUE%dof, idof=GetIDOF(obj=VALUE%dof, ivar=1_I4B, &
                                             idof=idof_value))
  tsize = obj%dof.tNodes.1

  realvec => VALUE%GetPointer()
  CALL obj%GetMultiple(VALUE=realvec, istart=s(1), iend=s(2), stride=s(3), &
                       tsize=tsize, istart_value=p(1), iend_value=p(2), &
                       stride_value=p(3))
  realvec => NULL()

! TYPE IS (STVectorField_)
!
!   s = GetNodeLoc(obj=obj%dof, idof=1)
!   p = GetNodeLoc(obj=VALUE%dof, idof=GetIDOF(obj=VALUE%dof, ivar=1_I4B, &
!                                              idof=idof_value))
!   tsize = obj%dof.tNodes.1
!
!   realvec => VALUE%GetPointer()
!   CALL obj%GetMultiple(VALUE=realvec, istart=s(1), iend=s(2), stride=s(3), &
!                        tsize=tsize, istart_value=p(1), iend_value=p(2), &
!                        stride_value=p(3))
!   realvec => NULL()

TYPE IS (ScalarFieldLis_)

  CALL VALUE%Set(ivar=1_I4B, idof=1_I4B, VALUE=obj, idof_value=1_I4B, &
                 ivar_value=1_I4B)

TYPE IS (STScalarFieldLis_)

  CALL VALUE%Set(ivar=1_I4B, idof=idof_value, VALUE=obj, idof_value=1_I4B, &
                 ivar_value=1_I4B)

TYPE IS (VectorFieldLis_)

  CALL VALUE%Set(ivar=1_I4B, idof=idof_value, VALUE=obj, idof_value=1_I4B, &
                 ivar_value=1_I4B)

! TYPE IS (STVectorFieldLis_)
!
!   CALL VALUE%Set(ivar=1_I4B, idof=idof_value, VALUE=obj, idof_value=1_I4B, &
!                  ivar_value=1_I4B)

CLASS DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: No case found for type value')
  RETURN
END SELECT

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

!----------------------------------------------------------------------------
!                                                                 GetPrefix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Size
ans = obj%local_n
END PROCEDURE obj_Size

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetMethods
