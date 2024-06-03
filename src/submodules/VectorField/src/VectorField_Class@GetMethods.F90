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

SUBMODULE(VectorField_Class) GetMethods
USE AbstractField_Class, ONLY: TypeField

USE ScalarField_Class, ONLY: ScalarField_

USE STScalarField_Class, ONLY: STScalarField_

USE RealVector_Method, ONLY: GetValue_

USE ArangeUtility, ONLY: Arange

USE BaseType, ONLY: TypeFEVariableVector, TypeFEVariableSpace

USE FEVariable_Method, ONLY: NodalVariable

USE DOF_Method, ONLY: GetIDOF, &
                      OPERATOR(.tnodes.), &
                      GetNodeLoc

USE Display_Method, ONLY: ToString
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get1
CHARACTER(*), PARAMETER :: myName = "obj_Get1()"
LOGICAL(LGT) :: bool1, bool2

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated, myName, &
                  'VectorField_::obj is not initiated')
#endif

bool1 = PRESENT(globalNode)
bool2 = PRESENT(spaceCompo)

#ifdef DEBUG_VER

IF (bool1 .AND. bool2) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
       '[INTERNAL ERROR] :: Both globalNode and spaceCompo cannot be present')
  RETURN
END IF

#endif

IF (bool2) THEN
  CALL GetValue_(obj=obj%realVec, dofobj=obj%dof, ivar=1, idof=spaceCompo, &
                 VALUE=VALUE, tsize=tsize)
  RETURN
END IF

! IF (bool1) if globalNode present
! note that we overwriting bool1
CALL GetValue_(obj=obj%realVec, dofobj=obj%dof, &
               ivar=1_I4B, VALUE=VALUE, &
               nodenum=[globalnode], tsize=tsize)

END PROCEDURE obj_Get1

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get2()"

CALL AssertError1(obj%isInitiated, myName, &
                  'VectorField_::obj is not initiated')
#endif

CALL GetValue_(obj=obj%realVec, dofobj=obj%dof, idof=obj%idofs, &
               VALUE=VALUE, nrow=nrow, ncol=ncol, storageFMT=storageFMT)

END PROCEDURE obj_Get2

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get3
CHARACTER(*), PARAMETER :: myName = "obj_Get3()"

#include "./localNodeError.inc"

CALL GetValue_(obj=obj%realVec, dofobj=obj%dof, &
               idof=obj%idofs, VALUE=VALUE, nodenum=globalNode, &
               nrow=nrow, ncol=ncol, storageFMT=storageFMT)
END PROCEDURE obj_Get3

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get4
CHARACTER(*), PARAMETER :: myName = "obj_Get4()"

#include "./localNodeError.inc"

CALL GetValue_(obj=obj%realVec, dofobj=obj%dof, ivar=1, &
               idof=spaceCompo, VALUE=VALUE, tsize=tsize, nodenum=globalNode)

END PROCEDURE obj_Get4

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get5
CHARACTER(*), PARAMETER :: myName = "obj_Get5()"
INTEGER(I4B) :: indx

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated, myName, &
                  'VectorField_::obj is not initiated')

CALL AssertError1(spaceCompo .LE. obj%spaceCompo, myName, &
            'given spaceCompo should be less than or equal to obj%spaceCompo')

#endif

#include "./localNodeError.inc"

indx = GetNodeLoc(obj=obj%dof, nodenum=globalNode, idof=spaceCompo)
CALL obj%GetSingle(VALUE=VALUE, indx=indx)
END PROCEDURE obj_Get5

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get6
CALL obj%GetFEVariable(VALUE=VALUE, globalNode=globalNode, islocal=islocal)
END PROCEDURE obj_Get6

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get7
CALL obj%Get(idof=spaceCompo, ivar=1_I4B, VALUE=VALUE, &
             idof_value=spaceCompo, ivar_value=1_I4B)
END PROCEDURE obj_Get7

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get8
#ifdef DEBUG_VER

CHARACTER(*), PARAMETER :: myName = "obj_Set9()"

CALL AssertError1(obj%isInitiated, myName, &
                  'VectorField_::obj is not initiated')

CALL AssertError1(VALUE%isInitiated, myName, &
                  'VectorField_::value is not initiated')
#endif

CALL VALUE%Copy(obj)
END PROCEDURE obj_Get8

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get9
CHARACTER(*), PARAMETER :: myName = "obj_Get9()"
INTEGER(I4B) :: idofvalue

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated, myName, &
                  'VectorField_::obj is not initiated')

CALL AssertError1(VALUE%isInitiated, myName, &
                  'VectorField_::value is not initiated')

CALL AssertError2((obj%dof.tNodes. [ivar, idof]), &
                  (VALUE%dof.tNodes. [ivar_value, idof_value]), myName, &
                  'a=obj%dof.tNodes. [ivar, idof], b=VALUE%dof.tNodes. [ivar_value, idof_value]')

CALL AssertError1(idof .LE. obj%spaceCompo, myName, &
                  'idof is out of bound.')
#endif

SELECT TYPE (VALUE)
TYPE IS (ScalarField_)
  CALL GetValue_(obj=obj%realVec, dofobj=obj%dof, VALUE=VALUE%realVec, &
                 idof=idof)

  ! TYPE IS(ScalarFieldLis_)

TYPE IS (STScalarField_)

  idofvalue = idof_value
  CALL GetValue_(obj=obj%realVec, dofobj=obj%dof, idofobj=idof, &
                 VALUE=VALUE%realVec, dofvalue=VALUE%dof, idofvalue=idofvalue)

TYPE IS (VectorField_)

  idofvalue = idof_value
  CALL GetValue_(obj=obj%realVec, dofobj=obj%dof, idofobj=idof, &
                 VALUE=VALUE%realVec, dofvalue=VALUE%dof, idofvalue=idofvalue)

  ! TYPE IS(STScalarFieldLis_)

CLASS DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: No case found for type of value')
  RETURN

END SELECT

END PROCEDURE obj_Get9

!----------------------------------------------------------------------------
!                                                           GetFEVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFEVariable
REAL(DFP) :: v(obj%spaceCompo, SIZE(globalNode))
INTEGER(I4B) :: nrow, jj

!$OMP PARALLEL DO PRIVATE(jj)
DO jj = 1, SIZE(v, 2)
  CALL obj%Get(VALUE=v(:, jj), globalNode=globalNode(jj), tsize=nrow)
END DO
!$OMP END PARALLEL DO

VALUE = NodalVariable(v, TypeFEVariableVector, TypeFEVariableSpace)
END PROCEDURE obj_GetFEVariable

!----------------------------------------------------------------------------
!                                                                  GetPrefix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPrefix
ans = myprefix
END PROCEDURE obj_GetPrefix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetMethods
