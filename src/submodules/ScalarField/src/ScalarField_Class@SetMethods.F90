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

SUBMODULE(ScalarField_Class) SetMethods
USE GlobalData, ONLY: Constant, Space, Scalar
USE InputUtility, ONLY: Input

USE AbstractField_Class, ONLY: TypeField

USE RealVector_Method, ONLY: Set, Add

USE Display_Method, ONLY: ToString

USE ArangeUtility, ONLY: Arange

USE DOF_Method, ONLY: GetNodeLoc, &
                      OPERATOR(.tNodes.), &
                      GetIDOF

USE BaseType, ONLY: TypeFEVariableScalar, &
                    TypeFEVariableConstant, &
                    TypeFEVariableSpace

USE FEVariable_Method, ONLY: GET

USE AbstractMesh_Class, ONLY: AbstractMesh_

USE ReallocateUtility, ONLY: Reallocate

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set1
CHARACTER(*), PARAMETER :: myName = "obj_Set1()"
INTEGER(I4B) :: indx

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated, myName, "ScalarField_::obj not initiated")
#endif

#include "./localNodeError.inc"

indx = GetNodeLoc(obj=obj%dof, nodenum=globalNode, idof=1_I4B)
CALL obj%SetSingle(indx=indx, VALUE=VALUE, scale=scale, &
                   addContribution=addContribution)

END PROCEDURE obj_Set1

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set2
CALL obj%SetAll(VALUE=VALUE, scale=scale, addContribution=addContribution)
END PROCEDURE obj_Set2

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set3()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: s(3)

#ifdef DEBUG_VER

CALL AssertError1(obj%isInitiated, myName, "ScalarField_::obj not initiated")

isok = obj%fieldType .NE. TypeField%constant
CALL AssertError1(isok, myName, "Not callable for Constant field")

isok = SIZE(VALUE) .GE. (obj%dof.tNodes.1_I4B)
CALL AssertError1(isok, myName, "Size of value is not enought")

#endif

s = GetNodeLoc(obj=obj%dof, idof=1_I4B)

CALL obj%SetMultiple(value=value, scale=scale, addContribution=addContribution, &
                     istart=s(1), iend=s(2), stride=s(3))

END PROCEDURE obj_Set3

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set4
#ifdef DEBUG_VER
LOGICAL(LGT) :: isok
#endif

CHARACTER(*), PARAMETER :: myName = "obj_Set4()"
REAL(DFP) :: value0(SIZE(globalNode))

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated, myName, "ScalarField_::obj not initiated")
isok = obj%fieldType .NE. TypeField%constant
CALL AssertError1(isok, myName, "Not callable for Constant field")
#endif

#include "./localNodeError.inc"

value0 = VALUE

CALL obj%SetMultiple(indx=globalNode, VALUE=value0, scale=scale, &
                     addContribution=addContribution)

END PROCEDURE obj_Set4

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set5
#ifdef DEBUG_VER
LOGICAL(LGT) :: isok
#endif

CHARACTER(*), PARAMETER :: myName = "obj_Set5()"

#ifdef DEBUG_VER

CALL AssertError1(obj%isInitiated, myName, "ScalarField_::obj not initiated")

isok = obj%fieldType .NE. TypeField%constant
CALL AssertError1(isok, myName, "Not callable for Constant field")

isok = SIZE(VALUE) .GE. SIZE(globalNode)
CALL AssertError1(isok, myName, "Size of value is not enought")

#endif

#include "./localNodeError.inc"

CALL obj%SetMultiple(indx=globalNode, VALUE=VALUE, scale=scale, &
                     addContribution=addContribution)

END PROCEDURE obj_Set5

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set6
CALL obj%Set(ivar=1_I4B, idof=1_I4B, VALUE=VALUE, ivar_value=1_I4B, &
             idof_value=1_I4B)
END PROCEDURE obj_Set6

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set7
CHARACTER(*), PARAMETER :: myName = "obj_Set7()"

SELECT CASE (VALUE%vartype)

CASE (Constant)

CALL obj%Set(VALUE=GET(VALUE, TypeFEVariableScalar, TypeFEVariableConstant), &
        globalNode=globalNode, scale=scale, addContribution=addContribution, &
               islocal=islocal)

CASE (Space)

  CALL obj%Set(VALUE=GET(VALUE, TypeFEVariableScalar, TypeFEVariableSpace), &
        globalNode=globalNode, scale=scale, addContribution=addContribution, &
               islocal=islocal)

CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: No case found')
  RETURN
END SELECT

END PROCEDURE obj_Set7

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set8
CALL obj%Set(ivar=1_I4B, idof=1_I4B, VALUE=obj2, ivar_value=1_I4B, &
             idof_value=1_I4B, scale=scale, addContribution=addContribution)
END PROCEDURE obj_Set8

!----------------------------------------------------------------------------
!                                                                      Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set9
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set9()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: idof1, idof2, s(3), p(3)
LOGICAL(LGT) :: abool
REAL(DFP) :: areal

#ifdef DEBUG_VER

CALL AssertError1(obj%isInitiated, myName, "ScalarField_::obj not initiated")
CALL AssertError1(value%isInitiated, myName, "AbstractNodeField_::value not initiated")
CALL AssertError2(obj%dof.tNodes. [ivar, idof], &
                  VALUE%dof.tNodes. [ivar_value, idof_value], myName, &
 "a=obj%dof.tNodes.[ivar, idof], b=value%dof.tNodes.[ivar_value, idof_value]")

#endif

abool = Input(option=addContribution, default=.FALSE.)

SELECT TYPE (VALUE)

TYPE IS (ScalarField_)

  s = GetNodeLoc(obj=obj%dof, idof=1_I4B)
  p = GetNodeLoc(obj=VALUE%dof, idof=idof2)

! TYPE is (STScalarField_)
!
! TYPE is (VectorField_)
!
! TYPE is (STVectorField_)
!
! TYPE is (ScalarFieldLis_)
!
! TYPE is (VectorFieldLis_)
!
! TYPE is (STScalarFieldLis_)
!
! TYPE is (STVectorFieldLis_)

CLASS DEFAULT

  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: No case found')
  RETURN

END SELECT

END PROCEDURE obj_Set9

!----------------------------------------------------------------------------
!                                                             SetByFunction
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetByFunction
CHARACTER(*), PARAMETER :: myName = "obj_SetByFunction()"
LOGICAL(LGT) :: istimes, problem
INTEGER(I4B) :: ttime, returnType, nsd, tnodes, ii
REAL(DFP), ALLOCATABLE :: xij(:, :)
REAL(DFP) :: args(4), VALUE
INTEGER(I4B), PARAMETER :: needed_returnType = Scalar
CLASS(AbstractMesh_), POINTER :: meshptr
CHARACTER(:), ALLOCATABLE :: baseInterpolation

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

baseInterpolation = obj%fedof%GetBaseInterpolation()

IF (baseInterpolation(1:3) .NE. "Lag") THEN

  baseInterpolation = ""
  CALL e%RaiseError(modName//'::'//myName//' - '// &
 '[INTERNAL ERROR] :: This routine is only valid for Lagrange interpolation.')
  RETURN

END IF

istimes = PRESENT(times)
problem = .FALSE.

args = 0.0_DFP
IF (istimes) THEN

  ttime = SIZE(times)
  args(4) = times(1)
  problem = ttime .NE. 1_I4B

END IF

IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: times size should be 1.')
  RETURN
END IF

returnType = func%GetReturnType()
problem = returnType .NE. needed_returnType

IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                '[INTERNAL ERROR] :: Return type of function is not correct.')
  RETURN
END IF

meshptr => NULL()
meshptr => obj%fedof%GetMeshPointer()
problem = .NOT. ASSOCIATED(meshptr)
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] ::  mesh in fedof is not ASSOCIATED.')
  RETURN
END IF

nsd = meshptr%GetNSD()
tnodes = meshptr%GetTotalNodes()
CALL Reallocate(xij, nsd, 1)

DO ii = 1, tnodes
  CALL meshptr%GetNodeCoord(globalNode=[ii], nodeCoord=xij, islocal=.TRUE.)
  args(1:nsd) = xij(1:nsd, 1)
  CALL func%Get(val=VALUE, args=args)
  CALL obj%Set(globalNode=ii, VALUE=VALUE, islocal=.TRUE.)
END DO

IF (ALLOCATED(xij)) DEALLOCATE (xij)
baseInterpolation = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetByFunction

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SetMethods
