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
REAL(DFP) :: areal
LOGICAL(LGT) :: abool

#ifdef DEBUG_VER
IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                   '[INTERNAL ERROR] :: Scalar field object is not initiated')
  RETURN
END IF

#endif

#include "./localNodeError.inc"

abool = Input(option=addContribution, default=.FALSE.)

IF (obj%fieldType .EQ. TypeField%constant) THEN

  IF (abool) THEN
    areal = Input(option=scale, default=1.0_DFP)
    CALL Add(obj%realVec, nodenum=[1_I4B], VALUE=[VALUE], scale=areal)
    RETURN
  END IF

  CALL Set(obj%realVec, nodenum=[1_I4B], VALUE=[VALUE])
  RETURN

END IF

IF (abool) THEN
  areal = Input(option=scale, default=1.0_DFP)
  CALL Add(obj%realVec, nodenum=[globalNode], VALUE=[VALUE], scale=areal)
  RETURN
END IF

CALL Set(obj%realVec, nodenum=[globalNode], VALUE=[VALUE])

END PROCEDURE obj_Set1

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set2
REAL(DFP) :: areal
LOGICAL(LGT) :: abool

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set2()"
IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                   '[INTERNAL ERROR] :: Scalar field object is not initiated')
  RETURN
END IF
#endif

abool = Input(option=addContribution, default=.FALSE.)

IF (abool) THEN
  areal = Input(option=scale, default=1.0_DFP)
  CALL Add(obj%realVec, VALUE=VALUE, scale=areal)
ELSE
  CALL Set(obj%realVec, VALUE=VALUE)
END IF

END PROCEDURE obj_Set2

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set3
REAL(DFP) :: areal
LOGICAL(LGT) :: abool

#ifdef DEBUG_VER

CHARACTER(*), PARAMETER :: myName = "obj_Set3()"

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    'Scalar field object is not initiated')
  RETURN
END IF

IF (obj%fieldType .EQ. TypeField%constant) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    '[INTERNAL ERROR] :: This routine should not be '// &
                    'called for constant field type.')
  RETURN
END IF

IF (obj%tSize .NE. SIZE(VALUE)) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
              '[INTERNAL ERROR] :: Size of value ('//ToString(SIZE(VALUE))// &
                    ') is not equal to size of scalarfield ('// &
                    ToString(obj%tSize)//')')
  RETURN
END IF

#endif

abool = Input(option=addContribution, default=.FALSE.)

IF (abool) THEN
  areal = Input(option=scale, default=1.0_DFP)
  CALL Add(obj%realVec, VALUE=VALUE, scale=areal)

ELSE
  CALL Set(obj%realVec, VALUE=VALUE)

END IF

END PROCEDURE obj_Set3

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set4
CHARACTER(*), PARAMETER :: myName = "obj_Set4()"
REAL(DFP) :: areal
LOGICAL(LGT) :: abool

#ifdef DEBUG_VER

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    'Scalar field object is not initiated')
  RETURN
END IF

IF (obj%fieldType .EQ. TypeField%constant) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                 'This routine should not be called for constant field type.')
  RETURN
END IF

#endif

#include "./localNodeError.inc"

abool = Input(option=addContribution, default=.FALSE.)

IF (abool) THEN
  areal = Input(option=scale, default=1.0_DFP)
  CALL Add(obj%realVec, nodenum=globalNode, VALUE=VALUE, scale=areal)
  RETURN
END IF

CALL Set(obj%realVec, nodenum=globalNode, VALUE=VALUE)

END PROCEDURE obj_Set4

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set5
CHARACTER(*), PARAMETER :: myName = "obj_Set5()"
REAL(DFP) :: areal
LOGICAL(LGT) :: abool

#ifdef DEBUG_VER

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                   '[INTERNAL ERROR] :: Scalar field object is not initiated')
  RETURN
END IF

IF (obj%fieldType .EQ. TypeField%constant) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    '[INTERNAL ERROR] :: Not valid for constant field type.')
  RETURN
END IF

#endif

#include "./localNodeError.inc"

abool = Input(option=addContribution, default=.FALSE.)

IF (abool) THEN
  areal = Input(option=scale, default=1.0_DFP)
  CALL Add(obj=obj%realVec, nodenum=globalNode, VALUE=VALUE, scale=areal)
  RETURN
END IF

CALL Set(obj%realVec, nodenum=globalNode, VALUE=VALUE)

END PROCEDURE obj_Set5

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set6
CALL Set(obj%realVec, VALUE=VALUE%realVec)
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
CALL Add(obj=obj%realVec, VALUE=obj2%realVec, scale=scale)
END PROCEDURE obj_Set8

!----------------------------------------------------------------------------
!                                                                      Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set9
INTEGER(I4B) :: idof1, idof2
LOGICAL(LGT) :: abool
REAL(DFP) :: areal

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set9()"
INTEGER(I4B) :: tsize, tsize_value, ivar_idof(2)

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                 '[INTERNAL ERROR] :: ScalarNodeField_::obj is not initiated')
  RETURN
END IF

IF (.NOT. VALUE%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
            '[INTERNAL ERROR] :: AbstractNodeField_ ::value is not initiated')
  RETURN
END IF

ivar_idof(1:2) = [ivar, idof]

tsize = obj%dof.tNodes.ivar_idof

ivar_idof(1:2) = [ivar_value, idof_value]
tsize_value = VALUE%dof.tNodes.ivar_idof

IF (tsize .NE. tsize_value) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                   '[INTERNAL ERROR] :: size mismatch between obj and value.')
  RETURN
END IF

#endif

abool = Input(option=addContribution, default=.FALSE.)

idof1 = GetIDOF(obj=obj%dof, ivar=ivar, idof=idof)
idof2 = GetIDOF(obj=VALUE%dof, ivar=ivar_value, idof=idof_value)

IF (abool) THEN
  areal = Input(option=scale, default=1.0_DFP)
  CALL Add(obj1=obj%realVec, dofobj1=obj%dof, idof1=idof1, &
           obj2=VALUE%realVec, dofobj2=VALUE%dof, idof2=idof2, scale=areal)
  RETURN
END IF

CALL Set(obj1=obj%realVec, dofobj1=obj%dof, idof1=idof1, &
         obj2=VALUE%realVec, dofobj2=VALUE%dof, idof2=idof2)

#ifdef DEBUG_VER

CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

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

END SUBMODULE SetMethods
