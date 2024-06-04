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

SUBMODULE(VectorField_Class) SetMethods
USE InputUtility, ONLY: Input

USE AbstractMesh_Class, ONLY: AbstractMesh_

USE AbstractField_Class, ONLY: TypeField

USE ScalarField_Class, ONLY: ScalarField_

! USE STVectorField_Class, ONLY: STVectorField_

USE RealVector_Method, ONLY: Set, Add, GetPointer

USE Display_Method, ONLY: tostring

USE GlobalData, ONLY: NONE, Scalar, Constant, Space, Vector

USE DOF_Method, ONLY: GetNodeLoc, &
                      OPERATOR(.tNodes.), &
                      GetIDOF

USE ArangeUtility, ONLY: Arange

USE BaseType, ONLY: TypeFEVariableVector, &
                    TypeFEVariableSpace, &
                    TypeFEVariableConstant
USE FEVariable_Method, ONLY: Get

USE ReallocateUtility, ONLY: Reallocate

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                   set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set1
CHARACTER(*), PARAMETER :: myName = "obj_Set1()"
INTEGER(I4B) :: localNode(1), conversion(1)
REAL(DFP) :: areal
LOGICAL(LGT) :: abool

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated, myName, "VectorField_::obj is not initiated")
CALL AssertError2(size(value), obj%spaceCompo, myName, "a=Value, b=obj%spaceCompo")
#endif

#include "./localNodeError.inc"

abool = Input(option=addContribution, default=.FALSE.)

localNode(1) = globalNode
conversion(1) = NONE

IF (abool) THEN

  areal = Input(option=scale, default=1.0_DFP)

  CALL Add(obj=obj%realVec, dofobj=obj%dof, nodenum=localNode, &
           VALUE=VALUE, conversion=conversion, scale=areal)

  RETURN

END IF

CALL Set(obj=obj%realVec, dofobj=obj%dof, nodenum=localNode, &
         VALUE=VALUE, conversion=conversion)

END PROCEDURE obj_Set1

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set2
INTEGER(I4B) :: s(3), idof
REAL(DFP) :: areal
LOGICAL(LGT) :: abool

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set2()"
CALL AssertError1(obj%isInitiated, myName, "VectorField_::obj is not initiated")
CALL AssertError2(size(value), obj%spaceCompo, myName, "a=Value, b=obj%spaceCompo")
#endif

abool = Input(option=addContribution, default=.FALSE.)

IF (abool) THEN

  areal = Input(option=scale, default=1.0_DFP)

  DO idof = 1, obj%spaceCompo
    s = GetNodeLoc(obj=obj%dof, idof=idof)
    CALL Add(obj=obj%realVec, VALUE=VALUE(idof), scale=areal, &
             istart=s(1), iend=s(2), stride=s(3))
  END DO

  RETURN

END IF

DO idof = 1, obj%spaceCompo

  s = GetNodeLoc(obj=obj%dof, idof=idof)
  CALL Set(obj=obj%realVec, VALUE=VALUE(idof), istart=s(1), iend=s(2), &
           stride=s(3))

END DO

END PROCEDURE obj_Set2

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set3
INTEGER(I4B) :: s(3)
REAL(DFP) :: areal
LOGICAL(LGT) :: abool

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set3()"
CALL AssertError1(obj%isInitiated, myName, &
                  "VectorField_::obj is not initiated")

CALL AssertError1(spaceCompo .LE. obj%spaceCompo, myName, &
            "given spaceCompo should be less than or equal to obj%spaceCompo")
#endif

s = GetNodeLoc(obj=obj%dof, idof=spaceCompo)

abool = Input(option=addContribution, default=.FALSE.)

IF (abool) THEN

  areal = Input(option=scale, default=1.0_DFP)
  CALL Add(obj=obj%realVec, VALUE=VALUE, istart=s(1), iend=s(2), &
           stride=s(3), scale=areal)
  RETURN

END IF

CALL Set(obj=obj%realVec, VALUE=VALUE, istart=s(1), iend=s(2), stride=s(3))
END PROCEDURE obj_Set3

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set4
CHARACTER(*), PARAMETER :: myName = "obj_Set4()"
INTEGER(I4B) :: ii, s(3)
REAL(DFP) :: areal
LOGICAL(LGT) :: abool

#ifdef DEBUG_VER

CALL AssertError1(obj%isInitiated, myName, &
                  'VectorField::obj is not initiated')

CALL AssertError1(obj%fieldType .NE. TypeField%constant, myName, &
                  'Not callable for constant STScalar field')

IF (storageFMT .EQ. NODES_FMT) THEN
  CALL AssertError2(SIZE(VALUE, 1), obj%spaceCompo, myName, &
                    'a=SIZE(VALUE, 1), b=obj%spaceCompo')

  CALL AssertError2(SIZE(VALUE, 2), obj%fedof%GetTotalDOF(), myName, &
                    'a=SIZE(VALUE, 2), b=obj%fedof%GetTotalDOF()')

ELSE

  CALL AssertError2(SIZE(VALUE, 2), obj%spaceCompo, myName, &
                    'a=SIZE(VALUE, 2), b=obj%spaceCompo')

  CALL AssertError2(SIZE(VALUE, 1), obj%fedof%GetTotalDOF(), myName, &
                    'a=SIZE(VALUE, 1), b=obj%fedof%GetTotalDOF()')
END IF

#endif

abool = Input(option=addContribution, default=.FALSE.)

SELECT CASE (storageFMT)

CASE (NODES_FMT)

  IF (abool) THEN
    areal = Input(option=scale, default=1.0_DFP)
    CALL Add(obj=obj%realVec, dofobj=obj%dof, VALUE=VALUE, scale=areal)
    RETURN
  END IF

  CALL Set(obj=obj%realVec, dofobj=obj%dof, VALUE=VALUE)

CASE (DOF_FMT)

  IF (abool) THEN
    areal = Input(option=scale, default=1.0_DFP)
    DO ii = 1, obj%spaceCompo
      s = GetNodeLoc(obj=obj%dof, idof=ii)
      CALL Add(obj=obj%realVec, VALUE=VALUE(:, ii), istart=s(1), iend=s(2), &
               stride=s(3), scale=areal)
    END DO
    RETURN
  END IF

  DO ii = 1, obj%spaceCompo
    s = GetNodeLoc(obj=obj%dof, idof=ii)
    CALL Set(obj=obj%realVec, VALUE=VALUE(:, ii), istart=s(1), iend=s(2), &
             stride=s(3))
  END DO

END SELECT

END PROCEDURE obj_Set4

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set5
CHARACTER(*), PARAMETER :: myName = "obj_Set5()"
INTEGER(I4B) :: s(3)
REAL(DFP) :: areal
LOGICAL(LGT) :: abool

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated, myName, &
                  'VectorField_::obj is not initiated')

CALL AssertError1(spaceCompo .LE. obj%spaceCompo, myName, &
            'given spaceCompo should be less than or equal to obj%spaceCompo')

CALL AssertError1(obj%fieldType .NE. TypeField%constant, myName, &
                  'Not callable for constant vector field')

CALL AssertError2(SIZE(VALUE), obj%fedof%GetTotalDOF(), myName, &
                 'Size of value should be equal to the total number of nodes')
#endif

abool = Input(option=addContribution, default=.FALSE.)
s = GetNodeLoc(obj=obj%dof, idof=spaceCompo)

IF (abool) THEN
  areal = Input(option=scale, default=1.0_DFP)
  CALL Add(obj=obj%realVec, VALUE=VALUE, istart=s(1), iend=s(2), &
           stride=s(3), scale=areal)
  RETURN
END IF

areal = Input(option=scale, default=1.0_DFP)
CALL Set(obj=obj%realVec, VALUE=VALUE, istart=s(1), iend=s(2), &
         stride=s(3))

END PROCEDURE obj_Set5

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set6
CALL obj%Set(ivar=1, idof=spaceCompo, VALUE=VALUE, ivar_value=1, &
          idof_value=spaceCompo, scale=scale, addContribution=addContribution)
END PROCEDURE obj_Set6

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set7
INTEGER(I4B) :: ii
!$OMP PARALLEL DO PRIVATE(ii)
DO ii = 1, SIZE(globalNode)
  CALL obj%Set(VALUE=VALUE, globalNode=globalNode(ii), islocal=islocal, &
               scale=scale, addContribution=addContribution)
END DO
!$OMP END PARALLEL DO
END PROCEDURE obj_Set7

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set8
INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set8()"

CALL AssertError1(obj%isInitiated, myName, &
                  'VectorField_::obj is not initiated')

CALL AssertError1(obj%fieldType .NE. TypeField%constant, myName, &
                  'Not callable for constant vector field')

IF (storageFMT .EQ. NODES_FMT) THEN
  CALL AssertError2(SIZE(VALUE, 1), obj%spaceCompo, myName, &
                    'a=SIZE(VALUE, 1), b=obj%spaceCompo')

  CALL AssertError2(SIZE(VALUE, 2), SIZE(globalNode), myName, &
                    'a=SIZE(VALUE, 2), b=size(globalNode)')

ELSE

  CALL AssertError2(SIZE(VALUE, 2), obj%spaceCompo, myName, &
                    'a=SIZE(VALUE, 2), b=obj%spaceCompo')

  CALL AssertError2(SIZE(VALUE, 1), SIZE(globalNode), myName, &
                    'a=SIZE(VALUE, 1), b=size(globalNode)')
END IF

#endif

SELECT CASE (storageFMT)

CASE (NODES_FMT)

  !$OMP PARALLEL DO PRIVATE(ii)
  DO ii = 1, SIZE(VALUE, 2)
    CALL obj%Set(globalNode=globalNode(ii), islocal=islocal, &
             VALUE=VALUE(:, ii), addContribution=addContribution, scale=scale)
  END DO
  !$OMP END PARALLEL DO

CASE (DOF_FMT)

  !$OMP PARALLEL DO PRIVATE(ii)
  DO ii = 1, SIZE(VALUE, 2)
    CALL obj%Set(globalNode=globalNode, islocal=islocal, &
                 spaceCompo=ii, VALUE=VALUE(:, ii), &
                 addContribution=addContribution, scale=scale)
  END DO
  !$OMP END PARALLEL DO

END SELECT

END PROCEDURE obj_Set8

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set9
LOGICAL(LGT) :: abool
REAL(DFP) :: areal

#ifdef DEBUG_VER

CHARACTER(*), PARAMETER :: myName = "obj_Set9()"

CALL AssertError1(obj%isInitiated, myName, &
                  'VectorField_::obj is not initiated')

CALL AssertError1(spaceCompo .LE. obj%spaceCompo, myName, &
            'given spaceCompo should be less than or equal to obj%spaceCompo')

CALL AssertError1(obj%fieldType .NE. TypeField%constant, myName, &
                  'Not callable for constant vector field')

CALL AssertError2(SIZE(VALUE), obj%spaceCompo, myName, &
                  'a=size(value), b=obj%spaceCompo')
#endif

#include "./localNodeError.inc"

abool = Input(option=addContribution, default=.FALSE.)

IF (abool) THEN
  areal = Input(option=scale, default=1.0_DFP)

  CALL Add(obj=obj%realVec, dofobj=obj%dof, nodenum=globalNode, VALUE=VALUE, &
           scale=areal, idof=spaceCompo)
  RETURN
END IF

CALL Set(obj=obj%realVec, dofobj=obj%dof, nodenum=globalNode, VALUE=VALUE, &
         idof=spaceCompo)

END PROCEDURE obj_Set9

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set10
INTEGER(I4B) :: indx

#ifdef DEBUG_VER

CHARACTER(*), PARAMETER :: myName = "obj_Set9()"

CALL AssertError1(obj%isInitiated, myName, &
                  'VectorField_::obj is not initiated')

CALL AssertError1(spaceCompo .LE. obj%spaceCompo, myName, &
            'given spaceCompo should be less than or equal to obj%spaceCompo')

CALL AssertError1(obj%fieldType .NE. TypeField%constant, myName, &
                  'Not callable for constant vector field')

#endif

#include "./localNodeError.inc"

indx = GetNodeLoc(obj=obj%dof, idof=spaceCompo, nodenum=globalNode)

CALL obj%SetSingle(VALUE=VALUE, indx=indx, scale=scale, &
                   addContribution=addContribution)

END PROCEDURE obj_Set10

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set11
CHARACTER(*), PARAMETER :: myName = "obj_Set11()"

#ifdef DEBUG_VER

CALL AssertError1(obj%isInitiated, myName, &
                  'VectorField_::obj is not initiated')

CALL AssertError1(obj%fieldType .NE. TypeField%constant, myName, &
                  'Not callable for constant vector field')

#endif

SELECT CASE (VALUE%vartype)

CASE (Constant)
  CALL obj%Set( &
    VALUE=GET(VALUE, TypeFEVariableVector, TypeFEVariableConstant), &
    globalNode=globalNode, scale=scale, addContribution=addContribution, &
    islocal=islocal)

CASE (Space)
  CALL obj%Set(VALUE=GET(VALUE, TypeFEVariableVector, TypeFEVariableSpace), &
               globalNode=globalNode, scale=scale, islocal=islocal, &
               addContribution=addContribution, storageFMT=NODES_FMT)

CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                   '[INTERNAL ERROR] :: No case found for the type of value.')
  RETURN
END SELECT

END PROCEDURE obj_Set11

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set12
CALL obj%SetAll(VALUE=VALUE, scale=scale, addContribution=addContribution)
END PROCEDURE obj_Set12

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set13
INTEGER(I4B) :: idof1, idof2
REAL(DFP) :: areal
LOGICAL(LGT) :: abool

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set13()"
CALL AssertError1(obj%isInitiated, myName, &
                  'VectorField_::obj is not initiated')

CALL AssertError1(VALUE%isInitiated, myName, &
                  'VectorField_::value is not initiated')

#endif

abool = Input(option=addContribution, default=.FALSE.)

IF (abool) THEN
  areal = Input(option=scale, default=1.0_DFP)
END IF

idof1 = GetIDOF(obj=obj%dof, ivar=ivar, idof=idof)

SELECT TYPE (VALUE)

TYPE IS (ScalarField_)

  IF (abool) THEN
    CALL Add(obj1=obj%realVec, dofobj1=obj%dof, idof1=idof1, &
             obj2=VALUE%realVec, dofobj2=VALUE%dof, idof2=1_I4B, scale=areal)
    RETURN
  END IF

  CALL Set(obj1=obj%realVec, dofobj1=obj%dof, idof1=idof1, &
           obj2=VALUE%realVec, dofobj2=VALUE%dof, idof2=1_I4B)

TYPE IS (VectorField_)

  idof2 = GetIDOF(obj=VALUE%dof, ivar=ivar_value, idof=idof_value)
  IF (abool) THEN
    CALL Add(obj1=obj%realVec, dofobj1=obj%dof, idof1=idof1, &
             obj2=VALUE%realVec, dofobj2=VALUE%dof, idof2=idof2, &
             scale=areal)
    RETURN
  END IF

  CALL Set(obj1=obj%realVec, dofobj1=obj%dof, idof1=idof1, &
           obj2=VALUE%realVec, dofobj2=VALUE%dof, idof2=idof2)

! TYPE IS (ScalarFieldLis_)
! TYPE IS (VectorFieldLis_)

CLASS DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                   '[INTERNAL ERORR] :: No case found for the type of value.')
  RETURN
END SELECT

END PROCEDURE obj_Set13

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set14
CALL obj%Copy(VALUE)
END PROCEDURE obj_Set14

!----------------------------------------------------------------------------
!                                                       SetFromSTVectorField
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetFromSTVectorField
CHARACTER(*), PARAMETER :: myName = "obj_SetFromSTVectorField()"
INTEGER(I4B) :: jj, ii

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated, myName, &
                  'VectorField_::obj is not initiated')

CALL AssertError1(VALUE%isInitiated, myName, &
                  'VectorField_::value is not initiated')

CALL AssertError1(obj%fieldType .NE. TypeField%constant, myName, &
                  'Not callable for constant vector field')

#endif

CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')

! SELECT TYPE (VALUE); TYPE IS (STVectorField_)
!
!   !$OMP PARALLEL DO PRIVATE(ii, jj)
!   DO ii = 1, obj%spaceCompo
!     jj = GetIDOF(spaceCompo=ii, timeCompo=timeCompo)
!     CALL obj%Set(ivar=1_I4B, idof=ii, VALUE=VALUE, scale=scale, &
!              addContribution=addContribution, ivar_value=1_I4B, idof_value=jj)
!   END DO
!   !$OMP END PARALLEL DO

! CLASS DEFAULT
! CALL e%RaiseError(modName//'::'//myName//' - '// &
!           '[INTERNAL ERROR] :: Value should be an instance of STVectorField_')
! RETURN
! END SELECT

END PROCEDURE obj_SetFromSTVectorField

!----------------------------------------------------------------------------
!                                                                      Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetByFunction
CHARACTER(*), PARAMETER :: myName = "obj_SetByFunction()"
LOGICAL(LGT) :: istimes, problem
INTEGER(I4B) :: ttime, returnType, nsd, tnodes, ii, globalNode(1)
REAL(DFP) :: args(4), xij(3, 1)
REAL(DFP), ALLOCATABLE :: VALUE(:)
INTEGER(I4B), PARAMETER :: needed_returnType = Vector
CLASS(AbstractMesh_), POINTER :: meshptr
CHARACTER(:), ALLOCATABLE :: baseInterpolation

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
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
                    '[INTERNAL ERROR] :: domain is not ASSOCIATED.')
  RETURN
END IF

nsd = meshptr%GetNSD()
tnodes = meshptr%GetTotalNodes()

DO ii = 1, tnodes
  globalNode(1) = ii
  CALL meshptr%GetNodeCoord(globalNode=globalNode, nodeCoord=xij, &
                            islocal=.TRUE.)

  args(1:nsd) = xij(1:nsd, 1)

  CALL func%Get(val=VALUE, args=args)

  CALL obj%Set(globalNode=globalNode(1), VALUE=VALUE, islocal=.TRUE.)

END DO

IF (ALLOCATED(VALUE)) DEALLOCATE (VALUE)
NULLIFY (meshptr)

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
