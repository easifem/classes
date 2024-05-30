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

SUBMODULE(STScalarField_Class) SetMethods
USE InputUtility, ONLY: Input

USE AbstractMesh_Class, ONLY: AbstractMesh_

USE AbstractField_Class, ONLY: TypeField

USE ScalarField_Class, ONLY: ScalarField_

USE RealVector_Method, ONLY: Set, Add, GetPointer

USE Display_Method, ONLY: tostring

USE GlobalData, ONLY: NONE, SpaceTime, Scalar

USE DOF_Method, ONLY: GetNodeLoc, &
                      OPERATOR(.tNodes.), &
                      GetIDOF

USE ArangeUtility, ONLY: Arange

USE BaseType, ONLY: TypeFEVariableScalar, &
                    TypeFEVariableSpaceTime

USE FEVariable_Method, ONLY: Get

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                   Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set1
CHARACTER(*), PARAMETER :: myName = "obj_Set1()"
INTEGER(I4B) :: localNode(1)
REAL(DFP) :: areal
LOGICAL(LGT) :: abool

#ifdef DEBUG_VER

LOGICAL(LGT) :: isok

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                   '[INTERNAL ERROR] :: STScalarField_::obj is not initiated')
  RETURN
END IF

IF (SIZE(VALUE) .NE. obj%timeCompo) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
         '[INTERNAL ERROR] :: Size of value should be equal to obj%timeCompo')
  RETURN
END IF

#endif

#include "./localNodeError.inc"

abool = Input(option=addContribution, default=.FALSE.)

IF (obj%fieldType .EQ. TypeField%constant) THEN

  localNode(1) = 1

ELSE

  localNode(1) = globalNode

END IF

IF (abool) THEN
  areal = Input(option=scale, default=1.0_DFP)
  CALL Add(obj=obj%realVec, dofobj=obj%dof, nodenum=localNode, &
           VALUE=VALUE, conversion=[NONE], scale=areal)
  RETURN
END IF

CALL Set(obj=obj%realVec, dofobj=obj%dof, nodenum=localNode, &
         VALUE=VALUE, conversion=[NONE])

END PROCEDURE obj_Set1

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set2
INTEGER(I4B) :: idof, s(3)
REAL(DFP) :: areal
LOGICAL(LGT) :: abool

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set2()"

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                   '[INTERNAL ERROR] :: STScalarField_::obj is not initiated')
  RETURN
END IF

IF (SIZE(VALUE) .NE. obj%timeCompo) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
            '[INTERNAL ERROR] :: size(value) should be same as obj%timeCompo')
  RETURN
END IF

#endif

abool = Input(option=addContribution, default=.FALSE.)

IF (obj%fieldType .EQ. TypeField%constant) THEN

  IF (abool) THEN
    areal = Input(option=scale, default=1.0_DFP)
    CALL Add(obj=obj%realVec, dofobj=obj%dof, nodenum=[1], VALUE=VALUE, &
             conversion=[NONE], scale=areal)
    RETURN
  END IF

  CALL Set(obj=obj%realVec, dofobj=obj%dof, nodenum=[1], VALUE=VALUE, &
           conversion=[NONE])

  RETURN
END IF

IF (abool) THEN
  areal = Input(option=scale, default=1.0_DFP)

  DO idof = 1, obj%timeCompo

    s = GetNodeLoc(obj=obj%dof, idof=idof)
    CALL Add(obj=obj%realVec, VALUE=VALUE(idof), scale=areal, &
             istart=s(1), iend=s(2), stride=s(3))

  END DO
  RETURN
END IF

DO idof = 1, obj%timeCompo

  s = GetNodeLoc(obj=obj%dof, idof=idof)
  CALL Set(obj=obj%realVec, VALUE=VALUE(idof), istart=s(1), iend=s(2), &
           stride=s(3))

END DO

END PROCEDURE obj_Set2

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set3
INTEGER(I4B) :: s(3)
REAL(DFP) :: areal
LOGICAL(LGT) :: abool

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set3()"

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                 '[INTERNAL ERROR] :: STScalar field object is not initiated')
  RETURN
END IF

IF (timeCompo .GT. obj%timeCompo) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
     '[INTERNAL ERROR] :: timeCompo should be less or equal to obj%timeCompo')
  RETURN
END IF

#endif

abool = Input(option=addContribution, default=.FALSE.)

IF (obj%fieldType .EQ. TypeField%constant) THEN

  IF (abool) THEN
    areal = Input(option=scale, default=1.0_DFP)
    CALL Add(obj=obj%realVec, dofobj=obj%dof, nodenum=[1], VALUE=[VALUE], &
             idof=timeCompo, scale=areal)
    RETURN
  END IF

  CALL Set(obj=obj%realVec, dofobj=obj%dof, nodenum=[1], VALUE=[VALUE], &
           idof=timeCompo)
  RETURN

END IF

s = GetNodeLoc(obj=obj%dof, idof=timeCompo)
IF (abool) THEN
  areal = Input(option=scale, default=1.0_DFP)
  CALL Add(obj=obj%realVec, VALUE=VALUE, scale=areal, istart=s(1), &
           iend=s(2), stride=s(3))
  RETURN
END IF

CALL Set(obj=obj%realVec, VALUE=VALUE, istart=s(1), iend=s(2), stride=s(3))

END PROCEDURE obj_Set3

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set4
REAL(DFP) :: areal
LOGICAL(LGT) :: abool

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set4()"
LOGICAL(LGT) :: isok
INTEGER(I4B) :: tnodes

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                 '[INTERNAL ERROR] :: STScalar field object is not initiated')
  RETURN
END IF

IF (obj%fieldType .EQ. TypeField%constant) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
               '[INTERNAL ERROR] :: Not callable for constant STScalar field')
  RETURN
END IF

tnodes = obj%fedof%GetTotalDOF()

isok = (SIZE(VALUE, 2) .NE. obj%timeCompo) .OR. (SIZE(VALUE, 1) .NE. tnodes)
IF (isok) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    '[INTERNAL ERROR] :: The shape of value should be [ ' &
                    //tostring(tnodes)//', '//tostring(obj%timeCompo)//' ]')
  RETURN
END IF

#endif

abool = Input(option=addContribution, default=.FALSE.)

IF (abool) THEN
  areal = Input(option=scale, default=1.0_DFP)
  CALL Add(obj=obj%realVec, dofobj=obj%dof, VALUE=VALUE, scale=areal)
  RETURN
END IF

CALL Set(obj=obj%realVec, dofobj=obj%dof, VALUE=VALUE)

END PROCEDURE obj_Set4

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set5
REAL(DFP) :: areal
LOGICAL(LGT) :: abool

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set5()"
INTEGER(I4B) :: tsize

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                 '[INTERNAL ERROR] :: STScalar field object is not initiated')
  RETURN
END IF

IF (timeCompo .GT. obj%timeCompo) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                   '[INTERNAL ERROR] :: timeCompo is more than obj%timeCompo')
  RETURN
END IF

IF (obj%fieldType .EQ. TypeField%constant) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
               '[INTERNAL ERROR] :: not callable for constant STScalar field')
  RETURN
END IF

tsize = obj%fedof%GetTotalDOF()

IF (SIZE(VALUE) .NE. tsize) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    '[INTERNAL ERROR] :: Size of value is out of bound')
  RETURN
END IF

#endif

abool = Input(option=addContribution, default=.FALSE.)

IF (abool) THEN
  areal = Input(option=scale, default=1.0_DFP)
  CALL Add(obj=obj%realVec, VALUE=VALUE, scale=areal, idof=timeCompo, &
           dofobj=obj%dof)

  RETURN

END IF

CALL Set(obj=obj%realVec, VALUE=VALUE, idof=timeCompo, dofobj=obj%dof)

END PROCEDURE obj_Set5

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set6
INTEGER(I4B) :: idof
REAL(DFP) :: areal, breal
LOGICAL(LGT) :: abool
CHARACTER(*), PARAMETER :: myName = "obj_Set6()"

#ifdef DEBUG_VER

LOGICAL(LGT) :: isok
INTEGER(I4B) :: tnodes

IF (.NOT. obj%isInitiated .OR. .NOT. VALUE%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                 '[INTERNAL ERROR] :: STScalar field object is not initiated')
  RETURN
END IF

IF (timeCompo .GT. obj%timeCompo) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                  '[INTERNAL ERROR] ::  timeCompo is more than obj%timeCompo')
  RETURN
END IF

IF (obj%fieldType .EQ. TypeField%constant) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
               '[INTERNAL ERROR] :: not callable for constant STScalar field')
  RETURN
END IF

#endif

SELECT TYPE (VALUE)

TYPE IS (ScalarField_)

#ifdef DEBUG_VER

  isok = VALUE%fedof%GetTotalDOF() .EQ. obj%fedof%GetTotalDOF()

  IF (.NOT. isok) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
               '[INTERNAL ERROR] :: Size of value .NE. total number of nodes')
    RETURN
  END IF

#endif

  IF (VALUE%fieldType .EQ. TypeField%constant) THEN

    CALL VALUE%Get(VALUE=breal, globalNode=1_I4B, islocal=.TRUE.)

    CALL obj%Set(VALUE=breal, timeCompo=timeCompo, scale=scale, &
                 addContribution=addContribution)

    RETURN

  END IF

  abool = Input(option=addContribution, default=.FALSE.)

  IF (abool) THEN
    areal = Input(option=scale, default=1.0_DFP)
    CALL Add(obj1=obj%realVec, dofobj1=obj%dof, idof1=timeCompo, &
             obj2=VALUE%realVec, dofobj2=VALUE%dof, idof2=1_I4B, scale=areal)
    RETURN
  END IF

  CALL Set(obj1=obj%realVec, dofobj1=obj%dof, idof1=timeCompo, &
           obj2=VALUE%realVec, dofobj2=VALUE%dof, idof2=1_I4B)

TYPE IS (STScalarField_)

  IF (VALUE%fieldType .EQ. TypeField%constant) THEN

    CALL VALUE%Get(VALUE=breal, globalNode=1_I4B, islocal=.TRUE., &
                   timeCompo=timeCompo)

    CALL obj%Set(VALUE=breal, timeCompo=timeCompo, scale=scale, &
                 addContribution=addContribution)

    RETURN

  END IF

  abool = Input(option=addContribution, default=.FALSE.)

  IF (abool) THEN
    areal = Input(option=scale, default=1.0_DFP)
    CALL Add(obj1=obj%realVec, dofobj1=obj%dof, idof1=timeCompo, &
             obj2=VALUE%realVec, dofobj2=VALUE%dof, idof2=timeCompo, &
             scale=areal)
    RETURN
  END IF

  CALL Set(obj1=obj%realVec, dofobj1=obj%dof, idof1=timeCompo, &
           obj2=VALUE%realVec, dofobj2=VALUE%dof, idof2=timeCompo)

CLASS DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: No case found for the type of value')
  RETURN
END SELECT

END PROCEDURE obj_Set6

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set7
! FIXME: remove this allocation
REAL(DFP) :: val(SIZE(VALUE), SIZE(globalNode))
INTEGER(I4B) :: ii

DO ii = 1, SIZE(globalNode)
  val(:, ii) = VALUE(:)
END DO

CALL obj%Set(VALUE=val, globalNode=globalNode, islocal=islocal, &
             scale=scale, addContribution=addContribution, &
             storageFormat=NODES_FMT)

END PROCEDURE obj_Set7

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set8
CHARACTER(*), PARAMETER :: myName = "obj_Set8()"
REAL(DFP) :: val(SIZE(VALUE))
INTEGER(I4B) :: conversion(1)
REAL(DFP) :: areal
LOGICAL(LGT) :: abool

#ifdef DEBUG_VER
LOGICAL(LGT) :: problem

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                   '[INTERNAL ERROR] :: Scalar field object is not initiated')
  RETURN
END IF

IF (obj%fieldType .EQ. TypeField%constant) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
               '[INTERNAL ERROR] :: Nor callable for constant STScalar field')
  RETURN
END IF

problem = (SIZE(VALUE, 1) .NE. obj%timeCompo) .OR. &
          (SIZE(VALUE, 2) .NE. SIZE(globalNode))
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    '[INTERNAL ERROR] :: SIZE(value,1) not equal timeCompo ' &
                    //'or SIZE( value, 2 ) not equal to the SIZE(globalNode)')
  RETURN
END IF

#endif

IF (storageFormat .EQ. mystorageformat) THEN
  conversion(1) = NONE
ELSE
  conversion(1) = myconversion
END IF

val = RESHAPE(VALUE, [SIZE(VALUE)])

abool = Input(option=addContribution, default=.FALSE.)

IF (abool) THEN
  areal = Input(option=scale, default=1.0_DFP)

  CALL Add(obj=obj%realVec, dofobj=obj%dof, nodenum=globalNode, &
           VALUE=val, conversion=conversion, scale=areal)
  RETURN

END IF

CALL Set(obj=obj%realVec, dofobj=obj%dof, nodenum=globalNode, &
         VALUE=val, conversion=conversion)

END PROCEDURE obj_Set8

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set9
CHARACTER(*), PARAMETER :: myName = "obj_Set9()"
REAL(DFP) :: areal
LOGICAL(LGT) :: abool

#ifdef DEBUG_VER

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                 '[INTERNAL ERROR] :: STScalar field object is not initiated')
  RETURN
END IF

IF (timeCompo .GT. obj%timeCompo) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    '[INTERNAL ERROR] :: timeCompo is out of bound')
  RETURN
END IF

IF (obj%fieldType .EQ. TypeField%constant) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
               '[INTERNAL ERROR] :: Not callable for constant STScalar field')
  RETURN
END IF

IF (SIZE(VALUE) .NE. SIZE(globalNode)) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    '[INTERNAL ERROR] :: The size of value is not correct')
  RETURN
END IF

#endif

#include "./localNodeError.inc"

abool = Input(option=addContribution, default=.FALSE.)

IF (abool) THEN
  areal = Input(option=scale, default=1.0_DFP)
  CALL Add(obj=obj%realVec, dofobj=obj%dof, nodenum=globalNode, &
           VALUE=VALUE, idof=timeCompo, scale=areal)
  RETURN
END IF

CALL Set(obj=obj%realVec, dofobj=obj%dof, nodenum=globalNode, &
         VALUE=VALUE, idof=timeCompo)

END PROCEDURE obj_Set9

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set10
CHARACTER(*), PARAMETER :: myName = "obj_Set10()"
REAL(DFP) :: areal
LOGICAL(LGT) :: abool

#ifdef DEBUG_VER

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                 '[INTERNAL ERROR] :: STScalar field object is not initiated')
  RETURN
END IF

IF (timeCompo .GT. obj%timeCompo) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    '[INTERNAL ERROR]:: timeCompo out of bound.')
  RETURN
END IF

IF (obj%fieldType .EQ. TypeField%constant) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
               '[INTERNAL ERROR] :: NOT Callable for constant STScalar field')
  RETURN
END IF

#endif

#include "./localNodeError.inc"

abool = Input(option=addContribution, default=.FALSE.)

IF (abool) THEN
  areal = Input(option=scale, default=1.0_DFP)
  CALL Add(obj=obj%realVec, dofobj=obj%dof, nodenum=globalNode, &
           VALUE=VALUE, scale=areal, idof=timeCompo)
  RETURN
END IF

CALL Set(obj=obj%realVec, dofobj=obj%dof, nodenum=globalNode, &
         VALUE=VALUE, idof=timeCompo)

END PROCEDURE obj_Set10

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set11
CHARACTER(*), PARAMETER :: myName = "obj_Set11()"

#ifdef DEBUG_VER

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                   '[INTERNAL ERROR] :: Scalar field object is not initiated')
  RETURN
END IF

IF (obj%fieldType .EQ. TypeField%constant) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
               '[INTERNAL ERROR] :: Not callable for constant STScalar field')
  RETURN
END IF

#endif

#include "./localNodeError.inc"

SELECT CASE (VALUE%vartype); CASE (SpaceTime)

  CALL obj%Set(VALUE=GET(VALUE, TypeFEVariableScalar, &
                         TypeFEVariableSpaceTime), &
               globalNode=globalNode, scale=scale, &
               addContribution=addContribution, islocal=islocal, &
               storageFormat=NODES_FMT)

CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: No case found for Value%vartype')
  RETURN
END SELECT

END PROCEDURE obj_Set11

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set12
LOGICAL(LGT) :: abool
REAL(DFP) :: areal

abool = Input(option=addContribution, default=.FALSE.)

IF (abool) THEN
  areal = Input(option=scale, default=1.0_DFP)
  CALL Add(obj=obj%realVec, VALUE=VALUE, scale=areal)
  RETURN
END IF

CALL Set(obj=obj%realVec, VALUE=VALUE)

END PROCEDURE obj_Set12

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set13
INTEGER(I4B) :: idof1
INTEGER(I4B) :: idof2
REAL(DFP) :: areal
LOGICAL(LGT) :: abool

#ifdef DEBUG_VER

CHARACTER(*), PARAMETER :: myName = "obj_Set13()"
INTEGER(I4B) :: tsize
INTEGER(I4B) :: tsize_value

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                   '[INTERNAL ERROR] :: STScalarField_::obj is not initiated')
  RETURN
END IF

IF (.NOT. VALUE%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
            '[INTERNAL ERROR] :: AbstractNodeField_ ::value is not initiated')
  RETURN
END IF

tsize = obj%dof.tNodes. [ivar, idof]
tsize_value = VALUE%dof.tNodes. [ivar_value, idof_value]
IF (tsize .NE. tsize_value) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                 '[INTERNAL ERROR] :: tSize of obj(ivar, idof) NOT equal '// &
                    'to value(ivar_value, idof_value)')
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

END PROCEDURE obj_Set13

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set14
CALL Set(obj=obj%realVec, VALUE=VALUE%realVec)
END PROCEDURE obj_Set14

!----------------------------------------------------------------------------
!                                                               SetByFunction
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetByFunction
CHARACTER(*), PARAMETER :: myName = "obj_SetByFunction()"
LOGICAL(LGT) :: istimes, problem
INTEGER(I4B) :: ttime, nsd, tnodes, ii, itime, i1(1)
REAL(DFP) :: args(4), VALUE(obj%timeCompo), aval, xij(3, 1)
INTEGER(I4B), PARAMETER :: needed_returnType = Scalar
CLASS(AbstractMesh_), POINTER :: meshptr
CHARACTER(:), ALLOCATABLE :: baseInterpolation

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
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
  problem = ttime .NE. obj%timeCompo
END IF

IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
              '[INTERNAL ERROR] :: size of times should be obj%timeCompo='// &
                    tostring(obj%timeCompo))
  RETURN
END IF

meshptr => NULL()
meshptr => obj%fedof%GetMeshPointer()
problem = .NOT. ASSOCIATED(meshptr)
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: meshptrain is not ASSOCIATED.')
  RETURN
END IF

nsd = meshptr%GetNSD()
tnodes = meshptr%GetTotalNodes()
CALL Reallocate(xij, nsd, 1)

IF (istimes) THEN

  DO ii = 1, tnodes
    i1(1) = ii
    CALL meshptr%GetNodeCoord(globalNode=i1, nodeCoord=xij, islocal=.TRUE.)

    args(1:nsd) = xij(1:nsd, 1)

    DO itime = 1, obj%timeCompo
      args(4) = times(itime)
      CALL func%Get(val=VALUE(itime), args=args)
    END DO

    CALL obj%Set(globalNode=ii, VALUE=VALUE, islocal=.TRUE.)

  END DO

  RETURN

END IF

DO ii = 1, tnodes
  i1(1) = ii
  CALL meshptr%GetNodeCoord(globalNode=i1, nodeCoord=xij, &
                            islocal=.TRUE.)
  args(1:nsd) = xij(1:nsd, 1)
  CALL func%Get(val=aval, args=args)
  VALUE = aval
  CALL obj%Set(globalNode=ii, VALUE=VALUE, islocal=.TRUE.)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetByFunction

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods
