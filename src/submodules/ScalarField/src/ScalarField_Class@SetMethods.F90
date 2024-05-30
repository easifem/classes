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

USE AbstractField_Class, ONLY: FIELD_TYPE_CONSTANT

USE RealVector_Method, ONLY: Set, Add

USE Display_Method, ONLY: ToString

USE ArangeUtility, ONLY: Arange

USE DOF_Method, ONLY: GetNodeLoc, &
                      OPERATOR(.tNodes.)

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
INTEGER(I4B) :: localNode(1)
REAL(DFP) :: areal
LOGICAL(LGT) :: abool

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set1()"

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                   '[INTERNAL ERROR] :: Scalar field object is not initiated')
  RETURN
END IF

#endif

abool = Input(option=AddContribution, default=.FALSE.)

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN

  IF (abool) THEN
    localNode(1) = 1
    areal = Input(option=scale, default=1.0_DFP)
    CALL Add(obj%realVec, nodenum=localnode, VALUE=[VALUE], scale=areal)
    RETURN
  END IF

  localnode(1) = 1
  CALL Set(obj%realVec, nodenum=localnode, VALUE=[VALUE])
  RETURN

END IF

localNode(1) = obj%fedof%mesh%GetLocalNodeNumber(globalNode=globalNode, &
                                                 islocal=islocal)

IF (localNode(1) .EQ. 0) RETURN

IF (abool) THEN
  areal = Input(option=scale, default=1.0_DFP)
  CALL Add(obj%realVec, nodenum=localNode, VALUE=[VALUE], scale=areal)
  RETURN
END IF

CALL Set(obj%realVec, nodenum=localNode, VALUE=[VALUE])

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

abool = Input(option=AddContribution, default=.FALSE.)

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

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
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

abool = Input(option=AddContribution, default=.FALSE.)

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
INTEGER(I4B) :: localNode(SIZE(globalNode))
REAL(DFP) :: areal
LOGICAL(LGT) :: abool

#ifdef DEBUG_VER

CHARACTER(*), PARAMETER :: myName = "obj_Set4()"

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    'Scalar field object is not initiated')
  RETURN
END IF

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                 'This routine should not be called for constant field type.')
  RETURN
END IF

#endif

localNode = obj%fedof%mesh%GetLocalNodeNumber(globalNode=globalNode, &
                                              islocal=islocal)

#ifdef DEBUG_VER

IF (ANY(localNode .GT. obj%tSize)) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    'Some of the globalNode are out of bound')
  RETURN
END IF

#endif

abool = Input(option=AddContribution, default=.FALSE.)

IF (abool) THEN
  areal = Input(option=scale, default=1.0_DFP)
  CALL Add(obj%realVec, nodenum=localNode, VALUE=VALUE, scale=areal)
ELSE
  CALL Set(obj%realVec, nodenum=localNode, VALUE=VALUE)
END IF

END PROCEDURE obj_Set4

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set5
INTEGER(I4B) :: localNode(SIZE(globalNode))
REAL(DFP) :: areal
LOGICAL(LGT) :: abool

#ifdef DEBUG_VER

CHARACTER(*), PARAMETER :: myName = "obj_Set5()"

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                   '[INTERNAL ERROR] :: Scalar field object is not initiated')
  RETURN
END IF

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    '[INTERNAL ERROR] :: Not valid for constant field type.')
  RETURN
END IF

#endif

localNode = obj%fedof%mesh%GetLocalNodeNumber(globalNode=globalNode, &
                                              islocal=islocal)

#ifdef DEBUG_VER

IF (ANY(localNode .GT. obj%tSize)) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                '[INTERNAL ERROR] :: Some of the globalNode are out of bound')
  RETURN
END IF

#endif

abool = Input(option=AddContribution, default=.FALSE.)

IF (abool) THEN
  areal = Input(option=scale, default=1.0_DFP)
  CALL Add(obj=obj%realVec, nodenum=localNode, VALUE=VALUE, scale=areal)

ELSE

  CALL Set(obj%realVec, nodenum=localNode, VALUE=VALUE)
END IF

END PROCEDURE obj_Set5

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set6
CALL obj%Set(globalNode=Arange(istart, iend, stride), &
             VALUE=VALUE, scale=scale, &
             AddContribution=addContribution, islocal=islocal)
END PROCEDURE obj_Set6

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set7
CALL obj%Set(globalNode=Arange(istart, iend, stride), &
             VALUE=VALUE, scale=scale, &
             AddContribution=addContribution, islocal=islocal)
END PROCEDURE obj_Set7

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set8
CALL Set(obj%realVec, VALUE=VALUE%realVec)
END PROCEDURE obj_Set8

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set9
CHARACTER(*), PARAMETER :: myName = "obj_Set9()"

SELECT CASE (VALUE%vartype)

CASE (Constant)

  CALL obj%Set( &
    VALUE=GET(VALUE, TypeFEVariableScalar, TypeFEVariableConstant), &
    globalNode=globalNode, scale=scale, AddContribution=addContribution, &
    islocal=islocal)

CASE (Space)

  CALL obj%Set( &
    VALUE=GET(VALUE, TypeFEVariableScalar, TypeFEVariableSpace), &
    globalNode=globalNode, scale=scale, AddContribution=addContribution, &
    islocal=islocal)

CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: No case found')
  RETURN
END SELECT

END PROCEDURE obj_Set9

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set10
CALL Add(obj%realVec, VALUE=obj2%realVec, scale=scale)
END PROCEDURE obj_Set10

!----------------------------------------------------------------------------
!                                                                      Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set11
INTEGER(I4B) :: tsize, tsize_value, ii, indx1, indx2, ivar_idof(2)
REAL(DFP) :: avar

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set11()"
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')

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

#endif

ivar_idof(1) = ivar
ivar_idof(2) = idof

tsize = obj%dof.tNodes.ivar_idof

ivar_idof(1) = ivar_value
ivar_idof(2) = idof_value
tsize_value = VALUE%dof.tNodes.ivar_idof

#ifdef DEBUG_VER

IF (tsize .NE. tsize_value) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                   '[INTERNAL ERROR] :: size mismatch between obj and value.')
  RETURN
END IF

#endif

DO ii = 1, tsize

  indx1 = GetNodeLoc(obj=VALUE%dof, nodenum=ii, ivar=ivar_value, &
                     idof=idof_value)

  CALL VALUE%GetSingle(VALUE=avar, indx=indx1)

  indx2 = GetNodeLoc(obj=obj%dof, nodenum=ii, ivar=ivar, idof=idof)

  CALL obj%SetSingle(VALUE=avar, indx=indx2, scale=scale, &
                     addContribution=addContribution)

END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Set11

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
