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

USE ScalarFieldLis_Class, ONLY: ScalarFieldLis_

USE STScalarField_Class, ONLY: STScalarField_

USE STScalarFieldLis_Class, ONLY: STScalarFieldLis_

USE VectorField_Class, ONLY: VectorField_

USE VectorFieldLis_Class, ONLY: VectorFieldLis_

USE BlockNodeField_Class, ONLY: BlockNodeField_

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

USE StringUtility, ONLY: UpperCase

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set1
CHARACTER(*), PARAMETER :: myName = "obj_Set1"
INTEGER(I4B) :: localNode
REAL(DFP) :: areal
LOGICAL(LGT) :: abool
areal = Input(option=scale, default=1.0_DFP)
abool = Input(option=addContribution, default=.FALSE.)

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: Scalar field object is not initiated')
  RETURN
END IF

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  IF (abool) THEN
    CALL add(obj%realVec, nodenum=[1], VALUE=[VALUE], scale=areal)
    RETURN
  END IF

  CALL Set(obj%realVec, nodenum=[1], VALUE=[VALUE])
  RETURN
END IF

localNode = obj%domain%GetLocalNodeNumber(globalNode)

IF (localNode .NE. 0) THEN
  IF (abool) THEN
    CALL add(obj%realVec, nodenum=[localNode], VALUE=[VALUE], scale=areal)
    RETURN
  END IF

  CALL Set(obj%realVec, nodenum=[localNode], VALUE=[VALUE])
  RETURN
END IF

END PROCEDURE obj_Set1

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set2
CHARACTER(*), PARAMETER :: myName = "obj_Set2"
REAL(DFP) :: areal
LOGICAL(LGT) :: abool
areal = Input(option=scale, default=1.0_DFP)
abool = Input(option=addContribution, default=.FALSE.)

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: Scalar field object is not initiated')
  RETURN
END IF

IF (abool) THEN
  CALL add(obj%realVec, VALUE=VALUE, scale=areal)
ELSE
  CALL Set(obj%realVec, VALUE=VALUE)
END IF

END PROCEDURE obj_Set2

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set3
CHARACTER(*), PARAMETER :: myName = "obj_Set3"
REAL(DFP) :: areal
LOGICAL(LGT) :: abool
areal = Input(option=scale, default=1.0_DFP)
abool = Input(option=addContribution, default=.FALSE.)

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & 'Scalar field object is not initiated')
END IF

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & '[INTERNAL ERROR] :: This routine should not be '//  &
    & 'called for constant field type.')
  RETURN
END IF

IF (obj%tSize .NE. SIZE(VALUE)) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & '[INTERNAL ERROR] :: Size of value ('//tostring(SIZE(VALUE))//  &
  & ') is not equal to size of scalarfield ('//  &
  & tostring(obj%tSize)//')')
  RETURN
END IF

IF (abool) THEN
  CALL add(obj%realVec, VALUE=VALUE, scale=areal)
ELSE
  CALL Set(obj%realVec, VALUE=VALUE)
END IF

END PROCEDURE obj_Set3

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set4
CHARACTER(*), PARAMETER :: myName = "obj_Set4"
INTEGER(I4B) :: localNode(SIZE(globalNode))
REAL(DFP) :: areal
LOGICAL(LGT) :: abool
areal = Input(option=scale, default=1.0_DFP)
abool = Input(option=addContribution, default=.FALSE.)

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'Scalar field object is not initiated')
  RETURN
END IF

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'This routine should not be called for constant field type.')
  RETURN
END IF

localNode = obj%domain%GetLocalNodeNumber(globalNode)
IF (ANY(localNode .GT. obj%tSize)) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'Some of the globalNode are out of bound')
  RETURN
END IF

IF (abool) THEN
  CALL add(obj%realVec, nodenum=localNode, VALUE=VALUE, scale=areal)
ELSE
  CALL Set(obj%realVec, nodenum=localNode, VALUE=VALUE)
END IF
END PROCEDURE obj_Set4

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set5
CHARACTER(*), PARAMETER :: myName = "obj_Set5"
INTEGER(I4B) :: localNode(SIZE(globalNode))
REAL(DFP) :: areal
LOGICAL(LGT) :: abool
areal = Input(option=scale, default=1.0_DFP)
abool = Input(option=addContribution, default=.FALSE.)

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'Scalar field object is not initiated')
  RETURN
END IF

IF (obj%fieldType .EQ. FIELD_TYPE_CONSTANT) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'This routine should not be called for constant field type.')
  RETURN
END IF

localNode = obj%domain%GetLocalNodeNumber(globalNode)

IF (ANY(localNode .GT. obj%tSize)) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    & 'Some of the globalNode are out of bound')
  RETURN
END IF

IF (abool) THEN
  CALL add( &
    & obj=obj%realVec, &
    & nodenum=localNode, &
    & VALUE=VALUE, &
    & scale=areal)
ELSE
  CALL Set(obj%realVec, nodenum=localNode, VALUE=VALUE)
END IF

END PROCEDURE obj_Set5

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set6
CHARACTER(*), PARAMETER :: myName = "obj_Set6"
INTEGER(I4B) :: globalNode(INT(1 + (iend - istart) / stride)), ii, jj

jj = 0
DO ii = istart, iend, stride
  jj = jj + 1
  globalNode(jj) = ii
END DO
CALL obj%Set(globalNode=globalNode, VALUE=VALUE, scale=scale, &
  & addContribution=addContribution)
END PROCEDURE obj_Set6

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set7
CHARACTER(*), PARAMETER :: myName = "obj_Set7"
INTEGER(I4B) :: globalNode(INT(1 + (iend - istart) / stride)), ii, jj

jj = 0
DO ii = istart, iend, stride
  jj = jj + 1
  globalNode(jj) = ii
END DO
CALL obj%Set(globalNode=globalNode, VALUE=VALUE, scale=scale, &
  & addContribution=addContribution)
END PROCEDURE obj_Set7

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set8
CHARACTER(*), PARAMETER :: myName = "obj_Set8()"

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

CALL Set(obj%realVec, VALUE=VALUE%realVec)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_Set8

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set9
CHARACTER(*), PARAMETER :: myName = "obj_Set9"

SELECT CASE (VALUE%vartype)
CASE (Constant)
  CALL obj%Set( &
  & VALUE=GET(VALUE, TypeFEVariableScalar, TypeFEVariableConstant), &
  & globalNode=globalNode, &
  & scale=scale, &
  & addContribution=addContribution)
CASE (Space)
  CALL obj%Set( &
    & VALUE=GET(VALUE, TypeFEVariableScalar, TypeFEVariableSpace), &
    & globalNode=globalNode, &
    & scale=scale, &
    & addContribution=addContribution)
CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
  & 'No case found for Value%vartype, only [Constant and Space is allowed]')
END SELECT
END PROCEDURE obj_Set9

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set10
CALL add(obj%realVec, VALUE=obj2%realVec, scale=scale)
END PROCEDURE obj_Set10

!----------------------------------------------------------------------------
!                                                                      Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set11
CHARACTER(*), PARAMETER :: myName = "obj_Set11"
INTEGER(I4B) :: tsize, tsize_value, ii, indx1, indx2, ivar_idof(2)
REAL(DFP) :: avar

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

s = GetNodeLoc(obj=obj%dof, idof=1_I4B)

SELECT TYPE (VALUE)

TYPE IS (ScalarField_)

  realvec => VALUE%GetPointer()

  CALL obj%SetMultiple(istart=s(1), iend=s(2), stride=s(3), &
                  VALUE=realvec, scale=scale, addContribution=addContribution)

  realvec => NULL()

TYPE IS (STScalarField_)

  p = GetNodeLoc(obj=VALUE%dof, idof=GetIDOF(obj=VALUE%dof, ivar=1_I4B, &
                                             idof=idof_value))
  realvec => VALUE%GetPointer()

  CALL obj%SetMultiple(istart=s(1), iend=s(2), stride=s(3), &
                      istart_value=p(1), iend_value=p(2), stride_value=p(3), &
                  VALUE=realvec, scale=scale, addContribution=addContribution)

  realvec => NULL()

TYPE IS (VectorField_)

  p = GetNodeLoc(obj=VALUE%dof, idof=GetIDOF(obj=VALUE%dof, ivar=1_I4B, &
                                             idof=idof_value))
  realvec => VALUE%GetPointer()

  CALL obj%SetMultiple(istart=s(1), iend=s(2), stride=s(3), &
                      istart_value=p(1), iend_value=p(2), stride_value=p(3), &
                  VALUE=realvec, scale=scale, addContribution=addContribution)

  realvec => NULL()

TYPE is (BlockNodeField_)

  p = GetNodeLoc(obj=VALUE%dof, idof=GetIDOF(obj=VALUE%dof, ivar=ivar_value, &
                                             idof=idof_value))
  realvec => VALUE%GetPointer()

  CALL obj%SetMultiple(istart=s(1), iend=s(2), stride=s(3), &
                      istart_value=p(1), iend_value=p(2), stride_value=p(3), &
                  VALUE=realvec, scale=scale, addContribution=addContribution)

  realvec => NULL()

#if USE_LIS

TYPE IS (ScalarFieldLis_)

  p = GetNodeLoc(obj=VALUE%dof, idof=1)
  tsize = obj%dof.tNodes.1

  realvec => obj%GetPointer()
  CALL lis_vector_get_values_from_range(VALUE%lis_ptr, p(1), p(3), &
                                        tsize, realvec, ierr)
  realvec => NULL()

TYPE IS (STScalarFieldLis_)

  tsize = obj%dof.tNodes.1
  p = GetNodeLoc(obj=VALUE%dof, idof=GetIDOF(obj=VALUE%dof, ivar=1_I4B, &
                                             idof=idof_value))

  realvec => obj%GetPointer()
  CALL lis_vector_get_values_from_range(VALUE%lis_ptr, p(1), p(3), &
                                        tsize, realvec, ierr)
  realvec => NULL()

TYPE IS (VectorFieldLis_)

  tsize = obj%dof.tNodes.1
  p = GetNodeLoc(obj=VALUE%dof, idof=GetIDOF(obj=VALUE%dof, ivar=1_I4B, &
                                             idof=idof_value))
  realvec => obj%GetPointer()
  CALL lis_vector_get_values_from_range(VALUE%lis_ptr, p(1), p(3), &
                                        tsize, realvec, ierr)
  realvec => NULL()

IF (.NOT. VALUE%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & 'AbstractNodeField_ ::value is not initiated')
END IF
#endif

ivar_idof(1) = ivar
ivar_idof(2) = idof

tsize = obj%dof.tNodes.ivar_idof

ivar_idof(1) = ivar_value
ivar_idof(2) = idof_value
tsize_value = VALUE%dof.tNodes.ivar_idof

IF (tsize .NE. tsize_value) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & 'tSize of obj(ivar, idof) is equal to value(ivar_value, idof_value)')
  RETURN
END IF

DO ii = 1, tsize
  indx1 = GetNodeLoc(&
    & obj=VALUE%dof, &
    & nodenum=ii, &
    & ivar=ivar_value, &
    & idof=idof_value)
  CALL VALUE%GetSingle(VALUE=avar, indx=indx1)
  indx2 = GetNodeLoc(&
    & obj=obj%dof, &
    & nodenum=ii, &
    & ivar=ivar, &
    & idof=idof)
  CALL obj%SetSingle(VALUE=avar, indx=indx2, scale=scale, &
    & addContribution=addContribution)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_Set11

!----------------------------------------------------------------------------
!                                                             SetByFunction
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetByFunction
CHARACTER(*), PARAMETER :: myName = "obj_SetByFunction()"
LOGICAL(LGT) :: istimes, problem
INTEGER(I4B) :: ttime, returnType, nsd, tnodes, ii, nrow, ncol
REAL(DFP) :: xij(3, 1)
REAL(DFP) :: args(4), VALUE
INTEGER(I4B), PARAMETER :: needed_returnType = Scalar
CLASS(Domain_), POINTER :: dom

baseInterpolation = obj%fedof%GetBaseInterpolation()

IF (UpperCase(baseInterpolation(1:3)) .NE. "LAG") THEN

  baseInterpolation = ""
  CALL e%RaiseError(modName//'::'//myName//' - '// &
 '[INTERNAL ERROR] :: This routine is only valid for Lagrange interpolation.')
  RETURN

END IF

istimes = PRESENT(times)
problem = .FALSE.

IF (istimes) THEN
  ALLOCATE (args(4))
  args = 0.0_DFP
  ttime = SIZE(times)
  args(4) = times(1)
  problem = ttime .NE. 1_I4B
ELSE
  ALLOCATE (args(3))
  args = 0.0_DFP
END IF

IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: times size should be 1.')
  RETURN
END IF

returnType = func%GetReturnType()
problem = returnType .NE. needed_returnType

IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: Return type of function is not correct.')
  RETURN
END IF

dom => NULL()
dom => obj%domain
problem = .NOT. ASSOCIATED(dom)
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: domain is not ASSOCIATED.')
  RETURN
END IF

nsd = meshptr%GetNSD()
tnodes = meshptr%GetTotalNodes()

DO ii = 1, tnodes
  globalNode = ii
  CALL dom%GetNodeCoord(globalNode=globalNode, nodeCoord=xij)
  args(1:nsd) = xij(1:nsd, 1)
  CALL func%Get(val=VALUE, args=args)
  CALL obj%Set(globalNode=globalNode(1), VALUE=VALUE)
END DO

baseInterpolation = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif
END PROCEDURE obj_SetByFunction

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods
