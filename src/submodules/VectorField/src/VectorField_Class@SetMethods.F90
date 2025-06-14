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

USE FieldOpt_Class, ONLY: TypeField => TypeFieldOpt

USE ScalarField_Class, ONLY: ScalarField_
USE ScalarFieldLis_Class, ONLY: ScalarFieldLis_

USE STScalarField_Class, ONLY: STScalarField_
USE STScalarFieldLis_Class, ONLY: STScalarFieldLis_

USE VectorFieldLis_Class, ONLY: VectorFieldLis_

USE STVectorField_Class, ONLY: STVectorField_

USE BlockNodeField_Class, ONLY: Blocknodefield_

USE RealVector_Method, ONLY: Set, Add, GetPointer

USE Display_Method, ONLY: tostring

USE GlobalData, ONLY: NONE, Scalar, Constant, Space, Vector

USE DOF_Method, ONLY: GetNodeLoc, &
                      OPERATOR(.tNodes.), &
                      GetIDOF, &
                      GetNodeLoc_, &
                      GetIndex_

USE ArangeUtility, ONLY: Arange

USE BaseType, ONLY: TypeFEVariableVector, &
                    TypeFEVariableSpace, &
                    TypeFEVariableConstant
USE FEVariable_Method, ONLY: Get

USE ReallocateUtility, ONLY: Reallocate

USE SafeSizeUtility, ONLY: SafeSize

USE StringUtility, ONLY: UpperCase

IMPLICIT NONE

INTEGER(I4B), PARAMETER :: EXPAND_FACTOR = 2

INTEGER(I4B), PARAMETER :: TEMP_INTVEC_LEN = 128
INTEGER(I4B) :: TEMP_INTVEC(TEMP_INTVEC_LEN)
!$OMP THREADPRIVATE(TEMP_INTVEC)

INTEGER(I4B), ALLOCATABLE :: TEMP_DYNA_INTVEC(:)
!$OMP THREADPRIVATE(TEMP_DYNA_INTVEC)

CONTAINS

!----------------------------------------------------------------------------
!                                                                   set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set1
#ifdef DEBUG_VER
LOGICAL(LGT) :: isok
#endif

CHARACTER(*), PARAMETER :: myName = "obj_Set1()"
INTEGER(I4B) :: ierr, tsize

#ifdef DEBUG_VER

CALL AssertError1(obj%isInitiated, myName, "STScalarField_::obj not initiated")

CALL AssertError1(obj%fieldType .NE. TypeField%constant, myName, &
                  "Not callable for constant STScalar field")

CALL AssertError2(SIZE(VALUE), obj%spaceCompo, myName, &
                  "a=SIZE(VALUE), b=obj%spaceCompo")

isok = obj%spaceCompo .LE. TEMP_INTVEC_LEN
CALL AssertError1(isok, myName, "size of TEMP_INTVEC is not enough")
#endif

#include "./localNodeError.inc"

CALL GetIndex_(obj=obj%dof, nodenum=globalNode, ans=TEMP_INTVEC, &
               tsize=tsize)

CALL obj%SetMultiple(indx=TEMP_INTVEC(1:tsize), VALUE=VALUE, scale=scale, &
                     addContribution=addContribution)
END PROCEDURE obj_Set1

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set2()"
#endif

INTEGER(I4B) :: idof, s(3)

#ifdef DEBUG_VER

CALL AssertError1(obj%isInitiated, myName, &
                  'STScalarField_::obj is not initiated')
CALL AssertError1(obj%fieldType .EQ. TypeField%constant, myName, &
                  'Not callable for constant STScalar field')

#endif

DO idof = 1, obj%spaceCompo
  s = GetNodeLoc(obj=obj%dof, idof=idof)
  CALL obj%SetMultiple(VALUE=VALUE(idof), istart=s(1), iend=s(2), &
                    stride=s(3), scale=scale, addContribution=addContribution)
END DO

END PROCEDURE obj_Set2

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set3
#ifdef DEBUG_VER

CHARACTER(*), PARAMETER :: myName = "obj_Set3()"
LOGICAL(LGT) :: isok

#endif

INTEGER(I4B) :: s(3)

#ifdef DEBUG_VER

CALL AssertError1(obj%isInitiated, myName, &
                  'STScalarField_::obj is not initiated')

CALL AssertError1(spaceCompo .LE. obj%spaceCompo, myName, &
                  'spaceComposhould be less or equal to obj%spaceCompo')

isok = obj%fieldType .NE. TypeField%constant
CALL AssertError1(isok, myName, &
                  'Not callable for constant field')
#endif

s = GetNodeLoc(obj=obj%dof, idof=spaceCompo)
CALL obj%SetMultiple(VALUE=VALUE, istart=s(1), iend=s(2), stride=s(3), &
                     scale=scale, addContribution=addContribution)

END PROCEDURE obj_Set3

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set4
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set4()"
LOGICAL(LGT) :: isok
INTEGER(I4B) :: nrow
#endif

INTEGER(I4B) :: jj, ncol

#ifdef DEBUG_VER

CALL AssertError1(obj%isInitiated, myName, &
                  'STScalarField::obj is not initiated')

CALL AssertError1(obj%fieldType .NE. TypeField%constant, myName, &
                  'Not callable for constant STScalar field')

IF (storageFMT .EQ. NODES_FMT) THEN
  nrow = obj%spaceCompo
  ncol = obj%dof.tNodes.1
ELSE
  nrow = obj%dof.tNodes.1
  ncol = obj%spaceCompo
END IF

CALL AssertError2(SIZE(VALUE, 1), nrow, myName, 'a=SIZE(VALUE, 1), b=nrow')
CALL AssertError2(SIZE(VALUE, 2), ncol, myName, 'a=SIZE(VALUE, 2), b=ncol')

#endif

IF (storageFMT .EQ. DOF_FMT) THEN
  DO jj = 1, obj%spaceCompo
    CALL obj%Set(VALUE=VALUE(:, jj), spaceCompo=jj, scale=scale, &
                 addContribution=addContribution)
  END DO
  RETURN
END IF

ncol = obj%dof.tNodes.1
!$OMP PARALLEL DO PRIVATE(jj)
DO jj = 1, ncol
  CALL obj%Set(VALUE=VALUE(:, jj), scale=scale, &
               addContribution=addContribution, globalNode=jj, islocal=.TRUE.)
END DO
!$OMP END PARALLEL DO

END PROCEDURE obj_Set4

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set5
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set5()"
INTEGER(I4B) :: tsize
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: s(3)

#ifdef DEBUG_VER

CALL AssertError1(obj%isInitiated, myName, &
                  'STScalarField_::obj is not initiated')

isok = spaceCompo .LE. obj%spaceCompo
CALL AssertError1(isok, myName, "spaceCompoout of bound")

isok = obj%fieldType .NE. TypeField%constant
CALL AssertError1(isok, myName, "Not callable for constant field")

tsize = obj%dof.tNodes.spaceCompo
CALL AssertError2(SIZE(VALUE), tsize, myName, &
                  "a=SIZE(VALUE), b=obj%dof.tNodes.spaceCompo")
#endif

s = GetNodeLoc(obj=obj%dof, idof=spaceCompo)
CALL obj%SetMultiple(VALUE=VALUE, scale=scale, istart=s(1), &
                     iend=s(2), stride=s(3), addContribution=addContribution)

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
#ifdef DEBUG_VER
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: tsize
CHARACTER(*), PARAMETER :: myName = "obj_Set9()"

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated, myName, &
                  "STScalarField_::obj not initiated")

isok = spaceCompo .LE. obj%spaceCompo
CALL AssertError1(isok, myName, "spaceCompois out of bound")

isok = obj%fieldType .NE. TypeField%constant
CALL AssertError1(isok, myName, &
                  'Not callable for constant STScalar field')

CALL AssertError2(SIZE(VALUE), SIZE(globalNode), &
                  myName, 'a=SIZE(VALUE), b=size(globalNode)')

#endif

#include "./localNodeError.inc"

tsize = SIZE(globalNode)

IF (tsize .LE. TEMP_INTVEC_LEN) THEN
  CALL GetNodeLoc_(obj=obj%dof, idof=spaceCompo, &
                   nodenum=globalNode, ans=TEMP_INTVEC, tsize=tsize)

  CALL obj%SetMultiple(indx=TEMP_INTVEC(1:tsize), VALUE=VALUE, scale=scale, &
                       addContribution=addContribution)

  RETURN

END IF

IF (tsize .GT. SafeSize(TEMP_DYNA_INTVEC)) THEN
  CALL Reallocate(TEMP_DYNA_INTVEC, EXPAND_FACTOR * tsize)
END IF

CALL GetNodeLoc_(obj=obj%dof, idof=spaceCompo, nodenum=globalNode, &
                 ans=TEMP_DYNA_INTVEC, tsize=tsize)

CALL obj%SetMultiple(indx=TEMP_DYNA_INTVEC(1:tsize), VALUE=VALUE, &
                     scale=scale, addContribution=addContribution)

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
CHARACTER(*), PARAMETER :: myName = "obj_Set13()"
INTEGER(I4B) :: s(3), p(3)
REAL(DFP), POINTER :: realvec(:)

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated, myName, &
                  'VectorField_::obj is not initiated')

CALL AssertError1(VALUE%isInitiated, myName, &
                  'AbstractNodeField_::value is not initiated')
#endif

s = GetNodeLoc(obj=obj%dof, idof=idof)

SELECT TYPE (VALUE)

TYPE IS (ScalarField_)

  realvec => VALUE%GetPointer()
  CALL obj%SetMultiple(VALUE=realvec, scale=scale, addContribution=addContribution, &
                       istart=s(1), iend=s(2), stride=s(3))
  realvec => NULL()

TYPE IS (STScalarField_)

  p = GetNodeLoc(obj=VALUE%dof, idof=idof_value)

  realvec => VALUE%GetPointer()
  CALL obj%SetMultiple(VALUE=realvec, scale=scale, addContribution=addContribution, &
                       istart=s(1), iend=s(2), stride=s(3), &
                       istart_value=p(1), iend_value=p(2), stride_value=p(3))
  realvec => NULL()

TYPE IS (VectorField_)

  p = GetNodeLoc(obj=VALUE%dof, idof=idof_value)

  realvec => VALUE%GetPointer()
  CALL obj%SetMultiple(VALUE=realvec, scale=scale, addContribution=addContribution, &
                       istart=s(1), iend=s(2), stride=s(3), &
                       istart_value=p(1), iend_value=p(2), stride_value=p(3))
  realvec => NULL()

TYPE IS (STVectorField_)

  p = GetNodeLoc(obj=VALUE%dof, idof=idof_value)

  realvec => VALUE%GetPointer()
  CALL obj%SetMultiple(VALUE=realvec, scale=scale, addContribution=addContribution, &
                       istart=s(1), iend=s(2), stride=s(3), &
                       istart_value=p(1), iend_value=p(2), stride_value=p(3))
  realvec => NULL()

TYPE IS (BlockNodeField_)

  p = GetNodeLoc(obj=VALUE%dof, idof=idof_value)

  realvec => VALUE%GetPointer()
  CALL obj%SetMultiple(VALUE=realvec, scale=scale, addContribution=addContribution, &
                       istart=s(1), iend=s(2), stride=s(3), &
                       istart_value=p(1), iend_value=p(2), stride_value=p(3))
  realvec => NULL()

! TYPE IS (ScalarFieldLis_)

! TYPE IS (STScalarFieldLis_)
!
! TYPE IS (VectorFieldLis_)

CLASS DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: No case found for the type of value')
  RETURN
END SELECT

END PROCEDURE obj_Set13

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set14
CHARACTER(*), PARAMETER :: myName = "obj_Set14()"
INTEGER(I4B) :: tsize
REAL(DFP), POINTER :: realvec(:)

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated, myName, &
                  "STScalarFieldLis_::obj is not initiated")
#endif

SELECT TYPE (VALUE)

TYPE IS (VectorField_)
  realvec => VALUE%GetPointer()
  tsize = SIZE(realvec)
  CALL obj%SetMultiple(VALUE=realvec, istart=1_I4B, iend=tsize, &
                       stride=1_I4B)
  realvec => NULL()

! TYPE is (STScalarFieldLis_)
!
!   DO ierr = 1, obj%timeCompo
!     CALL VALUE%Get(VALUE=obj, timeCompo=ierr)
!   END DO

CLASS DEFAULT

  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: Unknown type of ScalarField_::value')
  RETURN

END SELECT

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
!     jj = GetIDOF(spaceCompo=ii,spaceCompo=spaceCompo)
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
INTEGER(I4B) :: ttime, returnType, nsd, tnodes, ii, globalNode(1), nrow, &
                ncol
REAL(DFP) :: args(4), xij(3, 1)
REAL(DFP), ALLOCATABLE :: VALUE(:)
INTEGER(I4B), PARAMETER :: needed_returnType = Vector
CLASS(AbstractMesh_), POINTER :: meshptr
CHARACTER(:), ALLOCATABLE :: baseInterpolation

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

baseInterpolation = obj%fedof%GetBaseInterpolation()

IF (UpperCase(baseInterpolation(1:3)) .NE. "LAG") THEN

  baseInterpolation = ""
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: This routine is only valid '// &
                    'for Lagrange interpolation.')
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
                            islocal=.TRUE., nrow=nrow, ncol=ncol)

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
