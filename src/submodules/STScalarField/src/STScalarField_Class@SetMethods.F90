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
USE FieldOpt_Class, ONLY: TypeField => TypeFieldOpt
! USE ScalarField_Class, ONLY: ScalarField_
! USE VectorField_Class, ONLY: VectorField_
! USE STScalarFieldLis_Class, ONLY: STScalarFieldLis_
USE RealVector_Method, ONLY: Set, Add, GetPointer
USE Display_Method, ONLY: tostring
USE GlobalData, ONLY: NONE, SpaceTime, Scalar
USE DOF_Method, ONLY: GetNodeLoc
USE DOF_Method, ONLY: OPERATOR(.tNodes.)
USE DOF_Method, ONLY: GetIDOF
USE DOF_Method, ONLY: GetIndex_
USE DOF_Method, ONLY: GetNodeLoc_
USE ArangeUtility, ONLY: Arange
USE BaseType, ONLY: TypeFEVariableScalar
USE BaseType, ONLY: TypeFEVariableSpaceTime
USE FEVariable_Method, ONLY: Get
USE ReallocateUtility, ONLY: Reallocate
USE SafeSizeUtility, ONLY: SafeSize
USE FieldOpt_Class, ONLY: TypeFieldOpt
USE BaseType, ONLY: math => TypeMathOpt

IMPLICIT NONE

INTEGER(I4B), PARAMETER :: EXPAND_FACTOR = 2
INTEGER(I4B), PARAMETER :: TEMP_INTVEC_LEN = 128
INTEGER(I4B) :: TEMP_INTVEC(TEMP_INTVEC_LEN)
!$OMP THREADPRIVATE(TEMP_INTVEC)

INTEGER(I4B), ALLOCATABLE :: TEMP_DYNA_INTVEC(:)
!$OMP THREADPRIVATE(TEMP_DYNA_INTVEC)

CONTAINS

!----------------------------------------------------------------------------
!                                                                   Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set1()"
LOGICAL(LGT) :: isok
INTEGER(I4B) :: myint1, myint2
#endif

INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = obj%IsInitiated()
CALL AssertError1(isok, myName, "STScalarField_::obj not initiated")
#endif

#ifdef DEBUG_VER
isok = obj%fieldType .NE. TypeField%constant
CALL AssertError1(isok, myName, &
                  "Not callable for constant STScalar field")
#endif

#ifdef DEBUG_VER
myint1 = SIZE(VALUE)
myint2 = obj%timeCompo
CALL AssertError2(myint1, myint2, myName, &
                  "a=SIZE(VALUE), b=obj%timeCompo")
#endif

#ifdef DEBUG_VER
isok = obj%timeCompo .LE. TEMP_INTVEC_LEN
CALL AssertError1(isok, myName, "size of TEMP_INTVEC is not enough")
#endif

#include "./localNodeError.inc"

CALL GetIndex_(obj=obj%dof, nodenum=globalNode, ans=TEMP_INTVEC, &
               tsize=tsize)

CALL obj%SetMultiple(indx=TEMP_INTVEC(1:tsize), VALUE=VALUE, scale=scale, &
                     addContribution=addContribution)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set1

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set2()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: idof, s(3)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = obj%IsInitiated()
CALL AssertError1(isok, myName, &
                  'STScalarField_::obj is not initiated')
#endif

#ifdef DEBUG_VER
isok = obj%fieldType .EQ. TypeField%constant
CALL AssertError1(isok, myName, &
                  'Not callable for constant STScalar field')
#endif

DO idof = 1, obj%timeCompo
  s = GetNodeLoc(obj=obj%dof, idof=idof)
  CALL obj%SetMultiple(VALUE=VALUE(idof), istart=s(1), iend=s(2), &
                    stride=s(3), scale=scale, addContribution=addContribution)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
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
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = obj%IsInitiated()
CALL AssertError1(isok, myName, &
                  'STScalarField_::obj is not initiated')
#endif

#ifdef DEBUG_VER
isok = timeCompo .LE. obj%timeCompo
CALL AssertError1(isok, myName, &
                  'timeCompo should be less or equal to obj%timeCompo')
#endif

#ifdef DEBUG_VER
isok = obj%fieldType .NE. TypeField%constant
CALL AssertError1(isok, myName, &
                  'Not callable for constant field')
#endif

s = GetNodeLoc(obj=obj%dof, idof=timeCompo)
CALL obj%SetMultiple(VALUE=VALUE, istart=s(1), iend=s(2), stride=s(3), &
                     scale=scale, addContribution=addContribution)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set3

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set4
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set4()"
LOGICAL(LGT) :: isok
INTEGER(I4B) :: nrow, myint1, myint2
#endif

INTEGER(I4B) :: jj, ncol

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = obj%IsInitiated()
CALL AssertError1(isok, myName, &
                  'STScalarField::obj is not initiated')
#endif

#ifdef DEBUG_VER
isok = obj%fieldType .NE. TypeField%constant
CALL AssertError1(isok, myName, &
                  'Not callable for constant STScalar field')
#endif

#ifdef DEBUG_VER
IF (storageFMT .EQ. TypeFieldOpt%storageFormatNodes) THEN
  nrow = obj%timeCompo
  ncol = obj%dof.tNodes.1
ELSE
  nrow = obj%dof.tNodes.1
  ncol = obj%timeCompo
END IF

myint1 = SIZE(VALUE, 1)
myint2 = SIZE(VALUE, 2)

CALL AssertError2(myint1, nrow, myName, 'a=SIZE(VALUE, 1), b=nrow')
CALL AssertError2(myint2, ncol, myName, 'a=SIZE(VALUE, 2), b=ncol')
#endif

IF (storageFMT .EQ. TypeFieldOpt%storageFormatDOF) THEN
  DO jj = 1, obj%timeCompo
    CALL obj%Set(VALUE=VALUE(:, jj), timeCompo=jj, scale=scale, &
                 addContribution=addContribution)
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

ncol = obj%dof.tNodes.1
!$OMP PARALLEL DO PRIVATE(jj)
DO jj = 1, ncol
  CALL obj%Set(VALUE=VALUE(:, jj), scale=scale, &
               addContribution=addContribution, globalNode=jj, islocal=.TRUE.)
END DO
!$OMP END PARALLEL DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set4

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set5
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set5()"
INTEGER(I4B) :: tsize
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: s(3)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER

CALL AssertError1(obj%IsInitiated(), myName, &
                  'STScalarField_::obj is not initiated')

isok = timeCompo .LE. obj%timeCompo
CALL AssertError1(isok, myName, "timeCompo out of bound")

isok = obj%fieldType .NE. TypeField%constant
CALL AssertError1(isok, myName, "Not callable for constant field")

tsize = obj%dof.tNodes.timeCompo
CALL AssertError2(SIZE(VALUE), tsize, myName, &
                  "a=SIZE(VALUE), b=obj%dof.tNodes.timeCompo")
#endif

s = GetNodeLoc(obj=obj%dof, idof=timeCompo)
CALL obj%SetMultiple(VALUE=VALUE, scale=scale, istart=s(1), &
                     iend=s(2), stride=s(3), addContribution=addContribution)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set5

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set6
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set6()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
#endif

! CALL obj%Set(ivar=1, idof=timeCompo, VALUE=VALUE, ivar_value=1_I4B, &
!            idof_value=timeCompo, scale=scale, addContribution=addContribution)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set6

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set7
INTEGER(I4B) :: ii

!$OMP PARALLEL DO PRIVATE(ii)
DO ii = 1, SIZE(globalNode)
  CALL obj%Set(VALUE=VALUE, globalNode=globalNode(ii), &
               islocal=islocal, scale=scale, addContribution=addContribution)
END DO
!$OMP END PARALLEL DO

END PROCEDURE obj_Set7

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set8
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set8()"
INTEGER(I4B) :: nrow, ncol
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: ii

#ifdef DEBUG_VER

CALL AssertError1(obj%isInitiated(), myName, &
                  'STSCalarField_::obj is not initiated')

isok = obj%fieldType .NE. TypeField%constant
CALL AssertError1(isok, myName, &
                  'Not callable for constant STScalar field')

IF (storageFMT .EQ. NODES_FMT) THEN
  nrow = obj%timeCompo
  ncol = SIZE(globalNode)
ELSE
  nrow = SIZE(globalNode)
  ncol = obj%timeCompo
END IF

CALL AssertError2(SIZE(VALUE, 1), nrow, myName, &
                  'a=SIZE(VALUE, 1), b=nrow')

CALL AssertError2(SIZE(VALUE, 2), ncol, myName, &
                  'a=SIZE(VALUE, 2), b=ncol')
#endif

IF (storageFMT .EQ. NODES_FMT) THEN
  DO ii = 1, SIZE(globalNode)
    CALL obj%Set(VALUE=VALUE(:, ii), globalNode=globalNode(ii), &
                 scale=scale, addContribution=addContribution, &
                 islocal=islocal)
  END DO
  RETURN
END IF

DO ii = 1, obj%timeCompo
  CALL obj%Set(VALUE=VALUE(:, ii), globalNode=globalNode, &
               scale=scale, addContribution=addContribution, &
               islocal=islocal, timeCompo=ii)
END DO

END PROCEDURE obj_Set8

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set9
#ifdef DEBUG_VER
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: tsize
CHARACTER(*), PARAMETER :: myName = "obj_Set9()"

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated(), myName, &
                  "STScalarField_::obj not initiated")

isok = timeCompo .LE. obj%timeCompo
CALL AssertError1(isok, myName, "timeCompo is out of bound")

isok = obj%fieldType .NE. TypeField%constant
CALL AssertError1(isok, myName, &
                  'Not callable for constant STScalar field')

CALL AssertError2(SIZE(VALUE), SIZE(globalNode), &
                  myName, 'a=SIZE(VALUE), b=size(globalNode)')

#endif

#include "./localNodeError.inc"

tsize = SIZE(globalNode)

IF (tsize .LE. TEMP_INTVEC_LEN) THEN
  CALL GetNodeLoc_(obj=obj%dof, idof=timeCompo, &
                   nodenum=globalNode, ans=TEMP_INTVEC, tsize=tsize)

  CALL obj%SetMultiple(indx=TEMP_INTVEC(1:tsize), VALUE=VALUE, scale=scale, &
                       addContribution=addContribution)

  RETURN

END IF

IF (tsize .GT. SafeSize(TEMP_DYNA_INTVEC)) THEN
  CALL Reallocate(TEMP_DYNA_INTVEC, EXPAND_FACTOR * tsize)
END IF

CALL GetNodeLoc_(obj=obj%dof, idof=timeCompo, nodenum=globalNode, &
                 ans=TEMP_DYNA_INTVEC, tsize=tsize)

CALL obj%SetMultiple(indx=TEMP_DYNA_INTVEC(1:tsize), VALUE=VALUE, &
                     scale=scale, addContribution=addContribution)

END PROCEDURE obj_Set9

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set10
#ifdef DEBUG_VER
LOGICAL(LGT) :: isok
#endif

CHARACTER(*), PARAMETER :: myName = "obj_Set10()"
INTEGER(I4B) :: indx

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated(), myName, &
                  'STScalarField_::obj is not initiated')

isok = timeCompo .LE. obj%timeCompo
CALL AssertError1(isok, myName, "timeCompo is out of bound")

isok = obj%fieldType .NE. TypeField%constant
CALL AssertError1(isok, myName, 'Not callable for constant STScalar field')
#endif

#include "./localNodeError.inc"

indx = GetNodeLoc(obj=obj%dof, idof=timeCompo, nodenum=globalNode)

CALL obj%SetSingle(indx=indx, VALUE=VALUE, scale=scale, &
                   addContribution=addContribution)

END PROCEDURE obj_Set10

!----------------------------------------------------------------------------
!                                                                         Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set11
#ifdef DEBUG_VER
LOGICAL(LGT) :: isok
#endif

CHARACTER(*), PARAMETER :: myName = "obj_Set11()"

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated(), myName, &
                  'STScalarField_::obj is not initiated')

isok = obj%fieldType .NE. TypeField%constant
CALL AssertError1(isok, myName, &
                  'Not callable for constant STScalar field')
#endif

#include "./localNodeError.inc"

SELECT CASE (VALUE%vartype); CASE (SpaceTime)

  CALL obj%Set(VALUE=GET(VALUE, TypeFEVariableScalar, &
                         TypeFEVariableSpaceTime), &
               globalNode=globalNode, scale=scale, &
               addContribution=addContribution, islocal=islocal, &
               storageFMT=NODES_FMT)

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
CALL obj%SetAll(VALUE=VALUE, scale=scale, addContribution=addContribution)
END PROCEDURE obj_Set12

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set13
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set13()"
LOGICAL(LGT) :: isok
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = obj%IsInitiated()
CALL AssertError1(isok, myName, &
                  "STScalarField_::obj is not initiated")
#endif

#ifdef DEBUG_VER
isok = VALUE%IsInitiated()
CALL AssertError1(isok, myName, &
                  "STScalarField_::value is not initiated")
#endif

CALL obj%Copy(VALUE)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set13

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

! MODULE PROCEDURE obj_Set13
! #ifdef DEBUG_VER
! CHARACTER(*), PARAMETER :: myName = "obj_Set13()"
! LOGICAL(LGT) :: isok
! #endif
!
! INTEGER(I4B) :: s(3), p(3)
!
! REAL(DFP), POINTER :: realvec(:)
!
! #ifdef DEBUG_VER
! CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!                         '[START] ')
! #endif
!
! #ifdef DEBUG_VER
! isok = obj%IsInitiated()
! CALL AssertError1(isok, myName, &
!                   'STScalarField_::obj is not initiated')
! #endif
!
! #ifdef DEBUG_VER
! isok = VALUE%IsInitiated()
! CALL AssertError1(isok, myName, &
!                   'AbstractNodeField_::value is not initiated')
! #endif
!
! s = GetNodeLoc(obj=obj%dof, idof=idof)
!
! SELECT TYPE (VALUE)
!
! TYPE IS (ScalarField_)
!
!   realvec => VALUE%GetPointer()
!
!   CALL obj%SetMultiple( &
!     VALUE=realvec, scale=scale, addContribution=addContribution, &
!     istart=s(1), iend=s(2), stride=s(3))
!
!   realvec => NULL()
!
! TYPE IS (STScalarField_)
!
!   p = GetNodeLoc(obj=VALUE%dof, idof=idof_value)
!
!   realvec => VALUE%GetPointer()
!   CALL obj%SetMultiple( &
!     VALUE=realvec, scale=scale, addContribution=addContribution, &
!     istart=s(1), iend=s(2), stride=s(3), &
!     istart_value=p(1), iend_value=p(2), stride_value=p(3))
!   realvec => NULL()
!
! TYPE IS (VectorField_)
!
!   p = GetNodeLoc(obj=VALUE%dof, idof=idof_value)
!
!   realvec => VALUE%GetPointer()
!   CALL obj%SetMultiple( &
!     VALUE=realvec, scale=scale, addContribution=addContribution, &
!     istart=s(1), iend=s(2), stride=s(3), &
!     istart_value=p(1), iend_value=p(2), stride_value=p(3))
!   realvec => NULL()
!
! #ifdef DEBUG_VER
! CLASS DEFAULT
!   CALL AssertError1(.FALSE., myName, &
!                     'Unknown or unsupported type of VALUE ')
! #endif
!
! END SELECT
!
! #ifdef DEBUG_VER
! CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!                         '[END] ')
! #endif
! END PROCEDURE obj_Set13

!----------------------------------------------------------------------------
!                                                         SetFromScalarField
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetFromScalarField
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetFromScalarField()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: s(3)
REAL(DFP), POINTER :: realvec(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = obj%IsInitiated()
CALL AssertError1(isok, myName, &
                  'STScalarField_::obj is not initiated')
#endif

#ifdef DEBUG_VER
isok = VALUE%IsInitiated()
CALL AssertError1(isok, myName, &
                  'ScalarField_::value is not initiated')
#endif

s = GetNodeLoc(obj=obj%dof, idof=timeCompo)

realvec => VALUE%GetPointer()

#ifdef DEBUG_VER
isok = ASSOCIATED(realvec)
CALL AssertError1(isok, myName, &
                  'realvec obtained from value is not ASSOCIATED')
#endif

CALL obj%SetMultiple( &
  VALUE=realvec, scale=scale, addContribution=addContribution, &
  istart=s(1), iend=s(2), stride=s(3))

realvec => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetFromScalarField

!----------------------------------------------------------------------------
!                                                           SetToScalarField
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetToScalarField
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetToScalarField()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: s(3), p(3)
REAL(DFP), POINTER :: realvec(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = obj%IsInitiated()
CALL AssertError1(isok, myName, &
                  'STScalarField_::obj is not initiated')
#endif

#ifdef DEBUG_VER
isok = VALUE%IsInitiated()
CALL AssertError1(isok, myName, &
                  'ScalarField_::value is not initiated')
#endif

s = GetNodeLoc(obj=VALUE%dof, idof=math%one_i)
p = GetNodeLoc(obj=obj%dof, idof=timeCompo)

realvec => obj%GetPointer()

#ifdef DEBUG_VER
isok = ASSOCIATED(realvec)
CALL AssertError1(isok, myName, &
                  'realvec obtained To value is not ASSOCIATED')
#endif

CALL VALUE%SetMultiple( &
  VALUE=realvec, scale=scale, addContribution=addContribution, &
  istart_value=p(1), iend_value=p(2), stride_value=p(3), &
  istart=s(1), iend=s(2), stride=s(3))

realvec => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetToScalarField

!----------------------------------------------------------------------------
!                                                               SetByFunction
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetByFunction
CHARACTER(*), PARAMETER :: myName = "obj_SetByFunction()"
LOGICAL(LGT) :: istimes, problem
INTEGER(I4B) :: ttime, nsd, tnodes, ii, itime, i1(1), nrow, ncol
REAL(DFP) :: args(4), VALUE(obj%timeCompo), aval, xij(3, 1)
! INTEGER(I4B), PARAMETER :: needed_returnType = Scalar
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

IF (istimes) THEN

  DO ii = 1, tnodes
    i1(1) = ii
    CALL meshptr%GetNodeCoord(globalNode=i1, nodeCoord=xij, &
                              islocal=.TRUE., nrow=nrow, ncol=ncol)

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
                            islocal=.TRUE., nrow=nrow, ncol=ncol)
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

#include "../../include/errors.F90"
END SUBMODULE SetMethods
