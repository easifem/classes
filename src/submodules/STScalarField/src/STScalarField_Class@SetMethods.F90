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
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE ArangeUtility, ONLY: Arange
USE BaseType, ONLY: TypeFEVariableScalar
USE BaseType, ONLY: TypeFEVariableSpaceTime
USE Display_Method, ONLY: tostring
USE DOF_Method, ONLY: GetIDOF
USE DOF_Method, ONLY: GetIndex_
USE DOF_Method, ONLY: GetNodeLoc
USE DOF_Method, ONLY: GetNodeLoc_
USE DOF_Method, ONLY: OPERATOR(.tNodes.)
USE FEVariable_Method, ONLY: Get
USE FieldOpt_Class, ONLY: TypeField => TypeFieldOpt
USE FieldOpt_Class, ONLY: TypeFieldOpt
USE GlobalData, ONLY: NONE, Scalar, SpaceTime
USE InputUtility, ONLY: Input
USE ReallocateUtility, ONLY: Reallocate
USE RealVector_Method, ONLY: Add, GetPointer, Set
USE SafeSizeUtility, ONLY: SafeSize
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "STScalarField_Class@SetMethods.F90"
#endif

INTEGER(I4B), PARAMETER :: EXPAND_FACTOR = 2
INTEGER(I4B), PARAMETER :: TEMP_INTVEC_LEN = 128
INTEGER(I4B) :: TEMP_INTVEC(TEMP_INTVEC_LEN)
!$OMP THREADPRIVATE(TEMP_INTVEC)

INTEGER(I4B), ALLOCATABLE :: TEMP_DYNA_INTVEC(:)
!$OMP THREADPRIVATE(TEMP_DYNA_INTVEC)

CONTAINS

!----------------------------------------------------------------------------
!                                                                       Set
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

#include "./include/localNodeError.F90"

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
isok = obj%fieldType .NE. TypeField%constant
CALL AssertError1(isok, myName, &
                  'Not callable for constant STScalar field')
#endif

DO idof = 1, obj%timeCompo
  s = GetNodeLoc(obj=obj%dof, idof=idof)
  CALL obj%SetMultiple(VALUE=VALUE(idof), istart=s(1), iend=s(2), &
                       stride=s(3), scale=scale, &
                       addContribution=addContribution)
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
  ncol = obj%dof .tNodes. 1
ELSE
  nrow = obj%dof .tNodes. 1
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

ncol = obj%dof .tNodes. 1
DO jj = 1, ncol
  CALL obj%Set(VALUE=VALUE(:, jj), scale=scale, &
               addContribution=addContribution, &
               globalNode=jj, islocal=math%yes)
END DO

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

tsize = obj%dof .tNodes. timeCompo
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
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: s(3), vs(3)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = obj%IsInitiated()
CALL AssertError1(isok, myName, &
                  'STScalarField_::obj is not initiated')

isok = VALUE%IsInitiated()
CALL AssertError1(isok, myName, &
                  'STScalarField_::value is not initiated')

isok = obj%fieldType .NE. TypeField%constant
CALL AssertError1(isok, myName, &
                  'Not callable for constant STScalar field')

isok = (objTimeCompo .GE. 1) .AND. (objTimeCompo .LE. obj%timeCompo)
CALL AssertError1(isok, myName, &
                  'objTimeCompo should be in [1, obj%timeCompo]')

isok = (valueTimeCompo .GE. 1) .AND. &
       (valueTimeCompo .LE. VALUE%timeCompo)
CALL AssertError1(isok, myName, &
                  'valueTimeCompo should be in [1, value%timeCompo]')
#endif

s = GetNodeLoc(obj=obj%dof, idof=objTimeCompo)
vs = GetNodeLoc(obj=VALUE%dof, idof=valueTimeCompo)

#ifdef DEBUG_VER
CALL AssertError2((s(2) - s(1)) / s(3) + 1, (vs(2) - vs(1)) / vs(3) + 1, &
                  myName, 'a=size of obj time component, '// &
                  'b=size of value time component')
#endif

! the engines of obj and VALUE copy the values, see SetMultiple5
CALL obj%SetMultiple( &
  istart=s(1), iend=s(2), stride=s(3), VALUE=VALUE, istart_value=vs(1), &
  iend_value=vs(2), stride_value=vs(3), scale=scale, &
  addContribution=addContribution)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set6

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set7
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set7()"
#endif
INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

DO ii = 1, SIZE(globalNode)
  CALL obj%Set(VALUE=VALUE, globalNode=globalNode(ii), &
               islocal=islocal, scale=scale, addContribution=addContribution)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
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
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

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

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

DO ii = 1, obj%timeCompo
  CALL obj%Set(VALUE=VALUE(:, ii), globalNode=globalNode, &
               scale=scale, addContribution=addContribution, &
               islocal=islocal, timeCompo=ii)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set8

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set9
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set9()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

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

#include "./include/localNodeError.F90"

tsize = SIZE(globalNode)

IF (tsize .LE. TEMP_INTVEC_LEN) THEN
  CALL GetNodeLoc_(obj=obj%dof, idof=timeCompo, &
                   nodenum=globalNode, ans=TEMP_INTVEC, tsize=tsize)

  CALL obj%SetMultiple(indx=TEMP_INTVEC(1:tsize), VALUE=VALUE, scale=scale, &
                       addContribution=addContribution)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

IF (tsize .GT. SafeSize(TEMP_DYNA_INTVEC)) THEN
  CALL Reallocate(TEMP_DYNA_INTVEC, EXPAND_FACTOR * tsize)
END IF

CALL GetNodeLoc_(obj=obj%dof, idof=timeCompo, nodenum=globalNode, &
                 ans=TEMP_DYNA_INTVEC, tsize=tsize)

CALL obj%SetMultiple(indx=TEMP_DYNA_INTVEC(1:tsize), VALUE=VALUE, &
                     scale=scale, addContribution=addContribution)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set9

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set10
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set10()"
LOGICAL(LGT) :: isok
#endif
INTEGER(I4B) :: indx

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated(), myName, &
                  'STScalarField_::obj is not initiated')

isok = timeCompo .LE. obj%timeCompo
CALL AssertError1(isok, myName, "timeCompo is out of bound")

isok = obj%fieldType .NE. TypeField%constant
CALL AssertError1(isok, myName, 'Not callable for constant STScalar field')
#endif

#include "./include/localNodeError.F90"

indx = GetNodeLoc(obj=obj%dof, idof=timeCompo, nodenum=globalNode)

CALL obj%SetSingle(indx=indx, VALUE=VALUE, scale=scale, &
                   addContribution=addContribution)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set10

!----------------------------------------------------------------------------
!                                                                         Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set11
#ifdef DEBUG_VER
LOGICAL(LGT) :: isok
CHARACTER(*), PARAMETER :: myName = "obj_Set11()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated(), myName, &
                  'STScalarField_::obj is not initiated')

isok = obj%fieldType .NE. TypeField%constant
CALL AssertError1(isok, myName, &
                  'Not callable for constant STScalar field')
#endif

#include "./include/localNodeError.F90"

SELECT CASE (VALUE%vartype)

CASE (SpaceTime)
  CALL obj%Set(VALUE=GET(VALUE, TypeFEVariableScalar, &
                         TypeFEVariableSpaceTime), &
               globalNode=globalNode, scale=scale, &
               addContribution=addContribution, islocal=islocal, &
               storageFMT=TypeField%storageFormatDOF)

CASE DEFAULT
#ifdef DEBUG_VER
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: No case found for Value%vartype')
#endif
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set11

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set12
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set12()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%SetAll(VALUE=VALUE, scale=scale, addContribution=addContribution)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
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
!                                                         SetFromScalarField
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetFromScalarField
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetFromScalarField()"
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
isok = VALUE%IsInitiated()
CALL AssertError1(isok, myName, &
                  'ScalarField_::value is not initiated')
#endif

s = GetNodeLoc(obj=obj%dof, idof=timeCompo)

! the engines of obj and VALUE copy the values, see SetMultiple5
CALL obj%SetMultiple( &
  istart=s(1), iend=s(2), stride=s(3), VALUE=VALUE, istart_value=1, &
  iend_value=VALUE%SIZE(), stride_value=1, scale=scale, &
  addContribution=addContribution)

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

! the engines of VALUE and obj copy the values, see SetMultiple5
CALL VALUE%SetMultiple( &
  istart=s(1), iend=s(2), stride=s(3), VALUE=obj, istart_value=p(1), &
  iend_value=p(2), stride_value=p(3), scale=scale, &
  addContribution=addContribution)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetToScalarField

!----------------------------------------------------------------------------
!                                                               SetByFunction
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetByFunction
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetByFunction()"
#endif
LOGICAL(LGT) :: istimes, isok
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

#ifdef DEBUG_VER
isok = baseInterpolation .EQ. "LAGR"
CALL AssertError1(isok, myName, &
                  'This routine is only valid for Lagrange interpolation.')
#endif

istimes = PRESENT(times)
isok = math%yes

args = math%zero
IF (istimes) THEN
  ttime = SIZE(times)
  isok = ttime .EQ. obj%timeCompo
END IF

#ifdef DEBUG_VER
CALL AssertError1(isok, myName, &
                  'size of times should be obj%timeCompo='// &
                  tostring(obj%timeCompo))
#endif

meshptr => NULL()
meshptr => obj%fedof%GetMeshPointer()

#ifdef DEBUG_VER
isok = ASSOCIATED(meshptr)
CALL AssertError1(isok, myName, &
                  "meshptr is not associated.")
#endif

nsd = meshptr%GetNSD()
tnodes = meshptr%GetTotalNodes()

IF (istimes) THEN

  DO ii = 1, tnodes
    i1(1) = ii
    CALL meshptr%GetNodeCoord(globalNode=i1, nodeCoord=xij, &
                              islocal=math%yes, nrow=nrow, ncol=ncol)

    args(1:nsd) = xij(1:nsd, 1)

    DO itime = 1, obj%timeCompo
      args(4) = times(itime)
      CALL func%Get(val=VALUE(itime), args=args)
    END DO

    CALL obj%Set(globalNode=ii, VALUE=VALUE, islocal=math%yes)

  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

DO ii = 1, tnodes
  i1(1) = ii
  CALL meshptr%GetNodeCoord(globalNode=i1, nodeCoord=xij, &
                            islocal=math%yes, nrow=nrow, ncol=ncol)
  args(1:nsd) = xij(1:nsd, 1)
  CALL func%Get(val=aval, args=args)
  VALUE = aval
  CALL obj%Set(globalNode=ii, VALUE=VALUE, islocal=math%yes)
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
