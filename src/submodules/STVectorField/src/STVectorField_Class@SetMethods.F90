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

SUBMODULE(STVectorField_Class) SetMethods
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE AbstractNodeField_Class, ONLY: AbstractNodeFieldGetPointer
USE ArangeUtility, ONLY: Arange
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: TypeFEVariableConstant
USE BaseType, ONLY: TypeFEVariableSpaceTime
USE BaseType, ONLY: TypeFEVariableVector
USE Display_Method, ONLY: tostring
USE DOF_Method, ONLY: GetIDOF
USE DOF_Method, ONLY: GetIndex_
USE DOF_Method, ONLY: GetNodeLoc
USE DOF_Method, ONLY: GetNodeLoc_
USE DOF_Method, ONLY: OPERATOR(.tNodes.)
USE FEVariable_Method, ONLY: Get
USE FieldOpt_Class, ONLY: TypeField => TypeFieldOpt
USE GlobalData, ONLY: SpaceTime
USE InputUtility, ONLY: Input
USE ReallocateUtility, ONLY: Reallocate
USE RealVector_Method, ONLY: Set, Add, GetPointer
USE SafeSizeUtility, ONLY: SafeSize
USE ScalarField_Class, ONLY: ScalarField_
USE ScalarFieldLis_Class, ONLY: ScalarFieldLis_
USE STScalarField_Class, ONLY: STScalarField_
USE STScalarFieldLis_Class, ONLY: STScalarFieldLis_
USE STVectorFieldLis_Class, ONLY: STVectorFieldLis_
USE VectorField_Class, ONLY: VectorField_
USE VectorFieldLis_Class, ONLY: VectorFieldLis_
IMPLICIT NONE

INTEGER(I4B), PARAMETER :: EXPAND_FACTOR = 2
INTEGER(I4B), PARAMETER :: TEMP_INTVEC_LEN = 128
INTEGER(I4B) :: TEMP_INTVEC(TEMP_INTVEC_LEN)
!$OMP THREADPRIVATE(TEMP_INTVEC)

INTEGER(I4B), ALLOCATABLE :: TEMP_DYNA_INTVEC(:)
!$OMP THREADPRIVATE(TEMP_DYNA_INTVEC)
!
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "STVectorField_Class@SetMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                   set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set1()"
#endif
INTEGER(I4B) :: ii, tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated(), myName, &
                  'STVectorField_::obj is not initiated')
#endif

#ifdef DEBUG_VER
CALL AssertError2(SIZE(VALUE, 1), obj%spaceCompo, myName, &
                  "a=size(value, 1) b=obj%spaceCompo")
#endif

#ifdef DEBUG_VER
CALL AssertError2(SIZE(VALUE, 2), obj%timeCompo, myName, &
                  "a=size(value, 2) b=obj%timeCompo")
#endif

#include "./localNodeError.F90"

DO ii = 1, obj%timeCompo
  CALL GetNodeLoc_( &
    obj=obj%dof, nodenum=globalNode, ans=TEMP_INTVEC, &
    tsize=tsize, timeCompo=ii, spaceCompo=obj%space_idofs, ivar=1)

  CALL obj%SetMultiple( &
    VALUE=VALUE(:, ii), indx=TEMP_INTVEC(1:tsize), &
    scale=scale, addContribution=addContribution)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set1

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set2()"
#endif

INTEGER(I4B) :: ii, jj, s(3)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated(), myName, &
                  'STVectorField_::obj is not initiated')
#endif

#ifdef DEBUG_VER
CALL AssertError2(SIZE(VALUE, 1), obj%spaceCompo, myName, &
                  "a=size(value, 1) b=obj%spaceCompo")
#endif

#ifdef DEBUG_VER
CALL AssertError2(SIZE(VALUE, 2), obj%timeCompo, myName, &
                  "a=size(value, 2) b=obj%timeCompo")
#endif

DO jj = 1, obj%timeCompo
  DO ii = 1, obj%spaceCompo
    s = GetNodeLoc(obj=obj%dof, idof=GetIDOF(spaceCompo=ii, timeCompo=jj, &
                                             tspaceCompo=obj%spaceCompo))

    CALL obj%SetMultiple( &
      VALUE=VALUE(ii, jj), istart=s(1), iend=s(2), &
      stride=s(3), scale=scale, addContribution=addContribution)
  END DO
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set2

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set3()"
#endif
INTEGER(I4B) :: s(3)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated(), myName, &
                  'STVectorField_::obj is not initiated')
#endif

s = GetNodeLoc(obj=obj%dof, &
               idof=GetIDOF(spaceCompo=spaceCompo, timeCompo=timeCompo, &
                            tspaceCompo=obj%spaceCompo))

CALL obj%SetMultiple( &
  VALUE=VALUE, istart=s(1), iend=s(2), &
  stride=s(3), scale=scale, addContribution=addContribution)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set3

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set4
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set4()"
#endif
INTEGER(I4B) :: ii, tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated(), myName, &
                  'STVectorField_::obj is not initiated')

#endif

#ifdef DEBUG_VER
CALL AssertError2(SIZE(VALUE, 1), obj%spaceCompo, myName, &
                  "a=size(value, 1) b=obj%spaceCompo")
#endif

#ifdef DEBUG_VER
CALL AssertError2(SIZE(VALUE, 2), obj%timeCompo, myName, &
                  "a=size(value, 2) b=obj%timeCompo")
#endif

#ifdef DEBUG_VER
ii = obj%dof.tNodes.1
CALL AssertError2(SIZE(VALUE, 3), ii, myName, &
                  "a=size(value, 2) b=obj%dof.tNodes.1")
#endif

tsize = obj%dof.tNodes.1

DO ii = 1, tsize
  CALL obj%Set(VALUE=VALUE(:, :, ii), scale=scale, globalNode=ii, &
               islocal=.TRUE., addContribution=addContribution)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set4

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set5
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set5()"
#endif
INTEGER(I4B) :: s(3)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated(), myName, &
                  'STVectorField_::obj is not initiated')
#endif

s = GetNodeLoc(obj=obj%dof, &
               idof=GetIDOF(spaceCompo=spaceCompo, timeCompo=timeCompo, &
                            tspaceCompo=obj%spaceCompo))

CALL obj%SetMultiple( &
  VALUE=VALUE, istart=s(1), iend=s(2), &
  stride=s(3), scale=scale, addContribution=addContribution)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set5

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set6
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set6()"
#endif
INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated(), myName, &
                  'STVectorField_::obj is not initiated')
#endif

#ifdef DEBUG_VER
CALL AssertError2(SIZE(VALUE, 1), obj%spaceCompo, myName, &
                  "a=size(value, 1) b=obj%spaceCompo")
#endif

#ifdef DEBUG_VER
CALL AssertError2(SIZE(VALUE, 2), obj%timeCompo, myName, &
                  "a=size(value, 2) b=obj%timeCompo")
#endif

DO ii = 1, SIZE(globalNode)
  CALL obj%Set(VALUE=VALUE, globalNode=globalNode(ii), scale=scale, &
               addContribution=addContribution, islocal=islocal)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set6

!----------------------------------------------------------------------------
!                                                                       set
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

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated(), myName, &
                  'STVectorField_::obj is not initiated')
#endif

#ifdef DEBUG_VER
CALL AssertError2(SIZE(VALUE, 1), obj%spaceCompo, myName, &
                  "a=size(value, 1) b=obj%spaceCompo")
#endif

#ifdef DEBUG_VER
CALL AssertError2(SIZE(VALUE, 2), obj%timeCompo, myName, &
                  "a=size(value, 2) b=obj%timeCompo")
#endif

#ifdef DEBUG_VER
CALL AssertError2(SIZE(VALUE, 3), SIZE(globalNode), myName, &
                  "a=size(value, 2) b=size(globalNode)")
#endif

DO ii = 1, SIZE(globalNode)
  CALL obj%Set(VALUE=VALUE(:, :, ii), globalNode=globalNode(ii), &
               scale=scale, addContribution=addContribution, &
               islocal=islocal)
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
LOGICAL(LGT) :: isok
#endif
INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated(), myName, &
                  'STVectorField_::obj is not initiated')
#endif

#ifdef DEBUG_VER
isok = spaceCompo .LE. obj%spaceCompo
CALL AssertError1(isok, myName, "spaceCompo out of bound")
#endif

#ifdef DEBUG_VER
isok = timeCompo .LE. obj%timeCompo
CALL AssertError1(isok, myName, "timeCompo out of bound")
#endif

#ifdef DEBUG_VER
CALL AssertError2(SIZE(VALUE), SIZE(globalNode), myName, &
                  "a=size(value), b= size(globalNode)")
#endif

#include "./localNodeError.F90"

tsize = SIZE(globalNode)

IF (tsize .LE. TEMP_INTVEC_LEN) THEN
  CALL GetNodeLoc_( &
    obj=obj%dof, nodenum=globalNode, ans=TEMP_INTVEC, &
    tsize=tsize, timeCompo=timeCompo, spaceCompo=spaceCompo, ivar=1)

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

CALL GetNodeLoc_( &
  obj=obj%dof, nodenum=globalNode, ans=TEMP_DYNA_INTVEC, &
  tsize=tsize, timeCompo=timeCompo, spaceCompo=spaceCompo, ivar=1)

CALL obj%SetMultiple(indx=TEMP_DYNA_INTVEC(1:tsize), VALUE=VALUE, &
                     scale=scale, addContribution=addContribution)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set8

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set9
#ifdef DEBUG_VER
LOGICAL(LGT) :: isok
CHARACTER(*), PARAMETER :: myName = "obj_Set9()"
#endif
INTEGER(I4B) :: indx

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated(), myName, &
                  'STVectorField_::obj is not initiated')
#endif

#ifdef DEBUG_VER
isok = spaceCompo .LE. obj%spaceCompo
CALL AssertError1(isok, myName, "spaceCompo out of bound")
#endif

#ifdef DEBUG_VER
isok = timeCompo .LE. obj%timeCompo
CALL AssertError1(isok, myName, "timeCompo out of bound")
#endif

#include "./localNodeError.F90"

indx = GetNodeLoc(obj=obj%dof, nodenum=globalNode, &
                  ivar=1, spaceCompo=spaceCompo, timeCompo=timeCompo)

CALL obj%SetSingle(indx=indx, VALUE=VALUE, scale=scale, &
                   addContribution=addContribution)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set9

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set10
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set10()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

SELECT CASE (VALUE%vartype)
CASE (SpaceTime)
  CALL obj%Set( &
    VALUE=GET(VALUE, TypeFEVariableVector, TypeFEVariableSpaceTime), &
    globalNode=globalNode, islocal=islocal, scale=scale, &
    addContribution=addContribution)

CASE DEFAULT

#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, &
                    'No case found for Value%vartype '// &
                    ' only SpaceTime allowed')
#endif

END SELECT

END PROCEDURE obj_Set10

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set11
CALL obj%SetAll(VALUE=VALUE, scale=scale, addContribution=addContribution)
END PROCEDURE obj_Set11

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set12
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set12()"
LOGICAL(LGT) :: isok
INTEGER(I4B) :: nrow, ncol
#endif
INTEGER(I4B) :: indx, ii, s(3), tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated(), myName, &
                  'STVectorField_::obj is not initiated')
#endif

#ifdef DEBUG_VER
isok = timeCompo .LE. obj%timeCompo
CALL AssertError1(isok, myName, "timeCompo out of bound")
#endif

#ifdef DEBUG_VER
DO ii = 1, SIZE(spaceCompo)
  isok = spaceCompo(ii) .LE. obj%spaceCompo
  CALL AssertError1(isok, myName, &
                    "spaceCompo "//tostring(ii)//" out of bound")
END DO
#endif

#ifdef DEBUG_VER
IF (storageFMT .EQ. NODES_FMT) THEN
  nrow = SIZE(spaceCompo)
  ncol = obj%dof.tNodes.1
ELSE
  nrow = obj%dof.tNodes.1
  ncol = SIZE(spaceCompo)
END IF
#endif

#ifdef DEBUG_VER
CALL AssertError2(SIZE(VALUE, 1), nrow, myName, &
                  "a=size(value, 1) b=size(spaceCompo)")
#endif

#ifdef DEBUG_VER
CALL AssertError2(SIZE(VALUE, 2), ncol, myName, &
                  "a=size(value, 2) b=obj%dof.tNodes.1")
#endif

IF (storageFMT .EQ. DOF_FMT) THEN

  DO ii = 1, SIZE(spaceCompo)
    indx = GetIDOF(spaceCompo=spaceCompo(ii), timeCompo=timeCompo, &
                   tspaceCompo=obj%spaceCompo)

    s = GetNodeLoc(obj=obj%dof, idof=indx)

    CALL obj%SetMultiple( &
      VALUE=VALUE(:, ii), istart=s(1), iend=s(2), &
      stride=s(3), scale=scale, addContribution=addContribution)
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

indx = obj%dof.tNodes.1

DO ii = 1, indx
  CALL GetNodeLoc_(obj=obj%dof, nodenum=ii, ans=TEMP_INTVEC, tsize=tsize, &
                   timeCompo=timeCompo, spaceCompo=spaceCompo, ivar=1)

  CALL obj%SetMultiple(VALUE=VALUE(:, ii), indx=TEMP_INTVEC(1:tsize), &
                       scale=scale, addContribution=addContribution)
END DO

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
INTEGER(I4B) :: nrow, ncol
#endif
INTEGER(I4B) :: indx, ii, s(3), tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated(), myName, &
                  'STVectorField_::obj is not initiated')
#endif

#ifdef DEBUG_VER
isok = spaceCompo .LE. obj%spaceCompo
CALL AssertError1(isok, myName, "spaceCompo out of bound")
#endif

#ifdef DEBUG_VER
DO ii = 1, SIZE(timeCompo)
  isok = timeCompo(ii) .LE. obj%timeCompo
  CALL AssertError1(isok, myName, &
                    "timeCompo "//tostring(ii)//" out of bound")
END DO
#endif

#ifdef DEBUG_VER
IF (storageFMT .EQ. NODES_FMT) THEN
  nrow = SIZE(timeCompo)
  ncol = obj%dof.tNodes.1
ELSE
  nrow = obj%dof.tNodes.1
  ncol = SIZE(timeCompo)
END IF
#endif

#ifdef DEBUG_VER
CALL AssertError2(SIZE(VALUE, 1), nrow, myName, &
                  "a=size(value, 1) b=size( timeCompo)")
#endif

#ifdef DEBUG_VER
CALL AssertError2(SIZE(VALUE, 2), ncol, myName, &
                  "a=size(value, 2) b=obj%dof.tNodes.1")
#endif

IF (storageFMT .EQ. DOF_FMT) THEN

  DO ii = 1, SIZE(timeCompo)
    indx = GetIDOF(spaceCompo=spaceCompo, timeCompo=timeCompo(ii), &
                   tspaceCompo=obj%spaceCompo)

    s = GetNodeLoc(obj=obj%dof, idof=indx)

    CALL obj%SetMultiple( &
      VALUE=VALUE(:, ii), istart=s(1), iend=s(2), &
      stride=s(3), scale=scale, addContribution=addContribution)
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

indx = obj%dof.tNodes.1

DO ii = 1, indx
  CALL GetNodeLoc_(obj=obj%dof, nodenum=ii, ans=TEMP_INTVEC, tsize=tsize, &
                   timeCompo=timeCompo, spaceCompo=spaceCompo, ivar=1)

  CALL obj%SetMultiple(VALUE=VALUE(:, ii), indx=TEMP_INTVEC(1:tsize), &
                       scale=scale, addContribution=addContribution)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set13

!----------------------------------------------------------------------------
!                                                                      Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set14
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set14()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

SELECT TYPE (VALUE)
TYPE IS (STVectorField_)
  CALL obj%Copy(VALUE)

#ifdef DEBUG_VER
CLASS DEFAULT
  CALL AssertError1(math%no, myName, &
                    'No case found for the type of VALUE')
#endif
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set14

!----------------------------------------------------------------------------
!                                                         SetFromVectorField
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetFromVectorField
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetFromVectorField()"
LOGICAL(LGT) :: isok
#endif
INTEGER(I4B) :: idof, icompo, s(3), p(3)
REAL(DFP), POINTER :: realvec(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = obj%IsInitiated()
CALL AssertError1(isok, myName, &
                  'STVectorField_::obj is not initiated')
#endif

#ifdef DEBUG_VER
isok = VALUE%IsInitiated()
CALL AssertError1(isok, myName, &
                  'VectorField_::value is not initiated')
#endif

realvec => VALUE%GetPointer()

#ifdef DEBUG_VER
isok = ASSOCIATED(realvec)
CALL AssertError1(isok, myName, &
                  'realvec obtained To value is not ASSOCIATED')
#endif

DO icompo = 1, obj%spaceCompo
  s = GetNodeLoc(obj=VALUE%dof, idof=icompo)

  idof = GetIDOF( &
         spaceCompo=icompo, timeCompo=timeCompo, &
         tspaceCompo=obj%spaceCompo)

  p = GetNodeLoc(obj=obj%dof, idof=idof)

  CALL obj%SetMultiple( &
    VALUE=realvec, scale=scale, addContribution=addContribution, &
    istart_value=s(1), iend_value=s(2), stride_value=s(3), &
    istart=p(1), iend=p(2), stride=p(3))
END DO

realvec => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetFromVectorField

!----------------------------------------------------------------------------
!                                                           SetToVectorField
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetToVectorField
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetToVectorField()"
LOGICAL(LGT) :: isok
#endif
INTEGER(I4B) :: s(3), p(3), icompo, idof
REAL(DFP), POINTER :: realvec(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = obj%IsInitiated()
CALL AssertError1(isok, myName, &
                  'STVectorField_::obj is not initiated')
#endif

#ifdef DEBUG_VER
isok = VALUE%IsInitiated()
CALL AssertError1(isok, myName, &
                  'VectorField_::value is not initiated')
#endif

realvec => obj%GetPointer()

#ifdef DEBUG_VER
isok = ASSOCIATED(realvec)
CALL AssertError1(isok, myName, &
                  'realvec obtained To value is not ASSOCIATED')
#endif

DO icompo = 1, obj%spaceCompo
  s = GetNodeLoc(obj=VALUE%dof, idof=icompo)

  idof = GetIDOF( &
         spaceCompo=icompo, timeCompo=timeCompo, &
         tspaceCompo=obj%spaceCompo)
  p = GetNodeLoc(obj=obj%dof, idof=idof)

  CALL VALUE%SetMultiple( &
    VALUE=realvec, scale=scale, addContribution=addContribution, &
    istart_value=p(1), iend_value=p(2), stride_value=p(3), &
    istart=s(1), iend=s(2), stride=s(3))
END DO

realvec => NULL()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetToVectorField

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetByFunction
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetByFunction()"
#endif
LOGICAL(LGT) :: istimes, isok
INTEGER(I4B) :: ttime, nsd, tnodes, ii, globalNode(1), &
                itime, ispace, nrow, ncol
REAL(DFP) :: args(4), xij(3, 1)
REAL(DFP), ALLOCATABLE :: VALUE(:)
CLASS(AbstractMesh_), POINTER :: meshptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

istimes = PRESENT(times)
isok = math%yes

args = math%zero
ttime = 1
IF (istimes) THEN
  ttime = SIZE(times)
  isok = ttime .EQ. obj%timeCompo
END IF

#ifdef DEBUG_VER
CALL AssertError1(isok, myName, &
                  'Size of times should be obj%timeCompo='// &
                  ToString(obj%timeCompo))
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
    globalNode = ii
    CALL meshptr%GetNodeCoord(globalNode=globalNode, nodeCoord=xij, &
                              islocal=math%yes, nrow=nrow, ncol=ncol)
    args(1:nsd) = xij(1:nsd, 1)

    DO itime = 1, obj%timeCompo
      args(4) = times(itime)
      CALL func%Get(val=VALUE, args=args)
      DO ispace = 1, obj%spaceCompo
        CALL obj%Set(globalNode=globalNode(1), VALUE=VALUE(ispace), &
                     timeCompo=itime, spaceCompo=ispace, islocal=math%yes)
      END DO
    END DO
  END DO
END IF

IF (.NOT. istimes) THEN

  DO ii = 1, tnodes
    globalNode = ii
    CALL meshptr%GetNodeCoord( &
      globalNode=globalNode, nodeCoord=xij, &
      islocal=math%yes, nrow=nrow, ncol=ncol)

    args(1:nsd) = xij(1:nsd, 1)
    CALL func%Get(val=VALUE, args=args)

    DO itime = 1, obj%timeCompo
      DO ispace = 1, obj%spaceCompo
        CALL obj%Set(globalNode=globalNode(1), VALUE=VALUE(ispace), &
                     timeCompo=itime, spaceCompo=ispace, &
                     islocal=math%yes)
      END DO
    END DO
  END DO

END IF

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
