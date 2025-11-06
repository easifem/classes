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

SUBMODULE(BlockNodeField_Class) SetMethods

USE InputUtility, ONLY: Input

USE AbstractMesh_Class, ONLY: AbstractMesh_

USE FieldOpt_Class, ONLY: TypeField => TypeFieldOpt

USE AbstractNodeField_Class, ONLY: AbstractNodeFieldGetPointer

USE ScalarField_Class, ONLY: ScalarField_
USE ScalarFieldLis_Class, ONLY: ScalarFieldLis_

USE STScalarField_Class, ONLY: STScalarField_
USE STScalarFieldLis_Class, ONLY: STScalarFieldLis_

USE VectorField_Class, ONLY: VectorField_
USE VectorFieldLis_Class, ONLY: VectorFieldLis_

USE STVectorField_Class, ONLY: STVectorField_
USE STVectorFieldLis_Class, ONLY: STVectorFieldLis_

USE RealVector_Method, ONLY: Set, Add, GetPointer

USE Display_Method, ONLY: tostring

USE GlobalData, ONLY: SpaceTime

USE DOF_Method, ONLY: GetNodeLoc, &
                      OPERATOR(.tNodes.), &
                      GetIDOF, &
                      GetNodeLoc_, &
                      GetIndex_, &
                      OPERATOR(.tDOF.), &
                      GetIndex

USE ArangeUtility, ONLY: Arange

USE Basetype, ONLY: TypeFEVariableVector, &
                    TypeFEVariableSpaceTime, &
                    TypeFEVariableConstant

USE FEVariable_Method, ONLY: Get

USE ReallocateUtility, ONLY: Reallocate

USE SafeSizeUtility, ONLY: SafeSize

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set1
CALL obj%SetAll(VALUE=VALUE, scale=scale, addContribution=addContribution)
END PROCEDURE obj_Set1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set2()"
#endif

INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated(), myName, &
                  'BlockNodeField_::obj is not initiated')

tsize = obj%SIZE()

CALL AssertError2(tsize, SIZE(VALUE), myName, &
                  'a=obj%size(), b=size(value)')
#endif

tsize = obj%SIZE()

CALL obj%SetMultiple(value=value, scale=scale, addContribution=addContribution,&
                     istart=1_I4B, iend=tsize, stride=1_I4B)

END PROCEDURE obj_Set2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set3
CHARACTER(*), PARAMETER :: myName = "obj_Set3()"
INTEGER(I4B) :: indx

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated(), myName, &
                  'BlockNodeField_::obj is not initiated')
#endif

#include "./localNodeError.F90"

indx = GetNodeLoc(obj=obj%dof, nodenum=globalNode, ivar=ivar, idof=idof)

CALL obj%SetSingle(VALUE=VALUE, indx=indx, scale=scale, &
                   addContribution=addContribution)
END PROCEDURE obj_Set3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set4
CHARACTER(*), PARAMETER :: myName = "obj_Set4()"
INTEGER(I4B) :: indx(128), ii, tsize, idof(128)
REAL(DFP) :: areal(128)

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated(), myName, &
                  'BlockNodeField_::obj is not initiated')
#endif

#include "./localNodeError.F90"

areal = VALUE

tsize = obj%dof.tDOF.ivar
idof(1:tsize) = GetIDOF(obj=obj%dof, ivar=ivar)

DO ii = 1, SIZE(globalNode)
  CALL GetNodeLoc_(obj=obj%dof, nodenum=globalNode(ii), ivar=ivar, &
                   idof=idof(1:tsize), ans=indx, tsize=tsize)

  CALL obj%SetMultiple(VALUE=areal(1:tsize), indx=indx(1:tsize), &
                       scale=scale, addContribution=addContribution)
END DO

END PROCEDURE obj_Set4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set5
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set5()"
INTEGER(I4B) :: tsize1, tsize2
#endif

INTEGER(I4B) :: indx(SIZE(VALUE)), tsize

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated(), myName, &
                  'BlockNodeField_::obj is not initiated')

tsize1 = SIZE(VALUE)
tsize2 = SIZE(globalNode) * (obj%dof.tdof.ivar)

CALL AssertError2(tsize1, tsize2, myName, &
                  "a=size(value), b=size(globalNode) * (obj%tDOF%ivar)")
#endif

#include "./localNodeError.F90"

CALL GetIndex_(obj=obj%dof, nodenum=globalNode, ivar=ivar, ans=indx, &
               tsize=tsize)

CALL obj%SetMultiple(VALUE=VALUE, indx=indx, scale=scale, &
                     addContribution=addContribution)

END PROCEDURE obj_Set5

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set6
INTEGER(I4B) :: ii
DO ii = 1, SIZE(globalNode)
  CALL obj%Set(globalNode=globalNode(ii), islocal=islocal, scale=scale, &
               addContribution=addContribution, VALUE=VALUE, &
               ivar=ivar, idof=idof)
END DO
END PROCEDURE obj_Set6

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set7
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set7()"
#endif

INTEGER(I4B) :: indx(SIZE(globalNode)), tsize

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated(), myName, &
                  'BlockNodeField_::obj is not initiated')

CALL AssertError2(SIZE(VALUE), SIZE(globalNode), myName, &
                  "a=size(value), b=size(globalNode)")
#endif

#include "./localNodeError.F90"

CALL GetNodeLoc_(obj=obj%dof, ans=indx, tsize=tsize, ivar=ivar, &
                 idof=idof, nodenum=globalNode)

CALL obj%SetMultiple(indx=indx, VALUE=VALUE, scale=scale, &
                     addContribution=addContribution)

END PROCEDURE obj_Set7

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set8
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set8()"
#endif

INTEGER(I4B) :: indx(SIZE(globalNode)), tsize

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated(), myName, &
                  'BlockNodeField_::obj is not initiated')

CALL AssertError2(SIZE(VALUE), SIZE(globalNode), myName, &
                  "a=size(value), b=size(globalNode)")
#endif

#include "./localNodeError.F90"

CALL GetNodeLoc_(obj=obj%dof, ans=indx, tsize=tsize, ivar=ivar, &
                 spaceCompo=spaceCompo, timeCompo=timeCompo, &
                 nodenum=globalNode)

CALL obj%SetMultiple(indx=indx, VALUE=VALUE, scale=scale, &
                     addContribution=addContribution)

END PROCEDURE obj_Set8

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set9
REAL(DFP) :: value0(SIZE(globalNode))
value0 = VALUE
CALL obj%Set(VALUE=value0, scale=scale, addContribution=addContribution, &
   globalNode=globalNode, islocal=islocal, ivar=ivar, spaceCompo=spaceCompo, &
             timeCompo=timeCompo)
END PROCEDURE obj_Set9

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set10
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set10()"
INTEGER(I4B) :: tsize1, tsize2
#endif

INTEGER(I4B) :: indx(SIZE(VALUE)), tsize

#ifdef DEBUG_VER

CALL AssertError1(.NOT. obj%isInitiated(), myName, &
                  'BlockNodeField_::obj is not initiated')

tsize1 = SIZE(VALUE)
tsize2 = SIZE(globalNode) * SIZE(timeCompo)
CALL AssertError2(tsize1, tsize2, myName, &
                  "a=size(value), b=size(globalNode)*size(timeCompo)")

#endif

#include "./localNodeError.F90"

CALL GetNodeLoc_(obj=obj%dof, ivar=ivar, spaceCompo=spaceCompo, &
               timeCompo=timeCompo, nodenum=globalNode, ans=indx, tsize=tsize)

CALL obj%SetMultiple(indx=indx, VALUE=VALUE, scale=scale, &
                     addContribution=addContribution)

END PROCEDURE obj_Set10

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set11
REAL(DFP) :: value0(SIZE(globalNode) * SIZE(timeCompo))
value0 = VALUE
CALL obj%Set(VALUE=value0, globalNode=globalNode, islocal=islocal, &
             ivar=ivar, spaceCompo=spaceCompo, timeCompo=timeCompo, &
             scale=scale, addContribution=addContribution)
END PROCEDURE obj_Set11

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set12
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set12()"
INTEGER(I4B) :: tsize1, tsize2
#endif

INTEGER(I4B) :: indx(SIZE(VALUE)), tsize

#ifdef DEBUG_VER

CALL AssertError1(.NOT. obj%isInitiated(), myName, &
                  'BlockNodeField_::obj is not initiated')

tsize1 = SIZE(VALUE)
tsize2 = SIZE(globalNode) * SIZE(spaceCompo)
CALL AssertError2(tsize1, tsize2, myName, &
                  "a=size(value), b=size(globalNode)*size(spaceCompo)")

#endif

#include "./localNodeError.F90"

CALL GetNodeLoc_(obj=obj%dof, ivar=ivar, spaceCompo=spaceCompo, &
               timeCompo=timeCompo, nodenum=globalNode, ans=indx, tsize=tsize)

CALL obj%SetMultiple(indx=indx, VALUE=VALUE, scale=scale, &
                     addContribution=addContribution)

END PROCEDURE obj_Set12

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set13
REAL(DFP) :: value0(SIZE(globalNode) * SIZE(spaceCompo))
value0 = VALUE
CALL obj%Set(VALUE=value0, globalNode=globalNode, islocal=islocal, &
             ivar=ivar, spaceCompo=spaceCompo, timeCompo=timeCompo, &
             scale=scale, addContribution=addContribution)
END PROCEDURE obj_Set13

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set14
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set14"
#endif

INTEGER(I4B) :: indx

#ifdef DEBUG_VER
CALL AssertError1(.NOT. obj%isInitiated(), myName, &
                  'BlockNodeField_::obj is not initiated')

#endif

#include "./localNodeError.F90"

indx = GetNodeLoc(obj=obj%dof, nodenum=globalNode, ivar=ivar, &
                  spaceCompo=spaceCompo, timeCompo=timeCompo)

CALL obj%SetSingle(VALUE=VALUE, indx=indx, &
                   scale=scale, addContribution=addContribution)
END PROCEDURE obj_Set14

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set15
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set15()"
#endif

INTEGER(I4B) :: indx(SIZE(timeCompo)), tsize
REAL(DFP) :: value0(SIZE(timeCompo))

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated(), myName, &
                  'BlockNodeField_::obj is not initiated')
#endif

#include "./localNodeError.F90"

CALL GetNodeLoc_(obj=obj%dof, ivar=ivar, spaceCompo=spaceCompo, &
               timeCompo=timeCompo, nodenum=globalNode, ans=indx, tsize=tsize)

value0 = VALUE

CALL obj%SetMultiple(indx=indx, VALUE=value0, scale=scale, &
                     addContribution=addContribution)

END PROCEDURE obj_Set15

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set16
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set16()"
#endif

INTEGER(I4B) :: indx(SIZE(spaceCompo)), tsize
REAL(DFP) :: value0(SIZE(spaceCompo))

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated(), myName, &
                  'BlockNodeField_::obj is not initiated')
#endif

#include "./localNodeError.F90"

CALL GetNodeLoc_(obj=obj%dof, ivar=ivar, spaceCompo=spaceCompo, &
               timeCompo=timeCompo, nodenum=globalNode, ans=indx, tsize=tsize)

value0 = VALUE

CALL obj%SetMultiple(indx=indx, VALUE=value0, scale=scale, &
                     addContribution=addContribution)

END PROCEDURE obj_Set16

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set17
#ifdef DEBUG_VER

CHARACTER(*), PARAMETER :: myName = "obj_Set17()"
#endif

REAL(DFP) :: areal
LOGICAL(LGT) :: abool

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated(), myName, &
                  'BlockNodeField_::obj is not initiated')

CALL AssertError1(VALUE%isInitiated(), myName, &
                  'BlockNodeField_::value is not initiated')

#endif

abool = Input(option=addContribution, default=.FALSE.)

IF (abool) THEN
  areal = Input(option=scale, default=1.0_DFP)
  CALL obj%Axpy(x=VALUE, scale=areal)
  RETURN
END IF

CALL obj%Copy(VALUE)

END PROCEDURE obj_Set17

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set18
CHARACTER(*), PARAMETER :: myName = "obj_Set18()"
INTEGER(I4B) :: s(3), p(3), tsize
REAL(DFP), POINTER :: realvec(:)

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated(), myName, &
                  'BlockNodeField_ ::obj is not initiated')

CALL AssertError1(VALUE%isInitiated(), myName, &
                  'AbstractNodeField_::value is not initiated')
#endif

s = GetNodeLoc(obj=obj%dof, idof=GetIDOF(obj=obj%dof, ivar=ivar, idof=idof))

SELECT TYPE (VALUE)

TYPE IS (ScalarField_)

  realvec => VALUE%GetPointer()
  CALL obj%SetMultiple(VALUE=realvec, scale=scale, &
         addContribution=addContribution, istart=s(1), iend=s(2), stride=s(3))
  realvec => NULL()

TYPE IS (STScalarField_)

  p = GetNodeLoc(obj=VALUE%dof, idof=idof_value)

  realvec => VALUE%GetPointer()
  CALL obj%SetMultiple(VALUE=realvec, scale=scale, &
       addContribution=addContribution, istart=s(1), iend=s(2), stride=s(3), &
                       istart_value=p(1), iend_value=p(2), stride_value=p(3))
  realvec => NULL()

TYPE IS (VectorField_)

  p = GetNodeLoc(obj=VALUE%dof, idof=idof_value)

  realvec => VALUE%GetPointer()
  CALL obj%SetMultiple(VALUE=realvec, scale=scale, &
       addContribution=addContribution, istart=s(1), iend=s(2), stride=s(3), &
                       istart_value=p(1), iend_value=p(2), stride_value=p(3))
  realvec => NULL()

TYPE IS (STVectorField_)

  p = GetNodeLoc(obj=VALUE%dof, idof=idof_value)

  realvec => VALUE%GetPointer()
  CALL obj%SetMultiple(VALUE=realvec, scale=scale, &
       addContribution=addContribution, istart=s(1), iend=s(2), stride=s(3), &
                       istart_value=p(1), iend_value=p(2), stride_value=p(3))
  realvec => NULL()

TYPE IS (ScalarFieldLis_)

  p = GetNodeLoc(obj=VALUE%dof, idof=1)
  realvec => AbstractNodeFieldGetPointer(VALUE)
  CALL VALUE%GetMultiple(VALUE=realvec, istart=p(1), iend=p(2), stride=p(3), &
                         tsize=tsize, istart_value=p(1), iend_value=p(2), &
                         stride_value=p(3))

  CALL obj%SetMultiple(VALUE=realvec, scale=scale, &
                    addContribution=addContribution, istart=s(1), iend=s(2), &
           stride=s(3), istart_value=p(1), iend_value=p(2), stride_value=p(3))
  realvec => NULL()

TYPE IS (STScalarFieldLis_)

  p = GetNodeLoc(obj=VALUE%dof, idof=idof_value)
  realvec => AbstractNodeFieldGetPointer(VALUE)
  CALL VALUE%GetMultiple(VALUE=realvec, istart=p(1), iend=p(2), stride=p(3), &
                         tsize=tsize, istart_value=p(1), iend_value=p(2), &
                         stride_value=p(3))

  CALL obj%SetMultiple(VALUE=realvec, scale=scale, &
                    addContribution=addContribution, istart=s(1), iend=s(2), &
           stride=s(3), istart_value=p(1), iend_value=p(2), stride_value=p(3))
  realvec => NULL()

TYPE IS (VectorFieldLis_)
  p = GetNodeLoc(obj=VALUE%dof, idof=idof_value)
  realvec => AbstractNodeFieldGetPointer(VALUE)
  CALL VALUE%GetMultiple(VALUE=realvec, istart=p(1), iend=p(2), stride=p(3), &
                         tsize=tsize, istart_value=p(1), iend_value=p(2), &
                         stride_value=p(3))

  CALL obj%SetMultiple(VALUE=realvec, scale=scale, &
                    addContribution=addContribution, istart=s(1), iend=s(2), &
           stride=s(3), istart_value=p(1), iend_value=p(2), stride_value=p(3))
  realvec => NULL()

TYPE IS (STVectorFieldLis_)
  p = GetNodeLoc(obj=VALUE%dof, idof=idof_value)
  realvec => AbstractNodeFieldGetPointer(VALUE)
  CALL VALUE%GetMultiple(VALUE=realvec, istart=p(1), iend=p(2), stride=p(3), &
                         tsize=tsize, istart_value=p(1), iend_value=p(2), &
                         stride_value=p(3))

  CALL obj%SetMultiple(VALUE=realvec, scale=scale, &
                    addContribution=addContribution, istart=s(1), iend=s(2), &
           stride=s(3), istart_value=p(1), iend_value=p(2), stride_value=p(3))
  realvec => NULL()

CLASS DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: No case found for the type of VALUE')
  RETURN

END SELECT
END PROCEDURE obj_Set18

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_assign
CALL obj%SetAll(VALUE=VALUE)
END PROCEDURE obj_assign

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SetMethods
