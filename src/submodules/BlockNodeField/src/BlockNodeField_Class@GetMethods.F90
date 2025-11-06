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

SUBMODULE(BlockNodeField_Class) GetMethods

USE InputUtility, ONLY: Input

USE FieldOpt_Class, ONLY: TypeField => TypeFieldOpt

USE ScalarField_Class, ONLY: ScalarField_
! USE ScalarFieldLis_Class, ONLY: ScalarFieldLis_

USE STScalarField_Class, ONLY: STScalarField_
! USE STScalarFieldLis_Class, ONLY: STScalarFieldLis_

USE VectorField_Class, ONLY: VectorField_
! USE VectorFieldLis_Class, ONLY: VectorFieldLis_

USE STVectorField_Class, ONLY: STVectorField_

USE RealVector_Method, ONLY: GetValue_

USE ArangeUtility, ONLY: Arange

USE BaseType, ONLY: TypeFEVariableScalar, &
                    TypeFEVariableVector, &
                    TypeFEVariableSpace, &
                    TypeFEVariableSpaceTime

USE FEVariable_Method, ONLY: NodalVariable

USE DOF_Method, ONLY: GetIDOF, &
                      OPERATOR(.tnodes.), &
                      GetNodeLoc, &
                      GetNodeLoc_, &
                      OPERATOR(.tdof.), &
                      OPERATOR(.TimeComponents.), &
                      OPERATOR(.SpaceComponents.)

USE SwapUtility, ONLY: Swap_

USE Display_Method, ONLY: ToString

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get1()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: indx

#ifdef DEBUG_VER
isok = obj%IsInitiated()
CALL AssertError1(isok, myName, &
                  'BlockNodeField_::obj is not initiated')
#endif

#include "./localNodeError.F90"

indx = GetNodeLoc(obj=obj%dof, nodenum=globalNode, ivar=ivar, idof=idof)
CALL obj%GetSingle(VALUE=VALUE, indx=indx)

END PROCEDURE obj_Get1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get2
INTEGER(I4B) :: iend
iend = obj%SIZE()
CALL obj%GetMultiple(istart=1, iend=iend, stride=1, VALUE=VALUE, tsize=tsize)
END PROCEDURE obj_Get2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get3()"
LOGICAL(LGT) :: isok
INTEGER(I4B) :: myint1, myint2
#endif

INTEGER(I4B) :: indx(SIZE(globalNode))

#ifdef DEBUG_VER
isok = obj%IsInitiated()
CALL AssertError1(isok, myName, &
                  'BlockNodeField_::obj is not initiated')

myint1 = SIZE(VALUE)
myint2 = SIZE(globalNode)
CALL AssertError2(myint1, myint2, myName, &
                  "a=SIZE(VALUE) b = SIZE(globalNode)")
#endif

#include "./localNodeError.F90"

CALL GetNodeLoc_(obj=obj%dof, ivar=ivar, idof=idof, nodenum=globalNode, &
                 ans=indx, tsize=tsize)
CALL obj%GetMultiple(indx=indx, tsize=tsize, VALUE=VALUE)
END PROCEDURE obj_Get3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get4
REAL(DFP) :: value0(SIZE(globalNode))
INTEGER(I4B) :: tsize

CALL obj%Get(VALUE=value0, globalNode=globalNode, ivar=ivar, idof=idof, &
             islocal=islocal, tsize=tsize)

VALUE = NodalVariable(value0, TypeFEVariableScalar, TypeFEVariableSpace)
END PROCEDURE obj_Get4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get5
CHARACTER(*), PARAMETER :: myName = "obj_Get5()"
INTEGER(I4B) :: timeCompo, spaceCompo, ierr, case_id, tdof, ii, &
                tsize
REAL(DFP), ALLOCATABLE :: m3a(:, :, :)
REAL(DFP), ALLOCATABLE :: m3b(:, :, :)
REAL(DFP), ALLOCATABLE :: value0(:)

tdof = obj%dof.tdof.ivar
case_id = SIZE(globalNode)

ALLOCATE (m3a(case_id, tdof, 1))

DO ii = 1, tdof
  CALL obj%Get(VALUE=m3a(:, ii, 1), globalNode=globalNode, &
               ivar=ivar, idof=ii, islocal=islocal, tsize=tsize)
END DO

ALLOCATE (value0(case_id * tdof))
value0 = RESHAPE(m3a, [case_id * tdof])

DEALLOCATE (m3a)

case_id = 0_I4B
timeCompo = obj%dof.TimeComponents.ivar
spaceCompo = obj%dof.SpaceComponents.ivar

IF ((spaceCompo .GT. 1)) THEN
  IF (timeCompo .GT. 1) THEN
    case_id = 1
  ELSE
    case_id = 2
  END IF
ELSE
  IF (timeCompo .GT. 1) THEN
    case_id = 3
  ELSE
    case_id = 4
  END IF
END IF

SELECT CASE (case_id)
CASE (1)
  ! vector space-time
  m3b = RESHAPE(value0, [SIZE(globalNode), spaceCompo, timeCompo])
  ALLOCATE (m3a(spaceCompo, SIZE(globalNode), timeCompo))
  ! Here m3b is in (J, i, a) format, but we need (i,J,a) format
  CALL SWAP_(a=m3a, b=m3b, i1=2, i2=1, i3=3)
  VALUE = NodalVariable(m3a, TypeFEVariableVector, TypeFEVariableSpaceTime)

CASE (2)
  ! vector space
  VALUE = NodalVariable(TRANSPOSE(RESHAPE(value0, &
                                          [SIZE(globalNode), spaceCompo])), &
                        TypeFEVariableVector, TypeFEVariableSpace)

CASE (3)
  ! scalar space-time
  VALUE = NodalVariable(RESHAPE(value0, [SIZE(globalNode), timeCompo]), &
                        TypeFEVariableScalar, TypeFEVariableSpaceTime)

CASE (4)
  ! scalar space
  VALUE = NodalVariable(value0, TypeFEVariableScalar, TypeFEVariableSpace)

CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: No case found for given arguments')
  RETURN

END SELECT

IF (ALLOCATED(value0)) DEALLOCATE (value0)
IF (ALLOCATED(m3a)) DEALLOCATE (m3a)
IF (ALLOCATED(m3b)) DEALLOCATE (m3b)

END PROCEDURE obj_Get5

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get6
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get6()"
LOGICAL(LGT) :: isok
INTEGER(I4B) :: myint1, myint2
#endif

INTEGER(I4B) :: indx(SIZE(globalNode)), idof

#ifdef DEBUG_VER
isok = obj%IsInitiated()
CALL AssertError1(isok, myName, &
                  'BlockNodeField_::obj is not initiated')

myint1 = SIZE(globalNode)
myint2 = SIZE(VALUE)
CALL AssertError2(myint1, myint2, myName, &
                  "a=size(globalNode), b=Size(value)")
#endif

#include "./localNodeError.F90"

idof = GetIDOF(obj=obj%dof, ivar=ivar, spaceCompo=spaceCompo, &
               timeCompo=timeCompo)

CALL GetNodeLoc_(obj=obj%dof, idof=idof, nodenum=globalNode, &
                 ans=indx, tsize=tsize)

CALL obj%GetMultiple(indx=indx, VALUE=VALUE, tsize=tsize)

END PROCEDURE obj_Get6

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get7
REAL(DFP) :: value0(SIZE(globalNode))
INTEGER(I4B) :: tsize

CALL obj%Get(VALUE=value0, globalNode=globalNode, ivar=ivar, &
             spaceCompo=spaceCompo, timeCompo=timeCompo, islocal=islocal, &
             tsize=tsize)
VALUE = NodalVariable(value0, TypeFEVariableScalar, TypeFEVariableSpace)
END PROCEDURE obj_Get7

!----------------------------------------------------------------------------
!                                                                 Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get8
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get8()"
LOGICAL(LGT) :: isok
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = obj%IsInitiated()
CALL AssertError1(isok, myName, &
                  "BlockNodeField_:: obj is not initiated")

isok = VALUE%IsInitiated()
CALL AssertError1(isok, myName, &
                  "BlockNodeField_:: value is not initiated")
#endif

SELECT TYPE (VALUE)

CLASS IS (ScalarField_)
  CALL VALUE%Set(ivar=1, idof=1, VALUE=obj, ivar_value=ivar, idof_value=idof)

CLASS IS (STScalarField_)
  CALL VALUE%Set(ivar=1, idof=idof_value, VALUE=obj, ivar_value=ivar, &
                 idof_value=idof)

CLASS IS (VectorField_)
  CALL VALUE%Set(ivar=1, idof=idof_value, VALUE=obj, ivar_value=ivar, &
                 idof_value=idof)

CLASS IS (STVectorField_)
  CALL VALUE%Set(ivar=1, idof=idof_value, VALUE=obj, ivar_value=ivar, &
                 idof_value=idof)

CLASS IS (BlockNodeField_)
CALL VALUE%Set(ivar=ivar_value, idof=idof_value, VALUE=obj, ivar_value=ivar, &
                 idof_value=idof)

#ifdef DEBUG_VER
CLASS DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTENRAL ERROR] :: No case found for the type of value')
#endif
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Get8

!----------------------------------------------------------------------------
!                                                           GetFEVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFEVariable
INTEGER(I4B) :: ivar0
ivar0 = input(option=ivar, default=1_I4B)
CALL obj%Get(globalNode=globalNode, VALUE=VALUE, ivar=ivar0, &
             islocal=islocal)
END PROCEDURE obj_GetFEVariable

!----------------------------------------------------------------------------
!                                                                 GetPrefix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPrefix
ans = myprefix
END PROCEDURE obj_GetPrefix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetMethods
