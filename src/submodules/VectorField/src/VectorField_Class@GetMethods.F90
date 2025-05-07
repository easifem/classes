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

SUBMODULE(VectorField_Class) GetMethods
USE AbstractField_Class, ONLY: TypeField

USE ScalarField_Class, ONLY: ScalarField_
USE ScalarFieldLis_Class, ONLY: ScalarFieldLis_

USE STScalarField_Class, ONLY: STScalarField_
USE STScalarFieldLis_Class, ONLY: STScalarFieldLis_

USE VectorField_Class, ONLY: VectorField_
USE VectorFieldLis_Class, ONLY: VectorFieldLis_

USE RealVector_Method, ONLY: GetValue_

USE ArangeUtility, ONLY: Arange

USE BaseType, ONLY: TypeFEVariableVector, TypeFEVariableSpace

USE FEVariable_Method, ONLY: NodalVariable

USE DOF_Method, ONLY: GetIDOF, &
                      OPERATOR(.tnodes.), &
                      GetNodeLoc, &
                      GetNodeLoc_

USE Display_Method, ONLY: ToString

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get1()"
LOGICAL(LGT) :: isok
#endif

LOGICAL(LGT) :: bool1, bool2
INTEGER(I4B) :: ierr, ii, s(3), indx(obj%spaceCompo)

bool1 = PRESENT(globalNode)
bool2 = PRESENT(spaceCompo)

#ifdef DEBUG_VER

isok = .NOT. (bool1 .AND. bool2)

CALL AssertError1(isok, myName, &
                  "Both globalNode and spaceCompocannot be present")

isok = bool1 .OR. bool2
CALL AssertError1(isok, myName, &
                  "Either globalNode or spaceComposhould be present")

IF (bool1) THEN
  isok = SIZE(VALUE) .GE. obj%spaceCompo
  CALL AssertError1(isok, myName, "Size of value is not enough")
END IF

IF (bool2) THEN
  isok = SIZE(VALUE) .GE. (obj%dof.tNodes.1_I4B)
  CALL AssertError1(isok, myName, "Size of value is not enough")
END IF

#endif

! globalnode present
IF (bool1) THEN
  CALL GetNodeLoc_(obj=obj%dof, idof=obj%idofs, nodenum=globalNode, &
                   ans=indx, tsize=tsize)

  CALL obj%GetMultiple(indx=indx, VALUE=VALUE, tsize=tsize)

  RETURN

END IF

!> Get all values ofspaceCompo
! IF (bool2) THEN

tsize = obj%dof.tNodes.1_I4B

s = GetNodeLoc(obj=obj%dof, idof=spaceCompo)

CALL obj%GetMultiple(istart=s(1), iend=s(2), stride=s(3), VALUE=VALUE, &
                     tsize=tsize)

! END IF

END PROCEDURE obj_Get1

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get2()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: s(3), jj, mynrow
INTEGER(I4B) :: indx(obj%spaceCompo)

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated, myName, &
                  "STScalarField_:: obj is not initiated")

IF (storageFMT .EQ. DOF_FMT) THEN
  nrow = obj%dof.tNodes.1
  ncol = obj%spaceCompo

ELSE
  ncol = obj%dof.tNodes.1
  nrow = obj%spaceCompo
END IF

isok = SIZE(VALUE, 1) .GE. nrow
CALL AssertError1(isok, myName, "Number of rows in value is not enough")

isok = SIZE(VALUE, 2) .GE. ncol
CALL AssertError1(isok, myName, "Number of cols in not enough")
#endif

IF (obj%engine%chars() .EQ. "NATIVE_SERIAL") THEN
  CALL GetValue_(obj=obj%realVec, dofobj=obj%dof, idof=obj%idofs, &
                 VALUE=VALUE, nrow=nrow, ncol=ncol, storageFMT=storageFMT)

  RETURN
END IF

IF (storageFMT .EQ. DOF_FMT) THEN
  nrow = obj%dof.tNodes.1
  ncol = obj%spaceCompo

  !$OMP PARALLEL DO PRIVATE(jj, mynrow, s)
  DO jj = 1, ncol
    s = GetNodeLoc(obj=obj%dof, idof=jj)

    CALL obj%GetMultiple(istart=s(1), iend=s(2), stride=s(3), &
                         VALUE=VALUE(:, jj), tsize=mynrow)

  END DO
  !$OMP END PARALLEL DO

  RETURN
END IF

nrow = obj%spaceCompo
ncol = obj%dof.tNodes.1

!$OMP PARALLEL DO PRIVATE(jj, indx, mynrow)
DO jj = 1, ncol
  CALL GetNodeLoc_(obj=obj%dof, idof=obj%idofs, nodenum=jj, ans=indx, &
                   tsize=mynrow)
  CALL obj%GetMultiple(indx=indx, VALUE=VALUE(:, jj), tsize=nrow)
END DO
!$OMP END PARALLEL DO

END PROCEDURE obj_Get2

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get3
#ifdef DEBUG_VER
LOGICAL(LGT) :: isok
#endif

CHARACTER(*), PARAMETER :: myName = "obj_Get3()"
INTEGER(I4B) :: jj, mynrow

#include "./localNodeError.inc"

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated, myName, &
                  "STScalarField_:: obj is not initiated")

IF (storageFMT .EQ. DOF_FMT) THEN
  nrow = SIZE(globalNode)
  ncol = obj%spaceCompo

ELSE
  ncol = SIZE(globalNode)
  nrow = obj%spaceCompo
END IF

isok = SIZE(VALUE, 1) .GE. nrow
CALL AssertError1(isok, myName, "Number of rows in value is not enough")

isok = SIZE(VALUE, 2) .GE. ncol
CALL AssertError1(isok, myName, "Number of cols in not enough")
#endif

IF (obj%engine%chars() .EQ. "NATIVE_SERIAL") THEN
  CALL GetValue_(obj=obj%realVec, dofobj=obj%dof, idof=obj%idofs, &
                 VALUE=VALUE, nrow=nrow, ncol=ncol, storageFMT=storageFMT, &
                 nodenum=globalNode)

  RETURN
END IF

IF (storageFMT .EQ. DOF_FMT) THEN
  nrow = SIZE(globalNode)
  ncol = obj%spaceCompo

  !$OMP PARALLEL DO PRIVATE(jj, mynrow)
  DO jj = 1, ncol
    CALL obj%Get(globalNode=globalNode, islocal=islocal, VALUE=VALUE(:, jj), &
                 tsize=mynrow, spaceCompo=jj)
  END DO
  !$OMP END PARALLEL DO

  RETURN
END IF

nrow = obj%spaceCompo
ncol = SIZE(globalNode)

!$OMP PARALLEL DO PRIVATE(jj, mynrow)
DO jj = 1, ncol
  CALL obj%Get(globalNode=globalNode(jj), VALUE=VALUE(:, jj), &
               tsize=mynrow)
END DO
!$OMP END PARALLEL DO

END PROCEDURE obj_Get3

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get4
CHARACTER(*), PARAMETER :: myName = "obj_Get4()"

#include "./localNodeError.inc"

IF (obj%engine%chars() .NE. "NATIVE_SERIAL") THEN

  CALL GetValue_(obj=obj%realVec, dofobj=obj%dof, ivar=1, &
                 idof=spaceCompo, VALUE=VALUE, tsize=tsize, &
                 nodenum=globalNode)

ELSE

  CALL me_if_not_native

END IF

CONTAINS

SUBROUTINE me_if_not_native
  INTEGER(I4B) :: indx(SIZE(globalNode))
  CALL GetNodeLoc_(obj=obj%dof, nodenum=globalNode, idof=spaceCompo, &
                   ans=indx, tsize=tsize)
  CALL obj%GetMultiple(indx=indx, VALUE=VALUE, tsize=tsize)
END SUBROUTINE

END PROCEDURE obj_Get4

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get5
CHARACTER(*), PARAMETER :: myName = "obj_Get5()"
INTEGER(I4B) :: indx

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated, myName, &
                  'VectorField_::obj is not initiated')

CALL AssertError1(spaceCompo .LE. obj%spaceCompo, myName, &
            'given spaceCompo should be less than or equal to obj%spaceCompo')

#endif

#include "./localNodeError.inc"

indx = GetNodeLoc(obj=obj%dof, nodenum=globalNode, idof=spaceCompo)
CALL obj%GetSingle(VALUE=VALUE, indx=indx)
END PROCEDURE obj_Get5

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get6
CALL obj%GetFEVariable(VALUE=VALUE, globalNode=globalNode, islocal=islocal)
END PROCEDURE obj_Get6

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get7
CALL obj%Get(idof=spaceCompo, ivar=1_I4B, VALUE=VALUE, &
             idof_value=spaceCompo, ivar_value=1_I4B)
END PROCEDURE obj_Get7

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get8
#ifdef DEBUG_VER

CHARACTER(*), PARAMETER :: myName = "obj_Set9()"

CALL AssertError1(obj%isInitiated, myName, &
                  'VectorField_::obj is not initiated')

CALL AssertError1(VALUE%isInitiated, myName, &
                  'VectorField_::value is not initiated')
#endif

CALL VALUE%Copy(obj)
END PROCEDURE obj_Get8

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get9
CHARACTER(*), PARAMETER :: myName = "obj_Get9()"

INTEGER(I4B) :: s(3), tsize, p(3)
REAL(DFP), POINTER :: realvec(:)

#ifdef DEBUG_VER

CALL AssertError1(obj%isInitiated, myName, &
                  "STScalarField_:: obj is not initiated")

CALL AssertError1(VALUE%isInitiated, myName, &
                  "STScalarField_:: value is not initiated")

#endif

s = GetNodeLoc(obj=obj%dof, idof=idof)

SELECT TYPE (VALUE)

TYPE IS (ScalarField_)

  realvec => VALUE%GetPointer()
  CALL obj%GetMultiple(istart=s(1), iend=s(2), stride=s(3), VALUE=realvec, &
                       tsize=tsize)
  realvec => NULL()

TYPE IS (STScalarField_)

  p = GetNodeLoc(obj=VALUE%dof, idof=idof_value)
  realvec => VALUE%GetPointer()

  CALL obj%GetMultiple(istart=s(1), iend=s(2), stride=s(3), VALUE=realvec, &
                      istart_value=p(1), iend_value=p(2), stride_value=p(3), &
                       tsize=tsize)
  realvec => NULL()

TYPE IS (VectorField_)

  p = GetNodeLoc(obj=VALUE%dof, idof=idof_value)
  realvec => VALUE%GetPointer()

  CALL obj%GetMultiple(istart=s(1), iend=s(2), stride=s(3), VALUE=realvec, &
                      istart_value=p(1), iend_value=p(2), stride_value=p(3), &
                       tsize=tsize)
  realvec => NULL()

! TYPE IS (STVectorField_)

  ! p = GetNodeLoc(obj=VALUE%dof, idof=idof_value)
  ! realvec => VALUE%GetPointer()
  !
  ! CALL obj%GetMultiple(istart=s(1), iend=s(2), stride=s(3), VALUE=realvec, &
  !                     istart_value=p(1), iend_value=p(2), stride_value=p(3), &
  !                      tsize=tsize)
  ! realvec => NULL()

TYPE IS (ScalarFieldLis_)
  CALL VALUE%Set(ivar=1, idof=1, VALUE=obj, ivar_value=ivar, idof_value=idof)

TYPE IS (STScalarFieldLis_)
  CALL VALUE%Set(ivar=1, idof=idof_value, VALUE=obj, ivar_value=ivar, idof_value=idof)

TYPE IS (VectorFieldLis_)
  CALL VALUE%Set(ivar=1, idof=idof_value, VALUE=obj, ivar_value=ivar, idof_value=idof)

CLASS DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTENRAL ERROR] :: No case found for the type of value')
  RETURN
END SELECT

END PROCEDURE obj_Get9

!----------------------------------------------------------------------------
!                                                           GetFEVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFEVariable
REAL(DFP) :: v(obj%spaceCompo, SIZE(globalNode))
INTEGER(I4B) :: nrow, jj

!$OMP PARALLEL DO PRIVATE(jj)
DO jj = 1, SIZE(v, 2)
  CALL obj%Get(VALUE=v(:, jj), globalNode=globalNode(jj), tsize=nrow)
END DO
!$OMP END PARALLEL DO

VALUE = NodalVariable(v, TypeFEVariableVector, TypeFEVariableSpace)
END PROCEDURE obj_GetFEVariable

!----------------------------------------------------------------------------
!                                                                  GetPrefix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPrefix
ans = myprefix
END PROCEDURE obj_GetPrefix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetMethods
