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

SUBMODULE(STScalarField_Class) GetMethods
USE Display_Method, ONLY: ToString

USE FieldOpt_Class, ONLY: TypeField => TypeFieldOpt

USE RealVector_Method, ONLY: GetValue_

USE ArangeUtility, ONLY: Arange

USE BaseType, ONLY: TypeFEVariableScalar, TypeFEVariableSpaceTime

USE ScalarField_Class, ONLY: ScalarField_
USE ScalarFieldLis_Class, ONLY: ScalarFieldLis_

USE STScalarField_Class, ONLY: STScalarField_
USE STScalarFieldLis_Class, ONLY: STScalarFieldLis_

USE VectorField_Class, ONLY: VectorField_
USE VectorFieldLis_Class, ONLY: VectorFieldLis_

USE FEVariable_Method, ONLY: NodalVariable

USE DOF_Method, ONLY: GetIDOF, &
                      OPERATOR(.tnodes.), &
                      GetNodeLoc, &
                      GetNodeLoc_

IMPLICIT NONE

INTEGER(I4B), PARAMETER :: EXPAND_FACTOR = 2

INTEGER(I4B), PARAMETER :: TEMP_INTVEC_LEN = 128
INTEGER(I4B) :: TEMP_INTVEC(TEMP_INTVEC_LEN)
!$OMP THREADPRIVATE(TEMP_INTVEC)

INTEGER(I4B), ALLOCATABLE :: TEMP_DYNA_INTVEC(:)
!$OMP THREADPRIVATE(TEMP_DYNA_INTVEC)

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
INTEGER(I4B) :: ierr, ii, s(3), indx(obj%timeCompo)

bool1 = PRESENT(globalNode)
bool2 = PRESENT(timeCompo)

#ifdef DEBUG_VER

isok = .NOT. (bool1 .AND. bool2)

CALL AssertError1(isok, myName, &
                  "Both globalNode and timeCompo cannot be present")

isok = bool1 .OR. bool2
CALL AssertError1(isok, myName, &
                  "Either globalNode or timeCompo should be present")

IF (bool1) THEN
  isok = SIZE(VALUE) .GE. obj%timeCompo
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

!> Get all values of timeCompo
! IF (bool2) THEN

tsize = obj%dof.tNodes.1_I4B

s = GetNodeLoc(obj=obj%dof, idof=timeCompo)

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
INTEGER(I4B) :: indx(obj%timeCompo)

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated(), myName, &
                  "STScalarField_:: obj is not initiated")

IF (storageFMT .EQ. DOF_FMT) THEN
  nrow = obj%dof.tNodes.1
  ncol = obj%timeCompo

ELSE
  ncol = obj%dof.tNodes.1
  nrow = obj%timeCompo
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
  ncol = obj%timeCompo

  !$OMP PARALLEL DO PRIVATE(jj, mynrow, s)
  DO jj = 1, ncol
    s = GetNodeLoc(obj=obj%dof, idof=jj)

    CALL obj%GetMultiple(istart=s(1), iend=s(2), stride=s(3), &
                         VALUE=VALUE(:, jj), tsize=mynrow)

  END DO
  !$OMP END PARALLEL DO

  RETURN
END IF

nrow = obj%timeCompo
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
CALL AssertError1(obj%isInitiated(), myName, &
                  "STScalarField_:: obj is not initiated")

IF (storageFMT .EQ. DOF_FMT) THEN
  nrow = SIZE(globalNode)
  ncol = obj%timeCompo

ELSE
  ncol = SIZE(globalNode)
  nrow = obj%timeCompo
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
  ncol = obj%timeCompo

  !$OMP PARALLEL DO PRIVATE(jj, mynrow)
  DO jj = 1, ncol
    CALL obj%Get(globalNode=globalNode, islocal=islocal, VALUE=VALUE(:, jj), &
                 tsize=mynrow, timeCompo=jj)
  END DO
  !$OMP END PARALLEL DO

  RETURN
END IF

nrow = obj%timeCompo
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
                 idof=timeCompo, VALUE=VALUE, tsize=tsize, &
                 nodenum=globalNode)

ELSE

  CALL me_if_not_native

END IF

CONTAINS

SUBROUTINE me_if_not_native
  INTEGER(I4B) :: indx(SIZE(globalNode))
  CALL GetNodeLoc_(obj=obj%dof, nodenum=globalNode, idof=timeCompo, &
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

#include "./localNodeError.inc"

indx = GetNodeLoc(obj=obj%dof, nodenum=globalNode, idof=timeCompo)
CALL obj%GetSingle(indx=indx, VALUE=VALUE)

END PROCEDURE obj_Get5

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get6
CHARACTER(*), PARAMETER :: myName = "obj_Get6()"
REAL(DFP), ALLOCATABLE :: v(:, :)
INTEGER(I4B) :: nrow, ncol

nrow = obj%timeCompo
ncol = SIZE(globalNode)
ALLOCATE (v(nrow, ncol))

CALL obj%Get(VALUE=v, nrow=nrow, ncol=ncol, globalNode=globalNode, &
             islocal=islocal, storageFMT=NODES_FMT)

VALUE = NodalVariable(v, TypeFEVariableScalar, TypeFEVariableSpaceTime)

DEALLOCATE (v)
END PROCEDURE obj_Get6

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

! MODULE PROCEDURE obj_Get7
! CALL obj%Get(ivar=1_I4B, idof=timeCompo, VALUE=VALUE, ivar_value=1_I4B, &
!              idof_value=timeCompo)
! END PROCEDURE obj_Get7

!----------------------------------------------------------------------------
!                                                                     Get
!----------------------------------------------------------------------------

! MODULE PROCEDURE obj_Get8
! #ifdef DEBUG_VER
! CHARACTER(*), PARAMETER :: myName = "obj_Get8()"
! #endif
!
! INTEGER(I4B) :: s(3), tsize, p(3)
! REAL(DFP), POINTER :: realvec(:)
!
! #ifdef DEBUG_VER
!
! CALL AssertError1(obj%isInitiated(), myName, &
!                   "STScalarField_:: obj is not initiated")
!
! CALL AssertError1(VALUE%isInitiated(), myName, &
!                   "STScalarField_:: value is not initiated")
!
! #endif
!
! s = GetNodeLoc(obj=obj%dof, idof=idof)
!
! SELECT TYPE (VALUE)
!
! ! TYPE IS (ScalarField_)
! !
! !   realvec => VALUE%GetPointer()
! !   CALL obj%GetMultiple(istart=s(1), iend=s(2), stride=s(3), VALUE=realvec, &
! !                        tsize=tsize)
! !   realvec => NULL()
!
! TYPE IS (STScalarField_)
!
!   p = GetNodeLoc(obj=VALUE%dof, idof=idof_value)
!   realvec => VALUE%GetPointer()
!
!   CALL obj%GetMultiple(istart=s(1), iend=s(2), stride=s(3), VALUE=realvec, &
!                       istart_value=p(1), iend_value=p(2), stride_value=p(3), &
!                        tsize=tsize)
!   realvec => NULL()
!
! TYPE IS (VectorField_)
!
!   p = GetNodeLoc(obj=VALUE%dof, idof=idof_value)
!   realvec => VALUE%GetPointer()
!
!   CALL obj%GetMultiple(istart=s(1), iend=s(2), stride=s(3), VALUE=realvec, &
!                       istart_value=p(1), iend_value=p(2), stride_value=p(3), &
!                        tsize=tsize)
!   realvec => NULL()
!
! ! TYPE IS (STVectorField_)
!
!   ! p = GetNodeLoc(obj=VALUE%dof, idof=idof_value)
!   ! realvec => VALUE%GetPointer()
!   !
!   ! CALL obj%GetMultiple(istart=s(1), iend=s(2), stride=s(3), VALUE=realvec, &
!   !                     istart_value=p(1), iend_value=p(2), stride_value=p(3), &
!   !                      tsize=tsize)
!   ! realvec => NULL()
!
! ! TYPE IS (ScalarFieldLis_)
! !   CALL VALUE%Set(ivar=1, idof=1, VALUE=obj, ivar_value=ivar, idof_value=idof)
!
! TYPE IS (STScalarFieldLis_)
!   CALL VALUE%Set(ivar=1, idof=idof_value, VALUE=obj, ivar_value=ivar, &
!                  idof_value=idof)
!
! TYPE IS (VectorFieldLis_)
!   CALL VALUE%Set(ivar=1, idof=idof_value, VALUE=obj, ivar_value=ivar, &
!                  idof_value=idof)
!
! CLASS DEFAULT
!   CALL e%RaiseError(modName//'::'//myName//' - '// &
!                     '[INTENRAL ERROR] :: No case found for the type of value')
!   RETURN
! END SELECT
!
! END PROCEDURE obj_Get8

!----------------------------------------------------------------------------
!                                                              GetTimeCompo
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTimeCompo
ans = obj%timeCompo
END PROCEDURE obj_GetTimeCompo

!----------------------------------------------------------------------------
!                                                           GetFEVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFEVariable
CALL obj%Get(globalNode=globalNode, VALUE=VALUE, islocal=islocal)
END PROCEDURE obj_GetFEVariable

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetMethods
