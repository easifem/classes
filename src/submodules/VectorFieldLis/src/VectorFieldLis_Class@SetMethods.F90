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

SUBMODULE(VectorFieldLis_Class) SetMethods
USE InputUtility, ONLY: Input

USE SafeSizeUtility, ONLY: SafeSize

USE Display_Method, ONLY: ToString

USE AbstractField_Class, ONLY: TypeField

USE ScalarField_Class, ONLY: ScalarField_

USE STScalarField_Class, ONLY: STScalarField_

USE ScalarFieldLis_Class, ONLY: ScalarFieldLis_

USE STScalarFieldLis_Class, ONLY: STScalarFieldLis_

USE DOF_Method, ONLY: GetIndex_, &
                      GetIndex, &
                      GetNodeLoc, &
                      GetNodeLoc_, &
                      GetIDOF, &
                      OPERATOR(.tNodes.)

IMPLICIT NONE

INTEGER(I4B), PARAMETER :: EXPAND_FACTOR = 2

INTEGER(I4B), PARAMETER :: TEMP_INTVEC_LEN = 128
INTEGER(I4B) :: TEMP_INTVEC(TEMP_INTVEC_LEN)
!$OMP THREADPRIVATE(TEMP_INTVEC)

INTEGER(I4B), ALLOCATABLE :: TEMP_DYNA_INTVEC(:)
!$OMP THREADPRIVATE(TEMP_DYNA_INTVEC)

#include "lisf.h"

CONTAINS

!----------------------------------------------------------------------------
!                                                                 SetSingle
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSingle
INTEGER(I4B) :: ierr
REAL(DFP) :: value0
LOGICAL(LGT) :: abool

abool = Input(option=addContribution, default=.FALSE.)

IF (abool) THEN
  value0 = Input(option=scale, default=1.0_DFP) * VALUE
  CALL lis_vector_set_value(LIS_ADD_VALUE, indx, value0, obj%lis_ptr, ierr)

ELSE
  CALL lis_vector_set_value(LIS_INS_VALUE, indx, VALUE, obj%lis_ptr, ierr)

END IF

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

END PROCEDURE obj_SetSingle

!----------------------------------------------------------------------------
!                                                               SetMultiple
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMultiple1
INTEGER(I4B) :: ierr, n
LOGICAL(LGT) :: abool
REAL(DFP) :: areal

n = SIZE(indx)

abool = Input(option=addContribution, default=.FALSE.)

IF (abool) THEN
  areal = Input(option=scale, default=1.0_DFP)
  CALL lis_vector_set_values4(LIS_ADD_VALUE, n, indx, VALUE, obj%lis_ptr, &
                              areal, ierr)

ELSE
  CALL lis_vector_set_values(LIS_INS_VALUE, n, indx, VALUE, obj%lis_ptr, &
                             ierr)

END IF

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif
END PROCEDURE obj_SetMultiple1

!----------------------------------------------------------------------------
!                                                                     SetAll
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetAll
INTEGER(I4B) :: ierr, ii, n
REAL(DFP) :: value0
LOGICAL(LGT) :: abool

value0 = Input(option=scale, default=1.0_DFP) * VALUE
abool = Input(option=addContribution, default=.FALSE.)

IF (.NOT. abool) THEN
  CALL lis_vector_set_all(value0, obj%lis_ptr, ierr)

#ifdef DEBUG_VER
  CALL CHKERR(ierr)
#endif

  RETURN
END IF

n = obj%SIZE()
DO ii = 1, n
  CALL lis_vector_set_value(LIS_ADD_VALUE, ii, value0, obj%lis_ptr, ierr)
END DO

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

END PROCEDURE obj_SetAll

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set1
CHARACTER(*), PARAMETER :: myName = "obj_Set1()"
INTEGER(I4B) :: ierr, tsize

#include "./lis_null_error.F90"
#include "./localNodeError.F90"

#ifdef DEBUG_VER

CALL AssertError2(SIZE(VALUE), obj%spaceCompo, myName, &
                  "a=size(value), b=obj%spaceCompo")

#endif

IF (obj%spaceCompo .LE. TEMP_INTVEC_LEN) THEN
  CALL GetIndex_(obj=obj%dof, nodenum=globalNode, ans=TEMP_INTVEC, &
                 tsize=tsize)

  CALL obj%SetMultiple(indx=TEMP_INTVEC(1:tsize), VALUE=VALUE, scale=scale, &
                       addContribution=addContribution)

  RETURN
END IF

CALL obj%SetMultiple(indx=GetIndex(obj=obj%dof, nodenum=globalNode), &
                    VALUE=VALUE, scale=scale, addContribution=addContribution)

END PROCEDURE obj_Set1

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set2
CHARACTER(*), PARAMETER :: myName = "obj_Set2()"
INTEGER(I4B) :: ii, tsize, ierr, s(3), code
LOGICAL(LGT) :: abool
REAL(DFP) :: areal

#include "./lis_null_error.F90"

#ifdef DEBUG_VER

CALL AssertError2(SIZE(VALUE), obj%spaceCompo, myName, &
                  "size(value), obj%spaceCompo")

#endif

abool = Input(option=addContribution, default=.FALSE.)
areal = Input(option=scale, default=1.0_DFP)

IF (abool) THEN; code = LIS_ADD_VALUE; ELSE; code = LIS_INS_VALUE; END IF

!$OMP PARALLEL DO PRIVATE(ii, tsize, s, ierr)
DO ii = 1, obj%spaceCompo
  tsize = obj%dof.tNodes.ii
  s = GetNodeLoc(obj=obj%dof, idof=ii)
  ! void lis_vector_set_values6_f(LIS_INT *flag, LIS_INT *start,
  ! LIS_INT *stride, LIS_INT *count, LIS_SCALAR *values, LIS_VECTOR *v,
  ! LIS_INT *ierr)
  CALL lis_vector_set_values6(code, s(1), s(3), tsize, VALUE(ii) * areal, &
                              obj%lis_ptr, ierr)

#ifdef DEBUG_VER
  CALL CHKERR(ierr)
#endif

END DO
!$OMP END PARALLEL DO
END PROCEDURE obj_Set2

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set3
CHARACTER(*), PARAMETER :: myName = "obj_Set3()"
INTEGER(I4B) :: tsize, ierr, s(3), code
LOGICAL(LGT) :: abool
REAL(DFP) :: areal

#include "./lis_null_error.F90"

#ifdef DEBUG_VER

CALL AssertError2(spaceCompo, obj%spaceCompo, myName, &
                  "spaceCompo, obj%spaceCompo")
#endif

IF (abool) THEN; code = LIS_ADD_VALUE; ELSE; code = LIS_INS_VALUE; END IF
areal = Input(option=scale, default=1.0_DFP)
tsize = obj%dof.tNodes.spaceCompo
s = GetNodeLoc(obj=obj%dof, idof=spaceCompo)
! void lis_vector_set_values6_f(LIS_INT *flag, LIS_INT *start,
! LIS_INT *stride, LIS_INT *count, LIS_SCALAR *values, LIS_VECTOR *v,
! LIS_INT *ierr)
areal = areal * VALUE
CALL lis_vector_set_values6(code, s(1), s(3), tsize, areal, &
                            obj%lis_ptr, ierr)

END PROCEDURE obj_Set3

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set4
CHARACTER(*), PARAMETER :: myName = "obj_Set4()"
INTEGER(I4B) :: ii, tsize, ierr, code, s(3)
LOGICAL(LGT) :: abool
REAL(DFP) :: areal

#include "./lis_null_error.F90"

#ifdef DEBUG_VER

CALL AssertError1(obj%fieldType .NE. TypeField%constant, myName, &
                  "Not callable for constant STScalar field")

IF (storageFMT .EQ. NODES_FMT) THEN
  CALL AssertError2(SIZE(VALUE, 1), obj%spaceCompo, myName, &
                    "a=SIZE(VALUE, 1), b=obj%spaceCompo")

  CALL AssertError2(SIZE(VALUE, 2), obj%dof.tNodes.1, myName, &
                    "a=SIZE(VALUE, 2), b=obj%dof.tNodes.1")
ELSE

  CALL AssertError2(SIZE(VALUE, 2), obj%spaceCompo, myName, &
                    "a=SIZE(VALUE, 2), b=obj%spaceCompo")

  CALL AssertError2(SIZE(VALUE, 1), obj%dof.tNodes.1, myName, &
                    "a=SIZE(VALUE, 1), b=obj%dof.tNodes.1")

END IF

#endif

IF (storageFMT .EQ. DOF_FMT) THEN

  abool = Input(option=addContribution, default=.FALSE.)
  areal = Input(option=scale, default=1.0_DFP)
  IF (abool) THEN; code = LIS_ADD_VALUE; ELSE; code = LIS_INS_VALUE; END IF

  !$OMP PARALLEL DO PRIVATE(ii, tsize, s, ierr)
  DO ii = 1, obj%spaceCompo
    tsize = obj%dof.tNodes.ii
    s = GetNodeLoc(obj=obj%dof, idof=ii)
    ! void lis_vector_set_values5_f(LIS_INT *flag, LIS_INT *start,
    ! LIS_INT *stride,
    ! LIS_INT *count, LIS_SCALAR *values,
    ! LIS_VECTOR_F *v, LIS_SCALAR *scale,
    ! LIS_INT *ierr) {
    CALL lis_vector_set_values5(code, s(1), s(3), tsize, VALUE(:, ii), &
                                obj%lis_ptr, areal, ierr)

#ifdef DEBUG_VER
    CALL CHKERR(ierr)
#endif

  END DO
  !$OMP END PARALLEL DO

  RETURN
END IF

tsize = obj%dof.tNodes.1

!$OMP PARALLEL DO PRIVATE(ii, tsize)
DO ii = 1, tsize
  CALL obj%Set(VALUE=VALUE(:, ii), globalNode=ii, islocal=.TRUE., &
               addContribution=addContribution, scale=scale)
END DO
!$OMP END PARALLEL DO

END PROCEDURE obj_Set4

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set5
CHARACTER(*), PARAMETER :: myName = "obj_Set5()"
INTEGER(I4B) :: ierr, tsize, s(3), code
LOGICAL(LGT) :: abool
REAL(DFP) :: areal

#include "./lis_null_error.F90"

#ifdef DEBUG_VER

CALL AssertError1(spaceCompo .LE. obj%spaceCompo, myName, &
                  "spaceCompo is out of bound")

CALL AssertError1(obj%fieldType .NE. TypeField%constant, myName, &
                  "NOT callable for constant field")

CALL AssertError2(SIZE(VALUE), obj%dof.tNodes.spaceCompo, myName, &
                  "a=SIZE(VALUE), b=obj%dof.tNodes.spaceCompo")

#endif

abool = Input(option=addContribution, default=.FALSE.)
areal = Input(option=scale, default=1.0_DFP)
IF (abool) THEN; code = LIS_ADD_VALUE; ELSE; code = LIS_INS_VALUE; END IF

tsize = obj%dof.tNodes.spaceCompo
s = GetNodeLoc(obj=obj%dof, idof=spaceCompo)
! void lis_vector_set_values6_f(LIS_INT *flag, LIS_INT *start,
! LIS_INT *stride, LIS_INT *count, LIS_SCALAR *values, LIS_VECTOR *v,
! LIS_INT *ierr)
CALL lis_vector_set_values3(code, s(1), s(3), tsize, VALUE, &
                            obj%lis_ptr, areal, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

END PROCEDURE obj_Set5

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set9
CHARACTER(*), PARAMETER :: myName = "obj_Set9()"
INTEGER(I4B) :: ierr
INTEGER(I4B) :: tsize

#include "./lis_null_error.F90"

#ifdef DEBUG_VER

CALL AssertError1(spaceCompo .LE. obj%spaceCompo, myName, &
                  "spaceCompo is out of bound")

CALL AssertError1(obj%fieldType .NE. TypeField%constant, myName, &
                  "NOT callable for constant field")

CALL AssertError2(SIZE(VALUE), SIZE(globalNode), myName, &
                  "a=SIZE(VALUE), b=size(globalNode)")

#endif

#include "./localNodeError.F90"

tsize = SIZE(globalNode)

IF (tsize .LE. TEMP_INTVEC_LEN) THEN
  CALL GetNodeLoc_(obj=obj%dof, idof=spaceCompo, &
                   nodenum=globalNode, ans=TEMP_INTVEC, tsize=tsize)

  CALL obj%SetMultiple(indx=TEMP_INTVEC(1:tsize), VALUE=VALUE, scale=scale, &
                       addContribution=addContribution)

  RETURN

END IF

ierr = SafeSize(TEMP_DYNA_INTVEC)
IF (tsize .GT. ierr) THEN
  CALL Reallocate(TEMP_DYNA_INTVEC, EXPAND_FACTOR * tsize)
END IF

CALL GetNodeLoc_(obj=obj%dof, ivar=1, spaceCompo=spaceCompo, &
                 timeCompo=1_I4B, &
                 nodenum=globalNode, ans=TEMP_DYNA_INTVEC, tsize=tsize)

CALL obj%SetMultiple(indx=TEMP_DYNA_INTVEC(1:tsize), VALUE=VALUE, &
                     scale=scale, addContribution=addContribution)

END PROCEDURE obj_Set9

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set13
CHARACTER(*), PARAMETER :: myName = "obj_Set13()"
REAL(DFP), POINTER :: realvec(:)
INTEGER(I4B) :: ierr, tsize, s(3), p(3), code
REAL(DFP) :: areal
LOGICAL(LGT) :: abool

! void lis_vector_set_values8_f(LIS_INT *flag, LIS_INT *start,
! LIS_INT *stride,
! LIS_INT *count, LIS_SCALAR *values,
! LIS_VECTOR_F *v, LIS_SCALAR *scale,
! LIS_INT *start_value, LIS_INT *stride_value,
! LIS_INT *ierr)

abool = Input(option=addContribution, default=.FALSE.)
areal = Input(option=scale, default=1.0_DFP)
IF (abool) THEN; code = LIS_ADD_VALUE; ELSE; code = LIS_INS_VALUE; END IF

s = GetNodeLoc(obj=obj%dof, idof=GetIDOF(obj=obj%dof, ivar=ivar, idof=idof))

tsize = obj%dof.tNodes. [ivar, idof]

SELECT TYPE (VALUE)

TYPE IS (ScalarField_)

  p = GetNodeLoc(obj=VALUE%dof, idof=1_I4B)

  realvec => VALUE%GetPointer()

  CALL lis_vector_set_values8(code, s(1), s(3), tsize, realvec, obj%lis_ptr, &
                              areal, p(1), p(3), ierr)

  realvec => NULL()

TYPE IS (STScalarField_)

  p = GetNodeLoc(obj=VALUE%dof, idof=idof_value)

  realvec => VALUE%GetPointer()

  CALL lis_vector_set_values8(code, s(1), s(3), tsize, realvec, obj%lis_ptr, &
                              areal, p(1), p(3), ierr)

TYPE IS (VectorField_)

  p = GetNodeLoc(obj=VALUE%dof, idof=GetIDOF(obj=VALUE%dof, &
                                            ivar=ivar_value, idof=idof_value))

  realvec => VALUE%GetPointer()

  CALL lis_vector_set_values8(code, s(1), s(3), tsize, realvec, obj%lis_ptr, &
                              areal, p(1), p(3), ierr)

  realvec => NULL()

TYPE IS (ScalarFieldLis_)

  p = GetNodeLoc(obj=VALUE%dof, idof=1_I4B)

  CALL lis_vector_set_values9(code, s(1), s(3), tsize, VALUE%lis_ptr, &
                              obj%lis_ptr, areal, p(1), p(3), ierr)

TYPE IS (STScalarFieldLis_)

  p = GetNodeLoc(obj=VALUE%dof, idof=idof_value)

  CALL lis_vector_set_values9(code, s(1), s(3), tsize, VALUE%lis_ptr, &
                              obj%lis_ptr, areal, p(1), p(3), ierr)

TYPE IS (VectorFieldLis_)

  p = GetNodeLoc(obj=VALUE%dof, idof=GetIDOF(obj=VALUE%dof, &
                                            ivar=ivar_value, idof=idof_value))

  CALL lis_vector_set_values9(code, s(1), s(3), tsize, VALUE%lis_ptr, &
                              obj%lis_ptr, areal, p(1), p(3), ierr)

CLASS DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: Unknown type of ScalarField_::value')
  RETURN

END SELECT
END PROCEDURE obj_Set13

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SetMethods
