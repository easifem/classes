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

SUBMODULE(AbstractNodeField_Class) SetMethods
USE InputUtility, ONLY: Input
USE RealVector_Method, ONLY: Set, Add

#ifdef USE_LIS
#include "lisf.h"
#endif

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             SetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetParam
INTEGER(I4B) :: ii, tsize1

IF (PRESENT(dof_tPhysicalVars)) obj%dof_tPhysicalVars = dof_tPhysicalVars
IF (PRESENT(dof_storageFMT)) obj%dof_storageFMT = dof_storageFMT
IF (PRESENT(dof_spaceCompo)) obj%dof_spaceCompo = dof_spaceCompo
IF (PRESENT(dof_timeCompo)) obj%dof_timeCompo = dof_timeCompo
IF (PRESENT(dof_tNodes)) obj%dof_tNodes = dof_tNodes
IF (PRESENT(tSize)) obj%tsize = tsize

IF (PRESENT(dof_names_char)) THEN
  IF (ALLOCATED(obj%dof_names_char)) DEALLOCATE (obj%dof_names_char)
  tsize1 = SIZE(dof_names_char)
  ALLOCATE (obj%dof_names_char(tsize1))

  DO ii = 1, tsize1
    obj%dof_names_char(ii) (1:1) = dof_names_char(ii) (1:1)
  END DO
END IF

END PROCEDURE obj_SetParam

!----------------------------------------------------------------------------
!                                                                 SetSingle
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSingle
#ifdef USE_LIS
INTEGER(I4B) :: ierr, code
#endif

REAL(DFP) :: areal
LOGICAL(LGT) :: abool

abool = Input(option=AddContribution, default=.FALSE.)

! NATIVE_SERIAL engine

IF (obj%engine%chars() .EQ. "NATIVE_SERIAL") THEN

  IF (abool) THEN
    areal = Input(option=scale, default=1.0_DFP)
    CALL Add(obj%realVec, nodenum=indx, VALUE=VALUE, scale=areal)
    RETURN
  END IF

  CALL Set(obj%realVec, nodenum=indx, VALUE=VALUE)

  RETURN
END IF

! NATIVE_SERIAL ends here

! LIS_OMP engine

#ifdef USE_LIS

areal = Input(option=scale, default=1.0_DFP) * VALUE

IF (abool) THEN; code = LIS_ADD_VALUE; ELSE; code = LIS_INS_VALUE; END IF

CALL lis_vector_set_value(code, indx, areal, obj%lis_ptr, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

#endif
! end of USE_LIS

END PROCEDURE obj_SetSingle

!----------------------------------------------------------------------------
!                                                                 SetSingle
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMultiple1
#ifdef USE_LIS
INTEGER(I4B) :: ierr, code
#endif

LOGICAL(LGT) :: abool
REAL(DFP) :: areal

abool = Input(option=addContribution, default=.FALSE.)

IF (obj%engine%chars() .EQ. "NATIVE_SERIAL") THEN

  IF (abool) THEN
    areal = Input(option=scale, default=1.0_DFP)
    CALL Add(obj%realVec, VALUE=VALUE, scale=areal, nodenum=indx)
    RETURN
  END IF

  CALL Set(obj%realVec, VALUE=VALUE, nodenum=indx)

  RETURN
END IF

! LIS_OMP engine
#ifdef USE_LIS
areal = Input(option=scale, default=1.0_DFP)
IF (abool) THEN; code = LIS_ADD_VALUE; ELSE; code = LIS_INS_VALUE; END IF
CALL lis_vector_set_values4(code, SIZE(indx), indx, VALUE, obj%lis_ptr, &
                            areal, ierr)
#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif
#endif
! end of USE_LIS

END PROCEDURE obj_SetMultiple1

!----------------------------------------------------------------------------
!                                                                 SetSingle
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMultiple2
#ifdef USE_LIS
INTEGER(I4B) :: ierr, code, tsize
#endif

LOGICAL(LGT) :: abool
REAL(DFP) :: areal

abool = Input(option=addContribution, default=.FALSE.)

IF (obj%engine%chars() .EQ. "NATIVE_SERIAL") THEN

  IF (abool) THEN
    areal = Input(option=scale, default=1.0_DFP)
    CALL Add(obj%realVec, VALUE=VALUE, scale=areal, istart=istart, &
             iend=iend, stride=stride)
    RETURN
  END IF

  CALL Set(obj%realVec, VALUE=VALUE, istart=istart, &
           iend=iend, stride=stride)

  RETURN
END IF

! LIS_OMP engine

! LIS_INT lis_vector_set_values5(LIS_INT flag, LIS_INT start, LIS_INT stride,
!                                LIS_INT count, LIS_SCALAR value[], LIS_VECTOR v,
!                                LIS_SCALAR scale) {

#ifdef USE_LIS
IF (abool) THEN; code = LIS_ADD_VALUE; ELSE; code = LIS_INS_VALUE; END IF
areal = Input(option=scale, default=1.0_DFP)
tsize = (iend - istart) / stride + 1
CALL lis_vector_set_values5(code, istart, stride, tsize, VALUE, obj%lis_ptr, &
                            areal, ierr)
#endif

END PROCEDURE obj_SetMultiple2

!----------------------------------------------------------------------------
!                                                                 SetSingle
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMultiple3
#ifdef USE_LIS
INTEGER(I4B) :: ierr, code, tsize
#endif

LOGICAL(LGT) :: abool
REAL(DFP) :: areal

abool = Input(option=addContribution, default=.FALSE.)

IF (obj%engine%chars() .EQ. "NATIVE_SERIAL") THEN

  IF (abool) THEN
    areal = Input(option=scale, default=1.0_DFP)
    CALL Add(obj=obj%realVec, VALUE=VALUE, scale=areal, istart=istart, &
             iend=iend, stride=stride, istart_value=istart_value, &
             iend_value=iend_value, stride_value=stride_value)
    RETURN
  END IF

  CALL Set(obj=obj%realVec, VALUE=VALUE, istart=istart, &
           iend=iend, stride=stride, istart_value=istart_value, &
           iend_value=iend_value, stride_value=stride_value)

  RETURN
END IF

! LIS_OMP engine

! LIS_INT lis_vector_set_values8(LIS_INT flag, LIS_INT start, LIS_INT stride,
!                                LIS_INT count, LIS_SCALAR value[], LIS_VECTOR v,
!                                LIS_SCALAR scale, LIS_INT start_value,
!                                LIS_INT stride_value) {

#ifdef USE_LIS
IF (abool) THEN; code = LIS_ADD_VALUE; ELSE; code = LIS_INS_VALUE; END IF
areal = Input(option=scale, default=1.0_DFP)
tsize = (iend - istart) / stride + 1
CALL lis_vector_set_values8(code, istart, stride, tsize, VALUE, obj%lis_ptr, &
                            areal, istart_value, stride_value, ierr)
#endif
END PROCEDURE obj_SetMultiple3

!----------------------------------------------------------------------------
!                                                                 SetSingle
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMultiple4
#ifdef USE_LIS
INTEGER(I4B) :: ierr, code, tsize
#endif

LOGICAL(LGT) :: abool
REAL(DFP) :: areal

abool = Input(option=addContribution, default=.FALSE.)

IF (obj%engine%chars() .EQ. "NATIVE_SERIAL") THEN

  IF (abool) THEN
    areal = Input(option=scale, default=1.0_DFP)
    CALL Add(obj%realVec, VALUE=VALUE, scale=areal, istart=istart, &
             iend=iend, stride=stride)
    RETURN
  END IF

  CALL Set(obj%realVec, VALUE=VALUE, istart=istart, &
           iend=iend, stride=stride)

  RETURN
END IF

! LIS_OMP engine
#ifdef USE_LIS
IF (abool) THEN; code = LIS_ADD_VALUE; ELSE; code = LIS_INS_VALUE; END IF
areal = Input(option=scale, default=1.0_DFP)
areal = areal * VALUE
tsize = (iend - istart) / stride + 1
CALL lis_vector_set_values6(code, istart, stride, tsize, areal, &
                            obj%lis_ptr, ierr)
#endif

END PROCEDURE obj_SetMultiple4

!----------------------------------------------------------------------------
!                                                                 SetAll
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetAll
#ifdef USE_LIS
INTEGER(I4B) :: ierr, ii, n
#endif

REAL(DFP) :: areal
LOGICAL(LGT) :: abool

abool = Input(option=AddContribution, default=.FALSE.)

! NATIVE_SERIAL engine
IF (obj%engine%chars() .EQ. "NATIVE_SERIAL") THEN
  IF (abool) THEN
    areal = Input(option=scale, default=1.0_DFP)
    CALL Add(obj%realVec, VALUE=VALUE, scale=areal)
    RETURN
  END IF

  CALL Set(obj%realVec, VALUE=VALUE)

  RETURN

END IF
! end of NATIVE_SERIAL

! LIS_OMP engine
#ifdef USE_LIS

areal = Input(option=scale, default=1.0_DFP)
areal = areal * VALUE

IF (.NOT. abool) THEN
  CALL lis_vector_set_all(areal, obj%lis_ptr, ierr)

#ifdef DEBUG_VER
  CALL CHKERR(ierr)
#endif

  RETURN
END IF

n = obj%SIZE()

DO ii = 1, n
  CALL lis_vector_set_value(LIS_ADD_VALUE, ii, areal, obj%lis_ptr, ierr)
END DO

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

#endif

!end of USE_LIS

END PROCEDURE obj_SetAll

!----------------------------------------------------------------------------
!                                                             SetByFunction
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetByFunction
CHARACTER(*), PARAMETER :: myName = "obj_SetByFunction()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
END PROCEDURE obj_SetByFunction

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods
