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

SUBMODULE(STScalarFieldLis_Class) SetMethods
USE GlobalData, ONLY: NODES_FMT, DOF_FMT

USE ScalarField_Class, ONLY: ScalarField_

USE ScalarFieldLis_Class, ONLY: ScalarFieldLis_

USE Display_Method, ONLY: ToString

USE AbstractField_Class, ONLY: TypeField

USE InputUtility, ONLY: Input

USE SafeSizeUtility, ONLY: SafeSize

USE ReallocateUtility, ONLY: Reallocate

USE DOF_Method, ONLY: GetIndex, &
                      GetIndex_, &
                      GetNodeLoc, &
                      GetNodeLoc_, &
                      OPERATOR(.tNodes.), &
                      GetIDOF

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
!                                                                        Set
!----------------------------------------------------------------------------

! MODULE PROCEDURE obj_Set6
! REAL(DFP), POINTER :: vecPointer(:)
! CHARACTER(*), PARAMETER :: myName = "obj_Set6()"
! INTEGER(I4B) :: ierr, tsize, s(3), code
! REAL(DFP) :: areal
! LOGICAL(LGT) :: abool
!
! #ifdef DEBUG_VER
! INTEGER(I4B) :: tsize1
!
! #include "./lis_null_error.F90"
!
! CALL AssertError1(timeCompo .LE. obj%timeCompo, myName, &
!                   "timeCompo is out of bound")
!
! CALL AssertError1(obj%fieldType .NE. TypeField%constant, myName, &
!                   "Not callable for constant STScalar field")
!
! CALL AssertError2(obj%dof.tNodes.timeCompo, VALUE%dof.tNodes.1, myName, &
!                   "a=obj%dof.tNodes.timeCompo, b=VALUE%dof.tNodes.1 ")
! #endif
!
! SELECT TYPE (VALUE)
!
! TYPE IS (ScalarField_)
!
!   vecPointer => VALUE%GetPointer()
!   CALL obj%Set(VALUE=vecPointer, timeCompo=timeCompo, &
!                scale=scale, addContribution=addContribution)
!   vecPointer => NULL()
!
! TYPE IS (ScalarFieldLis_)
!
! #ifdef DEBUG_VER
!
!   CALL lis_vector_is_null(VALUE%lis_ptr, ierr)
!
!   IF (ierr .EQ. LIS_TRUE) THEN
!     CALL e%RaiseError(modName//'::'//myName//" - "// &
!         "[INTERNAL ERROR] :: ScalarFieldLis_::value%lis_ptr is not available")
!     RETURN
!   END IF
!
! #endif
!
!   abool = Input(option=addContribution, default=.FALSE.)
!   IF (abool) THEN; code = LIS_ADD_VALUE; ELSE; code = LIS_INS_VALUE; END IF
!   areal = Input(option=scale, default=1.0_DFP)
!   tsize = obj%dof.tNodes.timeCompo
!   s = GetNodeLoc(obj=VALUE%dof, idof=1)
!
!   ! void lis_vector_set_values7_f(LIS_INT *flag, LIS_INT *start, LIS_INT *stride,
!   ! LIS_INT *count, LIS_VECTOR_F *values,
!   ! LIS_VECTOR_F *v, LIS_SCALAR *scale,
!   ! LIS_INT *ierr)
!
!   CALL lis_vector_set_values7(code, s(1), s(3), tsize, VALUE%lis_ptr, &
!                               obj%lis_ptr, areal, ierr)
!
! #ifdef DEBUG_VER
!   CALL CHKERR(ierr)
! #endif
!
! CLASS DEFAULT
!   CALL e%RaiseError(modName//'::'//myName//' - '// &
!                     '[INTERNAL ERROR] :: No case found for the type of Value')
!   RETURN
! END SELECT
!
! END PROCEDURE obj_Set6

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set13
CHARACTER(*), PARAMETER :: myName = "obj_Set13()"
REAL(DFP), POINTER :: realvec(:)
INTEGER(I4B) :: ierr, tsize, s(3), p(3), code
REAL(DFP) :: areal
LOGICAL(LGT) :: abool

abool = Input(option=addContribution, default=.FALSE.)
areal = Input(option=scale, default=1.0_DFP)
IF (abool) THEN; code = LIS_ADD_VALUE; ELSE; code = LIS_INS_VALUE; END IF

s = GetNodeLoc(obj=obj%dof, idof=GetIDOF(obj=obj%dof, ivar=ivar, idof=idof))

! void lis_vector_set_values8_f(LIS_INT *flag, LIS_INT *start,
! LIS_INT *stride,
! LIS_INT *count, LIS_SCALAR *values,
! LIS_VECTOR_F *v, LIS_SCALAR *scale,
! LIS_INT *start_value, LIS_INT *stride_value,
! LIS_INT *ierr)

tsize = obj%dof.tNodes. [ivar, idof]

SELECT TYPE (VALUE)

TYPE IS (ScalarField_)

  p = GetNodeLoc(obj=VALUE%dof, idof=1_I4B)

  realvec => VALUE%GetPointer()

  CALL lis_vector_set_values8(code, s(1), s(3), tsize, realvec, obj%lis_ptr, &
                              areal, p(1), p(3), ierr)

  realvec => NULL()

TYPE IS (ScalarFieldLis_)

  p = GetNodeLoc(obj=VALUE%dof, idof=1_I4B)
  CALL lis_vector_set_values9(code, s(1), s(3), tsize, VALUE%lis_ptr, &
                              obj%lis_ptr, areal, p(1), p(3), ierr)

TYPE IS (STScalarField_)

  p = GetNodeLoc(obj=VALUE%dof, idof=GetIDOF(obj=VALUE%dof, &
                                            ivar=ivar_value, idof=idof_value))

  realvec => VALUE%GetPointer()
  CALL lis_vector_set_values8(code, s(1), s(3), tsize, realvec, obj%lis_ptr, &
                              areal, p(1), p(3), ierr)
  realvec => NULL()

TYPE IS (STScalarFieldLis_)

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
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set14
CHARACTER(*), PARAMETER :: myName = "obj_Set14()"
INTEGER(I4B) :: ierr
REAL(DFP), POINTER :: realvec(:)

#include "./lis_null_error.F90"

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated, myName, &
                  "STScalarFieldLis_::obj is not initiated")
#endif

SELECT TYPE (VALUE)

TYPE IS (STScalarField_)

  realvec => VALUE%GetPointer()
  CALL obj%Set(VALUE=realvec)
  realvec => NULL()

TYPE is (STScalarFieldLis_)

  CALL lis_vector_copy(VALUE%lis_ptr, obj%lis_ptr, ierr)

#ifdef DEBUG_VER
  CALL CHKERR(ierr)
#endif

CLASS DEFAULT

  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: Unknown type of ScalarField_::value')
  RETURN

END SELECT

END PROCEDURE obj_Set14

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SetMethods
