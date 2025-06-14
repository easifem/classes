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

USE VectorField_Class, ONLY: VectorField_

USE VectorFieldLis_Class, ONLY: VectorFieldLis_

USE Display_Method, ONLY: ToString

USE FieldOpt_Class, ONLY: TypeField => TypeFieldOpt

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

tsize = obj%dof.tNodes. [ivar, idof]

SELECT TYPE (VALUE)

TYPE IS (ScalarField_)
  realvec => VALUE%GetPointer()

  CALL obj%SetMultiple(VALUE=realvec, scale=scale, &
         addContribution=addContribution, istart=s(1), iend=s(2), stride=s(3))

  realvec => NULL()

TYPE IS (STScalarField_)

  p = GetNodeLoc(obj=VALUE%dof, idof=GetIDOF(obj=VALUE%dof, &
                                            ivar=ivar_value, idof=idof_value))

  realvec => VALUE%GetPointer()

  CALL obj%SetMultiple(VALUE=realvec, scale=scale, &
       addContribution=addContribution, istart=s(1), iend=s(2), stride=s(3), &
                       istart_value=p(1), iend_value=p(2), stride_value=p(3))

  realvec => NULL()

TYPE IS (VectorField_)

  p = GetNodeLoc(obj=VALUE%dof, idof=GetIDOF(obj=VALUE%dof, &
                                            ivar=ivar_value, idof=idof_value))

  realvec => VALUE%GetPointer()

  CALL obj%SetMultiple(VALUE=realvec, scale=scale, &
       addContribution=addContribution, istart=s(1), iend=s(2), stride=s(3), &
                       istart_value=p(1), iend_value=p(2), stride_value=p(3))

  realvec => NULL()

TYPE IS (ScalarFieldLis_)

  p = GetNodeLoc(obj=VALUE%dof, idof=1_I4B)
  CALL lis_vector_set_values9(code, s(1), s(3), tsize, VALUE%lis_ptr, &
                              obj%lis_ptr, areal, p(1), p(3), ierr)

  CALL CHKERR(ierr)

TYPE IS (STScalarFieldLis_)

  p = GetNodeLoc(obj=VALUE%dof, idof=GetIDOF(obj=VALUE%dof, &
                                            ivar=ivar_value, idof=idof_value))

  CALL lis_vector_set_values9(code, s(1), s(3), tsize, VALUE%lis_ptr, &
                              obj%lis_ptr, areal, p(1), p(3), ierr)

  CALL CHKERR(ierr)

TYPE IS (VectorFieldLis_)

  p = GetNodeLoc(obj=VALUE%dof, idof=GetIDOF(obj=VALUE%dof, &
                                            ivar=ivar_value, idof=idof_value))

  CALL lis_vector_set_values9(code, s(1), s(3), tsize, VALUE%lis_ptr, &
                              obj%lis_ptr, areal, p(1), p(3), ierr)

  CALL CHKERR(ierr)

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

SELECT TYPE (VALUE)

TYPE IS (STScalarField_)

  realvec => VALUE%GetPointer()
  CALL obj%Set(VALUE=realvec)
  realvec => NULL()

TYPE is (STScalarFieldLis_)

  CALL lis_vector_copy(VALUE%lis_ptr, obj%lis_ptr, ierr)
  CALL CHKERR(ierr)

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
