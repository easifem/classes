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

SUBMODULE(ScalarFieldLis_Class) SetMethods

USE Display_Method, ONLY: ToString

USE STScalarField_Class, ONLY: STScalarField_

USE STScalarFieldLis_Class, ONLY: STScalarFieldLis_

USE VectorField_Class, ONLY: VectorField_

USE VectorFieldLis_Class, ONLY: VectorFieldLis_

#ifdef _TODO_
USE STVectorField_Class, ONLY: STVectorField_

USE STVectorFieldLis_Class, ONLY: STVectorFieldLis_
#endif

USE InputUtility, ONLY: Input

USE RealVector_Method, ONLY: GetPointer

USE DOF_Method, ONLY: OPERATOR(.tNodes.), &
                      GetIDOF, &
                      GetNodeLoc

#include "lisf.h"

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                       Set9
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set9
#ifdef DEBUG_VER
LOGICAL(LGT) :: isok
#endif

CHARACTER(*), PARAMETER :: myName = "obj_Set9()"
INTEGER(I4B) :: s(3), p(3), code, tsize, ierr
REAL(DFP) :: areal
LOGICAL(LGT) :: abool
REAL(DFP), POINTER :: realvec(:)

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated, myName, "ScalarField_::obj not initiated")
CALL AssertError1(VALUE%isInitiated, myName, &
                  "AbstractNodeField_::value not initiated")
#endif

s = GetNodeLoc(obj=obj%dof, idof=1_I4B)

SELECT TYPE (VALUE)

TYPE IS (ScalarField_)

  realvec => VALUE%GetPointer()

  CALL obj%SetMultiple(istart=s(1), iend=s(2), stride=s(3), &
                  VALUE=realvec, scale=scale, addContribution=addContribution)

  realvec => NULL()

TYPE IS (STScalarField_)

  p = GetNodeLoc(obj=VALUE%dof, idof=GetIDOF(obj=VALUE%dof, ivar=1_I4B, &
                                             idof=idof_value))
  realvec => VALUE%GetPointer()

  CALL obj%SetMultiple(istart=s(1), iend=s(2), stride=s(3), &
                      istart_value=p(1), iend_value=p(2), stride_value=p(3), &
                  VALUE=realvec, scale=scale, addContribution=addContribution)

  realvec => NULL()

TYPE IS (VectorField_)

  p = GetNodeLoc(obj=VALUE%dof, idof=GetIDOF(obj=VALUE%dof, ivar=1_I4B, &
                                             idof=idof_value))
  realvec => VALUE%GetPointer()

  CALL obj%SetMultiple(istart=s(1), iend=s(2), stride=s(3), &
                      istart_value=p(1), iend_value=p(2), stride_value=p(3), &
                  VALUE=realvec, scale=scale, addContribution=addContribution)

  realvec => NULL()

#ifdef _TODO_

TYPE is (STVectorField_)

  p = GetNodeLoc(obj=VALUE%dof, idof=GetIDOF(obj=VALUE%dof, ivar=1_I4B, &
                                             idof=idof_value))
  realvec => VALUE%GetPointer()

  CALL obj%SetMultiple(istart=s(1), iend=s(2), stride=s(3), &
                      istart_value=p(1), iend_value=p(2), stride_value=p(3), &
                  VALUE=realvec, scale=scale, addContribution=addContribution)

  realvec => NULL()

#endif

TYPE is (ScalarFieldLis_)

  abool = Input(option=addContribution, default=.FALSE.)
  areal = Input(option=scale, default=1.0_DFP)
  IF (abool) THEN; code = LIS_ADD_VALUE; ELSE; code = LIS_INS_VALUE; END IF
  p = GetNodeLoc(obj=VALUE%dof, idof=1_I4B)
  tsize = obj%dof.tNodes.1
  CALL lis_vector_set_values9(code, s(1), s(3), tsize, VALUE%lis_ptr, &
                              obj%lis_ptr, areal, p(1), p(3), ierr)
  CALL CHKERR(ierr)

TYPE IS (STScalarFieldLis_)

  abool = Input(option=addContribution, default=.FALSE.)
  areal = Input(option=scale, default=1.0_DFP)
  IF (abool) THEN; code = LIS_ADD_VALUE; ELSE; code = LIS_INS_VALUE; END IF
  p = GetNodeLoc(obj=VALUE%dof, idof=GetIDOF(obj=VALUE%dof, ivar=1_I4B, &
                                             idof=idof_value))
  tsize = obj%dof.tNodes.1
  CALL lis_vector_set_values9(code, s(1), s(3), tsize, VALUE%lis_ptr, &
                              obj%lis_ptr, areal, p(1), p(3), ierr)
  CALL CHKERR(ierr)

TYPE IS (VectorFieldLis_)

  abool = Input(option=addContribution, default=.FALSE.)
  areal = Input(option=scale, default=1.0_DFP)
  IF (abool) THEN; code = LIS_ADD_VALUE; ELSE; code = LIS_INS_VALUE; END IF
  p = GetNodeLoc(obj=VALUE%dof, idof=GetIDOF(obj=VALUE%dof, ivar=1_I4B, &
                                             idof=idof_value))
  tsize = obj%dof.tNodes.1
  CALL lis_vector_set_values9(code, s(1), s(3), tsize, VALUE%lis_ptr, &
                              obj%lis_ptr, areal, p(1), p(3), ierr)
  CALL CHKERR(ierr)

#ifdef _TODO_

TYPE IS (STVectorFieldLis_)

  abool = Input(option=addContribution, default=.FALSE.)
  areal = Input(option=scale, default=1.0_DFP)
  IF (abool) THEN; code = LIS_ADD_VALUE; ELSE; code = LIS_INS_VALUE; END IF
  p = GetNodeLoc(obj=VALUE%dof, idof=GetIDOF(obj=VALUE%dof, ivar=1_I4B, &
                                             idof=idof_value))
  tsize = obj%dof.tNodes.1
  CALL lis_vector_set_values9(code, s(1), s(3), tsize, VALUE%lis_ptr, &
                              obj%lis_ptr, areal, p(1), p(3), ierr)
  CALL CHKERR(ierr)
#endif

CLASS DEFAULT

  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: No case found')
  RETURN

END SELECT

END PROCEDURE obj_Set9

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SetMethods
