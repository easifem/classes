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
USE InputUtility, ONLY: Input
USE BaseType, ONLY: math => TypeMathOpt
USE DOF_Method, ONLY: GetNodeLoc
USE DOF_Method, ONLY: OPERATOR(.tNodes.)

#include "lisf.h"

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                 SetSingle
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSingle
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetSingle()"
#endif

INTEGER(I4B) :: ierr, code
REAL(DFP) :: areal
LOGICAL(LGT) :: abool

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

abool = Input(option=AddContribution, default=math%no)
areal = Input(option=scale, default=math%one)
areal = areal * VALUE

IF (abool) THEN; code = LIS_ADD_VALUE; ELSE; code = LIS_INS_VALUE; END IF

CALL lis_vector_set_value(code, indx, areal, obj%lis_ptr, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetSingle

!----------------------------------------------------------------------------
!                                                               SetMultiple
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMultiple1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetMultiple1()"
#endif

INTEGER(I4B) :: ierr, code
LOGICAL(LGT) :: abool
REAL(DFP) :: areal

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

abool = Input(option=addContribution, default=math%no)
areal = Input(option=scale, default=math%one)

IF (abool) THEN; code = LIS_ADD_VALUE; ELSE; code = LIS_INS_VALUE; END IF

CALL lis_vector_set_values4( &
  code, SIZE(indx), indx, VALUE, obj%lis_ptr, areal, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetMultiple1

!----------------------------------------------------------------------------
!                                                                 SetSingle
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMultiple2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetMultiple2()"
#endif
INTEGER(I4B) :: ierr, code, tsize
LOGICAL(LGT) :: abool
REAL(DFP) :: areal

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

abool = Input(option=addContribution, default=math%no)

! LIS_OMP engine
! LIS_INT lis_vector_set_values5(
! LIS_INT flag, LIS_INT start, LIS_INT stride,
! LIS_INT count, LIS_SCALAR value[], LIS_VECTOR v,
! LIS_SCALAR scale)

IF (abool) THEN; code = LIS_ADD_VALUE; ELSE; code = LIS_INS_VALUE; END IF

areal = Input(option=scale, default=math%one)
tsize = (iend - istart) / stride + 1

CALL lis_vector_set_values5( &
  code, istart, stride, tsize, VALUE, obj%lis_ptr, areal, ierr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetMultiple2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMultiple3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetMultiple3()"
#endif
INTEGER(I4B) :: ierr, code, tsize
LOGICAL(LGT) :: abool
REAL(DFP) :: areal

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

abool = Input(option=addContribution, default=math%no)

! LIS_INT lis_vector_set_values8(
! LIS_INT flag, LIS_INT start, LIS_INT stride,
! LIS_INT count, LIS_SCALAR value[], LIS_VECTOR v,
! LIS_SCALAR scale, LIS_INT start_value,
! LIS_INT stride_value)

IF (abool) THEN; code = LIS_ADD_VALUE; ELSE; code = LIS_INS_VALUE; END IF

areal = Input(option=scale, default=math%one)

tsize = (iend - istart) / stride + 1

CALL lis_vector_set_values8( &
  code, istart, stride, tsize, VALUE, obj%lis_ptr, areal, istart_value, &
  stride_value, ierr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetMultiple3

!----------------------------------------------------------------------------
!                                                                 SetMultiple
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMultiple4
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetMultiple4()"
#endif
INTEGER(I4B) :: ierr, code, tsize
LOGICAL(LGT) :: abool
REAL(DFP) :: areal

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

abool = Input(option=addContribution, default=math%no)

IF (abool) THEN; code = LIS_ADD_VALUE; ELSE; code = LIS_INS_VALUE; END IF

areal = Input(option=scale, default=math%one)
areal = areal * VALUE

tsize = (iend - istart) / stride + 1
CALL lis_vector_set_values6( &
  code, istart, stride, tsize, areal, obj%lis_ptr, ierr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetMultiple4

!----------------------------------------------------------------------------
!                                                                     SetAll
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetAll
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetAll()"
#endif
INTEGER(I4B) :: ierr, ii, n
REAL(DFP) :: areal
LOGICAL(LGT) :: abool

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

abool = Input(option=addContribution, default=math%no)
areal = Input(option=scale, default=math%one)
areal = areal * VALUE

IF (.NOT. abool) THEN
  CALL lis_vector_set_all(areal, obj%lis_ptr, ierr)

#ifdef DEBUG_VER
  CALL CHKERR(ierr)
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
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

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetAll

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set8
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set8()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: s(3), p(3), code, ierr, tsize
LOGICAL(LGT) :: abool
REAL(DFP) :: areal
REAL(DFP), POINTER :: realvec(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = obj%IsInitiated()
CALL AssertError1(isok, myName, "ScalarField_::obj not initiated")
#endif

#ifdef DEBUG_VER
isok = VALUE%IsInitiated()
CALL AssertError1(isok, myName, "ScalarField_::value not initiated")
#endif

s = GetNodeLoc(obj=obj%dof, idof=math%one_i)

SELECT TYPE (VALUE)
TYPE IS (ScalarField_)
  realvec => VALUE%GetPointer()
  CALL obj%SetMultiple( &
    istart=s(1), iend=s(2), stride=s(3), VALUE=realvec, scale=scale, &
    addContribution=addContribution)
  realvec => NULL()

TYPE IS (ScalarFieldLis_)
  abool = Input(option=addContribution, default=math%no)
  areal = Input(option=scale, default=math%one)

  IF (abool) THEN; code = LIS_ADD_VALUE; ELSE; code = LIS_INS_VALUE; END IF

  p = GetNodeLoc(obj=VALUE%dof, idof=math%one_i)
  tsize = obj%dof.tNodes.1

  CALL lis_vector_set_values9( &
    code, s(1), s(3), tsize, VALUE%lis_ptr, obj%lis_ptr, areal, p(1), p(3), &
    ierr)

#ifdef DEBUG_VER
  CALL CHKERR(ierr)
#endif

#ifdef DEBUG_VER
CLASS DEFAULT
  CALL AssertError1(math%no, myName, "No case found for type of Value")
#endif
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set8

!----------------------------------------------------------------------------
!                                                                       Set9
!----------------------------------------------------------------------------

! MODULE PROCEDURE obj_Set9
! #ifdef DEBUG_VER
! LOGICAL(LGT) :: isok
! #endif
!
! CHARACTER(*), PARAMETER :: myName = "obj_Set9()"
! INTEGER(I4B) :: s(3), p(3), code, tsize, ierr
! REAL(DFP) :: areal
! LOGICAL(LGT) :: abool
! REAL(DFP), POINTER :: realvec(:)
!
! #ifdef DEBUG_VER
! isok = obj%IsInitiated()
! CALL AssertError1(isok, myName, "ScalarField_::obj not initiated")
!
! isok = VALUE%IsInitiated()
! CALL AssertError1(isok, myName, &
!                   "AbstractNodeField_::value not initiated")
! #endif
!
! s = GetNodeLoc(obj=obj%dof, idof=1_I4B)
!
! SELECT TYPE (VALUE)
!
! TYPE IS (ScalarField_)
!
!   realvec => VALUE%GetPointer()
!
!   CALL obj%SetMultiple(istart=s(1), iend=s(2), stride=s(3), &
!                   VALUE=realvec, scale=scale, addContribution=addContribution)
!
!   realvec => NULL()
!
! TYPE IS (STScalarField_)
!
!   p = GetNodeLoc(obj=VALUE%dof, idof=GetIDOF(obj=VALUE%dof, ivar=1_I4B, &
!                                              idof=idof_value))
!   realvec => VALUE%GetPointer()
!
!   CALL obj%SetMultiple(istart=s(1), iend=s(2), stride=s(3), &
!                       istart_value=p(1), iend_value=p(2), stride_value=p(3), &
!                   VALUE=realvec, scale=scale, addContribution=addContribution)
!
!   realvec => NULL()
!
! TYPE IS (VectorField_)
!
!   p = GetNodeLoc(obj=VALUE%dof, idof=GetIDOF(obj=VALUE%dof, ivar=1_I4B, &
!                                              idof=idof_value))
!   realvec => VALUE%GetPointer()
!
!   CALL obj%SetMultiple(istart=s(1), iend=s(2), stride=s(3), &
!                       istart_value=p(1), iend_value=p(2), stride_value=p(3), &
!                   VALUE=realvec, scale=scale, addContribution=addContribution)
!
!   realvec => NULL()
!
! #ifdef _TODO_
!
! TYPE is (STVectorField_)
!
!   p = GetNodeLoc(obj=VALUE%dof, idof=GetIDOF(obj=VALUE%dof, ivar=1_I4B, &
!                                              idof=idof_value))
!   realvec => VALUE%GetPointer()
!
!   CALL obj%SetMultiple(istart=s(1), iend=s(2), stride=s(3), &
!                       istart_value=p(1), iend_value=p(2), stride_value=p(3), &
!                   VALUE=realvec, scale=scale, addContribution=addContribution)
!
!   realvec => NULL()
!
! #endif
!
! TYPE is (ScalarFieldLis_)
!
!   abool = Input(option=addContribution, default=.FALSE.)
!   areal = Input(option=scale, default=1.0_DFP)
!   IF (abool) THEN; code = LIS_ADD_VALUE; ELSE; code = LIS_INS_VALUE; END IF
!   p = GetNodeLoc(obj=VALUE%dof, idof=1_I4B)
!   tsize = obj%dof.tNodes.1
!   CALL lis_vector_set_values9(code, s(1), s(3), tsize, VALUE%lis_ptr, &
!                               obj%lis_ptr, areal, p(1), p(3), ierr)
!   CALL CHKERR(ierr)
!
! TYPE IS (STScalarFieldLis_)
!
!   abool = Input(option=addContribution, default=.FALSE.)
!   areal = Input(option=scale, default=1.0_DFP)
!   IF (abool) THEN; code = LIS_ADD_VALUE; ELSE; code = LIS_INS_VALUE; END IF
!   p = GetNodeLoc(obj=VALUE%dof, idof=GetIDOF(obj=VALUE%dof, ivar=1_I4B, &
!                                              idof=idof_value))
!   tsize = obj%dof.tNodes.1
!   CALL lis_vector_set_values9(code, s(1), s(3), tsize, VALUE%lis_ptr, &
!                               obj%lis_ptr, areal, p(1), p(3), ierr)
!   CALL CHKERR(ierr)
!
! TYPE IS (VectorFieldLis_)
!
!   abool = Input(option=addContribution, default=.FALSE.)
!   areal = Input(option=scale, default=1.0_DFP)
!   IF (abool) THEN; code = LIS_ADD_VALUE; ELSE; code = LIS_INS_VALUE; END IF
!   p = GetNodeLoc(obj=VALUE%dof, idof=GetIDOF(obj=VALUE%dof, ivar=1_I4B, &
!                                              idof=idof_value))
!   tsize = obj%dof.tNodes.1
!   CALL lis_vector_set_values9(code, s(1), s(3), tsize, VALUE%lis_ptr, &
!                               obj%lis_ptr, areal, p(1), p(3), ierr)
!   CALL CHKERR(ierr)
!
! #ifdef _TODO_
!
! TYPE IS (STVectorFieldLis_)
!
!   abool = Input(option=addContribution, default=.FALSE.)
!   areal = Input(option=scale, default=1.0_DFP)
!   IF (abool) THEN; code = LIS_ADD_VALUE; ELSE; code = LIS_INS_VALUE; END IF
!   p = GetNodeLoc(obj=VALUE%dof, idof=GetIDOF(obj=VALUE%dof, ivar=1_I4B, &
!                                              idof=idof_value))
!   tsize = obj%dof.tNodes.1
!   CALL lis_vector_set_values9(code, s(1), s(3), tsize, VALUE%lis_ptr, &
!                               obj%lis_ptr, areal, p(1), p(3), ierr)
!   CALL CHKERR(ierr)
! #endif
!
! CLASS DEFAULT
!
!   CALL e%RaiseError(modName//'::'//myName//' - '// &
!                     '[INTERNAL ERROR] :: No case found')
!   RETURN
!
! END SELECT
!
! END PROCEDURE obj_Set9

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SetMethods
