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

SUBMODULE(BlockNodeFieldLis_Class) SetMethods
USE BaseType, ONLY: math => TypeMathOpt
USE DOF_Method, ONLY: GetIDOF
USE DOF_Method, ONLY: GetNodeLoc
USE DOF_Method, ONLY: OPERATOR(.tNodes.)
USE InputUtility, ONLY: Input
USE ScalarField_Class, ONLY: ScalarField_
USE ScalarFieldLis_Class, ONLY: ScalarFieldLis_
USE STScalarField_Class, ONLY: STScalarField_
USE STScalarFieldLis_Class, ONLY: STScalarFieldLis_
USE STVectorField_Class, ONLY: STVectorField_
USE STVectorFieldLis_Class, ONLY: STVectorFieldLis_
USE VectorField_Class, ONLY: VectorField_
USE VectorFieldLis_Class, ONLY: VectorFieldLis_
IMPLICIT NONE

#include "lisf.h"

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "BlockNodeFieldLis_Class@SetMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set18
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set18()"
#endif
REAL(DFP), POINTER :: realvec(:)
INTEGER(I4B) :: ierr, tsize, s(3), p(3), code
REAL(DFP) :: areal
LOGICAL(LGT) :: abool

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated(), myName, &
                  'BlockNodeFieldLis_::obj is not initiated')

CALL AssertError1(VALUE%isInitiated(), myName, &
                  'AbstractNodeField_::value is not initiated')
#endif

abool = Input(option=addContribution, default=.FALSE.)
areal = Input(option=scale, default=1.0_DFP)
IF (abool) THEN
  code = LIS_ADD_VALUE
ELSE
  code = LIS_INS_VALUE
END IF

s = GetNodeLoc(obj=obj%dof, idof=GetIDOF(obj=obj%dof, ivar=ivar, idof=idof))

tsize = obj%dof.tNodes. [ivar, idof]

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

TYPE IS (STVectorFieldLis_)

  p = GetNodeLoc(obj=VALUE%dof, idof=GetIDOF(obj=VALUE%dof, &
                                            ivar=ivar_value, idof=idof_value))

  CALL lis_vector_set_values9(code, s(1), s(3), tsize, VALUE%lis_ptr, &
                              obj%lis_ptr, areal, p(1), p(3), ierr)

  CALL CHKERR(ierr)

#ifdef DEBUG_VER
CLASS DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: No case found for the type of VALUE')
#endif

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set18

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

IF (abool) THEN
  code = LIS_ADD_VALUE
ELSE
  code = LIS_INS_VALUE
END IF

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

IF (abool) THEN
  code = LIS_ADD_VALUE
ELSE
  code = LIS_INS_VALUE
END IF

CALL lis_vector_set_values4(code, SIZE(indx), indx, VALUE, obj%lis_ptr, &
                            areal, ierr)

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

IF (abool) THEN
  code = LIS_ADD_VALUE
ELSE
  code = LIS_INS_VALUE
END IF

areal = Input(option=scale, default=math%one)
tsize = (iend - istart) / stride + 1

CALL lis_vector_set_values5(code, istart, stride, tsize, VALUE, obj%lis_ptr, &
                            areal, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

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

IF (abool) THEN
  code = LIS_ADD_VALUE
ELSE
  code = LIS_INS_VALUE
END IF

areal = Input(option=scale, default=math%one)

tsize = (iend - istart) / stride + 1

CALL lis_vector_set_values8(code, istart, stride, tsize, VALUE, obj%lis_ptr, &
                            areal, istart_value, stride_value, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

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

IF (abool) THEN
  code = LIS_ADD_VALUE
ELSE
  code = LIS_INS_VALUE
END IF

areal = Input(option=scale, default=math%one)
areal = areal * VALUE

tsize = (iend - istart) / stride + 1
CALL lis_vector_set_values6(code, istart, stride, tsize, areal, &
                            obj%lis_ptr, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetMultiple4

!----------------------------------------------------------------------------
!                                                                 SetMultiple
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMultiple5
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetMultiple5()"
LOGICAL(LGT) :: isok
#endif
INTEGER(I4B) :: ierr, code, tsize, tsize2, iend_value2
LOGICAL(LGT) :: isLis, isNative, isAdd
REAL(DFP) :: areal
REAL(DFP), POINTER :: realvec(:)
REAL(DFP), ALLOCATABLE :: temp(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isAdd = Input(option=addContribution, default=math%no)
areal = Input(option=scale, default=math%one)
tsize = (iend - istart) / stride + 1
iend_value2 = istart_value + (tsize - 1) * stride_value

isNative = VALUE%engine%INDEX("NATIVE") .EQ. math%one_i
isLis = VALUE%engine%INDEX("LIS") .EQ. math%one_i

! VALUE is native: its vector of values is passed, no copy
IF (isNative) THEN
  realvec => VALUE%GetPointer()

#ifdef DEBUG_VER
  isok = ASSOCIATED(realvec)
  CALL AssertError1(isok, myName, &
                    'realvec obtained from value is not ASSOCIATED')
#endif

  CALL obj%SetMultiple3( &
    istart=istart, iend=iend, stride=stride, VALUE=realvec, &
    istart_value=istart_value, iend_value=iend_value2, &
    stride_value=stride_value, scale=areal, addContribution=isAdd)

  realvec => NULL()

! VALUE is a LIS field: LIS sets (or adds) scale times the range of
! VALUE in the range of obj, no copy
ELSE IF (isLis) THEN
  IF (isAdd) THEN
    code = LIS_ADD_VALUE
  ELSE
    code = LIS_INS_VALUE
  END IF

  CALL lis_vector_set_values9(code, istart, stride, tsize, VALUE%lis_ptr, &
                              obj%lis_ptr, areal, istart_value, &
                              stride_value, ierr)

#ifdef DEBUG_VER
  CALL CHKERR(ierr)
#endif

! otherwise: copy the values of VALUE to a temporary array
ELSE
  ALLOCATE (temp(tsize))
  CALL VALUE%GetMultiple(istart=istart_value, iend=iend_value2, &
                         stride=stride_value, VALUE=temp, tsize=tsize2)
  CALL obj%SetMultiple3( &
    istart=istart, iend=iend, stride=stride, VALUE=temp, istart_value=1, &
    iend_value=tsize, stride_value=1, scale=areal, addContribution=isAdd)
  DEALLOCATE (temp)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetMultiple5

!----------------------------------------------------------------------------
!                                                                     SetAll
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetAll
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetAll()"
#endif
INTEGER(I4B) :: ierr
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

! lis_vector_shift computes x_i - sigma on the local block of each
! process, so sigma = -areal adds areal to every entry
CALL lis_vector_shift(-areal, obj%lis_ptr, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetAll

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SetMethods
