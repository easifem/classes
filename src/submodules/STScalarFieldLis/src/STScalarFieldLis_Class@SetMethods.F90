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
USE BaseType, ONLY: math => TypeMathOpt
USE InputUtility, ONLY: Input
IMPLICIT NONE

#include "lisf.h"

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "STScalarFieldLis_Class@SetMethods.F90"
#endif

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
