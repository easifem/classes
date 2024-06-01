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
USE AbstractField_Class, ONLY: TypeField
USE InputUtility, ONLY: Input
USE RealVector_Method, ONLY: GetPointer
USE DOF_Method, ONLY: OPERATOR(.tNodes.), &
                      GetIDOF

#include "lisf.h"

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 SetSingle
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSingle
INTEGER(I4B) :: i, ierr
REAL(DFP) :: value0
LOGICAL(LGT) :: abool

IF (obj%fieldType .EQ. TypeField%constant) THEN; i = 1; ELSE; i = indx; END IF

value0 = Input(option=scale, default=1.0_DFP) * VALUE
abool = Input(option=addContribution, default=.FALSE.)

IF (abool) THEN
  CALL lis_vector_set_value(LIS_ADD_VALUE, i, value0, obj%lis_ptr, ierr)

ELSE
  CALL lis_vector_set_value(LIS_INS_VALUE, i, value0, obj%lis_ptr, ierr)

END IF

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

END PROCEDURE obj_SetSingle

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
!                                                               SetMultiple
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMultiple
INTEGER(I4B) :: i(SIZE(indx)), ierr, n
REAL(DFP) :: value0(SIZE(VALUE))
LOGICAL(LGT) :: abool

i = indx
n = SIZE(indx)

value0 = Input(option=scale, default=1.0_DFP) * VALUE
abool = Input(option=addContribution, default=.FALSE.)

IF (abool) THEN
  CALL lis_vector_set_values(LIS_ADD_VALUE, n, i, value0, obj%lis_ptr, ierr)

ELSE
  CALL lis_vector_set_values(LIS_INS_VALUE, n, i, value0, obj%lis_ptr, ierr)

END IF

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif
END PROCEDURE obj_SetMultiple

!----------------------------------------------------------------------------
!                                                                   Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set1
CHARACTER(*), PARAMETER :: myName = "obj_Set1()"

#ifdef DEBUG_VER
INTEGER(I4B) :: ierr

CALL lis_vector_is_null(obj%lis_ptr, ierr)

CALL CHKERR(ierr)
IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
          '[INTERNAL ERROR] :: Either ScalarField object is not initiated'// &
                    " or, lis_ptr is not available")
  RETURN
END IF
#endif

#include "./localNodeError.inc"

CALL obj%SetSingle(indx=globalNode, VALUE=VALUE, scale=scale, &
                   addContribution=addContribution)

END PROCEDURE obj_Set1

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set2
#ifdef DEBUG_VER

CHARACTER(*), PARAMETER :: myName = "obj_Set2()"
INTEGER(I4B) :: ierr

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
          '[INTERNAL ERROR] :: Either ScalarField object is not initiated'// &
                    " or, lis_ptr is not available")
  RETURN
END IF

#endif

CALL obj%SetAll(VALUE=VALUE, scale=scale, addContribution=addContribution)

END PROCEDURE obj_Set2

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set3
LOGICAL(LGT) :: abool
INTEGER(I4B) :: ii
INTEGER(I4B) :: ierr
INTEGER(I4B) :: tsize
REAL(DFP) :: scale0
REAL(DFP) :: value0

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set3()"

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
          '[INTERNAL ERROR] :: Either ScalarField object is not initiated'// &
                    " or, lis_ptr is not available")
  RETURN
END IF

IF (obj%fieldType .EQ. TypeField%constant) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    '[INTERNAL ERROR] :: Not valid for constant field type.')
  RETURN
END IF
#endif

tsize = obj%SIZE()

#ifdef DEBUG_VER
IF (tsize .NE. SIZE(VALUE)) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
      '[INTERNAL ERROR] :: Size of value is not equal to size of scalarfield')
  RETURN
END IF

#endif

abool = Input(option=addContribution, default=.FALSE.)

IF (abool) THEN
  scale0 = Input(option=scale, default=1.0_DFP)

  DO ii = 1, tsize
    value0 = VALUE(ii) * scale0
    CALL lis_vector_set_value(LIS_ADD_VALUE, ii, value0, obj%lis_ptr, ierr)
  END DO

#ifdef DEBUG_VER
  CALL CHKERR(ierr)
#endif

  RETURN
END IF

DO ii = 1, tsize
  value0 = VALUE(ii)
  CALL lis_vector_set_value(LIS_INS_VALUE, ii, value0, obj%lis_ptr, ierr)
END DO

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

END PROCEDURE obj_Set3

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set4
REAL(DFP) :: value0(SIZE(globalNode))
INTEGER(I4B) :: ierr
CHARACTER(*), PARAMETER :: myName = "obj_Set4()"

#ifdef DEBUG_VER
INTEGER(I4B) :: tsize

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
          '[INTERNAL ERROR] :: Either ScalarField object is not initiated'// &
                    " or, lis_ptr is not available")
  RETURN
END IF

IF (obj%fieldType .EQ. TypeField%constant) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
    '[INTERNAL ERROR] :: This routine should not be called for constant field type.')
  RETURN
END IF

#endif

#include "./localNodeError.inc"

value0 = VALUE

CALL obj%SetMultiple(indx=globalNode, VALUE=VALUE0, scale=scale, &
                     addContribution=addContribution)

END PROCEDURE obj_Set4

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set5
INTEGER(I4B) :: ierr
CHARACTER(*), PARAMETER :: myName = "obj_Set5()"

#ifdef DEBUG_VER
CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
          '[INTERNAL ERROR] :: Either ScalarField object is not initiated'// &
                    " or, lis_ptr is not available")
  RETURN
END IF

IF (obj%fieldType .EQ. TypeField%constant) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    '[INTERNAL ERROR] :: Not valid for constant field type.')
  RETURN
END IF

#endif

#include "./localNodeError.inc"

CALL obj%SetMultiple(indx=globalNode, VALUE=VALUE, scale=scale, &
                     addContribution=addContribution)

END PROCEDURE obj_Set5

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set6
CHARACTER(*), PARAMETER :: myName = "obj_Set6()"
INTEGER(I4B) :: ierr
REAL(DFP), POINTER :: realvec(:)

#ifdef DEBUG_VER
LOGICAL(LGT) :: problem

CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')

CALL lis_vector_is_null(obj%lis_ptr, ierr)

problem = .NOT. obj%isInitiated .OR. (ierr .EQ. LIS_TRUE)

IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
        '[INTERNAL ERROR] :: Either ScalarFieldLis_::obj is not initiated'// &
                    " or, lis_ptr is not available")
  RETURN
END IF

problem = .NOT. VALUE%isInitiated
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
            '[INTERNAL ERROR] :: Either ScalarField_::value is not initiated')
  RETURN
END IF
#endif

realvec => NULL()

SELECT TYPE (VALUE)

TYPE IS (ScalarField_)
  realvec => VALUE%GetPointer()
  CALL obj%Set(VALUE=realvec)
  realvec => NULL()

TYPE IS (ScalarFieldLis_)

#ifdef DEBUG_VER
  CALL lis_vector_is_null(VALUE%lis_ptr, ierr)

  problem = ierr .EQ. LIS_TRUE
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
                 '[INTERNAL ERROR] :: Either ScalarFieldLis_::obj%lis_ptr'// &
                      " is not available")
    RETURN
  END IF

  problem = obj%SIZE() .NE. VALUE%SIZE()
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                     '[INTERNAL ERROR] :: Size of obj and value are not same')
    RETURN
  END IF
#endif

  CALL lis_vector_copy(VALUE%lis_ptr, obj%lis_ptr, ierr)

#ifdef DEBUG_VER
  CALL CHKERR(ierr)
#endif

CLASS DEFAULT

  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: Unknown type of ScalarField_::value')
  RETURN
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Set6

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

! obj = obj + scale * obj2
MODULE PROCEDURE obj_Set8
INTEGER(I4B) :: ierr
CHARACTER(*), PARAMETER :: myName = "obj_Set8()"
REAL(DFP), POINTER :: values(:)
LOGICAL(LGT) :: abool
REAL(DFP) :: areal

#ifdef DEBUG_VER
LOGICAL(LGT) :: problem

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
        '[INTERNAL ERROR] :: Either ScalarFieldLis_::obj is not initiated'// &
                    " or, lis_ptr is not available")
  RETURN
END IF

IF (.NOT. obj2%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
             '[INTERNAL ERROR] :: Either ScalarField_::obj2 is not initiated')
  RETURN
END IF

#endif

abool = Input(option=addContribution, default=.FALSE.)

SELECT TYPE (obj2)

TYPE IS (ScalarField_)

  ! lis_vector_set_values2_f(LIS_INT *flag, LIS_INT *start, &
  ! LIS_INT *count, LIS_SCALAR *values, LIS_VECTOR_F *v, LIS_INT *ierr)

  values => GetPointer(obj2%realVec)
  IF (abool) THEN
    areal = Input(option=scale, default=1.0_DFP)
    CALL lis_vector_set_values3(LIS_ADD_VALUE, 1_I4B, obj2%tsize, values, &
                                obj%lis_ptr, ierr, areal)
  ELSE
    CALL lis_vector_set_values2(LIS_INS_VALUE, 1_I4B, obj2%tsize, values, &
                                obj%lis_ptr, ierr)
  END IF
  values => NULL()

#ifdef DEBUG_VER
  CALL CHKERR(ierr)
#endif

TYPE IS (ScalarFieldLis_)

#ifdef DEBUG_VER
  CALL lis_vector_is_null(obj2%lis_ptr, ierr)
  problem = ierr .EQ. LIS_TRUE
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
                 '[INTERNAL ERROR] :: Either ScalarFieldLis_::obj%lis_ptr'// &
                      " is not available")
    RETURN
  END IF

  IF (obj%SIZE() .NE. obj2%SIZE()) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[INTERNAL ERROR] :: Size of obj and obj2 are not same')
    RETURN
  END IF

#endif

  CALL lis_vector_axpy(scale, obj2%lis_ptr, obj%lis_ptr, ierr)

#ifdef DEBUG_VER

  CALL CHKERR(ierr)

#endif

CLASS DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: Unknown type of ScalarField_::obj2')
  RETURN
END SELECT

END PROCEDURE obj_Set8

!----------------------------------------------------------------------------
!                                                                       Set9
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set9
CHARACTER(*), PARAMETER :: myName = "obj_Set9()"
! INTEGER(I4B) :: idof1, idof2
! LOGICAL(LGT) :: abool
! REAL(DFP) :: areal

#ifdef DEBUG_VER

INTEGER(I4B) :: tsize, tsize_value, ivar_idof(2)

CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                 '[INTERNAL ERROR] :: ScalarNodeField_::obj is not initiated')
  RETURN
END IF

IF (.NOT. VALUE%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
            '[INTERNAL ERROR] :: AbstractNodeField_ ::value is not initiated')
  RETURN
END IF

ivar_idof(1:2) = [ivar, idof]
tsize = obj%dof.tNodes.ivar_idof

ivar_idof(1:2) = [ivar_value, idof_value]
tsize_value = VALUE%dof.tNodes.ivar_idof

IF (tsize .NE. tsize_value) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                   '[INTERNAL ERROR] :: size mismatch between obj and value.')
  RETURN
END IF

#endif

! abool = Input(option=addContribution, default=.FALSE.)
! idof1 = GetIDOF(obj=obj%dof, ivar=ivar, idof=idof)
! idof2 = GetIDOF(obj=VALUE%dof, ivar=ivar_value, idof=idof_value)

SELECT TYPE (VALUE)

CLASS IS (ScalarField_)
  CALL obj%Set(obj2=VALUE, scale=scale, addContribution=addContribution)

CLASS DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: Unknown type of ScalarField_::obj2')

END SELECT

#ifdef DEBUG_VER

CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Set9

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods
