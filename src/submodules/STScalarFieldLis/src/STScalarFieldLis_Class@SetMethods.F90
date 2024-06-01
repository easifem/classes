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

USE DOF_Method, ONLY: GetIndex, &
                      GetIndex_, &
                      GetNodeLoc, &
                      GetNodeLoc_, &
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
!                                                                     SetAll
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMultiple
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
END PROCEDURE obj_SetMultiple

!----------------------------------------------------------------------------
!                                                                   Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set1
CHARACTER(*), PARAMETER :: myName = "obj_Set1()"
INTEGER(I4B) :: ierr, tsize

#ifdef DEBUG_VER

CALL lis_vector_is_null(obj%lis_ptr, ierr)

CALL CHKERR(ierr)

IF (.NOT. obj%IsInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
      '[INTERNAL ERROR] :: Either STScalarFieldLis_::obj is not initiated'// &
                    " or, obj%lis_ptr is not available")
  RETURN
END IF

IF (SIZE(VALUE) .NE. obj%timeCompo) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
         '[INTERNAL ERROR] :: Size of value should be equal to obj%timeCompo')
  RETURN
END IF

#endif

#include "./localNodeError.inc"

IF (obj%timeCompo .LE. TEMP_INTVEC_LEN) THEN
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
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set2
CHARACTER(*), PARAMETER :: myName = "obj_Set2()"
INTEGER(I4B) :: ii, tsize, ierr, s(3), code
LOGICAL(LGT) :: abool
REAL(DFP) :: areal

#ifdef DEBUG_VER

CALL lis_vector_is_null(obj%lis_ptr, ierr)
CALL CHKERR(ierr)

IF (.NOT. obj%IsInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
          '[INTENRAL ERROR] :: Either ScalarField object is not initiated'// &
                    " or, lis_ptr is not available")
  RETURN
END IF

IF (SIZE(VALUE) .NE. obj%timeCompo) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
            '[INTERNAL ERROR] :: size(value) should be same as obj%timeCompo')
  RETURN
END IF

#endif

abool = Input(option=addContribution, default=.FALSE.)
areal = Input(option=scale, default=1.0_DFP)

IF (abool) THEN; code = LIS_ADD_VALUE; ELSE; code = LIS_INS_VALUE; END IF

DO ii = 1, obj%timeCompo
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

END PROCEDURE obj_Set2

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set3
CHARACTER(*), PARAMETER :: myName = "obj_Set3()"
INTEGER(I4B) :: tsize, ierr, s(3), code
LOGICAL(LGT) :: abool
REAL(DFP) :: areal

#ifdef DEBUG_VER

CALL lis_vector_is_null(obj%lis_ptr, ierr)
CALL CHKERR(ierr)

IF (.NOT. obj%IsInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
          '[INTENRAL ERROR] :: Either ScalarField object is not initiated'// &
                    " or, lis_ptr is not available")
  RETURN
END IF

IF (timeCompo .GT. obj%timeCompo) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    '[INTERNAL ERROR] :: out of bound timeCompo')
  RETURN
END IF

#endif

IF (abool) THEN; code = LIS_ADD_VALUE; ELSE; code = LIS_INS_VALUE; END IF
areal = Input(option=scale, default=1.0_DFP)
tsize = obj%dof.tNodes.timeCompo
s = GetNodeLoc(obj=obj%dof, idof=timeCompo)
! void lis_vector_set_values6_f(LIS_INT *flag, LIS_INT *start,
! LIS_INT *stride, LIS_INT *count, LIS_SCALAR *values, LIS_VECTOR *v,
! LIS_INT *ierr)
areal = areal * VALUE
CALL lis_vector_set_values6(code, s(1), s(3), tsize, areal, &
                            obj%lis_ptr, ierr)

END PROCEDURE obj_Set3

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set4
CHARACTER(*), PARAMETER :: myName = "obj_Set4()"
INTEGER(I4B) :: ii, tsize, ierr, code, s(3)
LOGICAL(LGT) :: abool
REAL(DFP) :: areal

#ifdef DEBUG_VER

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
      '[INTERNAL ERROR] :: Either STScalarFieldLis_::obj is not initiated'// &
                    " or, obj%lis_ptr is not available")
END IF

IF (obj%fieldType .EQ. TypeField%constant) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
               '[INTERNAL ERROR] :: NOT callable for constant STScalar field')
  RETURN
END IF

tsize = obj%fedof%GetTotalDOF()

IF ((SIZE(VALUE, 1) .NE. tsize) &
    .OR. (SIZE(VALUE, 2) .NE. obj%timeCompo)) THEN

  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    '[INTENRAL ERROR] :: The shape of value should be [ '// &
                    ToString(obj%timeCompo)//', '//ToString(tsize)//' ]')

  RETURN

END IF

#endif

abool = Input(option=addContribution, default=.FALSE.)
areal = Input(option=scale, default=1.0_DFP)
IF (abool) THEN; code = LIS_ADD_VALUE; ELSE; code = LIS_INS_VALUE; END IF

DO ii = 1, obj%timeCompo
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

END PROCEDURE obj_Set4

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set5
CHARACTER(*), PARAMETER :: myName = "obj_Set5()"
INTEGER(I4B) :: ierr, tsize, s(3), code
LOGICAL(LGT) :: abool
REAL(DFP) :: areal

#ifdef DEBUG_VER

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
      '[INTERNAL ERROR] :: Either STScalarFieldLis_::obj is not initiated'// &
                    " or, obj%lis_ptr is not available")
END IF

IF (timeCompo .GT. obj%timeCompo) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    '[INTERNAL ERORR] :: timeCompo is out of bound')
  RETURN
END IF

IF (obj%fieldType .EQ. TypeField%constant) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
               '[INTERNAL ERROR] :: NOT callable for constant STScalar field')

  RETURN

END IF

tsize = SIZE(VALUE)

IF (tsize .NE. (obj%dof.tNodes.timeCompo)) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    '[INTERNAL ERROR] :: Size of value not correct')
  RETURN
END IF
#endif

abool = Input(option=addContribution, default=.FALSE.)
areal = Input(option=scale, default=1.0_DFP)
IF (abool) THEN; code = LIS_ADD_VALUE; ELSE; code = LIS_INS_VALUE; END IF

tsize = obj%dof.tNodes.timeCompo
s = GetNodeLoc(obj=obj%dof, idof=timeCompo)
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
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set6
REAL(DFP), POINTER :: vecPointer(:)
CHARACTER(*), PARAMETER :: myName = "obj_Set6()"
INTEGER(I4B) :: ierr
INTEGER(I4B) :: tsize1
INTEGER(I4B) :: tsize
INTEGER(I4B) :: ii
INTEGER(I4B) :: jj
INTEGER(I4B) :: indx
REAL(DFP) :: avar

#ifdef DEBUG_VER

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE &
    .OR. .NOT. VALUE%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
   '[INTERNAL ERROR] :: Either STScalarFieldLis_::object is not initiated'// &
                    ', or, ScalarField::value is not initiated'// &
                    ", or, obj%lis_ptr is not available")
END IF

IF (timeCompo .GT. obj%timeCompo) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    '[INTERNAL ERROR] :: out of bound timeCompo')
  RETURN
END IF

IF (obj%fieldType .EQ. TypeField%constant) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
               '[INTERNAL ERROR] :: NOT callable for constant STScalar field')
  RETURN
END IF

tsize = obj%dof.tNodes.timeCompo
tsize1 = VALUE%dof.tNodes.1

IF (tsize .NE. tsize1) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    '[INTERNAL ERROR] :: Size of obj and value not correct')
  RETURN
END IF

#endif

SELECT TYPE (VALUE)

TYPE IS (ScalarField_)

  vecPointer => VALUE%GetPointer()
  CALL obj%Set(VALUE=vecPointer, timeCompo=timeCompo, &
               scale=scale, addContribution=addContribution)
  vecPointer => NULL()

TYPE IS (ScalarFieldLis_)

#ifdef DEBUG_VER

  CALL lis_vector_is_null(VALUE%lis_ptr, ierr)

  IF (ierr .EQ. LIS_TRUE) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
        "[INTERNAL ERROR] :: ScalarFieldLis_::value%lis_ptr is not available")
    RETURN
  END IF

#endif

  tsize = obj%dof.tNodes.timeCompo

  DO ii = 1, tsize

    CALL VALUE%get(VALUE=avar, globalNode=jj, islocal=.TRUE.)

    indx = GetNodeLoc(obj=obj%dof, nodenum=ii, idof=timeCompo)

    CALL obj%SetSingle(VALUE=avar, indx=indx, scale=scale, &
                       addContribution=addContribution)

  END DO

CLASS DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: No case found for the type of Value')
  RETURN
END SELECT

END PROCEDURE obj_Set6

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set8
CHARACTER(*), PARAMETER :: myName = "obj_Set8()"
REAL(DFP) :: val(SIZE(VALUE))
INTEGER(I4B) :: indx(SIZE(VALUE))
INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
LOGICAL(LGT) :: problem

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
      '[INTERNAL ERROR] :: Either STScalarFieldLis_::obj is not initiated'// &
                    " or, obj%lis_ptr is not available")
  RETURN
END IF

IF (obj%fieldType .EQ. TypeField%constant) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
               '[INTERNAL ERROR] :: NOT callable for constant STScalar field')
  RETURN
END IF

IF (storageFMT .EQ. NODES_FMT) THEN
  problem = (SIZE(VALUE, 1) .NE. obj%timeCompo) .OR. &
            (SIZE(VALUE, 2) .NE. SIZE(globalNode))

  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
                    '[INTERNAL ERROR] :: SIZE(value,1) not equal timeCompo ' &
                      //'or SIZE(value, 2) not equal to the SIZE(globalNode)')
    RETURN
  END IF
END IF

IF (storageFMT .EQ. DOF_FMT) THEN
  problem = (SIZE(VALUE, 2) .NE. obj%timeCompo) .OR. &
            (SIZE(VALUE, 1) .NE. SIZE(globalNode))

  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
                    '[INTERNAL ERROR] :: SIZE(value,2) not equal timeCompo ' &
                      //'or SIZE(value, 1) not equal to the SIZE(globalNode)')
    RETURN
  END IF
END IF
#endif

#include "./localNodeError.inc"

val = RESHAPE(VALUE, [SIZE(VALUE)])

CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')

indx = GetNodeLoc( &
       obj=obj%dof, &
       nodenum=globalNode, &
       ivar=1_I4B, &
       spaceCompo=1_I4B, &
       timeCompo=obj%idofs)

CALL obj%SetMultiple(VALUE=val, &
                     indx=indx, &
                     scale=scale, &
                     addContribution=addContribution)

END PROCEDURE obj_Set8

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set9
CHARACTER(*), PARAMETER :: myName = "obj_Set9()"
INTEGER(I4B) :: ierr
INTEGER(I4B) :: tsize

#ifdef DEBUG_VER

CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    'Either STScalarFieldLis_::obj is not initiated'// &
                    " or, obj%lis_ptr is not available")
  RETURN
END IF

IF (timeCompo .GT. obj%timeCompo) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
              'given timeCompo should be less than or equal to obj%timeCompo')
  RETURN
END IF

IF (obj%fieldType .EQ. TypeField%constant) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                'This subroutine is not callable for constant STScalar field')
  RETURN
END IF

IF (SIZE(VALUE) .NE. SIZE(globalNode)) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    'Size of value should be equal to size of globalNode')
  RETURN
END IF

#endif

#include "./localNodeError.inc"

tsize = SIZE(globalNode)

IF (tsize .LE. TEMP_INTVEC_LEN) THEN
  CALL GetNodeLoc_(obj=obj%dof, idof=timeCompo, &
                   nodenum=globalNode, ans=TEMP_INTVEC, tsize=tsize)

  CALL obj%SetMultiple(indx=TEMP_INTVEC(1:tsize), VALUE=VALUE, scale=scale, &
                       addContribution=addContribution)

  RETURN

END IF

ierr = SafeSize(TEMP_DYNA_INTVEC)
IF (tsize .GT. ierr) THEN
  CALL Reallocate(TEMP_DYNA_INTVEC, EXPAND_FACTOR * tsize)
END IF

CALL GetNodeLoc_(obj=obj%dof, ivar=1, spaceCompo=1, timeCompo=timeCompo, &
                 nodenum=globalNode, ans=TEMP_DYNA_INTVEC, tsize=tsize)

CALL obj%SetMultiple(indx=TEMP_DYNA_INTVEC(1:tsize), VALUE=VALUE, &
                     scale=scale, addContribution=addContribution)

END PROCEDURE obj_Set9

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set10
CHARACTER(*), PARAMETER :: myName = "obj_Set10"
INTEGER(I4B) :: indx
INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
      '[INTERNAL ERROR] :: Either STScalarFieldLis_::obj is not initiated'// &
                    " or, obj%lis_ptr is not available")
END IF

IF (timeCompo .GT. obj%timeCompo) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
              'given timeCompo should be less than or equal to obj%timeCompo')
  RETURN
END IF

IF (obj%fieldType .EQ. TypeField%constant) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                'This subroutine is not callable for constant STScalar field')
  RETURN
END IF

#endif

#include "./localNodeError.inc"

indx = getNodeLoc(obj=obj%dof, idof=timeCompo, nodenum=globalNode)

CALL obj%SetSingle(indx=indx, VALUE=VALUE, scale=scale, &
                   addContribution=addContribution)

END PROCEDURE obj_Set10

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set12
CHARACTER(*), PARAMETER :: myName = "obj_Set12()"
INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
CALL lis_vector_is_null(obj%lis_ptr, ierr)

IF (.NOT. obj%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
  & 'Either STScalarFieldLis_::obj is not initiated'// &
  & " or, obj%lis_ptr is not available")
END IF

#endif

CALL obj%SetAll(VALUE=VALUE, scale=scale, addContribution=addContribution)

END PROCEDURE obj_Set12

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set14
CHARACTER(*), PARAMETER :: myName = "obj_Set14()"
INTEGER(I4B) :: ierr
REAL(DFP), POINTER :: realvec(:)

#ifdef DEBUG_VER

LOGICAL(LGT) :: problem

CALL lis_vector_is_null(obj%lis_ptr, ierr)
problem = .NOT. obj%isInitiated .OR. (ierr .EQ. LIS_TRUE)
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
      '[INTERNAL ERROR] :: Either STScalarFieldLis_::obj is not initiated'// &
                    " or, lis_ptr is not available")
  RETURN
END IF

problem = .NOT. VALUE%isInitiated
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
          '[INTERNAL ERROR] :: Either STScalarField_::value is not initiated')
  RETURN
END IF

#endif

SELECT TYPE (VALUE)
TYPE IS (STScalarField_)
  realvec => NULL()
  realvec => VALUE%GetPointer()

#ifdef DEBUG_VER
  problem = .NOT. ASSOCIATED(realvec)
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[INTERNAL ERROR] :: cannot get pointer from value.')
    RETURN
  END IF
#endif

  CALL obj%Set(VALUE=realvec)
  realvec => NULL()

TYPE is (STScalarFieldLis_)

#ifdef DEBUG_VER
  CALL lis_vector_is_null(VALUE%lis_ptr, ierr)
  problem = ierr .EQ. LIS_TRUE
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//" - "// &
                 '[INTERNAL ERROR] :: Either ScalarFieldLis_::obj%lis_ptr'// &
                      " is not available")
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

END PROCEDURE obj_Set14

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods
