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
