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

SUBMODULE(STScalarFieldLis_Class) GetMethods
USE GlobalData, ONLY: DOF_FMT, NODES_FMT

USE DOF_Method, ONLY: GetNodeLoc_, &
                      OPERATOR(.tNodes.), &
                      GetNodeLoc

USE ScalarField_Class, ONLY: ScalarField_

USE ScalarFieldLis_Class, ONLY: ScalarFieldLis_

IMPLICIT NONE
#include "lisf.h"

CONTAINS

!----------------------------------------------------------------------------
!                                                                 GetSingle
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSingle
INTEGER(I4B) :: ierr

CALL lis_vector_get_value(obj%lis_ptr, indx, VALUE, ierr)

#ifdef DEBUG_VER

CALL CHKERR(ierr)

#endif
END PROCEDURE obj_GetSingle

!----------------------------------------------------------------------------
!                                                               GetMultiple
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMultiple1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetMultiple3()"
#endif

INTEGER(I4B) :: ierr

#include "./lis_null_error.inc"

tsize = SIZE(indx)

CALL lis_vector_get_values_from_index(obj%lis_ptr, tsize, indx, VALUE, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

END PROCEDURE obj_GetMultiple1

!----------------------------------------------------------------------------
!                                                               GetMultiple
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMultiple2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetMultiple3()"
#endif

INTEGER(I4B) :: ierr

#include "./lis_null_error.inc"

tsize = (iend - istart) / stride + 1

CALL lis_vector_get_values_from_range(obj%lis_ptr, istart, stride, tsize, &
                                      VALUE, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

END PROCEDURE obj_GetMultiple2

!----------------------------------------------------------------------------
!                                                               GetMultiple
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMultiple3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetMultiple3()"
LOGICAL(LGT) :: problem
#endif

INTEGER(I4B) :: ii, jj, ierr, kk

#include "./lis_null_error.inc"

tsize = (iend - istart) / stride + 1

#ifdef DEBUG_VER
ii = (iend_value - istart_value) / stride_value + 1
problem = ii .LT. tsize
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
               '[INTERNAL ERROR] :: size of obj and value is not compatible.')
  RETURN
END IF

ii = SIZE(VALUE)
jj = istart_value + (tsize - 1) * stride_value
problem = ii .LT. jj
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: size of value is not enough.')
  RETURN
END IF

#endif

CALL lis_vector_get_values_from_range2(obj%lis_ptr, istart, stride, tsize, &
                                      VALUE, istart_value, stride_value, ierr)

#ifdef DEBUG_VER
CALL CHKERR(ierr)
#endif

END PROCEDURE obj_GetMultiple3

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get1
LOGICAL(LGT) :: bool1, bool2
INTEGER(I4B) :: ierr, ii, s(3), indx(obj%timeCompo)

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get1()"
#endif

bool1 = PRESENT(globalNode)
bool2 = PRESENT(timeCompo)

#ifdef DEBUG_VER

IF (bool1 .AND. bool2) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
        '[INTERNAL ERROR] :: Both globalNode and timeCompo cannot be present')
  RETURN
END IF

IF (.NOT. bool1 .AND. .NOT. bool2) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
      '[INTERNAL ERROR] :: Either globalNode and timeCompo should be present')
  RETURN
END IF

#endif

! globalnode present
IF (bool1) THEN

#ifdef DEBUG_VER

  IF (SIZE(VALUE) .LT. obj%timeCompo) THEN

    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      '[INTERNAL ERROR] :: size of value is not enough.')
    RETURN
  END IF

#endif

  CALL GetNodeLoc_(obj=obj%dof, idof=obj%idofs, nodenum=globalNode, &
                   ans=indx, tsize=tsize)

  CALL obj%GetMultiple(indx=indx, VALUE=VALUE, tsize=tsize)

  RETURN

END IF

!> Get all values of timeCompo
! IF (bool2) THEN

tsize = obj%dof.tNodes.1_I4B

#ifdef DEBUG_VER

IF (SIZE(VALUE) .LT. tsize) THEN

  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: size of value is not enough.')
  RETURN
END IF

#endif

s = GetNodeLoc(obj=obj%dof, idof=timeCompo)

CALL obj%GetMultiple(istart=s(1), iend=s(2), stride=s(3), VALUE=VALUE, &
                     tsize=tsize)

! END IF

END PROCEDURE obj_Get1

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get2
INTEGER(I4B) :: s(3), jj, mynrow
INTEGER(I4B) :: indx(obj%timeCompo)

IF (storageFMT .EQ. DOF_FMT) THEN
  ncol = obj%timeCompo
  nrow = obj%dof.tNodes.1

  !$OMP DO PRIVATE(jj, mynrow, s)
  DO jj = 1, ncol
    s = GetNodeLoc(obj=obj%dof, idof=jj)

    CALL obj%GetMultiple(istart=s(1), iend=s(2), stride=s(3), &
                         VALUE=VALUE(:, jj), tsize=mynrow)

  END DO
  !$OMP END DO

  RETURN
END IF

nrow = obj%timeCompo
ncol = obj%dof.tNodes.1

!$OMP DO PRIVATE(jj, indx, mynrow)
DO jj = 1, ncol
  CALL GetNodeLoc_(obj=obj%dof, idof=obj%idofs, nodenum=jj, ans=indx, &
                   tsize=mynrow)
  CALL obj%GetMultiple(indx=indx, VALUE=VALUE(:, jj), tsize=nrow)
END DO
!$OMP END DO

END PROCEDURE obj_Get2

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get3
CHARACTER(*), PARAMETER :: myName = "obj_Get3()"
INTEGER(I4B) :: jj, mynrow
INTEGER(I4B), ALLOCATABLE :: indx(:, :)

#ifdef DEBUG_VER
LOGICAL(LGT) :: problem
#endif

#include "./localNodeError.inc"

#ifdef DEBUG_VER
IF (storageFMT .EQ. NODES_FMT) THEN
  nrow = obj%timeCompo
  ncol = SIZE(globalNode)
  problem = (SIZE(VALUE, 1) .LT. nrow) .OR. (SIZE(VALUE, 2) .LT. ncol)
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
               '[INTERNAL ERROR] :: (NODES_FMT) size of value is not enough.')
    RETURN
  END IF
END IF

IF (storageFMT .EQ. DOF_FMT) THEN
  ncol = obj%timeCompo
  nrow = SIZE(globalNode)
  problem = (SIZE(VALUE, 1) .LT. nrow) .OR. (SIZE(VALUE, 2) .LT. ncol)
  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                '[INTERNAL ERROR] :: (DOF_FMT)  size of value is not enough.')
    RETURN
  END IF
END IF

#endif

CALL GetNodeLoc_(obj=obj%dof, idof=obj%idofs, nodenum=globalNode, &
                 ans=indx, nrow=nrow, ncol=ncol, storageFMT=storageFMT)

!$OMP DO PRIVATE(jj, mynrow)

DO jj = 1, ncol
  CALL obj%GetMultiple(indx=indx(:, jj), VALUE=VALUE(:, jj), tsize=mynrow)
END DO

!$OMP END DO

END PROCEDURE obj_Get3

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get4
CHARACTER(*), PARAMETER :: myName = "obj_Get4()"
INTEGER(I4B) :: indx(SIZE(globalNode))

#include "./localNodeError.inc"

CALL GetNodeLoc_(obj=obj%dof, nodenum=globalNode, idof=timeCompo, &
                 ans=indx, tsize=tsize)

CALL obj%GetMultiple(indx=indx, VALUE=VALUE, tsize=tsize)
END PROCEDURE obj_Get4

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get5
CHARACTER(*), PARAMETER :: myName = "obj_Get5()"
INTEGER(I4B) :: indx

#include "./localNodeError.inc"

indx = GetNodeLoc(obj=obj%dof, nodenum=globalNode, idof=timeCompo)
CALL obj%GetSingle(indx=indx, VALUE=VALUE)

END PROCEDURE obj_Get5

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get7
CHARACTER(*), PARAMETER :: myName = "obj_Get7()"
INTEGER(I4B) :: tsize, s(3), p(3), ierr

#include "./lis_null_error.inc"

s = GetNodeLoc(obj=obj%dof, idof=timeCompo)

SELECT TYPE (VALUE)

TYPE IS (ScalarField_)

  CALL obj%Get(ivar=1_I4B, idof=timeCompo, VALUE=VALUE, ivar_value=1, &
               idof_value=1)

TYPE IS (STScalarField_)

  CALL obj%Get(ivar=1_I4B, idof=timeCompo, VALUE=VALUE, ivar_value=1, &
               idof_value=timeCompo)

TYPE IS (ScalarFieldLis_)

  CALL obj%Get(ivar=1_I4B, idof=timeCompo, VALUE=VALUE, ivar_value=1, &
               idof_value=1)

TYPE IS (STScalarFieldLIS_)

  CALL obj%Get(ivar=1_I4B, idof=timeCompo, VALUE=VALUE, ivar_value=1, &
               idof_value=timeCompo)

CLASS DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTENRAL ERROR] :: No case found for the type of value')
END SELECT

END PROCEDURE obj_Get7

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get8
CHARACTER(*), PARAMETER :: myName = "obj_Get8()"
INTEGER(I4B) :: tsize, s(3), p(3), ierr
REAL(DFP), POINTER :: realvec(:)

#include "./lis_null_error.inc"

s = GetNodeLoc(obj=obj%dof, idof=idof)

SELECT TYPE (VALUE)

TYPE IS (ScalarField_)

  realvec => VALUE%GetPointer()
  CALL obj%GetMultiple(istart=s(1), iend=s(2), stride=s(3), VALUE=realvec, &
                       tsize=tsize)
  realvec => NULL()

TYPE IS (STScalarField_)

  p = GetNodeLoc(obj=VALUE%dof, idof=idof_value)
  realvec => VALUE%GetPointer()

  CALL obj%GetMultiple(istart=s(1), iend=s(2), stride=s(3), VALUE=realvec, &
                      istart_value=p(1), iend_value=p(2), stride_value=p(3), &
                       tsize=tsize)
  realvec => NULL()

TYPE IS (ScalarFieldLis_)

  tsize = obj%dof.tNodes.idof
  CALL lis_vector_get_values_from_range3(obj%lis_ptr, s(1), s(3), tsize, &
                                         VALUE%lis_ptr, ierr)

#ifdef DEBUG_VER
  CALL CHKERR(ierr)
#endif

TYPE IS (STScalarFieldLis_)

  tsize = obj%dof.tNodes.idof
  p = GetNodeLoc(obj=VALUE%dof, idof=idof_value)

  CALL lis_vector_get_values_from_range4(obj%lis_ptr, s(1), s(3), tsize, &
                                         VALUE%lis_ptr, p(1), p(3), ierr)

#ifdef DEBUG_VER
  CALL CHKERR(ierr)
#endif

CLASS DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTENRAL ERROR] :: No case found for the type of value')
  RETURN
END SELECT

END PROCEDURE obj_Get8

!----------------------------------------------------------------------------
!                                                                GetPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPointer
CHARACTER(*), PARAMETER :: myName = "obj_GetPointer()"
ans => NULL()
CALL e%RaiseError(modName//'::'//myName//' - '// &
     '[INTERNAL ERROR] :: This method is not available for STScalarFieldLis_')
END PROCEDURE obj_GetPointer

END SUBMODULE GetMethods
