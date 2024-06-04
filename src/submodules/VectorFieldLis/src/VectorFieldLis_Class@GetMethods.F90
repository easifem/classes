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

SUBMODULE(VectorFieldLis_Class) GetMethods
USE GlobalData, ONLY: DOF_FMT, NODES_FMT

USE DOF_Method, ONLY: GetNodeLoc_, &
                      OPERATOR(.tNodes.), &
                      GetNodeLoc, &
                      GetIDOF

USE ScalarField_Class, ONLY: ScalarField_

USE STScalarField_Class, ONLY: STScalarField_

USE ScalarFieldLis_Class, ONLY: ScalarFieldLis_

USE STScalarFieldLis_Class, ONLY: STScalarFieldLis_

USE Display_Method, ONLY: ToString

IMPLICIT NONE

#include "lisf.h"

CONTAINS

!----------------------------------------------------------------------------
!                                                                GetPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPointer
CHARACTER(*), PARAMETER :: myName = "obj_GetPointer()"
ans => NULL()
CALL e%RaiseError(modName//'::'//myName//' - '// &
     '[INTERNAL ERROR] :: This method is not available for STScalarFieldLis_')
END PROCEDURE obj_GetPointer

!----------------------------------------------------------------------------
!                                                                 GetSingle
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSingle
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetMultiple1()"
#endif

INTEGER(I4B) :: ierr

#include "./lis_null_error.F90"

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
CHARACTER(*), PARAMETER :: myName = "obj_GetMultiple1()"
#endif

INTEGER(I4B) :: ierr

#include "./lis_null_error.F90"

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
CHARACTER(*), PARAMETER :: myName = "obj_GetMultiple2()"
#endif

INTEGER(I4B) :: ierr

#include "./lis_null_error.F90"

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

#include "./lis_null_error.F90"

tsize = (iend - istart) / stride + 1

#ifdef DEBUG_VER
ii = (iend_value - istart_value) / stride_value + 1
CALL AssertError1(ii .GE. tsize, myName, "size of value not enought")

ii = SIZE(VALUE)
jj = istart_value + (tsize - 1) * stride_value
CALL AssertError1(ii .GE. jj, myName, "size of value not enought")
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
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get1()"
#endif

LOGICAL(LGT) :: bool1, bool2
INTEGER(I4B) :: s(3), indx(obj%spaceCompo)

bool1 = PRESENT(globalNode)
bool2 = PRESENT(spaceCompo)

#ifdef DEBUG_VER

CALL AssertError1(.NOT. (bool1 .AND. bool2), myName, &
                  "Both globalNode and spaceCompo are present")

CALL AssertError1(bool1 .OR. bool2, myName, &
                  "Both globalNode and spaceCompo are absent")

IF (bool1) CALL AssertError1(SIZE(VALUE) .GE. obj%spaceCompo, myName, &
                             "size of value is not enough")

IF (bool2) CALL AssertError1(SIZE(VALUE) .GE. (obj%dof.tNodes.1_I4B), &
                             myName, "size of value is not enough")
#endif

! globalnode present
IF (bool1) THEN
  CALL GetNodeLoc_(obj=obj%dof, idof=obj%idofs, nodenum=globalNode, &
                   ans=indx, tsize=tsize)

  CALL obj%GetMultiple(indx=indx, VALUE=VALUE, tsize=tsize)

  RETURN
END IF

!> Get all values of spaceCompo
! IF (bool2) THEN

s = GetNodeLoc(obj=obj%dof, idof=spaceCompo)
CALL obj%GetMultiple(istart=s(1), iend=s(2), stride=s(3), VALUE=VALUE, &
                     tsize=tsize)

! END IF

END PROCEDURE obj_Get1

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get2()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: s(3), jj, mynrow
INTEGER(I4B) :: indx(obj%spaceCompo)

#ifdef DEBUG_VER
IF (storageFMT .EQ. NODES_FMT) THEN

  isok = SIZE(VALUE, 1) .GE. obj%spaceCompo
  CALL AssertError1(isok, myName, "number of rows in value is not enough")

  isok = SIZE(VALUE, 2) .GE. (obj%dof.tNodes.1)
  CALL AssertError1(isok, myName, "number of cols in value is not enough")

ELSE

  isok = SIZE(VALUE, 1) .GE. (obj%dof.tNodes.1)
  CALL AssertError1(isok, myName, "number of rows in value is not enough")

  isok = SIZE(VALUE, 2) .GE. obj%spaceCompo
  CALL AssertError1(isok, myName, "number of cols in value is not enough")

END IF
#endif

IF (storageFMT .EQ. DOF_FMT) THEN
  ncol = obj%spaceCompo
  nrow = obj%dof.tNodes.1

  !$OMP PARALLEL DO PRIVATE(jj, mynrow, s)
  DO jj = 1, ncol
    s = GetNodeLoc(obj=obj%dof, idof=jj)

    CALL obj%GetMultiple(istart=s(1), iend=s(2), stride=s(3), &
                         VALUE=VALUE(:, jj), tsize=mynrow)

  END DO
  !$OMP END PARALLEL  DO

  RETURN
END IF

nrow = obj%spaceCompo
ncol = obj%dof.tNodes.1

!$OMP PARALLEL DO PRIVATE(jj, indx, mynrow)
DO jj = 1, ncol
  CALL GetNodeLoc_(obj=obj%dof, idof=obj%idofs, nodenum=jj, ans=indx, &
                   tsize=mynrow)
  CALL obj%GetMultiple(indx=indx, VALUE=VALUE(:, jj), tsize=nrow)
END DO
!$OMP END PARALLEL DO

END PROCEDURE obj_Get2

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get3
#ifdef DEBUG_VER
LOGICAL(LGT) :: isok
#endif

CHARACTER(*), PARAMETER :: myName = "obj_Get3()"
INTEGER(I4B) :: jj, mynrow
INTEGER(I4B), ALLOCATABLE :: indx(:, :)

#include "./localNodeError.F90"

IF (storageFMT .EQ. NODES_FMT) THEN
  nrow = obj%spaceCompo; ncol = SIZE(globalNode)
ELSE
  nrow = SIZE(globalNode); ncol = obj%spaceCompo
END IF

#ifdef DEBUG_VER
isok = SIZE(VALUE, 1) .GE. nrow
CALL AssertError1(isok, myName, "number of rows in value is not enough")

isok = SIZE(VALUE, 2) .GE. ncol
CALL AssertError1(isok, myName, "number of cols in value is not enough")
#endif

ALLOCATE (indx(nrow, ncol))

CALL GetNodeLoc_(obj=obj%dof, idof=obj%idofs, nodenum=globalNode, &
                 ans=indx, nrow=nrow, ncol=ncol, storageFMT=storageFMT)

!$OMP PARALLEL DO PRIVATE(jj, mynrow)

DO jj = 1, ncol
  CALL obj%GetMultiple(indx=indx(:, jj), VALUE=VALUE(:, jj), tsize=mynrow)
END DO

!$OMP END PARALLEL DO

DEALLOCATE (indx)

END PROCEDURE obj_Get3

! ----------------------------------------------------------------------------
!                                                                         Get
! ----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get4
CHARACTER(*), PARAMETER :: myName = "obj_Get4()"
INTEGER(I4B) :: indx(SIZE(globalNode))

#include "./localNodeError.F90"

CALL GetNodeLoc_(obj=obj%dof, nodenum=globalNode, idof=spaceCompo, &
                 ans=indx, tsize=tsize)

CALL obj%GetMultiple(indx=indx, VALUE=VALUE, tsize=tsize)
END PROCEDURE obj_Get4

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get9
CHARACTER(*), PARAMETER :: myName = "obj_Get9()"
INTEGER(I4B) :: tsize, s(3), p(3), ierr
REAL(DFP), POINTER :: realvec(:)

#include "./lis_null_error.F90"

s = GetNodeLoc(obj=obj%dof, idof=GetIDOF(obj=obj%dof, ivar=ivar, idof=idof))

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

TYPE IS (VectorField_)

  p = GetNodeLoc(obj=VALUE%dof, idof=GetIDOF(obj=VALUE%dof, &
                                            ivar=ivar_value, idof=idof_value))

  realvec => VALUE%GetPointer()

  CALL obj%GetMultiple(istart=s(1), iend=s(2), stride=s(3), VALUE=realvec, &
                      istart_value=p(1), iend_value=p(2), stride_value=p(3), &
                       tsize=tsize)
  realvec => NULL()

TYPE IS (ScalarFieldLis_)

  tsize = obj%dof.tNodes.idof
  CALL lis_vector_get_values_from_range3(obj%lis_ptr, s(1), s(3), tsize, &
                                         VALUE%lis_ptr, ierr)

  CALL check_lis_error

TYPE IS (STScalarFieldLis_)

  tsize = obj%dof.tNodes.idof
  p = GetNodeLoc(obj=VALUE%dof, idof=idof_value)

  CALL lis_vector_get_values_from_range4(obj%lis_ptr, s(1), s(3), tsize, &
                                         VALUE%lis_ptr, p(1), p(3), ierr)

  CALL check_lis_error

CLASS DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTENRAL ERROR] :: No case found for the type of value')
  RETURN
END SELECT

CONTAINS

SUBROUTINE check_lis_error
#ifdef DEBUG_VER
  CALL CHKERR(ierr)
#endif

END SUBROUTINE check_lis_error

END PROCEDURE obj_Get9

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetMethods
