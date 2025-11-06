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

SUBMODULE(LinSolverLis_Class) SolveMethods
USE GlobalData, ONLY: stdout

USE Display_Method, ONLY: EqualLine, Display, Blanklines, &
                          ToString

IMPLICIT NONE

#include "lisf.h"

CONTAINS

!----------------------------------------------------------------------------
!                                                                  CHECKERR
!----------------------------------------------------------------------------

SUBROUTINE CHECKERROR(IPAR, FPAR, myName)
  INTEGER(I4B), INTENT(IN) :: IPAR(:)
  REAL(DFP), INTENT(IN) :: FPAR(:)
  CHARACTER(*), INTENT(IN) :: myName
  ! internal variable
  INTEGER(I4B) :: ierr, unitNo
  CHARACTER(:), ALLOCATABLE :: msg

  ierr = IPAR(1)

  SELECT CASE (ierr)

  CASE (-1)

    IF (e%IsLogActive()) THEN
      unitNo = e%GetLogFileUnit()
    ELSE
      unitNo = stdout
    END IF

    CALL EqualLine(unitNo=unitNo)

    CALL Display(IPAR(7), "Number of Matrix-Vector Multiplication: ", &
                 unitNo=unitNo)
    CALL Display(FPAR(3), "Initial residual/error norm: ", &
                 unitNo=unitNo)
    CALL Display(FPAR(4), "Target residual/error norm: ", &
                 unitNo=unitNo)
    CALL Display(FPAR(6), "Current residual/error norm: ", &
                 unitNo=unitNo)
    CALL Display(FPAR(5), "Current residual norm: ", &
                 unitNo=unitNo)
    CALL Display(FPAR(7), "Convergence rate: ", &
                 unitNo=unitNo)

    CALL EqualLine(unitNo=unitNo)

    msg = "Termination because iteration number exceeds the limit"

  CASE (-2)

    msg = "Return due to insufficient work space"

  CASE (-3)

    msg = "Return due to anticipated break-down / divide by zero"

  CASE (-4)

    msg = "The values of `fpar(1)` and `fpar(2)` are both <= 0, &
      & the valid ranges are `0 <= fpar(1) < 1`, `0 <= fpar(2)`, &
      & and they can not be zero at the same time"

  CASE (-9)

    msg = "While trying to detect a break-down, &
      & an abnormal number is detected"

  CASE (-10)

    msg = "Return due to some non-numerical reasons, &
      & e.g. invalid floating-point numbers etc"

  CASE DEFAULT

    msg = "Unknown error encountered. Cannot read the error message"

  END SELECT

  CALL AssertError1(.FALSE., myname, msg)

END SUBROUTINE CHECKERROR

!----------------------------------------------------------------------------
!                                                        DisplayConvergence
!----------------------------------------------------------------------------

SUBROUTINE DisplayConvergence(myName, iter, FPAR)
  CHARACTER(*), INTENT(IN) :: myName
  INTEGER(I4B), INTENT(IN) :: iter
  REAL(DFP), INTENT(IN) :: FPAR(:)
  ! internal variable
  INTEGER(I4B) :: unitno

  IF (e%isLogActive()) THEN
    unitno = e%getLogFileUnit()
  ELSE
    unitno = stdout
  END IF

  CALL e%RaiseInformation(modName//'::'//myName//" - "// &
                          'Convergence is achieved')

  CALL Blanklines(nol=2, unitno=unitno)
  ! CALL EqualLine(unitNo=unitNo)
  CALL Display(iter, "Number of Matrix-Vector Multiplication: ", &
               unitno=unitno)
  CALL Display(fpar(3), "Initial residual/error norm: ", &
               unitno=unitno)
  CALL Display(fpar(4), "Target residual/error norm: ", &
               unitno=unitno)
  CALL Display(fpar(6), "Current residual/error norm: ", &
               unitno=unitno)
  CALL Display(fpar(5), "Current residual norm: ", &
               unitno=unitno)
  CALL Display(fpar(7), "Convergence rate: ", &
               unitno=unitno)
  CALL EqualLine(unitNo=unitNo)
END SUBROUTINE DisplayConvergence

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Solve
CHARACTER(*), PARAMETER :: myName = "obj_Solve()"
INTEGER(I4B) :: ierr
LOGICAL(LGT) :: isok
CLASS(AbstractMatrixField_), POINTER :: amat

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER

isok = obj%isInitiated()

CALL AssertError1(isok, myName, &
                  'LinSolverLis_::obj is not initiated, initiate first!')

CALL lis_vector_is_null(sol%lis_ptr, ierr)

CALL CHKERR(ierr)

CALL AssertError1(sol%isInitiated(), myname, &
                  'AbstractNodeField_::sol not initiated')

isok = ierr .NE. LIS_TRUE

CALL AssertError1(isok, myname, &
                  'AbstractNodeField_::sol not initiated')

CALL AssertError1(rhs%isInitiated(), myname, &
                  'AbstractNodeField_::rhs not initiated')

CALL lis_vector_is_null(rhs%lis_ptr, ierr)
isok = ierr .NE. LIS_TRUE

CALL AssertError1(isok, myname, &
                  'AbstractNodeField_::rhs not initiated')

#endif

amat => obj%GetMatrixPointer()

#ifdef DEBUG_VER
isok = ASSOCIATED(amat)
CALL AssertError1(isok, myname, &
                  'LinSolverLis_::obj%amat is not ASSOCIATED')
#endif

CALL lis_solve(amat%lis_ptr, rhs%lis_ptr, sol%lis_ptr, obj%lis_solver, ierr)

#ifdef DEBUG_VER
CALL chkerr(ierr)
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Solve

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SolveMethods
