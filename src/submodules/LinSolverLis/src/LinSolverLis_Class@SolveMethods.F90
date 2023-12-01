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
USE BaseMethod
IMPLICIT NONE
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

  ierr = IPAR(1)
  SELECT CASE (ierr)
  CASE (-1)
    IF (e%isLogActive()) THEN
      unitNo = e%getLogFileUnit()
    ELSE
      unitNo = stdout
    END IF
    CALL EqualLine(unitNo=unitNo)
    CALL Display(IPAR(7), "# Number of Matrix-Vector Multiplication = ",&
      & unitNo=unitNo)
    CALL Display(FPAR(3), "# Initial residual/error norm = ",&
      & unitNo=unitNo)
    CALL Display(FPAR(4), "# Target residual/error norm = ",&
      & unitNo=unitNo)
    CALL Display(FPAR(6), "# Current residual/error norm = ",&
      & unitNo=unitNo)
    CALL Display(FPAR(5), "# Current residual norm = ",&
      & unitNo=unitNo)
    CALL Display(FPAR(7), "# Convergence rate = ",&
      & unitNo=unitNo)
    CALL EqualLine(unitNo=unitNo)
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & "Termination because iteration number exceeds the limit")
  CASE (-2)
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & "Return due to insufficient work space")
  CASE (-3)
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & "Return due to anticipated break-down / divide by zero")
  CASE (-4)
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & "The values of `fpar(1)` and `fpar(2)` are both <= 0, &
      & the valid ranges are `0 <= fpar(1) < 1`, `0 <= fpar(2)`, &
      & and they can not be zero at the same time")
  CASE (-9)
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & "While trying to detect a break-down, &
      & an abnormal number is detected")
  CASE (-10)
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & "Return due to some non-numerical reasons, &
      & e.g. invalid floating-point numbers etc")
  CASE DEFAULT
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & "Unknown error encountered. Cannot read the error message")
  END SELECT
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

  CALL e%raiseInformation(modName//'::'//myName//" - "// &
    & 'Convergence is achieved 🎖')
  CALL Blanklines(nol=2, unitno=unitno)
  ! CALL EqualLine(unitNo=unitNo)
  CALL Display(iter, "# Number of Matrix-Vector Multiplication = ",&
    & unitno=unitno)
  CALL Display(fpar(3), "# Initial residual/error norm = ",&
    & unitno=unitno)
  CALL Display(fpar(4), "# Target residual/error norm = ",&
    & unitno=unitno)
  CALL Display(fpar(6), "# Current residual/error norm = ",&
    & unitno=unitno)
  CALL Display(fpar(5), "# Current residual norm = ",&
    & unitno=unitno)
  CALL Display(fpar(7), "# Convergence rate = ",&
    & unitno=unitno)
  CALL EqualLine(unitNo=unitNo)
END SUBROUTINE DisplayConvergence

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE ls_Solve
#include "lisf.h"
CHARACTER(*), PARAMETER :: myName = "ls_Solve"
INTEGER(I4B) :: ierr

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'LinSolverLis_::obj is not initiated, initiate first!')
END IF

CALL lis_vector_is_null(sol%lis_ptr, ierr)
CALL CHKERR(ierr)
IF (.NOT. sol%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'AbstractNodeField_::sol not initiated'// &
  & " or, lis_ptr is not available")
END IF

CALL lis_vector_is_null(rhs%lis_ptr, ierr)
CALL CHKERR(ierr)
IF (.NOT. rhs%isInitiated .OR. ierr .EQ. LIS_TRUE) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'AbstractNodeField_::rhs not initiated'// &
  & " or, lis_ptr is not available")
END IF

IF (.NOT. ASSOCIATED(obj%Amat)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
    & 'LinSolverLis_::obj%Amat is not ASSOCIATED')
END IF

CALL lis_solve(obj%Amat%lis_ptr, rhs%lis_ptr, sol%lis_ptr, obj%lis_solver, ierr)

CALL chkerr(ierr)

END PROCEDURE ls_Solve

END SUBMODULE SolveMethods
