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

SUBROUTINE _SUBROUTINE_NAME_(obj, sol, rhs)
  CLASS(LinSolver_), TARGET, INTENT(INOUT) :: obj
  REAL(DFP), INTENT(INOUT) :: sol(:)
  REAL(DFP), INTENT(INOUT) :: rhs(:)

  ! Internal variables
  CHARACTER(*), PARAMETER :: myName = _MY_NAME_
  INTEGER(I4B) :: n
  REAL(DFP), ALLOCATABLE :: diag(:)
  CLASS(AbstractMatrixField_), POINTER :: amat
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  obj%IPAR(1) = math%zero_i
  obj%FPAR(11) = math%zero
  CALL obj%GetParam(globalNumRow=n, amat=amat)
  obj%IPAR(7) = math%one_i

  main_loop: DO

    CALL _LIS_NAME_(n, rhs, sol, obj%IPAR, obj%FPAR, obj%W)

    IF (obj%IPAR(1) .GT. math%zero_i) THEN

      CALL PERFORM_TASK(amat, &
                        y=obj%W(obj%IPAR(9):obj%IPAR(9) + n - 1), &
                        x=obj%W(obj%IPAR(8):obj%IPAR(8) + n - 1), &
                        ierr=obj%IPAR(1))

    ELSE IF (obj%IPAR(1) .LT. math%zero_i) THEN

      CALL CHECKERROR(IPAR=obj%IPAR, FPAR=obj%FPAR, myName=myName)
      EXIT main_loop

    ELSE IF (obj%IPAR(1) .EQ. math%zero_i) THEN

      CALL obj%SetParam(ierr=obj%ipar(1), iter=obj%ipar(7))

      CALL DisplayConvergence(iter=obj%ipar(7), fpar=obj%FPAR)
      EXIT main_loop

    END IF

  END DO main_loop

  ! Initial residual/error norm
  CALL obj%SetParam(error0=obj%fpar(3), tol=obj%fpar(4), &
                    error=obj%fpar(6), normRes=obj%fpar(5))

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE _SUBROUTINE_NAME_

