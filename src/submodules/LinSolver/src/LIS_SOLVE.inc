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

#define _x1 obj%IPAR(8)
#define _x2 obj%IPAR(8) + n - 1
#define _y1 obj%IPAR(9)
#define _y2 obj%IPAR(9) + n - 1

SUBROUTINE _SUBROUTINE_NAME(obj, sol, rhs)
  CLASS(LinSolver_), TARGET, INTENT(INOUT) :: obj
  REAL(DFP), INTENT(INOUT) :: sol(:)
  REAL(DFP), INTENT(INOUT) :: rhs(:)
  !
  ! Internal variables
  !
  CHARACTER(LEN=*), PARAMETER :: myName = _MY_NAME
  INTEGER(I4B) :: n
  !
  ! main
  !
  obj%IPAR(1) = 0
  obj%FPAR(11) = 0.0_DFP
  n = obj%globalNumRow; 
  obj%IPAR(7) = 1
  !
  !
  !
  DO
    !
    CALL _LIS_NAME(n, rhs, sol, obj%IPAR, obj%FPAR, obj%W)
    !
    ! obj%RES(obj%IPAR(7)) = obj%FPAR(6)
    CALL Display(obj%FPAR(5), __FILE__//' debug, ||r|| : ')
    !
    !
    IF (obj%IPAR(1) .GT. 0) THEN
      !
      CALL PERFORM_TASK(obj%Amat, y=obj%W(_y1:_y2), x=obj%W(_x1:_x2), &
        & dbcIndx=obj%dbcIndx, ierr=obj%IPAR(1), myName=myName)
      !
      !
      !
    ELSE IF (obj%IPAR(1) .LT. 0) THEN
      !
      CALL CHECKERROR(IPAR=obj%IPAR, FPAR=obj%FPAR, myName=myName)
      EXIT
      !
      !
      !
    ELSE IF (obj%IPAR(1) .EQ. 0) THEN
      !
      obj%ierr = obj%IPAR(1)
      obj%iter = obj%IPAR(7)
      CALL DisplayConvergence(myName, obj%iter, obj%FPAR)
      EXIT
      !
      !
      !
    END IF
    !
  END DO
  !
  ! Initial residual/error norm
  !
  obj%error0 = obj%fpar(3)
  !
  ! Target residual/error norm
  !
  obj%tol = obj%fpar(4)
  !
  ! Current residual/error norm
  !
  obj%error = obj%fpar(6)
  !
  ! Current residual norm
  !
  obj%normRes = obj%fpar(5)
  !
END SUBROUTINE _SUBROUTINE_NAME

#undef _x1
#undef _x2
#undef _y1
#undef _y2

#ifdef _SUBROUTINE_NAME
#undef _SUBROUTINE_NAME
#endif

#ifdef _LIS_NAME
#undef _LIS_NAME
#endif

#ifdef _MY_NAME
#undef _MY_NAME
#endif
