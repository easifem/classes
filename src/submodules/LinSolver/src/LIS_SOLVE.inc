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

SUBROUTINE _SUBROUTINE_NAME(obj, sol, rhs)
  CLASS(LinSolver_), TARGET, INTENT(INOUT) :: obj
  REAL(DFP), INTENT(INOUT) :: sol(:)
  REAL(DFP), INTENT(INOUT) :: rhs(:)
  !
  ! Internal variables
  !
  CHARACTER(*), PARAMETER :: myName = _MY_NAME
  INTEGER(I4B) :: n, ii
  REAL(DFP), ALLOCATABLE :: diag(:)
  CLASS(AbstractMatrixField_), POINTER :: Amat

  CALL Blanklines(nol=5)
  CALL Display("File :: "//__FILE__)
  CALL Display(modName//myName)
  CALL Display("I am not applying diagonal scaling, it should be apply by the user")
  CALL Display("I am not applying diagonal scaling, it should be apply by the user")
  CALL Display("I am not applying diagonal scaling, it should be apply by the user")
  CALL Blanklines(nol=5)

  obj%IPAR(1) = 0
  obj%FPAR(11) = 0.0_DFP
  CALL obj%GetParam(globalNumRow=n, Amat=Amat)
  obj%IPAR(7) = 1

  IF (.NOT. ASSOCIATED(Amat)) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'Amat is not ASSOCIATED')
  END IF

  DO

    CALL _LIS_NAME(n, rhs, sol, obj%IPAR, obj%FPAR, obj%W)

    IF (obj%IPAR(1) .GT. 0) THEN

      CALL PERFORM_TASK( &
        & Amat, &
        & y=obj%W(obj%IPAR(9):obj%IPAR(9) + n - 1), &
        & x=obj%W(obj%IPAR(8):obj%IPAR(8) + n - 1), &
        & ierr=obj%IPAR(1), &
        & myName=myName)

    ELSE IF (obj%IPAR(1) .LT. 0) THEN

      CALL CHECKERROR( &
        & IPAR=obj%IPAR, &
        & FPAR=obj%FPAR, &
        & myName=myName)
      EXIT

    ELSE IF (obj%IPAR(1) .EQ. 0) THEN

      CALL obj%SetParam(ierr=obj%ipar(1), &
        & iter=obj%ipar(7))
      CALL DisplayConvergence( &
        & myName, &
        & obj%ipar(7), &
        & obj%FPAR)
      EXIT

    END IF
  END DO
  !
  ! Initial residual/error norm
  !
  CALL obj%SetParam(&
    & error0=obj%fpar(3), &
    & tol=obj%fpar(4), &
    & error=obj%fpar(6), &
    & normRes=obj%fpar(5) &
    & )
END SUBROUTINE _SUBROUTINE_NAME

#ifdef _SUBROUTINE_NAME
#undef _SUBROUTINE_NAME
#endif

#ifdef _LIS_NAME
#undef _LIS_NAME
#endif

#ifdef _MY_NAME
#undef _MY_NAME
#endif
