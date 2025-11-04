SUBROUTINE _SUBROUTINE_NAME(obj, sol, rhs, precond)
  CLASS(LinSolver_), TARGET, INTENT(INOUT) :: obj
  TYPE(CSRMatrix_), INTENT(IN) :: precond
  REAL(DFP), INTENT(INOUT) :: sol(:)
  REAL(DFP), INTENT(INOUT) :: rhs(:)

  ! Internal variables
  CHARACTER(*), PARAMETER :: myName = _MY_NAME
  INTEGER(I4B) :: n
  REAL(DFP), ALLOCATABLE :: diag(:)
  CLASS(AbstractMatrixField_), POINTER :: amat
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  obj%IPAR(1) = 0
  obj%FPAR(11) = 0.0_DFP
  CALL obj%GetParam(globalNumRow=n, amat=amat)
  obj%IPAR(7) = 1

  DO

    CALL _LIS_NAME(n, rhs, sol, obj%IPAR, obj%FPAR, obj%W)

    IF (obj%IPAR(1) .GT. 0) THEN

   CALL PERFORM_TASK_PRECOND(amat, y=obj%W(obj%IPAR(9):obj%IPAR(9) + n - 1), &
                                x=obj%W(obj%IPAR(8):obj%IPAR(8) + n - 1), &
                                precond=precond, &
                                ierr=obj%IPAR(1))

    ELSE IF (obj%IPAR(1) .LT. 0) THEN

      CALL CHECKERROR(IPAR=obj%IPAR, FPAR=obj%FPAR, myName=myName)
      EXIT

    ELSE IF (obj%IPAR(1) .EQ. 0) THEN

      CALL obj%SetParam(ierr=obj%ipar(1), iter=obj%ipar(7))
      CALL DisplayConvergence(myName, obj%ipar(7), obj%FPAR)
      EXIT

    END IF

  END DO

  ! Initial residual/error norm

  CALL obj%SetParam(error0=obj%fpar(3), tol=obj%fpar(4), &
                    error=obj%fpar(6), normRes=obj%fpar(5))

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
