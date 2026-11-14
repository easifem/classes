!> author: Shion Shimizu
! date: 2026-11-13
! summary: Testing NewmarkBetaMethod of CSDAlgorithm2_
!
! Newmark-beta is the composite method with one substep. Its
! coefficients must be the same as those of SDAlgorithm2_ in both forms.

PROGRAM main
USE CSDAlgorithm2_Class, ONLY: CSDAlgorithm2_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: LGT
USE SDAlgorithm2_Class, ONLY: SDAlgorithm2_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP

CALL Compare("U")
CALL Compare("A")

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION Same(a, b) RESULT(ans)
  REAL(DFP), INTENT(IN) :: a(:), b(:)
  LOGICAL(LGT) :: ans
  ans = ALL(ABS(a - b) .LE. tol * MAX(1.0_DFP, ABS(b)))
END FUNCTION Same

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Compare(form)
  CHARACTER(*), INTENT(IN) :: form

  REAL(DFP), PARAMETER :: beta = 0.3_DFP, gamma = 0.6_DFP, dt = 0.1_DFP
  TYPE(CSDAlgorithm2_) :: csd
  TYPE(SDAlgorithm2_) :: sd

  CALL csd%NewmarkBetaMethod(beta=beta, gamma=gamma, form=form)
  CALL csd%Initiate(dt=dt)
  CALL sd%NewmarkBetaMethod(beta=beta, gamma=gamma, form=form)
  CALL sd%Initiate(dt=dt)

  CALL OK(csd%GetTotalSubstep() .EQ. 1, form//"-form: one substep")
  CALL OK(Same(csd%tanmat(:, 1), sd%tanmat), form//"-form: tanmat")
  CALL OK(Same(csd%rhs(:, 1, 1), sd%rhs_u1), form//"-form: rhs of Un")
  CALL OK(Same(csd%rhs(:, 2, 1), sd%rhs_v1), form//"-form: rhs of Vn")
  CALL OK(Same(csd%rhs(:, 3, 1), sd%rhs_a1), form//"-form: rhs of An")
  CALL OK(Same([csd%rhs_f1(1), csd%rhs_f2(1)], [sd%rhs_f1, sd%rhs_f2]), &
          form//"-form: force weights")
  CALL OK(Same(csd%dis, sd%dis), form//"-form: dis")
  CALL OK(Same(csd%vel, sd%vel), form//"-form: vel")
  CALL OK(Same(csd%acc, sd%acc), form//"-form: acc")
  CALL OK(Same(csd%initialGuess(1:3, 1), sd%initialGuess), &
          form//"-form: initialGuess")
END SUBROUTINE Compare

END PROGRAM main
