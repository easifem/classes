!> author: Shion Shimizu
! date: 2026-11-13
! summary: Testing BatheMethod and Initiate of CSDAlgorithm2_
!
! Checks the weights b0, b1, b2 of the three variants, the tangent
! matrices of both substeps, the equivalence of the u-form and the
! a-form on a single degree of freedom, and that interpolating and
! evaluating a force linear in time give the same step.

PROGRAM main
USE CSDAlgorithm2_Class, ONLY: CSDAlgorithm2_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP, dt = 0.1_DFP
REAL(DFP), PARAMETER :: one3 = 1.0_DFP / 3.0_DFP

CALL CheckWeights()
CALL CheckTanmat()
CALL CheckVariant("gamma")
CALL CheckVariant("rho")
CALL CheckVariant("beta")

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

SUBROUTINE CheckWeights()
  TYPE(CSDAlgorithm2_) :: obj

  CALL obj%BatheMethod(splittingRatio=0.5_DFP)
  CALL OK(Same([obj%b0, obj%b1, obj%b2], [one3, one3, one3]), &
          "gamma-Bathe s = 0.5: b0 = b1 = b2 = 1/3")

  CALL obj%BatheMethod(splittingRatio=0.5_DFP, rhoInf=0.0_DFP)
  CALL OK(Same([obj%b0, obj%b1, obj%b2], [one3, one3, one3]), &
          "rho-Bathe rhoInf = 0, s = 0.5 equals gamma-Bathe")

  CALL obj%BatheMethod(splittingRatio=0.5_DFP, rhoInf=0.5_DFP)
  CALL OK(Same([obj%b0, obj%b1, obj%b2], &
               [2.0_DFP / 7.0_DFP, 3.0_DFP / 7.0_DFP, 2.0_DFP / 7.0_DFP]), &
          "rho-Bathe rhoInf = 0.5, s = 0.5")

  CALL obj%BatheMethod(splittingRatio=0.5_DFP, beta1=one3)
  CALL OK(Same([obj%b0, obj%b1, obj%b2], [one3, one3, one3]), &
          "beta1/beta2-Bathe beta1 = 1/3, beta2 = 2/3, s = 0.5")
END SUBROUTINE CheckWeights

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! gamma-Bathe s = 0.5, dt = 0.1: substep 1 is the trapezoidal rule over
! h = 0.05, substep 2 has b2 dt = 1/30

SUBROUTINE CheckTanmat()
  TYPE(CSDAlgorithm2_) :: obj

  CALL obj%BatheMethod()
  CALL obj%Initiate(dt=dt)
  CALL OK(obj%GetTotalSubstep() .EQ. 2, "two substeps")
  CALL OK(Same(obj%tanmat(:, 1), [1600.0_DFP, 40.0_DFP, 1.0_DFP]), &
          "u-form tanmat of substep 1 = (4/h^2, 2/h, 1)")
  CALL OK(Same(obj%tanmat(:, 2), [900.0_DFP, 30.0_DFP, 1.0_DFP]), &
          "u-form tanmat of substep 2 = (1/(b2 dt)^2, 1/(b2 dt), 1)")
  CALL OK(Same([obj%rhs_f1(1), obj%rhs_f2(1)], [0.5_DFP, 0.5_DFP]), &
          "force weights of substep 1 (1 - s, s)")
  CALL OK(Same([obj%GetSubstepTime(1.0_DFP, 1), &
                obj%GetSubstepTime(1.0_DFP, 2)], [1.05_DFP, 1.1_DFP]), &
          "substep times")

  CALL obj%SetForm("A")
  CALL obj%Initiate(dt=dt)
  CALL OK(Same(obj%tanmat(:, 1), [1.0_DFP, 0.025_DFP, 0.000625_DFP]), &
          "a-form tanmat of substep 1 = (1, h/2, (h/2)^2)")
  CALL OK(Same(obj%tanmat(:, 2), [1.0_DFP, dt / 3.0_DFP, &
                                  (dt / 3.0_DFP)**2]), &
          "a-form tanmat of substep 2 = (1, b2 dt, (b2 dt)^2)")
END SUBROUTINE CheckTanmat

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! One step of m a + c v + k u = p + q t with s = 0.6: the u-form and the
! a-form give the same state, and interpolate and evaluate give the same
! state because the force is linear in time

SUBROUTINE CheckVariant(variant)
  CHARACTER(*), INTENT(IN) :: variant

  TYPE(CSDAlgorithm2_) :: obj
  REAL(DFP) :: su(3), sa(3), se(3)

  CALL SetVariant(obj, variant, "U")
  su = OneStep(obj)
  CALL SetVariant(obj, variant, "A")
  sa = OneStep(obj)
  CALL OK(Same(su, sa), variant//"-Bathe: u-form and a-form agree")

  CALL obj%SetForceEvaluation("evaluate")
  CALL obj%Initiate(dt=dt)
  se = OneStep(obj)
  CALL OK(Same(sa, se), &
          variant//"-Bathe: interpolate and evaluate agree")
END SUBROUTINE CheckVariant

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE SetVariant(obj, variant, form)
  TYPE(CSDAlgorithm2_), INTENT(INOUT) :: obj
  CHARACTER(*), INTENT(IN) :: variant, form

  SELECT CASE (variant)
  CASE ("rho")
    CALL obj%BatheMethod(splittingRatio=0.6_DFP, rhoInf=0.5_DFP, &
                         form=form)
  CASE ("beta")
    CALL obj%BatheMethod(splittingRatio=0.6_DFP, beta1=0.3_DFP, &
                         beta2=0.6_DFP, form=form)
  CASE DEFAULT
    CALL obj%BatheMethod(splittingRatio=0.6_DFP, form=form)
  END SELECT
  CALL obj%Initiate(dt=dt)
END SUBROUTINE SetVariant

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! All substeps of one step, returns (u, v, a) at t(n+1). The history of
! substep k is [un, vn, an, sol_1, ..., sol_(k-1)].

FUNCTION OneStep(obj) RESULT(ans)
  TYPE(CSDAlgorithm2_), INTENT(IN) :: obj
  REAL(DFP) :: ans(3)

  REAL(DFP), PARAMETER :: mck(3) = [2.0_DFP, 0.3_DFP, 50.0_DFP], &
                          x0(3) = [0.2_DFP, -1.1_DFP, 3.0_DFP], &
                          p = 0.4_DFP, q = 3.0_DFP, tn = 2.0_DFP
  REAL(DFP) :: x(5), rhs, f2
  INTEGER(I4B) :: k, j, nhist

  nhist = 3 + obj%totalSubstep
  x = 0.0_DFP
  x(1:3) = x0
  DO k = 1, obj%totalSubstep
    ! force F2 at t(n+1) (interpolate) or at the force time (evaluate)
    f2 = p + q * (tn + dt)
    IF (obj%GetForceEvaluation() .EQ. "E") THEN
      f2 = p + q * obj%GetForceTime(tn, k)
    END IF
    rhs = obj%rhs_f1(k) * (p + q * tn) + obj%rhs_f2(k) * f2
    DO j = 1, 2 + k
      rhs = rhs + DOT_PRODUCT(obj%rhs(:, j, k), mck) * x(j)
    END DO
    x(3 + k) = rhs / DOT_PRODUCT(obj%tanmat(:, k), mck)
  END DO
  ans(1) = DOT_PRODUCT(obj%dis, x(1:nhist))
  ans(2) = DOT_PRODUCT(obj%vel, x(1:nhist))
  ans(3) = DOT_PRODUCT(obj%acc, x(1:nhist))
END FUNCTION OneStep

END PROGRAM main
