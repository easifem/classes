!> author: Shion Shimizu
! date: 2026-11-13
! summary: Testing the accuracy and dissipation of BatheMethod
!
! On the free vibration u'' + omega^2 u = 0 the test checks that Bathe
! method is second order accurate in both forms, and that the spectral
! radius at infinite frequency equals rhoInf for rho-infinity-Bathe.

PROGRAM main
USE CSDAlgorithm2_Class, ONLY: CSDAlgorithm2_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE

CALL CheckOrder("U")
CALL CheckOrder("A")
CALL CheckSpectralRadius(0.5_DFP, 0.5_DFP)
CALL CheckSpectralRadius(0.5_DFP, 0.7_DFP)
CALL CheckSpectralRadius(0.0_DFP, 0.5_DFP)
CALL CheckSpectralRadius(0.8_DFP, 0.4_DFP)

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! omega = 2 pi, u(0) = 1, v(0) = 0, a(0) = -omega^2; exact u = cos(omega t)
! is zero at t = 1/4, where the phase error is seen directly (at t = 1
! the phase error enters only quadratically). Halving dt divides the
! error by about 4.

SUBROUTINE CheckOrder(form)
  CHARACTER(*), INTENT(IN) :: form

  REAL(DFP), PARAMETER :: omega2 = (8.0_DFP * ATAN(1.0_DFP))**2
  REAL(DFP) :: err(2), ratio
  INTEGER(I4B) :: ii

  DO ii = 1, 2
    err(ii) = ABS(Integrate(form, 25 * ii, omega2))
  END DO
  ratio = err(1) / err(2)
  CALL OK(ABS(ratio - 4.0_DFP) .LT. 0.3_DFP, &
          form//"-form: second order accuracy")
END SUBROUTINE CheckOrder

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION Integrate(form, nstep, omega2) RESULT(ans)
  CHARACTER(*), INTENT(IN) :: form
  INTEGER(I4B), INTENT(IN) :: nstep
  REAL(DFP), INTENT(IN) :: omega2
  REAL(DFP) :: ans

  TYPE(CSDAlgorithm2_) :: obj
  REAL(DFP) :: x(3)
  INTEGER(I4B) :: n

  CALL obj%BatheMethod(form=form)
  CALL obj%Initiate(dt=0.25_DFP / nstep)
  x = [1.0_DFP, 0.0_DFP, -omega2]
  DO n = 1, nstep
    x = OneStep(obj, x, omega2)
  END DO
  ans = x(1)
END FUNCTION Integrate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! omega dt = 1.0E+6. The growth of the state per step converges to the
! spectral radius of the amplification matrix.

SUBROUTINE CheckSpectralRadius(rhoInf, splittingRatio)
  REAL(DFP), INTENT(IN) :: rhoInf, splittingRatio

  INTEGER(I4B), PARAMETER :: nstep = 400
  REAL(DFP), PARAMETER :: dt = 0.1_DFP, omega2 = 1.0E+12_DFP / dt**2
  TYPE(CSDAlgorithm2_) :: obj
  REAL(DFP) :: x(3), y(3), logGrowth, rho, scal(3)
  INTEGER(I4B) :: n
  CHARACTER(64) :: label

  CALL obj%BatheMethod(splittingRatio=splittingRatio, rhoInf=rhoInf)
  CALL obj%Initiate(dt=dt)

  ! state is measured as (u, v dt, a dt^2)
  scal = [1.0_DFP, dt, dt**2]
  x = [1.0_DFP, 0.3_DFP, -omega2]
  logGrowth = 0.0_DFP
  DO n = 1, nstep
    y = OneStep(obj, x, omega2)
    ! skip the first steps, which are dominated by the transient
    IF (n .GT. nstep / 2) THEN
      logGrowth = logGrowth + LOG(NORM2(y * scal) / NORM2(x * scal) &
                                  + TINY(1.0_DFP))
    END IF
    x = y / NORM2(y * scal)
  END DO
  rho = EXP(logGrowth / (nstep - nstep / 2))

  WRITE (label, "(A, F4.2, A, F4.2)") "rhoInf = ", rhoInf, &
    ", splittingRatio = ", splittingRatio
  CALL OK(ABS(rho - rhoInf) .LT. 0.02_DFP, &
          TRIM(label)//": spectral radius at infinite frequency")
END SUBROUTINE CheckSpectralRadius

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! One step of u'' + omega2 u = 0, x = (u, v, a) at tn, returns the state
! at t(n+1)

FUNCTION OneStep(obj, x0, omega2) RESULT(ans)
  TYPE(CSDAlgorithm2_), INTENT(IN) :: obj
  REAL(DFP), INTENT(IN) :: x0(3), omega2
  REAL(DFP) :: ans(3)

  REAL(DFP) :: x(5), mck(3), rhs
  INTEGER(I4B) :: k, j, nhist

  mck = [1.0_DFP, 0.0_DFP, omega2]
  nhist = 3 + obj%totalSubstep
  x = 0.0_DFP
  x(1:3) = x0
  DO k = 1, obj%totalSubstep
    rhs = 0.0_DFP
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
