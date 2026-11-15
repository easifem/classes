!> author: Shion Shimizu
! date: 2026-11-12
! summary: Testing GeneralizedAlphaMethod of SDAlgorithm2_
!
! Checks the optimal parameters for rhoInf, the special cases HHT-alpha
! and Newmark-beta, the tangent matrix of Chung and Hulbert (1993), and
! the spectral radius at infinite frequency, which is measured by
! stepping a single degree of freedom with very large omega dt.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE SDAlgorithm2_Class, ONLY: SDAlgorithm2_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP, h = 0.1_DFP

CALL CheckOptimalParameters()
CALL CheckSpecialCases("U")
CALL CheckSpecialCases("A")
CALL CheckTanmat()
CALL CheckSpectralRadius(0.8_DFP, "U")
CALL CheckSpectralRadius(0.5_DFP, "U")
CALL CheckSpectralRadius(0.0_DFP, "U")
CALL CheckSpectralRadius(0.8_DFP, "A")

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

FUNCTION SameCoefficients(a, b) RESULT(ans)
  TYPE(SDAlgorithm2_), INTENT(IN) :: a, b
  LOGICAL(LGT) :: ans
  ans = Same(a%tanmat, b%tanmat) .AND. Same(a%rhs_u1, b%rhs_u1) &
        .AND. Same(a%rhs_v1, b%rhs_v1) .AND. Same(a%rhs_a1, b%rhs_a1) &
        .AND. Same([a%rhs_f1, a%rhs_f2], [b%rhs_f1, b%rhs_f2]) &
        .AND. Same(a%dis, b%dis) .AND. Same(a%vel, b%vel) &
        .AND. Same(a%acc, b%acc)
END FUNCTION SameCoefficients

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! rhoInf = 0.8: alphaM = 1/3, alphaF = 4/9, gamma = 11/18, beta = 25/81

SUBROUTINE CheckOptimalParameters()
  TYPE(SDAlgorithm2_) :: obj

  CALL obj%GeneralizedAlphaMethod()
  CALL OK(obj%name .EQ. "GENA", "name is GENA")
  CALL OK(ABS(obj%alphaM - 1.0_DFP / 3.0_DFP) .LT. tol, &
          "default rhoInf = 0.8: alphaM")
  CALL OK(ABS(obj%alphaF - 4.0_DFP / 9.0_DFP) .LT. tol, &
          "default rhoInf = 0.8: alphaF")
  CALL OK(ABS(obj%gamma - 11.0_DFP / 18.0_DFP) .LT. tol, &
          "default rhoInf = 0.8: gamma")
  CALL OK(ABS(obj%beta - 25.0_DFP / 81.0_DFP) .LT. tol, &
          "default rhoInf = 0.8: beta")
  CALL obj%Initiate(dt=h)
  CALL OK(ABS(obj%forceTime - 5.0_DFP / 9.0_DFP) .LT. tol, &
          "forceTime = 1 - alphaF")
END SUBROUTINE CheckOptimalParameters

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! HHT-alpha is alphaM = 0, alphaF = -alpha; Newmark is alphaM = alphaF = 0

SUBROUTINE CheckSpecialCases(form)
  CHARACTER(*), INTENT(IN) :: form

  REAL(DFP), PARAMETER :: alpha = -0.2_DFP, beta = 0.3_DFP, &
                          gamma = 0.6_DFP
  TYPE(SDAlgorithm2_) :: ga, other

  CALL ga%GeneralizedAlphaMethod(alphaM=0.0_DFP, alphaF=-alpha, &
                                 beta=beta, gamma=gamma, form=form)
  CALL ga%Initiate(dt=h)
  CALL other%HHTAlphaMethod(alpha=alpha, beta=beta, gamma=gamma, &
                            form=form)
  CALL other%Initiate(dt=h)
  CALL OK(SameCoefficients(ga, other), &
          form//"-form: generalized-alpha equals HHT-alpha")

  CALL ga%GeneralizedAlphaMethod(alphaM=0.0_DFP, alphaF=0.0_DFP, &
                                 beta=beta, gamma=gamma, form=form)
  CALL ga%Initiate(dt=h)
  CALL other%NewmarkBetaMethod(beta=beta, gamma=gamma, form=form)
  CALL other%Initiate(dt=h)
  CALL OK(SameCoefficients(ga, other), &
          form//"-form: generalized-alpha equals Newmark-beta")
END SUBROUTINE CheckSpecialCases

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! u-form: T = (1-aM)/(beta h^2) M + (1-aF) gamma/(beta h) C + (1-aF) K
! a-form: T = (1-aM) M + (1-aF) gamma h C + (1-aF) beta h^2 K

SUBROUTINE CheckTanmat()
  TYPE(SDAlgorithm2_) :: obj
  REAL(DFP) :: am, af, b, g

  CALL obj%GeneralizedAlphaMethod(rhoInf=0.8_DFP)
  am = obj%alphaM
  af = obj%alphaF
  b = obj%beta
  g = obj%gamma
  CALL obj%Initiate(dt=h)
  CALL OK(Same(obj%tanmat, [(1.0_DFP - am) / (b * h**2), &
                            (1.0_DFP - af) * g / (b * h), &
                            1.0_DFP - af]), "u-form tanmat")
  CALL OK(ABS(obj%rhs_u1(3) + af) .LT. tol, "u-form rhs_u1(3) = -alphaF")
  CALL OK(Same([obj%rhs_f1, obj%rhs_f2], [af, 1.0_DFP - af]), &
          "force weights (alphaF, 1 - alphaF)")

  CALL obj%SetForm("A")
  CALL obj%Initiate(dt=h)
  CALL OK(Same(obj%tanmat, [1.0_DFP - am, (1.0_DFP - af) * g * h, &
                            (1.0_DFP - af) * b * h**2]), "a-form tanmat")
  CALL OK(ABS(obj%rhs_a1(1) + am) .LT. tol, "a-form rhs_a1(1) = -alphaM")
END SUBROUTINE CheckTanmat

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Free vibration u'' + omega^2 u = 0 with omega h = 1.0E+6. The growth
! of the state per step converges to the spectral radius of the
! amplification matrix, which is rhoInf at infinite frequency.

SUBROUTINE CheckSpectralRadius(rhoInf, form)
  REAL(DFP), INTENT(IN) :: rhoInf
  CHARACTER(*), INTENT(IN) :: form

  INTEGER(I4B), PARAMETER :: nstep = 1000
  REAL(DFP), PARAMETER :: omega2 = 1.0E+12_DFP / h**2
  TYPE(SDAlgorithm2_) :: obj
  REAL(DFP) :: x(3), y(3), s0, s1, logGrowth, rho
  INTEGER(I4B) :: n
  CHARACTER(32) :: label

  CALL obj%GeneralizedAlphaMethod(rhoInf=rhoInf, form=form)
  CALL obj%Initiate(dt=h)

  ! state (u, v h, a h^2), consistent initial acceleration a = -omega2 u
  x = [1.0_DFP, 0.3_DFP * h, -omega2 * h**2]
  logGrowth = 0.0_DFP
  DO n = 1, nstep
    y = OneStep(obj, x(1), x(2) / h, x(3) / h**2, omega2)
    y = [y(1), y(2) * h, y(3) * h**2]
    s0 = NORM2(x)
    s1 = NORM2(y)
    ! skip the first steps, which are dominated by the transient
    IF (n .GT. nstep / 2) logGrowth = logGrowth + LOG(s1 / s0)
    x = y / s1
  END DO
  rho = EXP(logGrowth / (nstep - nstep / 2))

  WRITE (label, "(A, F4.2)") form//"-form rhoInf = ", rhoInf
  CALL OK(ABS(rho - rhoInf) .LT. 0.02_DFP, &
          TRIM(label)//": spectral radius at infinite frequency")
END SUBROUTINE CheckSpectralRadius

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! One step of m a + k u = 0 (m = 1) with the coefficients of algo,
! returns (u, v, a) at t(n+1)

FUNCTION OneStep(algo, u, v, a, k) RESULT(ans)
  TYPE(SDAlgorithm2_), INTENT(IN) :: algo
  REAL(DFP), INTENT(IN) :: u, v, a, k
  REAL(DFP) :: ans(3)

  REAL(DFP) :: mck(3), tt, rhs, sol, x(4)

  mck = [1.0_DFP, 0.0_DFP, k]
  tt = DOT_PRODUCT(algo%tanmat, mck)
  rhs = DOT_PRODUCT(mck, algo%rhs_u1) * u &
        + DOT_PRODUCT(mck, algo%rhs_v1) * v &
        + DOT_PRODUCT(mck, algo%rhs_a1) * a
  sol = rhs / tt
  x = [u, v, a, sol]
  ans(1) = DOT_PRODUCT(algo%dis, x)
  ans(2) = DOT_PRODUCT(algo%vel, x)
  ans(3) = DOT_PRODUCT(algo%acc, x)
END FUNCTION OneStep

END PROGRAM main
