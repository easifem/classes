!> author: Shion Shimizu
! date: 2026-11-12
! summary: Testing Initiate, IsInitiated, SetForm and GetForm of
!   SDAlgorithm2_
!
! Checks the u-form coefficients against the former dimensionless,
! M-normalized coefficients (divided by c and multiplied by powers of
! dt), and checks that the u-form and the a-form give the same step on
! a single degree of freedom.

PROGRAM main
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE SDAlgorithm2_Class, ONLY: SDAlgorithm2_
USE Test_Method, ONLY: OK
IMPLICIT NONE

REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP, h = 0.1_DFP
REAL(DFP), PARAMETER :: alpha = -0.2_DFP, beta = 0.3_DFP, &
                        gamma = 0.6_DFP, theta = 1.4_DFP
INTEGER(I4B) :: im
CHARACTER(4), PARAMETER :: names(4) = ["NEWM", "HHTA", "COLL", "GENA"]

CALL CheckTextbookValues()
CALL CheckInitiateTwice()

! the former coefficients exist only for NEWM, HHTA and COLL
CALL CheckAgainstOld("NEWM")
CALL CheckAgainstOld("HHTA")
CALL CheckAgainstOld("COLL")

DO im = 1, SIZE(names)
  CALL CheckUAEquivalence(names(im))
  CALL CheckForceEvaluation(names(im), "U")
  CALL CheckForceEvaluation(names(im), "A")
END DO

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! IsInitiated, SetForm and the Newmark textbook values in both forms

SUBROUTINE CheckTextbookValues()
TYPE(SDAlgorithm2_) :: obj

CALL obj%NewmarkBetaMethod()
CALL OK(.NOT. obj%IsInitiated(), "not initiated after NewmarkBetaMethod")
CALL OK(obj%GetForm() .EQ. "U", "default form is U")
CALL obj%Initiate(dt=h)
CALL OK(obj%IsInitiated(), "initiated after Initiate")
CALL OK(Same(obj%tanmat, [400.0_DFP, 20.0_DFP, 1.0_DFP]), &
        "NEWM u-form tanmat = (1/(beta h^2), gamma/(beta h), 1)")

! a-form textbook values
CALL obj%SetForm("a")
CALL OK(obj%GetForm() .EQ. "A", "SetForm a gives A")
CALL OK(.NOT. obj%IsInitiated(), "not initiated after SetForm")
CALL obj%Initiate(dt=h)
CALL OK(Same(obj%tanmat, [1.0_DFP, 0.05_DFP, 0.0025_DFP]), &
        "NEWM a-form tanmat = (1, gamma h, beta h^2)")
END SUBROUTINE CheckTextbookValues

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Initiate twice depends only on the last dt

SUBROUTINE CheckInitiateTwice()
TYPE(SDAlgorithm2_) :: obj, obj2

CALL obj2%HHTAlphaMethod(alpha=alpha)
CALL obj2%Initiate(dt=0.37_DFP)
CALL obj2%Initiate(dt=h)
CALL obj%HHTAlphaMethod(alpha=alpha)
CALL obj%Initiate(dt=h)
CALL OK(Same(obj%tanmat, obj2%tanmat) .AND. &
        Same(obj%rhs_a1, obj2%rhs_a1) .AND. Same(obj%vel, obj2%vel), &
        "Initiate twice uses the last dt")
END SUBROUTINE CheckInitiateTwice

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

SUBROUTINE SetMethod(algo, aname, form)
  TYPE(SDAlgorithm2_), INTENT(INOUT) :: algo
  CHARACTER(*), INTENT(IN) :: aname, form

  SELECT CASE (aname)
  CASE ("NEWM")
    CALL algo%NewmarkBetaMethod(beta=beta, gamma=gamma, form=form)
  CASE ("HHTA")
    CALL algo%HHTAlphaMethod(alpha=alpha, beta=beta, gamma=gamma, &
                             form=form)
  CASE ("GENA")
    CALL algo%GeneralizedAlphaMethod(rhoInf=0.6_DFP, form=form)
  CASE ("COLL")
    CALL algo%CollocationMethod(beta=beta, gamma=gamma, theta=theta, &
                                form=form)
  CASE DEFAULT
    CALL OK(.FALSE., "unknown method "//aname)
  END SELECT
  CALL algo%Initiate(dt=h)
END SUBROUTINE SetMethod

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! The former coefficients (dimensionless, coefficient of M equal to 1)
! divided by c = t3 - u3 and multiplied by powers of h must be equal to
! the new u-form coefficients.

SUBROUTINE CheckAgainstOld(aname)
  CHARACTER(*), INTENT(IN) :: aname

  TYPE(SDAlgorithm2_) :: algo
  REAL(DFP) :: t(3), u1(3), v1(3), a1(3), dis(4), vel(4), acc(4), &
               c, bi, th, tau
  REAL(DFP) :: pk(3), pv(3), pa(3), pdis(4), pvel(4), pacc(4)

  bi = 1.0_DFP / beta
  ! Newmark
  t = [1.0_DFP, gamma, beta]
  tau = 1.0_DFP
  u1 = [1.0_DFP, gamma, 0.0_DFP]
  v1 = [1.0_DFP, -(beta - gamma), 0.0_DFP]
  a1 = [-(beta - 0.5_DFP), -(beta - gamma * 0.5_DFP), 0.0_DFP]
  dis = [0.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP]
  vel = [-gamma * bi, 1.0_DFP - gamma * bi, 1.0_DFP - gamma * bi * 0.5_DFP, &
         gamma * bi]
  acc = [-bi, -bi, 1.0_DFP - 0.5_DFP * bi, bi]

  SELECT CASE (aname)
  CASE ("HHTA")
    t(2:3) = t(2:3) * (1.0_DFP + alpha)
    u1(2) = u1(2) * (1.0_DFP + alpha)
    u1(3) = alpha * beta
    v1(2) = v1(2) * (1.0_DFP + alpha) + alpha * beta
    a1(2) = a1(2) * (1.0_DFP + alpha)
    tau = 1.0_DFP + alpha
  CASE ("COLL")
    th = theta
    t(2) = t(2) * th
    t(3) = t(3) * th**2
    tau = th
    u1(2) = u1(2) * th
    v1(1) = v1(1) * th
    v1(2) = v1(2) * th**2
    a1(1) = a1(1) * th**2
    a1(2) = a1(2) * th**3
    dis = [1.0_DFP - 1.0_DFP / th**3, 1.0_DFP - 1.0_DFP / th**2, &
           (1.0_DFP - 1.0_DFP / th) * 0.5_DFP, 1.0_DFP / th**3]
    vel(1) = vel(1) / th**3
    vel(2) = 1.0_DFP - gamma / (th**2 * beta)
    vel(3) = 1.0_DFP - gamma / (2.0_DFP * th * beta)
    vel(4) = vel(4) / th**3
    acc(1) = acc(1) / th**3
    acc(2) = acc(2) / th**2
    acc(3) = 1.0_DFP - 1.0_DFP / (2.0_DFP * th * beta)
    acc(4) = acc(4) / th**3
  CASE DEFAULT
    ! NEWM: the Newmark coefficients above
  END SELECT

  c = t(3) - u1(3)
  pk = [h**(-2), h**(-1), 1.0_DFP]
  pv = [h**(-1), 1.0_DFP, h]
  pa = [1.0_DFP, h, h**2]
  pdis = [1.0_DFP, h, h**2, 1.0_DFP]
  pvel = [h**(-1), 1.0_DFP, h, h**(-1)]
  pacc = [h**(-2), h**(-1), 1.0_DFP, h**(-2)]

  CALL SetMethod(algo, aname, "U")

  CALL OK(ABS(algo%tanmat(3) - algo%rhs_u1(3) - 1.0_DFP) .LT. tol, &
          aname//" u-form t3 - u3 = 1")
  CALL OK(Same(algo%tanmat, t / c * pk), aname//" tanmat = old / c")
  CALL OK(Same(algo%rhs_u1, u1 / c * pk), aname//" rhs_u1 = old / c")
  CALL OK(Same(algo%rhs_v1, v1 / c * pv), aname//" rhs_v1 = old / c")
  CALL OK(Same(algo%rhs_a1, a1 / c * pa), aname//" rhs_a1 = old / c")
  ! force at tn + tau dt, interpolated by default
  CALL OK(ABS(algo%forceTime - tau) .LT. tol, aname//" forceTime")
  CALL OK(ABS(algo%rhs_f2 - tau) .LT. tol, aname//" rhs_f2 = tau")
  CALL OK(ABS(algo%rhs_f1 - (1.0_DFP - tau)) .LT. tol, &
          aname//" rhs_f1 = 1 - tau")
  CALL OK(Same(algo%dis, dis * pdis), aname//" dis = old")
  CALL OK(Same(algo%vel, vel * pvel), aname//" vel = old")
  CALL OK(Same(algo%acc, acc * pacc), aname//" acc = old")
END SUBROUTINE CheckAgainstOld

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! One step of m a + c v + k u = F with the u-form and with the a-form
! must give the same displacement, velocity and acceleration.

SUBROUTINE CheckUAEquivalence(aname)
  CHARACTER(*), INTENT(IN) :: aname

  TYPE(SDAlgorithm2_) :: algo
  REAL(DFP) :: su(3), sa(3)

  CALL SetMethod(algo, aname, "U")
  su = OneStep(algo, 0.7_DFP, 1.5_DFP)
  CALL SetMethod(algo, aname, "A")
  sa = OneStep(algo, 0.7_DFP, 1.5_DFP)

  CALL OK(Same(su, sa), aname//" u-form and a-form give the same step")

END SUBROUTINE CheckUAEquivalence

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! For a force linear in time, F(t) = p + q t, interpolating F(tn) and
! F(t(n+1)) and evaluating F at the force time must give the same step.

SUBROUTINE CheckForceEvaluation(aname, form)
  CHARACTER(*), INTENT(IN) :: aname, form

  TYPE(SDAlgorithm2_) :: algo
  REAL(DFP), PARAMETER :: p = 0.4_DFP, q = 3.0_DFP, tn = 2.0_DFP
  REAL(DFP) :: si(3), se(3), tf

  CALL SetMethod(algo, aname, form)
  CALL OK(algo%GetForceEvaluation() .EQ. "I", &
          aname//" "//form//" default forceEvaluation is I")
  si = OneStep(algo, p + q * tn, p + q * (tn + h))

  CALL algo%SetForceEvaluation("Evaluate")
  CALL OK(.NOT. algo%IsInitiated(), &
          aname//" "//form//" not initiated after SetForceEvaluation")
  CALL algo%Initiate(dt=h)
  CALL OK(algo%GetForceEvaluation() .EQ. "E", &
          aname//" "//form//" GetForceEvaluation is E")
  CALL OK(ABS(algo%rhs_f1) .LT. tol .AND. &
          ABS(algo%rhs_f2 - 1.0_DFP) .LT. tol, &
          aname//" "//form//" evaluate weights are (0, 1)")

  tf = algo%GetForceTime(tn)
  CALL OK(ABS(tf - (tn + algo%forceTime * h)) .LT. tol, &
          aname//" "//form//" GetForceTime")
  se = OneStep(algo, 0.0_DFP, p + q * tf)

  CALL OK(Same(si, se), &
          aname//" "//form//" interpolate = evaluate for linear force")
END SUBROUTINE CheckForceEvaluation

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION OneStep(algo, fa, fb) RESULT(ans)
  TYPE(SDAlgorithm2_), INTENT(IN) :: algo
  REAL(DFP), INTENT(IN) :: fa, fb
  !! forces at tn and t(n+1)
  REAL(DFP) :: ans(3)

  REAL(DFP), PARAMETER :: m = 2.0_DFP, cd = 0.3_DFP, k = 50.0_DFP
  REAL(DFP), PARAMETER :: x0(3) = [0.2_DFP, -1.1_DFP, 3.0_DFP]
  REAL(DFP) :: mck(3), tt, rhs, sol, x(4)

  mck = [m, cd, k]
  tt = DOT_PRODUCT(algo%tanmat, mck)
  rhs = DOT_PRODUCT(mck, algo%rhs_u1) * x0(1) &
        + DOT_PRODUCT(mck, algo%rhs_v1) * x0(2) &
        + DOT_PRODUCT(mck, algo%rhs_a1) * x0(3) &
        + algo%rhs_f2 * fb + algo%rhs_f1 * fa
  sol = rhs / tt
  x = [x0, sol]
  ans(1) = DOT_PRODUCT(algo%dis, x)
  ans(2) = DOT_PRODUCT(algo%vel, x)
  ans(3) = DOT_PRODUCT(algo%acc, x)
END FUNCTION OneStep

END PROGRAM main
