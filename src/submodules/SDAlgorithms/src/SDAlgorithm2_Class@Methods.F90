! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
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

SUBMODULE(SDAlgorithm2_Class) Methods
USE ApproxUtility, ONLY: OPERATOR(.approxeq.)
USE Display_Method, ONLY: Display
USE InputUtility, ONLY: Input
USE StringUtility, ONLY: UpperCase

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                               NewmarkBeta
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_NewmarkBetaMethod
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_NewmarkBetaMethod()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()

obj%name = "NEWM"

obj%beta = Input(default=0.25_DFP, option=beta)

obj%gamma = Input(default=math%half, option=gamma)

IF (PRESENT(FORM)) CALL obj%SetForm(FORM)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_NewmarkBetaMethod

!----------------------------------------------------------------------------
!                                                             HHTAlphaMethod
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_HHTAlphaMethod
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_HHTAlphaMethod()"
#endif

REAL(DFP) :: alpha0, areal

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()

! HHT-alpha is the generalized-alpha method with alphaM = 0 and
! alphaF = -alpha

obj%name = "HHTA"
alpha0 = Input(default=-0.30_DFP, option=alpha)

obj%alphaM = math%zero

obj%alphaF = -alpha0

areal = (math%one - alpha0)**2 * 0.25_DFP
obj%beta = Input(default=areal, option=beta)

areal = (math%one - math%two * alpha0) * math%half
obj%gamma = Input(default=areal, option=gamma)

IF (PRESENT(FORM)) CALL obj%SetForm(FORM)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_HHTAlphaMethod

!----------------------------------------------------------------------------
!                                                     GeneralizedAlphaMethod
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GeneralizedAlphaMethod
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GeneralizedAlphaMethod()"
LOGICAL(LGT) :: isok
#endif

REAL(DFP) :: rho, areal

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()

obj%name = "GENA"

! optimal parameters for the spectral radius rhoInf at infinite frequency
rho = Input(default=0.8_DFP, option=rhoInf)

#ifdef DEBUG_VER
isok = (rho .GE. math%zero) .AND. (rho .LE. math%one)
CALL AssertError1(isok, myName, "rhoInf must be in [0, 1]")
#endif

areal = (math%two * rho - math%one) / (rho + math%one)
obj%alphaM = Input(default=areal, option=alphaM)

areal = rho / (rho + math%one)
obj%alphaF = Input(default=areal, option=alphaF)

#ifdef DEBUG_VER
isok = (obj%alphaM .LT. math%one) .AND. (obj%alphaF .LT. math%one)
CALL AssertError1(isok, myName, "alphaM and alphaF must be less than 1")
#endif

! second order accuracy and maximal high frequency dissipation
areal = math%one - obj%alphaM + obj%alphaF
obj%gamma = Input(default=areal - math%half, option=gamma)
obj%beta = Input(default=0.25_DFP * areal**2, option=beta)

IF (PRESENT(FORM)) CALL obj%SetForm(FORM)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GeneralizedAlphaMethod

!----------------------------------------------------------------------------
!                                                                Collocation
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_CollocationMethod
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_CollocationMethod()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()

obj%name = "COLL"

obj%beta = Input(default=math%one / 6.0_DFP, option=beta)

obj%gamma = Input(default=math%half, option=gamma)

obj%theta = Input(default=1.4_DFP, option=theta)

IF (PRESENT(FORM)) CALL obj%SetForm(FORM)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_CollocationMethod

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate()"
LOGICAL(LGT) :: isok
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = dt .GT. math%zero
CALL AssertError1(isok, myName, "dt must be positive")

isok = .NOT. (obj%theta .approxeq. math%zero)
CALL AssertError1(isok, myName, "theta must not be zero")

isok = (obj%FORM .EQ. "A") .OR. (.NOT. (obj%beta .approxeq. math%zero))
CALL AssertError1(isok, myName, "beta = 0 is not allowed in the u-form")

SELECT CASE (obj%name)
CASE ("NEWM", "HHTA", "GENA", "COLL")
  isok = math%yes
CASE DEFAULT
  isok = math%no
END SELECT
CALL AssertError1(isok, myName, "unknown method name: "//obj%name)
#endif

! coefficients of the method, see the routines of each method below
IF (obj%FORM .EQ. "A") THEN
  CALL SetAFormCoefficients_master(obj, dt)
ELSE
  CALL SetUFormCoefficients_master(obj, dt)
END IF

! force time ratio: 1 (Newmark), 1-alphaF (HHT, generalized-alpha),
! theta (collocation)
obj%forceTime = obj%theta * (math%one - obj%alphaF)

! the coefficient of the force is 1 in both forms
IF (obj%forceEvaluation .EQ. "E") THEN
  ! evaluate at the forceTime
  obj%rhs_f1 = math%zero
  obj%rhs_f2 = math%one
ELSE
  ! interpolate the force
  obj%rhs_f1 = math%one - obj%forceTime
  obj%rhs_f2 = obj%forceTime
END IF

obj%dt = dt
obj%isInit = math%yes

CALL obj%MakeZeros()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                SetUFormCoefficients_master
!----------------------------------------------------------------------------

! Coefficients of the u-form (the unknown sol is the displacement) of the
! method obj%name. The coefficient of the force is 1 in every method, so
! RHS has the units of force.

SUBROUTINE SetUFormCoefficients_master(obj, dt)
  CLASS(SDAlgorithm2_), INTENT(INOUT) :: obj
  !! algorithm object
  REAL(DFP), INTENT(IN) :: dt
  !! time step size

  SELECT CASE (obj%name)
  CASE ("NEWM")
    CALL NewmarkUFormCoefficients(obj, dt)
  CASE ("HHTA", "GENA")
    CALL GeneralizedAlphaUFormCoefficients(obj, dt)
  CASE ("COLL")
    CALL CollocationUFormCoefficients(obj, dt)
  CASE DEFAULT
    ! unknown names are rejected in Initiate
  END SELECT
END SUBROUTINE SetUFormCoefficients_master

!----------------------------------------------------------------------------
!                                                SetAFormCoefficients_master
!----------------------------------------------------------------------------

! Coefficients of the a-form (the unknown sol is the acceleration) of the
! method obj%name. The coefficient of the force is 1 in every method, so
! RHS has the units of force.

SUBROUTINE SetAFormCoefficients_master(obj, dt)
  CLASS(SDAlgorithm2_), INTENT(INOUT) :: obj
  !! algorithm object
  REAL(DFP), INTENT(IN) :: dt
  !! time step size

  SELECT CASE (obj%name)
  CASE ("NEWM")
    CALL NewmarkAFormCoefficients(obj, dt)
  CASE ("HHTA", "GENA")
    CALL GeneralizedAlphaAFormCoefficients(obj, dt)
  CASE ("COLL")
    CALL CollocationAFormCoefficients(obj, dt)
  CASE DEFAULT
    ! unknown names are rejected in Initiate
  END SELECT
END SUBROUTINE SetAFormCoefficients_master

!----------------------------------------------------------------------------
!                                                   NewmarkUFormCoefficients
!----------------------------------------------------------------------------

! Newmark-beta, u-form. The balance M A + C V + K U = F at t(n+1) with
!   U(n+1) = Un + dt Vn + dt^2 ((1/2 - beta) An + beta A(n+1))
!   V(n+1) = Vn + dt ((1 - gamma) An + gamma A(n+1))
! gives, with sol = U(n+1),
!   [M/(beta dt^2) + gamma C/(beta dt) + K] sol = F(n+1)
!     + M (Un/(beta dt^2) + Vn/(beta dt) + (1/(2 beta) - 1) An)
!     + C (gamma Un/(beta dt) + (gamma/beta - 1) Vn
!          + dt (gamma/(2 beta) - 1) An)

SUBROUTINE NewmarkUFormCoefficients(obj, dt)
  CLASS(SDAlgorithm2_), INTENT(INOUT) :: obj
  !! algorithm object
  REAL(DFP), INTENT(IN) :: dt
  !! time step size

  REAL(DFP) :: beta, gamma

  beta = obj%beta
  gamma = obj%gamma

  obj%tanmat(1) = math%one / (beta * dt**2)
  obj%tanmat(2) = gamma / (beta * dt)
  obj%tanmat(3) = math%one

  obj%rhs_u1(1) = math%one / (beta * dt**2)
  obj%rhs_u1(2) = gamma / (beta * dt)
  obj%rhs_u1(3) = math%zero

  obj%rhs_v1(1) = math%one / (beta * dt)
  obj%rhs_v1(2) = gamma / beta - math%one
  obj%rhs_v1(3) = math%zero

  obj%rhs_a1(1) = math%half / beta - math%one
  obj%rhs_a1(2) = dt * (math%half * gamma / beta - math%one)
  obj%rhs_a1(3) = math%zero

  obj%dis(1) = math%zero
  obj%dis(2) = math%zero
  obj%dis(3) = math%zero
  obj%dis(4) = math%one

  obj%vel(1) = -gamma / (beta * dt)
  obj%vel(2) = math%one - gamma / beta
  obj%vel(3) = dt * (math%one - math%half * gamma / beta)
  obj%vel(4) = gamma / (beta * dt)

  obj%acc(1) = -math%one / (beta * dt**2)
  obj%acc(2) = -math%one / (beta * dt)
  obj%acc(3) = math%one - math%half / beta
  obj%acc(4) = math%one / (beta * dt**2)

  obj%initialGuess = math%zero
END SUBROUTINE NewmarkUFormCoefficients

!----------------------------------------------------------------------------
!                                                   NewmarkAFormCoefficients
!----------------------------------------------------------------------------

! Newmark-beta, a-form. With sol = A(n+1) and the predictors
!   U~ = Un + dt Vn + dt^2 (1/2 - beta) An,  V~ = Vn + dt (1 - gamma) An
! the balance M A + C V + K U = F at t(n+1) gives
!   [M + gamma dt C + beta dt^2 K] sol = F(n+1) - C V~ - K U~
! and U(n+1) = U~ + beta dt^2 sol, V(n+1) = V~ + gamma dt sol.

SUBROUTINE NewmarkAFormCoefficients(obj, dt)
  CLASS(SDAlgorithm2_), INTENT(INOUT) :: obj
  !! algorithm object
  REAL(DFP), INTENT(IN) :: dt
  !! time step size

  REAL(DFP) :: beta, gamma

  beta = obj%beta
  gamma = obj%gamma

  obj%tanmat(1) = math%one
  obj%tanmat(2) = gamma * dt
  obj%tanmat(3) = beta * dt**2

  obj%rhs_u1(1) = math%zero
  obj%rhs_u1(2) = math%zero
  obj%rhs_u1(3) = math%minus_one

  obj%rhs_v1(1) = math%zero
  obj%rhs_v1(2) = math%minus_one
  obj%rhs_v1(3) = -dt

  obj%rhs_a1(1) = math%zero
  obj%rhs_a1(2) = -dt * (math%one - gamma)
  obj%rhs_a1(3) = -dt**2 * (math%half - beta)

  obj%dis(1) = math%one
  obj%dis(2) = dt
  obj%dis(3) = dt**2 * (math%half - beta)
  obj%dis(4) = beta * dt**2

  obj%vel(1) = math%zero
  obj%vel(2) = math%one
  obj%vel(3) = dt * (math%one - gamma)
  obj%vel(4) = gamma * dt

  obj%acc(1) = math%zero
  obj%acc(2) = math%zero
  obj%acc(3) = math%zero
  obj%acc(4) = math%one

  obj%initialGuess(1) = math%zero
  obj%initialGuess(2) = math%zero
  obj%initialGuess(3) = math%one
END SUBROUTINE NewmarkAFormCoefficients

!----------------------------------------------------------------------------
!                                          GeneralizedAlphaUFormCoefficients
!----------------------------------------------------------------------------

! Generalized-alpha (and HHT-alpha, alphaM = 0, alphaF = -alpha), u-form.
! The balance
!   M A(n+1-alphaM) + C V(n+1-alphaF) + K U(n+1-alphaF) = F(n+1-alphaF)
! with X(n+1-a) = (1-a) X(n+1) + a Xn and the Newmark relations for
! U(n+1), V(n+1) gives, with sol = U(n+1),
!   [(1-alphaM) M/(beta dt^2) + (1-alphaF) gamma C/(beta dt)
!    + (1-alphaF) K] sol = F(n+1-alphaF)
!     + M ((1-alphaM) (Un/(beta dt^2) + Vn/(beta dt))
!          + ((1-alphaM)/(2 beta) - 1) An)
!     + C ((1-alphaF) gamma Un/(beta dt)
!          + ((1-alphaF) (gamma/beta - 1) - alphaF) Vn
!          + (1-alphaF) dt (gamma/(2 beta) - 1) An)
!     - alphaF K Un
! U(n+1), V(n+1), A(n+1) follow from the Newmark relations.

SUBROUTINE GeneralizedAlphaUFormCoefficients(obj, dt)
  CLASS(SDAlgorithm2_), INTENT(INOUT) :: obj
  !! algorithm object
  REAL(DFP), INTENT(IN) :: dt
  !! time step size

  REAL(DFP) :: beta, gamma, alphaF, oneMinusAlphaM, oneMinusAlphaF

  beta = obj%beta
  gamma = obj%gamma
  alphaF = obj%alphaF
  oneMinusAlphaM = math%one - obj%alphaM
  oneMinusAlphaF = math%one - alphaF

  obj%tanmat(1) = oneMinusAlphaM / (beta * dt**2)
  obj%tanmat(2) = oneMinusAlphaF * gamma / (beta * dt)
  obj%tanmat(3) = oneMinusAlphaF

  obj%rhs_u1(1) = oneMinusAlphaM / (beta * dt**2)
  obj%rhs_u1(2) = oneMinusAlphaF * gamma / (beta * dt)
  obj%rhs_u1(3) = -alphaF

  obj%rhs_v1(1) = oneMinusAlphaM / (beta * dt)
  obj%rhs_v1(2) = oneMinusAlphaF * (gamma / beta - math%one) - alphaF
  obj%rhs_v1(3) = math%zero

  obj%rhs_a1(1) = math%half * oneMinusAlphaM / beta - math%one
  obj%rhs_a1(2) = oneMinusAlphaF * dt * (math%half * gamma / beta - math%one)
  obj%rhs_a1(3) = math%zero

  obj%dis(1) = math%zero
  obj%dis(2) = math%zero
  obj%dis(3) = math%zero
  obj%dis(4) = math%one

  obj%vel(1) = -gamma / (beta * dt)
  obj%vel(2) = math%one - gamma / beta
  obj%vel(3) = dt * (math%one - math%half * gamma / beta)
  obj%vel(4) = gamma / (beta * dt)

  obj%acc(1) = -math%one / (beta * dt**2)
  obj%acc(2) = -math%one / (beta * dt)
  obj%acc(3) = math%one - math%half / beta
  obj%acc(4) = math%one / (beta * dt**2)

  obj%initialGuess = math%zero
END SUBROUTINE GeneralizedAlphaUFormCoefficients

!----------------------------------------------------------------------------
!                                          GeneralizedAlphaAFormCoefficients
!----------------------------------------------------------------------------

! Generalized-alpha (and HHT-alpha), a-form. With sol = A(n+1) and the
! predictors U~, V~ of Newmark (see NewmarkAFormCoefficients), the
! balance of GeneralizedAlphaUFormCoefficients gives
!   [(1-alphaM) M + (1-alphaF) gamma dt C + (1-alphaF) beta dt^2 K] sol
!     = F(n+1-alphaF) - alphaM M An - C ((1-alphaF) V~ + alphaF Vn)
!       - K ((1-alphaF) U~ + alphaF Un)
! U(n+1), V(n+1) follow from the Newmark relations.

SUBROUTINE GeneralizedAlphaAFormCoefficients(obj, dt)
  CLASS(SDAlgorithm2_), INTENT(INOUT) :: obj
  !! algorithm object
  REAL(DFP), INTENT(IN) :: dt
  !! time step size

  REAL(DFP) :: beta, gamma, alphaM, oneMinusAlphaF

  beta = obj%beta
  gamma = obj%gamma
  alphaM = obj%alphaM
  oneMinusAlphaF = math%one - obj%alphaF

  obj%tanmat(1) = math%one - alphaM
  obj%tanmat(2) = oneMinusAlphaF * gamma * dt
  obj%tanmat(3) = oneMinusAlphaF * beta * dt**2

  obj%rhs_u1(1) = math%zero
  obj%rhs_u1(2) = math%zero
  obj%rhs_u1(3) = math%minus_one

  obj%rhs_v1(1) = math%zero
  obj%rhs_v1(2) = math%minus_one
  obj%rhs_v1(3) = -oneMinusAlphaF * dt

  obj%rhs_a1(1) = -alphaM
  obj%rhs_a1(2) = -oneMinusAlphaF * dt * (math%one - gamma)
  obj%rhs_a1(3) = -oneMinusAlphaF * dt**2 * (math%half - beta)

  obj%dis(1) = math%one
  obj%dis(2) = dt
  obj%dis(3) = dt**2 * (math%half - beta)
  obj%dis(4) = beta * dt**2

  obj%vel(1) = math%zero
  obj%vel(2) = math%one
  obj%vel(3) = dt * (math%one - gamma)
  obj%vel(4) = gamma * dt

  obj%acc(1) = math%zero
  obj%acc(2) = math%zero
  obj%acc(3) = math%zero
  obj%acc(4) = math%one

  obj%initialGuess(1) = math%zero
  obj%initialGuess(2) = math%zero
  obj%initialGuess(3) = math%one
END SUBROUTINE GeneralizedAlphaAFormCoefficients

!----------------------------------------------------------------------------
!                                               CollocationUFormCoefficients
!----------------------------------------------------------------------------

! Collocation (Wilson-theta when beta = 1/6, gamma = 1/2), u-form. The
! balance is written at tn + theta dt with the Newmark relations over the
! step thetaDt = theta dt, so it is the Newmark u-form with dt replaced by
! thetaDt, and sol = U(n+theta). The acceleration at t(n+1) is
! A(n+1) = (1 - 1/theta) An + A(n+theta)/theta, and U(n+1), V(n+1) follow
! from the Newmark relations over dt.

SUBROUTINE CollocationUFormCoefficients(obj, dt)
  CLASS(SDAlgorithm2_), INTENT(INOUT) :: obj
  !! algorithm object
  REAL(DFP), INTENT(IN) :: dt
  !! time step size

  REAL(DFP) :: beta, gamma, theta, thetaDt

  beta = obj%beta
  gamma = obj%gamma
  theta = obj%theta
  thetaDt = theta * dt

  obj%tanmat(1) = math%one / (beta * thetaDt**2)
  obj%tanmat(2) = gamma / (beta * thetaDt)
  obj%tanmat(3) = math%one

  obj%rhs_u1(1) = math%one / (beta * thetaDt**2)
  obj%rhs_u1(2) = gamma / (beta * thetaDt)
  obj%rhs_u1(3) = math%zero

  obj%rhs_v1(1) = math%one / (beta * thetaDt)
  obj%rhs_v1(2) = gamma / beta - math%one
  obj%rhs_v1(3) = math%zero

  obj%rhs_a1(1) = math%half / beta - math%one
  obj%rhs_a1(2) = thetaDt * (math%half * gamma / beta - math%one)
  obj%rhs_a1(3) = math%zero

  obj%dis(1) = math%one - math%one / theta**3
  obj%dis(2) = dt * (math%one - math%one / theta**2)
  obj%dis(3) = dt**2 * (math%one - math%one / theta) * math%half
  obj%dis(4) = math%one / theta**3

  obj%vel(1) = -gamma / (beta * theta**3 * dt)
  obj%vel(2) = math%one - gamma / (beta * theta**2)
  obj%vel(3) = dt * (math%one - math%half * gamma / (beta * theta))
  obj%vel(4) = gamma / (beta * theta**3 * dt)

  obj%acc(1) = -math%one / (beta * theta**3 * dt**2)
  obj%acc(2) = -math%one / (beta * theta**2 * dt)
  obj%acc(3) = math%one - math%half / (beta * theta)
  obj%acc(4) = math%one / (beta * theta**3 * dt**2)

  obj%initialGuess = math%zero
END SUBROUTINE CollocationUFormCoefficients

!----------------------------------------------------------------------------
!                                               CollocationAFormCoefficients
!----------------------------------------------------------------------------

! Collocation, a-form. The Newmark a-form over the step thetaDt =
! theta dt gives sol = A(n+theta). Then
! A(n+1) = (1 - 1/theta) An + sol/theta, and U(n+1), V(n+1) follow from
! the Newmark relations over dt.

SUBROUTINE CollocationAFormCoefficients(obj, dt)
  CLASS(SDAlgorithm2_), INTENT(INOUT) :: obj
  !! algorithm object
  REAL(DFP), INTENT(IN) :: dt
  !! time step size

  REAL(DFP) :: beta, gamma, theta, thetaDt

  beta = obj%beta
  gamma = obj%gamma
  theta = obj%theta
  thetaDt = theta * dt

  obj%tanmat(1) = math%one
  obj%tanmat(2) = gamma * thetaDt
  obj%tanmat(3) = beta * thetaDt**2

  obj%rhs_u1(1) = math%zero
  obj%rhs_u1(2) = math%zero
  obj%rhs_u1(3) = math%minus_one

  obj%rhs_v1(1) = math%zero
  obj%rhs_v1(2) = math%minus_one
  obj%rhs_v1(3) = -thetaDt

  obj%rhs_a1(1) = math%zero
  obj%rhs_a1(2) = -thetaDt * (math%one - gamma)
  obj%rhs_a1(3) = -thetaDt**2 * (math%half - beta)

  obj%dis(1) = math%one
  obj%dis(2) = dt
  obj%dis(3) = dt**2 * (math%half - beta / theta)
  obj%dis(4) = beta * dt**2 / theta

  obj%vel(1) = math%zero
  obj%vel(2) = math%one
  obj%vel(3) = dt * (math%one - gamma / theta)
  obj%vel(4) = gamma * dt / theta

  obj%acc(1) = math%zero
  obj%acc(2) = math%zero
  obj%acc(3) = math%one - math%one / theta
  obj%acc(4) = math%one / theta

  obj%initialGuess(1) = math%zero
  obj%initialGuess(2) = math%zero
  obj%initialGuess(3) = math%one
END SUBROUTINE CollocationAFormCoefficients

!----------------------------------------------------------------------------
!                                                               IsInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsInitiated
ans = obj%isInit
END PROCEDURE obj_IsInitiated

!----------------------------------------------------------------------------
!                                                                   SetForm
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetForm
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetForm()"
LOGICAL(LGT) :: isok
#endif

CHARACTER(1) :: aform

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

aform = UpperCase(FORM(1:1))

#ifdef DEBUG_VER
isok = (aform .EQ. "U") .OR. (aform .EQ. "A")
CALL AssertError1(isok, myName, 'form must be "U" or "A"')
#endif

obj%FORM = aform
obj%isInit = math%no

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetForm

!----------------------------------------------------------------------------
!                                                                   GetForm
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetForm
ans = obj%FORM
END PROCEDURE obj_GetForm

!----------------------------------------------------------------------------
!                                                        SetForceEvaluation
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetForceEvaluation
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetForceEvaluation()"
LOGICAL(LGT) :: isok
#endif

CHARACTER(1) :: aopt

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

aopt = UpperCase(option(1:1))

#ifdef DEBUG_VER
isok = (aopt .EQ. "I") .OR. (aopt .EQ. "E")
CALL AssertError1(isok, myName, &
                  'forceEvaluation must be "interpolate" or "evaluate"')
#endif

obj%forceEvaluation = aopt
obj%isInit = math%no

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetForceEvaluation

!----------------------------------------------------------------------------
!                                                        GetForceEvaluation
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetForceEvaluation
ans = obj%forceEvaluation
END PROCEDURE obj_GetForceEvaluation

!----------------------------------------------------------------------------
!                                                              GetForceTime
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetForceTime
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetForceTime()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
CALL AssertError1(obj%isInit, myName, &
                  "the object is not initiated, call Initiate(dt)")
#endif

ans = t + obj%forceTime * obj%dt

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetForceTime

!----------------------------------------------------------------------------
!                                                                     Houbolt
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_HouboltMethod
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_HouboltMethod()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_HouboltMethod

!----------------------------------------------------------------------------
!                                                                   MakeZeros
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_MakeZeros
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_MakeZeros()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%tanmat_zero = obj%tanmat .approxeq. math%zero
obj%rhs_u1_zero = obj%rhs_u1 .approxeq. math%zero
obj%rhs_v1_zero = obj%rhs_v1 .approxeq. math%zero
obj%rhs_a1_zero = obj%rhs_a1 .approxeq. math%zero
obj%rhs_f1_zero = obj%rhs_f1 .approxeq. math%zero
obj%rhs_f2_zero = obj%rhs_f2 .approxeq. math%zero
obj%dis_zero = obj%dis .approxeq. math%zero
obj%vel_zero = obj%vel .approxeq. math%zero
obj%acc_zero = obj%acc .approxeq. math%zero
obj%initialGuess_zero = obj%initialGuess .approxeq. math%zero

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_MakeZeros

!----------------------------------------------------------------------------
!                                                                  Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%name = "NEWM"
obj%FORM = "U"
obj%isInit = math%no
obj%alphaM = math%zero
obj%alphaF = math%zero
obj%beta = 0.25_DFP
obj%gamma = math%half
obj%theta = math%one
obj%dt = math%zero
obj%forceEvaluation = "I"
obj%forceTime = math%one

obj%tanmat = math%zero
obj%tanmat_zero = math%yes

obj%rhs_u1 = math%zero
obj%rhs_v1 = math%zero
obj%rhs_a1 = math%zero

obj%rhs_u1_zero = math%yes
obj%rhs_v1_zero = math%yes
obj%rhs_a1_zero = math%yes

obj%rhs_f1 = math%zero
obj%rhs_f1_zero = math%yes

obj%rhs_f2 = math%zero
obj%rhs_f2_zero = math%yes

obj%dis = math%zero
obj%vel = math%zero
obj%acc = math%zero

obj%dis_zero = math%yes
obj%vel_zero = math%yes
obj%acc_zero = math%yes

obj%initialGuess = math%zero
obj%initialGuess_zero = math%yes

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Display(msg, unitno=unitno)
CALL Display(obj%name, "name: ", unitno=unitno)
CALL Display(obj%form, "form: ", unitno=unitno)
CALL Display(obj%isInit, "isInit: ", unitno=unitno)
CALL Display(obj%alphaM, "alphaM: ", unitno=unitno)
CALL Display(obj%alphaF, "alphaF: ", unitno=unitno)
CALL Display(obj%beta, "beta: ", unitno=unitno)
CALL Display(obj%gamma, "gamma: ", unitno=unitno)
CALL Display(obj%theta, "theta: ", unitno=unitno)
CALL Display(obj%dt, "dt: ", unitno=unitno)
CALL Display(obj%forceEvaluation, "forceEvaluation: ", unitno=unitno)
CALL Display(obj%forceTime, "forceTime: ", unitno=unitno)
CALL Display(obj%tanmat, "tanmat: ", unitno=unitno, advance="NO")
CALL Display(obj%tanmat_zero, "tanmat_zero: ", unitno=unitno)
CALL Display(obj%rhs_u1, "rhs_u1: ", unitno=unitno, advance="NO")
CALL Display(obj%rhs_u1_zero, "rhs_u1_zero: ", unitno=unitno)
CALL Display(obj%rhs_v1, "rhs_v1: ", unitno=unitno, advance="NO")
CALL Display(obj%rhs_v1_zero, "rhs_v1_zero: ", unitno=unitno)
CALL Display(obj%rhs_a1, "rhs_a1: ", unitno=unitno, advance="NO")
CALL Display(obj%rhs_a1_zero, "rhs_a1_zero: ", unitno=unitno)
CALL Display(obj%rhs_f1, "rhs_f1: ", unitno=unitno, advance="NO")
CALL Display(obj%rhs_f1_zero, "rhs_f1_zero: ", unitno=unitno)
CALL Display(obj%rhs_f2, "rhs_f2: ", unitno=unitno, advance="NO")
CALL Display(obj%rhs_f2_zero, "rhs_f2_zero: ", unitno=unitno)
CALL Display(obj%dis, "dis: ", unitno=unitno, advance="NO")
CALL Display(obj%dis_zero, "dis_zero: ", unitno=unitno)
CALL Display(obj%vel, "vel: ", unitno=unitno, advance="NO")
CALL Display(obj%vel_zero, "vel_zero: ", unitno=unitno)
CALL Display(obj%acc, "acc: ", unitno=unitno, advance="NO")
CALL Display(obj%acc_zero, "acc_zero: ", unitno=unitno)
CALL Display(obj%initialGuess, "initialGuess: ", unitno=unitno, advance="NO")
CALL Display(obj%initialGuess_zero, "initialGuess_zero: ", unitno=unitno)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                             Include errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods
