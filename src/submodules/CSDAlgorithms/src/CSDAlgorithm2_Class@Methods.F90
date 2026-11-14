! This program is a part of EASIFEM library
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

SUBMODULE(CSDAlgorithm2_Class) Methods
USE ApproxUtility, ONLY: OPERATOR(.approxeq.)
USE Display_Method, ONLY: Display
USE Display_Method, ONLY: ToString
USE InputUtility, ONLY: Input
USE ReallocateUtility, ONLY: Reallocate
USE StringUtility, ONLY: UpperCase

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                         NewmarkBetaMethod
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
!                                                               BatheMethod
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_BatheMethod
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_BatheMethod()"
LOGICAL(LGT) :: isok
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()

obj%name = "BATH"

obj%splittingRatio = Input(default=math%half, option=splittingRatio)

#ifdef DEBUG_VER
isok = (obj%splittingRatio .GT. math%zero) .AND. &
       (obj%splittingRatio .LT. math%two)
CALL AssertError1(isok, myName, "splittingRatio must be in (0, 2)")
#endif

IF (PRESENT(rhoInf)) THEN
  obj%rhoInf = rhoInf
#ifdef DEBUG_VER
  isok = (rhoInf .GE. math%zero) .AND. (rhoInf .LE. math%one)
  CALL AssertError1(isok, myName, "rhoInf must be in [0, 1]")
#endif
  CALL SetWeightsRhoBathe(obj)
ELSE IF (PRESENT(beta1) .OR. PRESENT(beta2)) THEN
  obj%beta1 = Input(default=math%one / 3.0_DFP, option=beta1)
  obj%beta2 = Input(default=math%one - obj%beta1, option=beta2)
  CALL SetWeightsBetaBathe(obj)
ELSE
  CALL SetWeightsGammaBathe(obj)
END IF

#ifdef DEBUG_VER
isok = .NOT. (obj%b2 .approxeq. math%zero)
CALL AssertError1(isok, myName, "b2 of Bathe method must not be zero")
#endif

IF (PRESENT(FORM)) CALL obj%SetForm(FORM)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_BatheMethod

!----------------------------------------------------------------------------
!                                                       SetWeightsGammaBathe
!----------------------------------------------------------------------------

! gamma-Bathe: b0 = b1 = 1/(2 (2 - s)), b2 = (1 - s)/(2 - s), where s is
! the splitting ratio

SUBROUTINE SetWeightsGammaBathe(obj)
  CLASS(CSDAlgorithm2_), INTENT(INOUT) :: obj

  REAL(DFP) :: twoMinusRatio

  twoMinusRatio = math%two - obj%splittingRatio

  obj%b0 = math%half / twoMinusRatio

  obj%b1 = obj%b0

  obj%b2 = (math%one - obj%splittingRatio) / twoMinusRatio

END SUBROUTINE SetWeightsGammaBathe

!----------------------------------------------------------------------------
!                                                        SetWeightsBetaBathe
!----------------------------------------------------------------------------

! beta1/beta2-Bathe: b0 = (1 - beta1) s, b1 = (1 - beta2)(1 - s) + beta1 s,
! b2 = (1 - s) beta2, where s is the splitting ratio

SUBROUTINE SetWeightsBetaBathe(obj)
  CLASS(CSDAlgorithm2_), INTENT(INOUT) :: obj

  REAL(DFP) :: ratio

  ratio = obj%splittingRatio

  obj%b0 = (math%one - obj%beta1) * ratio

  obj%b1 = (math%one - obj%beta2) * (math%one - ratio) + obj%beta1 * ratio

  obj%b2 = (math%one - ratio) * obj%beta2

END SUBROUTINE SetWeightsBetaBathe

!----------------------------------------------------------------------------
!                                                         SetWeightsRhoBathe
!----------------------------------------------------------------------------

! rho-infinity-Bathe: b1 = (rhoInf + 1)/(2 s (rhoInf - 1) + 4),
! b0 = 1/2 + (s - 1) b1, b2 = 1/2 - s b1, where s is the splitting ratio

SUBROUTINE SetWeightsRhoBathe(obj)
  CLASS(CSDAlgorithm2_), INTENT(INOUT) :: obj

  REAL(DFP) :: ratio

  ratio = obj%splittingRatio

  obj%b1 = (obj%rhoInf + math%one) / &
           (math%two * ratio * (obj%rhoInf - math%one) + 4.0_DFP)

  obj%b0 = math%half + (ratio - math%one) * obj%b1

  obj%b2 = math%half - ratio * obj%b1

END SUBROUTINE SetWeightsRhoBathe

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = dt .GT. math%zero
CALL AssertError1(isok, myName, "dt must be positive")

isok = (obj%FORM .EQ. "A") .OR. (.NOT. (obj%beta .approxeq. math%zero))
CALL AssertError1(isok, myName, "beta = 0 is not allowed in the u-form")
#endif

SELECT CASE (obj%name)
CASE ("NEWM")
  CALL AllocateCoefficients(obj, totalSubstep=math%one_i)
CASE ("BATH")
  CALL AllocateCoefficients(obj, totalSubstep=math%two_i)
CASE DEFAULT
#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, "unknown method name: "//obj%name)
#endif
END SELECT

! coefficients of the method, see the routines of each method below
IF (obj%FORM .EQ. "A") THEN
  CALL SetAFormCoefficients_master(obj, dt)
ELSE
  CALL SetUFormCoefficients_master(obj, dt)
END IF

! the coefficient of the force is 1 in every substep
DO ii = 1, obj%totalSubstep
  IF (obj%forceEvaluation .EQ. "E") THEN
    obj%rhs_f1(ii) = math%zero
    obj%rhs_f2(ii) = math%one
  ELSE
    obj%rhs_f1(ii) = math%one - obj%forceTime(ii)
    obj%rhs_f2(ii) = obj%forceTime(ii)
  END IF
END DO

obj%dt = dt
obj%currentSubstep = math%one_i
obj%isInit = math%yes

CALL obj%MakeZeros()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                       AllocateCoefficients
!----------------------------------------------------------------------------

! Allocate the coefficients and their zero flags for totalSubstep
! substeps and set them to zero (false); the history has
! 3 + totalSubstep entries. Reallocate keeps the memory if the shape does
! not change.

SUBROUTINE AllocateCoefficients(obj, totalSubstep)
  CLASS(CSDAlgorithm2_), INTENT(INOUT) :: obj
  !! algorithm object
  INTEGER(I4B), INTENT(IN) :: totalSubstep
  !! number of substeps

  INTEGER(I4B) :: nhist

  obj%totalSubstep = totalSubstep
  nhist = 3 + totalSubstep

  CALL Reallocate(obj%tanmat, 3, totalSubstep)
  CALL Reallocate(obj%tanmat_zero, 3, totalSubstep)
  CALL Reallocate(obj%rhs, 3, nhist, totalSubstep)
  CALL Reallocate(obj%rhs_zero, 3, nhist, totalSubstep)
  CALL Reallocate(obj%rhs_f1, totalSubstep)
  CALL Reallocate(obj%rhs_f1_zero, totalSubstep)
  CALL Reallocate(obj%rhs_f2, totalSubstep)
  CALL Reallocate(obj%rhs_f2_zero, totalSubstep)
  CALL Reallocate(obj%forceTime, totalSubstep)
  CALL Reallocate(obj%substepTime, totalSubstep)
  CALL Reallocate(obj%dis, nhist)
  CALL Reallocate(obj%dis_zero, nhist)
  CALL Reallocate(obj%vel, nhist)
  CALL Reallocate(obj%vel_zero, nhist)
  CALL Reallocate(obj%acc, nhist)
  CALL Reallocate(obj%acc_zero, nhist)
  CALL Reallocate(obj%initialGuess, nhist, totalSubstep)
  CALL Reallocate(obj%initialGuess_zero, nhist, totalSubstep)
END SUBROUTINE AllocateCoefficients

!----------------------------------------------------------------------------
!                                                     DeallocateCoefficients
!----------------------------------------------------------------------------

SUBROUTINE DeallocateCoefficients(obj)
  CLASS(CSDAlgorithm2_), INTENT(INOUT) :: obj
  !! algorithm object

  ! the coefficients are allocated together in AllocateCoefficients
  IF (ALLOCATED(obj%tanmat)) THEN
    DEALLOCATE (obj%tanmat, obj%tanmat_zero, obj%rhs, obj%rhs_zero, &
                obj%rhs_f1, obj%rhs_f1_zero, obj%rhs_f2, obj%rhs_f2_zero, &
                obj%forceTime, obj%substepTime, obj%dis, obj%dis_zero, &
                obj%vel, obj%vel_zero, obj%acc, obj%acc_zero, &
                obj%initialGuess, obj%initialGuess_zero)
  END IF
  obj%totalSubstep = math%zero_i
END SUBROUTINE DeallocateCoefficients

!----------------------------------------------------------------------------
!                                                SetUFormCoefficients_master
!----------------------------------------------------------------------------

! Coefficients of the u-form (sol_k is a displacement) of obj%name

SUBROUTINE SetUFormCoefficients_master(obj, dt)
  CLASS(CSDAlgorithm2_), INTENT(INOUT) :: obj

  REAL(DFP), INTENT(IN) :: dt

  SELECT CASE (obj%name)
  CASE ("NEWM")
    CALL NewmarkUFormCoefficients(obj, dt)
  CASE ("BATH")
    CALL BatheUFormCoefficients(obj, dt)
  CASE DEFAULT
    ! unknown names are rejected in Initiate
  END SELECT
END SUBROUTINE SetUFormCoefficients_master

!----------------------------------------------------------------------------
!                                                SetAFormCoefficients_master
!----------------------------------------------------------------------------

! Coefficients of the a-form (sol_k is an acceleration) of obj%name

SUBROUTINE SetAFormCoefficients_master(obj, dt)
  CLASS(CSDAlgorithm2_), INTENT(INOUT) :: obj

  REAL(DFP), INTENT(IN) :: dt
  !! time step size

  SELECT CASE (obj%name)
  CASE ("NEWM")
    CALL NewmarkAFormCoefficients(obj, dt)
  CASE ("BATH")
    CALL BatheAFormCoefficients(obj, dt)
  CASE DEFAULT
    ! unknown names are rejected in Initiate
  END SELECT
END SUBROUTINE SetAFormCoefficients_master

!----------------------------------------------------------------------------
!                                                   NewmarkUFormCoefficients
!----------------------------------------------------------------------------

! Newmark-beta, u-form, one substep, sol_1 = U(n+1). History:
! x = [Un, Vn, An, sol_1]. The balance M A + C V + K U = F at t(n+1) with
!   U(n+1) = Un + dt Vn + dt^2 ((1/2 - beta) An + beta A(n+1))
!   V(n+1) = Vn + dt ((1 - gamma) An + gamma A(n+1))
! gives
!   [M/(beta dt^2) + gamma C/(beta dt) + K] sol_1 = F(n+1)
!     + M (Un/(beta dt^2) + Vn/(beta dt) + (1/(2 beta) - 1) An)
!     + C (gamma Un/(beta dt) + (gamma/beta - 1) Vn
!          + dt (gamma/(2 beta) - 1) An)

SUBROUTINE NewmarkUFormCoefficients(obj, dt)
  CLASS(CSDAlgorithm2_), INTENT(INOUT) :: obj

  REAL(DFP), INTENT(IN) :: dt
  !! time step size

  REAL(DFP) :: beta, gamma

  beta = obj%beta
  gamma = obj%gamma

  obj%substepTime(1) = math%one
  obj%forceTime(1) = math%one

  obj%tanmat(1, 1) = math%one / (beta * dt**2)
  obj%tanmat(2, 1) = gamma / (beta * dt)
  obj%tanmat(3, 1) = math%one

  ! M times Un, Vn, An
  obj%rhs(1, 1, 1) = math%one / (beta * dt**2)
  obj%rhs(1, 2, 1) = math%one / (beta * dt)
  obj%rhs(1, 3, 1) = math%half / beta - math%one

  ! C times Un, Vn, An
  obj%rhs(2, 1, 1) = gamma / (beta * dt)
  obj%rhs(2, 2, 1) = gamma / beta - math%one
  obj%rhs(2, 3, 1) = dt * (math%half * gamma / beta - math%one)

  ! update: x = [Un, Vn, An, sol_1]
  obj%dis(4) = math%one

  obj%vel(1) = -gamma / (beta * dt)
  obj%vel(2) = math%one - gamma / beta
  obj%vel(3) = dt * (math%one - math%half * gamma / beta)
  obj%vel(4) = gamma / (beta * dt)

  obj%acc(1) = -math%one / (beta * dt**2)
  obj%acc(2) = -math%one / (beta * dt)
  obj%acc(3) = math%one - math%half / beta
  obj%acc(4) = math%one / (beta * dt**2)
END SUBROUTINE NewmarkUFormCoefficients

!----------------------------------------------------------------------------
!                                                   NewmarkAFormCoefficients
!----------------------------------------------------------------------------

! Newmark-beta, a-form, one substep, sol_1 = A(n+1). With the predictors
!   U~ = Un + dt Vn + dt^2 (1/2 - beta) An,  V~ = Vn + dt (1 - gamma) An
! the balance M A + C V + K U = F at t(n+1) gives
!   [M + gamma dt C + beta dt^2 K] sol_1 = F(n+1) - C V~ - K U~
! and U(n+1) = U~ + beta dt^2 sol_1, V(n+1) = V~ + gamma dt sol_1.

SUBROUTINE NewmarkAFormCoefficients(obj, dt)
  CLASS(CSDAlgorithm2_), INTENT(INOUT) :: obj

  REAL(DFP), INTENT(IN) :: dt
  !! time step size

  REAL(DFP) :: beta, gamma

  beta = obj%beta
  gamma = obj%gamma

  obj%substepTime(1) = math%one
  obj%forceTime(1) = math%one

  obj%tanmat(1, 1) = math%one
  obj%tanmat(2, 1) = gamma * dt
  obj%tanmat(3, 1) = beta * dt**2

  ! C times Vn, An
  obj%rhs(2, 2, 1) = math%minus_one
  obj%rhs(2, 3, 1) = -dt * (math%one - gamma)

  ! K times Un, Vn, An
  obj%rhs(3, 1, 1) = math%minus_one
  obj%rhs(3, 2, 1) = -dt
  obj%rhs(3, 3, 1) = -dt**2 * (math%half - beta)

  ! update: x = [Un, Vn, An, sol_1]
  obj%dis(1) = math%one
  obj%dis(2) = dt
  obj%dis(3) = dt**2 * (math%half - beta)
  obj%dis(4) = beta * dt**2

  obj%vel(2) = math%one
  obj%vel(3) = dt * (math%one - gamma)
  obj%vel(4) = gamma * dt

  obj%acc(4) = math%one

  ! initial guess of sol_1 is An
  obj%initialGuess(3, 1) = math%one
END SUBROUTINE NewmarkAFormCoefficients

!----------------------------------------------------------------------------
!                                                     BatheUFormCoefficients
!----------------------------------------------------------------------------

! Bathe, u-form, two substeps, sol_1 = U(n+s), sol_2 = U(n+1), where s is
! the splitting ratio. History: x = [Un, Vn, An, sol_1, sol_2].
!
! Substep 1, trapezoidal rule over h = s dt:
!   V(n+s) = 2/h (sol_1 - Un) - Vn
!   A(n+s) = (2/h)^2 (sol_1 - Un) - 4/h Vn - An
!   [(2/h)^2 M + 2/h C + K] sol_1 = F(n+s)
!     + M ((2/h)^2 Un + 4/h Vn + An) + C (2/h Un + Vn)
!
! Substep 2, three point backward Euler rule over dt with b0, b1, b2:
!   V(n+1) = (sol_2 - Un)/(b2 dt) - b0/b2 Vn - b1/b2 V(n+s)
!   A(n+1) = (V(n+1) - Vn)/(b2 dt) - b0/b2 An - b1/b2 A(n+s)
!   [M/(b2 dt)^2 + C/(b2 dt) + K] sol_2 = F(n+1) + M (...) + C (...)
! where the M and C terms are the parts of A(n+1) and V(n+1) that do
! not contain sol_2, with the sign changed.

SUBROUTINE BatheUFormCoefficients(obj, dt)
  CLASS(CSDAlgorithm2_), INTENT(INOUT) :: obj

  REAL(DFP), INTENT(IN) :: dt
  !! time step size

  REAL(DFP) :: ratio, substepDt, b0, b1, b2

  ratio = obj%splittingRatio
  substepDt = ratio * dt
  b0 = obj%b0
  b1 = obj%b1
  b2 = obj%b2

  obj%substepTime(1) = ratio
  obj%forceTime(1) = ratio
  obj%substepTime(2) = math%one
  obj%forceTime(2) = math%one

  ! substep 1
  obj%tanmat(1, 1) = 4.0_DFP / substepDt**2
  obj%tanmat(2, 1) = math%two / substepDt
  obj%tanmat(3, 1) = math%one

  ! M times Un, Vn, An
  obj%rhs(1, 1, 1) = 4.0_DFP / substepDt**2
  obj%rhs(1, 2, 1) = 4.0_DFP / substepDt
  obj%rhs(1, 3, 1) = math%one

  ! C times Un, Vn
  obj%rhs(2, 1, 1) = math%two / substepDt
  obj%rhs(2, 2, 1) = math%one

  ! substep 2
  obj%tanmat(1, 2) = math%one / (b2 * dt)**2
  obj%tanmat(2, 2) = math%one / (b2 * dt)
  obj%tanmat(3, 2) = math%one

  ! M times Un, Vn, An, sol_1
  obj%rhs(1, 1, 2) = -(math%two * b1 / (b2**2 * ratio * dt**2) &
                       - math%one / (b2 * dt)**2 &
                       + 4.0_DFP * b1 / (b2 * ratio**2 * dt**2))
  obj%rhs(1, 2, 2) = -((b1 - b0) / (b2**2 * dt) - math%one / (b2 * dt) &
                       + 4.0_DFP * b1 / (b2 * ratio * dt))
  obj%rhs(1, 3, 2) = -(b1 - b0) / b2
  obj%rhs(1, 4, 2) = math%two * b1 / (b2**2 * ratio * dt**2) &
                     + 4.0_DFP * b1 / (b2 * ratio**2 * dt**2)

  ! C times Un, Vn, sol_1
  obj%rhs(2, 1, 2) = -(math%two * b1 / (b2 * ratio * dt) &
                       - math%one / (b2 * dt))
  obj%rhs(2, 2, 2) = -(b1 - b0) / b2
  obj%rhs(2, 4, 2) = math%two * b1 / (b2 * ratio * dt)

  ! update: x = [Un, Vn, An, sol_1, sol_2]
  obj%dis(5) = math%one

  obj%vel(1) = math%two * b1 / (b2 * ratio * dt) - math%one / (b2 * dt)
  obj%vel(2) = (b1 - b0) / b2
  obj%vel(4) = -math%two * b1 / (b2 * ratio * dt)
  obj%vel(5) = math%one / (b2 * dt)

  obj%acc(1) = -obj%rhs(1, 1, 2)
  obj%acc(2) = -obj%rhs(1, 2, 2)
  obj%acc(3) = -obj%rhs(1, 3, 2)
  obj%acc(4) = -obj%rhs(1, 4, 2)
  obj%acc(5) = math%one / (b2 * dt)**2
END SUBROUTINE BatheUFormCoefficients

!----------------------------------------------------------------------------
!                                                     BatheAFormCoefficients
!----------------------------------------------------------------------------

! Bathe, a-form, two substeps, sol_1 = A(n+s), sol_2 = A(n+1), where s is
! the splitting ratio. History: x = [Un, Vn, An, sol_1, sol_2].
!
! Substep 1, trapezoidal rule over h = s dt:
!   V(n+s) = Vn + h/2 (An + sol_1)
!   U(n+s) = Un + h Vn + (h/2)^2 (An + sol_1)
!   [M + h/2 C + (h/2)^2 K] sol_1 = F(n+s) - C (Vn + h/2 An)
!     - K (Un + h Vn + (h/2)^2 An)
!
! Substep 2, three point backward Euler rule over dt with b0, b1, b2:
!   V(n+1) = Vn + dt (b0 An + b1 sol_1 + b2 sol_2)
!   U(n+1) = Un + dt (b0 + b1 + b2) Vn + dt^2 (s b1/2 + b0 b2) An
!            + dt^2 (s b1/2 + b1 b2) sol_1 + (b2 dt)^2 sol_2
!   [M + b2 dt C + (b2 dt)^2 K] sol_2 = F(n+1) - C (...) - K (...)

SUBROUTINE BatheAFormCoefficients(obj, dt)
  CLASS(CSDAlgorithm2_), INTENT(INOUT) :: obj

  REAL(DFP), INTENT(IN) :: dt
  !! time step size

  REAL(DFP) :: ratio, substepDt, b0, b1, b2

  ratio = obj%splittingRatio
  substepDt = ratio * dt
  b0 = obj%b0
  b1 = obj%b1
  b2 = obj%b2

  obj%substepTime(1) = ratio
  obj%forceTime(1) = ratio
  obj%substepTime(2) = math%one
  obj%forceTime(2) = math%one

  ! substep 1
  obj%tanmat(1, 1) = math%one
  obj%tanmat(2, 1) = math%half * substepDt
  obj%tanmat(3, 1) = (math%half * substepDt)**2

  ! C times Vn, An
  obj%rhs(2, 2, 1) = math%minus_one
  obj%rhs(2, 3, 1) = -math%half * substepDt

  ! K times Un, Vn, An
  obj%rhs(3, 1, 1) = math%minus_one
  obj%rhs(3, 2, 1) = -substepDt
  obj%rhs(3, 3, 1) = -(math%half * substepDt)**2

  ! substep 2
  obj%tanmat(1, 2) = math%one
  obj%tanmat(2, 2) = b2 * dt
  obj%tanmat(3, 2) = (b2 * dt)**2

  ! C times Vn, An, sol_1
  obj%rhs(2, 2, 2) = math%minus_one
  obj%rhs(2, 3, 2) = -b0 * dt
  obj%rhs(2, 4, 2) = -b1 * dt

  ! K times Un, Vn, An, sol_1
  obj%rhs(3, 1, 2) = math%minus_one
  obj%rhs(3, 2, 2) = -(b0 + b1 + b2) * dt
  obj%rhs(3, 3, 2) = -(math%half * ratio * b1 + b0 * b2) * dt**2
  obj%rhs(3, 4, 2) = -(math%half * ratio * b1 + b1 * b2) * dt**2

  ! update: x = [Un, Vn, An, sol_1, sol_2]
  obj%dis(1) = math%one
  obj%dis(2) = (b0 + b1 + b2) * dt
  obj%dis(3) = (math%half * ratio * b1 + b0 * b2) * dt**2
  obj%dis(4) = (math%half * ratio * b1 + b1 * b2) * dt**2
  obj%dis(5) = (b2 * dt)**2

  obj%vel(2) = math%one
  obj%vel(3) = b0 * dt
  obj%vel(4) = b1 * dt
  obj%vel(5) = b2 * dt

  obj%acc(5) = math%one

  ! initial guess of sol_1 and sol_2 is An
  obj%initialGuess(3, 1) = math%one
  obj%initialGuess(3, 2) = math%one
END SUBROUTINE BatheAFormCoefficients

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
LOGICAL(LGT) :: isok
#endif

#ifdef DEBUG_VER
isok = obj%isInit .AND. (substep .GE. 1) .AND. &
       (substep .LE. obj%totalSubstep)
CALL AssertError1(isok, myName, &
                  "the object is not initiated or substep is out of range")
#endif

ans = t + obj%forceTime(substep) * obj%dt
END PROCEDURE obj_GetForceTime

!----------------------------------------------------------------------------
!                                                            GetSubstepTime
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSubstepTime
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetSubstepTime()"
LOGICAL(LGT) :: isok
#endif

#ifdef DEBUG_VER
isok = obj%isInit .AND. (substep .GE. 1) .AND. &
       (substep .LE. obj%totalSubstep)
CALL AssertError1(isok, myName, &
                  "the object is not initiated or substep is out of range")
#endif

ans = t + obj%substepTime(substep) * obj%dt

END PROCEDURE obj_GetSubstepTime

!----------------------------------------------------------------------------
!                                                           GetTotalSubstep
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalSubstep
ans = obj%totalSubstep
END PROCEDURE obj_GetTotalSubstep

!----------------------------------------------------------------------------
!                                                         GetCurrentSubstep
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetCurrentSubstep
ans = obj%currentSubstep
END PROCEDURE obj_GetCurrentSubstep

!----------------------------------------------------------------------------
!                                                      UpdateCurrentSubstep
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_UpdateCurrentSubstep
IF (obj%currentSubstep .GE. obj%totalSubstep) THEN
  obj%currentSubstep = math%one_i
ELSE
  obj%currentSubstep = obj%currentSubstep + math%one_i
END IF
END PROCEDURE obj_UpdateCurrentSubstep

!----------------------------------------------------------------------------
!                                                                 MakeZeros
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_MakeZeros
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_MakeZeros()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (ALLOCATED(obj%tanmat)) THEN
  obj%tanmat_zero = obj%tanmat .approxeq. math%zero
  obj%rhs_zero = obj%rhs .approxeq. math%zero
  obj%rhs_f1_zero = obj%rhs_f1 .approxeq. math%zero
  obj%rhs_f2_zero = obj%rhs_f2 .approxeq. math%zero
  obj%dis_zero = obj%dis .approxeq. math%zero
  obj%vel_zero = obj%vel .approxeq. math%zero
  obj%acc_zero = obj%acc .approxeq. math%zero
  obj%initialGuess_zero = obj%initialGuess .approxeq. math%zero
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_MakeZeros

!----------------------------------------------------------------------------
!                                                                Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%name = "BATH"
obj%FORM = "U"
obj%forceEvaluation = "I"
obj%isInit = math%no
obj%currentSubstep = math%one_i
obj%dt = math%zero
obj%beta = 0.25_DFP
obj%gamma = math%half
obj%splittingRatio = math%half
obj%rhoInf = math%zero
obj%beta1 = math%zero
obj%beta2 = math%zero
obj%b0 = math%zero
obj%b1 = math%zero
obj%b2 = math%zero

CALL DeallocateCoefficients(obj)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                   Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display()"
#endif

INTEGER(I4B) :: ii
CHARACTER(:), ALLOCATABLE :: astr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Display(msg, unitno=unitno)
CALL Display(obj%name, "name: ", unitno=unitno)
CALL Display(obj%form, "form: ", unitno=unitno)
CALL Display(obj%forceEvaluation, "forceEvaluation: ", unitno=unitno)
CALL Display(obj%isInit, "isInit: ", unitno=unitno)
CALL Display(obj%totalSubstep, "totalSubstep: ", unitno=unitno)
CALL Display(obj%currentSubstep, "currentSubstep: ", unitno=unitno)
CALL Display(obj%dt, "dt: ", unitno=unitno)
CALL Display(obj%beta, "beta: ", unitno=unitno)
CALL Display(obj%gamma, "gamma: ", unitno=unitno)
CALL Display(obj%splittingRatio, "splittingRatio: ", unitno=unitno)
CALL Display(obj%rhoInf, "rhoInf: ", unitno=unitno)
CALL Display(obj%beta1, "beta1: ", unitno=unitno)
CALL Display(obj%beta2, "beta2: ", unitno=unitno)
CALL Display([obj%b0, obj%b1, obj%b2], "[b0, b1, b2]: ", unitno=unitno)

IF (ALLOCATED(obj%tanmat)) THEN
  DO ii = 1, obj%totalSubstep
    astr = "substep "//ToString(ii)//" "
    CALL Display(obj%tanmat(:, ii), astr//"tanmat: ", unitno=unitno)
    CALL Display(obj%rhs(1, :, ii), astr//"rhs of M: ", unitno=unitno)
    CALL Display(obj%rhs(2, :, ii), astr//"rhs of C: ", unitno=unitno)
    CALL Display(obj%rhs(3, :, ii), astr//"rhs of K: ", unitno=unitno)
    CALL Display([obj%rhs_f1(ii), obj%rhs_f2(ii)], &
                 astr//"[rhs_f1, rhs_f2]: ", unitno=unitno)
    CALL Display([obj%substepTime(ii), obj%forceTime(ii)], &
                 astr//"[substepTime, forceTime]: ", unitno=unitno)
    CALL Display(obj%initialGuess(:, ii), astr//"initialGuess: ", &
                 unitno=unitno)
  END DO
  CALL Display(obj%dis, "dis: ", unitno=unitno)
  CALL Display(obj%vel, "vel: ", unitno=unitno)
  CALL Display(obj%acc, "acc: ", unitno=unitno)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                            Include errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods
