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

MODULE SDAlgorithm2_Class
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE GlobalData, ONLY: DFP, I4B, LGT
USE tomlf, ONLY: toml_table
USE TxtFile_Class, ONLY: TxtFile_

IMPLICIT NONE

PRIVATE
PUBLIC :: SDAlgorithm2_
PUBLIC :: SDAlgorithm2Pointer_
CHARACTER(*), PARAMETER :: modName = "SDAlgorithm2_Class()"

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-24
! summary:  Single step semi discrete algorithm for second order systems
!
! The object holds the parameters of a single step method (Newmark-beta,
! HHT-alpha, generalized-alpha, collocation) and the coefficients that a
! kernel needs to
! assemble and solve one time step of
! $M \ddot{u} + C \dot{u} + K u = F$.
!
! A method routine (`NewmarkBetaMethod`, `HHTAlphaMethod`,
! `GeneralizedAlphaMethod`, `CollocationMethod`) or `ImportFromToml` sets
! only the parameters. The
! coefficients are computed by `Initiate(dt)`, and they already contain
! the time step size dt. Call `Initiate` again whenever dt changes. The
! kernel uses the coefficients as they are:
!
! T   = tanmat(1) M + tanmat(2) C + tanmat(3) K
! RHS = M (rhs_u1(1) Un + rhs_v1(1) Vn + rhs_a1(1) An)
!     + C (rhs_u1(2) Un + rhs_v1(2) Vn + rhs_a1(2) An)
!     + K (rhs_u1(3) Un + rhs_v1(3) Vn + rhs_a1(3) An)
!     + rhs_f2 F(n+1) + rhs_f1 F(n)
!
! where Un, Vn, An are the displacement, velocity and acceleration at tn.
! After solving T sol = RHS, the state at t(n+1) is
!
! U = dis(1) Un + dis(2) Vn + dis(3) An + dis(4) sol
! V = vel(1) Un + vel(2) Vn + vel(3) An + vel(4) sol
! A = acc(1) Un + acc(2) Vn + acc(3) An + acc(4) sol
!
! Newmark-beta and HHT-alpha are special cases of the generalized-alpha
! method (Chung and Hulbert, 1993)
!
! M A(n+1-alphaM) + C V(n+1-alphaF) + K U(n+1-alphaF) = F(n+1-alphaF)
!
! with X(n+1-a) = (1-a) X(n+1) + a X(n): Newmark has alphaM = alphaF = 0
! and HHT has alphaM = 0, alphaF = -alpha. The coefficient of the force
! is 1 in both forms below, so RHS has the units of force.
!
! Two forms are available. In the u-form (default, form = "U") the
! unknown sol is the displacement at t(n+1), and the coefficient of K
! in T is 1-alphaF. This is the effective stiffness form of the
! textbooks, for example Newmark gives
! T = M/(beta dt^2) + gamma C/(beta dt) + K. In the a-form (form = "A")
! the unknown sol is the acceleration at t(n+1), and the coefficient of
! M in T is 1-alphaM, for example Newmark gives
! T = M + gamma dt C + beta dt^2 K.
!
! Each method writes the balance at the force time
! t_f = tn + forceTime dt, where forceTime is 1 for Newmark, 1-alphaF for
! HHT and generalized-alpha, and theta for collocation. The force of that
! balance is obtained in one of two ways (forceEvaluation):
!
! - "I", interpolate (default): F(t_f) = (1-forceTime) F(n) +
!   forceTime F(n+1), so rhs_f1 = 1-forceTime and rhs_f2 = forceTime.
!   The kernel needs only the forces at tn and t(n+1).
! - "E", evaluate: the kernel evaluates the force at t_f (see
!   GetForceTime) and passes it as F(n+1); rhs_f1 = 0 and rhs_f2 = 1.
!
! Both are equal when the force is linear in time inside the step.
! Dirichlet values are always given at t(n+1).

TYPE :: SDAlgorithm2_
  CHARACTER(4) :: name = "NEWM"
  !! name of the method: NEWM, HHTA, GENA, COLL
  CHARACTER(1) :: FORM = "U"
  !! U: displacement is the unknown, A: acceleration is the unknown
  LOGICAL(LGT) :: isInit = math%no
  !! true after Initiate, false after a parameter changes

  REAL(DFP) :: alphaM = math%zero
  !! generalized-alpha parameter of the inertia term, zero for Newmark,
  !! HHT and collocation
  REAL(DFP) :: alphaF = math%zero
  !! generalized-alpha parameter of the other terms, zero for Newmark and
  !! collocation, -alpha for HHT
  REAL(DFP) :: beta = 0.25_DFP
  !! Newmark beta parameter
  REAL(DFP) :: gamma = math%half
  !! Newmark gamma parameter
  REAL(DFP) :: theta = math%one
  !! Collocation (Wilson) theta parameter, 1 for other methods
  REAL(DFP) :: dt = math%zero
  !! time step size used in the last Initiate
  CHARACTER(1) :: forceEvaluation = "I"
  !! I: interpolate the force, E: evaluate the force at the force time
  REAL(DFP) :: forceTime = math%one
  !! force time ratio, t_f = tn + forceTime dt, set by Initiate

  REAL(DFP) :: tanmat(3) = math%zero
  !! coefficients of M, C, K in the tangent matrix
  LOGICAL(LGT) :: tanmat_zero(3) = math%yes
  !! true where tanmat is zero

  REAL(DFP) :: rhs_u1(3) = math%zero
  !! coefficients of M Un, C Un, K Un in the right hand side
  LOGICAL(LGT) :: rhs_u1_zero(3) = math%yes
  !! true where rhs_u1 is zero

  REAL(DFP) :: rhs_v1(3) = math%zero
  !! coefficients of M Vn, C Vn, K Vn in the right hand side
  LOGICAL(LGT) :: rhs_v1_zero(3) = math%yes
  !! true where rhs_v1 is zero

  REAL(DFP) :: rhs_a1(3) = math%zero
  !! coefficients of M An, C An, K An in the right hand side
  LOGICAL(LGT) :: rhs_a1_zero(3) = math%yes
  !! true where rhs_a1 is zero

  REAL(DFP) :: rhs_f1 = math%zero
  !! coefficient of the force at tn in the right hand side
  LOGICAL(LGT) :: rhs_f1_zero = math%yes
  !! true if rhs_f1 is zero

  REAL(DFP) :: rhs_f2 = math%zero
  !! coefficient of the force at t(n+1) in the right hand side
  LOGICAL(LGT) :: rhs_f2_zero = math%yes
  !! true if rhs_f2 is zero

  REAL(DFP) :: dis(4) = math%zero
  !! coefficients of Un, Vn, An, sol in the displacement at t(n+1)
  LOGICAL(LGT) :: dis_zero(4) = math%yes
  !! true where dis is zero

  REAL(DFP) :: vel(4) = math%zero
  !! coefficients of Un, Vn, An, sol in the velocity at t(n+1)
  LOGICAL(LGT) :: vel_zero(4) = math%yes
  !! true where vel is zero

  REAL(DFP) :: acc(4) = math%zero
  !! coefficients of Un, Vn, An, sol in the acceleration at t(n+1)
  LOGICAL(LGT) :: acc_zero(4) = math%yes
  !! true where acc is zero

  REAL(DFP) :: initialGuess(3) = math%zero
  !! coefficients of Un, Vn, An in the initial guess of sol
  LOGICAL(LGT) :: initialGuess_zero(3) = math%yes
  !! true where initialGuess is zero

CONTAINS

  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Reset parameters and coefficients
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the content
  PROCEDURE, PUBLIC, PASS(obj) :: MakeZeros => obj_MakeZeros
  !! Set the zero flags from the coefficients
  PROCEDURE, PUBLIC, PASS(obj) :: NewmarkBetaMethod => obj_NewmarkBetaMethod
  !! Set the parameters of Newmark-beta method
  PROCEDURE, PUBLIC, PASS(obj) :: HHTAlphaMethod => obj_HHTAlphaMethod
  !! Set the parameters of HHT-alpha method
  PROCEDURE, PUBLIC, PASS(obj) :: GeneralizedAlphaMethod => &
    obj_GeneralizedAlphaMethod
  !! Set the parameters of generalized-alpha method
  PROCEDURE, PUBLIC, PASS(obj) :: CollocationMethod => obj_CollocationMethod
  !! Set the parameters of collocation method
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  !! Compute the coefficients for a time step size
  PROCEDURE, PUBLIC, PASS(obj) :: IsInitiated => obj_IsInitiated
  !! True if the coefficients are computed
  PROCEDURE, PUBLIC, PASS(obj) :: SetForm => obj_SetForm
  !! Select the u-form or the a-form
  PROCEDURE, PUBLIC, PASS(obj) :: GetForm => obj_GetForm
  !! Get the form, U or A
  PROCEDURE, PUBLIC, PASS(obj) :: SetForceEvaluation => &
    obj_SetForceEvaluation
  !! Select how the force is obtained, interpolate or evaluate
  PROCEDURE, PUBLIC, PASS(obj) :: GetForceEvaluation => &
    obj_GetForceEvaluation
  !! Get how the force is obtained, I or E
  PROCEDURE, PUBLIC, PASS(obj) :: GetForceTime => obj_GetForceTime
  !! Get the time at which the force is evaluated
  PROCEDURE, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  !! Import parameters from a toml table
  PROCEDURE, PASS(obj) :: ImportFromToml2 => obj_ImportFromToml2
  !! Import parameters from a toml file
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, ImportFromToml2
  !! Import parameters from toml
END TYPE SDAlgorithm2_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE SDAlgorithm2Pointer_
  TYPE(SDAlgorithm2_), POINTER :: ptr => NULL()
END TYPE SDAlgorithm2Pointer_

!----------------------------------------------------------------------------
!                                             NewmarkBeta@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-06
! summary: Set the parameters of Newmark-beta method
!
! The routine resets the object and stores beta, gamma and the form. It
! does not compute the coefficients; call `Initiate(dt)` afterwards.
!
! In the u-form, `Initiate(dt)` gives, with h = dt,
! tanmat = (1/(beta h^2), gamma/(beta h), 1). In the a-form it gives
! tanmat = (1, gamma h, beta h^2).
!
! Changes:
!   - 2026-11-12: Only sets parameters; added form. Coefficients are
!     computed by Initiate and are K-normalized in the u-form.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE SDAlgorithm2_Class, ONLY: SDAlgorithm2_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(SDAlgorithm2_) :: obj
!
! CALL obj%NewmarkBetaMethod(beta=0.25_DFP, gamma=0.5_DFP)
! CALL obj%Initiate(dt=0.1_DFP)
! CALL OK(ABS(obj%tanmat(1) - 400.0_DFP) .LT. 1.0E-10_DFP, "tanmat(1)")
!
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]]

INTERFACE
  MODULE SUBROUTINE obj_NewmarkBetaMethod(obj, beta, gamma, FORM)
    CLASS(SDAlgorithm2_), INTENT(INOUT) :: obj
    !! algorithm object
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Default is 0.25
    REAL(DFP), OPTIONAL, INTENT(IN) :: gamma
    !! Default is 0.5
    CHARACTER(*), OPTIONAL, INTENT(IN) :: FORM
    !! "U" (default) or "A"
  END SUBROUTINE obj_NewmarkBetaMethod
END INTERFACE

!----------------------------------------------------------------------------
!                                          HHTAlphaMethod@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-06
! summary:  Set the parameters of Hilber-Hughes-Taylor alpha method
!
! The routine resets the object and stores the parameters and the form.
! It does not compute the coefficients; call `Initiate(dt)` afterwards.
! The method solves
! M A(n+1) + (1+alpha) (C V(n+1) + K U(n+1)) - alpha (C Vn + K Un) = F
! with -1/3 <= alpha <= 0. It is the generalized-alpha method with
! alphaM = 0 and alphaF = -alpha, which are the stored parameters.
!
! Changes:
!   - 2026-11-12: Only sets parameters; added form. Coefficients are
!     computed by Initiate.
!   - 2026-11-12: alpha is stored as alphaF = -alpha, alphaM = 0.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE SDAlgorithm2_Class, ONLY: SDAlgorithm2_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(SDAlgorithm2_) :: obj
!
! CALL obj%HHTAlphaMethod(alpha=-0.1_DFP)
! CALL obj%Initiate(dt=0.1_DFP)
! CALL OK(ABS(obj%tanmat(3) - 0.9_DFP) .LT. 1.0E-10_DFP, "tanmat(3)")
!
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]]

INTERFACE
  MODULE SUBROUTINE obj_HHTAlphaMethod(obj, alpha, beta, gamma, FORM)
    CLASS(SDAlgorithm2_), INTENT(INOUT) :: obj
    !! algorithm object
    REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
    !! Default -0.30
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Default is (1-alpha)**2/4
    REAL(DFP), OPTIONAL, INTENT(IN) :: gamma
    !! Default is (1-2alpha)/2
    CHARACTER(*), OPTIONAL, INTENT(IN) :: FORM
    !! "U" (default) or "A"
  END SUBROUTINE obj_HHTAlphaMethod
END INTERFACE

!----------------------------------------------------------------------------
!                                  GeneralizedAlphaMethod@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-11-12
! summary:  Set the parameters of generalized-alpha method
!
! The routine resets the object and stores the parameters and the form
! of the generalized-alpha method (Chung and Hulbert, 1993)
! M A(n+1-alphaM) + C V(n+1-alphaF) + K U(n+1-alphaF) = F(n+1-alphaF),
! where X(n+1-a) = (1-a) X(n+1) + a X(n). It does not compute the
! coefficients; call `Initiate(dt)` afterwards.
!
! By default the parameters are the optimal ones for the spectral radius
! rhoInf at infinite frequency (rhoInf = 1: no dissipation, rhoInf = 0:
! maximal dissipation). A given alphaM or alphaF replaces the optimal
! value, and beta and gamma, if not given, follow the second order
! accuracy condition with the final alphaM and alphaF. The method is
! unconditionally stable for alphaM <= alphaF <= 1/2 with these beta and
! gamma. Special cases are Newmark (alphaM = alphaF = 0), HHT-alpha
! (alphaM = 0, alphaF = -alpha) and WBZ-alpha (alphaF = 0).
!
! Formula:
!   $$ \alpha_m = \frac{2\rho_\infty - 1}{\rho_\infty + 1}, \quad
!      \alpha_f = \frac{\rho_\infty}{\rho_\infty + 1} $$
!   $$ \gamma = \frac{1}{2} - \alpha_m + \alpha_f, \quad
!      \beta = \frac{1}{4} (1 - \alpha_m + \alpha_f)^2 $$
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE SDAlgorithm2_Class, ONLY: SDAlgorithm2_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(SDAlgorithm2_) :: obj
!
! CALL obj%GeneralizedAlphaMethod(rhoInf=0.5_DFP)
! CALL OK(ABS(obj%alphaM) .LT. 1.0E-10_DFP, "alphaM = 0 for rhoInf = 0.5")
! CALL OK(ABS(obj%alphaF - 1.0_DFP / 3.0_DFP) .LT. 1.0E-10_DFP, "alphaF")
! CALL obj%Initiate(dt=0.1_DFP)
! CALL OK(ABS(obj%GetForceTime(0.0_DFP) - 0.2_DFP / 3.0_DFP) &
!         .LT. 1.0E-10_DFP, "force time")
!
! END PROGRAM main
!```
!
! See also: [[obj_HHTAlphaMethod]], [[obj_Initiate]]

INTERFACE
  MODULE SUBROUTINE obj_GeneralizedAlphaMethod(obj, rhoInf, alphaM, &
                                               alphaF, beta, gamma, form)
    CLASS(SDAlgorithm2_), INTENT(INOUT) :: obj
    !! algorithm object
    REAL(DFP), OPTIONAL, INTENT(IN) :: rhoInf
    !! spectral radius at infinite frequency, 0 <= rhoInf <= 1,
    !! default is 0.8
    REAL(DFP), OPTIONAL, INTENT(IN) :: alphaM
    !! Default is (2 rhoInf - 1)/(rhoInf + 1)
    REAL(DFP), OPTIONAL, INTENT(IN) :: alphaF
    !! Default is rhoInf/(rhoInf + 1)
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Default is (1 - alphaM + alphaF)**2/4
    REAL(DFP), OPTIONAL, INTENT(IN) :: gamma
    !! Default is 1/2 - alphaM + alphaF
    CHARACTER(*), OPTIONAL, INTENT(IN) :: form
    !! "U" (default) or "A"
  END SUBROUTINE obj_GeneralizedAlphaMethod
END INTERFACE

!----------------------------------------------------------------------------
!                                        CollocationMethod@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-06
! summary:  Set the parameters of collocation method
!
! The routine resets the object and stores beta, gamma, theta and the
! form. It does not compute the coefficients; call `Initiate(dt)`
! afterwards. The system is solved at t(n) + theta dt with the Newmark
! relations, and the acceleration at t(n+1) is interpolated linearly.
!
! Changes:
!   - 2026-11-12: Only sets parameters; added form. Coefficients are
!     computed by Initiate.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE SDAlgorithm2_Class, ONLY: SDAlgorithm2_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(SDAlgorithm2_) :: obj
!
! CALL obj%CollocationMethod(theta=1.4_DFP)
! CALL obj%Initiate(dt=0.1_DFP)
! CALL OK(ABS(obj%tanmat(3) - 1.0_DFP) .LT. 1.0E-10_DFP, "tanmat(3)")
!
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]]

INTERFACE
  MODULE SUBROUTINE obj_CollocationMethod(obj, beta, gamma, theta, FORM)
    CLASS(SDAlgorithm2_), INTENT(INOUT) :: obj
    !! algorithm object
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Default is 1/6
    REAL(DFP), OPTIONAL, INTENT(IN) :: gamma
    !! Default is 0.50
    REAL(DFP), OPTIONAL, INTENT(IN) :: theta
    !! Default is 1.4 (Wilson theta)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: FORM
    !! "U" (default) or "A"
  END SUBROUTINE obj_CollocationMethod
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-11-12
! summary: Compute the coefficients for a time step size
!
! The routine computes all coefficients (tanmat, rhs_*, dis, vel, acc,
! initialGuess) from the stored parameters, the method name and the
! form. The coefficients contain dt, so the kernel does not multiply
! them by dt. Call it again whenever dt or a parameter changes. It also
! sets forceTime (1 for Newmark, 1-alphaF for HHT and generalized-alpha,
! theta for collocation) and the force weights rhs_f1, rhs_f2 from
! forceEvaluation.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE SDAlgorithm2_Class, ONLY: SDAlgorithm2_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(SDAlgorithm2_) :: obj
!
! CALL obj%NewmarkBetaMethod(form="A")
! CALL OK(.NOT. obj%IsInitiated(), "not initiated")
! CALL obj%Initiate(dt=0.1_DFP)
! CALL OK(obj%IsInitiated(), "initiated")
! CALL OK(ABS(obj%tanmat(3) - 0.0025_DFP) .LT. 1.0E-10_DFP, "tanmat(3)")
!
! END PROGRAM main
!```
!
! See also: [[obj_IsInitiated]]

INTERFACE
  MODULE SUBROUTINE obj_Initiate(obj, dt)
    CLASS(SDAlgorithm2_), INTENT(INOUT) :: obj
    !! algorithm object
    REAL(DFP), INTENT(IN) :: dt
    !! time step size, dt > 0
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                             IsInitiated@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-11-12
! summary: Returns true if the coefficients are computed
!
! Returns:
!   ans : LOGICAL(LGT) - true after Initiate, false after a method
!   routine, SetForm or Deallocate
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE SDAlgorithm2_Class, ONLY: SDAlgorithm2_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(SDAlgorithm2_) :: obj
!
! CALL obj%NewmarkBetaMethod()
! CALL OK(.NOT. obj%IsInitiated(), "not initiated")
! CALL obj%Initiate(dt=0.1_DFP)
! CALL OK(obj%IsInitiated(), "initiated")
!
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]]

INTERFACE
  MODULE FUNCTION obj_IsInitiated(obj) RESULT(ans)
    CLASS(SDAlgorithm2_), INTENT(IN) :: obj
    !! algorithm object
    LOGICAL(LGT) :: ans
    !! true if initiated
  END FUNCTION obj_IsInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                                                 SetForm@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-11-12
! summary: Select the u-form or the a-form
!
! The routine stores the form ("U" or "A", case insensitive) and marks
! the object as not initiated, so `Initiate(dt)` must be called again.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE SDAlgorithm2_Class, ONLY: SDAlgorithm2_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(SDAlgorithm2_) :: obj
!
! CALL obj%NewmarkBetaMethod()
! CALL obj%SetForm("a")
! CALL OK(obj%GetForm() .EQ. "A", "GetForm")
!
! END PROGRAM main
!```
!
! See also: [[obj_GetForm]]

INTERFACE
  MODULE SUBROUTINE obj_SetForm(obj, FORM)
    CLASS(SDAlgorithm2_), INTENT(INOUT) :: obj
    !! algorithm object
    CHARACTER(*), INTENT(IN) :: FORM
    !! "U" or "A"
  END SUBROUTINE obj_SetForm
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetForm@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-11-12
! summary: Get the form
!
! Returns:
!   ans : CHARACTER(1) - "U" if displacement is the unknown, "A" if
!   acceleration is the unknown
!
! Examples
!
!```fortran
! PROGRAM main
! USE SDAlgorithm2_Class, ONLY: SDAlgorithm2_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(SDAlgorithm2_) :: obj
!
! CALL obj%NewmarkBetaMethod()
! CALL OK(obj%GetForm() .EQ. "U", "default form")
!
! END PROGRAM main
!```
!
! See also: [[obj_SetForm]]

INTERFACE
  MODULE FUNCTION obj_GetForm(obj) RESULT(ans)
    CLASS(SDAlgorithm2_), INTENT(IN) :: obj
    !! algorithm object
    CHARACTER(1) :: ans
    !! "U" or "A"
  END FUNCTION obj_GetForm
END INTERFACE

!----------------------------------------------------------------------------
!                                      SetForceEvaluation@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-11-12
! summary: Select how the force of the balance is obtained
!
! The routine stores the option ("interpolate" or "evaluate"; only the
! first letter is used, case insensitive) and marks the object as not
! initiated, so `Initiate(dt)` must be called again. With interpolate,
! the kernel passes the forces at tn and t(n+1), and the weights are
! rhs_f1 = 1-forceTime and rhs_f2 = forceTime. With evaluate, the kernel
! passes the force at `GetForceTime(tn)` as F(n+1), and the weights are
! rhs_f1 = 0 and rhs_f2 = 1.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE SDAlgorithm2_Class, ONLY: SDAlgorithm2_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(SDAlgorithm2_) :: obj
!
! CALL obj%HHTAlphaMethod(alpha=-0.1_DFP)
! CALL obj%Initiate(dt=0.1_DFP)
! CALL OK(ABS(obj%rhs_f1 - 0.1_DFP) .LT. 1.0E-10_DFP, "interpolate f1")
! CALL OK(ABS(obj%rhs_f2 - 0.9_DFP) .LT. 1.0E-10_DFP, "interpolate f2")
!
! CALL obj%SetForceEvaluation("evaluate")
! CALL obj%Initiate(dt=0.1_DFP)
! CALL OK(obj%GetForceEvaluation() .EQ. "E", "GetForceEvaluation")
! CALL OK(ABS(obj%rhs_f2 - 1.0_DFP) .LT. 1.0E-10_DFP, "evaluate f2")
!
! END PROGRAM main
!```
!
! See also: [[obj_GetForceTime]], [[obj_GetForceEvaluation]]

INTERFACE
  MODULE SUBROUTINE obj_SetForceEvaluation(obj, option)
    CLASS(SDAlgorithm2_), INTENT(INOUT) :: obj
    !! algorithm object
    CHARACTER(*), INTENT(IN) :: option
    !! "interpolate" or "evaluate"
  END SUBROUTINE obj_SetForceEvaluation
END INTERFACE

!----------------------------------------------------------------------------
!                                      GetForceEvaluation@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-11-12
! summary: Get how the force of the balance is obtained
!
! Returns:
!   ans : CHARACTER(1) - "I" for interpolate, "E" for evaluate
!
! Examples
!
!```fortran
! PROGRAM main
! USE SDAlgorithm2_Class, ONLY: SDAlgorithm2_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(SDAlgorithm2_) :: obj
!
! CALL obj%NewmarkBetaMethod()
! CALL OK(obj%GetForceEvaluation() .EQ. "I", "default is interpolate")
!
! END PROGRAM main
!```
!
! See also: [[obj_SetForceEvaluation]]

INTERFACE
  MODULE FUNCTION obj_GetForceEvaluation(obj) RESULT(ans)
    CLASS(SDAlgorithm2_), INTENT(IN) :: obj
    !! algorithm object
    CHARACTER(1) :: ans
    !! "I" or "E"
  END FUNCTION obj_GetForceEvaluation
END INTERFACE

!----------------------------------------------------------------------------
!                                            GetForceTime@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-11-12
! summary: Get the time at which the force of the balance is evaluated
!
! The routine returns t + forceTime dt, where t is the time tn at the
! start of the step and dt is the time step size of the last Initiate.
! forceTime is 1 for Newmark, 1-alphaF for HHT and generalized-alpha,
! and theta for collocation.
! A kernel that uses forceEvaluation = "evaluate" evaluates its loads at
! this time. The object must be initiated.
!
! Returns:
!   ans : REAL(DFP) - force time tn + forceTime dt
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE SDAlgorithm2_Class, ONLY: SDAlgorithm2_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(SDAlgorithm2_) :: obj
!
! CALL obj%HHTAlphaMethod(alpha=-0.1_DFP)
! CALL obj%Initiate(dt=0.1_DFP)
! CALL OK(ABS(obj%GetForceTime(1.0_DFP) - 1.09_DFP) .LT. 1.0E-10_DFP, &
!         "force time of HHT")
!
! END PROGRAM main
!```
!
! See also: [[obj_SetForceEvaluation]]

INTERFACE
  MODULE FUNCTION obj_GetForceTime(obj, t) RESULT(ans)
    CLASS(SDAlgorithm2_), INTENT(IN) :: obj
    !! algorithm object, initiated
    REAL(DFP), INTENT(IN) :: t
    !! time tn at the start of the step
    REAL(DFP) :: ans
    !! force time tn + forceTime dt
  END FUNCTION obj_GetForceTime
END INTERFACE

!----------------------------------------------------------------------------
!                                            HouboltMethod@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-06
! summary: Initiate Houbolt method

INTERFACE
  MODULE SUBROUTINE obj_HouboltMethod(obj)
    CLASS(SDAlgorithm2_), INTENT(INOUT) :: obj
    CHARACTER(*), PARAMETER :: myName = "Houbolt"
  END SUBROUTINE obj_HouboltMethod
END INTERFACE

!----------------------------------------------------------------------------
!                                                MakeZeros@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-06
! summary: Reset the SDAlgorithm2_ object to zero values

INTERFACE
  MODULE SUBROUTINE obj_MakeZeros(obj)
    CLASS(SDAlgorithm2_), INTENT(INOUT) :: obj
    ! internal varibales
    REAL(DFP), PARAMETER :: myzero = math%zero
  END SUBROUTINE obj_MakeZeros
END INTERFACE

!----------------------------------------------------------------------------
!                                                  Display@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-06
! summary: Display the content

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitno)
    CLASS(SDAlgorithm2_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                               Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-06
! summary:  Deallocate the object

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(SDAlgorithm2_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                  ImportFromToml@TomlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-06
! summary: Import data from toml table
!
! The table gives `methodName` (default NEWM; only the first four letters
! are used, case insensitive), an optional `form` ("U" default or "A"),
! an optional `forceEvaluation` ("interpolate" default or "evaluate"),
! an optional `dt` and a sub table named after `methodName` with the
! parameters of the method:
!
! - Newmark ("Newmark" or "Trapezoidal"): beta, gamma
! - HHT-alpha ("HHTAlpha"): alpha, beta, gamma
! - generalized-alpha ("GeneralizedAlpha"): rhoInf, alphaM, alphaF,
!   beta, gamma
! - collocation ("Collocation"): beta, gamma, theta
!
! A missing parameter takes the default of the method routine. If `dt`
! is given, `Initiate(dt)` is called; otherwise the kernel must call it.
!
! ```toml
! [timeIntegration]
! methodName = "GeneralizedAlpha"
! form = "U"
! forceEvaluation = "interpolate"
! dt = 0.01
! [timeIntegration.GeneralizedAlpha]
! rhoInf = 0.8
! ```
!
! Changes:
!   - 2026-11-12: Added form, forceEvaluation and dt keys. Removed user
!     defined coefficient tables; an unknown methodName is an error.
!   - 2026-11-12: Added generalized-alpha method.

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table)
    CLASS(SDAlgorithm2_), INTENT(INOUT) :: obj
    !! algorithm object
    TYPE(toml_table), INTENT(INOUT) :: table
    !! toml table of the algorithm
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

!----------------------------------------------------------------------------
!                                                  ImportFromToml@TomlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate kernel from the toml file

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml2( &
    obj, tomlName, afile, filename, printToml)
    CLASS(SDAlgorithm2_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: tomlName
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
    ! internal variables
    CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml2()"
    TYPE(toml_table), ALLOCATABLE :: table
    TYPE(toml_table), POINTER :: node
    INTEGER(I4B) :: origin, STAT
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE SDAlgorithm2_Class
