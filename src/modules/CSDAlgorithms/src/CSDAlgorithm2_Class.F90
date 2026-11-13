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

!> author: Shion Shimizu
! date: 2026-07-03
! summary: Composite semi discrete algorithms for second order systems
!
! Changes:
! - 2026-11-13: Independent of SDAlgorithm2_. Coefficients of every
!   substep are stored in arrays indexed by the substep and the history.

MODULE CSDAlgorithm2_Class
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE tomlf, ONLY: toml_table
USE TxtFile_Class, ONLY: TxtFile_

IMPLICIT NONE

PRIVATE

PUBLIC :: CSDAlgorithm2_

CHARACTER(*), PARAMETER :: modName = "CSDAlgorithm2_Class()"

!----------------------------------------------------------------------------
!                                                             CSDAlgorithm2_
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-07-03
! summary: Composite semi discrete algorithm
!
! A composite method advances M A + C V + K U = F from tn to t(n+1) in
! K substeps (K = totalSubstep). Substep k solves one linear system for
! its unknown sol_k at the time tn + substepTime(k) dt. The history of
! substep k is
!
! x = [Un, Vn, An, sol_1, ..., sol_K]   (j = 1, 2, 3, 3+1, ..., 3+K)
!
! where only the entries j < 3+k are used by substep k. The kernel
! assembles, for substep k,
!
! T   = tanmat(1,k) M + tanmat(2,k) C + tanmat(3,k) K
! RHS = sum_j (rhs(1,j,k) M + rhs(2,j,k) C + rhs(3,j,k) K) x_j
!     + rhs_f1(k) F(tn) + rhs_f2(k) F2
!
! and after the last substep the state at t(n+1) is
!
! U = sum_j dis(j) x_j,  V = sum_j vel(j) x_j,  A = sum_j acc(j) x_j
!
! The initial guess of sol_k is sum_j initialGuess(j,k) x_j. Dirichlet
! values of substep k are given at `GetSubstepTime(tn, k)`.
!
! A method routine (`NewmarkBetaMethod`, `BatheMethod`) or
! `ImportFromToml` sets only the parameters. `Initiate(dt)` allocates
! and computes the coefficients, which contain dt; call it again
! whenever dt changes.
!
! In the u-form (default, form = "U") sol_k is a displacement, in the
! a-form (form = "A") it is an acceleration. The coefficient of the
! force is 1 in both forms, so RHS has the units of force.
!
! Substep k writes the balance at the force time tn + forceTime(k) dt.
! With forceEvaluation = "I" (interpolate, default) F2 = F(t(n+1)) and
! the weights are rhs_f1 = 1 - forceTime, rhs_f2 = forceTime. With "E"
! (evaluate) F2 is the force at `GetForceTime(tn, k)`, rhs_f1 = 0 and
! rhs_f2 = 1.
!
! Available methods:
!
! - Newmark-beta (K = 1, name NEWM)
! - Bathe (K = 2, name BATH): trapezoidal rule over splittingRatio dt,
!   then the three point backward Euler rule over dt with parameters
!   b0, b1, b2 (gamma-Bathe, rho-infinity-Bathe, beta1/beta2-Bathe)

TYPE :: CSDAlgorithm2_
  CHARACTER(4) :: name = "BATH"
  !! name of the method: NEWM, BATH
  CHARACTER(1) :: form = "U"
  !! U: displacement is the unknown, A: acceleration is the unknown
  CHARACTER(1) :: forceEvaluation = "I"
  !! I: interpolate the force, E: evaluate the force at the force time
  LOGICAL(LGT) :: isInit = math%no
  !! true after Initiate, false after a parameter changes
  INTEGER(I4B) :: totalSubstep = math%zero_i
  !! number of substeps K, set by Initiate
  INTEGER(I4B) :: currentSubstep = math%one_i
  !! current substep, 1 <= currentSubstep <= totalSubstep
  REAL(DFP) :: dt = math%zero
  !! time step size used in the last Initiate

  REAL(DFP) :: beta = 0.25_DFP
  !! Newmark beta parameter
  REAL(DFP) :: gamma = math%half
  !! Newmark gamma parameter
  REAL(DFP) :: splittingRatio = math%half
  !! Bathe: the first substep ends at tn + splittingRatio dt
  REAL(DFP) :: rhoInf = math%zero
  !! Bathe: spectral radius at infinite frequency (rho-infinity-Bathe)
  REAL(DFP) :: beta1 = math%zero
  !! Bathe: parameter of beta1/beta2-Bathe
  REAL(DFP) :: beta2 = math%zero
  !! Bathe: parameter of beta1/beta2-Bathe
  REAL(DFP) :: b0 = math%zero
  !! Bathe: weight of tn in the second substep
  REAL(DFP) :: b1 = math%zero
  !! Bathe: weight of tn + splittingRatio dt in the second substep
  REAL(DFP) :: b2 = math%zero
  !! Bathe: weight of t(n+1) in the second substep

  REAL(DFP), ALLOCATABLE :: tanmat(:, :)
  !! coefficients of M, C, K in the tangent matrix, size (3, K)
  LOGICAL(LGT), ALLOCATABLE :: tanmat_zero(:, :)
  !! true where tanmat is zero
  REAL(DFP), ALLOCATABLE :: rhs(:, :, :)
  !! rhs(i, j, k): coefficient of M (i=1), C (i=2), K (i=3) times the
  !! history x_j in the RHS of substep k, size (3, 3+K, K)
  LOGICAL(LGT), ALLOCATABLE :: rhs_zero(:, :, :)
  !! true where rhs is zero
  REAL(DFP), ALLOCATABLE :: rhs_f1(:)
  !! coefficient of F(tn) in the RHS of substep k, size K
  LOGICAL(LGT), ALLOCATABLE :: rhs_f1_zero(:)
  !! true where rhs_f1 is zero
  REAL(DFP), ALLOCATABLE :: rhs_f2(:)
  !! coefficient of F2 in the RHS of substep k, size K
  LOGICAL(LGT), ALLOCATABLE :: rhs_f2_zero(:)
  !! true where rhs_f2 is zero
  REAL(DFP), ALLOCATABLE :: forceTime(:)
  !! force time of substep k is tn + forceTime(k) dt, size K
  REAL(DFP), ALLOCATABLE :: substepTime(:)
  !! substep k ends at tn + substepTime(k) dt, size K
  REAL(DFP), ALLOCATABLE :: dis(:)
  !! coefficients of the history in U(n+1), size 3+K
  LOGICAL(LGT), ALLOCATABLE :: dis_zero(:)
  !! true where dis is zero
  REAL(DFP), ALLOCATABLE :: vel(:)
  !! coefficients of the history in V(n+1), size 3+K
  LOGICAL(LGT), ALLOCATABLE :: vel_zero(:)
  !! true where vel is zero
  REAL(DFP), ALLOCATABLE :: acc(:)
  !! coefficients of the history in A(n+1), size 3+K
  LOGICAL(LGT), ALLOCATABLE :: acc_zero(:)
  !! true where acc is zero
  REAL(DFP), ALLOCATABLE :: initialGuess(:, :)
  !! coefficients of the history in the initial guess of sol_k,
  !! size (3+K, K)
  LOGICAL(LGT), ALLOCATABLE :: initialGuess_zero(:, :)
  !! true where initialGuess is zero

CONTAINS

  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Reset parameters and deallocate coefficients
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the content
  PROCEDURE, PUBLIC, PASS(obj) :: MakeZeros => obj_MakeZeros
  !! Set the zero flags from the coefficients
  PROCEDURE, PUBLIC, PASS(obj) :: NewmarkBetaMethod => obj_NewmarkBetaMethod
  !! Set the parameters of Newmark-beta method (one substep)
  PROCEDURE, PUBLIC, PASS(obj) :: BatheMethod => obj_BatheMethod
  !! Set the parameters of Bathe method (two substeps)
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
  !! Get the time at which the force of a substep is evaluated
  PROCEDURE, PUBLIC, PASS(obj) :: GetSubstepTime => obj_GetSubstepTime
  !! Get the end time of a substep
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalSubstep => obj_GetTotalSubstep
  !! Get the number of substeps
  PROCEDURE, PUBLIC, PASS(obj) :: GetCurrentSubstep => obj_GetCurrentSubstep
  !! Get the current substep
  PROCEDURE, PUBLIC, PASS(obj) :: UpdateCurrentSubstep => &
    obj_UpdateCurrentSubstep
  !! Go to the next substep, after the last one go back to the first
  PROCEDURE, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  !! Import parameters from a toml table
  PROCEDURE, PASS(obj) :: ImportFromToml2 => obj_ImportFromToml2
  !! Import parameters from a toml file
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, ImportFromToml2
  !! Import parameters from toml
END TYPE CSDAlgorithm2_

!----------------------------------------------------------------------------
!                                                    NewmarkBetaMethod@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-11-13
! summary: Set the parameters of Newmark-beta method
!
! The routine resets the object and stores beta, gamma and the form of
! Newmark-beta method, which is the composite method with one substep.
! It does not compute the coefficients; call `Initiate(dt)` afterwards.
! The coefficients are the same as those of `SDAlgorithm2_`.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE CSDAlgorithm2_Class, ONLY: CSDAlgorithm2_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(CSDAlgorithm2_) :: obj
!
! CALL obj%NewmarkBetaMethod(beta=0.25_DFP, gamma=0.5_DFP)
! CALL obj%Initiate(dt=0.1_DFP)
! CALL OK(obj%GetTotalSubstep() .EQ. 1, "one substep")
! CALL OK(ABS(obj%tanmat(1, 1) - 400.0_DFP) .LT. 1.0E-10_DFP, "tanmat")
!
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]]

INTERFACE
  MODULE SUBROUTINE obj_NewmarkBetaMethod(obj, beta, gamma, form)
    CLASS(CSDAlgorithm2_), INTENT(INOUT) :: obj
    !! algorithm object
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta
    !! Default is 0.25
    REAL(DFP), OPTIONAL, INTENT(IN) :: gamma
    !! Default is 0.5
    CHARACTER(*), OPTIONAL, INTENT(IN) :: form
    !! "U" (default) or "A"
  END SUBROUTINE obj_NewmarkBetaMethod
END INTERFACE

!----------------------------------------------------------------------------
!                                                          BatheMethod@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-07-03
! summary: Set the parameters of Bathe method
!
! The routine resets the object and stores the parameters and the form
! of Bathe method. It does not compute the coefficients; call
! `Initiate(dt)` afterwards.
!
! The first substep is the trapezoidal rule from tn to
! tn + splittingRatio dt. The second substep is the three point
! backward Euler rule from tn to t(n+1),
! V(n+1) = Vn + dt (b0 An + b1 A(n+s) + b2 A(n+1)) and
! U(n+1) = Un + dt (b0 Vn + b1 V(n+s) + b2 V(n+1)), where
! s = splittingRatio. The weights depend on the variant:
!
! - rhoInf present: rho-infinity-Bathe,
!   b1 = (rhoInf + 1)/(2 s (rhoInf - 1) + 4), b0 = 1/2 + (s - 1) b1,
!   b2 = 1/2 - s b1
! - beta1 or beta2 present: beta1/beta2-Bathe (default beta1 = 1/3,
!   beta2 = 1 - beta1), b0 = (1 - beta1) s,
!   b1 = (1 - beta2)(1 - s) + beta1 s, b2 = (1 - s) beta2
! - otherwise: gamma-Bathe, b0 = b1 = 1/(2 (2 - s)),
!   b2 = (1 - s)/(2 - s)
!
! In every variant b0 + b1 + b2 = 1. b2 must not be zero.
!
! Changes:
!   - 2026-11-13: Only sets parameters; added form. The beta1/beta2
!     variant is used when beta1 or beta2 is present (before, rhoInf
!     was used). gamma is renamed splittingRatio.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE CSDAlgorithm2_Class, ONLY: CSDAlgorithm2_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(CSDAlgorithm2_) :: obj
!
! CALL obj%BatheMethod(splittingRatio=0.5_DFP)
! CALL OK(ABS(obj%b2 - 1.0_DFP / 3.0_DFP) .LT. 1.0E-10_DFP, "b2")
! CALL obj%Initiate(dt=0.1_DFP)
! CALL OK(obj%GetTotalSubstep() .EQ. 2, "two substeps")
! CALL OK(ABS(obj%GetSubstepTime(1.0_DFP, 1) - 1.05_DFP) &
!         .LT. 1.0E-10_DFP, "end of the first substep")
!
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]]

INTERFACE
  MODULE SUBROUTINE obj_BatheMethod(obj, splittingRatio, rhoInf, beta1, &
                                    beta2, form)
    CLASS(CSDAlgorithm2_), INTENT(INOUT) :: obj
    !! algorithm object
    REAL(DFP), OPTIONAL, INTENT(IN) :: splittingRatio
    !! end of the first substep / dt, 0 < splittingRatio < 2,
    !! default is 0.5
    REAL(DFP), OPTIONAL, INTENT(IN) :: rhoInf
    !! spectral radius at infinite frequency, 0 <= rhoInf <= 1
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta1
    !! parameter of beta1/beta2-Bathe, default 1/3
    REAL(DFP), OPTIONAL, INTENT(IN) :: beta2
    !! parameter of beta1/beta2-Bathe, default 1 - beta1
    CHARACTER(*), OPTIONAL, INTENT(IN) :: form
    !! "U" (default) or "A"
  END SUBROUTINE obj_BatheMethod
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Initiate@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-11-13
! summary: Compute the coefficients for a time step size
!
! The routine allocates and computes all coefficients (tanmat, rhs,
! rhs_f1, rhs_f2, forceTime, substepTime, dis, vel, acc, initialGuess)
! from the stored parameters, the method name, the form and
! forceEvaluation. The coefficients contain dt. currentSubstep is set to
! 1. Call it again whenever dt or a parameter changes.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE CSDAlgorithm2_Class, ONLY: CSDAlgorithm2_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(CSDAlgorithm2_) :: obj
!
! CALL obj%BatheMethod(form="A")
! CALL OK(.NOT. obj%IsInitiated(), "not initiated")
! CALL obj%Initiate(dt=0.1_DFP)
! CALL OK(obj%IsInitiated(), "initiated")
! CALL OK(ABS(obj%tanmat(2, 1) - 0.025_DFP) .LT. 1.0E-10_DFP, &
!         "a-form tanmat of the first substep")
!
! END PROGRAM main
!```
!
! See also: [[obj_IsInitiated]]

INTERFACE
  MODULE SUBROUTINE obj_Initiate(obj, dt)
    CLASS(CSDAlgorithm2_), INTENT(INOUT) :: obj
    !! algorithm object
    REAL(DFP), INTENT(IN) :: dt
    !! time step size, dt > 0
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                          IsInitiated@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-11-13
! summary: Returns true if the coefficients are computed
!
! Returns:
!   ans : LOGICAL(LGT) - true after Initiate, false after a method
!   routine, SetForm, SetForceEvaluation or Deallocate
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE CSDAlgorithm2_Class, ONLY: CSDAlgorithm2_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(CSDAlgorithm2_) :: obj
!
! CALL obj%BatheMethod()
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
    CLASS(CSDAlgorithm2_), INTENT(IN) :: obj
    !! algorithm object
    LOGICAL(LGT) :: ans
    !! true if initiated
  END FUNCTION obj_IsInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                                                              SetForm@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-11-13
! summary: Select the u-form or the a-form
!
! The routine stores the form ("U" or "A", case insensitive) and marks
! the object as not initiated, so `Initiate(dt)` must be called again.
!
! Examples
!
!```fortran
! PROGRAM main
! USE CSDAlgorithm2_Class, ONLY: CSDAlgorithm2_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(CSDAlgorithm2_) :: obj
!
! CALL obj%BatheMethod()
! CALL obj%SetForm("a")
! CALL OK(obj%GetForm() .EQ. "A", "GetForm")
!
! END PROGRAM main
!```
!
! See also: [[obj_GetForm]]

INTERFACE
  MODULE SUBROUTINE obj_SetForm(obj, form)
    CLASS(CSDAlgorithm2_), INTENT(INOUT) :: obj
    !! algorithm object
    CHARACTER(*), INTENT(IN) :: form
    !! "U" or "A"
  END SUBROUTINE obj_SetForm
END INTERFACE

!----------------------------------------------------------------------------
!                                                              GetForm@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-11-13
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
! USE CSDAlgorithm2_Class, ONLY: CSDAlgorithm2_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(CSDAlgorithm2_) :: obj
!
! CALL obj%BatheMethod()
! CALL OK(obj%GetForm() .EQ. "U", "default form")
!
! END PROGRAM main
!```
!
! See also: [[obj_SetForm]]

INTERFACE
  MODULE FUNCTION obj_GetForm(obj) RESULT(ans)
    CLASS(CSDAlgorithm2_), INTENT(IN) :: obj
    !! algorithm object
    CHARACTER(1) :: ans
    !! "U" or "A"
  END FUNCTION obj_GetForm
END INTERFACE

!----------------------------------------------------------------------------
!                                                   SetForceEvaluation@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-11-13
! summary: Select how the force of a substep is obtained
!
! The routine stores the option ("interpolate" or "evaluate"; only the
! first letter is used, case insensitive) and marks the object as not
! initiated, so `Initiate(dt)` must be called again. With interpolate
! the kernel passes F(tn) and F(t(n+1)) for every substep. With
! evaluate the kernel passes the force at `GetForceTime(tn, k)` as F2.
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE CSDAlgorithm2_Class, ONLY: CSDAlgorithm2_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(CSDAlgorithm2_) :: obj
!
! CALL obj%BatheMethod(splittingRatio=0.5_DFP)
! CALL obj%Initiate(dt=0.1_DFP)
! CALL OK(ABS(obj%rhs_f2(1) - 0.5_DFP) .LT. 1.0E-10_DFP, "interpolate")
! CALL obj%SetForceEvaluation("evaluate")
! CALL obj%Initiate(dt=0.1_DFP)
! CALL OK(ABS(obj%rhs_f2(1) - 1.0_DFP) .LT. 1.0E-10_DFP, "evaluate")
!
! END PROGRAM main
!```
!
! See also: [[obj_GetForceTime]], [[obj_GetForceEvaluation]]

INTERFACE
  MODULE SUBROUTINE obj_SetForceEvaluation(obj, option)
    CLASS(CSDAlgorithm2_), INTENT(INOUT) :: obj
    !! algorithm object
    CHARACTER(*), INTENT(IN) :: option
    !! "interpolate" or "evaluate"
  END SUBROUTINE obj_SetForceEvaluation
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetForceEvaluation@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-11-13
! summary: Get how the force of a substep is obtained
!
! Returns:
!   ans : CHARACTER(1) - "I" for interpolate, "E" for evaluate
!
! Examples
!
!```fortran
! PROGRAM main
! USE CSDAlgorithm2_Class, ONLY: CSDAlgorithm2_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(CSDAlgorithm2_) :: obj
!
! CALL obj%BatheMethod()
! CALL OK(obj%GetForceEvaluation() .EQ. "I", "default is interpolate")
!
! END PROGRAM main
!```
!
! See also: [[obj_SetForceEvaluation]]

INTERFACE
  MODULE FUNCTION obj_GetForceEvaluation(obj) RESULT(ans)
    CLASS(CSDAlgorithm2_), INTENT(IN) :: obj
    !! algorithm object
    CHARACTER(1) :: ans
    !! "I" or "E"
  END FUNCTION obj_GetForceEvaluation
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetForceTime@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-11-13
! summary: Get the time at which the force of a substep is evaluated
!
! The routine returns t + forceTime(substep) dt, where t is tn and dt
! is the time step size of the last Initiate. A kernel that uses
! forceEvaluation = "evaluate" evaluates its loads at this time. The
! object must be initiated.
!
! Returns:
!   ans : REAL(DFP) - force time of the substep
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE CSDAlgorithm2_Class, ONLY: CSDAlgorithm2_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(CSDAlgorithm2_) :: obj
!
! CALL obj%BatheMethod(splittingRatio=0.6_DFP)
! CALL obj%Initiate(dt=0.1_DFP)
! CALL OK(ABS(obj%GetForceTime(1.0_DFP, 1) - 1.06_DFP) &
!         .LT. 1.0E-10_DFP, "force time of the first substep")
!
! END PROGRAM main
!```
!
! See also: [[obj_GetSubstepTime]]

INTERFACE
  MODULE FUNCTION obj_GetForceTime(obj, t, substep) RESULT(ans)
    CLASS(CSDAlgorithm2_), INTENT(IN) :: obj
    !! algorithm object, initiated
    REAL(DFP), INTENT(IN) :: t
    !! time tn at the start of the step
    INTEGER(I4B), INTENT(IN) :: substep
    !! substep number, 1 <= substep <= totalSubstep
    REAL(DFP) :: ans
    !! force time of the substep
  END FUNCTION obj_GetForceTime
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetSubstepTime@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-11-13
! summary: Get the end time of a substep
!
! The routine returns t + substepTime(substep) dt, where t is tn. The
! unknown of the substep and its Dirichlet values belong to this time.
! The object must be initiated.
!
! Returns:
!   ans : REAL(DFP) - end time of the substep
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE CSDAlgorithm2_Class, ONLY: CSDAlgorithm2_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(CSDAlgorithm2_) :: obj
!
! CALL obj%BatheMethod(splittingRatio=0.5_DFP)
! CALL obj%Initiate(dt=0.1_DFP)
! CALL OK(ABS(obj%GetSubstepTime(0.0_DFP, 2) - 0.1_DFP) &
!         .LT. 1.0E-10_DFP, "end of the second substep")
!
! END PROGRAM main
!```
!
! See also: [[obj_GetForceTime]]

INTERFACE
  MODULE FUNCTION obj_GetSubstepTime(obj, t, substep) RESULT(ans)
    CLASS(CSDAlgorithm2_), INTENT(IN) :: obj
    !! algorithm object, initiated
    REAL(DFP), INTENT(IN) :: t
    !! time tn at the start of the step
    INTEGER(I4B), INTENT(IN) :: substep
    !! substep number, 1 <= substep <= totalSubstep
    REAL(DFP) :: ans
    !! end time of the substep
  END FUNCTION obj_GetSubstepTime
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetTotalSubstep@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-07-03
! summary: Get the number of substeps
!
! Returns:
!   ans : INTEGER(I4B) - number of substeps, set by Initiate
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE CSDAlgorithm2_Class, ONLY: CSDAlgorithm2_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(CSDAlgorithm2_) :: obj
!
! CALL obj%BatheMethod()
! CALL obj%Initiate(dt=0.1_DFP)
! CALL OK(obj%GetTotalSubstep() .EQ. 2, "Bathe has two substeps")
!
! END PROGRAM main
!```

INTERFACE
  MODULE FUNCTION obj_GetTotalSubstep(obj) RESULT(ans)
    CLASS(CSDAlgorithm2_), INTENT(IN) :: obj
    !! algorithm object
    INTEGER(I4B) :: ans
    !! number of substeps
  END FUNCTION obj_GetTotalSubstep
END INTERFACE

!----------------------------------------------------------------------------
!                                                    GetCurrentSubstep@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-07-03
! summary: Get the current substep
!
! Returns:
!   ans : INTEGER(I4B) - current substep
!
! Examples
!
!```fortran
! PROGRAM main
! USE GlobalData, ONLY: DFP
! USE CSDAlgorithm2_Class, ONLY: CSDAlgorithm2_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! TYPE(CSDAlgorithm2_) :: obj
!
! CALL obj%BatheMethod()
! CALL obj%Initiate(dt=0.1_DFP)
! CALL OK(obj%GetCurrentSubstep() .EQ. 1, "first substep")
! CALL obj%UpdateCurrentSubstep()
! CALL OK(obj%GetCurrentSubstep() .EQ. 2, "second substep")
! CALL obj%UpdateCurrentSubstep()
! CALL OK(obj%GetCurrentSubstep() .EQ. 1, "back to the first substep")
!
! END PROGRAM main
!```
!
! See also: [[obj_UpdateCurrentSubstep]]

INTERFACE
  MODULE FUNCTION obj_GetCurrentSubstep(obj) RESULT(ans)
    CLASS(CSDAlgorithm2_), INTENT(IN) :: obj
    !! algorithm object
    INTEGER(I4B) :: ans
    !! current substep
  END FUNCTION obj_GetCurrentSubstep
END INTERFACE

!----------------------------------------------------------------------------
!                                                 UpdateCurrentSubstep@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-07-03
! summary: Go to the next substep
!
! After the last substep the current substep goes back to 1.
!
! Examples
!
! See the example of [[obj_GetCurrentSubstep]].

INTERFACE
  MODULE SUBROUTINE obj_UpdateCurrentSubstep(obj)
    CLASS(CSDAlgorithm2_), INTENT(INOUT) :: obj
    !! algorithm object
  END SUBROUTINE obj_UpdateCurrentSubstep
END INTERFACE

!----------------------------------------------------------------------------
!                                                            MakeZeros@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-07-03
! summary: Set the zero flags from the coefficients
!
! The routine is called by Initiate.

INTERFACE
  MODULE SUBROUTINE obj_MakeZeros(obj)
    CLASS(CSDAlgorithm2_), INTENT(INOUT) :: obj
    !! algorithm object
  END SUBROUTINE obj_MakeZeros
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Deallocate@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-07-03
! summary: Reset parameters and deallocate coefficients

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(CSDAlgorithm2_), INTENT(INOUT) :: obj
    !! algorithm object
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Display@Methods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-07-03
! summary: Display the content

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitno)
    CLASS(CSDAlgorithm2_), INTENT(INOUT) :: obj
    !! algorithm object
    CHARACTER(*), INTENT(IN) :: msg
    !! message
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
    !! unit number, default is stdout
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                ImportFromToml@TomlMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-07-03
! summary: Import data from toml table
!
! The table gives `methodName` (default "Bathe"; only the first four
! letters are used, case insensitive), an optional `form` ("U" default
! or "A"), an optional `forceEvaluation` ("interpolate" default or
! "evaluate"), an optional `dt` and a sub table named after
! `methodName` with the parameters of the method:
!
! - Bathe ("Bathe", also "GammaBathe", "GBathe", "BetaBathe", "BBathe",
!   "RhoBathe", "RBathe"): splittingRatio (or gamma), rhoInf, beta1,
!   beta2. "Rho..." uses rhoInf = 0 and "Beta..." uses beta1 = 1/3 if
!   they are not given.
! - Newmark ("Newmark" or "Trapezoidal"): beta, gamma
!
! A missing parameter takes the default of the method routine. If `dt`
! is given, `Initiate(dt)` is called; otherwise the kernel must call it.
!
! ```toml
! [timeIntegration]
! methodName = "Bathe"
! form = "U"
! forceEvaluation = "interpolate"
! [timeIntegration.Bathe]
! splittingRatio = 0.5
! rhoInf = 0.0
! ```
!
! Changes:
!   - 2026-11-13: Added form, forceEvaluation and dt keys, the Newmark
!     method and the splittingRatio key. Removed the single step methods
!     through SDAlgorithm2_.

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table)
    CLASS(CSDAlgorithm2_), INTENT(INOUT) :: obj
    !! algorithm object
    TYPE(toml_table), INTENT(INOUT) :: table
    !! toml table of the algorithm
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

!----------------------------------------------------------------------------
!                                                ImportFromToml@TomlMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-07-03
! summary: Import data from a toml file
!
! The table tomlName of the file is passed to ImportFromToml1.

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml2(obj, tomlName, afile, filename, &
                                        printToml)
    CLASS(CSDAlgorithm2_), INTENT(INOUT) :: obj
    !! algorithm object
    CHARACTER(*), INTENT(IN) :: tomlName
    !! name of the table
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    !! toml file
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    !! name of the toml file
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
    !! print the table in debug mode
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE

END MODULE CSDAlgorithm2_Class
