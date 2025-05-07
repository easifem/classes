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

MODULE SDAlgorithms
USE BaseMethod
USE BaseType
USE TxtFile_Class
USE ExceptionHandler_Class, ONLY: e
USE TomlUtility, ONLY: GetValue, GetValue_
USE tomlf, ONLY: toml_table, &
  & toml_serialize,  &
  & toml_get => get_value, &
  & toml_len => len, &
  & toml_array,  &
  & toml_stat
IMPLICIT NONE

PRIVATE
PUBLIC :: SDAlgoParam_
CHARACTER(*), PARAMETER :: modName = "SDAlgorithms()"

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-24
! summary:  Displacement based single step semi discrete algorithm

TYPE :: SDAlgoParam_
  CHARACTER(4) :: name = "NEWM"
  REAL(DFP) :: tanmat(3) = 0.0_DFP
  !! tanmat = tanmat(1) * M + tanmat(2) * C *dt + tanmat(3) * K * dt * dt
  !! tanmat(1) : coefficient of mass matrix (M)
  !! tanmat(2) : coefficient of damping matrix (C*dt)
  !! tanmat(3) : coefficient of stiffness matrix (K*dt*dt)
  LOGICAL(LGT) :: tanmat_zero(3) = .TRUE.

  REAL(DFP) :: rhs_u1(3) = 0.0_DFP
  !! coefficient for rhs for displacement at time tn
  !! rhs = rhs + rhs_u1(1) * M + rhs_u1(2) * C * dt + rhs_u1(3) * K * dt * dt
  LOGICAL(LGT) :: rhs_u1_zero(3) = .TRUE.

  REAL(DFP) :: rhs_v1(3) = 0.0_DFP
  !! coefficient for rhs for (velocity*dt) at time tn
  !! rhs = rhs + rhs_v1(1) *dt * M + rhs_v1(2) * dt * C * dt  &
  !! & + rhs_v1(3) * dt * K * dt * dt
  LOGICAL(LGT) :: rhs_v1_zero(3) = .TRUE.

  REAL(DFP) :: rhs_a1(3) = 0.0_DFP
  !! coefficient for rhs for (acceleration*dt*dt) at time tn
  !! rhs = rhs + rhs_v1(1) *dt * M + rhs_v1(2) * dt * C * dt  &
  !! & + rhs_v1(3) * dt * K * dt * dt
  LOGICAL(LGT) :: rhs_a1_zero(3) = .TRUE.

  REAL(DFP) :: rhs_f1 = 0.0_DFP
  !! coefficient for rhs for (force*dt*dt) at time tn
  !! rhs = rhs + rhs_f1 *dt *dt * force1
  LOGICAL(LGT) :: rhs_f1_zero = .TRUE.

  REAL(DFP) :: rhs_f2 = 0.0_DFP
  !! coefficient for rhs for (force*dt*dt) at time tn+1
  !! rhs = rhs + rhs_f2 *dt *dt * force2
  LOGICAL(LGT) :: rhs_f2_zero = .TRUE.

  REAL(DFP) :: dis(4) = 0.0_DFP
  !! dis coefficient for velocity update
  !! displacement = dis(1)*u+dis(2)*v*dt+dis(3)*a*dt*dt+dis(4)*sol
  !! dis(1) coefficient of displacement
  !! dis(2) coefficient of velocity
  !! dis(3) coefficient of acceleration
  !! dis(4) coefficient of solution
  LOGICAL(LGT) :: dis_zero(4) = .TRUE.

  REAL(DFP) :: vel(4) = 0.0_DFP
  !! vel coefficient for velocity update
  !! velocity = vel(1)*u / dt + vel(2) * v + vel(3) * a *dt + vel(4)*sol/dt
  !! vel(1) coefficient of displacement
  !! vel(2) coefficient of velocity
  !! vel(3) coefficient of acceleration
  !! vel(4) coefficient of solution
  LOGICAL(LGT) :: vel_zero(4) = .TRUE.

  REAL(DFP) :: acc(4) = 0.0_DFP
  !! acc coefficient for accocity update
  !! acc = acc(1)*u / dt/dt + acc(2) * v /dt + acc(3) * a + acc(4)*sol/dt/dt
  !! acc(1) coefficient of displacement
  !! acc(2) coefficient of accocity
  !! acc(3) coefficient of acceleration
  !! acc(4) coefficient of solution
  LOGICAL(LGT) :: acc_zero(4) = .TRUE.

  REAL(DFP) :: alpha = 0.0_DFP
  !! only in case of HHT-alpha method it can be less than 0.0

CONTAINS
  PRIVATE

  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  PROCEDURE, PUBLIC, PASS(obj) :: MakeZeros => obj_MakeZeros
  PROCEDURE, PUBLIC, PASS(obj) :: NewmarkBeta => obj_NewmarkBeta
  PROCEDURE, PUBLIC, PASS(obj) :: HHTAlpha => obj_HHTAlpha
  PROCEDURE, PUBLIC, PASS(obj) :: Collocation => obj_Collocation
  ! PROCEDURE, PUBLIC, PASS(obj) :: Houbolt => obj_Houbolt
  PROCEDURE, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  PROCEDURE, PASS(obj) :: ImportFromToml2 => obj_ImportFromToml2
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, ImportFromToml2
END TYPE SDAlgoParam_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!                                                               NewmarkBeta
!----------------------------------------------------------------------------

SUBROUTINE obj_NewmarkBeta(obj, beta, gamma)
  CLASS(SDAlgoParam_), INTENT(INOUT) :: obj
  REAL(DFP), OPTIONAL, INTENT(IN) :: beta
  !! Default is 0.25
  REAL(DFP), OPTIONAL, INTENT(IN) :: gamma
  !! Default is 0.5

  ! internal varibales
  REAL(DFP) :: beta_inv, gamma_inv, beta0, gamma0

  CALL obj%DEALLOCATE()

  beta0 = Input(default=0.25_DFP, option=beta)
  gamma0 = Input(default=0.5_DFP, option=gamma)

  beta_inv = 1.0_DFP / beta0
  gamma_inv = 1.0_DFP / gamma0

  obj%tanmat(1) = 1.0_DFP
  obj%tanmat(2) = gamma0
  obj%tanmat(3) = beta0

  obj%rhs_f2 = beta0

  obj%rhs_u1(1) = 1.0_DFP
  obj%rhs_u1(2) = gamma0

  obj%rhs_v1(1) = 1.0_DFP
  obj%rhs_v1(2) = -(beta0 - gamma0)

  obj%rhs_a1(1) = -(beta0 - 0.5_DFP)
  obj%rhs_a1(2) = -(beta0 - gamma0 * 0.5_DFP)

  obj%dis(4) = 1.0_DFP

  obj%vel(1) = -gamma0 * beta_inv
  obj%vel(2) = 1.0_DFP - gamma0 * beta_inv
  obj%vel(3) = 1.0_DFP - gamma0 * beta_inv * 0.5_DFP
  obj%vel(4) = gamma0 * beta_inv

  obj%acc(1) = -beta_inv
  obj%acc(2) = -beta_inv
  obj%acc(3) = 1.0_DFP - 0.5_DFP * beta_inv
  obj%acc(4) = beta_inv

  CALL obj%MakeZeros()

END SUBROUTINE obj_NewmarkBeta

!----------------------------------------------------------------------------
!                                                               HHTAlpha
!----------------------------------------------------------------------------

SUBROUTINE obj_HHTAlpha(obj, alpha, beta, gamma)
  CLASS(SDAlgoParam_), INTENT(INOUT) :: obj
  REAL(DFP), OPTIONAL, INTENT(IN) :: alpha
  !! Default -0.30
  REAL(DFP), OPTIONAL, INTENT(IN) :: beta
  !! Default is (1-alpha)**2/4
  REAL(DFP), OPTIONAL, INTENT(IN) :: gamma
  !! Default is (1-2alpha)/2

  ! internal varibales
  REAL(DFP) :: alpha0, beta0, gamma0, areal

  CALL obj%DEALLOCATE()

  alpha0 = Input(default=-0.30_DFP, option=alpha)
  areal = (1.0_DFP - alpha0)**2 * 0.25_DFP
  beta0 = Input(default=areal, option=beta)
  areal = (1.0_DFP - 2.0_DFP * alpha0) * 0.50_DFP
  gamma0 = Input(default=areal, option=gamma)

  obj%alpha = alpha0

  CALL obj%NewmarkBeta(beta=beta0, gamma=gamma0)

  obj%tanmat(2) = obj%tanmat(2) * (1.0_DFP + alpha0)
  obj%tanmat(3) = obj%tanmat(3) * (1.0_DFP + alpha0)

  obj%rhs_u1(2) = obj%rhs_u1(2) * (1.0_DFP + alpha0)
  obj%rhs_u1(3) = alpha0 * beta0

  obj%rhs_v1(2) = obj%rhs_v1(2) * (1.0_DFP + alpha0)  &
                  & + alpha0 * beta0

  obj%rhs_a1(2) = obj%rhs_a1(2) * (1.0_DFP + alpha0)

  CALL obj%MakeZeros()

END SUBROUTINE obj_HHTAlpha

!----------------------------------------------------------------------------
!                                                               Collocation
!----------------------------------------------------------------------------

SUBROUTINE obj_Collocation(obj, beta, gamma, theta)
  CLASS(SDAlgoParam_), INTENT(INOUT) :: obj
  REAL(DFP), OPTIONAL, INTENT(IN) :: beta
  !! Default is 1/6
  REAL(DFP), OPTIONAL, INTENT(IN) :: gamma
  !! Default is 0.50
  REAL(DFP), OPTIONAL, INTENT(IN) :: theta
  !! Default is 1.4 (Wilson theta)

  ! internal varibales
  REAL(DFP) :: theta0, beta0, gamma0, areal

  CALL obj%DEALLOCATE()

  beta0 = Input(default=1.0_DFP / 6.0_DFP, option=beta)
  gamma0 = Input(default=0.50_DFP, option=gamma)
  theta0 = Input(default=1.4_DFP, option=theta)

  CALL obj%NewmarkBeta(beta=beta0, gamma=gamma0)

  obj%tanmat(2) = obj%tanmat(2) * theta0
  obj%tanmat(3) = obj%tanmat(3) * theta0**2

  obj%rhs_f2 = obj%rhs_f2 * theta0**2

  obj%rhs_u1(2) = obj%rhs_u1(2) * theta0

  obj%rhs_v1(1) = obj%rhs_v1(1) * theta0
  obj%rhs_v1(2) = obj%rhs_v1(2) * theta0**2

  obj%rhs_a1(1) = obj%rhs_a1(1) * theta0**2
  obj%rhs_a1(2) = obj%rhs_a1(2) * theta0**3

  obj%dis(1) = 1.0_DFP - 1.0_DFP / theta0**3
  obj%dis(2) = 1.0_DFP - 1.0_DFP / theta0**2
  obj%dis(3) = (1.0_DFP - 1.0_DFP / theta0) * 0.50_DFP
  obj%dis(4) = 1.0_DFP / theta0**3

  obj%vel(1) = obj%vel(1) / theta0**3
  obj%vel(2) = 1.0_DFP - gamma0 / (theta0**2 * beta0)
  obj%vel(3) = 1.0_DFP - gamma0 / (2.0_DFP * theta0 * beta0)
  obj%vel(4) = obj%vel(4) / theta0**3

  obj%acc(1) = obj%acc(1) / theta0**3
  obj%acc(2) = obj%acc(2) / theta0**2
  obj%acc(3) = 1.0_DFP - 1.0_DFP / (2.0_DFP * theta0 * beta0)
  obj%acc(4) = obj%acc(4) / theta0**3

  CALL obj%MakeZeros()

END SUBROUTINE obj_Collocation

!----------------------------------------------------------------------------
!                                                                 Houbolt
!----------------------------------------------------------------------------

SUBROUTINE obj_Houbolt(obj)
  CLASS(SDAlgoParam_), INTENT(INOUT) :: obj
  CHARACTER(*), PARAMETER :: myName = "Houbolt"
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[WIP] not implemented yet')
END SUBROUTINE obj_Houbolt

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE obj_MakeZeros(obj)
  CLASS(SDAlgoParam_), INTENT(INOUT) :: obj
  ! internal varibales
  REAL(DFP), PARAMETER :: myzero = 0.0_DFP

  obj%tanmat_zero = obj%tanmat.approxeq.myzero
  obj%rhs_u1_zero = obj%rhs_u1.approxeq.myzero
  obj%rhs_v1_zero = obj%rhs_v1.approxeq.myzero
  obj%rhs_a1_zero = obj%rhs_a1.approxeq.myzero
  obj%rhs_f1_zero = obj%rhs_f1.approxeq.myzero
  obj%rhs_f2_zero = obj%rhs_f2.approxeq.myzero
  obj%dis_zero = obj%dis.approxeq.myzero
  obj%vel_zero = obj%vel.approxeq.myzero
  obj%acc_zero = obj%acc.approxeq.myzero
END SUBROUTINE obj_MakeZeros

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

SUBROUTINE obj_Display(obj, msg, unitno)
  CLASS(SDAlgoParam_), INTENT(INOUT) :: obj
  CHARACTER(*), INTENT(IN) :: msg
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno

  CALL Display(msg, unitno=unitno)
  CALL EqualLine(unitno=unitno)
  CALL BlankLines(unitno=unitno)

  CALL Display(obj%name, "name: ", unitno=unitno)
  CALL BlankLines(unitno=unitno)

  CALL Display(obj%tanmat, "tanmat: ", unitno=unitno, advance="NO")
  CALL Display(obj%tanmat_zero, "tanmat_zero: ", unitno=unitno)

  CALL BlankLines(unitno=unitno)
  CALL Display(obj%rhs_u1, "rhs_u1: ", unitno=unitno, advance="NO")
  CALL Display(obj%rhs_u1_zero, "rhs_u1_zero: ", unitno=unitno)

  CALL BlankLines(unitno=unitno)
  CALL Display(obj%rhs_v1, "rhs_v1: ", unitno=unitno, advance="NO")
  CALL Display(obj%rhs_v1_zero, "rhs_v1_zero: ", unitno=unitno)

  CALL BlankLines(unitno=unitno)
  CALL Display(obj%rhs_a1, "rhs_a1: ", unitno=unitno, advance="NO")
  CALL Display(obj%rhs_a1_zero, "rhs_a1_zero: ", unitno=unitno)

  CALL BlankLines(unitno=unitno)
  CALL Display(obj%rhs_f1, "rhs_f1: ", unitno=unitno, advance="NO")
  CALL Display(obj%rhs_f1_zero, "rhs_f1_zero: ", unitno=unitno)

  CALL BlankLines(unitno=unitno)
  CALL Display(obj%rhs_f2, "rhs_f2: ", unitno=unitno, advance="NO")
  CALL Display(obj%rhs_f2_zero, "rhs_f2_zero: ", unitno=unitno)

  CALL BlankLines(unitno=unitno)
  CALL Display(obj%alpha, "alpha: ", unitno=unitno, advance="NO")

  CALL BlankLines(unitno=unitno)
  CALL Display(obj%dis, "dis: ", unitno=unitno, advance="NO")
  CALL Display(obj%dis_zero, "dis_zero: ", unitno=unitno)

  CALL BlankLines(unitno=unitno)
  CALL Display(obj%vel, "vel: ", unitno=unitno, advance="NO")
  CALL Display(obj%vel_zero, "vel_zero: ", unitno=unitno)

  CALL BlankLines(unitno=unitno)
  CALL Display(obj%acc, "acc: ", unitno=unitno, advance="NO")
  CALL Display(obj%acc_zero, "acc_zero: ", unitno=unitno)
END SUBROUTINE obj_Display

!----------------------------------------------------------------------------
!                                                               Deallocate
!----------------------------------------------------------------------------

SUBROUTINE obj_Deallocate(obj)
  CLASS(SDAlgoParam_), INTENT(INOUT) :: obj

  obj%tanmat = 0.0_DFP
  obj%tanmat_zero = .TRUE.

  obj%rhs_u1 = 0.0_DFP
  obj%rhs_v1 = 0.0_DFP
  obj%rhs_a1 = 0.0_DFP

  obj%rhs_u1_zero = .TRUE.
  obj%rhs_v1_zero = .TRUE.
  obj%rhs_a1_zero = .TRUE.

  obj%rhs_f1 = 0.0_DFP
  obj%rhs_f1_zero = .TRUE.

  obj%rhs_f2 = 0.0_DFP
  obj%rhs_f2_zero = .TRUE.

  obj%dis = 0.0_DFP
  obj%vel = 0.0_DFP
  obj%acc = 0.0_DFP

  obj%dis_zero = .TRUE.
  obj%vel_zero = .TRUE.
  obj%acc_zero = .TRUE.

  obj%alpha = 0.0_DFP

END SUBROUTINE obj_Deallocate

!----------------------------------------------------------------------------
!                                                             ImportFromToml
!----------------------------------------------------------------------------

SUBROUTINE obj_ImportFromToml1(obj, table)
  CLASS(SDAlgoParam_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

  CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
  TYPE(toml_table), POINTER :: node, node2
  INTEGER(I4B) :: origin, stat
  REAL(DFP) :: alpha, beta, gamma, theta, areal
  LOGICAL(LGT) :: problem
  TYPE(String) :: astr

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START]')
#endif

  CALL obj%DEALLOCATE()

  CALL GetValue(table=table, key="algorithm", VALUE=astr, &
                default_value="NEWM", origin=origin, stat=stat, &
                isfound=problem)

  IF (.NOT. problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: cannot find algorithm field in toml table. '  &
      & //'algorithm specifies the name of algorithm.')
    RETURN
  END IF

  obj%name = UpperCase(astr%slice(1, 4))

  SELECT CASE (obj%name)
  CASE ("TRAP")
    CALL obj%NewmarkBeta(beta=0.25_DFP, gamma=0.5_DFP)

  CASE ("NEWM")

    CALL toml_get(table, astr%chars(), node, origin=origin, &
                  requested=.FALSE., stat=stat)

    IF (.NOT. ASSOCIATED(node)) THEN
      beta = 0.25_DFP; gamma = 0.5_DFP
    ELSE
      CALL toml_get(node, "beta", beta, 0.25_DFP,   &
        & origin=origin, stat=stat)
      CALL toml_get(node, "gamma", gamma, 0.5_DFP,   &
        & origin=origin, stat=stat)
    END IF
    CALL obj%NewmarkBeta(beta=beta, gamma=gamma)

  CASE ("HHTA")

  CALL toml_get(table, astr%chars(), node, origin=origin, requested=.FALSE., &
            & stat=stat)

    IF (.NOT. ASSOCIATED(node)) THEN
      alpha = -0.30_DFP
      beta = (1.0_DFP - alpha)**2 * 0.25_DFP
      gamma = (1.0_DFP - 2.0_DFP * alpha) * 0.50_DFP
    ELSE
      CALL toml_get(node, "alpha", alpha, -0.30_DFP,   &
        & origin=origin, stat=stat)
      areal = (1.0_DFP - alpha)**2 * 0.25_DFP
      CALL toml_get(node, "beta", beta, areal,   &
        & origin=origin, stat=stat)
      areal = (1.0_DFP - 2.0_DFP * alpha) * 0.50_DFP
      CALL toml_get(node, "gamma", gamma, areal,   &
        & origin=origin, stat=stat)
    END IF

    CALL obj%HHTAlpha(alpha=alpha, beta=beta, gamma=gamma)

  CASE ("COLL")

  CALL toml_get(table, astr%chars(), node, origin=origin, requested=.FALSE., &
            & stat=stat)

    IF (.NOT. ASSOCIATED(node)) THEN
      beta = 1.0_DFP / 6.0_DFP
      gamma = 0.50_DFP
      theta = 1.4_DFP
    ELSE
      CALL toml_get(node, "beta", beta, 1.0_DFP / 6.0_DFP,   &
        & origin=origin, stat=stat)
      CALL toml_get(node, "gamma", gamma, 0.50_DFP,   &
        & origin=origin, stat=stat)
      CALL toml_get(node, "theta", theta, 1.4_DFP,   &
        & origin=origin, stat=stat)
    END IF

    CALL obj%Collocation(beta=beta, gamma=gamma, theta=theta)

  CASE DEFAULT
    node => NULL()
  CALL toml_get(table, astr%chars(), node, origin=origin, requested=.FALSE., &
            & stat=stat)

    IF (.NOT. ASSOCIATED(node)) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[CONFIG ERROR] :: following error occured while reading '//  &
        & 'the toml file :: cannot find '//astr%chars()//" table")
      RETURN
    END IF

    CALL toml_get(node, "tanmat", node2, origin=origin, requested=.FALSE., &
      & stat=stat)

    IF (.NOT. ASSOCIATED(node2)) THEN
      obj%tanmat = 0.0_DFP
    ELSE
      CALL toml_get(node2, "M", obj%tanmat(1), 0.0_DFP, origin=origin, &
        & stat=stat)
      CALL toml_get(node2, "C", obj%tanmat(2), 0.0_DFP, origin=origin, &
        & stat=stat)
      CALL toml_get(node2, "K", obj%tanmat(3), 0.0_DFP, origin=origin, &
        & stat=stat)
    END IF

    CALL toml_get(node, "rhs", node2, origin=origin, requested=.FALSE., &
      & stat=stat)

    IF (.NOT. ASSOCIATED(node2)) THEN
      obj%rhs_u1 = 0.0_DFP
      obj%rhs_v1 = 0.0_DFP
      obj%rhs_a1 = 0.0_DFP
      obj%rhs_f1 = 0.0_DFP
      obj%rhs_f2 = 0.0_DFP
    ELSE

      CALL toml_get(node2, "MU", obj%rhs_u1(1), 0.0_DFP, origin=origin, &
        & stat=stat)

      CALL toml_get(node2, "CU", obj%rhs_u1(2), 0.0_DFP, origin=origin, &
        & stat=stat)

      CALL toml_get(node2, "KU", obj%rhs_u1(3), 0.0_DFP, origin=origin, &
        & stat=stat)

      CALL toml_get(node2, "MV", obj%rhs_v1(1), 0.0_DFP, origin=origin, &
        & stat=stat)

      CALL toml_get(node2, "CV", obj%rhs_v1(2), 0.0_DFP, origin=origin, &
        & stat=stat)

      CALL toml_get(node2, "KV", obj%rhs_v1(3), 0.0_DFP, origin=origin, &
        & stat=stat)

      CALL toml_get(node2, "MA", obj%rhs_a1(1), 0.0_DFP, origin=origin, &
        & stat=stat)

      CALL toml_get(node2, "CA", obj%rhs_a1(2), 0.0_DFP, origin=origin, &
        & stat=stat)

      CALL toml_get(node2, "KA", obj%rhs_a1(3), 0.0_DFP, origin=origin, &
        & stat=stat)

      CALL toml_get(node2, "F1", obj%rhs_f1, 0.0_DFP, origin=origin, &
        & stat=stat)

      CALL toml_get(node2, "F2", obj%rhs_f2, 0.0_DFP, origin=origin, &
        & stat=stat)
    END IF

    CALL toml_get(node, "alpha", obj%alpha, 0.0_DFP, origin=origin, &
      & stat=stat)

    CALL toml_get(node, "dis", node2, origin=origin, requested=.FALSE., &
      & stat=stat)

    IF (.NOT. ASSOCIATED(node2)) THEN
      obj%dis = 0.0_DFP
    ELSE

      CALL toml_get(node2, "U1", obj%dis(1), 0.0_DFP, origin=origin, &
        & stat=stat)

      CALL toml_get(node2, "U2", obj%dis(4), 0.0_DFP, origin=origin, &
        & stat=stat)

      CALL toml_get(node2, "V1", obj%dis(2), 0.0_DFP, origin=origin, &
        & stat=stat)

      CALL toml_get(node2, "A1", obj%dis(3), 0.0_DFP, origin=origin, &
        & stat=stat)
    END IF

    CALL toml_get(node, "vel", node2, origin=origin, requested=.FALSE., &
      & stat=stat)

    IF (.NOT. ASSOCIATED(node2)) THEN
      obj%vel = 0.0_DFP
    ELSE

      CALL toml_get(node2, "U1", obj%vel(1), 0.0_DFP, origin=origin, &
        & stat=stat)

      CALL toml_get(node2, "U2", obj%vel(4), 0.0_DFP, origin=origin, &
        & stat=stat)

      CALL toml_get(node2, "V1", obj%vel(2), 0.0_DFP, origin=origin, &
        & stat=stat)

      CALL toml_get(node2, "A1", obj%vel(3), 0.0_DFP, origin=origin, &
        & stat=stat)
    END IF

    CALL toml_get(node, "acc", node2, origin=origin, requested=.FALSE., &
      & stat=stat)

    IF (.NOT. ASSOCIATED(node2)) THEN
      obj%acc = 0.0_DFP
    ELSE

      CALL toml_get(node2, "U1", obj%acc(1), 0.0_DFP, origin=origin, &
        & stat=stat)

      CALL toml_get(node2, "U2", obj%acc(4), 0.0_DFP, origin=origin, &
        & stat=stat)

      CALL toml_get(node2, "V1", obj%acc(2), 0.0_DFP, origin=origin, &
        & stat=stat)

      CALL toml_get(node2, "A1", obj%acc(3), 0.0_DFP, origin=origin, &
        & stat=stat)
    END IF

    CALL obj%MakeZeros()

  END SELECT

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END]')
#endif
END SUBROUTINE obj_ImportFromToml1

!----------------------------------------------------------------------------
!                                                            ImportFromToml
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate kernel from the toml file

SUBROUTINE obj_ImportFromToml2(obj, tomlName, afile,  &
  & filename, printToml)
  CLASS(SDAlgoParam_), INTENT(INOUT) :: obj
  CHARACTER(*), INTENT(IN) :: tomlName
  TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
  CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
  LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml2()"
  TYPE(toml_table), ALLOCATABLE :: table
  TYPE(toml_table), POINTER :: node
  INTEGER(I4B) :: origin, stat

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START]')
#endif

  CALL GetValue(table=table, afile=afile, filename=filename)

  node => NULL()
  CALL toml_get(table, tomlName, node, origin=origin, requested=.FALSE.,  &
    & stat=stat)

  IF (.NOT. ASSOCIATED(node)) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] :: following error occured while reading '//  &
      & 'the toml file :: cannot find ['//tomlName//"] table in config.")
  END IF

  CALL obj%ImportFromToml(table=node)

#ifdef DEBUG_VER
  IF (PRESENT(printToml)) THEN
    CALL Display(toml_serialize(node), "toml config = "//CHAR_LF,  &
      & unitNo=stdout)
  END IF
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END]')
#endif
END SUBROUTINE obj_ImportFromToml2

END MODULE SDAlgorithms
