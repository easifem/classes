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
USE AbstractKernel_Class, ONLY: AbstractAlgoParam_
USE TxtFile_Class
USE ExceptionHandler_Class, ONLY: e
USE TomlUtility
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

TYPE, EXTENDS(AbstractAlgoParam_) :: SDAlgoParam_
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
  !! displacement = vel(1)*u / dt + vel(2) * v + vel(3) * a *dt + vel(4)*sol/dt
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

CONTAINS
  PRIVATE

  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  PROCEDURE, PUBLIC, PASS(obj) :: MakeZeros => obj_MakeZeros
  PROCEDURE, PUBLIC, PASS(obj) :: NewmarkBeta => obj_NewmarkBeta
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
  REAL(DFP) :: beta, gamma
  CHARACTER(:), ALLOCATABLE :: str1, algorithm
  LOGICAL(LGT) :: problem

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START]')
#endif

  CALL obj%DEALLOCATE()

  CALL toml_get(table, "algorithm", algorithm, origin=origin, stat=stat)

  problem = (.NOT. ALLOCATED(algorithm)) .OR. (stat .NE. toml_stat%success)

  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: cannot find algorithm field in toml table. '  &
      & //'algorithm specifies the name of algorithm.')
    RETURN
  END IF

  str1 = UpperCase(algorithm)

  SELECT CASE (str1)
  CASE ("TRAPEZOIDAL")
    CALL obj%NewmarkBeta(beta=0.25_DFP, gamma=0.5_DFP)

  CASE ("NEWMARKBETA")

    CALL toml_get(table, algorithm, node, origin=origin, requested=.FALSE., &
      & stat=stat)

    IF (.NOT. ASSOCIATED(node)) THEN
      beta = 0.25_DFP; gamma = 0.5_DFP
    ELSE
      CALL toml_get(node, "beta", beta, 0.25_DFP,   &
        & origin=origin, stat=stat)
      CALL toml_get(node, "gamma", gamma, 0.5_DFP,   &
        & origin=origin, stat=stat)
    END IF
    CALL obj%NewmarkBeta(beta=beta, gamma=gamma)

  CASE DEFAULT
    node => NULL()
    CALL toml_get(table, algorithm, node, origin=origin, requested=.FALSE., &
      & stat=stat)

    IF (.NOT. ASSOCIATED(node)) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[CONFIG ERROR] :: following error occured while reading '//  &
        & 'the toml file :: cannot find '//algorithm//" table")
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
