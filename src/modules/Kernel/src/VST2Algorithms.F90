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

MODULE VST2Algorithms
USE BaseMethod
USE BaseType
USE AbstractKernel_Class, ONLY: AbstractAlgoParam_
USE TxtFile_Class
USE ExceptionHandler_Class, ONLY: e
USE TomlUtility
USE tomlf, ONLY: toml_table, &
                 toml_serialize, &
                 toml_get => get_value, &
                 toml_len => len, &
                 toml_array, &
                 toml_stat
IMPLICIT NONE

PRIVATE
PUBLIC :: VST2AlgoParam_
CHARACTER(*), PARAMETER :: modName = "VST2Algorithms()"
REAL(DFP), PARAMETER :: default_a = 1.0_DFP

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-24
! summary:  Algorithm data for 2x2 v-STFEM type Algorithms

TYPE :: VST2AlgoParam_
  REAL(DFP) :: tanmat(3, 4) = 0.0_DFP
  !! block(tanmat) = tanmat(1,a)*M + tanmat(2, a)*C*dt + tanmat(3, a)*K*dt*dt
  !! col1 is (1,1) block
  !! col2 is (1,2) block
  !! col3 is (2,1) block
  !! col4 is (2,2) block
  !! tanmat(1, a) : coefficient of mass matrix (M)
  !! tanmat(2, a) : coefficient of damping matrix (C*dt)
  !! tanmat(3, a) : coefficient of stiffness matrix (K*dt*dt)
  LOGICAL(LGT) :: tanmat_zero(3, 4) = .TRUE.
  !! It is set internally by calling MakeZeros

  REAL(DFP) :: rhs_u1(3, 2) = 0.0_DFP
  !! col1 if for rhs 1 (equation at time t1)
  !! col2 is for rhs 2 (equation at time t2)
  !!
  !! coefficient for rhs for (displacement/dt) at time tn
  !!
  !! rhs1 += (rhs_u1(1,1)*M + rhs_u1(2,1)*C*dt + rhs_u1(3,1)*K*dt*dt)*u/dt
  !!
  !! rhs2 += (rhs_u1(1,2)*M + rhs_u1(2,2)*C*dt + rhs_u1(3,2)*K*dt*dt)*u/dt
  !!
  !! rhs_u1(1,1) = coefficient for M
  !! rhs_u1(2,1) = coefficient for C*dt
  !! rhs_u1(3,1) = coefficient for K*dt*dt

  LOGICAL(LGT) :: rhs_u1_zero(3, 2) = .TRUE.

  REAL(DFP) :: rhs_v1(3, 2) = 0.0_DFP
  !! col1 if for rhs 1 (equation at time t1)
  !! col2 is for rhs 2 (equation at time t2)
  !!
  !! coefficient for rhs for (velocity) at time tn
  !!
  !! rhs1 += (rhs_v1(1,1)*M + rhs_v1(2,1)*C*dt + rhs_v1(3,1)*K*dt*dt)*v
  !!
  !! rhs2 += (rhs_v1(1,2)*M + rhs_v1(2,2)*C*dt + rhs_v1(3,2)*K*dt*dt)*v
  !!
  !! rhs_v1(1,1) = coefficient for M
  !! rhs_v1(2,1) = coefficient for C*dt
  !! rhs_v1(3,1) = coefficient for K*dt*dt

  LOGICAL(LGT) :: rhs_v1_zero(3, 2) = .TRUE.

  REAL(DFP) :: rhs_a1(3, 2) = 0.0_DFP
  !! col1 if for rhs 1 (equation at time t1)
  !! col2 is for rhs 2 (equation at time t2)
  !!
  !! coefficient for rhs for (acceleration) at time tn
  !!
  !! rhs1 += (rhs_a1(1,1)*M + rhs_a1(2,1)*C*dt + rhs_a1(3,1)*K*dt*dt)*a*dt
  !!
  !! rhs2 += (rhs_a1(1,2)*M + rhs_a1(2,2)*C*dt + rhs_a1(3,2)*K*dt*dt)*a*dt
  LOGICAL(LGT) :: rhs_a1_zero(3, 2) = .TRUE.

  REAL(DFP) :: rhs_f1(2) = 0.0_DFP
  !! coefficient for rhs for (force*dt) at time tn
  !! rhs(1) = rhs(1) + rhs_f1(1) * force1 * dt
  LOGICAL(LGT) :: rhs_f1_zero(2) = .TRUE.

  REAL(DFP) :: rhs_f2(2) = 0.0_DFP
  !! coefficient for rhs for (force*dt*dt) at time tn+1
  !! rhs = rhs + rhs_f2 *dt *dt * force2
  LOGICAL(LGT) :: rhs_f2_zero(2) = .TRUE.

  REAL(DFP) :: disp(4) = 0.0_DFP
  !! disp coefficient for displacement update
  !! disp = disp(1)*u + disp(2) * v * dt + disp(3) * sol(1) * dt +  &
  !! disp(4) * sol(2) * dt
  !! disp(1) coefficient of u0
  !! disp(2) coefficient of v0 * dt
  !! disp(3) coefficient of v1 * dt
  !! disp(4) coefficient of v2 * dt
  LOGICAL(LGT) :: disp_zero(4) = .TRUE.

  REAL(DFP) :: vel(4) = 0.0_DFP
  !! vel coefficient for velocity update
  !! vel = vel(1)*u/dt + vel(2) * v + vel(3) * sol(1) +  &
  !! vel(4) * sol(2)
  !! vel(1) coefficient of u0 / dt
  !! vel(2) coefficient of v0
  !! vel(3) coefficient of v1
  !! vel(4) coefficient of v2
  LOGICAL(LGT) :: vel_zero(4) = .TRUE.

  REAL(DFP) :: acc(4) = 0.0_DFP
  !! acc coefficient for acceleration update
  !! acc = acc(1)*u/dt/dt + acc(2)*v/dt + acc(3)*sol(1) /dt +  &
  !! acc(4)*sol(2)/dt
  !!
  !! acc(1) coefficient of u0 / dt / dt
  !! acc(2) coefficient of v0 / dt
  !! acc(3) coefficient of v1 / dt
  !! acc(4) coefficient of v2 / dt

  LOGICAL(LGT) :: acc_zero(4) = .TRUE.

CONTAINS
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  PROCEDURE, PUBLIC, PASS(obj) :: MakeZeros => obj_MakeZeros
  PROCEDURE, PUBLIC, PASS(obj) :: VSTFEM => obj_VSTFEM
  PROCEDURE, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  PROCEDURE, PASS(obj) :: ImportFromToml2 => obj_ImportFromToml2
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, ImportFromToml2
END TYPE VST2AlgoParam_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!                                                                 MakeZeros
!----------------------------------------------------------------------------

SUBROUTINE obj_MakeZeros(obj)
  CLASS(VST2AlgoParam_), INTENT(INOUT) :: obj
  REAL(DFP), PARAMETER :: myzero = 0.0_DFP
  obj%tanmat_zero = obj%tanmat.approxeq.myzero
  obj%rhs_u1_zero = obj%rhs_u1.approxeq.myzero
  obj%rhs_v1_zero = obj%rhs_v1.approxeq.myzero
  obj%rhs_a1_zero = obj%rhs_a1.approxeq.myzero
  obj%rhs_f1_zero = obj%rhs_f1.approxeq.myzero
  obj%rhs_f2_zero = obj%rhs_f2.approxeq.myzero
  obj%disp_zero = obj%disp.approxeq.myzero
  obj%vel_zero = obj%vel.approxeq.myzero
  obj%acc_zero = obj%acc.approxeq.myzero
END SUBROUTINE obj_MakeZeros

!----------------------------------------------------------------------------
!                                                               Deallocate
!----------------------------------------------------------------------------

SUBROUTINE obj_Deallocate(obj)
  CLASS(VST2AlgoParam_), INTENT(INOUT) :: obj

  obj%tanmat = 0.0_DFP
  obj%tanmat_zero = .TRUE.

  obj%rhs_u1 = 0.0_DFP
  obj%rhs_u1_zero = .TRUE.

  obj%rhs_v1 = 0.0_DFP
  obj%rhs_v1_zero = .TRUE.

  obj%rhs_a1 = 0.0_DFP
  obj%rhs_a1_zero = .TRUE.

  obj%rhs_f1 = 0.0_DFP
  obj%rhs_f1_zero = .TRUE.

  obj%rhs_f2 = 0.0_DFP
  obj%rhs_f2_zero = .TRUE.

  obj%disp = 0.0_DFP
  obj%vel = 0.0_DFP
  obj%acc = 0.0_DFP

  obj%disp_zero = .TRUE.
  obj%vel_zero = .TRUE.
  obj%acc_zero = .TRUE.
END SUBROUTINE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                  Display
!----------------------------------------------------------------------------

SUBROUTINE obj_Display(obj, msg, unitno)
  CLASS(VST2AlgoParam_), INTENT(INOUT) :: obj
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
  CALL Display(obj%disp, "disp: ", unitno=unitno, advance="NO")
  CALL Display(obj%disp_zero, "disp_zero: ", unitno=unitno)

  CALL BlankLines(unitno=unitno)
  CALL Display(obj%vel, "vel: ", unitno=unitno, advance="NO")
  CALL Display(obj%vel_zero, "vel_zero: ", unitno=unitno)

  CALL BlankLines(unitno=unitno)
  CALL Display(obj%acc, "acc: ", unitno=unitno, advance="NO")
  CALL Display(obj%acc_zero, "acc_zero: ", unitno=unitno)
END SUBROUTINE obj_Display

!----------------------------------------------------------------------------
!                                                                   vSTFEM
!----------------------------------------------------------------------------

SUBROUTINE obj_VSTFEM(obj, a)
  CLASS(VST2AlgoParam_), INTENT(INOUT) :: obj
  REAL(DFP), OPTIONAL, INTENT(IN) :: a

  REAL(DFP) :: k11, k12, k21, k22, alpha

  alpha = Input(default=default_a, option=a)

  k11 = (9.0_DFP + alpha) / 36.0_DFP
  k12 = (3.0_DFP - alpha) / 36.0_DFP
  k21 = (15.0_DFP - alpha) / 36.0_DFP
  k22 = k11

  CALL obj%DEALLOCATE()
  obj%tanmat(1, 1) = 0.5_DFP
  obj%tanmat(2, 1) = 1.0_DFP / 3.0_DFP
  obj%tanmat(3, 1) = 0.5_DFP * k11
  obj%tanmat(1, 2) = 0.5_DFP
  obj%tanmat(2, 2) = 1.0_DFP / 6.0_DFP
  obj%tanmat(3, 2) = 0.5_DFP * k12

  obj%tanmat(1, 3) = -0.5_DFP
  obj%tanmat(2, 3) = 1.0_DFP / 6.0_DFP
  obj%tanmat(3, 3) = 0.5_DFP * k21

  obj%tanmat(1, 4) = 0.5_DFP
  obj%tanmat(2, 4) = 1.0_DFP / 3.0_DFP
  obj%tanmat(3, 4) = 0.5_DFP * k22

  obj%rhs_u1(3, 1) = -0.5_DFP
  obj%rhs_u1(3, 2) = -0.5_DFP

  obj%rhs_v1(1, 1) = 1.0_DFP

  obj%rhs_f1(1) = 1.0_DFP / 3.0
  obj%rhs_f1(2) = 1.0_DFP / 6.0

  obj%rhs_f2(1) = 1.0_DFP / 6.0
  obj%rhs_f2(2) = 1.0_DFP / 3.0

  obj%disp(1) = 1.0_DFP
  obj%disp(3) = 0.5_DFP
  obj%disp(4) = 0.5_DFP

  obj%vel(4) = 1.0_DFP

  obj%acc(3) = -1.0_DFP
  obj%acc(4) = 1.0_DFP

  CALL obj%MakeZeros()

END SUBROUTINE obj_VSTFEM

!----------------------------------------------------------------------------
!                                                             ImportFromToml
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-01-14
! summary:  Initiate all paramters of VST2Algo from given toml table

SUBROUTINE obj_ImportFromToml1(obj, table)
  CLASS(VST2AlgoParam_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

  CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
  TYPE(toml_table), POINTER :: node, node2
  INTEGER(I4B) :: origin, stat
  REAL(DFP) :: a
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
  CASE ("UVSTFEM", "UVST")
    CALL obj%VSTFEM(a=1.0_DFP)

  CASE ("VSTFEM", "VST")
    CALL obj%VSTFEM(a=0.0_DFP)

  CASE ("LCVSTFEM", "LCVST")
    CALL toml_get(table, algorithm, node, origin=origin, requested=.FALSE., &
      & stat=stat)

    IF (.NOT. ASSOCIATED(node)) THEN
      a = default_a
    ELSE
      CALL toml_get(node, "a", a, default_a,   &
        & origin=origin, stat=stat)
    END IF
    CALL obj%VSTFEM(a=a)

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
      CALL toml_get(node2, "M11", obj%tanmat(1, 1), 0.0_DFP, origin=origin, &
        & stat=stat)
      CALL toml_get(node2, "M12", obj%tanmat(1, 2), 0.0_DFP, origin=origin, &
        & stat=stat)
      CALL toml_get(node2, "M21", obj%tanmat(1, 3), 0.0_DFP, origin=origin, &
        & stat=stat)
      CALL toml_get(node2, "M22", obj%tanmat(1, 4), 0.0_DFP, origin=origin, &
        & stat=stat)
      CALL toml_get(node2, "C11", obj%tanmat(2, 1), 0.0_DFP, origin=origin, &
        & stat=stat)
      CALL toml_get(node2, "C12", obj%tanmat(2, 2), 0.0_DFP, origin=origin, &
        & stat=stat)
      CALL toml_get(node2, "C21", obj%tanmat(2, 3), 0.0_DFP, origin=origin, &
        & stat=stat)
      CALL toml_get(node2, "C22", obj%tanmat(2, 4), 0.0_DFP, origin=origin, &
        & stat=stat)
      CALL toml_get(node2, "K11", obj%tanmat(3, 1), 0.0_DFP, origin=origin, &
        & stat=stat)
      CALL toml_get(node2, "K12", obj%tanmat(3, 2), 0.0_DFP, origin=origin, &
        & stat=stat)
      CALL toml_get(node2, "K21", obj%tanmat(3, 3), 0.0_DFP, origin=origin, &
        & stat=stat)
      CALL toml_get(node2, "K22", obj%tanmat(3, 4), 0.0_DFP, origin=origin, &
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

      CALL toml_get(node2, "MU1", obj%rhs_u1(1, 1), 0.0_DFP, origin=origin, &
        & stat=stat)
      CALL toml_get(node2, "MU2", obj%rhs_u1(1, 2), 0.0_DFP, origin=origin, &
        & stat=stat)

      CALL toml_get(node2, "CU1", obj%rhs_u1(2, 1), 0.0_DFP, origin=origin, &
        & stat=stat)
      CALL toml_get(node2, "CU2", obj%rhs_u1(2, 2), 0.0_DFP, origin=origin, &
        & stat=stat)

      CALL toml_get(node2, "KU1", obj%rhs_u1(3, 1), 0.0_DFP, origin=origin, &
        & stat=stat)
      CALL toml_get(node2, "KU2", obj%rhs_u1(3, 2), 0.0_DFP, origin=origin, &
        & stat=stat)

      CALL toml_get(node2, "MV1", obj%rhs_v1(1, 1), 0.0_DFP, origin=origin, &
        & stat=stat)
      CALL toml_get(node2, "MV2", obj%rhs_v1(1, 2), 0.0_DFP, origin=origin, &
        & stat=stat)

      CALL toml_get(node2, "CV1", obj%rhs_v1(2, 1), 0.0_DFP, origin=origin, &
        & stat=stat)
      CALL toml_get(node2, "CV2", obj%rhs_v1(2, 2), 0.0_DFP, origin=origin, &
        & stat=stat)

      CALL toml_get(node2, "KV1", obj%rhs_v1(3, 1), 0.0_DFP, origin=origin, &
        & stat=stat)
      CALL toml_get(node2, "KV2", obj%rhs_v1(3, 2), 0.0_DFP, origin=origin, &
        & stat=stat)

      CALL toml_get(node2, "MA1", obj%rhs_a1(1, 1), 0.0_DFP, origin=origin, &
        & stat=stat)
      CALL toml_get(node2, "MA2", obj%rhs_a1(1, 2), 0.0_DFP, origin=origin, &
        & stat=stat)

      CALL toml_get(node2, "CA1", obj%rhs_a1(2, 1), 0.0_DFP, origin=origin, &
        & stat=stat)
      CALL toml_get(node2, "CA2", obj%rhs_a1(2, 2), 0.0_DFP, origin=origin, &
        & stat=stat)

      CALL toml_get(node2, "KA1", obj%rhs_a1(3, 1), 0.0_DFP, origin=origin, &
        & stat=stat)
      CALL toml_get(node2, "KA2", obj%rhs_a1(3, 2), 0.0_DFP, origin=origin, &
        & stat=stat)

      CALL toml_get(node2, "F11", obj%rhs_f1(1), 0.0_DFP, origin=origin, &
        & stat=stat)
      CALL toml_get(node2, "F12", obj%rhs_f1(2), 0.0_DFP, origin=origin, &
        & stat=stat)

      CALL toml_get(node2, "F21", obj%rhs_f2(1), 0.0_DFP, origin=origin, &
        & stat=stat)
      CALL toml_get(node2, "F22", obj%rhs_f2(2), 0.0_DFP, origin=origin, &
        & stat=stat)

    END IF

    CALL toml_get(node, "disp", node2, origin=origin, requested=.FALSE., &
      & stat=stat)

    IF (.NOT. ASSOCIATED(node2)) THEN
      obj%disp = 0.0_DFP
    ELSE

      CALL toml_get(node2, "U0", obj%disp(1), 0.0_DFP, origin=origin, &
        & stat=stat)

      CALL toml_get(node2, "V0", obj%disp(2), 0.0_DFP, origin=origin, &
        & stat=stat)

      CALL toml_get(node2, "V1", obj%disp(3), 0.0_DFP, origin=origin, &
        & stat=stat)

      CALL toml_get(node2, "V2", obj%disp(4), 0.0_DFP, origin=origin, &
        & stat=stat)
    END IF

    CALL toml_get(node, "vel", node2, origin=origin, requested=.FALSE., &
      & stat=stat)

    IF (.NOT. ASSOCIATED(node2)) THEN
      obj%vel = 0.0_DFP
    ELSE

      CALL toml_get(node2, "U0", obj%vel(1), 0.0_DFP, origin=origin, &
        & stat=stat)

      CALL toml_get(node2, "V0", obj%vel(2), 0.0_DFP, origin=origin, &
        & stat=stat)

      CALL toml_get(node2, "V1", obj%vel(3), 0.0_DFP, origin=origin, &
        & stat=stat)

      CALL toml_get(node2, "V2", obj%vel(4), 0.0_DFP, origin=origin, &
        & stat=stat)
    END IF

    CALL toml_get(node, "acc", node2, origin=origin, requested=.FALSE., &
      & stat=stat)

    IF (.NOT. ASSOCIATED(node2)) THEN
      obj%acc = 0.0_DFP
    ELSE

      CALL toml_get(node2, "U0", obj%acc(1), 0.0_DFP, origin=origin, &
        & stat=stat)

      CALL toml_get(node2, "V0", obj%acc(2), 0.0_DFP, origin=origin, &
        & stat=stat)

      CALL toml_get(node2, "V1", obj%acc(3), 0.0_DFP, origin=origin, &
        & stat=stat)

      CALL toml_get(node2, "V2", obj%acc(4), 0.0_DFP, origin=origin, &
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

!> author: Shion Shimizu
! date:   2024-01-14
! summary:  Initiate kernel from the toml file

SUBROUTINE obj_ImportFromToml2(obj, tomlName, afile,  &
  & filename, printToml)
  CLASS(VST2AlgoParam_), INTENT(INOUT) :: obj
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

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE VST2Algorithms
