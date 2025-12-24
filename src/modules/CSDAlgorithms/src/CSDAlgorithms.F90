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

MODULE CSDAlgorithms
USE ApproxUtility, ONLY: OPERATOR(.APPROXEQ.)

USE Display_Method, ONLY: Display, &
                          EqualLine, &
                          BlankLines, &
                          tostring

USE ExceptionHandler_Class, ONLY: e

USE GlobalData, ONLY: DFP, I4B, LGT, &
                      CHAR_LF, stdout

USE InputUtility, ONLY: Input

USE SDAlgorithms, ONLY: SDAlgoParam_

USE StringUtility, ONLY: UpperCase

USE TomlUtility, ONLY: GetValue

USE TxtFile_Class, ONLY: TxtFile_

USE tomlf, ONLY: toml_table, &
  & toml_serialize,  &
  & toml_get => get_value, &
  & toml_len => len, &
  & toml_array,  &
  & toml_stat

IMPLICIT NONE

PRIVATE
PUBLIC :: CSDAlgoParam_
CHARACTER(*), PARAMETER :: modName = "CSDAcousticAlgorithms()"

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-01-19
! summary:  declare of composite type sd algorithm

TYPE :: CSDAlgoParam_
  TYPE(SDAlgoParam_) :: subAlgoParams(2)

  REAL(DFP) :: splitRatio = 0.5_DFP
  ! split ratio for a time interval
  ! for example, in bathe method,
  ! this value corresponds to gamma

  REAL(DFP) :: rhs_subsol(3, 2) = 0.0_DFP
  LOGICAL(LGT) :: rhs_subsol_zero(3, 2) = .TRUE.

  REAL(DFP) :: rhs_subf(2) = 0.0_DFP
  LOGICAL(LGT) :: rhs_subf_zero(2) = .TRUE.

  REAL(DFP) :: dis_subsol = 0.0_DFP
  LOGICAL(LGT) :: dis_subsol_zero = .TRUE.

  REAL(DFP) :: vel_subsol = 0.0_DFP
  LOGICAL(LGT) :: vel_subsol_zero = .TRUE.

  REAL(DFP) :: acc_subsol = 0.0_DFP
  LOGICAL(LGT) :: acc_subsol_zero = .TRUE.

  LOGICAL(LGT) :: singleStep = .FALSE.

CONTAINS
  PRIVATE

  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  PROCEDURE, PUBLIC, PASS(obj) :: MakeZeros => obj_MakeZeros
  PROCEDURE, PUBLIC, PASS(obj) :: Bathe => obj_Bathe_master
  PROCEDURE, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  PROCEDURE, PASS(obj) :: ImportFromToml2 => obj_ImportFromToml2
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, ImportFromToml2
END TYPE CSDAlgoParam_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!                                                       GetBParams_gammaBathe
!----------------------------------------------------------------------------

SUBROUTINE GetBParams_gammaBathe(bParams, gamma)
  REAL(DFP), INTENT(INOUT) :: bParams(3)
  REAL(DFP), OPTIONAL, INTENT(in) :: gamma

  CHARACTER(*), PARAMETER :: myName = "GetBParams_gammaBathe"
  REAL(DFP) :: gamma0, areal

  gamma0 = Input(default=0.50_DFP, option=gamma)

  IF (gamma0.approxeq.2.0_DFP) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
     & '[CONFIG ERROR] gamma = 2.0 is not allowed')
    RETURN
  END IF

  areal = 2.0_DFP - gamma0
  bParams(1) = 0.50_DFP / areal
  bParams(2) = bParams(1)
  bParams(3) = (1.0_DFP - gamma0) / areal

END SUBROUTINE GetBParams_gammaBathe

!----------------------------------------------------------------------------
!                                                        GetBParams_betaBathe
!----------------------------------------------------------------------------

SUBROUTINE GetBParams_betaBathe(bParams, gamma, beta1, beta2)
  REAL(DFP), INTENT(INOUT) :: bParams(3)
  REAL(DFP), OPTIONAL, INTENT(in) :: gamma
  REAL(DFP), OPTIONAL, INTENT(in) :: beta1
  REAL(DFP), OPTIONAL, INTENT(in) :: beta2

  CHARACTER(*), PARAMETER :: myName = "GetBParams_betaBathe"
  REAL(DFP) :: gamma0, beta1_0, beta2_0

  gamma0 = Input(default=0.50_DFP, option=gamma)
  beta1_0 = Input(default=1.0_DFP / 3.0_DFP, option=beta1)
  beta2_0 = Input(default=1.0_DFP - beta1_0, option=beta2)

  bParams(1) = (1.0_DFP - beta1_0) * gamma0
  bParams(2) = (1.0_DFP - beta2_0) * (1.0_DFP - gamma0) + beta1_0 * gamma0
  bParams(3) = (1.0_DFP - gamma0) * beta2_0

END SUBROUTINE GetBParams_betaBathe

!----------------------------------------------------------------------------
!                                                        GetBParams_rhoBathe
!----------------------------------------------------------------------------

SUBROUTINE GetBParams_rhoBathe(bParams, gamma, rhoInf)
  REAL(DFP), INTENT(INOUT) :: bParams(3)
  REAL(DFP), OPTIONAL, INTENT(in) :: gamma
  REAL(DFP), OPTIONAL, INTENT(in) :: rhoInf

  CHARACTER(*), PARAMETER :: myName = "GetBParams_rhoBathe"
  REAL(DFP) :: gamma0, rhoInf0, areal

  gamma0 = Input(default=0.50_DFP, option=gamma)
  rhoInf0 = Input(default=0.0_DFP, option=rhoInf)

  IF (rhoInf0 .LT. 0.0_DFP) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] asymptotic spectral radius must be positive')
    RETURN
  END IF

  areal = 2.0_DFP * gamma0 * (rhoInf0 - 1.0_DFP) + 4.0_DFP
  bParams(2) = (rhoInf0 + 1.0_DFP) / areal
  bParams(1) = 0.50_DFP + (gamma0 - 1.0_DFP) * bParams(2)
  bParams(3) = 0.50_DFP - gamma0 * bParams(2)

END SUBROUTINE GetBParams_rhoBathe

!----------------------------------------------------------------------------
!                                                        GetBParams_master
!----------------------------------------------------------------------------

SUBROUTINE GetBParams_master(bParams, gamma, rhoInf, beta1, beta2)
  REAL(DFP), INTENT(INOUT) :: bParams(3)
  REAL(DFP), OPTIONAL, INTENT(in) :: gamma
  REAL(DFP), OPTIONAL, INTENT(in) :: rhoInf
  REAL(DFP), OPTIONAL, INTENT(in) :: beta1
  REAL(DFP), OPTIONAL, INTENT(in) :: beta2

  CHARACTER(*), PARAMETER :: myName = "GetBParams_master"
  LOGICAL(LGT) :: isGamma, isRhoInf, isBeta1, isBeta2, problem

  isGamma = PRESENT(gamma)
  isRhoInf = PRESENT(rhoInf)
  isBeta1 = PRESENT(beta1)
  isBeta2 = PRESENT(beta2)

  problem = isGamma .AND. isRhoInf .AND. isBeta1 .AND. isBeta2

  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] too much parameters are passed')
    RETURN
  END IF

  IF (isRhoInf) THEN
    CALL GetBParams_rhoBathe(bParams, gamma=gamma, rhoInf=rhoInf)
    RETURN
  END IF

  IF (isBeta1 .OR. isBeta2) THEN
    CALL GetBParams_rhoBathe(bParams, gamma=gamma, rhoInf=rhoInf)
    RETURN
  END IF

  IF (isGamma) THEN
    CALL GetBParams_gammaBathe(bParams, gamma=gamma)
    RETURN
  END IF

  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[CONFIG ERROR] no case is found ')

END SUBROUTINE GetBParams_master

!----------------------------------------------------------------------------
!                                                               Bathe_master
!----------------------------------------------------------------------------

SUBROUTINE obj_Bathe_master(obj, gamma, rhoInf, beta1, beta2)
  CLASS(CSDAlgoParam_), INTENT(INOUT) :: obj
  REAL(DFP), OPTIONAL, INTENT(IN) :: gamma
  REAL(DFP), OPTIONAL, INTENT(IN) :: rhoInf
  REAL(DFP), OPTIONAL, INTENT(IN) :: beta1
  REAL(DFP), OPTIONAL, INTENT(IN) :: beta2
  REAL(DFP) :: bParams(3)

  ! internal varibales
  CHARACTER(*), PARAMETER :: myName = "obj_Bathe_master"
  REAL(DFP) :: gamma0, gamma2, b0, b1, b2

  CALL obj%DEALLOCATE()

  gamma0 = Input(default=0.5_DFP, option=gamma)

  obj%splitRatio = gamma0
  gamma2 = gamma0 * gamma0

  !! sub-step 1
  !! equivalent to Trapezoidal rule
  obj%subAlgoParams(1)%tanmat(1) = 1.0_DFP
  obj%subAlgoParams(1)%tanmat(2) = gamma0 * 0.50_DFP
  obj%subAlgoParams(1)%tanmat(3) = gamma2 * 0.25_DFP

  obj%rhs_subf(1) = gamma2 * 0.25_DFP

  obj%subAlgoParams(1)%rhs_u1(1) = 1.0_DFP
  obj%subAlgoParams(1)%rhs_u1(2) = 0.50_DFP * gamma0

  obj%subAlgoParams(1)%rhs_v1(1) = gamma0
  obj%subAlgoParams(1)%rhs_v1(2) = gamma2 * 0.25_DFP

  obj%subAlgoParams(1)%rhs_a1(1) = gamma2 * 0.25_DFP

  !! sub-step 2
  CALL GetBParams_master(bParams, gamma=gamma0, rhoInf=rhoInf, &
                        & beta1=beta1, beta2=beta2)
  b0 = bParams(1)
  b1 = bParams(2)
  b2 = bParams(3)

  obj%subAlgoParams(2)%tanmat(1) = 1.0_DFP
  obj%subAlgoParams(2)%tanmat(2) = b2
  obj%subAlgoParams(2)%tanmat(3) = b2**2

  obj%subAlgoParams(2)%rhs_f2 = b2**2

  obj%subAlgoParams(2)%rhs_u1(1) = -2.0_DFP * b1 / gamma0  &
          & - 4.0_DFP * b1 * b2 / gamma2 + 1.0_DFP
  obj%subAlgoParams(2)%rhs_u1(2) = -2.0_DFP * b1 * b2 / gamma0 + b2

  obj%rhs_subsol(1, 2) = 2.0_DFP * b1 / gamma0 + &
                      & 4.0_DFP * b1 * b2 / gamma2
  obj%rhs_subsol(2, 2) = 2.0_DFP * b1 * b2 / gamma0

  obj%subAlgoParams(2)%rhs_v1(1) = -b0 + b1 + b2 -  &
                          & 4.0_DFP * b1 * b2 / gamma0
  obj%subAlgoParams(2)%rhs_v1(2) = -b2 * (b1 - b0)

  obj%subAlgoParams(2)%rhs_a1(1) = -b2 * (b1 - b0)

  obj%subAlgoParams(2)%acc(1) = 2.0_DFP * b1 / (b2**2 * gamma0)  &
                & - 1.0_DFP / b2**2 + 4.0_DFP * b1 / (b2 * gamma2)
  obj%subAlgoParams(2)%acc(2) = (b1 - b0) / b2**2 - 1.0_DFP / b2  &
                & + 4.0_DFP * b1 / (b2 * gamma0)
  obj%subAlgoParams(2)%acc(3) = (b1 - b0) / b2
  obj%subAlgoParams(2)%acc(4) = 1.0_DFP / b2**2

  obj%acc_subsol = -2.0_DFP*b1 / (b2**2*gamma0) - 4.0_DFP*b1 /(b2*gamma2)

  obj%subAlgoParams(2)%vel(1) = 2.0_DFP * b1 / (b2 * gamma0) - 1.0_DFP / b2
  obj%subAlgoParams(2)%vel(2) = (b1 - b0) / b2
  obj%subAlgoParams(2)%vel(3) = 0.0_DFP
  obj%subAlgoParams(2)%vel(4) = 1.0_DFP / b2

  obj%vel_subsol = -2.0_DFP * b1 / (b2 * gamma0)

  obj%subAlgoParams(2)%dis(4) = 1.0_DFP

  obj%dis_subsol = 0.0_DFP

  CALL obj%MakeZeros()

END SUBROUTINE obj_Bathe_master

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE obj_MakeZeros(obj)
  CLASS(CSDAlgoParam_), INTENT(INOUT) :: obj
  ! internal varibales
  REAL(DFP), PARAMETER :: myzero = 0.0_DFP
  INTEGER(I4B) :: ii

  DO ii = 1, SIZE(obj%subAlgoParams)
    CALL obj%subAlgoParams(ii)%MakeZeros()
  END DO

  obj%rhs_subsol_zero = obj%rhs_subsol.approxeq.myzero
  obj%rhs_subf_zero = obj%rhs_subf.approxeq.myzero
  obj%dis_subsol_zero = obj%dis_subsol.approxeq.myzero
  obj%vel_subsol_zero = obj%vel_subsol.approxeq.myzero
  obj%acc_subsol_zero = obj%acc_subsol.approxeq.myzero

END SUBROUTINE obj_MakeZeros

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

SUBROUTINE obj_Display(obj, msg, unitno)
  CLASS(CSDAlgoParam_), INTENT(INOUT) :: obj
  CHARACTER(*), INTENT(IN) :: msg
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  INTEGER(I4B) :: ii

  CALL Display(msg, unitno=unitno)
  CALL EqualLine(unitno=unitno)
  CALL BlankLines(unitno=unitno)

  DO ii = 1, SIZE(obj%subAlgoParams)
    CALL obj%subAlgoParams(ii)%Display(msg="sub-step "//tostring(ii),  &
    & unitno=unitno)
  END DO

  CALL BlankLines(unitno=unitno)
  CALL Display(obj%splitRatio, "splitRatio: ", unitno=unitno, advance="NO")

  CALL BlankLines(unitno=unitno)
  CALL Display(obj%rhs_subsol, "rhs_subsol: ", unitno=unitno, advance="NO")
  CALL Display(obj%rhs_subsol_zero, "rhs_subsol_zero: ", unitno=unitno)

  CALL BlankLines(unitno=unitno)
  CALL Display(obj%rhs_subf, "rhs_subf: ", unitno=unitno, advance="NO")
  CALL Display(obj%rhs_subf_zero, "rhs_subf_zero: ", unitno=unitno)

  CALL BlankLines(unitno=unitno)
  CALL Display(obj%dis_subsol, "dis_subsol: ", unitno=unitno, advance="NO")
  CALL Display(obj%dis_subsol_zero, "dis_subsol_zero: ", unitno=unitno)

  CALL BlankLines(unitno=unitno)
  CALL Display(obj%vel_subsol, "vel_subsol: ", unitno=unitno, advance="NO")
  CALL Display(obj%vel_subsol_zero, "vel_subsol_zero: ", unitno=unitno)

  CALL BlankLines(unitno=unitno)
  CALL Display(obj%acc_subsol, "acc_subsol_zero: ", unitno=unitno,  &
              & advance="NO")
  CALL Display(obj%acc_subsol_zero, "acc_subsol_zero: ", unitno=unitno)

END SUBROUTINE obj_Display

!----------------------------------------------------------------------------
!                                                               Deallocate
!----------------------------------------------------------------------------

SUBROUTINE obj_Deallocate(obj)
  CLASS(CSDAlgoParam_), INTENT(INOUT) :: obj
  INTEGER(I4B) :: ii

  DO ii = 1, SIZE(obj%subAlgoParams)
    CALL obj%subAlgoParams(ii)%DEALLOCATE()
  END DO

  obj%splitRatio = 0.0_DFP

  obj%rhs_subsol = 0.0_DFP
  obj%rhs_subf = 0.0_DFP

  obj%rhs_subsol_zero = .TRUE.
  obj%rhs_subf_zero = .TRUE.

  obj%dis_subsol = 0.0_DFP
  obj%vel_subsol = 0.0_DFP
  obj%acc_subsol = 0.0_DFP

  obj%dis_subsol_zero = .TRUE.
  obj%vel_subsol_zero = .TRUE.
  obj%acc_subsol_zero = .TRUE.

  obj%singleStep = .FALSE.

END SUBROUTINE obj_Deallocate

!----------------------------------------------------------------------------
!                                                             ImportFromToml1
!----------------------------------------------------------------------------

SUBROUTINE obj_ImportFromToml1(obj, table)
  CLASS(CSDAlgoParam_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

  CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
  TYPE(toml_table), POINTER :: node
  INTEGER(I4B) :: origin, stat
  REAL(DFP) :: beta, gamma, rhoInf, beta1, beta2
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
  CASE ("GAMMABATHE", "GBATHE", "BATHE")

    CALL toml_get(table, algorithm, node, origin=origin, requested=.FALSE., &
      & stat=stat)

    IF (.NOT. ASSOCIATED(node)) THEN
      gamma = 0.50_DFP
    ELSE
      CALL toml_get(node, "gamma", gamma, 0.5_DFP,   &
        & origin=origin, stat=stat)
    END IF
    CALL obj%Bathe(gamma=gamma)

  CASE ("BETABATHE", "BBATHE")

    CALL toml_get(table, algorithm, node, origin=origin, requested=.FALSE., &
      & stat=stat)

    IF (.NOT. ASSOCIATED(node)) THEN
      gamma = 0.50_DFP
      beta1 = 1.0_DFP / 3.0_DFP
      beta2 = 1.0_DFP - beta1
    ELSE
      CALL toml_get(node, "gamma", gamma, 0.5_DFP,   &
        & origin=origin, stat=stat)
      CALL toml_get(node, "beta1", beta1, 1.0_DFP / 3.0_DFP, &
        & origin=origin, stat=stat)
      CALL toml_get(node, "beta2", beta2, 1.0_DFP - beta1, &
        & origin=origin, stat=stat)
    END IF
    CALL obj%Bathe(gamma=gamma, beta1=beta1, beta2=beta2)

  CASE ("RHOBATHE", "RBATHE")

    CALL toml_get(table, algorithm, node, origin=origin, requested=.FALSE., &
      & stat=stat)

    IF (.NOT. ASSOCIATED(node)) THEN
      gamma = 0.50_DFP
      rhoInf = 0.0_DFP
    ELSE
      CALL toml_get(node, "gamma", gamma, 0.5_DFP,   &
        & origin=origin, stat=stat)
      CALL toml_get(node, "rhoInf", rhoInf, 0.0_DFP, &
        & origin=origin, stat=stat)
    END IF
    CALL obj%Bathe(gamma=gamma, rhoInf=rhoInf)

  CASE ("NEWMARK", "NEWMARKBETA")

    CALL toml_get(table, algorithm, node, origin=origin, &
                  requested=.FALSE., stat=stat)

    IF (.NOT. ASSOCIATED(node)) THEN
      beta = 0.25_DFP; gamma = 0.5_DFP
    ELSE
      CALL toml_get(node, "beta", beta, 0.25_DFP,   &
        & origin=origin, stat=stat)
      CALL toml_get(node, "gamma", gamma, 0.5_DFP,   &
        & origin=origin, stat=stat)
    END IF

    CALL obj%subAlgoParams(1)%NewmarkBeta(beta=beta, gamma=gamma)
    obj%splitRatio = 1.0_DFP
    obj%singleStep = .TRUE.

  CASE DEFAULT

    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] no case is found ')

  END SELECT

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END]')
#endif

END SUBROUTINE obj_ImportFromToml1

!----------------------------------------------------------------------------
!                                                             ImportFromToml2
!----------------------------------------------------------------------------

SUBROUTINE obj_ImportFromToml2(obj, tomlName, afile,  &
  & filename, printToml)
  CLASS(CSDAlgoParam_), INTENT(INOUT) :: obj
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

END MODULE CSDAlgorithms
