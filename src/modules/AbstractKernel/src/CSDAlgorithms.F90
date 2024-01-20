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

MODULE CSDAlgorithms
USE BaseMethod
USE BaseType
USE AbstractKernel_Class, ONLY: AbstractAlgoParam_
USE SDAlgorithms
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
PUBLIC :: CSDAlgoParam_
CHARACTER(*), PARAMETER :: modName = "CSDAcousticAlgorithms()"

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2024-01-19
! summary:  declare of composite type sd algorithm

TYPE, EXTENDS(AbstractAlgoParam_) :: CSDAlgoParam_
  TYPE(SDAlgoParam_) :: subAlgoParams(2)

  REAL(DFP) :: splitRatio = 0.5_DFP

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

CONTAINS
  PRIVATE

  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  PROCEDURE, PUBLIC, PASS(obj) :: MakeZeros => obj_MakeZeros
  PROCEDURE, PUBLIC, PASS(obj) :: gammaBathe => obj_gammaBatheU
  ! PROCEDURE, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  ! PROCEDURE, PASS(obj) :: ImportFromToml2 => obj_ImportFromToml2
  ! GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, ImportFromToml2
END TYPE CSDAlgoParam_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!                                                               getbparams
!----------------------------------------------------------------------------

SUBROUTINE GetBParams(bParams, theta, rhoInf, beta1)
  REAL(DFP), INTENT(inout) :: bParams(3)
  REAL(DFP), OPTIONAL, INTENT(in) :: theta
  REAL(DFP), OPTIONAL, INTENT(in) :: rhoInf
  REAL(DFP), OPTIONAL, INTENT(in) :: beta1

  CHARACTER(*), PARAMETER :: myName = "GetBParams"
  LOGICAL(LGT) :: isTheta, isRhoInf, isBeta1, abool
  REAL(DFP) :: areal

  isTheta = PRESENT(theta)
  isRhoInf = PRESENT(rhoInf)
  isBeta1 = PRESENT(beta1)

  abool = (isTheta .AND. isRhoInf .AND. isBeta1)

  IF (abool) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
     & '[CONFIG ERROR] too many parameters present')
    RETURN
  END IF

  IF (isTheta .AND. isRhoInf) THEN
    !! rhoInfBathe setting
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[WIP] not implemented yet')
    RETURN
  END IF

  IF (isBeta1) THEN
    !! b1b2Bathe 1st order ver
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[WIP] not implemented yet')
    RETURN
  END IF

  IF (isRhoInf) THEN
    !! b1b2Bathe 2nd order ver
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[WIP] not implemented yet')
    RETURN
  END IF

  IF (isTheta) THEN
    !! gammaBathe
    areal = 2.0_DFP - theta
    bParams(1) = 0.50_DFP / areal
    bParams(2) = bParams(1)
    bParams(3) = (1.0_DFP - theta) / areal
    RETURN
  END IF

END SUBROUTINE GetBParams

!----------------------------------------------------------------------------
!                                                               gammaBathe
!----------------------------------------------------------------------------

SUBROUTINE obj_gammaBatheA(obj, splitRatio)
  CLASS(CSDAlgoParam_), INTENT(INOUT) :: obj
  REAL(DFP), OPTIONAL, INTENT(IN) :: splitRatio
  !! Default is 0.5
  REAL(DFP) :: bParams(3)

  ! internal varibales
  CHARACTER(*), PARAMETER :: myName = "obj_gammaBathe"
  REAL(DFP) :: splitRatio0

  CALL obj%DEALLOCATE()

  splitRatio0 = Input(default=0.5_DFP, option=splitRatio)

  IF (splitRatio0.approxeq.2.0_DFP) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] split ratio cannot be 2.0 in gamma Bathe')
    RETURN
  END IF

  obj%splitRatio = splitRatio0

  ! obj%algoParams(1)%NewmarkBeta(beta=0.25_DFP, gamma=0.50_DFP)
  !! sub-step 1
  !! equivalent to Trapezoidal rule
  obj%subAlgoParams(1)%tanmat(1) = 1.0_DFP
  obj%subAlgoParams(1)%tanmat(2) = splitRatio0 * 0.50_DFP
  obj%subAlgoParams(1)%tanmat(3) = splitRatio0**2 * 0.25_DFP

  obj%rhs_subf(1) = 1.0_DFP

  obj%subAlgoParams(1)%rhs_u1(3) = -1.0_DFP

  obj%subAlgoParams(1)%rhs_v1(2) = -1.0_DFP
  obj%subAlgoParams(1)%rhs_v1(3) = -splitRatio0

  obj%subAlgoParams(1)%rhs_a1(2) = -splitRatio0 * 0.50_DFP
  obj%subAlgoParams(1)%rhs_a1(3) = -splitRatio0**2 * 0.25_DFP

  !! sub-step 2
  !! equivalent to three point backward euler
  CALL GetBParams(bParams, theta=splitRatio0)
  obj%subAlgoParams(2)%tanmat(1) = 1.0_DFP
  obj%subAlgoParams(2)%tanmat(2) = bParams(3)
  obj%subAlgoParams(2)%tanmat(3) = bParams(3)**2

  obj%subAlgoParams(2)%rhs_f2 = 1.0_DFP

  obj%subAlgoParams(2)%rhs_u1(1) = 1.0_DFP
  obj%subAlgoParams(2)%rhs_u1(2) = bParams(3)

  obj%rhs_subsol(2, 2) = -bParams(2)
  obj%rhs_subsol(3, 2) = -bParams(2)*splitRatio0*0.50_DFP - bParams(2)*bParams(3)

  obj%subAlgoParams(2)%rhs_v1(2) = -1.0_DFP
  obj%subAlgoParams(2)%rhs_v1(3) = -(bParams(1) + bParams(2) + bParams(3))

  obj%subAlgoParams(2)%rhs_a1(2) = -bParams(1)
  obj%subAlgoParams(2)%rhs_a1(3) = -bParams(2) * splitRatio0 * 0.50_DFP  &
                              & - bParams(1) * bParams(3)

  obj%subAlgoParams(2)%dis(1) = 1.0_DFP
  obj%subAlgoParams(2)%dis(2) = -(bParams(1) + bParams(2) + bParams(3))
  obj%subAlgoParams(2)%dis(3) = bParams(2) * splitRatio0 * 0.50_DFP  &
                              & + bParams(1) * bParams(3)
  obj%subAlgoParams(2)%dis(4) = bParams(3)**2

  obj%dis_subsol = bParams(2) * splitRatio0 * 0.50_DFP  &
                & + bParams(2) * bParams(3)

  obj%subAlgoParams(2)%vel(2) = 1.0_DFP
  obj%subAlgoParams(2)%vel(3) = bParams(1)
  obj%subAlgoParams(2)%vel(4) = bParams(3)

  obj%vel_subsol = bParams(2)

  obj%subAlgoParams(2)%acc(4) = 1.0_DFP

  CALL obj%MakeZeros()

END SUBROUTINE obj_gammaBatheA

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE obj_gammaBatheU(obj, splitRatio)
  CLASS(CSDAlgoParam_), INTENT(INOUT) :: obj
  REAL(DFP), OPTIONAL, INTENT(IN) :: splitRatio
  !! Default is 0.5
  REAL(DFP) :: bParams(3)

  ! internal varibales
  CHARACTER(*), PARAMETER :: myName = "obj_gammaBathe"
  REAL(DFP) :: theta, theta2, b0, b1, b2

  CALL obj%DEALLOCATE()

  theta = Input(default=0.5_DFP, option=splitRatio)

  IF (theta.approxeq.2.0_DFP) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[CONFIG ERROR] split ratio cannot be 2.0 in gamma Bathe')
    RETURN
  END IF

  obj%splitRatio = theta
  theta2 = theta * theta

  ! obj%algoParams(1)%NewmarkBeta(beta=0.25_DFP, gamma=0.50_DFP)
  !! sub-step 1
  !! equivalent to Trapezoidal rule
  obj%subAlgoParams(1)%tanmat(1) = 1.0_DFP
  obj%subAlgoParams(1)%tanmat(2) = theta * 0.50_DFP
  obj%subAlgoParams(1)%tanmat(3) = theta2 * 0.25_DFP

  obj%rhs_subf(1) = theta2 * 0.25_DFP

  obj%subAlgoParams(1)%rhs_u1(1) = 1.0_DFP
  obj%subAlgoParams(1)%rhs_u1(2) = 0.50_DFP * theta

  obj%subAlgoParams(1)%rhs_v1(1) = theta
  obj%subAlgoParams(1)%rhs_v1(2) = theta2 * 0.25_DFP

  obj%subAlgoParams(1)%rhs_a1(1) = theta2 * 0.25_DFP

  !! sub-step 2
  !! equivalent to three point backward euler
  CALL GetBParams(bParams, theta=theta)
  b0 = bParams(1)
  b1 = bParams(2)
  b2 = bParams(3)

  obj%subAlgoParams(2)%tanmat(1) = 1.0_DFP
  obj%subAlgoParams(2)%tanmat(2) = b2
  obj%subAlgoParams(2)%tanmat(3) = b2**2

  obj%subAlgoParams(2)%rhs_f2 = b2**2

  obj%subAlgoParams(2)%rhs_u1(1) = -2.0_DFP * b1 / theta  &
          & - 4.0_DFP * b1 * b2 / theta2 + 1.0_DFP
  obj%subAlgoParams(2)%rhs_u1(2) = -2.0_DFP * b1 * b2 / theta + b2

  obj%rhs_subsol(1, 2) = 2.0_DFP * b1 / theta + &
                      & 4.0_DFP * b1 * b2 / theta2
  obj%rhs_subsol(2, 2) = 2.0_DFP * b1 * b2 / theta

  obj%subAlgoParams(2)%rhs_v1(1) = -b0 + b1 + b2 -  &
                          & 4.0_DFP * b1 * b2 / theta
  obj%subAlgoParams(2)%rhs_v1(2) = -b2 * (b1 - b0)

  obj%subAlgoParams(2)%rhs_a1(1) = -b2 * (b1 - b0)

  obj%subAlgoParams(2)%acc(1) = 2.0_DFP * b1 / (b2**2 * theta)  &
                & - 1.0_DFP / b2**2 + 4.0_DFP * b1 / (b2 * theta2)
  obj%subAlgoParams(2)%acc(2) = (b1 - b0) / b2**2 - 1.0_DFP / b2  &
                & + 4.0_DFP * b1 / (b2 * theta)
  obj%subAlgoParams(2)%acc(3) = (b1 - b0) / b2
  obj%subAlgoParams(2)%acc(4) = 1.0_DFP / b2**2

  obj%acc_subsol = -2.0_DFP*b1 / (b2**2*theta) - 4.0_DFP*b1 /(b2*theta2)

  obj%subAlgoParams(2)%vel(1) = 2.0_DFP * b1 / (b2 * theta) - 1.0_DFP / b2
  obj%subAlgoParams(2)%vel(2) = (b1 - b0) / b2
  obj%subAlgoParams(2)%vel(3) = 0.0_DFP
  obj%subAlgoParams(2)%vel(4) = 1.0_DFP / b2

  obj%vel_subsol = -2.0_DFP * b1 / (b2 * theta)

  obj%subAlgoParams(2)%dis(4) = 1.0_DFP

  obj%dis_subsol = 0.0_DFP

  CALL obj%MakeZeros()

END SUBROUTINE obj_gammaBatheU

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

END SUBROUTINE obj_Deallocate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
!
END MODULE CSDAlgorithms
