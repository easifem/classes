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

SUBMODULE(CSDAlgorithm2_Class) ConstructorMethods

USE ApproxUtility, ONLY: OPERATOR(.APPROXEQ.)

USE Display_Method, ONLY: Display

USE InputUtility, ONLY: Input

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                       GetBParams_gammaBathe
!----------------------------------------------------------------------------

SUBROUTINE GetBParams_gammaBathe(bParams, gamma)
  REAL(DFP), INTENT(INOUT) :: bParams(3)
  REAL(DFP), OPTIONAL, INTENT(IN) :: gamma

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetBParams_gammaBathe"
  LOGICAL(LGT) :: isok
#endif

  REAL(DFP) :: gamma0, areal

  gamma0 = Input(default=math%half, option=gamma)

#ifdef DEBUG_VER
  isok = .NOT. (gamma0.approxeq.math%two)
  CALL AssertError1(isok, myName, "gamma = 2.0 is not allowed")
#endif

  areal = math%two - gamma0
  bParams(1) = math%half / areal
  bParams(2) = bParams(1)
  bParams(3) = (math%one - gamma0) / areal

END SUBROUTINE GetBParams_gammaBathe

!----------------------------------------------------------------------------
!                                                        GetBParams_betaBathe
!----------------------------------------------------------------------------

SUBROUTINE GetBParams_betaBathe(bParams, gamma, beta1, beta2)
  REAL(DFP), INTENT(INOUT) :: bParams(3)
  REAL(DFP), OPTIONAL, INTENT(in) :: gamma
  REAL(DFP), OPTIONAL, INTENT(in) :: beta1
  REAL(DFP), OPTIONAL, INTENT(in) :: beta2

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetBParams_betaBathe"
#endif
  REAL(DFP) :: gamma0, beta1_0, beta2_0

  gamma0 = Input(default=math%half, option=gamma)
  beta1_0 = Input(default=math%one / 3.0_DFP, option=beta1)
  beta2_0 = Input(default=math%one - beta1_0, option=beta2)

  bParams(1) = (math%one - beta1_0) * gamma0
  bParams(2) = (math%one - beta2_0) * (math%one - gamma0) + beta1_0 * gamma0
  bParams(3) = (math%one - gamma0) * beta2_0

END SUBROUTINE GetBParams_betaBathe

!----------------------------------------------------------------------------
!                                                        GetBParams_rhoBathe
!----------------------------------------------------------------------------

SUBROUTINE GetBParams_rhoBathe(bParams, gamma, rhoInf)
  REAL(DFP), INTENT(INOUT) :: bParams(3)
  REAL(DFP), OPTIONAL, INTENT(in) :: gamma
  REAL(DFP), OPTIONAL, INTENT(in) :: rhoInf

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetBParams_rhoBathe"
  LOGICAL(LGT) :: isok
#endif
  REAL(DFP) :: gamma0, rhoInf0, areal

  gamma0 = Input(default=math%half, option=gamma)
  rhoInf0 = Input(default=math%zero, option=rhoInf)

#ifdef DEBUG_VER
  isok = rhoInf0 .GE. math%zero
  CALL AssertError1(isok, myName, &
                    "asymptotic spectral radius must be non-negative")
#endif

  areal = math%two * gamma0 * (rhoInf0 - math%one) + 4.0_DFP
  bParams(2) = (rhoInf0 + math%one) / areal
  bParams(1) = math%half + (gamma0 - math%one) * bParams(2)
  bParams(3) = math%half - gamma0 * bParams(2)

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

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "GetBParams_master"
  LOGICAL(LGT) :: abool
#endif
  LOGICAL(LGT) :: isGamma, isRhoInf, isBeta1, isBeta2, problem

  isGamma = PRESENT(gamma)
  isRhoInf = PRESENT(rhoInf)
  isBeta1 = PRESENT(beta1)
  isBeta2 = PRESENT(beta2)

#ifdef DEBUG_VER
  abool = isGamma .AND. isRhoInf .AND. isBeta1 .AND. isBeta2
  CALL AssertError1(.NOT. abool, myName, &
                    "too much parameters are passed")
#endif

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

#ifdef DEBUG_VER
  CALL AssertError1(.FALSE., myName, &
                    "No method found")
#endif

END SUBROUTINE GetBParams_master

!----------------------------------------------------------------------------
!                                                               Bathe_master
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_BatheMethod
REAL(DFP) :: bParams(3)

! internal varibales
CHARACTER(*), PARAMETER :: myName = "obj_Bathe_master"
REAL(DFP) :: gamma0, gamma2, b0, b1, b2

CALL obj%DEALLOCATE()

obj%totalSubstep = math%two_i

ALLOCATE (obj%substeps(1)%ptr)
ALLOCATE (obj%substeps(2)%ptr)

gamma0 = Input(default=0.5_DFP, option=gamma)

obj%splitRatios(1) = gamma0
obj%splitRatios(2) = math%one
gamma2 = gamma0 * gamma0

  !! sub-step 1
  !! equivalent to Trapezoidal rule
obj%substeps(1)%ptr%tanmat(1) = 1.0_DFP
obj%substeps(1)%ptr%tanmat(2) = gamma0 * 0.50_DFP
obj%substeps(1)%ptr%tanmat(3) = gamma2 * 0.25_DFP

obj%rhs_subf(1) = gamma2 * 0.25_DFP

obj%substeps(1)%ptr%rhs_u1(1) = 1.0_DFP
obj%substeps(1)%ptr%rhs_u1(2) = 0.50_DFP * gamma0

obj%substeps(1)%ptr%rhs_v1(1) = gamma0
obj%substeps(1)%ptr%rhs_v1(2) = gamma2 * 0.25_DFP

obj%substeps(1)%ptr%rhs_a1(1) = gamma2 * 0.25_DFP

  !! sub-step 2
CALL GetBParams_master(bParams, gamma=gamma0, rhoInf=rhoInf, &
                      & beta1=beta1, beta2=beta2)
b0 = bParams(1)
b1 = bParams(2)
b2 = bParams(3)

obj%substeps(2)%ptr%tanmat(1) = 1.0_DFP
obj%substeps(2)%ptr%tanmat(2) = b2
obj%substeps(2)%ptr%tanmat(3) = b2**2

obj%substeps(2)%ptr%rhs_f2 = b2**2

obj%substeps(2)%ptr%rhs_u1(1) = -2.0_DFP * b1 / gamma0  &
        & - 4.0_DFP * b1 * b2 / gamma2 + 1.0_DFP
obj%substeps(2)%ptr%rhs_u1(2) = -2.0_DFP * b1 * b2 / gamma0 + b2

obj%rhs_subsol(1, 2) = 2.0_DFP * b1 / gamma0 + &
                    & 4.0_DFP * b1 * b2 / gamma2
obj%rhs_subsol(2, 2) = 2.0_DFP * b1 * b2 / gamma0

obj%substeps(2)%ptr%rhs_v1(1) = -b0 + b1 + b2 -  &
                        & 4.0_DFP * b1 * b2 / gamma0
obj%substeps(2)%ptr%rhs_v1(2) = -b2 * (b1 - b0)

obj%substeps(2)%ptr%rhs_a1(1) = -b2 * (b1 - b0)

obj%substeps(2)%ptr%acc(1) = 2.0_DFP * b1 / (b2**2 * gamma0)  &
              & - 1.0_DFP / b2**2 + 4.0_DFP * b1 / (b2 * gamma2)
obj%substeps(2)%ptr%acc(2) = (b1 - b0) / b2**2 - 1.0_DFP / b2  &
              & + 4.0_DFP * b1 / (b2 * gamma0)
obj%substeps(2)%ptr%acc(3) = (b1 - b0) / b2
obj%substeps(2)%ptr%acc(4) = 1.0_DFP / b2**2

obj%acc_subsol = -2.0_DFP*b1 / (b2**2*gamma0) - 4.0_DFP*b1 /(b2*gamma2)

obj%substeps(2)%ptr%vel(1) = 2.0_DFP * b1 / (b2 * gamma0) - 1.0_DFP / b2
obj%substeps(2)%ptr%vel(2) = (b1 - b0) / b2
obj%substeps(2)%ptr%vel(3) = 0.0_DFP
obj%substeps(2)%ptr%vel(4) = 1.0_DFP / b2

obj%vel_subsol = -2.0_DFP * b1 / (b2 * gamma0)

obj%substeps(2)%ptr%dis(4) = 1.0_DFP

obj%dis_subsol = 0.0_DFP

CALL obj%MakeZeros()

END PROCEDURE obj_BatheMethod

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_MakeZeros
INTEGER(I4B) :: ii

DO ii = 1, SIZE(obj%substeps)
  CALL obj%substeps(ii)%ptr%MakeZeros()
END DO

obj%rhs_subsol_zero = obj%rhs_subsol.approxeq.math%zero
obj%rhs_subf_zero = obj%rhs_subf.approxeq.math%zero
obj%dis_subsol_zero = obj%dis_subsol.approxeq.math%zero
obj%vel_subsol_zero = obj%vel_subsol.approxeq.math%zero
obj%acc_subsol_zero = obj%acc_subsol.approxeq.math%zero

END PROCEDURE obj_MakeZeros

!----------------------------------------------------------------------------
!                                                                 Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
INTEGER(I4B) :: ii

DO ii = 1, SIZE(obj%substeps)
  IF (ASSOCIATED(obj%substeps(ii)%ptr)) THEN
    CALL obj%substeps(ii)%ptr%DEALLOCATE()
  END IF
END DO

obj%splitRatios = math%zero

obj%rhs_subsol = math%zero
obj%rhs_subf = math%zero

obj%rhs_subsol_zero = .TRUE.
obj%rhs_subf_zero = .TRUE.

obj%dis_subsol = math%zero
obj%vel_subsol = math%zero
obj%acc_subsol = math%zero

obj%dis_subsol_zero = .TRUE.
obj%vel_subsol_zero = .TRUE.
obj%acc_subsol_zero = .TRUE.

obj%singleStep = .FALSE.

obj%totalSubstep = math%zero_i
obj%currentSubstep = math%one_i

END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalSubstep
ans = obj%totalSubstep
END PROCEDURE obj_GetTotalSubstep

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetCurrentSubstep
ans = obj%currentSubstep
END PROCEDURE obj_GetCurrentSubstep

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetSubstepSize
ans = dt * obj%splitRatios(substepIndex)
END PROCEDURE obj_GetSubstepSize

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_UpdateCurrentSubstep

IF (obj%currentSubstep .EQ. obj%totalSubstep) THEN
  obj%currentSubstep = 1
ELSE
  obj%currentSubstep = obj%currentSubstep + 1
END IF

END PROCEDURE obj_UpdateCurrentSubstep

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
