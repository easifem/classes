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

SUBMODULE(SDAlgorithm2_Class) ConstructorMethods
USE ApproxUtility, ONLY: OPERATOR(.approxeq.)
USE InputUtility, ONLY: Input

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                               NewmarkBeta
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_NewmarkBetaMethod
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_NewmarkBetaMethod()"
#endif

REAL(DFP) :: beta_inv, gamma_inv, beta0, gamma0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

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

REAL(DFP) :: alpha0, beta0, gamma0, areal

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()

alpha0 = Input(default=-0.30_DFP, option=alpha)
areal = (1.0_DFP - alpha0)**2 * 0.25_DFP
beta0 = Input(default=areal, option=beta)
areal = (1.0_DFP - 2.0_DFP * alpha0) * 0.50_DFP
gamma0 = Input(default=areal, option=gamma)

obj%alpha = alpha0

CALL obj%NewmarkBetaMethod(beta=beta0, gamma=gamma0)

obj%tanmat(2) = obj%tanmat(2) * (1.0_DFP + alpha0)
obj%tanmat(3) = obj%tanmat(3) * (1.0_DFP + alpha0)

obj%rhs_u1(2) = obj%rhs_u1(2) * (1.0_DFP + alpha0)
obj%rhs_u1(3) = alpha0 * beta0

obj%rhs_v1(2) = obj%rhs_v1(2) * (1.0_DFP + alpha0)  &
                & + alpha0 * beta0

obj%rhs_a1(2) = obj%rhs_a1(2) * (1.0_DFP + alpha0)

CALL obj%MakeZeros()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_HHTAlphaMethod

!----------------------------------------------------------------------------
!                                                                Collocation
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_CollocationMethod
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_CollocationMethod()"
#endif

REAL(DFP) :: theta0, beta0, gamma0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()

beta0 = Input(default=1.0_DFP / 6.0_DFP, option=beta)
gamma0 = Input(default=0.50_DFP, option=gamma)
theta0 = Input(default=1.4_DFP, option=theta)

CALL obj%NewmarkBetaMethod(beta=beta0, gamma=gamma0)

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

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_CollocationMethod

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

REAL(DFP), PARAMETER :: myzero = 0.0_DFP

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%tanmat_zero = obj%tanmat.approxeq.myzero
obj%rhs_u1_zero = obj%rhs_u1.approxeq.myzero
obj%rhs_v1_zero = obj%rhs_v1.approxeq.myzero
obj%rhs_a1_zero = obj%rhs_a1.approxeq.myzero
obj%rhs_f1_zero = obj%rhs_f1.approxeq.myzero
obj%rhs_f2_zero = obj%rhs_f2.approxeq.myzero
obj%dis_zero = obj%dis.approxeq.myzero
obj%vel_zero = obj%vel.approxeq.myzero
obj%acc_zero = obj%acc.approxeq.myzero

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

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                             Include errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
