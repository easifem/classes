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

SUBMODULE(TDGAlgorithm1_Class) ConstructorMethods
USE ApproxUtility, ONLY: OPERATOR(.approxeq.)
USE InputUtility, ONLY: Input

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                               NewmarkBeta
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AlphaMethod
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AlphaMethod()"
#endif

REAL(DFP) :: alpha0, alpha_inv

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()

alpha0 = Input(default=0.5_DFP, option=alpha)
alpha_inv = 1.0_DFP / alpha0

obj%tanmat(1) = 1.0_DFP ! coeff of M
obj%tanmat(2) = alpha0 ! coeff of K

obj%rhs_u1(1) = 1.0_DFP
obj%rhs_u1(2) = alpha0 - 1.0_DFP

obj%rhs_v1(1) = 0.0_DFP
obj%rhs_v1(2) = 0.0_DFP

obj%rhs_f1 = 1.0_DFP - alpha0
obj%rhs_f2 = alpha0

obj%dis(1:2) = 0.0_DFP
obj%dis(3) = 1.0_DFP

obj%vel(1) = -alpha_inv ! coeff of U_n
obj%vel(2) = -(1.0_DFP - alpha0) * alpha_inv ! coeff of V_n
obj%vel(3) = alpha_inv ! coeff of U_n+1

obj%initialGuess(1) = 1.0_DFP
obj%initialGuess(2) = 1.0_DFP - alpha0

CALL obj%MakeZeros()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AlphaMethod

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
obj%rhs_f1_zero = obj%rhs_f1.approxeq.myzero
obj%rhs_f2_zero = obj%rhs_f2.approxeq.myzero
obj%dis_zero = obj%dis.approxeq.myzero
obj%vel_zero = obj%vel.approxeq.myzero
obj%initialGuess_zero = obj%initialGuess.approxeq.myzero

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

obj%rhs_u1_zero = .TRUE.
obj%rhs_v1_zero = .TRUE.

obj%rhs_f1 = 0.0_DFP
obj%rhs_f1_zero = .TRUE.

obj%rhs_f2 = 0.0_DFP
obj%rhs_f2_zero = .TRUE.

obj%dis = 0.0_DFP
obj%vel = 0.0_DFP

obj%dis_zero = .TRUE.
obj%vel_zero = .TRUE.

obj%initialGuess = 0.0_DFP
obj%initialGuess_zero = .TRUE.

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
