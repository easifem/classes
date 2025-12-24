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
USE MassMatrix_Method, ONLY: MassMatrix_
USE ProductUtility, ONLY: OuterProd_
USE BaseType, ONLY: math => TypeMathOpt

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 IsInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsInitiated
ans = obj%isInit
END PROCEDURE obj_IsInitiated

!----------------------------------------------------------------------------
!                                                                    Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate()"
#endif

INTEGER(I4B) :: i1, i2

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()

obj%isInit = .TRUE.

obj%name(1:4) = "TDG "
obj%nrow = elemsd%nns
obj%ncol = obj%nrow
CALL MassMatrix_(test=elemsd, trial=elemsd, ans=obj%kt, nrow=obj%nrow, &
                 ncol=obj%ncol)

CALL MassMatrix_(N=elemsd%N, M=elemsd%dNdXt(:, 1, :), &
                 js=elemsd%js, ws=elemsd%ws, thickness=elemsd%thickness, &
                 nips=elemsd%nips, nns1=elemsd%nns, nns2=elemsd%nns, &
                 ans=obj%mt, nrow=obj%nrow, ncol=obj%ncol)

CALL OuterProd_(a=facetElemsd%N(1:obj%nrow, 1), &
                b=facetElemsd%N(1:obj%nrow, 1), &
                ans=obj%mt, nrow=i1, ncol=i2, scale=math%one, &
                anscoeff=math%one)

DO i1 = 1, obj%nrow
  obj%dis(i1 + 2) = facetElemsd%N(i1, 2)
  obj%vel(i1 + 2) = facetElemsd%dNdXt(i1, 1, 2)
  obj%rhs_m_u1(i1) = facetElemsd%N(i1, 1)
END DO

CALL obj%MakeZeros()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate

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

obj%initialGuess_zero = obj%initialGuess.approxeq.myzero
obj%dis_zero = obj%dis.approxeq.myzero
obj%vel_zero = obj%vel.approxeq.myzero
obj%rhs_m_u1_zero = obj%rhs_m_u1.approxeq.myzero
obj%rhs_m_v1_zero = obj%rhs_m_v1.approxeq.myzero
obj%rhs_k_u1_zero = obj%rhs_k_u1.approxeq.myzero
obj%rhs_k_v1_zero = obj%rhs_k_v1.approxeq.myzero

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

obj%isInit = .FALSE.

obj%name = "TDG1"
obj%nrow = 0_I4B
obj%ncol = 0_I4B

obj%initialGuess = 0.0_DFP
obj%initialGuess_zero = .TRUE.

obj%dis = 0.0_DFP
obj%dis_zero = .TRUE.

obj%vel = 0.0_DFP
obj%vel_zero = .TRUE.

obj%mt = 0.0_DFP
obj%kt = 0.0_DFP

obj%rhs_m_u1 = 0.0_DFP
obj%rhs_m_v1 = 0.0_DFP
obj%rhs_k_u1 = 0.0_DFP
obj%rhs_k_v1 = 0.0_DFP

obj%rhs_m_u1_zero = .TRUE.
obj%rhs_m_v1_zero = .TRUE.
obj%rhs_k_u1_zero = .TRUE.
obj%rhs_k_v1_zero = .TRUE.

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
