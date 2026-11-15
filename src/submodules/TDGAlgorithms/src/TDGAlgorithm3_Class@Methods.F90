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

SUBMODULE(TDGAlgorithm3_Class) Methods
USE ApproxUtility, ONLY: OPERATOR(.approxeq.)
USE BaseType, ONLY: elemOpt => TypeElemNameOpt
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: quadOpt => TypeQuadratureOpt
USE Display_Method, ONLY: Display
USE ElemshapeData_Method, ONLY: Elemsd_Allocate => ALLOCATE
USE ElemshapeData_Method, ONLY: Elemsd_Initiate => Initiate
USE ElemshapeData_Method, ONLY: Elemsd_Set => Set
USE ElemshapeData_Method, ONLY: HierarchicalElemShapeData
USE ElemshapeData_Method, ONLY: LagrangeElemShapeData
USE ElemshapeData_Method, ONLY: OrthogonalElemShapeData
USE FEFactoryUtility, ONLY: OneDimFEFactory
USE InputUtility, ONLY: Input
USE Lapack_Method, ONLY: GetInvMat
USE MassMatrix_Method, ONLY: MassMatrix_
USE ProductUtility, ONLY: OuterProd_
USE QuadraturePoint_Method, ONLY: QuadPoint_Initiate => Initiate
USE QuadraturePoint_Method, ONLY: Quad_Display => Display
USE QuadraturePoint_Method, ONLY: Quad_Size => Size

IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "TDGAlgorithm3_Class@Methods.F90"
#endif

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

INTEGER(I4B) :: ii, jj, nns, nips

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isInit = math%yes
nips = elemsd%nips
nns = elemsd%nns

obj%nrow = nns
obj%ncol = nns

! make kt
CALL MassMatrix_(test=elemsd, trial=elemsd, ans=obj%kt, nrow=ii, &
                 ncol=jj)

! Calculate inverse of kt
CALL GetInvMat(A=obj%kt(1:nns, 1:nns), invA=obj%invKt(1:nns, 1:nns))

obj%forceCoeff(1:nns, 1:nns) = obj%invKt(1:nns, 1:nns)

! make mt: part 1: (T, dT/dt)_In
obj%mt(1:nns, 1:nns) = math%zero
CALL MassMatrix_(N=elemsd%N, &
                 M=elemsd%dNdXt(:, 1, :), &
                 js=elemsd%js, ws=elemsd%ws, thickness=elemsd%thickness, &
                 nips=elemsd%nips, nns1=elemsd%nns, nns2=elemsd%nns, &
                 ans=obj%mt, nrow=ii, ncol=jj)

! make mt: part 2: Tn x Tn jump contribution
CALL OuterProd_(a=facetElemsd%N(1:nns, 1), &
                b=facetElemsd%N(1:nns, 1), &
                ans=obj%mt, nrow=ii, ncol=jj, &
                scale=math%one, anscoeff=math%one)

! make stiffMatCoeff
obj%stiffMatCoeff(1:nns, 1:nns) = math%zero
DO ii = 1, nns
  obj%stiffMatCoeff(ii, ii) = math%one
END DO

! make dampMatCoeff
obj%dampMatCoeff(1:nns, 1:nns) = MATMUL(obj%invKt(1:nns, 1:nns), &
                                        obj%mt(1:nns, 1:nns))

! make massMatCoeff
obj%massMatCoeff(1:nns, 1:nns) = MATMUL(obj%dampMatCoeff(1:nns, 1:nns), &
                                        obj%dampMatCoeff(1:nns, 1:nns))

! make rhs coeff for mass matrix: coeff for MU0, MV0, MA0
! MV0
obj%rhs_m_v1(1:nns) = MATMUL(obj%invKt(1:nns, 1:nns), facetElemsd%N(1:nns, 1))

! MU0
obj%rhs_m_a1(1:nns) = MATMUL(obj%mt(1:nns, 1:nns), obj%rhs_m_v1(1:nns))
obj%rhs_m_u1(1:nns) = MATMUL(obj%invKt(1:nns, 1:nns), obj%rhs_m_a1(1:nns))

! MA0
obj%rhs_m_a1(1:nns) = math%zero

! make rhs coeff for damping matrix: coeff for CU0, CV0, CA0
! CU0
obj%rhs_c_u1(1:nns) = obj%rhs_m_v1(1:nns)
! CV0
obj%rhs_c_v1(1:nns) = math%zero
! CA0
obj%rhs_c_a1(1:nns) = math%zero

! make rhs coeff for stiffness matrix: coeff for KU0, KV0, KA0
! KU0
obj%rhs_k_u1(1:nns) = math%zero

! KV0
obj%rhs_k_v1(1:nns) = math%zero

! KA0
obj%rhs_k_a1(1:nns) = math%zero

! make coeff for updating velocity
obj%vel(1) = -DOT_PRODUCT(obj%rhs_m_v1(1:nns), facetElemsd%N(1:nns, 2))
obj%vel(2:3) = math%zero
obj%vel(4:nns + 3) = MATMUL(facetElemsd%N(1:nns, 2), &
                            obj%dampMatCoeff(1:nns, 1:nns))

obj%acc(1) = -DOT_PRODUCT(obj%rhs_m_v1(1:nns), facetElemsd%dNdXt(1:nns, 1, 2))
obj%acc(2:3) = math%zero
obj%acc(4:nns + 3) = MATMUL(facetElemsd%dNdXt(1:nns, 1, 2), &
                            obj%dampMatCoeff(1:nns, 1:nns))

! make coeff for updating displacement
obj%dis(1:3) = math%zero
obj%dis(4:3 + nns) = facetElemsd%N(1:nns, 2)

obj%jumpDis(1:nns + 3) = math%zero
obj%jumpVel(1:nns + 3) = math%zero

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
obj%jumpDis_zero = obj%jumpDis.approxeq.myzero
obj%jumpVel_zero = obj%jumpVel.approxeq.myzero
obj%dis_zero = obj%dis.approxeq.myzero
obj%vel_zero = obj%vel.approxeq.myzero
obj%acc_zero = obj%acc.approxeq.myzero
obj%rhs_m_u1_zero = obj%rhs_m_u1.approxeq.myzero
obj%rhs_m_v1_zero = obj%rhs_m_v1.approxeq.myzero
obj%rhs_m_a1_zero = obj%rhs_m_a1.approxeq.myzero
obj%rhs_c_u1_zero = obj%rhs_c_u1.approxeq.myzero
obj%rhs_c_v1_zero = obj%rhs_c_v1.approxeq.myzero
obj%rhs_c_a1_zero = obj%rhs_c_a1.approxeq.myzero
obj%rhs_k_u1_zero = obj%rhs_k_u1.approxeq.myzero
obj%rhs_k_v1_zero = obj%rhs_k_v1.approxeq.myzero
obj%rhs_k_a1_zero = obj%rhs_k_a1.approxeq.myzero

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

obj%isInit = math%no
obj%nrow = math%zero_i
obj%ncol = math%zero_i
obj%initialGuess = math%zero
obj%initialGuess_zero = math%yes
obj%jumpDis = math%zero
obj%jumpDis_zero = math%yes
obj%jumpVel = math%zero
obj%jumpVel_zero = math%yes
obj%dis = math%zero
obj%dis_zero = math%yes
obj%vel = math%zero
obj%vel_zero = math%yes
obj%acc = math%zero
obj%acc_zero = math%yes
obj%mt = math%zero
obj%kt = math%zero
obj%massMatCoeff = math%zero
obj%dampMatCoeff = math%zero
obj%stiffMatCoeff = math%zero
obj%rhs_m_u1 = math%zero
obj%rhs_m_v1 = math%zero
obj%rhs_m_a1 = math%zero
obj%rhs_c_u1 = math%zero
obj%rhs_c_v1 = math%zero
obj%rhs_c_a1 = math%zero
obj%rhs_k_u1 = math%zero
obj%rhs_k_v1 = math%zero
obj%rhs_k_a1 = math%zero
obj%rhs_m_u1_zero = math%yes
obj%rhs_m_v1_zero = math%yes
obj%rhs_m_a1_zero = math%yes
obj%rhs_c_u1_zero = math%yes
obj%rhs_c_v1_zero = math%yes
obj%rhs_c_a1_zero = math%yes
obj%rhs_k_u1_zero = math%yes
obj%rhs_k_v1_zero = math%yes
obj%rhs_k_a1_zero = math%yes

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                    Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display()"
#endif

INTEGER(I4B) :: nns

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Display(msg, unitno=unitno)
CALL Display(obj%isInit, "isInit: ", unitno=unitno)

IF (.NOT. obj%isInit) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

CALL Display(obj%nrow, "nrow: ", unitno=unitno)
CALL Display(obj%ncol, "ncol: ", unitno=unitno)

nns = obj%nrow

CALL Display(obj%initialGuess(1:nns + 3), "initialGuess: ", &
             unitno=unitno, advance="NO")
CALL Display(obj%initialGuess_zero(1:nns + 3), "initialGuess_zero: ", &
             unitno=unitno)

CALL Display(obj%jumpDis(1:nns + 3), "jumpDis: ", unitno=unitno, &
             advance="NO")
CALL Display(obj%jumpDis_zero(1:nns + 3), "jumpDis_zero: ", unitno=unitno)

CALL Display(obj%jumpVel(1:nns + 3), "jumpVel: ", unitno=unitno, &
             advance="NO")
CALL Display(obj%jumpVel_zero(1:nns + 3), "jumpVel_zero: ", unitno=unitno)

CALL Display(obj%dis(1:nns + 3), "dis: ", unitno=unitno, &
             advance="NO")
CALL Display(obj%dis_zero(1:nns + 3), "dis_zero: ", unitno=unitno)

CALL Display(obj%vel(1:nns + 3), "vel: ", unitno=unitno, advance="NO")
CALL Display(obj%vel_zero(1:nns + 3), "vel_zero: ", unitno=unitno)

CALL Display(obj%acc(1:nns + 3), "acc: ", unitno=unitno, advance="NO")
CALL Display(obj%acc_zero(1:nns + 3), "acc_zero: ", unitno=unitno)

CALL Display(obj%mt(1:nns, 1:nns), "mt: ", unitno=unitno)
CALL Display(obj%kt(1:nns, 1:nns), "kt: ", unitno=unitno)
CALL Display(obj%invKt(1:nns, 1:nns), "invKt: ", unitno=unitno)

CALL Display(obj%massMatCoeff(1:nns, 1:nns), "massMatCoeff: ", &
             unitno=unitno)
CALL Display(obj%dampMatCoeff(1:nns, 1:nns), "dampMatCoeff: ", &
             unitno=unitno)
CALL Display(obj%stiffMatCoeff(1:nns, 1:nns), "stiffMatCoeff: ", &
             unitno=unitno)

CALL Display(obj%rhs_m_u1(1:nns), "rhs_m_u1: ", unitno=unitno, advance="NO")
CALL Display(obj%rhs_m_u1_zero(1:nns), "rhs_m_u1_zero: ", unitno=unitno)

CALL Display(obj%rhs_m_v1(1:nns), "rhs_m_v1: ", unitno=unitno, advance="NO")
CALL Display(obj%rhs_m_v1_zero(1:nns), "rhs_m_v1_zero: ", unitno=unitno)

CALL Display(obj%rhs_m_a1(1:nns), "rhs_m_a1: ", unitno=unitno, advance="NO")
CALL Display(obj%rhs_m_a1_zero(1:nns), "rhs_m_a1_zero: ", unitno=unitno)

CALL Display(obj%rhs_c_u1(1:nns), "rhs_c_u1: ", unitno=unitno, advance="NO")
CALL Display(obj%rhs_c_u1_zero(1:nns), "rhs_c_u1_zero: ", unitno=unitno)

CALL Display(obj%rhs_c_v1(1:nns), "rhs_c_v1: ", unitno=unitno, advance="NO")
CALL Display(obj%rhs_c_v1_zero(1:nns), "rhs_c_v1_zero: ", unitno=unitno)

CALL Display(obj%rhs_c_a1(1:nns), "rhs_c_a1: ", unitno=unitno, advance="NO")
CALL Display(obj%rhs_c_a1_zero(1:nns), "rhs_c_a1_zero: ", unitno=unitno)

CALL Display(obj%rhs_k_u1(1:nns), "rhs_k_u1: ", unitno=unitno, advance="NO")
CALL Display(obj%rhs_k_u1_zero(1:nns), "rhs_k_u1_zero: ", unitno=unitno)

CALL Display(obj%rhs_k_v1(1:nns), "rhs_k_v1: ", unitno=unitno, advance="NO")
CALL Display(obj%rhs_k_v1_zero(1:nns), "rhs_k_v1_zero: ", unitno=unitno)

CALL Display(obj%rhs_k_a1(1:nns), "rhs_k_a1: ", unitno=unitno, advance="NO")
CALL Display(obj%rhs_k_a1_zero(1:nns), "rhs_k_a1_zero: ", unitno=unitno)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                             Include errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods
