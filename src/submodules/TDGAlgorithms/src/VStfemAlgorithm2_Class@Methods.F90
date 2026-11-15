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

SUBMODULE(VStfemAlgorithm2_Class) Methods
USE ApproxUtility, ONLY: OPERATOR(.APPROXEQ.)
USE BaseType, ONLY: math => TypeMathOpt
USE Display_Method, ONLY: Display
USE InputUtility, ONLY: Input
USE Lapack_Method, ONLY: GetInvMat
USE MassMatrix_Method, ONLY: MassMatrix_
USE ProductUtility, ONLY: OuterProd_
USE ReallocateUtility, ONLY: Reallocate
USE StringUtility, ONLY: UpperCase

IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "VStfemAlgorithm2_Class@Methods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                    Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate()"
#endif

INTEGER(I4B) :: nnt

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isInit = math%yes
nnt = elemsd%nns
obj%nrow = nnt
obj%ncol = nnt
CALL obj%AllocateData(nnt=nnt)

IF (PRESENT(scalingOpt)) THEN
  obj%scalingOpt = UpperCase(scalingOpt)
END IF

SELECT CASE (obj%scalingOpt)
CASE ("K")
  CALL InitiateVStfem_kt(obj=obj, elemsd=elemsd, facetElemsd=facetElemsd)
CASE ("I")
  CALL InitiateVStfem_It(obj=obj, elemsd=elemsd, facetElemsd=facetElemsd)
CASE DEFAULT
  CALL InitiateVStfem_mt(obj=obj, elemsd=elemsd, facetElemsd=facetElemsd)
END SELECT

CALL obj%MakeZeros()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                          InitiateVStfem_It
!----------------------------------------------------------------------------

SUBROUTINE InitiateVStfem_It(obj, elemsd, facetElemsd)
  CLASS(VStfemAlgorithm2_), INTENT(INOUT) :: obj
  TYPE(ElemShapeData_), INTENT(IN) :: elemsd, facetElemsd

! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "InitiateVStfem_It()"
#endif
  INTEGER(I4B) :: ii, jj, nnt, nips
  REAL(DFP), ALLOCATABLE :: tempVec(:)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  nips = elemsd%nips
  nnt = elemsd%nns

  CALL Reallocate(tempVec, nnt)

  ! make kt
  obj%kt = math%zero
  CALL MassMatrix_(N=elemsd%N, M=elemsd%N, js=elemsd%js, ws=elemsd%ws, &
                   thickness=elemsd%thickness, nips=nips, nns1=nnt, &
                   nns2=nnt, ans=obj%kt, nrow=ii, ncol=jj)

  ! make mt: part 1: (T, dT/dt)_In
  obj%mt = math%zero
  CALL MassMatrix_(N=elemsd%N, M=elemsd%dNdXt(:, 1, :), &
                   js=elemsd%js, ws=elemsd%ws, thickness=elemsd%thickness, &
                   nips=nips, nns1=nnt, nns2=nnt, ans=obj%mt, nrow=ii, &
                   ncol=jj)

  ! make obj%mt: part 2: Tn x Tn jump contribution
  CALL OuterProd_(a=facetElemsd%N(1:nnt, 1), &
                  b=facetElemsd%N(1:nnt, 1), &
                  ans=obj%mt, nrow=ii, ncol=jj, &
                  scale=math%one, anscoeff=math%one)

  ! make mt_inv
  CALL GetInvMat(A=obj%mt, invA=obj%mt_inv)

  ! make kt_mt_inv
  obj%kt_mt_inv = MATMUL(obj%kt, obj%mt_inv)

  ! make forceCoeff
  obj%forceCoeff = math%zero
  DO ii = 1, nnt
    obj%forceCoeff(ii, ii) = math%one
  END DO

  ! make massMatCoeff:
  obj%massMatCoeff = obj%mt

  ! make dampMatCoeff
  obj%dampMatCoeff = obj%kt

  ! make stiffMatCoeff
  obj%stiffMatCoeff = MATMUL(obj%kt_mt_inv, obj%kt)

  ! make rhs coeff for mass matrix: coeff for MU0, MV0, MA0
  ! MV0
  obj%rhs_m_v1 = facetElemsd%N(1:nnt, 1)

  ! MU0
  obj%rhs_m_u1 = math%zero

  ! CU0
  obj%rhs_c_u1 = math%zero

  ! CV0
  obj%rhs_c_v1 = math%zero

  ! make rhs coeff for stiffness matrix: coeff for KU0, KV0, KA0
  ! KU0
  obj%rhs_k_u1 = math%minus_one * MATMUL(obj%kt_mt_inv, &
                                         facetElemsd%N(1:nnt, 1))

  ! KV0
  obj%rhs_k_v1(1:nnt) = math%zero

  !-----------------------------------------------------------------
  ! Updating solutions
  !-----------------------------------------------------------------

  CALL MakeUpdateCoeffs(obj=obj, facetElemsd=facetElemsd, nnt=nnt, &
                        tempVec=tempVec)

  !-----------------------------------------------------------------
  ! Deallocate data
  !-----------------------------------------------------------------

  DEALLOCATE (tempVec)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE InitiateVStfem_It

!----------------------------------------------------------------------------
!                                                           InitiateVStfem_kt
!----------------------------------------------------------------------------

SUBROUTINE InitiateVStfem_kt(obj, elemsd, facetElemsd)
  CLASS(VStfemAlgorithm2_), INTENT(INOUT) :: obj
  TYPE(ElemShapeData_), INTENT(IN) :: elemsd, facetElemsd

! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "InitiateVStfem_kt()"
#endif
  INTEGER(I4B) :: ii, jj, nnt, nips
  REAL(DFP), ALLOCATABLE, DIMENSION(:, :) :: kt_inv_mt_kt_inv
  REAL(DFP), ALLOCATABLE :: tempVec(:)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  nips = elemsd%nips
  nnt = elemsd%nns

  CALL Reallocate(kt_inv_mt_kt_inv, nnt, nnt)
  CALL Reallocate(tempVec, nnt)

  ! make kt
  obj%kt = math%zero
  CALL MassMatrix_(N=elemsd%N, M=elemsd%N, js=elemsd%js, ws=elemsd%ws, &
                   thickness=elemsd%thickness, nips=nips, nns1=nnt, &
                   nns2=nnt, ans=obj%kt, nrow=ii, ncol=jj)

  ! make mt: part 1: (T, dT/dt)_In
  obj%mt = math%zero
  CALL MassMatrix_(N=elemsd%N, M=elemsd%dNdXt(:, 1, :), &
                   js=elemsd%js, ws=elemsd%ws, thickness=elemsd%thickness, &
                   nips=nips, nns1=nnt, nns2=nnt, ans=obj%mt, nrow=ii, &
                   ncol=jj)

  ! make mt: part 2: Tn x Tn jump contribution
  CALL OuterProd_(a=facetElemsd%N(1:nnt, 1), &
                  b=facetElemsd%N(1:nnt, 1), &
                  ans=obj%mt, nrow=ii, ncol=jj, &
                  scale=math%one, anscoeff=math%one)

  ! Calculate inverse of kt
  CALL GetInvMat(A=obj%kt, invA=obj%kt_inv)

  ! Calculate inverse of mt
  CALL GetInvMat(A=obj%mt, invA=obj%mt_inv)

  ! make kt_inv_mt
  obj%kt_inv_mt = MATMUL(obj%kt_inv, obj%mt)

  ! make kt_inv_mt_kt_inv
  kt_inv_mt_kt_inv = MATMUL(obj%kt_inv_mt, obj%kt_inv)

  obj%forceCoeff = obj%kt_inv

  ! make stiffMatCoeff
  obj%stiffMatCoeff = math%zero
  DO ii = 1, nnt
    obj%stiffMatCoeff(ii, ii) = math%one
  END DO

  ! make dampMatCoeff
  obj%dampMatCoeff = obj%kt_inv_mt

  ! make massMatCoeff: mt * kt_inv * mt
  obj%massMatCoeff = MATMUL(kt_inv_mt_kt_inv, obj%mt)

  ! make rhs coeff for mass matrix: coeff for MU0, MV0, MA0
  ! MV0
  obj%rhs_m_v1 = MATMUL(kt_inv_mt_kt_inv, facetElemsd%N(1:nnt, 1))

  ! MU0
  obj%rhs_m_u1 = math%zero

  ! make rhs coeff for damping matrix: coeff for CU0, CV0, CA0
  ! CU0
  obj%rhs_c_u1 = math%zero

  ! CV0
  obj%rhs_c_v1 = math%zero

  ! make rhs coeff for stiffness matrix: coeff for KU0, KV0, KA0
  ! KU0
  obj%rhs_k_u1 = math%minus_one * MATMUL(obj%kt_inv, facetElemsd%N(1:nnt, 1))

  ! KV0
  obj%rhs_k_v1(1:nnt) = math%zero

  !-----------------------------------------------------------------
  ! Updating solutions
  !-----------------------------------------------------------------

  CALL MakeUpdateCoeffs(obj=obj, facetElemsd=facetElemsd, nnt=nnt, &
                        tempVec=tempVec)

  !-----------------------------------------------------------------
  ! Deallocate data
  !-----------------------------------------------------------------

  DEALLOCATE (kt_inv_mt_kt_inv, tempVec)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE InitiateVStfem_kt

!----------------------------------------------------------------------------
!                                                          InitiateVStfem_mt
!----------------------------------------------------------------------------

SUBROUTINE InitiateVStfem_mt(obj, elemsd, facetElemsd)
  CLASS(VStfemAlgorithm2_), INTENT(INOUT) :: obj
  TYPE(ElemShapeData_), INTENT(IN) :: elemsd, facetElemsd

! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "InitiateVStfem_mt()"
#endif
  INTEGER(I4B) :: ii, jj, nnt, nips
  REAL(DFP), ALLOCATABLE, DIMENSION(:, :) :: mt_inv_kt_mt_inv
  REAL(DFP), ALLOCATABLE :: tempVec(:)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  nips = elemsd%nips
  nnt = elemsd%nns

  CALL Reallocate(mt_inv_kt_mt_inv, nnt, nnt)
  CALL Reallocate(tempVec, nnt)

  ! make kt
  obj%kt = math%zero
  CALL MassMatrix_(N=elemsd%N, M=elemsd%N, js=elemsd%js, ws=elemsd%ws, &
                   thickness=elemsd%thickness, nips=nips, nns1=nnt, &
                   nns2=nnt, ans=obj%kt, nrow=ii, ncol=jj)

  ! make mt: part 1: (T, dT/dt)_In
  obj%mt = math%zero
  CALL MassMatrix_(N=elemsd%N, M=elemsd%dNdXt(:, 1, :), &
                   js=elemsd%js, ws=elemsd%ws, thickness=elemsd%thickness, &
                   nips=nips, nns1=nnt, nns2=nnt, ans=obj%mt, nrow=ii, &
                   ncol=jj)

  ! make obj%mt: part 2: Tn x Tn jump contribution
  CALL OuterProd_(a=facetElemsd%N(1:nnt, 1), &
                  b=facetElemsd%N(1:nnt, 1), &
                  ans=obj%mt, nrow=ii, ncol=jj, &
                  scale=math%one, anscoeff=math%one)

  ! make mt_inv
  CALL GetInvMat(A=obj%mt, invA=obj%mt_inv)

  ! make mt_inv_kt
  obj%mt_inv_kt = MATMUL(obj%mt_inv, obj%kt)

  ! make mt_inv_kt_mt_inv
  mt_inv_kt_mt_inv = MATMUL(obj%mt_inv_kt, obj%mt_inv)

  ! forceCoeff
  obj%forceCoeff = obj%mt_inv

  ! make massMatCoeff
  obj%massMatCoeff = math%zero
  DO ii = 1, nnt
    obj%massMatCoeff(ii, ii) = math%one
  END DO

  ! make dampMatCoeff
  obj%dampMatCoeff = obj%mt_inv_kt

  ! make stiffMatCoeff:
  obj%stiffMatCoeff = MATMUL(mt_inv_kt_mt_inv, obj%kt)

  ! make rhs coeff for mass matrix: coeff for MU0, MV0, MA0
  ! MV0
  obj%rhs_m_v1 = MATMUL(obj%mt_inv, facetElemsd%N(1:nnt, 1))

  ! MU0
  obj%rhs_m_u1 = math%zero

  ! make rhs coeff for damping matrix: coeff for CU0, CV0, CA0
  ! CU0
  obj%rhs_c_u1 = math%zero

  ! CV0
  obj%rhs_c_v1 = math%zero

  ! make rhs coeff for stiffness matrix: coeff for KU0, KV0, KA0
  ! KU0
  obj%rhs_k_u1 = math%minus_one * MATMUL(mt_inv_kt_mt_inv, &
                                         facetElemsd%N(1:nnt, 1))

  ! KV0
  obj%rhs_k_v1 = math%zero

  !-----------------------------------------------------------------
  ! Updating solutions
  !-----------------------------------------------------------------

  CALL MakeUpdateCoeffs(obj=obj, facetElemsd=facetElemsd, nnt=nnt, &
                        tempVec=tempVec)

  !-----------------------------------------------------------------
  ! Deallocate data
  !-----------------------------------------------------------------

  DEALLOCATE (mt_inv_kt_mt_inv, tempVec)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE InitiateVStfem_mt

!----------------------------------------------------------------------------
!                                                           MakeUpdateCoeffs
!----------------------------------------------------------------------------

SUBROUTINE MakeUpdateCoeffs(obj, facetElemsd, nnt, tempVec)
  TYPE(VStfemAlgorithm2_), INTENT(INOUT) :: obj
  TYPE(ElemShapeData_), INTENT(IN) :: facetElemsd
  INTEGER(I4B), INTENT(IN) :: nnt
  REAL(DFP), INTENT(INOUT) :: tempVec(:)

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "MakeUpdateCoeffs()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  ! make coeff for updating displacement
  obj%vel(1:3) = math%zero
  obj%vel(4:3 + nnt) = facetElemsd%N(1:nnt, 2)

  !-----------------------------------------------------------------
  ! Updating solutions
  !-----------------------------------------------------------------
  !
  ! make coeff for updating displacement, 1: u0, 2: v0, 3: a0
  ! update disp: coeff for u0:
  tempVec = MATMUL(obj%mt_inv, facetElemsd%N(1:nnt, 1))
  obj%dis(1) = DOT_PRODUCT(tempVec, facetElemsd%N(1:nnt, 2))
  ! update disp: coeff for v0 and a0 are zero
  obj%dis(2:3) = math%zero

  ! update disp: coeff for nnt solution
  !  Tn+1 * mt_inv * kt
  !     step1: using rhs_k_a1 as temp storage
  tempVec = MATMUL(facetElemsd%N(1:nnt, 2), obj%mt_inv)
  obj%dis(4:nnt + 3) = MATMUL(tempVec, obj%kt)

  ! update acc
  ! update acc: coeff for u0:
  tempVec = MATMUL(obj%mt_inv, facetElemsd%N(1:nnt, 1))
  obj%acc(1) = DOT_PRODUCT(tempVec, facetElemsd%dNdXt(1:nnt, 1, 2))
  !   step2: resetting rhs_k_a1 to original value

  ! update acc: coeff for v0 and a0 are zero
  obj%acc(2:3) = math%zero

  ! update acc: coeff for nnt solution
  obj%acc(4:nnt + 3) = facetElemsd%dNdXt(1:nnt, 1, 2)

  !     step2: reset rhs_k_a1
  tempVec = math%zero

  obj%jumpDis(1:nnt + 3) = math%zero
  obj%jumpVel(1:nnt + 3) = math%zero

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE MakeUpdateCoeffs

!----------------------------------------------------------------------------
!                                                             Include errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods
