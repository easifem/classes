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

SUBMODULE(VStfemAlgorithm_Class) Methods
USE ApproxUtility, ONLY: OPERATOR(.approxeq.)
USE BaseType, ONLY: math => TypeMathOpt
USE Display_Method, ONLY: Display
USE InputUtility, ONLY: Input
USE Lapack_Method, ONLY: GetInvMat
USE MassMatrix_Method, ONLY: MassMatrix_
USE ProductUtility, ONLY: OuterProd_
USE ReallocateUtility, ONLY: Reallocate

IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "VStfemAlgorithm_Class@Methods.F90"
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

SELECT CASE (scalingOpt)
CASE ("M")
  CALL InitiateVStfem_mt(obj=obj, elemsd=elemsd, facetElemsd=facetElemsd)
CASE ("I")
  CALL InitiateVStfem_It(obj=obj, elemsd=elemsd, facetElemsd=facetElemsd)
CASE DEFAULT
  CALL InitiateVStfem_kt(obj=obj, elemsd=elemsd, facetElemsd=facetElemsd)
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                               AllocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AllocateData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AllocateData()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Reallocate(obj%massMatCoeff, nnt, nnt)
CALL Reallocate(obj%dampMatCoeff, nnt, nnt)
CALL Reallocate(obj%stiffMatCoeff, nnt, nnt)
CALL Reallocate(obj%forceCoeff, nnt, nnt)

CALL Reallocate(obj%rhs_m_u1, nnt)
CALL Reallocate(obj%rhs_k_u1, nnt)
CALL Reallocate(obj%rhs_c_u1, nnt)
CALL Reallocate(obj%rhs_m_v1, nnt)
CALL Reallocate(obj%rhs_k_v1, nnt)
CALL Reallocate(obj%rhs_c_v1, nnt)

CALL Reallocate(obj%rhs_m_u1_zero, nnt)
CALL Reallocate(obj%rhs_k_u1_zero, nnt)
CALL Reallocate(obj%rhs_c_u1_zero, nnt)
CALL Reallocate(obj%rhs_m_v1_zero, nnt)
CALL Reallocate(obj%rhs_k_v1_zero, nnt)
CALL Reallocate(obj%rhs_c_v1_zero, nnt)

CALL Reallocate(obj%initialGuess, nnt + 3_I4B)
CALL Reallocate(obj%jumpDis, nnt + 3_I4B)
CALL Reallocate(obj%jumpVel, nnt + 3_I4B)
CALL Reallocate(obj%dis, nnt + 3_I4B)
CALL Reallocate(obj%vel, nnt + 3_I4B)
CALL Reallocate(obj%acc, nnt + 3_I4B)

CALL Reallocate(obj%initialGuess_zero, nnt + 3_I4B)
CALL Reallocate(obj%jumpDis_zero, nnt + 3_I4B)
CALL Reallocate(obj%jumpVel_zero, nnt + 3_I4B)
CALL Reallocate(obj%dis_zero, nnt + 3_I4B)
CALL Reallocate(obj%vel_zero, nnt + 3_I4B)
CALL Reallocate(obj%acc_zero, nnt + 3_I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_AllocateData

!----------------------------------------------------------------------------
!                                                           InitiateVStfem_kt
!----------------------------------------------------------------------------

SUBROUTINE InitiateVStfem_kt(obj, elemsd, facetElemsd)
  CLASS(VStfemAlgorithm_), INTENT(INOUT) :: obj
  TYPE(ElemShapeData_), INTENT(IN) :: elemsd, facetElemsd

! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "InitiateVStfem_kt()"
#endif
  INTEGER(I4B) :: ii, jj, nnt, nips
  REAL(DFP), ALLOCATABLE, DIMENSION(:, :) :: mt, kt, kt_inv, kt_inv_mt, &
                                             kt_inv_mt_kt_inv, mt_inv
  REAL(DFP), ALLOCATABLE :: tempVec(:)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  nips = elemsd%nips
  nnt = elemsd%nns

  CALL Reallocate(mt, nnt, nnt)
  CALL Reallocate(kt, nnt, nnt)
  CALL Reallocate(kt_inv, nnt, nnt)
  CALL Reallocate(kt_inv_mt, nnt, nnt)
  CALL Reallocate(kt_inv_mt_kt_inv, nnt, nnt)
  CALL Reallocate(tempVec, nnt)

  ! make kt
  kt = math%zero
  CALL MassMatrix_(N=elemsd%N, M=elemsd%N, js=elemsd%js, ws=elemsd%ws, &
                   thickness=elemsd%thickness, nips=nips, nns1=nnt, &
                   nns2=nnt, ans=kt, nrow=ii, ncol=jj)

  ! make mt: part 1: (T, dT/dt)_In
  mt = math%zero
  CALL MassMatrix_(N=elemsd%N, M=elemsd%dNdXt(:, 1, :), &
                   js=elemsd%js, ws=elemsd%ws, thickness=elemsd%thickness, &
                   nips=nips, nns1=nnt, nns2=nnt, ans=mt, nrow=ii, &
                   ncol=jj)

  ! make mt: part 2: Tn x Tn jump contribution
  CALL OuterProd_(a=facetElemsd%N(1:nnt, 1), &
                  b=facetElemsd%N(1:nnt, 1), &
                  ans=mt, nrow=ii, ncol=jj, &
                  scale=math%one, anscoeff=math%one)

  ! Calculate inverse of kt
  CALL GetInvMat(A=kt, invA=kt_inv)

  ! Calculate inverse of mt
  CALL GetInvMat(A=mt, invA=mt_inv)

  ! make kt_inv_mt
  kt_inv_mt = MATMUL(kt_inv, mt)

  ! make kt_inv_mt_kt_inv
  kt_inv_mt_kt_inv = MATMUL(kt_inv_mt, kt_inv)

  obj%forceCoeff = kt_inv

  ! make stiffMatCoeff
  obj%stiffMatCoeff = math%zero
  DO ii = 1, nnt
    obj%stiffMatCoeff = math%one
  END DO

  ! make dampMatCoeff
  obj%dampMatCoeff = kt_inv_mt

  ! make massMatCoeff: mt * kt_inv * mt
  obj%massMatCoeff = MATMUL(kt_inv_mt_kt_inv, mt)

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
  obj%rhs_k_u1 = math%minus_one * MATMUL(kt_inv, facetElemsd%N(1:nnt, 1))

  ! KV0
  obj%rhs_k_v1(1:nnt) = math%zero

  !-----------------------------------------------------------------
  ! Updating solutions
  !-----------------------------------------------------------------

  CALL MakeUpdateCoeffs(obj=obj, facetElemsd=facetElemsd, nnt=nnt, &
                        tempVec=tempVec, kt=kt, mt_inv=mt_inv)

  !-----------------------------------------------------------------
  ! Make zeros
  !-----------------------------------------------------------------

  CALL obj%MakeZeros()

  !-----------------------------------------------------------------
  ! Deallocate data
  !-----------------------------------------------------------------

  DEALLOCATE (mt, kt, kt_inv, kt_inv_mt, kt_inv_mt_kt_inv, tempVec)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE InitiateVStfem_kt

!----------------------------------------------------------------------------
!                                                          InitiateVStfem_mt
!----------------------------------------------------------------------------

SUBROUTINE InitiateVStfem_mt(obj, elemsd, facetElemsd)
  CLASS(VStfemAlgorithm_), INTENT(INOUT) :: obj
  TYPE(ElemShapeData_), INTENT(IN) :: elemsd, facetElemsd

! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "InitiateVStfem_mt()"
#endif
  INTEGER(I4B) :: ii, jj, nnt, nips
  REAL(DFP), ALLOCATABLE, DIMENSION(:, :) :: mt, kt, mt_inv, &
                                             mt_inv_kt, mt_inv_kt_mt_inv
  REAL(DFP), ALLOCATABLE :: tempVec(:)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  nips = elemsd%nips
  nnt = elemsd%nns

  CALL Reallocate(mt, nnt, nnt)
  CALL Reallocate(kt, nnt, nnt)
  CALL Reallocate(mt_inv, nnt, nnt)
  CALL Reallocate(mt_inv_kt, nnt, nnt)
  CALL Reallocate(mt_inv_kt_mt_inv, nnt, nnt)
  CALL Reallocate(tempVec, nnt)

  ! make kt
  kt = math%zero
  CALL MassMatrix_(N=elemsd%N, M=elemsd%N, js=elemsd%js, ws=elemsd%ws, &
                   thickness=elemsd%thickness, nips=nips, nns1=nnt, &
                   nns2=nnt, ans=kt, nrow=ii, ncol=jj)

  ! make mt: part 1: (T, dT/dt)_In
  mt = math%zero
  CALL MassMatrix_(N=elemsd%N, M=elemsd%dNdXt(:, 1, :), &
                   js=elemsd%js, ws=elemsd%ws, thickness=elemsd%thickness, &
                   nips=nips, nns1=nnt, nns2=nnt, ans=mt, nrow=ii, &
                   ncol=jj)

  ! make mt: part 2: Tn x Tn jump contribution
  CALL OuterProd_(a=facetElemsd%N(1:nnt, 1), &
                  b=facetElemsd%N(1:nnt, 1), &
                  ans=mt, nrow=ii, ncol=jj, &
                  scale=math%one, anscoeff=math%one)

  ! make mt_inv
  CALL GetInvMat(A=mt, invA=mt_inv)

  ! make mt_inv_kt
  mt_inv_kt = MATMUL(mt_inv, kt)

  ! make mt_inv_kt_mt_inv
  mt_inv_kt_mt_inv = MATMUL(mt_inv_kt, mt_inv)

  ! forceCoeff
  obj%forceCoeff = mt_inv

  ! make massMatCoeff
  obj%massMatCoeff = math%zero
  DO ii = 1, nnt
    obj%massMatCoeff(ii, ii) = math%one
  END DO

  ! make dampMatCoeff
  obj%dampMatCoeff = mt_inv_kt

  ! make stiffMatCoeff:
  obj%stiffMatCoeff = MATMUL(mt_inv_kt_mt_inv, kt)

  ! make rhs coeff for mass matrix: coeff for MU0, MV0, MA0
  ! MV0
  obj%rhs_m_v1 = MATMUL(mt_inv, facetElemsd%N(1:nnt, 1))

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
                        tempVec=tempVec, kt=kt, mt_inv=mt_inv)

  !-----------------------------------------------------------------
  ! Make zeros
  !-----------------------------------------------------------------

  CALL obj%MakeZeros()

  !-----------------------------------------------------------------
  ! Deallocate data
  !-----------------------------------------------------------------

  DEALLOCATE (mt, kt, mt_inv, mt_inv_kt, mt_inv_kt_mt_inv, tempVec)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE InitiateVStfem_mt

!----------------------------------------------------------------------------
!                                                          InitiateVStfem_It
!----------------------------------------------------------------------------

SUBROUTINE InitiateVStfem_It(obj, elemsd, facetElemsd)
  CLASS(VStfemAlgorithm_), INTENT(INOUT) :: obj
  TYPE(ElemShapeData_), INTENT(IN) :: elemsd, facetElemsd

! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "InitiateVStfem_It()"
#endif
  INTEGER(I4B) :: ii, jj, nnt, nips
  REAL(DFP), ALLOCATABLE, DIMENSION(:, :) :: mt, kt, mt_inv, kt_mt_inv
  REAL(DFP), ALLOCATABLE :: tempVec(:)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  nips = elemsd%nips
  nnt = elemsd%nns

  CALL Reallocate(mt, nnt, nnt)
  CALL Reallocate(kt, nnt, nnt)
  CALL Reallocate(mt_inv, nnt, nnt)
  CALL Reallocate(kt_mt_inv, nnt, nnt)
  CALL Reallocate(tempVec, nnt)

  ! make kt
  kt = math%zero
  CALL MassMatrix_(N=elemsd%N, M=elemsd%N, js=elemsd%js, ws=elemsd%ws, &
                   thickness=elemsd%thickness, nips=nips, nns1=nnt, &
                   nns2=nnt, ans=kt, nrow=ii, ncol=jj)

  ! make mt: part 1: (T, dT/dt)_In
  mt = math%zero
  CALL MassMatrix_(N=elemsd%N, M=elemsd%dNdXt(:, 1, :), &
                   js=elemsd%js, ws=elemsd%ws, thickness=elemsd%thickness, &
                   nips=nips, nns1=nnt, nns2=nnt, ans=mt, nrow=ii, &
                   ncol=jj)

  ! make mt: part 2: Tn x Tn jump contribution
  CALL OuterProd_(a=facetElemsd%N(1:nnt, 1), &
                  b=facetElemsd%N(1:nnt, 1), &
                  ans=mt, nrow=ii, ncol=jj, &
                  scale=math%one, anscoeff=math%one)

  ! make mt_inv
  CALL GetInvMat(A=mt, invA=mt_inv)

  ! make kt_mt_inv
  kt_mt_inv = MATMUL(kt, mt_inv)

  ! make forceCoeff
  obj%forceCoeff = math%zero
  DO ii = 1, nnt
    obj%forceCoeff(ii, ii) = math%one
  END DO

  ! make massMatCoeff:
  obj%massMatCoeff = mt

  ! make dampMatCoeff
  obj%dampMatCoeff = kt

  ! make stiffMatCoeff
  obj%stiffMatCoeff = MATMUL(kt_mt_inv, kt)

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
  obj%rhs_k_u1 = math%minus_one * MATMUL(kt_mt_inv, &
                                         facetElemsd%N(1:nnt, 1))

  ! KV0
  obj%rhs_k_v1(1:nnt) = math%zero

  !-----------------------------------------------------------------
  ! Updating solutions
  !-----------------------------------------------------------------

  CALL MakeUpdateCoeffs(obj=obj, facetElemsd=facetElemsd, nnt=nnt, &
                        tempVec=tempVec, kt=kt, mt_inv=mt_inv)

  !-----------------------------------------------------------------
  ! Make zeros
  !-----------------------------------------------------------------

  CALL obj%MakeZeros()

  !-----------------------------------------------------------------
  ! Deallocate data
  !-----------------------------------------------------------------

  DEALLOCATE (mt, kt, mt_inv, kt_mt_inv, tempVec)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE InitiateVStfem_It

!----------------------------------------------------------------------------
!                                                           MakeUpdateCoeffs
!----------------------------------------------------------------------------

SUBROUTINE MakeUpdateCoeffs(obj, facetElemsd, nnt, tempVec, kt, mt_inv)
  TYPE(VStfemAlgorithm_), INTENT(INOUT) :: obj
  TYPE(ElemShapeData_), INTENT(IN) :: facetElemsd
  INTEGER(I4B), INTENT(IN) :: nnt
  REAL(DFP), INTENT(INOUT) :: tempVec(:)
  REAL(DFP), INTENT(IN) :: kt(:, :)
  REAL(DFP), INTENT(IN) :: mt_inv(:, :)

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
  tempVec = MATMUL(mt_inv, facetElemsd%N(1:nnt, 1))
  obj%dis(1) = DOT_PRODUCT(tempVec, facetElemsd%N(1:nnt, 2))
  ! update disp: coeff for v0 and a0 are zero
  obj%dis(2:3) = math%zero

  ! update disp: coeff for nnt solution
  !  Tn+1 * mt_inv * kt
  !     step1: using rhs_k_a1 as temp storage
  tempVec = MATMUL(facetElemsd%N(1:nnt, 2), mt_inv)
  obj%vel(4:nnt + 3) = MATMUL(tempVec, kt)

  ! update acc
  ! update acc: coeff for u0:
  tempVec = MATMUL(mt_inv, facetElemsd%N(1:nnt, 1))
  obj%acc(1) = DOT_PRODUCT(tempVec, facetElemsd%dNdXt(1:nnt, 1, 2))
  !   step2: resetting rhs_k_a1 to original value

  ! update acc: coeff for v0 and a0 are zero
  obj%acc(2:3) = math%zero

  ! update acc: coeff for nnt solution
  !  Tn+1 * mt_inv * mt
  tempVec = MATMUL(facetElemsd%dNdXt(1:nnt, 1, 2), mt_inv)
  obj%acc(4:nnt + 3) = MATMUL(tempVec(1:nnt), kt)
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
!                                                                   MakeZeros
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_MakeZeros
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_MakeZeros()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%initialGuess_zero = obj%initialGuess.approxeq.math%zero
obj%jumpDis_zero = obj%jumpDis.approxeq.math%zero
obj%jumpVel_zero = obj%jumpVel.approxeq.math%zero
obj%dis_zero = obj%dis.approxeq.math%zero
obj%vel_zero = obj%vel.approxeq.math%zero
obj%acc_zero = obj%acc.approxeq.math%zero
obj%rhs_m_u1_zero = obj%rhs_m_u1.approxeq.math%zero
obj%rhs_m_v1_zero = obj%rhs_m_v1.approxeq.math%zero
obj%rhs_c_u1_zero = obj%rhs_c_u1.approxeq.math%zero
obj%rhs_c_v1_zero = obj%rhs_c_v1.approxeq.math%zero
obj%rhs_k_u1_zero = obj%rhs_k_u1.approxeq.math%zero
obj%rhs_k_v1_zero = obj%rhs_k_v1.approxeq.math%zero

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
obj%massMatCoeff = math%zero
obj%dampMatCoeff = math%zero
obj%stiffMatCoeff = math%zero
obj%rhs_m_u1 = math%zero
obj%rhs_m_v1 = math%zero
obj%rhs_c_u1 = math%zero
obj%rhs_c_v1 = math%zero
obj%rhs_k_u1 = math%zero
obj%rhs_k_v1 = math%zero
obj%rhs_m_u1_zero = math%yes
obj%rhs_m_v1_zero = math%yes
obj%rhs_c_u1_zero = math%yes
obj%rhs_c_v1_zero = math%yes
obj%rhs_k_u1_zero = math%yes
obj%rhs_k_v1_zero = math%yes

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

CALL Display(obj%rhs_c_u1(1:nns), "rhs_c_u1: ", unitno=unitno, advance="NO")
CALL Display(obj%rhs_c_u1_zero(1:nns), "rhs_c_u1_zero: ", unitno=unitno)

CALL Display(obj%rhs_c_v1(1:nns), "rhs_c_v1: ", unitno=unitno, advance="NO")
CALL Display(obj%rhs_c_v1_zero(1:nns), "rhs_c_v1_zero: ", unitno=unitno)

CALL Display(obj%rhs_k_u1(1:nns), "rhs_k_u1: ", unitno=unitno, advance="NO")
CALL Display(obj%rhs_k_u1_zero(1:nns), "rhs_k_u1_zero: ", unitno=unitno)

CALL Display(obj%rhs_k_v1(1:nns), "rhs_k_v1: ", unitno=unitno, advance="NO")
CALL Display(obj%rhs_k_v1_zero(1:nns), "rhs_k_v1_zero: ", unitno=unitno)

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
