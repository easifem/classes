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

SUBMODULE(TDGAlgorithm2_Class) ConstructorMethods
USE ApproxUtility, ONLY: OPERATOR(.approxeq.)
USE InputUtility, ONLY: Input
USE MassMatrix_Method, ONLY: MassMatrix_
USE ProductUtility, ONLY: OuterProd_
USE BaseType, ONLY: math => TypeMathOpt
USE Lapack_Method, ONLY: GetInvMat
USE FEFactoryUtility, ONLY: OneDimFEFactory

USE BaseType, ONLY: elemOpt => TypeElemNameOpt
USE BaseType, ONLY: quadOpt => TypeQuadratureOpt

USE QuadraturePoint_Method, ONLY: QuadPoint_Initiate => Initiate, &
                                  Quad_Size => Size, &
                                  Quad_Display => Display

USE ElemshapeData_Method, ONLY: LagrangeElemShapeData, &
                                Elemsd_Allocate => ALLOCATE, &
                                HierarchicalElemShapeData, &
                                Elemsd_Set => Set, &
                                Elemsd_Initiate => Initiate, &
                                OrthogonalElemShapeData

#ifdef DEBUG_VER
USE Display_Method, ONLY: Display
#endif

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

INTEGER(I4B) :: i1

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isInit = .TRUE.

obj%nrow = elemsd%nns
obj%ncol = obj%nrow

IF (PRESENT(alpha)) THEN
  obj%alpha = alpha
END IF

CALL GetCt(obj, elemsd)

CALL GetMt(obj, elemsd, facetElemsd)

CALL GetWt(obj, elemsd)

CALL GetAt(obj, elemsd, facetElemsd)

CALL GetBt(obj, elemsd, facetElemsd, fe)

CALL GetKt(obj, elemsd)

obj%dis(1) = obj%at_right

DO i1 = 1, obj%nrow
  obj%dis(i1 + 3) = obj%bt_right(i1)
  obj%vel(i1 + 3) = facetElemsd%N(i1, 2)
  obj%acc(i1 + 3) = facetElemsd%dNdXt(i1, 1, 2)
  obj%rhs_m_v1(i1) = facetElemsd%N(i1, 1)
  obj%rhs_k_u1(i1) = -obj%tat(i1) ! minus sign
END DO

CALL obj%MakeZeros()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Getting Kt, coefficients for the stiffness matrix
! This should be called after GetBt

SUBROUTINE GetKt(obj, elemsd)
  CLASS(TDGAlgorithm2_), INTENT(INOUT) :: obj
  TYPE(ElemShapeData_), INTENT(IN) :: elemsd

  INTEGER(I4B) :: nrow, ncol, ips, ii, jj
  REAL(DFP) :: scale

  nrow = obj%nrow
  ncol = nrow
  obj%kt(1:nrow, 1:ncol) = math%zero

  DO ips = 1, elemsd%nips

    scale = elemsd%ws(ips) * elemsd%thickness(ips) * elemsd%js(ips)

    CALL OuterProd_(a=elemsd%N(1:nrow, ips), &
                    b=obj%bt(1:ncol, ips), &
                    ans=obj%kt, nrow=ii, ncol=jj, &
                    anscoeff=math%one, scale=scale)

  END DO

END SUBROUTINE GetKt

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Getting Ct, coefficients for the damping matrix

SUBROUTINE GetCt(obj, elemsd)
  CLASS(TDGAlgorithm2_), INTENT(INOUT) :: obj
  TYPE(ElemShapeData_), INTENT(IN) :: elemsd

  CALL MassMatrix_(test=elemsd, trial=elemsd, ans=obj%ct, nrow=obj%nrow, &
                   ncol=obj%ncol)

END SUBROUTINE GetCt

!----------------------------------------------------------------------------
!                                                                       GetMt
!----------------------------------------------------------------------------

! Getting Mt, coefficients for the mass matrix (not temporal mass matrix)

SUBROUTINE GetMt(obj, elemsd, facetElemsd)
  CLASS(TDGAlgorithm2_), INTENT(INOUT) :: obj
  TYPE(ElemShapeData_), INTENT(IN) :: elemsd, facetElemsd

  INTEGER(I4B) :: i1, i2

  CALL MassMatrix_(N=elemsd%N, M=elemsd%dNdXt(:, 1, :), &
                   js=elemsd%js, ws=elemsd%ws, thickness=elemsd%thickness, &
                   nips=elemsd%nips, nns1=elemsd%nns, nns2=elemsd%nns, &
                   ans=obj%mt, nrow=obj%nrow, ncol=obj%ncol)

  ! jump contribution
  CALL OuterProd_(a=facetElemsd%N(1:obj%nrow, 1), &
                  b=facetElemsd%N(1:obj%nrow, 1), &
                  ans=obj%mt, nrow=i1, ncol=i2, scale=math%one, &
                  anscoeff=math%one)

END SUBROUTINE GetMt

!----------------------------------------------------------------------------
!                                                                GetWt
!----------------------------------------------------------------------------

! Getting Wt and Wmt. This should be called after GetMt

SUBROUTINE GetWt(obj, elemsd)
  CLASS(TDGAlgorithm2_), INTENT(INOUT) :: obj
  TYPE(ElemShapeData_), INTENT(IN) :: elemsd

  INTEGER(I4B) :: nrow, ncol

  nrow = obj%nrow
  ncol = obj%ncol

  obj%wt(1:nrow, 1:ncol) = obj%mt(1:nrow, 1:ncol) ! temp mass + jump contri
  CALL GetInvMat(obj%wt(1:nrow, 1:ncol))

  obj%wmt(1:nrow, 1:ncol) = &
    MATMUL(obj%wt(1:nrow, 1:ncol), obj%ct(1:nrow, 1:ncol))

  obj%wmt(1:nrow, 1:ncol) = TRANSPOSE(obj%wmt(1:nrow, 1:ncol))

END SUBROUTINE GetWt

!----------------------------------------------------------------------------
!                                                               GetAt
!----------------------------------------------------------------------------

! Getting At
! This should be called after GetWt

SUBROUTINE GetAt(obj, elemsd, facetElemsd)
  CLASS(TDGAlgorithm2_), INTENT(INOUT) :: obj
  TYPE(ElemShapeData_), INTENT(IN) :: elemsd, facetElemsd
  REAL(DFP) :: temp(MAX_ORDER_TIME), areal
  INTEGER(I4B) :: nrow, ncol, tsize, ii

  nrow = obj%nrow
  ncol = obj%ncol

  tsize = elemsd%nips

  temp(1:nrow) = &
    MATMUL(obj%wt(1:nrow, 1:ncol), facetElemsd%N(1:ncol, 1))

  obj%tat(1:nrow) = math%zero

  obj%at_right = DOT_PRODUCT(facetElemsd%N(1:nrow, 2), temp(1:nrow))

  DO ii = 1, tsize
    obj%at(ii) = DOT_PRODUCT(elemsd%N(1:nrow, ii), temp(1:nrow))
    areal = obj%at(ii) * elemsd%ws(ii) * elemsd%thickness(ii) * elemsd%js(ii)
    obj%tat(1:nrow) = obj%tat(1:nrow) + elemsd%N(1:nrow, ii) * areal
  END DO

END SUBROUTINE GetAt

!----------------------------------------------------------------------------
!                                                                 GetBt
!----------------------------------------------------------------------------

! Getting Bt
! This should be called after GetWt

SUBROUTINE GetBt(obj, elemsd, facetElemsd, fe)
  CLASS(TDGAlgorithm2_), INTENT(INOUT) :: obj
  TYPE(ElemShapeData_), INTENT(IN) :: elemsd, facetElemsd
  CLASS(AbstractOneDimFE_), INTENT(INOUT) :: fe

  INTEGER(I4B) :: nrow, ncol, ii, jj, kk, quadOrder, subsetNipt
  REAL(DFP) :: tmpBt(obj%nrow, elemsd%nips), &
               quadPoints(2, elemsd%nips), &
               subsetRefTime(1, 2), subsetTime(1, 2), scale, ja
  TYPE(ElemShapeData_) :: linearElemsd, subsetElemsd
  TYPE(QuadraturePoint_) :: quad, subsetQuad
  CLASS(AbstractOneDimFE_), POINTER :: subfe => NULL()
  CHARACTER(:), ALLOCATABLE :: ipType, baseType

  nrow = obj%nrow
  ncol = elemsd%nips

  obj%bt_right(1:nrow) = MATMUL(obj%wmt(1:nrow, 1:nrow), &
                                facetElemsd%N(1:nrow, 2))

  obj%bt_right(1:nrow) = obj%bt_right(1:nrow)

  !----------------------------------
  !                       BT for uvst
  !----------------------------------

  obj%bt(1:nrow, 1:ncol) = MATMUL(obj%wmt(1:nrow, 1:nrow), &
                                  elemsd%N(1:nrow, 1:ncol))

  obj%bt(1:nrow, 1:ncol) = obj%alpha * obj%bt(1:nrow, 1:ncol)

  IF (obj%alpha .EQ. math%one) RETURN

  !----------------------------------
  !                       BT for vst
  !----------------------------------

  tmpBt(1:nrow, 1:ncol) = obj%bt(1:nrow, 1:ncol)

  obj%bt(1:nrow, 1:ncol) = math%zero

  baseType = fe%GetBaseContinuity()
  ipType = fe%GetBaseInterpolation()
  subfe => OneDimFEFactory(baseType, ipType)

  CALL subfe%Copy(fe)
  CALL subfe%GetParam(quadratureOrder=quadOrder)
  CALL subfe%SetOrder(1)
  CALL subfe%SetQuadratureOrder(quadOrder)

  CALL fe%GetQuadraturePoints(quad)
  CALL subfe%GetQuadraturePoints(subsetQuad)
  CALL subfe%GetLocalElemShapeData(linearElemsd, subsetQuad)

  subsetRefTime(1, 1) = -math%one
  quadPoints = subsetQuad%points
  subsetNipt = SIZE(quadPoints, 2)

  DO ii = 1, ncol

    subsetRefTime(1, 2) = quad%points(1, ii)

    CALL Elemsd_Set(obj=linearElemsd, val=subsetRefTime, &
                    N=linearElemsd%N, dNdXi=linearElemsd%dNdXi)
    quadPoints(1, 1:subsetNipt) = linearElemsd%coord(1, 1:subsetNipt)
    ! ja = linearElemsd%jacobian(1, 1, 1)
    CALL QuadPoint_Initiate(obj=subsetQuad, &
                            points=quadPoints(:, 1:subsetNipt))

    CALL fe%GetLocalElemShapeData(subsetElemsd, subsetQuad)

    DO jj = 1, nrow
      DO kk = 1, subsetNipt
        scale = subsetElemsd%ws(kk) * elemsd%js(1) * math%half
        obj%bt(jj, ii) = obj%bt(jj, ii) + &
                         scale * subsetElemsd%N(jj, kk)
      END DO
    END DO

  END DO

  obj%bt(1:nrow, 1:ncol) = tmpBt(1:nrow, 1:ncol) + &
                           (1.0_DFP - obj%alpha) * obj%bt(1:nrow, 1:ncol)

END SUBROUTINE GetBt

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

obj%isInit = .FALSE.

obj%name = "TDG2"
obj%alpha = 1.0_DFP
obj%nrow = 0_I4B
obj%ncol = 0_I4B

obj%initialGuess = 0.0_DFP
obj%initialGuess_zero = .TRUE.

obj%dis = 0.0_DFP
obj%dis_zero = .TRUE.

obj%vel = 0.0_DFP
obj%vel_zero = .TRUE.

obj%acc = 0.0_DFP
obj%acc_zero = .TRUE.

obj%mt = 0.0_DFP
obj%ct = 0.0_DFP
obj%bt = 0.0_DFP
obj%bt_right = 0.0_DFP
obj%wt = 0.0_DFP
obj%wmt = 0.0_DFP
obj%at = 0.0_DFP
obj%at_right = 0.0_DFP
obj%tat = 0.0_DFP
obj%kt = 0.0_DFP

obj%rhs_m_u1 = 0.0_DFP
obj%rhs_m_v1 = 0.0_DFP
obj%rhs_m_a1 = 0.0_DFP
obj%rhs_c_u1 = 0.0_DFP
obj%rhs_c_v1 = 0.0_DFP
obj%rhs_c_a1 = 0.0_DFP
obj%rhs_k_u1 = 0.0_DFP
obj%rhs_k_v1 = 0.0_DFP
obj%rhs_k_a1 = 0.0_DFP

obj%rhs_m_u1_zero = .TRUE.
obj%rhs_m_v1_zero = .TRUE.
obj%rhs_m_a1_zero = .TRUE.
obj%rhs_c_u1_zero = .TRUE.
obj%rhs_c_v1_zero = .TRUE.
obj%rhs_c_a1_zero = .TRUE.
obj%rhs_k_u1_zero = .TRUE.
obj%rhs_k_v1_zero = .TRUE.
obj%rhs_k_a1_zero = .TRUE.

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
