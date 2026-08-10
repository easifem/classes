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
USE Display_Method, ONLY: Display
USE BaseType, ONLY: elemOpt => TypeElemNameOpt
USE BaseType, ONLY: quadOpt => TypeQuadratureOpt
USE QuadraturePoint_Method, ONLY: QuadPoint_Initiate => Initiate
USE QuadraturePoint_Method, ONLY: Quad_Size => Size
USE QuadraturePoint_Method, ONLY: Quad_Display => Display
USE ElemshapeData_Method, ONLY: LagrangeElemShapeData
USE ElemshapeData_Method, ONLY: Elemsd_Allocate => ALLOCATE
USE ElemshapeData_Method, ONLY: HierarchicalElemShapeData
USE ElemshapeData_Method, ONLY: Elemsd_Set => Set
USE ElemshapeData_Method, ONLY: Elemsd_Initiate => Initiate
USE ElemshapeData_Method, ONLY: OrthogonalElemShapeData
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

obj%isInit = math%yes

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

obj%jumpDis(1) = obj%at_left - math%one

obj%jumpVel(2) = math%minus_one

DO i1 = 1, obj%nrow
  obj%dis(i1 + 3) = obj%bt_right(i1)
  obj%jumpDis(i1 + 3) = obj%bt_left(i1)
  obj%vel(i1 + 3) = facetElemsd%N(i1, 2)
  obj%jumpVel(i1 + 3) = facetElemsd%N(i1, 1)
  obj%acc(i1 + 3) = facetElemsd%dNdXt(i1, 1, 2)
  obj%rhs_m_v1(i1) = facetElemsd%N(i1, 1)
  obj%rhs_k_u1(i1) = -obj%tat(i1)
  ! minus sign
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
  obj%at_left = DOT_PRODUCT(facetElemsd%N(1:nrow, 1), temp(1:nrow))

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
               subsetRefTime(1, 2), scale
  TYPE(ElemShapeData_) :: linearElemsd, subsetElemsd
  TYPE(QuadraturePoint_) :: quad, subsetQuad
  CLASS(AbstractOneDimFE_), POINTER :: subfe => NULL()
  CHARACTER(:), ALLOCATABLE :: ipType, baseType

  nrow = obj%nrow
  ncol = elemsd%nips

  obj%bt_right(1:nrow) = MATMUL(obj%wmt(1:nrow, 1:nrow), &
                                facetElemsd%N(1:nrow, 2))

  obj%bt_left(1:nrow) = MATMUL(obj%wmt(1:nrow, 1:nrow), &
                               facetElemsd%N(1:nrow, 1))

  ! BT for uvst
  obj%bt(1:nrow, 1:ncol) = MATMUL(obj%wmt(1:nrow, 1:nrow), &
                                  elemsd%N(1:nrow, 1:ncol))

  obj%bt(1:nrow, 1:ncol) = obj%alpha * obj%bt(1:nrow, 1:ncol)

  IF (obj%alpha .EQ. math%one) RETURN

  ! BT for vst
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

obj%name = "TDG2"
obj%alpha = math%one
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
obj%ct = math%zero
obj%bt = math%zero
obj%bt_right = math%zero
obj%bt_left = math%zero
obj%wt = math%zero
obj%wmt = math%zero
obj%at = math%zero
obj%at_right = math%zero
obj%at_left = math%zero
obj%tat = math%zero
obj%kt = math%zero

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

INTEGER(I4B) :: nrow, ncol

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

CALL Display(obj%alpha, "alpha: ", unitno=unitno)
CALL Display(obj%name, "name: ", unitno=unitno)
CALL Display(obj%nrow, "nrow: ", unitno=unitno)
CALL Display(obj%ncol, "ncol: ", unitno=unitno)

nrow = obj%nrow
ncol = obj%ncol

CALL Display(obj%initialGuess(1:nrow + 3), "initialGuess: ", &
             unitno=unitno, advance="NO")
CALL Display(obj%initialGuess_zero(1:nrow + 3), "initialGuess_zero: ", &
             unitno=unitno)

CALL Display(obj%jumpDis(1:nrow + 3), "jumpDis: ", unitno=unitno, &
             advance="NO")
CALL Display(obj%jumpDis_zero(1:nrow + 3), "jumpDis_zero: ", unitno=unitno)

CALL Display(obj%jumpVel(1:nrow + 3), "jumpVel: ", unitno=unitno, &
             advance="NO")
CALL Display(obj%jumpVel_zero(1:nrow + 3), "jumpVel_zero: ", unitno=unitno)

CALL Display(obj%dis(1:nrow + 3), "dis: ", unitno=unitno, &
             advance="NO")
CALL Display(obj%dis_zero(1:nrow + 3), "dis_zero: ", unitno=unitno)

CALL Display(obj%vel(1:nrow + 3), "vel: ", unitno=unitno, advance="NO")
CALL Display(obj%vel_zero(1:nrow + 3), "vel_zero: ", unitno=unitno)

CALL Display(obj%acc(1:nrow + 3), "acc: ", unitno=unitno, advance="NO")
CALL Display(obj%acc_zero(1:nrow + 3), "acc_zero: ", unitno=unitno)

CALL Display(obj%mt(1:nrow, 1:ncol), "mt: ", unitno=unitno)
CALL Display(obj%ct(1:nrow, 1:ncol), "ct: ", unitno=unitno)
CALL Display(obj%kt(1:nrow, 1:ncol), "kt: ", unitno=unitno)

CALL Display(obj%rhs_m_u1(1:nrow), "rhs_m_u1: ", unitno=unitno, advance="NO")
CALL Display(obj%rhs_m_u1_zero(1:nrow), "rhs_m_u1_zero: ", unitno=unitno)

CALL Display(obj%rhs_m_v1(1:nrow), "rhs_m_v1: ", unitno=unitno, advance="NO")
CALL Display(obj%rhs_m_v1_zero(1:nrow), "rhs_m_v1_zero: ", unitno=unitno)

CALL Display(obj%rhs_m_a1(1:nrow), "rhs_m_a1: ", unitno=unitno, advance="NO")
CALL Display(obj%rhs_m_a1_zero(1:nrow), "rhs_m_a1_zero: ", unitno=unitno)

CALL Display(obj%rhs_c_u1(1:nrow), "rhs_c_u1: ", unitno=unitno, advance="NO")
CALL Display(obj%rhs_c_u1_zero(1:nrow), "rhs_c_u1_zero: ", unitno=unitno)

CALL Display(obj%rhs_c_v1(1:nrow), "rhs_c_v1: ", unitno=unitno, advance="NO")
CALL Display(obj%rhs_c_v1_zero(1:nrow), "rhs_c_v1_zero: ", unitno=unitno)

CALL Display(obj%rhs_c_a1(1:nrow), "rhs_c_a1: ", unitno=unitno, advance="NO")
CALL Display(obj%rhs_c_a1_zero(1:nrow), "rhs_c_a1_zero: ", unitno=unitno)

CALL Display(obj%rhs_k_u1(1:nrow), "rhs_k_u1: ", unitno=unitno, advance="NO")
CALL Display(obj%rhs_k_u1_zero(1:nrow), "rhs_k_u1_zero: ", unitno=unitno)

CALL Display(obj%rhs_k_v1(1:nrow), "rhs_k_v1: ", unitno=unitno, advance="NO")
CALL Display(obj%rhs_k_v1_zero(1:nrow), "rhs_k_v1_zero: ", unitno=unitno)

CALL Display(obj%rhs_k_a1(1:nrow), "rhs_k_a1: ", unitno=unitno, advance="NO")
CALL Display(obj%rhs_k_a1_zero(1:nrow), "rhs_k_a1_zero: ", unitno=unitno)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                             Include errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ConstructorMethods
