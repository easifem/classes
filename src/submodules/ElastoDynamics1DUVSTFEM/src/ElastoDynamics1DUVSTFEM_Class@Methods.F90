! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
! Vikas Sharma, Ph.D., vickysharma0812@gmail.com
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

SUBMODULE(ElastoDynamics1DUVSTFEM_Class) Methods
USE tomlf, ONLY: toml_serialize, &
                 toml_get => get_value, &
                 toml_stat, toml_array, &
                 toml_len => len

USE Lapack_Method, ONLY: GetInvMat, SymLinSolve

USE TomlUtility, ONLY: GetValue, GetValue_

USE StringUtility, ONLY: UpperCase

USE Display_Method, ONLY: ToString, Display

USE GlobalData, ONLY: stdout, &
                      CHAR_LF, &
                      DOF_FMT, &
                      NONE, &
                      LIS_GMRES, &
                      CHAR_SLASH

USE BaseInterpolation_Method, ONLY: BaseInterpolation_ToInteger, &
                                    BaseType_ToInteger, &
                                    BaseType_ToChar, &
                                    BaseInterpolation_ToChar

USE LineInterpolationUtility, ONLY: OrthogonalBasis_Line_

USE ReallocateUtility, ONLY: Reallocate

USE ProductUtility, ONLY: OuterProd_, OTimesTilda

USE BaseType, ONLY: elem => TypeElemNameOpt

USE QuadraturePoint_Method, ONLY: QuadPoint_Initiate => Initiate, &
                                  Quad_Size => Size, &
                                  Quad_Display => Display

USE ElemshapeData_Method, ONLY: LagrangeElemShapeData, &
                                Elemsd_Allocate => ALLOCATE, &
                                HierarchicalElemShapeData, &
                                Elemsd_Set => Set, &
                                OrthogonalElemShapeData

USE SwapUtility, ONLY: SWAP

USE CSRMatrix_Method, ONLY: CSRMatrix_Initiate => Initiate, &
                            CSRMatrix_Add => Add, &
                            CSRMatrix_GetSubMatrix => GetSubMatrix, &
                            CSRMatrix_Display => Display, &
                            CSRMatrix_Size => Size, &
                            CSRMatrix_SetSparsity => SetSparsity, &
                            CSRMatrix_ApplyDBC => ApplyDBC, &
                            CSRMatrix_Set => Set, &
                            CSRMatrix_Matvec => Matvec, &
                            CSRMatrix_LinSolve => CSRMatrix_GMRES, &
                            CSRMatrixLinSolveInitiate

USE DOF_Method, ONLY: DOF_Initiate => Initiate, &
                      DOF_SIZE => Size, &
                      DOF_GetIndex_ => GetIndex_, &
                      DOF_GetNodeLoc => GetNodeLoc

USE RealVector_Method, ONLY: RealVector_Initiate => Initiate, &
                             RealVector_Add => Add, &
                             RealVector_GetValue_ => GetValue_, &
                             RealVector_Set => Set, &
                             RealVector_Display => Display, &
                             RealVector_Scale => SCAL

USE LagrangePolynomialUtility, ONLY: InterpolationPoint_
USE InputUtility

IMPLICIT NONE

REAL(DFP), PARAMETER :: one = 1.0_DFP, zero = 0.0_DFP, minus_one = -1.0_DFP, &
                        half = 0.5_DFP

CONTAINS

!----------------------------------------------------------------------------
!                              -                     obj_ImportFromToml1
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AssembleTanmat
CHARACTER(*), PARAMETER :: myName = "obj_AssembleTanmat()"
INTEGER(I4B) :: ielSpace, nrow, ncol, nns, nnt, tcon, con(256)
REAL(DFP) :: dt, dt_by_2, dts, dts_by_2, dx, dx_by_2, &
             dx2, dx2_by_2, two_by_dx, xij(1, 2)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%InitiateConnectivity()

CALL obj%InitiateFields(timeElemNum=timeElemNum)

dt = obj%timeElemLength(timeElemNum)
dt_by_2 = dt * 0.5_DFP
dts = dt * dt
dts_by_2 = dts * 0.5_DFP

CALL obj%SetQuadForTime(timeElemNum)
CALL obj%SetElemsdForTime(timeElemNum, tij)

CALL obj%GetMt(ans=obj%mt, nrow=nrow, ncol=ncol)
nnt = nrow

CALL obj%GetMtPlus(ans=obj%mtplus, nrow=nrow, ncol=ncol)

CALL obj%GetCt(ans=obj%ct, nrow=nrow, ncol=ncol)

CALL obj%GetWt(ans=obj%ct, nrow=nrow, ncol=ncol)

CALL obj%GetAt()

CALL obj%GetBt()

CALL obj%GetKt_Tilda(ans=obj%kt_tilda, nrow=nrow, ncol=ncol)

xij(1, 1) = obj%spaceDomain(1)

CALL CSRMatrix_Set(obj=obj%tanmat, VALUE=zero)

DO ielSpace = 1, obj%totalSpaceElements

  CALL obj%GetConnectivity(spaceElemNum=ielSpace, ans=con, tsize=tcon)

  dx = obj%spaceElemLength(ielSpace)
  dx_by_2 = dx * 0.5_DFP
  dx2 = dx * dx
  dx2_by_2 = dx2 * 0.5_DFP
  two_by_dx = 2.0_DFP / dx

  xij(1, 2) = xij(1, 1) + dx

  CALL obj%SetQuadForSpace(ielSpace)
  CALL obj%SetElemsdForSpace(ielSpace, xij)

  CALL obj%GetMs(ans=obj%ms, nrow=nrow, ncol=ncol)
  obj%ms(1:nrow, 1:ncol) = obj%density(ielSpace) * dx_by_2 &
                           * obj%ms(1:nrow, 1:ncol)

  nns = nrow

  CALL obj%GetKs(ans=obj%ks, nrow=nrow, ncol=ncol)
  obj%ks(1:nrow, 1:ncol) = obj%elasticModulus(ielSpace) * &
                           two_by_dx * obj%ks(1:nrow, 1:ncol)

  CALL obj%GetCs(ans=obj%cs, nrow=nrow, ncol=ncol, &
                 alpha=obj%rayleighAlpha(ielSpace), &
                 beta=obj%rayleighBeta(ielSpace))

  obj%ke = 0.0_DFP
  obj%mt(1:2, 1) = [dt / 8.0_DFP, -dt / 24.0_DFP]
  obj%mt(1:2, 2) = [-dt / 24.0_DFP, dt / 8.0_DFP]
  CALL OTimesTilda(a=obj%mt(1:nnt, 1:nnt), &
                   b=obj%ks(1:nns, 1:nns), &
                   ans=obj%ke, nrow=nrow, ncol=ncol, &
                   anscoeff=zero, scale=one)
  obj%mt(1:2, 1) = [-5.0_DFP / (6.0_DFP * dt), 5.0_DFP / (6.0_DFP * dt)]
  obj%mt(1:2, 2) = [5.0_DFP / (6.0_DFP * dt), -5.0_DFP / (6.0_DFP * dt)]
  CALL OTimesTilda(a=obj%mt(1:nnt, 1:nnt), &
                   b=obj%ms(1:nns, 1:nns), &
                   ans=obj%ke, nrow=nrow, ncol=ncol, &
                   anscoeff=one, scale=one)

  ! vu block and uv block
  CALL CSRMatrix_Add(obj=obj%tanmat, VALUE=obj%ke(1:nrow, 1:ncol), &
                     jNodeNum=con(1:tcon), iNodeNum=con(1:tcon), &
                     ivar=2_I4B, jvar=1_I4B, scale=one)
  CALL CSRMatrix_Add(obj=obj%tanmat, VALUE=obj%ke(1:nrow, 1:ncol), &
                     jNodeNum=con(1:tcon), iNodeNum=con(1:tcon), &
                     ivar=1_I4B, jvar=2_I4B, scale=minus_one)

  obj%ct(1:2, 1) = [half, 1.0_DFP / 3.0_DFP]
  obj%ct(1:2, 2) = [-1.0_DFP / 3.0_DFP, half]
  CALL OTimesTilda(a=obj%ct(1:nnt, 1:nnt), &
                   b=obj%ms(1:nns, 1:nns), &
                   ans=obj%ke, nrow=nrow, ncol=ncol, &
                   anscoeff=zero, scale=one)
  ! vv block
  CALL CSRMatrix_Add(obj=obj%tanmat, VALUE=obj%ke(1:nrow, 1:ncol), &
                     jNodeNum=con(1:tcon), iNodeNum=con(1:tcon), &
                     ivar=2_I4B, jvar=2_I4B, scale=one)

  CALL OTimesTilda(a=obj%ct(1:nnt, 1:nnt), &
                   b=obj%ks(1:nns, 1:nns), &
                   ans=obj%ke, nrow=nrow, ncol=ncol, &
                   anscoeff=zero, scale=one)

  ! block  uu block
  CALL CSRMatrix_Add(obj=obj%tanmat, VALUE=obj%ke(1:nrow, 1:ncol), &
                     jNodeNum=con(1:tcon), iNodeNum=con(1:tcon), &
                     ivar=1_I4B, jvar=1_I4B, scale=one)

  xij(1, 1) = xij(1, 2)

END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_AssembleTanmat

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AssembleRHS
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AssembleRHS()"
#endif

INTEGER(I4B) :: con(256), ielSpace, nrow, ncol, nns, nnt, tsize, ii

REAL(DFP) :: dx, dx_by_2, two_by_dx, dt, minus_dt_by_2, &
             f1(MAX_ORDER_SPACE + 1), &
             f2(MAX_ORDER_SPACE + 1), &
             v0(MAX_ORDER_SPACE + 1), &
             u0(MAX_ORDER_SPACE + 1), &
             xij(1, 2), scale

INTEGER(I4B), PARAMETER :: conversion(1) = [NONE]

LOGICAL(LGT) :: isTractionLeft, isTractionRight

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL RealVector_Set(obj=obj%rhs, VALUE=zero)

nnt = obj%elemsdForTime%nns

isTractionRight = ASSOCIATED(obj%tractionRight)
isTractionLeft = ASSOCIATED(obj%tractionLeft)

dt = obj%timeElemLength(timeElemNum)
minus_dt_by_2 = minus_one * dt * 0.5_DFP

xij(1, 1) = obj%spaceDomain(1)

DO ielSpace = 1, obj%totalSpaceElements

  dx = obj%spaceElemLength(ielSpace)
  dx_by_2 = dx * 0.5_DFP
  two_by_dx = 2.0_DFP / dx

  xij(1, 2) = xij(1, 1) + dx

  CALL obj%GetConnectivity(spaceElemNum=ielSpace, ans=con, tsize=nns)

  CALL obj%SetQuadForSpace(ielSpace)
  CALL obj%SetElemsdForSpace(ielSpace, xij)

  obj%rhse = 0.0_DFP

  CALL obj%GetMs(ans=obj%ms, nrow=nrow, ncol=ncol)
  obj%ms(1:nrow, 1:ncol) = obj%density(ielSpace) * &
                           dx_by_2 * obj%ms(1:nrow, 1:ncol)

  CALL obj%GetKs(ans=obj%ks, nrow=nrow, ncol=ncol)
  obj%ks(1:nrow, 1:ncol) = obj%elasticModulus(ielSpace) * &
                           two_by_dx * obj%ks(1:nrow, 1:ncol)

  CALL RealVector_GetValue_(obj=obj%v0, nodenum=con(1:nns), VALUE=v0, &
                            tsize=nns)

  CALL RealVector_GetValue_(obj=obj%u0, nodenum=con(1:nns), VALUE=u0, &
                            tsize=nns)

  f1(1:nns) = MATMUL(obj%ms(1:nrow, 1:ncol), v0(1:ncol))
  f2(1:nns) = MATMUL(obj%ks(1:nrow, 1:ncol), u0(1:ncol))

  CALL OTimesTilda(a=obj%timeShapeFuncBndy(1:nnt, 1), b=f1(1:nns), ans=obj%rhse, &
                   tsize=tsize, anscoeff=zero, scale=one)

  CALL obj%GetBodyForce(ans=obj%rhse, tsize=tsize, spaceElemNum=ielSpace, &
                        timeElemNum=timeElemNum, anscoeff=one, scale=one)
  ! momentum
  DO ii = 1, nnt
    CALL RealVector_Add(obj=obj%rhs, VALUE=obj%rhse((ii-1)*nns+1:ii*nns),  &
                        ivar=2_I4B, scale=one, dofobj=obj%dof, &
                        nodenum=con(1:nns), spaceCompo=1, timeCompo=ii)
  END DO

  CALL OTimesTilda(a=obj%timeShapeFuncBndy(1:nnt, 1), b=f2(1:nns), &
                   ans=obj%rhse, tsize=tsize, anscoeff=zero, scale=one)
  ! kinematic
  DO ii = 1, nnt
    CALL RealVector_Add(obj=obj%rhs, VALUE=obj%rhse((ii-1)*nns+1:ii*nns),  &
                        ivar=1_I4B, scale=one, dofobj=obj%dof, &
                        nodenum=con(1:nns), spaceCompo=1, timeCompo=ii)
  END DO

  xij(1, 1) = xij(1, 2)

END DO

! Traction right
IF (isTractionRight) THEN
  CALL obj%GetConnectivity(spaceElemNum=obj%totalSpaceElements, ans=con, tsize=nns)
  CALL obj%GetTractionRight(ans=obj%rhse, tsize=tsize, &
                            timeElemNum=timeElemNum, anscoeff=zero, scale=one)

  DO ii = 1, nnt
    CALL RealVector_Add(obj=obj%rhs, VALUE=obj%rhse((ii-1)*nns+1:ii*nns),  &
                        ivar=2_I4B, scale=one, dofobj=obj%dof, &
                        nodenum=con(1:nns), spaceCompo=1, timeCompo=ii)
  END DO

  CALL GetTractionRight_forBubble(obj=obj, ans=obj%rhse, tsize=tsize, &
                            timeElemNum=timeElemNum, anscoeff=zero, scale=one)
  DO ii = 1, nnt
    scale = -5.0_DFP / 8.0_DFP
    CALL RealVector_Add(obj=obj%rhs, VALUE=obj%rhse(1:nns), &
                        ivar=2_I4B, scale=scale, dofobj=obj%dof, &
                        nodenum=con(1:nns), spaceCompo=1, timeCompo=ii)
    scale = minus_one**ii * 5.0_DFP / (4.0_DFP * dt)
    CALL RealVector_Add(obj=obj%rhs, VALUE=obj%rhse(1:nns), &
                        ivar=1_I4B, scale=scale, dofobj=obj%dof, &
                        nodenum=con(1:nns), spaceCompo=1, timeCompo=ii)
  END DO

END IF

! Traction left
IF (isTractionLeft) THEN
  CALL obj%GetConnectivity(spaceElemNum=1, ans=con, tsize=nns)
  CALL obj%GetTractionLeft(ans=obj%rhse, tsize=tsize, &
                           timeElemNum=timeElemNum, anscoeff=zero, scale=one)

  DO ii = 1, nnt
    CALL RealVector_Add(obj=obj%rhs, VALUE=obj%rhse((ii-1)*nns+1:ii*nns),  &
                        ivar=2_I4B, scale=one, dofobj=obj%dof, &
                        nodenum=con(1:nns), spaceCompo=1, timeCompo=ii)
  END DO

  CALL GetTractionLeft_forBubble(obj=obj, ans=obj%rhse, tsize=tsize, &
                            timeElemNum=timeElemNum, anscoeff=zero, scale=one)
  DO ii = 1, nnt
    scale = -5.0_DFP / 8.0_DFP
    CALL RealVector_Add(obj=obj%rhs, VALUE=obj%rhse(1:nns), &
                        ivar=2_I4B, scale=scale, dofobj=obj%dof, &
                        nodenum=con(1:nns), spaceCompo=1, timeCompo=ii)
    scale = minus_one**ii * 5.0_DFP / (4.0_DFP * dt)
    CALL RealVector_Add(obj=obj%rhs, VALUE=obj%rhse(1:nns), &
                        ivar=1_I4B, scale=scale, dofobj=obj%dof, &
                        nodenum=con(1:nns), spaceCompo=1, timeCompo=ii)
  END DO

END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_AssembleRHS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE GetTractionLeft_forBubble(obj, ans, tsize, timeElemNum, &
                                     anscoeff, scale)
  CLASS(ElastoDynamics1DUVSTFEM_), INTENT(INOUT) :: obj
  REAL(DFP), INTENT(INOUT) :: ans(:)
  INTEGER(I4B), INTENT(INOUT) :: tsize
  INTEGER(I4B), INTENT(IN) :: timeElemNum
  REAL(DFP), INTENT(IN) :: anscoeff, scale

  INTEGER(I4B) :: nns, nnt, nipt, ii, a, ipt
  REAL(DFP) :: r(10), args(1), dt_by_2
  LOGICAL(LGT) :: isTractionLeft

  isTractionLeft = ASSOCIATED(obj%tractionLeft)
  IF (.NOT. isTractionLeft) RETURN

  nns = obj%elemsdForSpace%nns
  nipt = obj%elemsdForTime%nips
  tsize = nns * 1
  ans(1:tsize) = ans(1:tsize) * anscoeff

  dt_by_2 = obj%timeElemLength(timeElemNum) * 0.5_DFP

  DO ipt = 1, nipt

    args(1) = obj%elemsdForTime%coord(1, ipt)
    r(1) = obj%elemsdForTime%ws(ipt) * dt_by_2
    CALL obj%tractionLeft%Get(val=r(2), args=args)
    r(3) = r(1) * r(2)

    DO a = 1, 1

      ! only for quad bubble
      r(4) = 2.0_DFP / 3.0_DFP
      r(5) = r(3) * r(4)

      DO ii = 1, nns

        r(6) = obj%spaceShapeFuncBndy(ii, 1)
        r(7) = scale * r(5) * r(6)

        ans(ii + (a - 1) * nns) = ans(ii + (a - 1) * nns) + r(7)

      END DO

    END DO

  END DO

END SUBROUTINE GetTractionLeft_forBubble

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE GetTractionRight_forBubble(obj, ans, tsize, timeElemNum, &
                                      anscoeff, scale)
  CLASS(ElastoDynamics1DUVSTFEM_), INTENT(INOUT) :: obj
  REAL(DFP), INTENT(INOUT) :: ans(:)
  INTEGER(I4B), INTENT(INOUT) :: tsize
  INTEGER(I4B), INTENT(IN) :: timeElemNum
  REAL(DFP), INTENT(IN) :: anscoeff, scale

  INTEGER(I4B) :: nns, nnt, nipt, ii, a, ipt
  REAL(DFP) :: r(10), args(1), dt_by_2
  LOGICAL(LGT) :: isTractionLeft

  isTractionLeft = ASSOCIATED(obj%tractionRight)
  IF (.NOT. isTractionLeft) RETURN

  nns = obj%elemsdForSpace%nns
  nipt = obj%elemsdForTime%nips
  tsize = nns * 1
  ans(1:tsize) = ans(1:tsize) * anscoeff

  dt_by_2 = obj%timeElemLength(timeElemNum) * 0.5_DFP

  DO ipt = 1, nipt

    args(1) = obj%elemsdForTime%coord(1, ipt)
    r(1) = obj%elemsdForTime%ws(ipt) * dt_by_2
    CALL obj%tractionRight%Get(val=r(2), args=args)
    r(3) = r(1) * r(2)

    DO a = 1, 1

      ! only for quad bubble
      r(4) = 2.0_DFP / 3.0_DFP
      r(5) = r(3) * r(4)

      DO ii = 1, nns

        r(6) = obj%spaceShapeFuncBndy(ii, 2)
        r(7) = scale * r(5) * r(6)

        ans(ii + (a - 1) * nns) = ans(ii + (a - 1) * nns) + r(7)

      END DO

    END DO

  END DO

END SUBROUTINE GetTractionRight_forBubble

!----------------------------------------------------------------------------
!                                                                    Solve
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Run
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Run()"
#endif

INTEGER(I4B) :: ielTime
REAL(DFP) :: x1, tij(1, 2)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

x1 = obj%spaceDomain(1)
tij(1, 1) = obj%timeDomain(1)

CALL obj%SetInitialVelocity()
CALL obj%SetInitialDisplacement()

CALL obj%WriteData()

DO ielTime = 1, obj%totalTimeElements
  CALL Display(tij(1, 1), myname//" t1: ")
  tij(1, 2) = tij(1, 1) + obj%timeElemLength(ielTime)
  CALL obj%AssembleTanmat(timeElemNum=ielTime, tij=tij)
  CALL obj%AssembleRHS(timeElemNum=ielTime, tij=tij)
  CALL obj%ApplyDirichletBC(timeElemNum=ielTime, tij=tij)
  CALL obj%Solve()
  CALL obj%Update()
  IF (MOD(ielTime, obj%outputFreq) .EQ. 0_I4B) &
    CALL obj%WriteData()
  tij(1, 1) = tij(1, 2)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Run

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods
