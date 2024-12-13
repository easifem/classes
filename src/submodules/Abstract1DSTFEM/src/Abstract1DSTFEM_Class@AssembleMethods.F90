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

SUBMODULE(Abstract1DSTFEM_Class) AssembleMethods

USE Lapack_Method, ONLY: GetInvMat, SymLinSolve

USE TomlUtility, ONLY: GetValue, GetValue_

USE StringUtility, ONLY: UpperCase

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
!                                                             AssembleTanmat
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

  obj%ke = 0.0_DFP

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

  CALL OTimesTilda(a=obj%ct(1:nnt, 1:nnt), &
                   b=obj%ms(1:nns, 1:nns), &
                   ans=obj%ke, nrow=nrow, ncol=ncol, &
                   anscoeff=zero, scale=one)

  CALL OTimesTilda(a=obj%mtplus(1:nnt, 1:nnt), &
                   b=obj%ms(1:nns, 1:nns), &
                   ans=obj%ke, nrow=nrow, ncol=ncol, &
                   anscoeff=one, scale=one)

  CALL OTimesTilda(a=obj%kt_tilda(1:nnt, 1:nnt), &
                   b=obj%ks(1:nns, 1:nns), &
                   ans=obj%ke, nrow=nrow, ncol=ncol, &
                   anscoeff=one, scale=dts_by_2)

  CALL OTimesTilda(a=obj%mt(1:nnt, 1:nnt), &
                   b=obj%cs(1:nns, 1:nns), &
                   ans=obj%ke, nrow=nrow, ncol=ncol, &
                   anscoeff=one, scale=dt_by_2)

  CALL CSRMatrix_Add(obj=obj%tanmat, VALUE=obj%ke(1:nrow, 1:ncol), &
                     scale=one, storageFMT=DOF_FMT, nodenum=con(1:tcon))

  xij(1, 1) = xij(1, 2)

END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_AssembleTanmat

!----------------------------------------------------------------------------
!                                                                AssembleRHS
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AssembleRHS
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_AssembleRHS()"
#endif

INTEGER(I4B) :: con(256), ielSpace, nrow, ncol, nns, nnt, tsize

REAL(DFP) :: dx, dx_by_2, two_by_dx, dt, minus_dt_by_2, &
             f1(MAX_ORDER_SPACE + 1), &
             f2(MAX_ORDER_SPACE + 1), &
             v0(MAX_ORDER_SPACE + 1), &
             u0(MAX_ORDER_SPACE + 1), &
             xij(1, 2)

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

  CALL OTimesTilda(a=obj%tat(1:nnt), b=f2(1:nns), ans=obj%rhse, &
                   tsize=tsize, anscoeff=one, scale=minus_dt_by_2)

  !! Body force
  CALL obj%GetBodyForce(ans=obj%rhse, tsize=tsize, spaceElemNum=ielSpace, &
                        timeElemNum=timeElemNum, anscoeff=one, scale=one)

  ! CALL RealVector_Add(obj=)
  CALL RealVector_Add(obj=obj%rhs, VALUE=obj%rhse(1:tsize), &
         scale=one, dofobj=obj%dof, nodenum=con(1:nns), conversion=conversion)

  xij(1, 1) = xij(1, 2)

END DO

! Traction right
IF (isTractionRight) THEN
  CALL obj%GetTractionRight(ans=obj%rhse, tsize=tsize, &
                            timeElemNum=timeElemNum, anscoeff=zero, scale=one)

  CALL RealVector_Add(obj=obj%rhs, VALUE=obj%rhse(1:tsize), &
         scale=one, dofobj=obj%dof, nodenum=con(1:nns), conversion=conversion)

END IF

! Traction left
IF (isTractionLeft) THEN
  CALL obj%GetTractionLeft(ans=obj%rhse, tsize=tsize, &
                           timeElemNum=timeElemNum, anscoeff=zero, scale=one)

  CALL RealVector_Add(obj=obj%rhs, VALUE=obj%rhse(1:tsize), &
         scale=one, dofobj=obj%dof, nodenum=con(1:nns), conversion=conversion)

END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_AssembleRHS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE AssembleMethods
