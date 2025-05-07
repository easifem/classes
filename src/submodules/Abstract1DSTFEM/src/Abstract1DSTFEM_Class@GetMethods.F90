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

SUBMODULE(Abstract1DSTFEM_Class) GetMethods

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
!                                                            getConnectivity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetConnectivity
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetConnectivity()"
#endif

INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = 0

DO ii = obj%conIA(spaceElemNum), obj%conIA(spaceElemNum + 1) - 1
  tsize = tsize + 1
  ans(tsize) = obj%conJA(ii)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetConnectivity

!----------------------------------------------------------------------------
!                                                                 obj_GetCt
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetCt
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetCt()"
#endif

INTEGER(I4B) :: ips, ii, jj
REAL(DFP) :: scale

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nrow = obj%elemsdForTime%nns
ncol = nrow
ans(1:nrow, 1:ncol) = 0.0

DO ips = 1, obj%elemsdForTime%nips
  scale = obj%elemsdForTime%ws(ips) * obj%elemsdForTime%thickness(ips)
  CALL OuterProd_(a=obj%elemsdForTime%N(1:nrow, ips), &
                  b=obj%elemsdForTime%dNdXi(1:ncol, 1, ips), &
                  ans=ans, nrow=ii, ncol=jj, anscoeff=1.0_DFP, scale=scale)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetCt

!----------------------------------------------------------------------------
!                                                                      GetMt
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMt
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetMt()"
#endif

INTEGER(I4B) :: ips, ii, jj
REAL(DFP) :: scale

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nrow = obj%elemsdForTime%nns
ncol = nrow
ans(1:nrow, 1:ncol) = 0.0

DO ips = 1, obj%elemsdForTime%nips
  scale = obj%elemsdForTime%ws(ips) * obj%elemsdForTime%thickness(ips)
  CALL OuterProd_(a=obj%elemsdForTime%N(1:nrow, ips), &
                  b=obj%elemsdForTime%N(1:ncol, ips), &
                  ans=ans, nrow=ii, ncol=jj, anscoeff=1.0_DFP, scale=scale)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetMt

!----------------------------------------------------------------------------
!                                                                  GetMtPlus
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMtPlus
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetMtPlus()"
#endif
INTEGER(I4B) :: ii, jj
REAL(DFP) :: scale

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nrow = obj%elemsdForTime%nns
ncol = nrow
ans(1:nrow, 1:ncol) = 0.0

scale = 1.0_DFP
CALL OuterProd_(a=obj%timeShapeFuncBndy(1:nrow, 1), &
                b=obj%timeShapeFuncBndy(1:ncol, 1), &
                ans=ans, nrow=ii, ncol=jj, anscoeff=0.0_DFP, scale=scale)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetMtPlus

!----------------------------------------------------------------------------
!                                                                GetKt_Tilda
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetKt_Tilda
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetKt_Tilda()"
#endif

INTEGER(I4B) :: ips, ii, jj
REAL(DFP) :: scale

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nrow = obj%elemsdForTime%nns
ncol = nrow
ans(1:nrow, 1:ncol) = 0.0

DO ips = 1, obj%elemsdForTime%nips

  scale = obj%elemsdForTime%ws(ips) * obj%elemsdForTime%thickness(ips)

  CALL OuterProd_(a=obj%elemsdForTime%N(1:nrow, ips), &
                  b=obj%bt(1:ncol, ips), &
                  ans=ans, nrow=ii, ncol=jj, anscoeff=1.0_DFP, scale=scale)

END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetKt_Tilda

!----------------------------------------------------------------------------
!                                                                    GetWt
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetWt
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetWt()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nrow = obj%elemsdForTime%nns
ncol = nrow

obj%wt(1:nrow, 1:ncol) = obj%ct(1:nrow, 1:ncol) + obj%mtplus(1:nrow, 1:ncol)
CALL GetInvMat(obj%wt(1:nrow, 1:ncol))
obj%wmt(1:nrow, 1:ncol) = MATMUL(obj%wt(1:nrow, 1:ncol), obj%mt(1:nrow, 1:ncol))
obj%wmt(1:nrow, 1:ncol) = TRANSPOSE(obj%wmt(1:nrow, 1:ncol))

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetWt

!----------------------------------------------------------------------------
!                                                                    GetAt
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetAt
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetAt()"
#endif

INTEGER(I4B) :: nrow, ncol, ii, tsize
REAL(DFP) :: temp(MAX_ORDER_TIME), areal

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nrow = obj%elemsdForTime%nns
ncol = nrow

tsize = obj%elemsdForTime%nips

temp(1:nrow) = MATMUL(obj%wt(1:nrow, 1:ncol), obj%timeShapeFuncBndy(1:ncol, 1))

obj%tat(1:nrow) = 0.0_DFP

obj%at_right = DOT_PRODUCT(obj%timeShapeFuncBndy(1:nrow, 2), temp(1:nrow))

DO ii = 1, tsize
  obj%at(ii) = DOT_PRODUCT(obj%elemsdForTime%N(1:nrow, ii), temp(1:nrow))
  areal = obj%at(ii) * obj%elemsdForTime%ws(ii)
  obj%tat(1:nrow) = obj%tat(1:nrow) + obj%elemsdForTime%N(1:nrow, ii) * areal
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetAt

!----------------------------------------------------------------------------
!                                                                    GetBt
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetBt
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetBt()"
#endif

INTEGER(I4B) :: nrow, ncol

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nrow = obj%elemsdForTime%nns
ncol = obj%elemsdForTime%nips
obj%bt(1:nrow, 1:ncol) = MATMUL(obj%wmt(1:nrow, 1:nrow), &
                                obj%elemsdForTime%N(1:nrow, 1:ncol))

obj%bt(1:nrow, 1:ncol) = obj%bt(1:nrow, 1:ncol) * half

obj%bt_right(1:nrow) = MATMUL(obj%wmt(1:nrow, 1:nrow), &
                              obj%timeShapeFuncBndy(1:nrow, 2))

obj%bt_right(1:nrow) = obj%bt_right(1:nrow) * half

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetBt

!----------------------------------------------------------------------------
!                                                                     GetMs
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMs
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetMs()"
#endif

INTEGER(I4B) :: ips, ii, jj
REAL(DFP) :: scale

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nrow = obj%elemsdForSpace%nns
ncol = nrow
ans(1:nrow, 1:ncol) = 0.0

DO ips = 1, obj%elemsdForSpace%nips
  scale = obj%elemsdForSpace%ws(ips) * obj%elemsdForSpace%thickness(ips)
  CALL OuterProd_(a=obj%elemsdForSpace%N(1:nrow, ips), &
                  b=obj%elemsdForSpace%N(1:ncol, ips), &
                  ans=ans, nrow=ii, ncol=jj, anscoeff=1.0_DFP, scale=scale)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetMs

!----------------------------------------------------------------------------
!                                                                    GetKs
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetKs
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetKs()"
#endif

INTEGER(I4B) :: ips, ii, jj
REAL(DFP) :: scale

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nrow = obj%elemsdForSpace%nns
ncol = nrow
ans(1:nrow, 1:ncol) = 0.0

DO ips = 1, obj%elemsdForSpace%nips
  scale = obj%elemsdForSpace%ws(ips) * obj%elemsdForSpace%thickness(ips)
  CALL OuterProd_(a=obj%elemsdForSpace%dNdXi(1:nrow, 1, ips), &
                  b=obj%elemsdForSpace%dNdXi(1:ncol, 1, ips), &
                  ans=ans, nrow=ii, ncol=jj, anscoeff=1.0_DFP, scale=scale)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetKs

!----------------------------------------------------------------------------
!                                                                     GetCs
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetCs
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetCs()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nrow = obj%elemsdForSpace%nns
ncol = nrow
ans(1:nrow, 1:ncol) = alpha * obj%ks(1:nrow, 1:ncol) &
                      + beta * obj%ms(1:nrow, 1:ncol)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetCs

!----------------------------------------------------------------------------
!                                                              GetBodyForce
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetBodyForce
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetBodyForce()"
#endif

INTEGER(I4B) :: nns, nnt, nips, nipt, ii, a, ipt, ips
REAL(DFP) :: r(10), args(2), t, js_jt
LOGICAL(LGT) :: isBodyForce

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isBodyForce = ASSOCIATED(obj%bodyForce)
IF (.NOT. isBodyForce) RETURN

nns = obj%elemsdForSpace%nns
nnt = obj%elemsdForTime%nns
nips = obj%elemsdForSpace%nips
nipt = obj%elemsdForTime%nips
tsize = nns * nnt
ans(1:tsize) = ans(1:tsize) * anscoeff

js_jt = obj%spaceElemLength(spaceElemNum) * obj%timeElemLength(timeElemNum) &
        * 0.25_DFP * obj%density(spaceElemNum)

DO ipt = 1, nipt

  t = obj%elemsdForTime%coord(1, ipt)
  args(2) = t

  r(1) = obj%elemsdForTime%ws(ipt) * js_jt

  DO ips = 1, nips

    args(1) = obj%elemsdForSpace%coord(1, ips)

    CALL obj%bodyForce%Get(val=r(2), args=args)

    r(3) = obj%elemsdForSpace%ws(ips)

    r(4) = r(1) * r(2) * r(3)

    DO a = 1, nnt

      r(5) = obj%elemsdForTime%N(a, ipt)

      r(6) = r(4) * r(5)

      DO ii = 1, nns

        r(7) = obj%elemsdForSpace%N(ii, ips)

        r(8) = scale * r(6) * r(7)

        ans(ii + (a - 1) * nns) = ans(ii + (a - 1) * nns) + r(8)

      END DO

    END DO

  END DO

END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetBodyForce

!----------------------------------------------------------------------------
!                                                            GetTractionLeft
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTractionLeft
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTractionLeft()"
#endif

INTEGER(I4B) :: nns, nnt, nipt, ii, a, ipt
REAL(DFP) :: r(10), args(1), dt_by_2
LOGICAL(LGT) :: isTractionLeft

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isTractionLeft = ASSOCIATED(obj%tractionLeft)
IF (.NOT. isTractionLeft) RETURN

nns = obj%elemsdForSpace%nns
nnt = obj%elemsdForTime%nns
nipt = obj%elemsdForTime%nips
tsize = nns * nnt
ans(1:tsize) = ans(1:tsize) * anscoeff

dt_by_2 = obj%timeElemLength(timeElemNum) * 0.5_DFP

DO ipt = 1, nipt

  args(1) = obj%elemsdForTime%coord(1, ipt)
  r(1) = obj%elemsdForTime%ws(ipt) * dt_by_2
  CALL obj%tractionLeft%Get(val=r(2), args=args)
  r(3) = r(1) * r(2)

  DO a = 1, nnt

    r(4) = obj%elemsdForTime%N(a, ipt)
    r(5) = r(3) * r(4)

    DO ii = 1, nns

      r(6) = obj%spaceShapeFuncBndy(ii, 1)
      r(7) = scale * r(5) * r(6)

      ans(ii + (a - 1) * nns) = ans(ii + (a - 1) * nns) + r(7)

    END DO

  END DO

END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetTractionLeft

!----------------------------------------------------------------------------
!                                                          GetTractionRight
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTractionRight
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTractionRight()"
#endif

INTEGER(I4B) :: nns, nnt, nipt, ii, a, ipt
REAL(DFP) :: r(10), args(1), dt_by_2
LOGICAL(LGT) :: isTractionRight

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isTractionRight = ASSOCIATED(obj%tractionRight)
IF (.NOT. isTractionRight) RETURN

nns = obj%elemsdForSpace%nns
nnt = obj%elemsdForTime%nns
nipt = obj%elemsdForTime%nips
tsize = nns * nnt
ans(1:tsize) = ans(1:tsize) * anscoeff

dt_by_2 = obj%timeElemLength(timeElemNum) * 0.5_DFP

DO ipt = 1, nipt

  args(1) = obj%elemsdForTime%coord(1, ipt)
  r(1) = obj%elemsdForTime%ws(ipt) * dt_by_2
  CALL obj%tractionRight%Get(val=r(2), args=args)
  r(3) = r(1) * r(2)

  DO a = 1, nnt

    r(4) = obj%elemsdForTime%N(a, ipt)
    r(5) = r(3) * r(4)

    DO ii = 1, nns

      r(6) = obj%spaceShapeFuncBndy(ii, 2)
      r(7) = scale * r(5) * r(6)

      ans(ii + (a - 1) * nns) = ans(ii + (a - 1) * nns) + r(7)

    END DO

  END DO

END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetTractionRight

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetMethods
