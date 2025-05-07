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

SUBMODULE(Abstract1DSTFEM_Class) ErrorMethods

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
!                                                                 EvalError
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_EvalErrorNorm
CHARACTER(*), PARAMETER :: myName = "obj_EvalErrorNorm()"

IF (obj%saveErrorNorm(1)) THEN
  SELECT CASE (obj%errorType(1)%slice(3, 4))
  CASE ("SP")
    obj%errorDisp(timeElemNum, 1) = GetErrorNorm_L2SP(obj=obj, &
                                       solution=obj%u0, reference=obj%refDisp)
  CASE ("ST")
    obj%errorDisp(timeElemNum, 1) = GetErrorNorm_L2ST(obj=obj, &
                                    solution=obj%sol, reference=obj%refDisp, &
                                                      tij=tij, disp=.TRUE.)
  CASE ("BO")
    obj%errorDisp(timeElemNum, 1) = GetErrorNorm_L2SP(obj=obj, &
                                       solution=obj%u0, reference=obj%refDisp)
    obj%errorDisp(timeElemNum, 2) = GetErrorNorm_L2ST(obj=obj, &
                                    solution=obj%sol, reference=obj%refDisp, &
                                                      tij=tij, disp=.TRUE.)
  END SELECT
END IF

IF (obj%saveErrorNorm(2)) THEN
  SELECT CASE (obj%errorType(2)%slice(3, 4))
  CASE ("SP")
    obj%errorVel(timeElemNum, 1) = GetErrorNorm_L2SP(obj=obj, &
                                        solution=obj%v0, reference=obj%refVel)
  CASE ("ST")
    obj%errorVel(timeElemNum, 1) = GetErrorNorm_L2ST(obj=obj, &
                                     solution=obj%sol, reference=obj%refVel, &
                                                     tij=tij, disp=.FALSE.)
  CASE ("BO")
    obj%errorVel(timeElemNum, 1) = GetErrorNorm_L2SP(obj=obj, &
                                        solution=obj%v0, reference=obj%refVel)
    obj%errorVel(timeElemNum, 2) = GetErrorNorm_L2ST(obj=obj, &
                                     solution=obj%sol, reference=obj%refVel, &
                                                     tij=tij, disp=.FALSE.)
  END SELECT
END IF

IF (obj%saveErrorNorm(3)) THEN
  ! cal error norm for acceleration field
END IF

END PROCEDURE obj_EvalErrorNorm

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION GetErrorNorm_L2SP(obj, solution, reference) RESULT(anorm)
  CLASS(Abstract1DSTFEM_), INTENT(inout) :: obj
  TYPE(RealVector_), INTENT(INOUT) :: solution
  CLASS(UserFunction_), INTENT(INOUT) :: reference
  REAL(DFP) :: anorm
  INTEGER(I4B) :: ielSpace, nns, iqs, tsizeQP
  INTEGER(I4B) :: con(MAX_ORDER_SPACE + 1)
  REAL(DFP) :: dx, xij(1, 2), args(2), weight
  REAL(DFP) :: sol0(MAX_ORDER_SPACE + 1), refQP
  REAL(DFP), ALLOCATABLE :: solQP(:)

  args = obj%currentTime

  anorm = 0.0_DFP

  xij(1, 1) = obj%spaceDomain(1)
  DO ielSpace = 1, obj%totalSpaceElements

    CALL obj%GetConnectivity(spaceElemNum=ielSpace, ans=con, tsize=nns)

    dx = obj%spaceElemLength(ielSpace)
    xij(1, 2) = xij(1, 1) + dx

    CALL obj%SetQuadForSpace(ielSpace)
    tsizeQP = obj%elemsdForSpace%nips
    CALL obj%SetElemsdForSpace(ielSpace, xij)

    CALL RealVector_GetValue_(obj=solution, nodenum=con(1:nns), VALUE=sol0, &
                              tsize=nns)
    ALLOCATE (solQP(tsizeQP))
    solQP = MATMUL(sol0(1:nns), obj%elemsdForSpace%N)

    DO iqs = 1, tsizeQP
      args(1) = obj%elemsdForSpace%coord(1, iqs)
      CALL reference%Get(val=refQP, args=args)
      weight = obj%elemsdForSpace%ws(iqs) * obj%elemsdForSpace%js(iqs)
      anorm = anorm + (refQP - solQP(iqs))**2 * weight
    END DO

    xij(1, 1) = xij(1, 2)
    DEALLOCATE (solQP)

  END DO

  anorm = SQRT(anorm)

END FUNCTION GetErrorNorm_L2SP

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION GetErrorNorm_L2ST(obj, solution, reference, tij, disp) RESULT(anorm)
  CLASS(Abstract1DSTFEM_), INTENT(inout) :: obj
  TYPE(RealVector_), INTENT(INOUT) :: solution
  CLASS(UserFunction_), INTENT(INOUT) :: reference
  REAL(DFP), INTENT(IN) :: tij(1, 2)
  LOGICAL(LGT), OPTIONAL, INTENT(IN) :: disp
  REAL(DFP) :: anorm
  INTEGER(I4B) :: ielSpace, nns, nnt, tqs, tqt, &
                  iqs, iqt, ii
  INTEGER(I4B) :: con(MAX_ORDER_SPACE + 1)
  REAL(DFP) :: dx, dt, xij(1, 2), args(2), weight0, weight1
  REAL(DFP) :: solST(MAX_ORDER_TIME + 1, MAX_ORDER_SPACE + 1), refQP, &
               u0(MAX_ORDER_SPACE + 1)
  REAL(DFP), ALLOCATABLE :: solQPST(:, :), u0QP(:)
  LOGICAL(LGT) :: disp0

  disp0 = Input(default=.FALSE., option=disp)
  anorm = 0.0_DFP

  nnt = obj%elemsdForTime%nns
  tqt = obj%elemsdForTime%nips
  dt = tij(1, 2) - tij(1, 1)

  xij(1, 1) = obj%spaceDomain(1)
  DO ielSpace = 1, obj%totalSpaceElements

    CALL obj%GetConnectivity(spaceElemNum=ielSpace, ans=con, tsize=nns)

    dx = obj%spaceElemLength(ielSpace)
    xij(1, 2) = xij(1, 1) + dx

    CALL obj%SetQuadForSpace(ielSpace)
    CALL obj%SetElemsdForSpace(ielSpace, xij)
    tqs = obj%elemsdForSpace%nips

    DO ii = 1, nnt
      CALL RealVector_GetValue_(obj=solution, dofobj=obj%dof, ivar=1_I4B, &
                                idof=ii, nodenum=con(1:nns), &
                                VALUE=solST(ii, 1:nns), tsize=nns)
    END DO

    ALLOCATE (solQPST(tqt, tqs))

    IF (disp) THEN
      CALL RealVector_GetValue_(obj=obj%um1, nodenum=con(1:nns), &
                                VALUE=u0(1:nns), tsize=nns)
      ALLOCATE (u0QP(tqs))
      u0QP = MATMUL(u0(1:nns), obj%elemsdForSpace%N)
      DO ii = 1, tqt
        solQPST(ii, :) = u0QP
      END DO

      solQPST = solQPST + dt * MATMUL(TRANSPOSE(obj%bt(1:nnt, 1:tqt)), &
                            MATMUL(solST(1:nnt, 1:nns), obj%elemsdForSpace%N))

      DEALLOCATE (u0QP)
    ELSE
      solQPST = MATMUL(TRANSPOSE(obj%elemsdForTime%N), &
                       MATMUL(solST(1:nnt, 1:nns), obj%elemsdForSpace%N))
    END IF

    DO iqs = 1, tqs
      args(1) = obj%elemsdForSpace%coord(1, iqs)
      weight0 = obj%elemsdForSpace%ws(iqs) * obj%elemsdForSpace%js(iqs)
      DO iqt = 1, tqt
        args(2) = obj%elemsdForTime%coord(1, iqt)
        CALL reference%Get(val=refQP, args=args)
        weight1 = weight0 * obj%elemsdForTime%ws(iqt) * &
                  obj%elemsdForTime%js(iqt)
        anorm = anorm + (refQP - solQPST(iqt, iqs))**2 * weight1
      END DO
    END DO

    xij(1, 1) = xij(1, 2)
    DEALLOCATE (solQPST)

  END DO

  anorm = SQRT(anorm)

END FUNCTION GetErrorNorm_L2ST

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ErrorMethods
