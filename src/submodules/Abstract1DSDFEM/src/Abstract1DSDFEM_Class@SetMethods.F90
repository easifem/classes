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

SUBMODULE(Abstract1DSDFEM_Class) SetMethods

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

IMPLICIT NONE

REAL(DFP), PARAMETER :: one = 1.0_DFP, zero = 0.0_DFP, minus_one = -1.0_DFP, &
                        half = 0.5_DFP

CONTAINS

!----------------------------------------------------------------------------
!                                                                   obj_Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set()"
#endif

INTEGER(I4B) :: tnodes

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%currentTimeStep = 1
obj%currentTime = obj%timeRange(1)

CALL SetTotalDOFSpace(obj=obj)

tnodes = obj%totalVertexDOFSpace + obj%totalEdgeDOFSpace
CALL RealVector_Initiate(obj%u0, tnodes)
CALL RealVector_Initiate(obj%a0, tnodes)
CALL RealVector_Initiate(obj%v0, tnodes)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Set

!----------------------------------------------------------------------------
!                                                           SetTotalDOFSpace
!----------------------------------------------------------------------------

SUBROUTINE SetTotalDOFSpace(obj)
  CLASS(Abstract1DSDFEM_), INTENT(INOUT) :: obj

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "SetTotalDOFSpace()"
#endif

  INTEGER(I4B) :: iel

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL Reallocate(obj%totalDOFSpace, obj%totalSpaceElements)

  obj%totalVertexDOFSpace = obj%totalSpaceNodes
  obj%totalEdgeDOFSpace = 0

  DO iel = 1, obj%totalSpaceElements
    obj%totalDOFSpace(iel) = obj%spaceOrder(iel) + 1

    IF (obj%totalDOFSpace(iel) .GE. 2) THEN
      obj%totalEdgeDOFSpace = obj%totalEdgeDOFSpace &
                              + obj%totalDOFSpace(iel) - 2
    END IF
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE SetTotalDOFSpace

!----------------------------------------------------------------------------
!                                                            SetQuadForSpace
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetQuadForSpace
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetQuadForSpace()"
#endif

INTEGER(I4B) :: order, integralOrder

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

order = obj%spaceOrder(spaceElemNum)
integralOrder = 2 * order

CALL QuadPoint_Initiate(obj=obj%quadForSpace, elemType=elem%line, &
                        domainName="B", order=integralOrder, &
                        quadratureType=obj%quadTypeForSpace)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetQuadForSpace

!----------------------------------------------------------------------------
!                                                          SetElemsdForSpace
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetElemsdForSpace
CHARACTER(*), PARAMETER :: myName = "obj_SetElemsdForSpace()"
INTEGER(I4B) :: nips, nns, cellOrder(1), order, cellOrient(1)
REAL(DFP) :: refElemCoord(1, 2)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

order = obj%spaceOrder(spaceElemNum)
nips = Quad_Size(obj%quadForSpace, 2)
nns = obj%totalDOFSpace(spaceElemNum)
refElemCoord(1, 1) = -1.0_DFP
refElemCoord(1, 2) = 1.0_DFP
cellOrient = 1

CALL Elemsd_Allocate(obj=obj%elemsdForSpace, nsd=1_I4B, xidim=1_I4B, &
                     nns=nns, nips=nips)

CALL LagrangeElemShapeData(obj=obj%linElemsdForSpace, &
                           quad=obj%quadForSpace, &
                           nsd=obj%elemsdForSpace%nsd, &
                           xidim=obj%elemsdForSpace%xidim, &
                           elemtype=elem%line, &
                           refelemCoord=refelemCoord, &
                           domainName="B", &
                           order=1_I4B)

SELECT CASE (obj%baseInterpolationForSpace)
CASE ("LAGR")

  CALL LagrangeElemShapeData(obj=obj%elemsdForSpace, &
                             quad=obj%quadForSpace, &
                             nsd=obj%elemsdForSpace%nsd, &
                             xidim=obj%elemsdForSpace%xidim, &
                             elemtype=elem%line, &
                             refelemCoord=refelemCoord, &
                             domainName="B", &
                             order=order, &
                             ipType=obj%ipTypeForSpace, &
                             basisType=obj%baseTypeForSpace)

  obj%spaceShapeFuncBndy(1:nns, 1) = 0.0_DFP
  obj%spaceShapeFuncBndy(1, 1) = 1.0_DFP

  obj%spaceShapeFuncBndy(1:nns, 2) = 0.0_DFP
  obj%spaceShapeFuncBndy(2, 2) = 1.0_DFP

CASE ("HIER", "HEIR")

  cellOrder = order
  CALL HierarchicalElemShapeData(obj=obj%elemsdForSpace, &
                                 quad=obj%quadForSpace, &
                                 nsd=obj%elemsdForSpace%nsd, &
                                 xidim=obj%elemsdForSpace%xidim, &
                                 elemtype=elem%line, &
                                 refelemCoord=refelemCoord, &
                                 domainName="B", &
                                 cellOrder=cellOrder, &
                                 cellOrient=cellOrient)

  obj%spaceShapeFuncBndy(1:nns, 1) = 0.0_DFP
  obj%spaceShapeFuncBndy(1, 1) = 1.0_DFP

  obj%spaceShapeFuncBndy(1:nns, 2) = 0.0_DFP
  obj%spaceShapeFuncBndy(2, 2) = 1.0_DFP

CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    'no case found for baseInterpolationForSpace')

END SELECT

CALL Elemsd_Set(obj=obj%elemsdForSpace, val=xij, &
                N=obj%linElemsdForSpace%N(1:2, 1:nips), &
                dNdXi=obj%linElemsdForSpace%dNdXi(1:2, 1:1, 1:nips))

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetElemsdForSpace

!----------------------------------------------------------------------------
!                                                      setInitialAcceleration
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetInitialAcceleration
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetInitialAcceleration()"
#endif

LOGICAL(LGT) :: isok
INTEGER(I4B) :: ii, nns, nips, jj, ielSpace, con(256), tcon
REAL(DFP) :: xij(1, 2), val(MAX_ORDER_SPACE + 1), dx, &
             coeff(MAX_ORDER_SPACE * 2 + 2), rhs(MAX_ORDER_SPACE + 1)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = ASSOCIATED(obj%initialAcc)
IF (.NOT. isok) THEN
  CALL RealVector_Set(obj=obj%a0, VALUE=zero)
  RETURN
END IF

CALL obj%InitiateConnectivity()

xij(1, 1) = obj%spaceDomain(1)

DO ielSpace = 1, obj%totalSpaceElements

  CALL obj%GetConnectivity(spaceElemNum=ielSpace, ans=con, tsize=tcon)

  dx = obj%spaceElemLength(ielSpace)
  xij(1, 2) = xij(1, 1) + dx

  CALL obj%SetQuadForSpace(ielSpace)
  CALL obj%SetElemsdForSpace(ielSpace, xij)
  CALL obj%GetMs(ans=obj%ms, nrow=ii, ncol=jj)

  nns = obj%elemsdForSpace%nns
  nips = obj%elemsdForSpace%nips

  DO ii = 1, nips
    CALL obj%initialAcc%Get(val=coeff(ii), &
                            args=obj%elemsdForSpace%coord(1, ii:ii))
  END DO

  rhs(1:nns) = 0.0

  DO jj = 1, nns
    DO ii = 1, nips
      rhs(jj) = rhs(jj) + obj%elemsdForSpace%N(jj, ii) &
                * obj%elemsdForSpace%ws(ii) * coeff(ii)
    END DO
  END DO

  CALL SymLinSolve(X=val(1:nns), A=obj%ms(1:nns, 1:nns), &
                   B=rhs(1:nns))

  CALL RealVector_Set(obj=obj%a0, nodenum=con(1:nns), VALUE=val(1:nns))

  xij(1, 1) = xij(1, 2)

END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetInitialAcceleration

!----------------------------------------------------------------------------
!                                                         SetInitialVelocity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetInitialVelocity
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetInitialVelocity()"
#endif

LOGICAL(LGT) :: isok
INTEGER(I4B) :: ii, nns, nips, jj, ielSpace, con(256), tcon
REAL(DFP) :: xij(1, 2), val(MAX_ORDER_SPACE + 1), dx, &
             coeff(MAX_ORDER_SPACE * 2 + 2), rhs(MAX_ORDER_SPACE + 1)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = ASSOCIATED(obj%initialVel)
IF (.NOT. isok) THEN
  CALL RealVector_Set(obj=obj%v0, VALUE=zero)
  RETURN
END IF

CALL obj%InitiateConnectivity()

xij(1, 1) = obj%spaceDomain(1)

DO ielSpace = 1, obj%totalSpaceElements

  CALL obj%GetConnectivity(spaceElemNum=ielSpace, ans=con, tsize=tcon)

  dx = obj%spaceElemLength(ielSpace)
  xij(1, 2) = xij(1, 1) + dx

  CALL obj%SetQuadForSpace(ielSpace)
  CALL obj%SetElemsdForSpace(ielSpace, xij)
  CALL obj%GetMs(ans=obj%ms, nrow=ii, ncol=jj)

  nns = obj%elemsdForSpace%nns
  nips = obj%elemsdForSpace%nips

  DO ii = 1, nips
    CALL obj%initialVel%Get(val=coeff(ii), &
                            args=obj%elemsdForSpace%coord(1, ii:ii))
  END DO

  rhs(1:nns) = 0.0

  DO jj = 1, nns
    DO ii = 1, nips
      rhs(jj) = rhs(jj) + obj%elemsdForSpace%N(jj, ii) &
                * obj%elemsdForSpace%ws(ii) * coeff(ii)
    END DO
  END DO

  CALL SymLinSolve(X=val(1:nns), A=obj%ms(1:nns, 1:nns), &
                   B=rhs(1:nns))

  CALL RealVector_Set(obj=obj%v0, nodenum=con(1:nns), VALUE=val(1:nns))

  xij(1, 1) = xij(1, 2)

END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetInitialVelocity

!----------------------------------------------------------------------------
!                                                         SetInitialVelocity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetInitialDisplacement
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetInitialDisplacement()"
#endif

LOGICAL(LGT) :: isok
INTEGER(I4B) :: ii, nns, nips, jj, ielSpace, con(256), tcon
REAL(DFP) :: xij(1, 2), val(MAX_ORDER_SPACE + 1), dx, &
             coeff(MAX_ORDER_SPACE * 2 + 2), rhs(MAX_ORDER_SPACE + 1)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = ASSOCIATED(obj%initialDisp)
IF (.NOT. isok) THEN
  CALL RealVector_Set(obj=obj%u0, VALUE=zero)
  RETURN
END IF

CALL obj%InitiateConnectivity()

xij(1, 1) = obj%spaceDomain(1)

DO ielSpace = 1, obj%totalSpaceElements

  CALL obj%GetConnectivity(spaceElemNum=ielSpace, ans=con, tsize=tcon)

  dx = obj%spaceElemLength(ielSpace)
  xij(1, 2) = xij(1, 1) + dx

  CALL obj%SetQuadForSpace(ielSpace)
  CALL obj%SetElemsdForSpace(ielSpace, xij)
  CALL obj%GetMs(ans=obj%ms, nrow=ii, ncol=jj)

  nns = obj%elemsdForSpace%nns
  nips = obj%elemsdForSpace%nips

  DO ii = 1, nips
    CALL obj%initialDisp%Get(val=coeff(ii), &
                             args=obj%elemsdForSpace%coord(1, ii:ii))
  END DO

  rhs(1:nns) = 0.0

  DO jj = 1, nns
    DO ii = 1, nips
      rhs(jj) = rhs(jj) + obj%elemsdForSpace%N(jj, ii) &
                * obj%elemsdForSpace%ws(ii) * coeff(ii)
    END DO
  END DO

  CALL SymLinSolve(X=val(1:nns), A=obj%ms(1:nns, 1:nns), &
                   B=rhs(1:nns))

  CALL RealVector_Set(obj=obj%u0, nodenum=con(1:nns), VALUE=val(1:nns))

  xij(1, 1) = xij(1, 2)

END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetInitialDisplacement

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SetMethods
