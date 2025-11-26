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

SUBMODULE(OneDimBasisOpt_Class) GetMethods
USE ElemshapeData_Method, ONLY: Elemsd_Allocate => ALLOCATE
USE ElemshapeData_Method, ONLY: HierarchicalElemShapeData
USE LineInterpolationUtility, ONLY: InterpolationPoint_Line_
USE LineInterpolationUtility, ONLY: LagrangeEvalAll_Line_
USE LineInterpolationUtility, ONLY: LagrangeGradientEvalAll_Line_
USE LineInterpolationUtility, ONLY: HeirarchicalBasis_Line_
USE LineInterpolationUtility, ONLY: HeirarchicalBasisGradient_Line_
USE LineInterpolationUtility, ONLY: OrthogonalBasis_Line_
USE LineInterpolationUtility, ONLY: OrthogonalBasisGradient_Line_
USE QuadraturePoint_Method, ONLY: GetTotalQuadraturePoints
USE QuadraturePoint_Method, ONLY: GetQuadratureWeights_
USE QuadraturePoint_Method, ONLY: QuadraturePoint_Initiate => Initiate
USE QuadraturePoint_Method, ONLY: QuadraturePoint_Deallocate => DEALLOCATE
USE ReallocateUtility, ONLY: Reallocate
USE SwapUtility, ONLY: SWAP_
! USE ReverseUtility, ONLY: Reverse

#ifdef DEBUG_VER
USE Display_Method, ONLY: Display
#endif

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                  GetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetParam()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (PRESENT(order)) order = obj%order
IF (PRESENT(tdof)) tdof = obj%tdof
IF (PRESENT(feType)) feType = obj%feType
IF (PRESENT(ipType)) ipType = obj%ipType
IF (PRESENT(baseContinuity)) baseContinuity = obj%baseContinuity
IF (PRESENT(baseInterpolation)) baseInterpolation = obj%baseInterpolation
IF (PRESENT(refElemDomain)) refElemDomain = obj%refElemDomain
IF (PRESENT(basisType)) basisType = obj%basisType
IF (PRESENT(alpha)) alpha = obj%alpha
IF (PRESENT(beta)) beta = obj%beta
IF (PRESENT(lambda)) lambda = obj%lambda
IF (PRESENT(firstCall)) firstCall = obj%firstCall
IF (PRESENT(dofType)) dofType = obj%dofType
IF (PRESENT(transformType)) transformType = obj%transformType

CALL obj%quadOpt%GetParam( &
  quadratureType=quadratureType, order=quadratureOrder, &
  nips=quadratureNips, alpha=quadratureAlpha, &
  beta=quadratureBeta, lambda=quadratureLambda)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetParam

!----------------------------------------------------------------------------
!                                                         GetBaseContinuity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetBaseContinuity
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetBaseContinuity()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%baseContinuity

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetBaseContinuity

!----------------------------------------------------------------------------
!                                                      GetBaseInterpolation
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetBaseInterpolation
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetBaseInterpolation()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%baseInterpolation

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetBaseInterpolation

!----------------------------------------------------------------------------
!                                                       GetQuadraturePoints
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetQuadraturePoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetQuadraturePoints()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%quadOpt%GetQuadraturePoints(quad=quad)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetQuadraturePoints

!----------------------------------------------------------------------------
!                                                   GetTotalQuadraturePoints
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalQuadraturePoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalQuadraturePoints()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%quadOpt%GetTotalQuadraturePoints()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalQuadraturePoints

!----------------------------------------------------------------------------
!                                                             GetCaseName
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetCaseName
ans = obj%baseContinuity//obj%baseInterpolation
END PROCEDURE obj_GetCaseName

!----------------------------------------------------------------------------
!                                                                  GetOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetOrder
ans = obj%order
END PROCEDURE obj_GetOrder

!----------------------------------------------------------------------------
!                                                                  GetOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalDOF
ans = obj%tdof
END PROCEDURE obj_GetTotalDOF

!----------------------------------------------------------------------------
!                                              Lagrange_GetLocalElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE Lagrange_GetLocalElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Lagrange_GetLocalElemShapeData()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: nips, tdof, indx(10), ii, jj

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nips = GetTotalQuadraturePoints(obj=quad)
tdof = obj%GetTotalDOF()

#ifdef DEBUG_VER
isok = tdof .GT. 0
CALL AssertError1(isok, myName, "zero tdof found")
#endif

CALL Elemsd_Allocate(obj=elemsd, nsd=1_I4B, xidim=1_I4B, nns=tdof, nips=nips)
CALL GetQuadratureWeights_(obj=quad, weights=elemsd%ws, tsize=nips)

CALL Reallocate(obj%xij, 3, tdof, isExpand=.TRUE., expandFactor=2_I4B)
CALL Reallocate(obj%coeff, tdof, tdof, isExpand=.TRUE., expandFactor=2_I4B)
CALL Reallocate(obj%xx, tdof, nips, isExpand=.TRUE., expandFactor=2_I4B)
CALL Reallocate(obj%temp, nips, tdof, 3, isExpand=.TRUE., expandFactor=2_I4B)

CALL InterpolationPoint_Line_( &
  order=obj%order, ipType=obj%ipType, layout="VEFC", &
  xij=obj%refelemCoord(1:1, 1:2), &
  alpha=obj%alpha, beta=obj%beta, &
  lambda=obj%lambda, ans=obj%xij, nrow=indx(1), ncol=indx(2))

CALL LagrangeEvalAll_Line_( &
  order=obj%order, &
  x=quad%points(1:quad%txi, 1:nips), &
  xij=obj%xij(1:indx(1), 1:indx(2)), &
  ans=obj%temp(:, :, 1), &
  nrow=indx(3), ncol=indx(4), &
  coeff=obj%coeff(1:tdof, 1:tdof), &
  xx=obj%xx(1:tdof, 1:nips), &
  firstCall=obj%firstCall, &
  basisType=obj%basisType, &
  alpha=obj%alpha, beta=obj%beta, lambda=obj%lambda)

DO CONCURRENT(ii=1:indx(4), jj=1:indx(3))
  elemsd%N(ii, jj) = obj%temp(jj, ii, 1)
END DO

CALL LagrangeGradientEvalAll_Line_( &
  order=obj%order, &
  x=quad%points(1:quad%txi, 1:nips), &
  xij=obj%xij(1:indx(1), 1:indx(2)), &
  ans=obj%temp, &
  dim1=indx(5), dim2=indx(6), dim3=indx(7), &
  coeff=obj%coeff(1:tdof, 1:tdof), &
  xx=obj%xx(1:tdof, 1:nips), &
  firstCall=.FALSE., &
  basisType=obj%basisType, &
  alpha=obj%alpha, beta=obj%beta, &
  lambda=obj%lambda)

CALL SWAP_(a=elemsd%dNdXi, b=obj%temp(1:indx(5), 1:indx(6), 1:indx(7)), &
           i1=2, i2=3, i3=1)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE Lagrange_GetLocalElemShapeData

!----------------------------------------------------------------------------
!                                        Lagrange_GetLocalFacetElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE Lagrange_GetLocalFacetElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Lagrange_GetLocalElemShapeData()"
#endif

REAL(DFP) :: xij(2, 2)
TYPE(QuadraturePoint_) :: quad

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

xij = 0.0_DFP
xij(1, 1:2) = obj%refelemCoord(1, 1:2)
CALL QuadraturePoint_Initiate(obj=quad, points=xij)
CALL obj%Lagrange_GetLocalElemShapeData(elemsd=elemsd, quad=quad)
CALL QuadraturePoint_Deallocate(quad)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE Lagrange_GetLocalFacetElemShapeData

!----------------------------------------------------------------------------
!                                          Hierarchical_GetLocalElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE Hierarchical_GetLocalElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Hierarchical_GetLocalElemShapeData()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: nips, tdof, indx(10), ii, jj

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nips = GetTotalQuadraturePoints(obj=quad)
tdof = obj%GetTotalDOF()

#ifdef DEBUG_VER
isok = tdof .GT. 0
CALL AssertError1(isok, myName, "zero tdof is found")
#endif

CALL Elemsd_Allocate( &
  obj=elemsd, nsd=1_I4B, xidim=1_I4B, nns=tdof, nips=nips)

CALL GetQuadratureWeights_(obj=quad, weights=elemsd%ws, tsize=nips)

CALL Reallocate(obj%temp, nips, tdof, 3, isExpand=.TRUE., expandFactor=2_I4B)

CALL HeirarchicalBasis_Line_( &
  order=obj%order, xij=quad%points(1:quad%txi, 1:nips), &
  refLine=obj%refelemDomain, orient=obj%cellOrient, ans=obj%temp(:, :, 1), &
  nrow=indx(3), ncol=indx(4))

DO CONCURRENT(ii=1:indx(4), jj=1:indx(3))
  elemsd%N(ii, jj) = obj%temp(jj, ii, 1)
END DO

CALL HeirarchicalBasisGradient_Line_( &
  order=obj%order, xij=quad%points(1:quad%txi, 1:nips), &
  refLine=obj%refelemDomain, orient=obj%cellOrient, &
  ans=obj%temp, dim1=indx(5), dim2=indx(6), dim3=indx(7))

CALL SWAP_(a=elemsd%dNdXi, b=obj%temp(1:indx(5), 1:indx(6), 1:indx(7)), &
           i1=2, i2=3, i3=1)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE Hierarchical_GetLocalElemShapeData

!----------------------------------------------------------------------------
!                                     Hierarchical_GetLocalFacetElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE Hierarchical_GetLocalFacetElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Lagrange_GetLocalElemShapeData()"
#endif

REAL(DFP) :: xij(2, 2)
TYPE(QuadraturePoint_) :: quad

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

xij = 0.0_DFP
xij(1, 1:2) = obj%refelemCoord(1, 1:2)
CALL QuadraturePoint_Initiate(obj=quad, points=xij)
CALL obj%Hierarchical_GetLocalElemShapeData(elemsd=elemsd, quad=quad)
CALL QuadraturePoint_Deallocate(quad)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE Hierarchical_GetLocalFacetElemShapeData

!----------------------------------------------------------------------------
!                                            Orthogonal_GetLocalElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthogonal_GetLocalElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Orthogonal_GetLocalElemShapeData()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: nips, tdof, indx(10), ii, jj

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nips = GetTotalQuadraturePoints(obj=quad)
tdof = obj%GetTotalDOF()

#ifdef DEBUG_VER
isok = tdof .GT. 0
CALL AssertError1(isok, myName, "zero tdof is found")
#endif

CALL Elemsd_Allocate( &
  obj=elemsd, nsd=1_I4B, xidim=1_I4B, nns=tdof, nips=nips)

CALL GetQuadratureWeights_(obj=quad, weights=elemsd%ws, tsize=nips)

CALL Reallocate(obj%temp, nips, tdof, 3, isExpand=.TRUE., expandFactor=2_I4B)

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
#endif

CALL OrthogonalBasis_Line_( &
  order=obj%order, xij=quad%points(1:quad%txi, 1:nips), &
  refLine=obj%refelemDomain, basisType=obj%basisType, ans=obj%temp(:, :, 1), &
  nrow=indx(3), ncol=indx(4), alpha=obj%alpha, beta=obj%beta, &
  lambda=obj%lambda)

DO CONCURRENT(ii=1:indx(4), jj=1:indx(3))
  elemsd%N(ii, jj) = obj%temp(jj, ii, 1)
END DO

CALL OrthogonalBasisGradient_Line_( &
  order=obj%order, xij=quad%points(1:quad%txi, 1:nips), &
  refLine=obj%refelemDomain, basisType=obj%basisType, ans=obj%temp, &
  dim1=indx(5), dim2=indx(6), dim3=indx(7), alpha=obj%alpha, &
  beta=obj%beta, lambda=obj%lambda)

CALL SWAP_(a=elemsd%dNdXi, b=obj%temp(1:indx(5), 1:indx(6), 1:indx(7)), &
           i1=2, i2=3, i3=1)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE Orthogonal_GetLocalElemShapeData

!----------------------------------------------------------------------------
!                                      Orthogonal_GetLocalFacetElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE Orthogonal_GetLocalFacetElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Lagrange_GetLocalElemShapeData()"
#endif

REAL(DFP) :: xij(2, 2)
TYPE(QuadraturePoint_) :: quad

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

xij = 0.0_DFP
xij(1, 1:2) = obj%refelemCoord(1, 1:2)
CALL QuadraturePoint_Initiate(obj=quad, points=xij)
CALL obj%Orthogonal_GetLocalElemShapeData(elemsd=elemsd, quad=quad)
CALL QuadraturePoint_Deallocate(quad)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE Orthogonal_GetLocalFacetElemShapeData

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetMethods
