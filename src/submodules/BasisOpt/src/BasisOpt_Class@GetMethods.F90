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

SUBMODULE(BasisOpt_Class) GetMethods
USE Display_Method, ONLY: ToString, Display
USE ReferenceElement_Method, ONLY: GetTotalNodes
USE ElemshapeData_Method, ONLY: LagrangeElemShapeData, &
                                HierarchicalElemShapeData, &
                                Elemsd_Set => Set, &
                                LagrangeFacetElemShapeData, &
                                HierarchicalFacetElemShapeData

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                          GetRefElemCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetRefElemCoord
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetRefElemCoord()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nrow = 3
ncol = GetTotalNodes(obj%topoType)
ans(1:nrow, 1:ncol) = obj%refelemCoord(1:nrow, 1:ncol)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetRefElemCoord

!----------------------------------------------------------------------------
!                                                                    GetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetParam()"
#endif

INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (PRESENT(nsd)) nsd = obj%nsd
IF (PRESENT(order)) order = obj%order
IF (PRESENT(anisoOrder)) anisoOrder = obj%anisoOrder

IF (PRESENT(edgeOrder)) THEN
  DO ii = 1, obj%tEdgeOrder
    edgeOrder(ii) = obj%edgeOrder(ii)
  END DO
END IF

IF (PRESENT(faceOrder)) THEN
  DO ii = 1, obj%tfaceOrder
    faceOrder(1:3, ii) = obj%faceOrder(1:3, ii)
  END DO
END IF

IF (PRESENT(cellOrder)) THEN
  DO ii = 1, obj%tcellOrder
    cellOrder(ii) = obj%cellOrder(ii)
  END DO
END IF

IF (PRESENT(feType)) feType = obj%feType
IF (PRESENT(elemType)) elemType = obj%elemType
IF (PRESENT(topoType)) topoType = obj%topoType
IF (PRESENT(elemIndx)) elemIndx = obj%elemIndx
IF (PRESENT(ipType)) ipType = obj%ipType

IF (PRESENT(dofType)) dofType = obj%dofType
IF (PRESENT(transformType)) transformType = obj%transformType

IF (PRESENT(baseContinuity)) THEN
  baseContinuity = obj%baseContinuity
END IF

IF (PRESENT(baseInterpolation)) THEN
  baseInterpolation = obj%baseInterpolation
END IF

IF (PRESENT(refElemDomain)) refElemDomain = obj%refElemDomain
IF (PRESENT(isIsotropicOrder)) isIsotropicOrder = obj%isIsotropicOrder
IF (PRESENT(isAnisotropicOrder)) isAnisotropicOrder = obj%isAnisotropicOrder
IF (PRESENT(isEdgeOrder)) isEdgeOrder = obj%isEdgeOrder
IF (PRESENT(isFaceOrder)) isFaceOrder = obj%isFaceOrder
IF (PRESENT(isCellOrder)) isCellOrder = obj%isCellOrder

IF (PRESENT(tEdgeOrder)) tEdgeOrder = obj%tEdgeOrder
IF (PRESENT(tFaceOrder)) tFaceOrder = obj%tFaceOrder
IF (PRESENT(tCellOrder)) tCellOrder = obj%tCellOrder

IF (PRESENT(basisType)) basisType = obj%basisType
IF (PRESENT(alpha)) alpha = obj%alpha
IF (PRESENT(beta)) beta = obj%beta
IF (PRESENT(lambda)) lambda = obj%lambda

CALL obj%quadOpt%GetParam( &
  isHomogeneous=quadratureIsHomogeneous, &
  quadratureType=quadratureType, &
  order=quadratureOrder, &
  nips=quadratureNips, &
  alpha=quadratureAlpha, &
  beta=quadratureBeta, &
  lambda=quadratureLambda, &
  isOrder=quadratureIsOrder, &
  isNips=quadratureIsNips)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetParam

!----------------------------------------------------------------------------
!                                                      GetLocalElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetLocalElemShapeData()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

SELECT CASE (obj%baseInterpolation)
CASE ("LAGR")

  CALL LagrangeElemShapeData(obj=elemsd, quad=quad, nsd=obj%nsd, &
                             xidim=obj%xidim, elemType=obj%elemType, &
                             refelemCoord=obj%refelemCoord, &
                             domainName=obj%refelemDomain, &
                             order=obj%order, &
                             ipType=obj%ipType, &
                             basisType=obj%basisType(1), &
                             coeff=obj%coeff, firstCall=obj%firstCall, &
                             alpha=obj%alpha(1), beta=obj%beta(1), &
                             lambda=obj%lambda(1))

CASE ("HIER", "HEIR")

  CALL HierarchicalElemShapeData(obj=elemsd, quad=quad, nsd=obj%nsd, &
                                 xidim=obj%xidim, elemType=obj%elemType, &
                                 refelemCoord=obj%refelemCoord, &
                                 domainName=obj%refelemDomain, &
                                 cellOrder=obj%cellOrder, &
                                 faceOrder=obj%faceOrder, &
                                 edgeOrder=obj%edgeOrder, &
                                 cellOrient=obj%cellOrient, &
                                 faceOrient=obj%faceOrient, &
                                 edgeOrient=obj%edgeOrient)

#ifdef DEBUG_VER
CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: No case found for baseInterpolation')
#endif
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetLocalElemShapeData

!----------------------------------------------------------------------------
!                                                 GetLocalFacetElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalFacetElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetLocalFacetElemShapeData()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

SELECT CASE (obj%baseInterpolation(1:1))
CASE ("L")

  CALL LagrangeFacetElemShapeData( &
    obj=elemsd, facetElemsd=facetElemsd, quad=quad, facetQuad=facetQuad, &
    localFaceNumber=localFaceNumber, nsd=obj%nsd, xidim=obj%xidim, &
    elemType=obj%elemType, refelemCoord=obj%refelemCoord, &
    domainName=obj%refelemDomain, order=obj%order, ipType=obj%ipType, &
    basisType=obj%basisType(1), coeff=obj%coeff, firstCall=obj%firstCall, &
    alpha=obj%alpha(1), beta=obj%beta(1), lambda=obj%lambda(1))

CASE ("H")

  CALL HierarchicalFacetElemShapeData( &
    obj=elemsd, facetElemsd=facetElemsd, quad=quad, facetQuad=facetQuad, &
    localFaceNumber=localFaceNumber, nsd=obj%nsd, xidim=obj%xidim, &
    elemType=obj%elemType, refelemCoord=obj%refelemCoord, &
    domainName=obj%refelemDomain, cellOrder=obj%cellOrder, &
    faceOrder=obj%faceOrder, edgeOrder=obj%edgeOrder, &
    cellOrient=obj%cellOrient, faceOrient=obj%faceOrient, &
    edgeOrient=obj%edgeOrient)

#ifdef DEBUG_VER
CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: No case found for baseInterpolation')
#endif
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetLocalFacetElemShapeData

!----------------------------------------------------------------------------
!                                                     GetGlobalElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetGlobalElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetGlobalElemShapeData()"
#endif

INTEGER(I4B) :: nns, nips, nsd, xidim

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

nns = geoelemsd%nns
nips = geoelemsd%nips
nsd = geoelemsd%nsd
xidim = geoelemsd%xidim

CALL Elemsd_Set(obj=elemsd, val=xij(1:nsd, 1:nns), &
                N=geoelemsd%N(1:nns, 1:nips), &
                dNdXi=geoelemsd%dNdXi(1:nns, 1:xidim, 1:nips))

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetGlobalElemShapeData

!----------------------------------------------------------------------------
!                                                       GetQuadraturePoints
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetQuadraturePoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetQuadraturePoints1()"
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
!                                                   GetFacetQuadraturePoints
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFacetQuadraturePoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetFacetQuadraturePoints1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%quadOpt%GetFacetQuadraturePoints(quad=quad, facetQuad=facetQuad, &
                                          localFaceNumber=localFaceNumber)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetFacetQuadraturePoints

!----------------------------------------------------------------------------
!                                                   GetTotalQuadraturePoints
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalQuadraturePoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalQuadraturePoints1()"
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
!                                                           GetTopologyType
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTopologyType
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTopologyName()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%topoType

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTopologyType

!----------------------------------------------------------------------------
!                                                                GetTotalDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalDOF
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalDOF()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%tdof

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalDOF

!----------------------------------------------------------------------------
!                                                     GetBaseInterpolation
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
!                                                          GetBaseContinuity
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
!                                                    Line_GetQuadraturePoints
!----------------------------------------------------------------------------

MODULE PROCEDURE Line_GetQuadraturePoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Line_GetQuadraturePoints()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%quadOpt%Line_GetQuadraturePoints(quad=quad)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE Line_GetQuadraturePoints

!----------------------------------------------------------------------------
!                                                Triangle_GetQuadraturePoints
!----------------------------------------------------------------------------

MODULE PROCEDURE Triangle_GetQuadraturePoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Triangle_GetQuadraturePoints()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%quadOpt%Triangle_GetQuadraturePoints(quad=quad)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE Triangle_GetQuadraturePoints

!----------------------------------------------------------------------------
!                                              Quadrangle_GetQuadraturePoints
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrangle_GetQuadraturePoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Quadrangle_GetQuadraturePoints()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%quadOpt%Quadrangle_GetQuadraturePoints(quad=quad)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE Quadrangle_GetQuadraturePoints

!----------------------------------------------------------------------------
!                                                           Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetMethods
