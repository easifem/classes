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
USE ElemshapeData_Method, ONLY: LagrangeElemShapeData, &
                                HierarchicalElemShapeData, &
                                Elemsd_Set => Set

USE QuadraturePoint_Method, ONLY: QuadraturePoint_Initiate => Initiate
IMPLICIT NONE
CONTAINS

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
IF (PRESENT(topoType)) topoType = obj%topoName
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

SELECT CASE (obj%baseInterpolation)
CASE ("LAGR")
  CALL LagrangeElemShapeData(obj=elemsd, quad=quad, nsd=obj%nsd, &
      xidim=obj%xidim, elemType=obj%elemType, refelemCoord=obj%refelemCoord, &
           domainName=obj%refelemDomain, order=obj%order, ipType=obj%ipType, &
           basisType=obj%basisType(1), coeff=coeff, firstCall=obj%firstCall, &
                   alpha=obj%alpha(1), beta=obj%beta(1), lambda=obj%lambda(1))

CASE ("HIER", "HEIR")
  CALL HierarchicalElemShapeData(obj=elemsd, quad=quad, nsd=obj%nsd, &
      xidim=obj%xidim, elemType=obj%elemType, refelemCoord=obj%refelemCoord, &
                      domainName=obj%refelemDomain, cellOrder=obj%cellOrder, &
                           faceOrder=obj%faceOrder, edgeOrder=obj%edgeOrder, &
                       cellOrient=obj%cellOrient, faceOrient=obj%faceOrient, &
                                 edgeOrient=obj%edgeOrient)

CASE DEFAULT
#ifdef DEBUG_VER
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: No case found for baseInterpolation')
  RETURN
#endif
END SELECT
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

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
#endif

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

IF (PRESENT(geoelemsd)) THEN
  nns = geoelemsd%nns
  nips = geoelemsd%nips
  nsd = geoelemsd%nsd
  xidim = geoelemsd%xidim
  CALL Elemsd_Set(obj=elemsd, val=xij(1:nsd, 1:nns), &
                  N=geoelemsd%N(1:nns, 1:nips), &
                  dNdXi=geoelemsd%dNdXi(1:nns, 1:xidim, 1:nips))
ELSE
  nns = elemsd%nns
  nips = elemsd%nips
  nsd = elemsd%nsd
  xidim = elemsd%xidim
  CALL Elemsd_Set(obj=elemsd, val=xij(1:nsd, 1:nns), &
                  N=elemsd%N(1:nns, 1:nips), &
                  dNdXi=elemsd%dNdXi(1:nns, 1:xidim, 1:nips))
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetGlobalElemShapeData

!----------------------------------------------------------------------------
!                                                       GetQuadraturePoints
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetQuadraturePoints1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetQuadraturePoints1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL QuadraturePoint_Initiate(obj=quad, &
                              elemType=obj%elemType, &
                              domainName=obj%refelemDomain, &
                              order=order, &
                              quadratureType=quadratureType, &
                              alpha=alpha, &
                              beta=beta, &
                              lambda=lambda, &
                              xij=obj%refelemCoord(1:obj%xidim, :))

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetQuadraturePoints1

!----------------------------------------------------------------------------
!                                                         GetQuadraturePoints
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetQuadraturePoints2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetQuadraturePoints2()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL QuadraturePoint_Initiate(obj=quad, &
                              elemType=obj%elemType, &
                              domainName=obj%refelemDomain, &
                              p=p, q=q, r=r, &
                              quadratureType1=quadratureType1, &
                              quadratureType2=quadratureType2, &
                              quadratureType3=quadratureType3, &
                              alpha1=alpha1, &
                              alpha2=alpha2, &
                              alpha3=alpha3, &
                              beta1=beta1, &
                              beta2=beta2, &
                              beta3=beta3, &
                              lambda1=lambda1, &
                              lambda2=lambda2, &
                              lambda3=lambda3, &
                              xij=obj%refelemCoord(1:obj%xidim, :))

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetQuadraturePoints2

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

ans = obj%topoName

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTopologyType

!----------------------------------------------------------------------------
!                                                                 GetPrefix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPrefix
ans = myprefix
END PROCEDURE obj_GetPrefix

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

END SUBMODULE GetMethods
