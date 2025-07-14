! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
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

SUBMODULE(AbstractFE_Class) GetMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                GetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetParam()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%opt%GetParam(nsd=nsd, order=order, anisoOrder=anisoOrder, &
              edgeOrder=edgeOrder, faceOrder=faceOrder, cellOrder=cellOrder, &
       fetype=fetype, elemType=elemType, ipType=ipType, basisType=basisType, &
                      topoType=topoType, elemIndx=elemIndx, &
                     alpha=alpha, beta=beta, lambda=lambda, dofType=dofType, &
                   transformType=transformType, refElemDomain=refElemDomain, &
         baseContinuity=baseContinuity, baseInterpolation=baseInterpolation, &
   isIsotropicOrder=isIsotropicOrder, isAnisotropicOrder=isAnisotropicOrder, &
  isEdgeOrder=isEdgeOrder, isFaceOrder=isFaceOrder, isCellOrder=isCellOrder, &
          tEdgeOrder=tEdgeOrder, tFaceOrder=tFaceOrder, tCellOrder=tCellOrder)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif DEBUG_VER

END PROCEDURE obj_GetParam

!----------------------------------------------------------------------------
!                                                            GetTopologyType
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTopologyType
ans = obj%opt%GetTopologyType()
END PROCEDURE obj_GetTopologyType

!----------------------------------------------------------------------------
!                                                     GetLocalElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetLocalElemShapeData()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%opt%GetLocalElemShapeData(elemsd=elemsd, quad=quad, coeff=obj%coeff)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetLocalElemShapeData

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetGlobalElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetGlobalElemShapeData()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%opt%GetGlobalElemShapeData(elemsd=elemsd, xij=xij, &
                                    geoelemsd=geoelemsd)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetGlobalElemShapeData

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

CALL obj%opt%GetLocalFacetElemShapeData(cellElemsd=cellElemsd, &
                                        facetElemsd=facetElemsd, quad=quad)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetLocalFacetElemShapeData

!----------------------------------------------------------------------------
!                                                         GetQuadraturePoints
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetQuadraturePoints1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetQuadraturePoints1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%opt%GetQuadraturePoints(quad=quad, quadratureType=quadratureType, &
                           order=order, alpha=alpha, beta=beta, lambda=lambda)

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

CALL obj%opt%GetQuadraturePoints(quad=quad, p=p, q=q, r=r, &
           quadratureType1=quadratureType1, quadratureType2=quadratureType2, &
                                 quadratureType3=quadratureType3, &
    alpha1=alpha1, beta1=beta1, lambda1=lambda1, alpha2=alpha2, beta2=beta2, &
                 lambda2=lambda2, alpha3=alpha3, beta3=beta3, lambda3=lambda3)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetQuadraturePoints2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE GetMethods
