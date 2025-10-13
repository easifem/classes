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

SUBMODULE(BasisOpt_Class) SetMethods
USE LagrangePolynomialUtility, ONLY: LagrangeDOF
USE HierarchicalPolynomialUtility, ONLY: HierarchicalDOF
USE ReallocateUtility, ONLY: Reallocate
USE Display_Method, ONLY: ToString, Display
USE ElemshapeData_Method, ONLY: LagrangeElemShapeData, &
                                HierarchicalElemShapeData

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 SetTotalDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetTotalDOF
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetTotalDOF()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

SELECT CASE (obj%baseInterpolation)
CASE ("LAGR")
  IF (obj%isIsotropicOrder) THEN
    obj%tdof = LagrangeDOF(order=obj%order, elemType=obj%topoType)
  END IF

  IF (obj%isAnisotropicOrder) THEN
    obj%tdof = LagrangeDOF(p=obj%anisoOrder(1), q=obj%anisoOrder(2), &
                           r=obj%anisoOrder(3), elemType=obj%topoType)
  END IF

CASE ("HIER", "HEIR")

  obj%tdof = HierarchicalDOF(elemType=obj%topoType, cellOrder=obj%cellOrder, &
                             faceOrder=obj%faceOrder, edgeOrder=obj%edgeOrder)

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetTotalDOF

!----------------------------------------------------------------------------
!                                                                    SetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetParam()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (PRESENT(nsd)) obj%nsd = nsd
IF (PRESENT(xidim)) obj%xidim = xidim
IF (PRESENT(refelemCoord)) obj%refelemCoord = refelemCoord
IF (PRESENT(order)) obj%order = order
IF (PRESENT(anisoOrder)) obj%anisoOrder = anisoOrder
IF (PRESENT(edgeOrder)) obj%edgeOrder(1:SIZE(edgeOrder)) = edgeOrder
IF (PRESENT(faceOrder)) obj%faceOrder(1:3, 1:SIZE(faceOrder)) = faceOrder(1:3, :)
IF (PRESENT(cellOrder)) obj%cellOrder(1:SIZE(cellOrder)) = cellOrder
IF (PRESENT(feType)) obj%feType = feType
IF (PRESENT(elemType)) obj%elemType = elemType
IF (PRESENT(topoType)) obj%topoType = topoType
IF (PRESENT(elemIndx)) obj%elemIndx = elemIndx
IF (PRESENT(ipType)) obj%ipType = ipType
IF (PRESENT(dofType)) obj%dofType = dofType
IF (PRESENT(transformType)) obj%transformType = transformType

IF (PRESENT(baseContinuity)) obj%baseContinuity = baseContinuity(1:2)
IF (PRESENT(baseInterpolation)) obj%baseInterpolation = baseInterpolation(1:4)
IF (PRESENT(refElemDomain)) obj%refElemDomain = refElemDomain(1:1)

IF (PRESENT(isIsotropicOrder)) obj%isIsotropicOrder = isIsotropicOrder
IF (PRESENT(isAnisotropicOrder)) obj%isAnisotropicOrder = isAnisotropicOrder
IF (PRESENT(isEdgeOrder)) obj%isEdgeOrder = isEdgeOrder
IF (PRESENT(isFaceOrder)) obj%isFaceOrder = isFaceOrder
IF (PRESENT(isCellOrder)) obj%isCellOrder = isCellOrder

IF (PRESENT(tEdgeOrder)) obj%tEdgeOrder = tEdgeOrder
IF (PRESENT(tFaceOrder)) obj%tFaceOrder = tFaceOrder
IF (PRESENT(tCellOrder)) obj%tCellOrder = tCellOrder

IF (PRESENT(basisType)) obj%basisType = basisType
IF (PRESENT(alpha)) obj%alpha = alpha
IF (PRESENT(beta)) obj%beta = beta
IF (PRESENT(lambda)) obj%lambda = lambda

CALL obj%quadOpt%SetParam(isHomogeneous=quadratureIsHomogeneous, &
                          quadratureType=quadratureType, &
                          quadratureType1=quadratureType1, &
                          quadratureType2=quadratureType2, &
                          quadratureType3=quadratureType3, &
                          order=quadratureOrder, &
                          order1=quadratureOrder1, &
                          order2=quadratureOrder2, &
                          order3=quadratureOrder3, &
                          nips=quadratureNips, &
                          nips1=quadratureNips1, &
                          nips2=quadratureNips2, &
                          nips3=quadratureNips3, &
                          alpha=quadratureAlpha, &
                          alpha1=quadratureAlpha1, &
                          alpha2=quadratureAlpha2, &
                          alpha3=quadratureAlpha3, &
                          beta=quadratureBeta, &
                          beta1=quadratureBeta1, &
                          beta2=quadratureBeta2, &
                          beta3=quadratureBeta3, &
                          lambda=quadratureLambda, &
                          lambda1=quadratureLambda1, &
                          lambda2=quadratureLambda2, &
                          lambda3=quadratureLambda3, &
                          nsd=nsd, &
                          topoType=topoType, &
                          isOrder=quadratureIsOrder, &
                          isNips=quadratureIsNips, &
                          xidim=xidim, &
                          refelemDomain=refelemDomain, &
                          refelemCoord=refelemCoord)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetParam

!----------------------------------------------------------------------------
!                                                                    SetOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetOrder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetOrder()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

SELECT CASE (obj%baseInterpolation)
CASE ("LAGR")
  CALL obj%SetLagrangeOrder(order=order, anisoOrder=anisoOrder, &
                            errCheck=errCheck)

CASE ("HIER", "HEIR")
  CALL obj%SetHierarchicalOrder( &
    cellOrder=cellOrder, faceOrder=faceOrder, edgeOrder=edgeOrder, &
    cellOrient=cellOrient, faceOrient=faceOrient, edgeOrient=edgeOrient, &
    errCheck=errCheck, tcell=tcell, tface=tface, tedge=tedge)

#ifdef DEBUG_VER
CASE DEFAULT
  CALL AssertError1(.FALSE., myName, &
                    'No case found for baseInterpolation is not defined.')
#endif

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetOrder

!----------------------------------------------------------------------------
!                                                       ResetIsotropicOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ResetIsotropicOrder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ResetIsotropicOrder()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isIsotropicOrder = TypeBasisOpt%isIsotropicOrder
obj%order = TypeBasisOpt%order

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_ResetIsotropicOrder

!----------------------------------------------------------------------------
!                                                       ResetAnisotropicOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ResetAnisotropicOrder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ResetAnisotropicOrder()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isAnisotropicOrder = TypeBasisOpt%isAnisotropicOrder
obj%anisoOrder = TypeBasisOpt%anisoOrder

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_ResetAnisotropicOrder

!----------------------------------------------------------------------------
!                                                         SetQuadratureOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetQuadratureOrder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetQuadratureOrder()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%quadOpt%SetOrder(order=order, order1=order1, &
                          order2=order2, order3=order3)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetQuadratureOrder

!----------------------------------------------------------------------------
!                                                    Line_SetQuadratureOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE Line_SetQuadratureOrder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Line_SetQuadratureOrder()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%quadOpt%Line_SetOrder(order=order)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE Line_SetQuadratureOrder

!----------------------------------------------------------------------------
!                                                 Triangle_SetQuadratureOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE Triangle_SetQuadratureOrder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Triangle_SetQuadratureOrder()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%quadOpt%Triangle_SetOrder(order=order)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE Triangle_SetQuadratureOrder

!----------------------------------------------------------------------------
!                                               Quadrangle_SetQuadratureOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrangle_SetQuadratureOrder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Quadrangle_SetQuadratureOrder()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%quadOpt%Quadrangle_SetOrder(order=order, order1=order1, &
                                     order2=order2)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE Quadrangle_SetQuadratureOrder

!----------------------------------------------------------------------------
!                                                         SetQuadratureType
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetQuadratureType
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetQuadratureType()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%quadOpt%SetQuadratureType( &
  quadratureType=quadratureType, quadratureType1=quadratureType1, &
  quadratureType2=quadratureType2, quadratureType3=quadratureType3)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetQuadratureType

!----------------------------------------------------------------------------
!                                                     Line_SetQuadratureType
!----------------------------------------------------------------------------

MODULE PROCEDURE Line_SetQuadratureType
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Line_SetQuadratureType()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%quadOpt%Line_SetQuadratureType(quadratureType=quadratureType)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE Line_SetQuadratureType

!----------------------------------------------------------------------------
!                                                 Triangle_SetQuadratureType
!----------------------------------------------------------------------------

MODULE PROCEDURE Triangle_SetQuadratureType
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Triangle_SetQuadratureType()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%quadOpt%Triangle_SetQuadratureType(quadratureType=quadratureType)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE Triangle_SetQuadratureType

!----------------------------------------------------------------------------
!                                                Quadrangle_SetQuadratureType
!----------------------------------------------------------------------------

MODULE PROCEDURE Quadrangle_SetQuadratureType
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "Quadrangle_SetQuadratureType()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%quadOpt%Quadrangle_SetQuadratureType( &
  quadratureType=quadratureType, quadratureType1=quadratureType1, &
  quadratureType2=quadratureType2)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE Quadrangle_SetQuadratureType

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SetMethods
