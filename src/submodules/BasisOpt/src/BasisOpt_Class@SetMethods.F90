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
USE InputUtility, ONLY: Input
USE BasisOptUtility, ONLY: SetIntegerType
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

CALL obj%quadOpt%SetParam( &
  isHomogeneous=quadratureIsHomogeneous, quadratureType=quadratureType, &
  quadratureType1=quadratureType1, quadratureType2=quadratureType2, &
  quadratureType3=quadratureType3, order=quadratureOrder, &
  order1=quadratureOrder1, order2=quadratureOrder2, &
  order3=quadratureOrder3, nips=quadratureNips, nips1=quadratureNips1, &
  nips2=quadratureNips2, nips3=quadratureNips3, alpha=quadratureAlpha, &
  alpha1=quadratureAlpha1, alpha2=quadratureAlpha2, &
  alpha3=quadratureAlpha3, beta=quadratureBeta, beta1=quadratureBeta1, &
  beta2=quadratureBeta2, beta3=quadratureBeta3, lambda=quadratureLambda, &
  lambda1=quadratureLambda1, lambda2=quadratureLambda2, &
  lambda3=quadratureLambda3, nsd=nsd, topoType=topoType, &
  isOrder=quadratureIsOrder, isNips=quadratureIsNips, xidim=xidim, &
  refelemDomain=refelemDomain, refelemCoord=refelemCoord)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetParam

!----------------------------------------------------------------------------
!                                                         SetCellOrientation
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetCellOrientation
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetCellOrientation()"
INTEGER(I4B) :: tsize
LOGICAL(LGT) :: errCheck0
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
errCheck0 = Input(option=errCheck, default=.FALSE.)

IF (errCheck0) THEN
  isok = PRESENT(cellOrient)
  CALL AssertError1(isok, myName, "cellOrient is not present")

  isok = PRESENT(tCell)
  CALL AssertError1(isok, myName, "tCell is not present")

  tsize = SIZE(cellOrient)
  isok = tCell .LE. tsize
  CALL AssertError1(isok, myName, &
                    "size of cellOrient is not enough. tCell = "// &
                    ToString(tCell)//", size(cellOrient) = "// &
                    ToString(tsize))
END IF
#endif

obj%isCellOrient = PRESENT(cellOrient)
isok = PRESENT(tCell)
IF (isok) obj%tCellOrder = tCell

IF (obj%isCellOrient) CALL SetIntegerType(a=obj%cellOrient, &
                                          n=obj%tCellOrder, b=cellOrient)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetCellOrientation

!----------------------------------------------------------------------------
!                                                         SetFaceOrientation
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetFaceOrientation
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetFaceOrientation()"
INTEGER(I4B) :: tsize
LOGICAL(LGT) :: errCheck0
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
errCheck0 = Input(option=errCheck, default=.FALSE.)
errCheck0 = errCheck0 .AND. (obj%xidim .GE. 2)

IF (errCheck0) THEN
  isok = PRESENT(tFace)
  CALL AssertError1(isok, myName, "tFace is not present")

  isok = PRESENT(faceOrient)
  CALL AssertError1(isok, myName, "faceOrient is not present")

  tsize = SIZE(faceOrient, 1)
  isok = tsize .EQ. 3
  CALL AssertError1(isok, myName, &
           "rowsize in faceOrient should be 3.  but size(faceOrient,1) = "// &
                    ToString(tsize))

  tsize = SIZE(faceOrient, 2)
  isok = tsize .GE. tFace
  CALL AssertError1(isok, myName, &
                    "colsize in faceOrient is not enough. tFace = "// &
                    ToString(tFace)//", size(faceOrient, 2) = "// &
                    ToString(tsize))
END IF
#endif

obj%isFaceOrient = PRESENT(faceOrient)
isok = PRESENT(tFace)
IF (isok) obj%tFaceOrder = tFace

IF (obj%isFaceOrient) CALL SetIntegerType(a=obj%faceOrient, b=faceOrient, &
                                          nrow=3, ncol=obj%tFaceOrder)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetFaceOrientation

!----------------------------------------------------------------------------
!                                                         SetEdgeOrientation
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetEdgeOrientation
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetEdgeOrientation()"
INTEGER(I4B) :: tsize
LOGICAL(LGT) :: errCheck0
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
errCheck0 = Input(option=errCheck, default=.FALSE.)
errCheck0 = errCheck0 .AND. (obj%xidim .GE. 3)

IF (errCheck0) THEN
  isok = PRESENT(edgeOrient)
  CALL AssertError1(isok, myName, "edgeOrient is not present")

  isok = PRESENT(tedge)
  CALL AssertError1(isok, myName, "tedge is not present")

  tsize = SIZE(edgeOrient)
  isok = tsize .GE. tedge
  CALL AssertError1(isok, myName, &
                    "size of edgeOrient is not enough. tedge = "// &
                    ToString(tedge)//", size(edgeOrient) = "// &
                    ToString(tsize))
END IF
#endif

obj%isEdgeOrient = PRESENT(edgeOrient)
isok = PRESENT(tEdge)
IF (isok) obj%tEdgeOrder = tEdge

IF (obj%isEdgeOrient) CALL SetIntegerType(a=obj%edgeOrient, &
                                          n=obj%tEdgeOrder, b=edgeOrient)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetEdgeOrientation

!----------------------------------------------------------------------------
!                                                               SetCellOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetCellOrder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetCellOrder()"
INTEGER(I4B) :: tsize
LOGICAL(LGT) :: errCheck0
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
errCheck0 = Input(option=errCheck, default=.FALSE.)

IF (errCheck0) THEN
  isok = PRESENT(cellOrder)
  CALL AssertError1(isok, myName, "cellOrder is not present")

  isok = PRESENT(tCell)
  CALL AssertError1(isok, myName, "tCell is not present")

  tsize = SIZE(cellOrder)
  isok = tCell .LE. tsize
  CALL AssertError1(isok, myName, &
                    "size of cellOrder is not enough. tCell = "// &
                    ToString(tCell)//", size(cellOrder) = "// &
                    ToString(tsize))
END IF
#endif

obj%isCellOrder = PRESENT(cellOrder)
isok = PRESENT(tCell)
IF (isok) obj%tCellOrder = tCell

IF (obj%isCellOrder) CALL SetIntegerType(a=obj%cellOrder, &
                                         n=obj%tCellOrder, b=cellOrder)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetCellOrder

!----------------------------------------------------------------------------
!                                                               SetFaceOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetFaceOrder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetFaceOrder()"
INTEGER(I4B) :: tsize
LOGICAL(LGT) :: errCheck0
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
errCheck0 = Input(option=errCheck, default=.FALSE.)
errCheck0 = errCheck0 .AND. (obj%xidim .GE. 2)

IF (errCheck0) THEN
  isok = PRESENT(tFace)
  CALL AssertError1(isok, myName, "tFace is not present")

  isok = PRESENT(faceOrder)
  CALL AssertError1(isok, myName, "faceOrder is not present")

  tsize = SIZE(faceOrder, 1)
  isok = tsize .EQ. 3
  CALL AssertError1(isok, myName, &
             "rowsize in faceOrder should be 3.  but size(faceOrder,1) = "// &
                    ToString(tsize))

  tsize = SIZE(faceOrder, 2)
  isok = tsize .GE. tFace
  CALL AssertError1(isok, myName, &
                    "colsize in faceOrder is not enough. tFace = "// &
                    ToString(tFace)//", size(faceOrder, 2) = "// &
                    ToString(tsize))
END IF
#endif

obj%isFaceOrder = PRESENT(faceOrder)
isok = PRESENT(tFace)
IF (isok) obj%tFaceOrder = tFace

IF (obj%isFaceOrder) CALL SetIntegerType(a=obj%faceOrder, b=faceOrder, &
                                         nrow=3, ncol=obj%tFaceOrder)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetFaceOrder

!----------------------------------------------------------------------------
!                                                               SetEdgeOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetEdgeOrder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetEdgeOrder()"
INTEGER(I4B) :: tsize
LOGICAL(LGT) :: errCheck0
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
errCheck0 = Input(option=errCheck, default=.FALSE.)
errCheck0 = errCheck0 .AND. (obj%xidim .GE. 3)

IF (errCheck0) THEN
  isok = PRESENT(edgeOrder)
  CALL AssertError1(isok, myName, "edgeOrder is not present")

  isok = PRESENT(tEdge)
  CALL AssertError1(isok, myName, "tEdge is not present")

  tsize = SIZE(edgeOrder)
  isok = tsize .GE. tEdge
  CALL AssertError1(isok, myName, &
                    "size of edgeOrder is not enough. tEdge = "// &
                    ToString(tEdge)//", size(edgeOrder) = "// &
                    ToString(tsize))
END IF
#endif

obj%isEdgeOrder = PRESENT(edgeOrder)
isok = PRESENT(tEdge)
IF (isok) obj%tEdgeOrder = tEdge

IF (obj%isEdgeOrder) CALL SetIntegerType(a=obj%edgeOrder, n=obj%tEdgeOrder, &
                                         b=edgeOrder)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetEdgeOrder

!----------------------------------------------------------------------------
!                                                          SetIsotropicOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetIsotropicOrder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetIsotropicOrder()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isIsotropicOrder = PRESENT(order)
IF (obj%isIsotropicOrder) THEN
  CALL obj%ResetAnisotropicOrder()
  obj%order = order
  obj%tdof = LagrangeDOF(order=obj%order, elemType=obj%topoType)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetIsotropicOrder

!----------------------------------------------------------------------------
!                                                        SetAnisotropicOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetAnisotropicOrder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetAnisotropicOrder()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%isAnisotropicOrder = PRESENT(anisoOrder)
IF (obj%isAnisotropicOrder) THEN
  CALL obj%ResetIsotropicOrder()
  CALL SetIntegerType(a=obj%anisoOrder, b=anisoOrder, n=obj%xidim)
  obj%tdof = LagrangeDOF( &
             p=obj%anisoOrder(1), q=obj%anisoOrder(2), r=obj%anisoOrder(3), &
             elemType=obj%topoType)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetAnisotropicOrder

!----------------------------------------------------------------------------
!                                                                   SetOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetOrder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetOrder()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

SELECT CASE (obj%baseInterpolation(1:1))
CASE ("L") !! Lagrange
  CALL obj%SetCellOrientation(cellOrient=cellOrient, tCell=tCell, &
                              errCheck=.FALSE.)

  CALL obj%SetFaceOrientation(faceOrient=faceOrient, tFace=tFace, &
                              errCheck=.FALSE.)

  CALL obj%SetEdgeOrientation(edgeOrient=edgeOrient, tEdge=tEdge, &
                              errCheck=.FALSE.)

  CALL obj%SetIsotropicOrder(order=order, errCheck=errCheck)

  CALL obj%SetAnisotropicOrder(anisoOrder=anisoOrder, errCheck=errCheck)

CASE ("H") !! Hierarchical
  CALL obj%SetCellOrientation(cellOrient=cellOrient, tCell=tCell, &
                              errCheck=errCheck)

  CALL obj%SetFaceOrientation(faceOrient=faceOrient, tFace=tFace, &
                              errCheck=errCheck)

  CALL obj%SetEdgeOrientation(edgeOrient=edgeOrient, tEdge=tEdge, &
                              errCheck=errCheck)

  CALL obj%SetCellOrder(cellOrder=cellOrder, tCell=tCell, errCheck=errCheck)
  CALL obj%SetFaceOrder(faceOrder=faceOrder, tFace=tFace, errCheck=errCheck)
  CALL obj%SetEdgeOrder(edgeOrder=edgeOrder, tEdge=tEdge, errCheck=errCheck)

  obj%tdof = HierarchicalDOF(elemType=obj%topoType, cellOrder=obj%cellOrder, &
                             faceOrder=obj%faceOrder, edgeOrder=obj%edgeOrder)

#ifdef DEBUG_VER
CASE DEFAULT
  CALL AssertError1(.FALSE., myName, &
                    'No case found for baseInterpolation.')
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
