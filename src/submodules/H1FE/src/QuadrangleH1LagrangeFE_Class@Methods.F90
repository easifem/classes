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

SUBMODULE(QuadrangleH1LagrangeFE_Class) Methods
USE BaseType, ONLY: TypeElemNameOpt, TypePolynomialOpt, &
                    TypeFEVariableOpt, TypeInterpolationOpt
USE InputUtility, ONLY: Input
USE Display_Method, ONLY: ToString

USE QuadrangleInterpolationUtility, ONLY: GetTotalDOF_Quadrangle, &
                                          InterpolationPoint_Quadrangle_

USE LineInterpolationUtility, ONLY: GetTotalDOF_Line, &
                                    InterpolationPoint_Line_

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                              QuadrangleH1LagrangeFEPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_QuadrangleH1LagrangeFEPointer1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_QuadrangleH1LagrangeFEPointer1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ALLOCATE (ans)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_QuadrangleH1LagrangeFEPointer1

!----------------------------------------------------------------------------
!                                              QuadrangleH1LagrangeFEPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_QuadrangleH1LagrangeFEPointer2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_QuadrangleH1LagrangeFEPointer2()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ALLOCATE (ans)

CALL ans%Initiate( &
  elemType=TypeElemNameOpt%Quadrangle, nsd=nsd, baseContinuity="H1", &
  baseInterpolation="Lagrange", fetype=TypeFEVariableOpt%scalar, &
  ipType=ipType, basisType=basisType, alpha=alpha, beta=beta, lambda=lambda, &
  order=order, cellOrient=cellOrient, tCell=3_I4B, faceOrient=faceOrient, &
  tFace=4_I4B, quadratureIsHomogeneous=quadratureIsHomogeneous, &
  quadratureIsOrder=.TRUE., quadratureOrder=quadratureOrder, &
  quadratureType=quadratureType, quadratureAlpha=quadratureAlpha, &
  quadratureBeta=quadratureBeta, quadratureLambda=quadratureLambda)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_QuadrangleH1LagrangeFEPointer2

!----------------------------------------------------------------------------
!                                                       GetLocalElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetLocalElemShapeData()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%opt%QuadrangleH1LagFE_GetLocalElemShapeData(elemsd=elemsd, &
                                                     quad=quad)

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

CALL obj%opt%QuadrangleH1LagFE_GetLocalFacetElemShapeData( &
  elemsd=elemsd, facetElemsd=facetElemsd, quad=quad, facetQuad=facetQuad, &
  localFaceNumber=localFaceNumber)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetLocalFacetElemShapeData

!----------------------------------------------------------------------------
!                                                                    SetOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetOrder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetOrder()"
#endif

LOGICAL(LGT) :: isok
INTEGER(I4B) :: order0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = PRESENT(order) .OR. PRESENT(anisoOrder)
CALL AssertError1(isok, myName, &
                  "either order or anisoOrder must be provided")
#endif

isok = PRESENT(order)

IF (isok) THEN
  order0 = order
ELSE
  order0 = anisoOrder(1)
END IF

CALL obj%opt%QuadrangleH1LagFE_SetOrder(order=order0)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetOrder

!----------------------------------------------------------------------------
!                                                         GetQuadraturePoints
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetQuadraturePoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetQuadraturePoints()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%opt%Quadrangle_GetQuadraturePoints(quad=quad)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetQuadraturePoints

!----------------------------------------------------------------------------
!                                                    GetFacetQuadraturePoints
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFacetQuadraturePoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetFacetQuadraturePoints()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%opt%Quadrangle_GetFacetQuadraturePoints( &
  quad=quad, facetQuad=facetQuad, localFaceNumber=localFaceNumber)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetFacetQuadraturePoints

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

CALL obj%opt%Quadrangle_SetQuadratureOrder(order=order, order1=order1, &
                                           order2=order2)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetQuadratureOrder

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

CALL obj%opt%Quadrangle_SetQuadratureType( &
  quadratureType=quadratureType, quadratureType1=quadratureType1, &
  quadratureType2=quadratureType2)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetQuadratureType

!----------------------------------------------------------------------------
!                                                 GetTotalInterpolationPoints
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalInterpolationPoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalInterpolationPoints()"
#endif

INTEGER(I4B) :: p, q, tsize
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

p = order(1)
q = p

tsize = SIZE(order)
isok = tsize .GT. 1
IF (isok) q = order(2)

ans = GetTotalDOF_Line(order=p, baseContinuity="H1", &
                       baseInterpolation="Lagrange")

tsize = GetTotalDOF_Line(order=q, baseContinuity="H1", &
                         baseInterpolation="Lagrange")

ans = ans * tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalInterpolationPoints

!----------------------------------------------------------------------------
!                                                      GetInterpolationPoints
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetInterpolationPoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetInterpolationPoints()"
#endif

REAL(DFP) :: alpha1, beta1, lambda1, alpha2, beta2, lambda2
INTEGER(I4B) :: ipType1, ipType2, order1, order2, tsize
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

alpha1 = 0.0_DFP; beta1 = 0.0_DFP; lambda1 = 0.5_DFP
alpha2 = 0.0_DFP; beta2 = 0.0_DFP; lambda2 = 0.5_DFP

order1 = order(1)
order2 = order1
tsize = SIZE(order)
isok = tsize .GT. 1
IF (isok) order2 = order(2)

ipType1 = ipType(1)
ipType2 = ipType1
tsize = SIZE(ipType)
isok = tsize .GT. 1
IF (isok) ipType2 = ipType(2)

IF (PRESENT(alpha)) THEN
  alpha1 = alpha(1)
  alpha2 = alpha1
  tsize = SIZE(alpha)
  isok = tsize .GT. 1
  IF (isok) alpha2 = alpha(2)
END IF

IF (PRESENT(beta)) THEN
  beta1 = beta(1)
  beta2 = beta1
  tsize = SIZE(beta)
  isok = tsize .GT. 1
  IF (isok) beta2 = beta(2)
END IF

IF (PRESENT(lambda)) THEN
  lambda1 = lambda(1)
  lambda2 = lambda1
  tsize = SIZE(lambda)
  isok = tsize .GT. 1
  IF (isok) lambda2 = lambda(2)
END IF

CALL InterpolationPoint_Quadrangle_( &
  p=order1, q=order2, ipType1=ipType1, ipType2=ipType2, ans=ans, &
  nrow=nrow, ncol=ncol, layout="VEFC", xij=xij, alpha1=alpha1, beta1=beta1, &
  lambda1=lambda1, alpha2=alpha2, beta2=beta2, lambda2=lambda2)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetInterpolationPoints

!----------------------------------------------------------------------------
!                                                     GetGlobalElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetGlobalElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetGlobalElemShapeData()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%opt%QuadrangleH1LagFE_GetGlobalElemShapeData(elemsd=elemsd, xij=xij, &
                                                      geoelemsd=geoelemsd)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetGlobalElemShapeData

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods
