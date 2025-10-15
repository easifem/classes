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

SUBMODULE(TriangleH1LagrangeFE_Class) Methods
USE BaseType, ONLY: TypeElemNameOpt, TypePolynomialOpt, &
                    TypeFEVariableOpt, TypeInterpolationOpt
USE InputUtility, ONLY: Input
USE Display_Method, ONLY: ToString
USE TriangleInterpolationUtility, ONLY: GetTotalDOF_Triangle, &
                                        InterpolationPoint_Triangle_

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                     TriangleH1LagrangeFEPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_TriangleH1LagrangeFEPointer1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_TriangleH1LagrangeFEPointer1()"
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

END PROCEDURE obj_TriangleH1LagrangeFEPointer1

!----------------------------------------------------------------------------
!                                                     TriangleH1LagrangeFEPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_TriangleH1LagrangeFEPointer2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_TriangleH1LagrangeFEPointer2()"
#endif

INTEGER(I4B) :: basisType0(1), quadratureType0(1), &
                quadratureOrder0(1)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ALLOCATE (ans)

basisType0(1) = Input(option=basisType, default=TypePolynomialOpt%default)
quadratureType0(1) = Input(option=quadratureType, &
                           default=TypeInterpolationOpt%default)
quadratureOrder0(1) = Input(option=quadratureOrder, default=order)

CALL ans%Initiate( &
  elemType=TypeElemNameOpt%Triangle, nsd=nsd, baseContinuity="H1", &
  baseInterpolation="Lagrange", ipType=ipType, basisType=basisType0, &
  fetype=TypeFEVariableOpt%scalar, order=order, cellOrient=cellOrient, &
  tcell=3_I4B, faceOrient=faceOrient, tFace=3_I4B, &
  quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
  quadratureOrder=quadratureOrder0, quadratureType=quadratureType0)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_TriangleH1LagrangeFEPointer2

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

CALL obj%opt%TriangleH1LagFE_GetLocalElemShapeData(elemsd=elemsd, quad=quad)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetLocalElemShapeData

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

CALL obj%opt%TriangleH1LagFE_SetOrder(order=order0)

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

CALL obj%opt%Triangle_GetQuadraturePoints(quad=quad)

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

CALL obj%opt%Triangle_GetFacetQuadraturePoints( &
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

LOGICAL(LGT) :: isok
INTEGER(I4B) :: order0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = PRESENT(order) .OR. PRESENT(order1) .OR. &
       PRESENT(order2) .OR. PRESENT(order3)
CALL AssertError1(isok, myName, &
                  'order, order1, order2, or order3 must be provided')
#endif

IF (PRESENT(order)) THEN
  order0 = order(1)
ELSE IF (PRESENT(order1)) THEN
  order0 = order1
ELSE IF (PRESENT(order2)) THEN
  order0 = order2
ELSE IF (PRESENT(order3)) THEN
  order0 = order3
END IF

CALL obj%opt%Triangle_SetQuadratureOrder(order=order0)

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

LOGICAL(LGT) :: isok
INTEGER(I4B) :: quadratureType0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = PRESENT(quadratureType) .OR. PRESENT(quadratureType1) .OR. &
       PRESENT(quadratureType2) .OR. PRESENT(quadratureType3)
CALL AssertError1(isok, myName, &
                  'quadratureType, quadratureType1, quadratureType2, or &
                  &quadratureType3 must be provided')
#endif

IF (PRESENT(quadratureType)) THEN
  quadratureType0 = quadratureType(1)
ELSE IF (PRESENT(quadratureType1)) THEN
  quadratureType0 = quadratureType1
ELSE IF (PRESENT(quadratureType2)) THEN
  quadratureType0 = quadratureType2
ELSE IF (PRESENT(quadratureType3)) THEN
  quadratureType0 = quadratureType3
END IF

CALL obj%opt%Triangle_SetQuadratureType(quadratureType=quadratureType0)

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

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = GetTotalDOF_Triangle(order=order(1), baseContinuity="H1", &
                           baseInterpolation="Lagrange")

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

REAL(DFP) :: alpha0, beta0, lambda0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

alpha0 = 0.0_DFP; beta0 = 0.0_DFP; lambda0 = 0.5_DFP

IF (PRESENT(alpha)) alpha0 = alpha(1)
IF (PRESENT(beta)) beta0 = beta(1)
IF (PRESENT(lambda)) lambda0 = lambda(1)

! order, ipType, ans, nrow, ncol, layout, xij, alpha, beta, lambda)
CALL InterpolationPoint_Triangle_( &
  order=order(1), ipType=ipType(1), ans=ans, nrow=nrow, ncol=ncol, &
  layout="VEFC", xij=xij, alpha=alpha0, beta=beta0, lambda=lambda0)

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

CALL obj%opt%TriangleH1LagFE_GetGlobalElemShapeData(elemsd=elemsd, xij=xij, &
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
