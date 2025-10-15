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

SUBMODULE(LineH1LagrangeFE_Class) Methods
USE BaseType, ONLY: TypeElemNameOpt, TypePolynomialOpt, &
                    TypeFEVariableOpt, TypeInterpolationOpt
USE InputUtility, ONLY: Input
USE Display_Method, ONLY: ToString

USE LineInterpolationUtility, ONLY: GetTotalDOF_Line, &
                                    InterpolationPoint_Line_

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                     LineH1LagrangeFEPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_LineH1LagrangeFEPointer1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_LineH1LagrangeFEPointer1()"
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

END PROCEDURE obj_LineH1LagrangeFEPointer1

!----------------------------------------------------------------------------
!                                                     LineH1LagrangeFEPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_LineH1LagrangeFEPointer2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_LineH1LagrangeFEPointer2()"
#endif

INTEGER(I4B) :: basisType0(1), cellOrient0(1), quadratureType0(1), &
                quadratureOrder0(1)
REAL(DFP) :: alpha0(1), beta0(1), lambda0(1), quadratureAlpha0(1), &
             quadratureBeta0(1), quadratureLambda0(1)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ALLOCATE (ans)

basisType0(1) = Input(option=basisType, default=TypePolynomialOpt%default)
alpha0(1) = Input(option=alpha, default=0.0_DFP)
beta0(1) = Input(option=beta, default=0.0_DFP)
lambda0(1) = Input(option=lambda, default=0.0_DFP)
cellOrient0(1) = Input(option=cellOrient, default=1_I4B)
quadratureType0(1) = Input(option=quadratureType, &
                           default=TypeInterpolationOpt%default)
quadratureAlpha0(1) = Input(option=quadratureAlpha, default=0.0_DFP)
quadratureBeta0(1) = Input(option=quadratureBeta, default=0.0_DFP)
quadratureLambda0(1) = Input(option=quadratureLambda, default=0.0_DFP)
quadratureOrder0(1) = Input(option=quadratureOrder, default=order)

CALL ans%Initiate( &
  elemType=TypeElemNameOpt%line, nsd=nsd, baseContinuity="H1", &
  baseInterpolation="Lagrange", ipType=ipType, basisType=basisType0, &
  alpha=alpha0, beta=beta0, lambda=lambda0, &
  fetype=TypeFEVariableOpt%scalar, order=order, cellOrient=cellOrient0, &
  tcell=1_I4B, quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
  quadratureOrder=quadratureOrder0, quadratureType=quadratureType0, &
  quadratureAlpha=quadratureAlpha0, quadratureBeta=quadratureBeta0, &
  quadratureLambda=quadratureLambda0)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_LineH1LagrangeFEPointer2

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

CALL obj%opt%LineH1LagFE_GetLocalElemShapeData(elemsd=elemsd, quad=quad)

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

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = PRESENT(order)

IF (isok) THEN
  CALL obj%opt%LineH1LagFE_SetOrder(order=order)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

isok = PRESENT(anisoOrder)
IF (isok) THEN
  CALL obj%opt%LineH1LagFE_SetOrder(order=anisoOrder(1))

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

#ifdef DEBUG_VER
CALL AssertError1(.FALSE., myName, &
                  "either order or anisoOrder must be provided")
#endif

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

CALL obj%opt%Line_GetQuadraturePoints(quad=quad)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetQuadraturePoints

!----------------------------------------------------------------------------
!                                                         SetQuadratureOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetQuadratureOrder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetQuadratureOrder()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = PRESENT(order)
IF (isok) THEN
  CALL obj%opt%Line_SetQuadratureOrder(order=order(1))

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

isok = PRESENT(order1)
IF (isok) THEN
  CALL obj%opt%Line_SetQuadratureOrder(order=order1)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

isok = PRESENT(order2)
IF (isok) THEN
  CALL obj%opt%Line_SetQuadratureOrder(order=order2)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

isok = PRESENT(order3)
IF (isok) THEN
  CALL obj%opt%Line_SetQuadratureOrder(order=order3)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

#ifdef DEBUG_VER
CALL AssertError1(.FALSE., myName, &
                  'order, order1, order2, or order3 must be provided')
#endif

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

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = PRESENT(quadratureType)
IF (isok) THEN
  CALL obj%opt%Line_SetQuadratureType(quadratureType=quadratureType(1))

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

isok = PRESENT(quadratureType1)
IF (isok) THEN
  CALL obj%opt%Line_SetQuadratureType(quadratureType=quadratureType1)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

isok = PRESENT(quadratureType2)
IF (isok) THEN
  CALL obj%opt%Line_SetQuadratureType(quadratureType=quadratureType2)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

isok = PRESENT(quadratureType3)
IF (isok) THEN
  CALL obj%opt%Line_SetQuadratureType(quadratureType=quadratureType3)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

#ifdef DEBUG_VER
CALL AssertError1(.FALSE., myName, &
                  'quadratureType, quadratureType1, quadratureType2, or &
                  &quadratureType3 must be provided')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetQuadratureType

!----------------------------------------------------------------------------
!                                                      GetInterpolationPoints
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalInterpolationPoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalInterpolationPoints()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = GetTotalDOF_Line(order=order(1), baseContinuity="H1", &
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
CALL InterpolationPoint_Line_( &
  order=order(1), ipType=ipType(1), ans=ans, nrow=nrow, ncol=ncol, &
  layout="VEFC", xij=xij, alpha=alpha0, beta=beta0, lambda=lambda0)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetInterpolationPoints

!----------------------------------------------------------------------------
!                                                      GetGlobalElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetGlobalElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetGlobalElemShapeData()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%opt%LineH1LagFE_GetGlobalElemShapeData(elemsd=elemsd, xij=xij, &
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
