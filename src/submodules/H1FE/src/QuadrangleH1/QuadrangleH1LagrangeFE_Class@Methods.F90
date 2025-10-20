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
USE Display_Method, ONLY: ToString, Display

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
INTEGER(I4B) :: order0, tdof

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

CALL obj%opt%SetIsotropicOrder(order=order0)

tdof = GetTotalDOF_Quadrangle(order=order0, baseContinuity="H1", &
                              baseInterpolation="Lagrange")

CALL obj%opt%SetTotalDOF(tdof)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetOrder

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
!                                                     GetGlobalElemShapeData
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetGlobalFacetElemShapeData
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetGlobalFacetElemShapeData()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%opt%QuadrangleH1LagFE_GetGlobalFacetElemShapeData( &
  elemsd=elemsd, facetElemsd=facetElemsd, localFaceNumber=localFaceNumber, &
  geoElemsd=geoElemsd, geoFacetElemsd=geoFacetElemsd, xij=xij)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetGlobalFacetElemShapeData

!----------------------------------------------------------------------------
!                                           GetFacetDOFValueFromUserFunction
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFacetDOFValueFromUserFunction
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetFacetDOFValueFromUserFunction()"
LOGICAL(LGT) :: isok
INTEGER(I4B) :: tReturns
#endif

INTEGER(I4B), PARAMETER :: tVertices = 2
INTEGER(I4B) :: tArgs, ii, nips, nsd
REAL(DFP) :: args(4)
LOGICAL(LGT) :: onlyFaceBubble0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
tReturns = func%GetNumReturns()
isok = tReturns .EQ. 1
CALL AssertError1(isok, myName, &
                  "WIP: the user function must return a single value")
#endif

nips = facetElemsd%nips
nsd = facetElemsd%nsd
tArgs = func%GetNumArgs()

#ifdef DEBUG_VER
isok = SIZE(funcValue) .GE. nips
CALL AssertError1(isok, myName, &
             "funcValue array size is less than number of integration points")
#endif

#ifdef DEBUG_VER
isok = SIZE(ans) .GE. facetElemsd%nns
CALL AssertError1(isok, myName, &
             "ans array size is less than number of shape functions on facet")
! Even if onlyFaceBubble is true, the minimum size of ans should be nns
#endif

args = 0.0_DFP
DO ii = 1, nips
  args(1:nsd) = facetElemsd%coord(1:nsd, ii)
  CALL func%GetScalarValue(args=args(1:tArgs), val=funcValue(ii))
END DO

CALL obj%GetFacetDOFValueFromQuadrature( &
  elemsd=elemsd, facetElemsd=facetElemsd, xij=xij, &
  localFaceNumber=localFaceNumber, func=funcValue, ans=ans, tsize=tsize, &
  massMat=massMat, ipiv=ipiv)

onlyFaceBubble0 = Input(option=onlyFaceBubble, default=.FALSE.)
IF (onlyFaceBubble0) THEN
  DO ii = tVertices + 1, tsize
    ans(ii - 2) = ans(ii)
  END DO
  tsize = tsize - tVertices
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetFacetDOFValueFromUserFunction

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../../include/errors.F90"

END SUBMODULE Methods
