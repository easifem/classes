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

SUBMODULE(QuadrangleH1HierarchicalFE_Class) Methods
USE BaseType, ONLY: TypeElemNameOpt, TypePolynomialOpt, &
                    TypeFEVariableOpt, TypeInterpolationOpt
USE InputUtility, ONLY: Input
USE Display_Method, ONLY: ToString, Display

USE QuadrangleInterpolationUtility, ONLY: GetTotalDOF_Quadrangle, &
                                          InterpolationPoint_Quadrangle_, &
                                          GetTotalInDOF_Quadrangle, &
                                          GetHierarchicalDOF_Quadrangle, &
                                          FacetConnectivity_Quadrangle

USE LineInterpolationUtility, ONLY: GetTotalDOF_Line, &
                                    GetTotalInDOF_Line, &
                                    InterpolationPoint_Line_

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                              QuadrangleH1HierarchicalFEPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_QuadrangleH1HierarchicalFEPointer1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_QuadrangleH1HierarchicalFEPointer1()"
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

END PROCEDURE obj_QuadrangleH1HierarchicalFEPointer1

!----------------------------------------------------------------------------
!                                              QuadrangleH1HierarchicalFEPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_QuadrangleH1HierarchicalFEPointer2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_QuadrangleH1HierarchicalFEPointer2()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ALLOCATE (ans)

CALL ans%Initiate( &
  elemType=TypeElemNameOpt%Quadrangle, nsd=nsd, baseContinuity="H1", &
  baseInterpolation="Hierarchical", fetype=TypeFEVariableOpt%scalar, &
  cellOrder=cellOrder, faceOrder=faceOrder, cellOrient=cellOrient, &
  tCell=3_I4B, faceOrient=faceOrient, tFace=4_I4B, &
  quadratureIsHomogeneous=quadratureIsHomogeneous, &
  quadratureIsOrder=.TRUE., quadratureOrder=quadratureOrder, &
  quadratureType=quadratureType, quadratureAlpha=quadratureAlpha, &
  quadratureBeta=quadratureBeta, quadratureLambda=quadratureLambda)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_QuadrangleH1HierarchicalFEPointer2

!----------------------------------------------------------------------------
!                                                                   SetOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetOrder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetOrder()"
#endif

LOGICAL(LGT) :: isok
INTEGER(I4B) :: cellOrder0(3), faceOrder0(3, 4), ii, jj, tdof

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = (PRESENT(cellOrder) .AND. PRESENT(faceOrder)) .OR. PRESENT(order)
CALL AssertError1(isok, myName, &
                  "(cellOrder, faceOrder) or order must be provided.")
#endif

isok = PRESENT(cellOrder) .AND. PRESENT(faceOrder)

IF (isok) THEN
  CALL obj%opt%SetCellOrder(cellOrder=cellOrder, tCell=tCell, &
                            errCheck=errCheck)
  CALL obj%opt%SetFaceOrder(faceOrder=faceOrder, tFace=tFace, &
                            errCheck=errCheck)
ELSE
  cellOrder0 = order
  faceOrder0 = order
  CALL obj%opt%SetCellOrder(cellOrder=cellOrder0, tCell=3_I4B)
  CALL obj%opt%SetFaceOrder(faceOrder=faceOrder0, tFace=4_I4B)
END IF

CALL obj%opt%GetCellOrder(ans=cellOrder0, tsize=ii)
CALL obj%opt%GetFaceOrder(ans=faceOrder0, nrow=ii, ncol=jj)

tdof = GetHierarchicalDOF_Quadrangle( &
       pb=cellOrder0(1), qb=cellOrder0(2), pe3=faceOrder0(1, 1), &
       pe4=faceOrder0(1, 3), qe1=faceOrder0(1, 4), qe2=faceOrder0(1, 2), &
       opt="A")

CALL obj%opt%SetTotalDOF(tdof)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetOrder

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

CALL obj%opt%QuadrangleH1HieFE_GetLocalElemShapeData(elemsd=elemsd, &
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

CALL obj%opt%QuadrangleH1HieFE_GetLocalFacetElemShapeData( &
  elemsd=elemsd, facetElemsd=facetElemsd, quad=quad, facetQuad=facetQuad, &
  localFaceNumber=localFaceNumber)

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

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%opt%QuadrangleH1HieFE_GetGlobalElemShapeData( &
  elemsd=elemsd, xij=xij, geoelemsd=geoelemsd)

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

CALL obj%opt%QuadrangleH1HieFE_GetGlobalFacetElemShapeData( &
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
INTEGER(I4B) :: tArgs, ii, nips, nns, nsd, faceCon(tVertices, 4)
REAL(DFP) :: args(4), scale, vertexVal(tVertices), xijLine(3, tVertices), &
             vertexInterpol
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
nns = facetElemsd%nns
nsd = facetElemsd%nsd

tArgs = func%GetNumArgs()
scale = 0.0_DFP
vertexVal = 0.0_DFP

onlyFaceBubble0 = Input(option=onlyFaceBubble, default=.FALSE.)

IF (onlyFaceBubble0) THEN
  faceCon = FacetConnectivity_Quadrangle()
  xijLine(1:nsd, :) = xij(1:nsd, faceCon(:, localFaceNumber))

  DO ii = 1, tVertices
    args(1:nsd) = xijLine(1:nsd, ii)
    CALL func%GetScalarValue(args=args, val=vertexVal(ii))
  END DO

  scale = 1.0_DFP
END IF

args = 0.0_DFP
DO ii = 1, nips
  args(1:nsd) = facetElemsd%coord(1:nsd, ii)
  CALL func%GetScalarValue(args=args, val=funcValue(ii))

  vertexInterpol = DOT_PRODUCT(facetElemsd%N(1:tVertices, ii), &
                               vertexVal(1:tVertices))

  funcValue(ii) = funcValue(ii) - scale * vertexInterpol
END DO

CALL obj%GetFacetDOFValueFromQuadrature( &
  elemsd=elemsd, facetElemsd=facetElemsd, xij=xij, &
  localFaceNumber=localFaceNumber, func=funcValue, ans=ans, tsize=tsize, &
  massMat=massMat, ipiv=ipiv, onlyFaceBubble=onlyFaceBubble, &
  tVertices=tVertices)

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
