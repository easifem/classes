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
                                          GetHierarchicalDOF_Quadrangle

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

isok = PRESENT(order)
IF (isok) THEN
  cellOrder0 = order
  faceOrder0 = order
  CALL obj%opt%SetCellOrder(cellOrder=cellOrder0, tCell=3_I4B)
  CALL obj%opt%SetFaceOrder(faceOrder=faceOrder0, tFace=4_I4B)
  tdof = GetHierarchicalDOF_Quadrangle( &
      pb=order, qb=order, pe3=order, pe4=order, qe1=order, qe2=order, opt="A")
  CALL obj%opt%SetTotalDOF(tdof)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

#ifdef DEBUG_VER
isok = PRESENT(cellOrder) .AND. PRESENT(faceOrder)
CALL AssertError1(isok, myName, &
                  "cellOrder and faceOrder must be provided.")
#endif

isok = PRESENT(cellOrder)
IF (isok) THEN
CALL obj%opt%SetCellOrder(cellOrder=cellOrder, tCell=tCell, errCheck=errCheck)
END IF

isok = PRESENT(faceOrder)
IF (isok) THEN
CALL obj%opt%SetFaceOrder(faceOrder=faceOrder, tFace=tFace, errCheck=errCheck)
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
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../../include/errors.F90"

END SUBMODULE Methods
