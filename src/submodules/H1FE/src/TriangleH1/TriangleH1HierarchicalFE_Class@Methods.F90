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

SUBMODULE(TriangleH1HierarchicalFE_Class) Methods
USE BaseType, ONLY: TypeElemNameOpt, TypePolynomialOpt, &
                    TypeFEVariableOpt, TypeInterpolationOpt
USE InputUtility, ONLY: Input
USE Display_Method, ONLY: ToString
USE TriangleInterpolationUtility, ONLY: GetTotalDOF_Triangle, &
                                        InterpolationPoint_Triangle_, &
                                        GetHierarchicalDOF_Triangle

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                            TriangleH1HierarchicalFEPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_TriangleH1HierarchicalFEPointer1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_TriangleH1HierarchicalFEPointer1()"
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

END PROCEDURE obj_TriangleH1HierarchicalFEPointer1

!----------------------------------------------------------------------------
!                                             TriangleH1HierarchicalFEPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_TriangleH1HierarchicalFEPointer2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_TriangleH1HierarchicalFEPointer2()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ALLOCATE (ans)

CALL ans%Initiate( &
  elemType=TypeElemNameOpt%Triangle, nsd=nsd, baseContinuity="H1", &
  baseInterpolation="Hierarchical", fetype=TypeFEVariableOpt%scalar, &
  cellOrder=cellOrder, faceOrder=faceOrder, cellOrient=cellOrient, &
  faceOrient=faceOrient, tCell=3_I4B, tFace=3_I4B, &
  quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
  quadratureOrder=quadratureOrder, quadratureType=quadratureType)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_TriangleH1HierarchicalFEPointer2

!----------------------------------------------------------------------------
!                                                                    SetOrder
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
  tdof = GetHierarchicalDOF_Triangle( &
         order=order, pe1=order, pe2=order, pe3=order, opt="A")
  CALL obj%opt%SetTotalDOF(tdof=tdof)

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

tdof = GetHierarchicalDOF_Triangle( &
       order=cellOrder0(1), pe1=faceOrder0(1, 1), pe2=faceOrder0(1, 2), &
       pe3=faceOrder0(1, 3), opt="A")

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

CALL obj%opt%TriangleH1HieFE_GetLocalElemShapeData(elemsd=elemsd, quad=quad)

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

CALL obj%opt%TriangleH1LagFE_GetLocalFacetElemShapeData( &
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

CALL obj%opt%TriangleH1LagFE_GetGlobalElemShapeData(elemsd=elemsd, xij=xij, &
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

CALL obj%opt%TriangleH1LagFE_GetGlobalFacetElemShapeData( &
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
