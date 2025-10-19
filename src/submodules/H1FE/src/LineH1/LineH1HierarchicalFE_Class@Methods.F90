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

SUBMODULE(LineH1HierarchicalFE_Class) Methods
USE BaseType, ONLY: TypeElemNameOpt, TypePolynomialOpt, &
                    TypeFEVariableOpt, TypeInterpolationOpt
USE InputUtility, ONLY: Input
USE Display_Method, ONLY: ToString

USE LineInterpolationUtility, ONLY: GetTotalDOF_Line, &
                                    InterpolationPoint_Line_, &
                                    LagrangeDOF_Line

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                     LineH1HierarchicalFEPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_LineH1HierarchicalFEPointer1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_LineH1HierarchicalFEPointer1()"
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

END PROCEDURE obj_LineH1HierarchicalFEPointer1

!----------------------------------------------------------------------------
!                                                     LineH1HierarchicalFEPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_LineH1HierarchicalFEPointer2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_LineH1HierarchicalFEPointer2()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ALLOCATE (ans)

CALL ans%Initiate( &
  elemType=TypeElemNameOpt%line, nsd=nsd, baseContinuity="H1", &
  baseInterpolation="Hierarchical", fetype=TypeFEVariableOpt%scalar, &
  order=order, cellOrient=cellOrient, tcell=3_I4B, &
  quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
  quadratureOrder=quadratureOrder, quadratureType=quadratureType, &
  quadratureAlpha=quadratureAlpha, quadratureBeta=quadratureBeta, &
  quadratureLambda=quadratureLambda)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_LineH1HierarchicalFEPointer2

!----------------------------------------------------------------------------
!                                                                    SetOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetOrder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetOrder()"
#endif

LOGICAL(LGT) :: isok
INTEGER(I4B) :: cellOrder0(1), tdof

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = PRESENT(order) .OR. PRESENT(anisoOrder) .OR. PRESENT(cellOrder)
CALL AssertError1(isok, myName, &
    "At least one of 'order', 'anisoOrder', or 'cellOrder' must be provided.")
#endif

cellOrder0(1) = 0

isok = PRESENT(order)
IF (isok) cellOrder0(1) = order

isok = PRESENT(anisoOrder)
IF (isok) cellOrder0(1) = anisoOrder(1)

isok = PRESENT(cellOrder)
IF (isok) cellOrder0(1) = cellOrder(1)

CALL obj%opt%SetCellOrder(cellOrder=cellOrder0, tCell=1_I4B, &
                          errCheck=errCheck)

tdof = LagrangeDOF_Line(order=cellOrder0(1))

CALL obj%opt%SetTotalDOF(tdof=tdof)

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

CALL obj%opt%LineH1HieFE_GetLocalElemShapeData(elemsd=elemsd, quad=quad)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetLocalElemShapeData

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

#include "../../../include/errors.F90"

END SUBMODULE Methods
