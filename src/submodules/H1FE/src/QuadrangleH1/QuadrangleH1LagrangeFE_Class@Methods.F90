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
USE BaseType, ONLY: TypeElemNameOpt
USE BaseType, ONLY: TypeFEVariableOpt
USE BaseType, ONLY: TypeInterpolationOpt
USE BaseType, ONLY: TypePolynomialOpt
USE Display_Method, ONLY: Display
USE Display_Method, ONLY: ToString
USE InputUtility, ONLY: Input
USE LineInterpolationUtility, ONLY: GetTotalDOF_Line
USE LineInterpolationUtility, ONLY: InterpolationPoint_Line_
USE QuadrangleInterpolationUtility, ONLY: GetTotalDOF_Quadrangle
USE QuadrangleInterpolationUtility, ONLY: InterpolationPoint_Quadrangle_
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "QuadrangleH1LagragngeFE_Class@Methods.F90"
#endif
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

CALL obj%opt%QuadrangleH1LagFE_GetGlobalElemShapeData( &
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

CALL obj%opt%QuadrangleH1LagFE_GetGlobalFacetElemShapeData( &
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
