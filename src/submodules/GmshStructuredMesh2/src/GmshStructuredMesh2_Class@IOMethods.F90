! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
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

SUBMODULE(GmshStructuredMesh2_Class) IOMethods
USE Display_Method, ONLY: Display
USE RealMatrix_Method, ONLY: RealMatrixDisplay => Display
USE RealVector_Method, ONLY: RealVectorDisplay => Display
USE IntVector_Method, ONLY: IntVectorDisplay => Display
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: &
  modName = "GmshStructuredMesh2_Class@IOMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Display(msg, unitno=unitno)
CALL Display(obj%recombineAll, "recombineAll: ", unitno=unitno)
CALL obj%filename%Display("filename: ", unitno=unitno)

CALL Display(obj%tPoints, "tPoints : ", unitno=unitno)

CALL RealMatrixDisplay(obj%points(1), "pointsOnAxis1 : ", unitno=unitno)
CALL RealMatrixDisplay(obj%points(2), "pointsOnAxis2 : ", unitno=unitno)

CALL Display(obj%allPoints, "All Points : ", unitno=unitno)

CALL IntVectorDisplay(obj%transfinitePoints(1), &
                      "transfinitePointsOnAxis1: ", &
                      unitno=unitno)
CALL IntVectorDisplay(obj%transfiniteMeshType(1), &
                      "transfiniteMeshTypeOnAxis1: ", &
                      unitno=unitno)
CALL RealVectorDisplay(obj%transfiniteCoeff(1), &
                       "transfiniteCoeff1: ", &
                       unitno=unitno)

CALL IntVectorDisplay(obj%transfinitePoints(2), &
                      "transfinitePointsOnAxis2: ", &
                      unitno=unitno)
CALL IntVectorDisplay(obj%transfiniteMeshType(2), &
                      "transfiniteMeshTypeOnAxis2: ", &
                      unitno=unitno)
CALL RealVectorDisplay(obj%transfiniteCoeff(2), &
                       "transfiniteCoeff2: ", &
                       unitno=unitno)

CALL Display(obj%edge_tfp, "Transfinite Points on edges : ", &
             unitno=unitno)

CALL Display(obj%edge_coeff, "Transfinite coeff on edges : ", &
             unitno=unitno)

CALL Display(obj%edges, "Edges : ", unitno=unitno)

CALL Display(obj%tEdges1, "Total edges in x direction : ", unitno=unitno)
CALL Display(obj%tEdges2, "Total edges in y direction : ", unitno=unitno)
CALL Display(obj%tEdges, "Total edges : ", unitno=unitno)

CALL Display(obj%curveLoops, "curveLoops : ", unitno=unitno)

CALL Display(obj%tSurfaces, "Total surfaces : ", unitno=unitno)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE IOMethods
