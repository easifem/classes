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

SUBMODULE(GmshStructuredMesh2_Class) GenerateMethods
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: &
  modName = "GmshStructuredMesh2_Class@GenerateMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                 Generate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Generate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Generate()"
#endif
INTEGER(I4B) :: ierr
INTEGER(I4B), PARAMETER :: nsd = 2

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                  'GeneratePoints()...')
#endif

CALL obj%GeneratePoints(gmsh)

#ifdef DEBUG_VER
CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                  'GenerateCurves()...')
#endif

CALL obj%GenerateCurves(gmsh)

#ifdef DEBUG_VER
CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                  'GenerateSurfaces()...')
#endif

CALL obj%GenerateSurfaces(gmsh)

#ifdef DEBUG_VER
CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                  'calling gmsh%model%geo%Synchronize()...')
#endif

ierr = gmsh%model%geo%Synchronize()

#ifdef DEBUG_VER
CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                  'calling gmsh%option%SetNumber()...')
#endif

ierr = gmsh%option%SetNumber(name="Mesh.SaveAll", VALUE=1_I4B)

#ifdef DEBUG_VER
CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                  'calling gmsh%model%mesh%Generate()...')
#endif

ierr = gmsh%model%mesh%Generate(nsd)

#ifdef DEBUG_VER
CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                  'calling gmsh%Write()...')
#endif
ierr = gmsh%WRITE(obj%filename%chars())

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Generate

!----------------------------------------------------------------------------
!                                                             GeneratePoints
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GeneratePoints
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GeneratePoints()"
#endif
INTEGER(I4B) :: ipoint, ierr, tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = SIZE(obj%allPoints, 2)

DO ipoint = 1, tsize
  ierr = gmsh%model%geo%addPoint( &
         x=obj%allPoints(1, ipoint), &
         y=obj%allPoints(2, ipoint), &
         z=obj%allPoints(3, ipoint), &
         meshSize=math%one)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GeneratePoints

!----------------------------------------------------------------------------
!                                                             GenerateCurves
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GenerateCurves
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GenerateCurves()"
#endif
INTEGER(I4B) :: iedge, ierr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

DO iedge = 1, obj%tEdges

  ierr = gmsh%model%geo%AddLine(obj%edges(1, iedge), obj%edges(2, iedge))

  ierr = gmsh%model%geo%mesh%SetTransfiniteCurve( &
         tag=iedge, &
         nPoints=obj%edge_tfp(iedge), &
         meshType=obj%edge_meshType(iedge)%chars(), &
         coef=obj%edge_coeff(iedge))

END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GenerateCurves

!----------------------------------------------------------------------------
!                                                           GenerateSurfaces
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GenerateSurfaces
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GenerateSurfaces()"
#endif
INTEGER(I4B) :: isurf, ierr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

DO isurf = 1, obj%tSurfaces
  ierr = gmsh%model%geo%AddCurveLoop( &
         curveTags=obj%curveLoops(:, isurf), &
         reorient=math%yes)
END DO

DO isurf = 1, obj%tSurfaces
  ierr = gmsh%model%geo%AddPlaneSurface(wireTags=[isurf])
  ierr = gmsh%model%geo%mesh%SetTransfiniteSurface(tag=isurf)
END DO

IF (obj%recombineAll) THEN
  DO isurf = 1, obj%tSurfaces
    ierr = gmsh%model%geo%mesh%SetRecombine(dim=math%two_i, tag=isurf)
  END DO
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GenerateSurfaces

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GenerateMethods
