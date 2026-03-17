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

MODULE GmshModelOcc_Class
USE GlobalData, ONLY: DFP, I4B, LGT
USE GmshModelOccMesh_Class, ONLY: GmshModelOccMesh_
IMPLICIT NONE

PRIVATE
PUBLIC :: GmshModelOcc_
PUBLIC :: TypeGmshModelOcc
PUBLIC :: GmshModelOccPointer_

!----------------------------------------------------------------------------
!                                                              GmshModelOcc_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-17
! summary: GmshModel Occ model

TYPE :: GmshModelOcc_
  PRIVATE
  TYPE(GmshModelOccMesh_), PUBLIC, POINTER :: mesh => NULL()

CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  PROCEDURE, PUBLIC, PASS(obj) :: AddPoint => obj_AddPoint
  PROCEDURE, PUBLIC, PASS(obj) :: AddLine => obj_AddLine
  PROCEDURE, PUBLIC, PASS(obj) :: AddCircleArc => obj_AddCircleArc
  PROCEDURE, PUBLIC, PASS(obj) :: AddCircle => obj_AddCircle
  PROCEDURE, PUBLIC, PASS(obj) :: AddEllipseArc => obj_AddEllipseArc
  PROCEDURE, PUBLIC, PASS(obj) :: AddEllipse => obj_AddEllipse
  PROCEDURE, PUBLIC, PASS(obj) :: AddSpline => obj_AddSpline
  PROCEDURE, PUBLIC, PASS(obj) :: AddBSpline => obj_AddBSpline
  PROCEDURE, PUBLIC, PASS(obj) :: AddBezier => obj_AddBezier
  PROCEDURE, PUBLIC, PASS(obj) :: AddWire => obj_AddWire
  PROCEDURE, PUBLIC, PASS(obj) :: AddCurveLoop => obj_AddCurveLoop
  PROCEDURE, PUBLIC, PASS(obj) :: AddRectangle => obj_AddRectangle
  PROCEDURE, PUBLIC, PASS(obj) :: AddDisk => obj_AddDisk
  PROCEDURE, PUBLIC, PASS(obj) :: AddPlaneSurface => obj_AddPlaneSurface
  PROCEDURE, PUBLIC, PASS(obj) :: AddSurfaceFilling => obj_AddSurfaceFilling
  PROCEDURE, PUBLIC, PASS(obj) :: AddBSplineFilling => obj_AddBSplineFilling
  PROCEDURE, PUBLIC, PASS(obj) :: AddBezierFilling => obj_AddBezierFilling
  PROCEDURE, PUBLIC, PASS(obj) :: AddBSplineSurface => obj_AddBSplineSurface
  PROCEDURE, PUBLIC, PASS(obj) :: AddTrimmedSurface => obj_AddTrimmedSurface
  PROCEDURE, PUBLIC, PASS(obj) :: AddSurfaceLoop => obj_AddSurfaceLoop
  PROCEDURE, PUBLIC, PASS(obj) :: AddVolume => obj_AddVolume
  PROCEDURE, PUBLIC, PASS(obj) :: AddSphere => obj_AddSphere
  PROCEDURE, PUBLIC, PASS(obj) :: AddBox => obj_AddBox
  PROCEDURE, PUBLIC, PASS(obj) :: AddCylinder => obj_AddCylinder
  PROCEDURE, PUBLIC, PASS(obj) :: AddCone => obj_AddCone
  PROCEDURE, PUBLIC, PASS(obj) :: AddWedge => obj_AddWedge
  PROCEDURE, PUBLIC, PASS(obj) :: AddTorus => obj_AddTorus
  PROCEDURE, PUBLIC, PASS(obj) :: AddThruSections => obj_AddThruSections
  PROCEDURE, PUBLIC, PASS(obj) :: Synchronize => obj_Synchronize
END TYPE GmshModelOcc_

!----------------------------------------------------------------------------
!                                                           TypeGmshModelOcc
!----------------------------------------------------------------------------

TYPE(GmshModelOcc_), PARAMETER :: TypeGmshModelOcc = GmshModelOcc_()

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: GmshModelOccPointer_
  CLASS(GmshModelOcc_), POINTER :: Ptr => NULL()
END TYPE GmshModelOccPointer_

!----------------------------------------------------------------------------
!                                                                  Initiate
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Initiate(obj)
    CLASS(GmshModelOcc_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_AddPoint(obj, x, y, z, meshSize, tag) RESULT(ans)
    CLASS(GmshModelOcc_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: x, y, z, meshSize
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
    INTEGER(I4B) :: ans
  END FUNCTION obj_AddPoint
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_AddLine(obj, startTag, endTag, tag) RESULT(ans)
    CLASS(GmshModelOcc_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
    INTEGER(I4B), INTENT(IN) :: startTag, endTag
    INTEGER(I4B) :: ans
  END FUNCTION obj_AddLine
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_AddCircleArc(obj, startTag, centerTag, endTag, tag) &
    RESULT(ans)
    CLASS(GmshModelOcc_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
    INTEGER(I4B), INTENT(IN) :: startTag, centerTag, endTag
    INTEGER(I4B) :: ans
  END FUNCTION obj_AddCircleArc
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_AddCircle(obj, x, y, z, r, tag, angle1, angle2) &
    RESULT(ans)
    CLASS(GmshModelOcc_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
    REAL(DFP), INTENT(IN) :: x, y, z, r
    REAL(DFP), OPTIONAL, INTENT(IN) :: angle1, angle2
    INTEGER(I4B) :: ans
  END FUNCTION obj_AddCircle
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_AddEllipseArc( &
    obj, startTag, centerTag, majorTag, endTag, tag) RESULT(ans)
    CLASS(GmshModelOcc_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
    INTEGER(I4B), INTENT(IN) :: startTag, centerTag, majorTag, endTag
    INTEGER(I4B) :: ans
  END FUNCTION obj_AddEllipseArc
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_AddEllipse(obj, x, y, z, r1, r2, tag, angle1, angle2) &
    RESULT(ans)
    CLASS(GmshModelOcc_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
    REAL(DFP), INTENT(IN) :: x, y, z, r1, r2, angle1, angle2
    INTEGER(I4B) :: ans
  END FUNCTION obj_AddEllipse
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_AddSpline(obj, pointTags, tag) RESULT(ans)
    CLASS(GmshModelOcc_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: pointTags(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
    INTEGER(I4B) :: ans
  END FUNCTION obj_AddSpline
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_AddBSpline( &
    obj, pointTags, tag, degree, weights, knots, multiplicities) RESULT(ans)
    CLASS(GmshModelOcc_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: pointTags(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: multiplicities(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: weights(:), knots(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
    INTEGER(I4B), INTENT(IN) :: degree
    INTEGER(I4B) :: ans
  END FUNCTION obj_AddBSpline
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_AddBezier(obj, pointTags, tag) RESULT(ans)
    CLASS(GmshModelOcc_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
    INTEGER(I4B), INTENT(IN) :: pointTags(:)
    INTEGER(I4B) :: ans
  END FUNCTION obj_AddBezier
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_AddWire(obj, curveTags, tag, checkClosed) RESULT(ans)
    CLASS(GmshModelOcc_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
    INTEGER(I4B), INTENT(IN) :: curveTags(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: checkClosed
    INTEGER(I4B) :: ans
  END FUNCTION obj_AddWire
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_AddCurveLoop(obj, curveTags, tag) &
    RESULT(ans)
    CLASS(GmshModelOcc_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
    INTEGER(I4B), INTENT(IN) :: curveTags(:)
    INTEGER(I4B) :: ans
  END FUNCTION obj_AddCurveLoop
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_AddRectangle( &
    obj, x, y, z, dx, dy, tag, roundedRadius) RESULT(ans)
    CLASS(GmshModelOcc_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: x, y, z, dx, dy, roundedRadius
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
    INTEGER(I4B) :: ans
  END FUNCTION obj_AddRectangle
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_AddDisk(obj, xc, yc, zc, rx, ry, tag) RESULT(ans)
    CLASS(GmshModelOcc_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: xc, yc, zc, rx, ry
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
    INTEGER(I4B) :: ans
  END FUNCTION obj_AddDisk
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_AddPlaneSurface(obj, wireTags, tag) RESULT(ans)
    CLASS(GmshModelOcc_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
    INTEGER(I4B), INTENT(IN) :: wireTags(:)
    INTEGER(I4B) :: ans
  END FUNCTION obj_AddPlaneSurface
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_AddSurfaceFilling(obj, wireTag, tag, pointTags) &
    RESULT(ans)
    CLASS(GmshModelOcc_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
    INTEGER(I4B), INTENT(IN) :: wireTag, pointTags(:)
    INTEGER(I4B) :: ans
  END FUNCTION obj_AddSurfaceFilling
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_AddBSplineFilling(obj, wireTag, tag, typeOfFilling) &
    RESULT(ans)
    CLASS(GmshModelOcc_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
    INTEGER(I4B), INTENT(IN) :: wireTag
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: typeOfFilling
    INTEGER(I4B) :: ans
  END FUNCTION obj_AddBSplineFilling
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_AddBezierFilling(obj, wireTag, tag, typeOfFilling) &
    RESULT(ans)
    CLASS(GmshModelOcc_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
    INTEGER(I4B), INTENT(IN) :: wireTag
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: typeOfFilling
    INTEGER(I4B) :: ans
  END FUNCTION obj_AddBezierFilling
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_AddBSplineSurface( &
    obj, pointTags, numPointsU, tag, degreeU, degreeV, weights, knotsU, &
    knotsV, multiplicitiesU, multiplicitiesV, wireTags, wire3D) &
    RESULT(ans)
    CLASS(GmshModelOcc_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: pointTags(:)
    INTEGER(I4B), INTENT(IN) :: numPointsU, degreeU, degreeV
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: multiplicitiesU(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: multiplicitiesV(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: wireTags(:), wire3D
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
    REAL(DFP), OPTIONAL, INTENT(IN) :: weights(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: knotsU(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: knotsV(:)
    INTEGER(I4B) :: ans
  END FUNCTION obj_AddBSplineSurface
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_AddBezierSurface(obj, pointTags, numPointsU, &
                                       tag, wireTags, wire3D) RESULT(ans)
    CLASS(GmshModelOcc_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: pointTags(:), numPointsU
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag, wireTags(:), wire3D
    INTEGER(I4B) :: ans
  END FUNCTION obj_AddBezierSurface
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_AddTrimmedSurface(obj, surfaceTag, wireTags, &
                                        wire3D, tag) RESULT(ans)
    CLASS(GmshModelOcc_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: wireTags(:)
    INTEGER(I4B), INTENT(IN) :: surfaceTag
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag, wire3D
    INTEGER(I4B) :: ans
  END FUNCTION obj_AddTrimmedSurface
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_AddSurfaceLoop(obj, surfaceTags, tag, sewing) &
    RESULT(ans)
    CLASS(GmshModelOcc_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: surfaceTags(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag, sewing
    INTEGER(I4B) :: ans
  END FUNCTION obj_AddSurfaceLoop
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_AddVolume(obj, shellTags, tag) RESULT(ans)
    CLASS(GmshModelOcc_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: shellTags(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
    INTEGER(I4B) :: ans
  END FUNCTION obj_AddVolume
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_AddSphere( &
    obj, xc, yc, zc, radius, tag, angle1, angle2, angle3) RESULT(ans)
    CLASS(GmshModelOcc_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: xc, yc, zc, radius
    REAL(DFP), OPTIONAL, INTENT(IN) :: angle1, angle2, angle3
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
    INTEGER(I4B) :: ans
  END FUNCTION obj_AddSphere
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_AddBox(obj, x, y, z, dx, dy, dz, tag) &
    RESULT(ans)
    CLASS(GmshModelOcc_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
    REAL(DFP), INTENT(IN) :: x, y, z, dx, dy, dz
    INTEGER(I4B) :: ans
  END FUNCTION obj_AddBox
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_AddCylinder(obj, x, y, z, dx, dy, dz, r, tag, angle) &
    RESULT(ans)
    CLASS(GmshModelOcc_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
    REAL(DFP), INTENT(IN) :: x, y, z, dx, dy, dz, r
    REAL(DFP), OPTIONAL, INTENT(IN) :: angle
    INTEGER(I4B) :: ans
  END FUNCTION obj_AddCylinder
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_AddCone(obj, x, y, z, dx, dy, dz, r1, r2, tag, angle) &
    RESULT(ans)
    CLASS(GmshModelOcc_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
    REAL(DFP), INTENT(IN) :: x, y, z, dx, dy, dz, r1, r2
    REAL(DFP), OPTIONAL, INTENT(IN) :: angle
    INTEGER(I4B) :: ans
  END FUNCTION obj_AddCone
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_AddWedge(obj, x, y, z, dx, dy, dz, tag, ltx) &
    RESULT(ans)
    CLASS(GmshModelOcc_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
    REAL(DFP), INTENT(IN) :: x, y, z, dx, dy, dz
    REAL(DFP), OPTIONAL, INTENT(IN) :: ltx
    INTEGER(I4B) :: ans
  END FUNCTION obj_AddWedge
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_AddTorus(obj, x, y, z, r1, r2, tag, angle) &
    RESULT(ans)
    CLASS(GmshModelOcc_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
    REAL(DFP), INTENT(IN) :: x, y, z, r1, r2
    REAL(DFP), OPTIONAL, INTENT(IN) :: angle
    INTEGER(I4B) :: ans
  END FUNCTION obj_AddTorus
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_AddThruSections( &
    obj, wireTags, outDimTags, tag, makeSolid, makeRuled, maxDegree) &
    RESULT(ans)
    CLASS(GmshModelOcc_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: wireTags(:)
    INTEGER(I4B), ALLOCATABLE, INTENT(OUT) :: outDimTags(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag, makeSolid, makeRuled, &
                                          maxDegree
    INTEGER(I4B) :: ans
  END FUNCTION obj_AddThruSections
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_Synchronize(obj) RESULT(ans)
    CLASS(GmshModelOcc_), INTENT(INOUT) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_Synchronize
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE GmshModelOcc_Class
