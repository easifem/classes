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

MODULE GmshModelGeo_Class
USE GlobalData, ONLY: DFP, I4B, LGT
USE BaseType, ONLY: math => TypeMathOpt
USE GmshModelGeoMesh_Class, ONLY: GmshModelGeoMesh_
USE ExceptionHandler_Class, ONLY: e
IMPLICIT NONE

PRIVATE
PUBLIC :: GmshModelGeoPointer_
PUBLIC :: GmshModelGeo_
PUBLIC :: TypeGmshModelGeo

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: GmshModelGeo_
  TYPE(GmshModelGeoMesh_), POINTER :: mesh => NULL()
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  PROCEDURE, PUBLIC, NOPASS :: AddPoint => obj_AddPoint
  PROCEDURE, PUBLIC, NOPASS :: AddLine => obj_AddLine
  PROCEDURE, PUBLIC, NOPASS :: AddCircleArc => obj_AddCircleArc
  PROCEDURE, PUBLIC, NOPASS :: AddEllipseArc => obj_AddEllipseArc
  PROCEDURE, PUBLIC, NOPASS :: AddSpline => obj_AddSpline
  PROCEDURE, PUBLIC, NOPASS :: AddBSpline => obj_AddBSpline
  PROCEDURE, PUBLIC, NOPASS :: AddBezier => obj_AddBezier
  PROCEDURE, PUBLIC, NOPASS :: AddPolyline => obj_AddPolyline
  PROCEDURE, PUBLIC, NOPASS :: AddCompoundSpline => obj_AddCompoundSpline
  PROCEDURE, PUBLIC, NOPASS :: AddCompoundBSpline => &
    obj_AddCompoundBSpline
  PROCEDURE, PUBLIC, NOPASS :: AddCurveLoop => obj_AddCurveLoop
  PROCEDURE, PUBLIC, NOPASS :: AddCurveLoops => obj_AddCurveLoops
  PROCEDURE, PUBLIC, NOPASS :: AddPlaneSurface => obj_AddPlaneSurface
  PROCEDURE, PUBLIC, NOPASS :: AddSurfaceFilling => obj_AddSurfaceFilling
  PROCEDURE, PUBLIC, NOPASS :: AddSurfaceLoop => obj_AddSurfaceLoop
  PROCEDURE, PUBLIC, NOPASS :: AddVolume => obj_AddVolume
  PROCEDURE, PUBLIC, NOPASS :: AddGeometry => obj_AddGeometry
  PROCEDURE, PUBLIC, NOPASS :: AddPointOnGeometry => obj_AddPointOnGeometry
  PROCEDURE, PUBLIC, NOPASS :: Extrude => obj_Extrude
  PROCEDURE, PUBLIC, NOPASS :: Revolve => obj_Revolve
  PROCEDURE, PUBLIC, NOPASS :: Twist => obj_Twist
  PROCEDURE, PUBLIC, NOPASS :: ExtrudeBoundaryLayer => &
    obj_ExtrudeBoundaryLayer
  PROCEDURE, PUBLIC, NOPASS :: Translate => obj_Translate
  PROCEDURE, PUBLIC, NOPASS :: Rotate => obj_Rotate
  PROCEDURE, PUBLIC, NOPASS :: Dilate => obj_Dilate
  PROCEDURE, PUBLIC, NOPASS :: Mirror => obj_Mirror
  PROCEDURE, PUBLIC, NOPASS :: Symmetrize => obj_Symmetrize
  PROCEDURE, PUBLIC, NOPASS :: Copy => obj_Copy
  PROCEDURE, PUBLIC, NOPASS :: Remove => obj_Remove
  PROCEDURE, PUBLIC, NOPASS :: RemoveAllDuplicates => &
    obj_RemoveAllDuplicates
  PROCEDURE, PUBLIC, NOPASS :: SplitCurve => obj_SplitCurve
  PROCEDURE, PUBLIC, NOPASS :: GetMaxTag => obj_GetMaxTag
  PROCEDURE, PUBLIC, NOPASS :: SetMaxTag => obj_SetMaxTag
  PROCEDURE, PUBLIC, NOPASS :: AddPhysicalGroup => obj_AddPhysicalGroup
  PROCEDURE, PUBLIC, NOPASS :: RemovePhysicalGroups => &
    obj_RemovePhysicalGroups
  PROCEDURE, PUBLIC, NOPASS :: Synchronize => obj_Synchronize
END TYPE GmshModelGeo_

!----------------------------------------------------------------------------
!                                                           TypeGmshModelGeo
!----------------------------------------------------------------------------

TYPE(GmshModelGeo_), PARAMETER :: TypeGmshModelGeo = GmshModelGeo_()

!----------------------------------------------------------------------------
!                                                       GmshModelGeoPointer_
!----------------------------------------------------------------------------

TYPE :: GmshModelGeoPointer_
  CLASS(GmshModelGeo_), POINTER :: Ptr => NULL()
END TYPE GmshModelGeoPointer_

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-17
! summary:         Initiate GmshModelGeo

INTERFACE
  MODULE SUBROUTINE obj_Initiate(obj)
    CLASS(GmshModelGeo_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   AddPoint
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-17
! summary: Add a geometrical point
!
!# AddPoint
!
! Add a geometrical point in the built-in CAD representation, at coordinates
! (`x', `y', `z'). If `meshSize' is > 0, add a meshing constraint at that
! point. If `tag' is positive, set the tag explicitly; otherwise a new tag
! is
! selected automatically. Return the tag of the point. (Note that the point
! will be added in the current model only after `synchronize' is called.
! This
! behavior holds for all the entities added in the geo module.)

INTERFACE
  MODULE FUNCTION obj_AddPoint(x, y, z, meshSize, tag) RESULT(ans)
    CLASS(*), INTENT(IN) :: x, y, z, meshSize
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
    INTEGER(I4B) :: ans
  END FUNCTION obj_AddPoint
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-17
! summary: Add a line
!
!# AddLine
!
! Add a straight line segment in the built-in CAD representation, between the
! two points with tags `startTag' and `endTag'. If `tag' is positive, set the
! tag explicitly; otherwise a new tag is selected automatically. Return the
! tag of the line.

INTERFACE
  MODULE FUNCTION obj_AddLine(startTag, endTag, tag) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: startTag, endTag
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
    INTEGER(I4B) :: ans
  END FUNCTION obj_AddLine
END INTERFACE

!----------------------------------------------------------------------------
!                                                               AddCircleArc
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-17
! summary: Add Circle Arc
!
!# AddCircleArc
!
! Add a circle arc (strictly smaller than Pi) in the built-in CAD
! representation, between the two points with tags `startTag' and `endTag',
! and with center `centerTag'.
!
! If `tag' is positive, set the tag explicitly;
! otherwise a new tag is selected automatically. If (`nx', `ny', `nz') != (0,
! 0, 0), explicitly set the plane of the circle arc. Return the tag of the
! circle arc.

INTERFACE
  MODULE FUNCTION obj_AddCircleArc( &
    startTag, centerTag, endTag, tag, nx, ny, nz) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: startTag, endTag, centerTag
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
    CLASS(*), OPTIONAL, INTENT(IN) :: nx, ny, nz
    INTEGER(I4B) :: ans
  END FUNCTION obj_AddCircleArc
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-17
! summary: Add an ellipse arc
!
!# AddEllipseArc
!
! Add an ellipse arc (strictly smaller than Pi) in the built-in CAD
! representation, between the two points `startTag' and `endTag', and with
! center `centerTag' and major axis point `majorTag'. If `tag' is positive,
! set the tag explicitly; otherwise a new tag is selected automatically. If
! (`nx', `ny', `nz') != (0, 0, 0), explicitly set the plane of the circle
! arc. Return the tag of the ellipse arc.

INTERFACE
  MODULE FUNCTION obj_AddEllipseArc( &
    startTag, centerTag, majorTag, endTag, tag, nx, ny, nz) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: startTag, centerTag, majorTag, endTag
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
    CLASS(*), INTENT(IN) :: nx, ny, nz
    INTEGER(I4B) :: ans
  END FUNCTION obj_AddEllipseArc
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2026-03-17
! summary: Add a spline
!
!# AddSpline
!
!Add a spline (Catmull-Rom) curve in the built-in CAD representation, going
!through the points `pointTags'. If `tag' is positive, set the tag
!explicitly; otherwise a new tag is selected automatically. Create a
!periodic curve if the first and last points are the same. Return the tag of
!the spline curve.

INTERFACE
  MODULE FUNCTION obj_AddSpline(pointTags, tag) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pointTags(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
    INTEGER(I4B) :: ans
  END FUNCTION obj_AddSpline
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-17
! summary: Add a cubic bspline
!
!# AddBSpline
!
! Add a cubic b-spline curve in the built-in CAD representation, with
! `pointTags' control points
!
! If `tag' is positive, set the tag explicitly; otherwise a new tag
! is selected automatically.
!
! Creates a periodic curve if the first and last points are the same.
!
! Return the tag of the b-spline curve.

INTERFACE
  MODULE FUNCTION obj_AddBSpline(pointTags, tag) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pointTags(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
    INTEGER(I4B) :: ans
  END FUNCTION obj_AddBSpline
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-17
! summary: Add Bezier curve
!
!# AddBezier
!
! Add a Bezier curve in the built-in CAD representation, with `pointTags'
! control points.
!
! If `tag' is positive, set the tag explicitly; otherwise a
! new tag is selected automatically.
!
! Return the tag of the Bezier curve.

INTERFACE
  MODULE FUNCTION obj_AddBezier(pointTags, tag) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pointTags(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
    INTEGER(I4B) :: ans
  END FUNCTION obj_AddBezier
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-17
! summary: Add polyline
!
!# AddPolyline
!
! Add a polyline curve in the built-in CAD representation, going through the
! points `pointTags'.
!
! If `tag' is positive, set the tag explicitly; otherwise
! a new tag is selected automatically.
!
! Create a periodic curve if the first
! and last points are the same. Return the tag of the polyline curve.

INTERFACE
  MODULE FUNCTION obj_AddPolyline(pointTags, tag) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: pointTags(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
    INTEGER(I4B) :: ans
  END FUNCTION obj_AddPolyline
END INTERFACE

!----------------------------------------------------------------------------
! AddCompoundSpline
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-17
! summary: Add a spline.
!
!# AddCompoundSpline
!
!
! Add a spline (Catmull-Rom) curve in the built-in CAD representation, going
! through points sampling the curves in `curveTags'. The density of sampling
! points on each curve is governed by `numIntervals'. If `tag' is positive,
! set the tag explicitly; otherwise a new tag is selected automatically.
! Return the tag of the spline.

INTERFACE
  MODULE FUNCTION obj_AddCompoundSpline(curveTags, numIntervals, tag) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: curveTags(:)
    INTEGER(I4B), INTENT(IN) :: numIntervals
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
    INTEGER(I4B) :: ans
  END FUNCTION obj_AddCompoundSpline
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-17
! summary:         Add compound bspline
!
!# AddCompoundBSpline
!
!
! Add a b-spline curve in the built-in CAD representation, with control
! points sampling the curves in `curveTags'. The density of sampling points
! on each curve is governed by `numIntervals'. If `tag' is positive, set the
! tag explicitly; otherwise a new tag is selected automatically. Return the
! tag of the b-spline.

INTERFACE
  MODULE FUNCTION obj_AddCompoundBSpline( &
    curveTags, numIntervals, tag) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: curveTags(:)
    INTEGER(I4B), INTENT(IN) :: numIntervals
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
    INTEGER(I4B) :: ans
  END FUNCTION obj_AddCompoundBSpline
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-17
! summary:         Add curve loop
!
!# AddCurveLoop
!
!
! Add a curve loop (a closed wire) in the built-in CAD representation, formed
! by the curves `curveTags'. `curveTags' should contain (signed) tags of
! model entities of dimension 1 forming a closed loop: a negative tag
! signifies that the underlying curve is considered with reversed
! orientation. If `tag' is positive, set the tag explicitly; otherwise a new
! tag is selected automatically. If `reorient' is set, automatically reorient
! the curves if necessary. Return the tag of the curve loop.

INTERFACE
  MODULE FUNCTION obj_AddCurveLoop(curveTags, tag, reorient) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: curveTags(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: reorient
    INTEGER(I4B) :: ans
  END FUNCTION obj_AddCurveLoop
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Add curve loops in the built-in CAD representation based on the curves
!! `curveTags'. Return the `tags' of found curve loops, if any.

INTERFACE
  MODULE FUNCTION obj_AddCurveLoops(curveTags, tags) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: curveTags(:)
    INTEGER(I4B), ALLOCATABLE, INTENT(OUT) :: tags(:)
    INTEGER(I4B) :: ans
  END FUNCTION obj_AddCurveLoops
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Add a plane surface in the built-in CAD representation, defined by one or
!! more curve loops `wireTags'. The first curve loop defines the exterior
!! contour; additional curve loop define holes. If `tag' is positive, set the
!! tag explicitly; otherwise a new tag is selected automatically. Return the
!! tag of the surface

INTERFACE
  MODULE FUNCTION obj_AddPlaneSurface(wireTags, tag) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: wireTags(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
    INTEGER(I4B) :: ans
  END FUNCTION obj_AddPlaneSurface
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Add a surface in the built-in CAD representation, filling the curve loops
!! in `wireTags' using transfinite interpolation. Currently only a single
!! curve loop is supported; this curve loop should be composed by 3 or 4
!! curves only. If `tag' is positive, set the tag explicitly; otherwise a new
!! tag is selected automatically. Return the tag of the surface.

INTERFACE
  MODULE FUNCTION obj_AddSurfaceFilling( &
    wireTags, tag, sphereCenterTag) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: wireTags(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
    INTEGER(I4B), INTENT(IN) :: sphereCenterTag
    INTEGER(I4B) :: ans
  END FUNCTION obj_AddSurfaceFilling
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Add a surface loop (a closed shell) formed by `surfaceTags' in the built-in
!! CAD representation.  If `tag' is positive, set the tag explicitly;
!! otherwise a new tag is selected automatically. Return the tag of the shell.

INTERFACE
  MODULE FUNCTION obj_AddSurfaceLoop(surfaceTags, tag) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: surfaceTags(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
    INTEGER(I4B) :: ans
  END FUNCTION obj_AddsurfaceLoop
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Add a volume (a region) in the built-in CAD representation, defined by one
!! or more shells `shellTags'. The first surface loop defines the exterior
!! boundary; additional surface loop define holes. If `tag' is positive, set
!! the tag explicitly; otherwise a new tag is selected automatically. Return
!! the tag of the volume.

INTERFACE
  MODULE FUNCTION obj_AddVolume(shellTags, tag) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: shellTags(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
    INTEGER(I4B) :: ans
  END FUNCTION obj_AddVolume
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Add a `geometry' in the built-in CAD representation. `geometry' can
!! currently be one of "Sphere" or "PolarSphere" (where `numbers' should
!! contain the x, y, z coordinates of the center, followed by the radius), or
!! "Parametric" (where `strings' should contains three expression evaluating
!! to the x, y and z coordinates. If `tag' is positive, set the tag of the
!! geometry explicitly; otherwise a new tag is selected automatically. Return
!! the tag of the geometry.

INTERFACE
  MODULE FUNCTION obj_AddGeometry( &
    geometry, numbers, strings, tag) RESULT(ans)
    CHARACTER(*), INTENT(in) :: geometry
    CLASS(*), INTENT(in), OPTIONAL :: numbers(:)
    CHARACTER(*), INTENT(in), OPTIONAL :: strings(:)
    INTEGER(i4b), INTENT(in), OPTIONAL :: tag
    INTEGER(i4b) :: ans
  END FUNCTION obj_AddGeometry
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Add a point in the built-in CAD representation, at coordinates (`x', `y',
!! `z') on the geometry `geometryTag'. If `meshSize' is > 0, add a meshing
!! constraint at that point. If `tag' is positive, set the tag explicitly;
!! otherwise a new tag is selected automatically. Return the tag of the point.
!! For surface geometries, only the `x' and `y' coordinates are used.

INTERFACE
  MODULE FUNCTION obj_AddPointOnGeometry( &
    geometryTag, x, y, z, meshSize, tag) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: geometryTag
    CLASS(*), INTENT(IN) :: x
    CLASS(*), INTENT(IN) :: y
    CLASS(*), INTENT(IN), OPTIONAL :: z
    CLASS(*), INTENT(IN), OPTIONAL :: meshSize
    INTEGER(I4B), INTENT(IN), OPTIONAL :: tag
    INTEGER(I4B) :: ans
  END FUNCTION obj_AddPointOnGeometry
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Extrude the entities `dimTags' (given as a vector of (dim, tag) pairs) in
!! the built-in CAD representation, using a translation along (`dx', `dy',
!! `dz'). Return extruded entities in `outDimTags'. If the `numElements'
!! vector is not empty, also extrude the mesh: the entries in `numElements'
!! give the number of elements in each layer. If the `height' vector is not
!! empty, it provides the (cumulative) height of the different layers,
!! normalized to 1. If `recombine' is set, recombine the mesh in the layers.

INTERFACE
  MODULE FUNCTION obj_Extrude( &
    dimTags, dx, dy, dz, numElements, heights, recombine) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dimTags(:, :)
    CLASS(*), INTENT(IN) :: dx, dy, dz
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: numElements(:)
    !! make it optional
    CLASS(*), OPTIONAL, INTENT(IN) :: heights(:)
    !! make it optional
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: recombine
    !! make it optional
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
    !! outDimTags is stored in ans
  END FUNCTION obj_Extrude
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Extrude the entities `dimTags' (given as a vector of (dim, tag) pairs) in
!! the built-in CAD representation, using a rotation of `angle' radians around
!! the axis of revolution defined by the point (`x', `y', `z') and the
!! direction (`ax', `ay', `az'). The angle should be strictly smaller than Pi.
!! Return extruded entities in `outDimTags'. If the `numElements' vector is
!! not empty, also extrude the mesh: the entries in `numElements' give the
!! number of elements in each layer. If the `height' vector is not empty, it
!! provides the (cumulative) height of the different layers, normalized to 1.
!! If `recombine' is set, recombine the mesh in the layers.

INTERFACE
  MODULE FUNCTION obj_Revolve( &
    dimTags, x, y, z, ax, ay, az, angle, numElements, heights, &
    recombine) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dimTags(:, :)
    CLASS(*), INTENT(IN) :: x, y, z, ax, ay, az, angle
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: numElements(:)
    CLASS(*), OPTIONAL, INTENT(IN) :: heights(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: recombine
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
    !! outDimTags are stored in ans
  END FUNCTION obj_Revolve
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Extrude the entities `dimTags' (given as a vector of (dim, tag) pairs) in
!! the built-in CAD representation, using a combined translation and rotation
!! of `angle' radians, along (`dx', `dy', `dz') and around the axis of
!! revolution defined by the point (`x', `y', `z') and the direction (`ax',
!! `ay', `az'). The angle should be strictly smaller than Pi. Return extruded
!! entities in `outDimTags'. If the `numElements' vector is not empty, also
!! extrude the mesh: the entries in `numElements' give the number of elements
!! in each layer. If the `height' vector is not empty, it provides the
!! (cumulative) height of the different layers, normalized to 1. If
!! `recombine' is set, recombine the mesh in the layers.

INTERFACE
  MODULE FUNCTION obj_Twist( &
    dimTags, x, y, z, dx, dy, dz, ax, ay, az, angle, numElements, &
    heights, recombine) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dimTags(:, :)
    CLASS(*), INTENT(IN) :: x, y, z, dx, dy, dz, ax, ay, az, angle
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: numElements(:)
    CLASS(*), OPTIONAL, INTENT(IN) :: heights(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: recombine
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
    !! outDimTags
  END FUNCTION obj_Twist
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Extrude the entities `dimTags' (given as a vector of (dim, tag) pairs) in
!! the built-in CAD representation along the normals of the mesh, creating
!! discrete boundary layer entities. Return extruded entities in `outDimTags'.
!! The entries in `numElements' give the number of elements in each layer. If
!! the `height' vector is not empty, it provides the (cumulative) height of
!! the different layers. If `recombine' is set, recombine the mesh in the
!! layers. A second boundary layer can be created from the same entities if
!! `second' is set. If `viewIndex' is >= 0, use the corresponding view to
!! either specify the normals (if the view contains a vector field) or scale
!! the normals (if the view is scalar).

INTERFACE
  MODULE FUNCTION obj_ExtrudeBoundaryLayer( &
    dimTags, numElements, heights, recombine, second, viewIndex) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dimTags(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: numElements(:)
    CLASS(*), OPTIONAL, INTENT(IN) :: heights(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: recombine, second, viewIndex
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
  END FUNCTION obj_ExtrudeBoundaryLayer
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Translate the entities `dimTags' (given as a vector of (dim, tag) pairs) in
!! the built-in CAD representation along (`dx', `dy', `dz').

INTERFACE
  MODULE FUNCTION obj_Translate(dimTags, dx, dy, dz) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dimTags(:, :)
    CLASS(*), INTENT(IN) :: dx, dy, dz
    INTEGER(I4B) :: ans
  END FUNCTION obj_Translate
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Rotate the entities `dimTags' (given as a vector of (dim, tag) pairs) in
!! the built-in CAD representation by `angle' radians around the axis of
!! revolution defined by the point (`x', `y', `z') and the direction (`ax',
!! `ay', `az').

INTERFACE
  MODULE FUNCTION obj_Rotate( &
    dimTags, x, y, z, ax, ay, az, angle) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dimTags(:, :)
    CLASS(*), INTENT(IN) :: x, y, z, ax, ay, az, angle
    INTEGER(I4B) :: ans
  END FUNCTION obj_Rotate
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Scale the entities `dimTags' (given as a vector of (dim, tag) pairs) in the
!! built-in CAD representation by factors `a', `b' and `c' along the three
!! coordinate axes; use (`x', `y', `z') as the center of the homothetic
!! transformation.

INTERFACE
  MODULE FUNCTION obj_Dilate(dimTags, x, y, z, a, b, c) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dimTags(:, :)
    CLASS(*), INTENT(IN) :: x, y, z, a, b, c
    INTEGER(I4B) :: ans
  END FUNCTION obj_Dilate
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Mirror the entities `dimTags' (given as a vector of (dim, tag) pairs) in
!! the built-in CAD representation, with respect to the plane of equation `a'
!! * x + `b' * y + `c' * z + `d' = 0.

INTERFACE
  MODULE FUNCTION obj_Mirror(dimTags, a, b, c, d) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dimTags(:, :)
    CLASS(*), INTENT(IN) :: a, b, c, d
    INTEGER(I4B) :: ans
  END FUNCTION obj_Mirror
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Mirror the entities `dimTags' (given as a vector of (dim, tag) pairs) in
!! the built-in CAD representation, with respect to the plane of equation `a'
!! * x + `b' * y + `c' * z + `d' = 0. (This is a synonym for `mirror', which
!! will be deprecated in a future release.)

INTERFACE
  MODULE FUNCTION obj_Symmetrize(dimTags, a, b, c, d) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dimTags(:, :)
    CLASS(*), INTENT(IN) :: a, b, c, d
    INTEGER(I4B) :: ans
  END FUNCTION obj_Symmetrize
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Copy the entities `dimTags' (given as a vector of (dim, tag) pairs) in the
!! built-in CAD representation; the new entities are returned in `outDimTags'.

INTERFACE
  MODULE FUNCTION obj_Copy(dimTags) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dimTags(:, :)
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
    !! outDimTags
  END FUNCTION obj_Copy
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Remove the entities `dimTags' (given as a vector of (dim, tag) pairs) in
!! the built-in CAD representation, provided that they are not on the boundary
!! of higher-dimensional entities. If `recursive' is true, remove all the
!! entities on their boundaries, down to dimension 0.

INTERFACE
  MODULE FUNCTION obj_Remove(dimTags, RECURSIVE) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dimTags(:, :)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: RECURSIVE
    INTEGER(I4B) :: ans
  END FUNCTION obj_Remove
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_RemoveAllDuplicates() RESULT(ans)
    INTEGER(I4B) :: ans
  END FUNCTION obj_RemoveAllDuplicates
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Split the curve of tag `tag' in the built-in CAD representation, on the
!! specified control points `pointTags'. This feature is only available for
!! lines, splines and b-splines. Return the tag(s) `curveTags' of the newly
!! created curve(s).

INTERFACE
  MODULE FUNCTION obj_SplitCurve(tag, pointTags) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: tag
    INTEGER(I4B), INTENT(IN) :: pointTags(:)
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !! curveTags
  END FUNCTION obj_SplitCurve
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Get the maximum tag of entities of dimension `dim' in the built-in CAD
!! representation.

INTERFACE
  MODULE FUNCTION obj_GetMaxTag(dim) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetMaxTag
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Set the maximum tag `maxTag' for entities of dimension `dim' in the built-
!! in CAD representation.

INTERFACE
  MODULE FUNCTION obj_SetMaxTag(dim, maxTag) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim, maxTag
    INTEGER(I4B) :: ans
  END FUNCTION obj_SetMaxTag
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Add a physical group of dimension `dim', grouping the entities with tags
!! `tags' in the built-in CAD representation. Return the tag of the physical
!! group, equal to `tag' if `tag' is positive, or a new tag if `tag' < 0. Set
!! the name of the physical group if `name' is not empty.

INTERFACE
  MODULE FUNCTION obj_AddPhysicalGroup(dim, tags, tag, name) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), INTENT(IN) :: tags(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
    CHARACTER(*), OPTIONAL, INTENT(IN) :: name
    INTEGER(I4B) :: ans
  END FUNCTION obj_AddPhysicalGroup
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Remove the physical groups `dimTags' (given as a vector of (dim, tag)
!! pairs) from the built-in CAD representation. If `dimTags' is empty, remove
!! all groups.

INTERFACE
  MODULE FUNCTION obj_RemovePhysicalGroups(dimTags) RESULT(ans)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dimTags(:, :)
    INTEGER(I4B) :: ans
  END FUNCTION obj_RemovePhysicalGroups
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Synchronize the built-in CAD representation with the current Gmsh model.
!! This can be called at any time, but since it involves a non trivial amount
!! of processing, the number of synchronization points should normally be
!! minimized. Without synchronization the entities in the built-in CAD
!! representation are not available to any function outside of the built-in
!! CAD kernel functions.

INTERFACE
  MODULE FUNCTION obj_Synchronize() RESULT(ans)
    INTEGER(I4B) :: ans
  END FUNCTION obj_Synchronize
END INTERFACE

END MODULE GmshModelGeo_Class
