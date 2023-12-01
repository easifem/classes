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
USE Utility, ONLY: Reallocate, Input
USE GmshUtility
USE GmshInterface
USE GmshModelGeoMesh_Class
USE ExceptionHandler_Class, ONLY: e
USE CInterface
USE ISO_C_BINDING
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "GMSHOPTION_CLASS"
INTEGER(C_INT) :: ierr
!$OMP THREADPRIVATE(ierr)
INTEGER(C_INT) :: cintvar
!$OMP THREADPRIVATE(cintvar)
!!
INTEGER(I4B), PARAMETER :: maxStrLen = GMSH_API_MAX_STR_LEN

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: GmshModelGeo_
  TYPE(GmshModelGeoMesh_), POINTER :: mesh => NULL()
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => geo_Initiate
  PROCEDURE, PUBLIC, NOPASS :: AddPoint => geo_AddPoint
  PROCEDURE, PUBLIC, NOPASS :: AddLine => geo_AddLine
  PROCEDURE, PUBLIC, NOPASS :: AddCircleArc => geo_AddCircleArc
  PROCEDURE, PUBLIC, NOPASS :: AddEllipseArc => geo_AddEllipseArc
  PROCEDURE, PUBLIC, NOPASS :: AddSpline => geo_AddSpline
  PROCEDURE, PUBLIC, NOPASS :: AddBSpline => geo_AddBSpline
  PROCEDURE, PUBLIC, NOPASS :: AddBezier => geo_AddBezier
  PROCEDURE, PUBLIC, NOPASS :: AddPolyline => geo_AddPolyline
  PROCEDURE, PUBLIC, NOPASS :: AddCompoundSpline => geo_AddCompoundSpline
  PROCEDURE, PUBLIC, NOPASS :: AddCompoundBSpline => &
    & geo_AddCompoundBSpline
  PROCEDURE, PUBLIC, NOPASS :: AddCurveLoop => geo_AddCurveLoop
  PROCEDURE, PUBLIC, NOPASS :: AddCurveLoops => geo_AddCurveLoops
  PROCEDURE, PUBLIC, NOPASS :: AddPlaneSurface => geo_AddPlaneSurface
  PROCEDURE, PUBLIC, NOPASS :: AddSurfaceFilling => geo_AddSurfaceFilling
  PROCEDURE, PUBLIC, NOPASS :: AddSurfaceLoop => geo_AddSurfaceLoop
  PROCEDURE, PUBLIC, NOPASS :: AddVolume => geo_AddVolume
  PROCEDURE, PUBLIC, NOPASS :: AddGeometry => geo_AddGeometry
  PROCEDURE, PUBLIC, NOPASS :: AddPointOnGeometry => geo_AddPointOnGeometry
  PROCEDURE, PUBLIC, NOPASS :: Extrude => geo_Extrude
  PROCEDURE, PUBLIC, NOPASS :: Revolve => geo_GeoRevolve
  PROCEDURE, PUBLIC, NOPASS :: Twist => geo_Twist
  PROCEDURE, PUBLIC, NOPASS :: ExtrudeBoundaryLayer => &
    & geo_ExtrudeBoundaryLayer
  PROCEDURE, PUBLIC, NOPASS :: GeoTranslate => geo_GeoTranslate
  PROCEDURE, PUBLIC, NOPASS :: GeoRotate => geo_GeoRotate
  PROCEDURE, PUBLIC, NOPASS :: GeoDilate => geo_GeoDilate
  PROCEDURE, PUBLIC, NOPASS :: Mirror => geo_Mirror
  PROCEDURE, PUBLIC, NOPASS :: Symmetrize => geo_Symmetrize
  PROCEDURE, PUBLIC, NOPASS :: Copy => geo_Copy
  PROCEDURE, PUBLIC, NOPASS :: Remove => geo_Remove
  PROCEDURE, PUBLIC, NOPASS :: RemoveAllDuplicates => &
    & geo_RemoveAllDuplicates
  PROCEDURE, PUBLIC, NOPASS :: SplitCurve => geo_SplitCurve
  PROCEDURE, PUBLIC, NOPASS :: GetMaxTag => geo_GetMaxTag
  PROCEDURE, PUBLIC, NOPASS :: SetMaxTag => geo_SetMaxTag
  PROCEDURE, PUBLIC, NOPASS :: AddPhysicalGroup => geo_AddPhysicalGroup
  PROCEDURE, PUBLIC, NOPASS :: RemovePhysicalGroups => &
    & geo_RemovePhysicalGroups
  PROCEDURE, PUBLIC, NOPASS :: Synchronize => geo_Synchronize
END TYPE GmshModelGeo_

PUBLIC :: GmshModelGeo_
TYPE(GmshModelGeo_), PUBLIC, PARAMETER :: TypeGmshModelGeo = GmshModelGeo_()

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: GmshModelGeoPointer_
  CLASS(GmshModelGeo_), POINTER :: Ptr => NULL()
END TYPE

PUBLIC :: GmshModelGeoPointer_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE geo_Initiate(obj)
  CLASS(GmshModelGeo_), INTENT(INOUT) :: obj
  !> internal var
  CHARACTER(*), PARAMETER :: myName = "geo_Initiate"
  !> main program
  IF (ASSOCIATED(obj%Mesh)) THEN
    CALL e%raiseError(modName//"::"//myName//" - "// &
      & "gmsh::Model::Geo::Mesh is already associated;")
  END IF
  ALLOCATE (obj%Mesh)
END SUBROUTINE geo_Initiate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Add a geometrical point in the built-in CAD representation, at coordinates
!! (`x', `y', `z'). If `meshSize' is > 0, add a meshing constraint at that
!! point. If `tag' is positive, set the tag explicitly; otherwise a new tag is
!! selected automatically. Return the tag of the point. (Note that the point
!! will be added in the current model only after `synchronize' is called. This
!! behavior holds for all the entities added in the geo module.)

FUNCTION geo_AddPoint(x, y, z, meshSize, tag) RESULT(ans)
  CLASS(*), INTENT(IN) :: x, y, z, meshSize
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
  INTEGER(I4B) :: ans
  ! Internal variable
  cintvar = gmshModelGeoAddPoint( &
    & x=gmsh_cdouble(x), &
    & y=gmsh_cdouble(y), &
    & z=gmsh_cdouble(z), &
    & meshSize=gmsh_cdouble(meshSize), &
    & tag=gmsh_cint(INPUT(default=-1_I4B, option=tag)), &
    & ierr=ierr)
  !
  ans = INT(cintvar, i4b)
  !
END FUNCTION geo_AddPoint

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Add a straight line segment in the built-in CAD representation, between the
!! two points with tags `startTag' and `endTag'. If `tag' is positive, set the
!! tag explicitly; otherwise a new tag is selected automatically. Return the
!! tag of the line.

FUNCTION geo_AddLine(startTag, endTag, tag) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: startTag, endTag
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
  INTEGER(I4B) :: ans
  !!
  cintvar = gmshModelGeoAddLine(&
    & startTag=gmsh_cint(startTag), &
    & endTag=gmsh_cint(endTag), &
    & tag=gmsh_cint(INPUT(default=-1, option=tag)), &
    & ierr=ierr)
  !
  ans = INT(cintvar, i4b)
END FUNCTION geo_AddLine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Add a circle arc (strictly smaller than Pi) in the built-in CAD
!! representation, between the two points with tags `startTag' and `endTag',
!! and with center `centerTag'. If `tag' is positive, set the tag explicitly;
!! otherwise a new tag is selected automatically. If (`nx', `ny', `nz') != (0,
!! 0, 0), explicitly set the plane of the circle arc. Return the tag of the
!! circle arc.

FUNCTION geo_AddCircleArc(startTag, centerTag, endTag, tag, nx, ny, &
  & nz) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: startTag, endTag, centerTag
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
  CLASS(*), OPTIONAL, INTENT(IN) :: nx, ny, nz
  INTEGER(I4B) :: ans
  !!
  cintvar = gmshModelGeoAddCircleArc( &
    & startTag=gmsh_cint(startTag), &
    & centerTag=gmsh_cint(centerTag), &
    & endTag=gmsh_cint(endTag), &
    & tag=gmsh_opt_cint(default=-1, option=tag), &
    & nx=gmsh_opt_cdouble(0.0_DFP, nx), &
    & ny=gmsh_opt_cdouble(0.0_DFP, ny), &
    & nz=gmsh_opt_cdouble(0.0_DFP, nz), &
    & ierr=ierr)
  !!
  ans = INT(cintvar, i4b)
  !!
END FUNCTION geo_AddCircleArc

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Add an ellipse arc (strictly smaller than Pi) in the built-in CAD
!! representation, between the two points `startTag' and `endTag', and with
!! center `centerTag' and major axis point `majorTag'. If `tag' is positive,
!! set the tag explicitly; otherwise a new tag is selected automatically. If
!! (`nx', `ny', `nz') != (0, 0, 0), explicitly set the plane of the circle
!! arc. Return the tag of the ellipse arc.

FUNCTION geo_AddEllipseArc(startTag, centerTag, majorTag, endTag, &
  & tag, nx, ny, nz) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: startTag, centerTag, majorTag, endTag
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
  CLASS(*), INTENT(IN) :: nx, ny, nz
  INTEGER(I4B) :: ans
  !!
  cintvar = gmshModelGeoAddEllipseArc( &
    & startTag=gmsh_cint(startTag), &
    & centerTag=gmsh_cint(centerTag), &
    & majorTag=gmsh_cint(majorTag), &
    & endTag=gmsh_cint(endTag), &
    & tag=gmsh_cint(input(default=-1, option=tag)), &
    & nx=gmsh_cdouble(nx), &
    & ny=gmsh_cdouble(ny), &
    & nz=gmsh_cdouble(nz), &
    & ierr=ierr)
  !!
  ans = INT(cintvar, i4b)
  !!
END FUNCTION geo_AddEllipseArc

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Add a spline (Catmull-Rom) curve in the built-in CAD representation, going
!! through the points `pointTags'. If `tag' is positive, set the tag
!! explicitly; otherwise a new tag is selected automatically. Create a
!! periodic curve if the first and last points are the same. Return the tag of
!! the spline curve.

FUNCTION geo_AddSpline(pointTags, tag) &
  & RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: pointTags(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
  INTEGER(I4B) :: ans
  ! internal
  cintvar = gmshModelGeoAddSpline( &
    & pointTags=gmsh_cint(pointTags),  &
    & pointTags_n=INT(SIZE(pointTags), KIND=C_SIZE_T),  &
    & tag=gmsh_cint(INPUT(default=-1, option=tag)), &
    & ierr=ierr)
  !!
  ans = INT(cintvar, i4b)
END FUNCTION geo_AddSpline

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Add a cubic b-spline curve in the built-in CAD representation, with
!! `pointTags' control points. If `tag' is positive, set the tag explicitly;
!! otherwise a new tag is selected automatically. Creates a periodic curve if
!! the first and last points are the same. Return the tag of the b-spline
!! curve.

FUNCTION geo_AddBSpline(pointTags, tag) &
  & RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: pointTags(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
  INTEGER(I4B) :: ans
  !!
  cintvar = gmshModelGeoAddBSpline( &
    & pointTags=gmsh_cint(pointTags), &
    & pointTags_n=INT(SIZE(pointTags), KIND=C_SIZE_T), &
    & tag=gmsh_cint(INPUT(default=-1, option=tag)), &
    & ierr=ierr)
  !!
  ans = INT(cintvar, i4b)
  !!
END FUNCTION geo_AddBSpline

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Add a Bezier curve in the built-in CAD representation, with `pointTags'
!! control points. If `tag' is positive, set the tag explicitly; otherwise a
!! new tag is selected automatically.  Return the tag of the Bezier curve.

FUNCTION geo_AddBezier(pointTags, tag) &
  & RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: pointTags(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
  INTEGER(I4B) :: ans
  !!
  cintvar = gmshModelGeoAddBezier( &
    & pointTags=gmsh_cint(pointTags), &
    & pointTags_n=INT(SIZE(pointTags), KIND=C_SIZE_T), &
    & tag=gmsh_cint(INPUT(default=-1, option=tag)), &
    & ierr=ierr)
  !!
  ans = INT(cintvar, i4b)
  !!
END FUNCTION geo_AddBezier

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Add a polyline curve in the built-in CAD representation, going through the
!! points `pointTags'. If `tag' is positive, set the tag explicitly; otherwise
!! a new tag is selected automatically. Create a periodic curve if the first
!! and last points are the same. Return the tag of the polyline curve.

FUNCTION geo_AddPolyline(pointTags, tag) &
  & RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: pointTags(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
  INTEGER(I4B) :: ans
  !!
  cintvar = gmshModelGeoAddPolyline( &
    & pointTags=gmsh_cint(pointTags), &
    & pointTags_n=INT(SIZE(pointTags), KIND=C_SIZE_T), &
    & tag=gmsh_cint(INPUT(default=-1, option=tag)), &
    & ierr=ierr)
  !!
  ans = INT(cintvar, i4b)
END FUNCTION geo_AddPolyline

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Add a spline (Catmull-Rom) curve in the built-in CAD representation, going
!! through points sampling the curves in `curveTags'. The density of sampling
!! points on each curve is governed by `numIntervals'. If `tag' is positive,
!! set the tag explicitly; otherwise a new tag is selected automatically.
!! Return the tag of the spline.

FUNCTION geo_AddCompoundSpline(curveTags, &
  & numIntervals, tag) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: curveTags(:)
  INTEGER(I4B), INTENT(IN) :: numIntervals
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
  INTEGER(I4B) :: ans
  !!
  cintvar = gmshModelGeoAddCompoundSpline( &
    & curveTags=gmsh_cint(curveTags), &
    & curveTags_n=INT(SIZE(curveTags), KIND=C_SIZE_T), &
    & numIntervals=gmsh_cint(numIntervals), &
    & tag=gmsh_cint(INPUT(default=-1, option=tag)), &
    & ierr=ierr)
  ans = INT(cintvar, i4b)
END FUNCTION geo_AddCompoundSpline

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Add a b-spline curve in the built-in CAD representation, with control
!! points sampling the curves in `curveTags'. The density of sampling points
!! on each curve is governed by `numIntervals'. If `tag' is positive, set the
!! tag explicitly; otherwise a new tag is selected automatically. Return the
!! tag of the b-spline.

FUNCTION geo_AddCompoundBSpline(curveTags, &
  & numIntervals, tag) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: curveTags(:)
  INTEGER(I4B), INTENT(IN) :: numIntervals
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
  INTEGER(I4B) :: ans
  !!
  cintvar = gmshModelGeoAddCompoundBSpline( &
    & curveTags=gmsh_cint(curveTags), &
    & curveTags_n=INT(SIZE(curveTags), KIND=C_SIZE_T), &
    & numIntervals=gmsh_cint(numIntervals), &
    & tag=gmsh_cint(INPUT(default=-1, option=tag)), &
    & ierr=ierr)
  !!
  ans = INT(cintvar, i4b)
END FUNCTION geo_AddCompoundBSpline

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Add a curve loop (a closed wire) in the built-in CAD representation, formed
!! by the curves `curveTags'. `curveTags' should contain (signed) tags of
!! model entities of dimension 1 forming a closed loop: a negative tag
!! signifies that the underlying curve is considered with reversed
!! orientation. If `tag' is positive, set the tag explicitly; otherwise a new
!! tag is selected automatically. If `reorient' is set, automatically reorient
!! the curves if necessary. Return the tag of the curve loop.

FUNCTION geo_AddCurveLoop(curveTags, tag, reorient) &
  & RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: curveTags(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
  LOGICAL(LGT), OPTIONAL, INTENT(IN) :: reorient
  INTEGER(I4B) :: ans
  !
  cintvar = gmshModelGeoAddCurveLoop( &
    & curveTags=gmsh_cint(curveTags), &
    & curveTags_n=INT(SIZE(curveTags), C_SIZE_T),  &
    & tag=gmsh_cint(INPUT(default=-1, option=tag)), &
    & reorient=optval_c_bool(.FALSE., reorient), &
    & ierr=ierr)
  !!
  ans = INT(cintvar, i4b)
END FUNCTION geo_AddCurveLoop

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Add curve loops in the built-in CAD representation based on the curves
!! `curveTags'. Return the `tags' of found curve loops, if any.

FUNCTION geo_AddCurveLoops(curveTags, tags) &
  & RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: curveTags(:)
  INTEGER(I4B), ALLOCATABLE, INTENT(OUT) :: tags(:)
  INTEGER(I4B) :: ans
  !
  TYPE(C_PTR) :: cptr
  INTEGER(C_SIZE_T) :: tags_n
  !
  CALL gmshModelGeoAddCurveLoops( &
    & curveTags=gmsh_cint(curveTags), &
    & curveTags_n=INT(SIZE(curveTags), C_SIZE_T), &
    & tags=cptr, &
    & tags_n=tags_n, &
    & ierr=ierr)
  !
  ans = INT(ierr, I4B)
  !
  tags = gmsh_intvec_c2f(cptr, tags_n)
  !
END FUNCTION geo_AddCurveLoops

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Add a plane surface in the built-in CAD representation, defined by one or
!! more curve loops `wireTags'. The first curve loop defines the exterior
!! contour; additional curve loop define holes. If `tag' is positive, set the
!! tag explicitly; otherwise a new tag is selected automatically. Return the
!! tag of the surface

FUNCTION geo_AddPlaneSurface(wireTags, tag) &
  & RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: wireTags(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
  INTEGER(I4B) :: ans
  !!
  cintvar = gmshModelGeoAddPlaneSurface( &
    & wireTags=gmsh_cint(wireTags), &
    & wireTags_n=INT(SIZE(wireTags), C_SIZE_T), &
    & tag=gmsh_cint(input(default=-1, option=tag)), &
    & ierr=ierr)
  !!
  ans = INT(cintvar, i4b)
END FUNCTION geo_AddPlaneSurface

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Add a surface in the built-in CAD representation, filling the curve loops
!! in `wireTags' using transfinite interpolation. Currently only a single
!! curve loop is supported; this curve loop should be composed by 3 or 4
!! curves only. If `tag' is positive, set the tag explicitly; otherwise a new
!! tag is selected automatically. Return the tag of the surface.

FUNCTION geo_AddSurfaceFilling(wireTags, tag, &
  & sphereCenterTag) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: wireTags(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
  INTEGER(I4B), INTENT(IN) :: sphereCenterTag
  INTEGER(I4B) :: ans
  !!
  cintvar = gmshModelGeoAddSurfaceFilling( &
    & wireTags=gmsh_cint(wireTags),  &
    & wireTags_n=INT(SIZE(wireTags), C_SIZE_T), &
    & tag=gmsh_cint(input(default=-1, option=tag)), &
    & sphereCenterTag=gmsh_cint(sphereCenterTag), &
    & ierr=ierr)
  !!
  ans = INT(cintvar, i4b)
  !!
END FUNCTION geo_AddSurfaceFilling

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Add a surface loop (a closed shell) formed by `surfaceTags' in the built-in
!! CAD representation.  If `tag' is positive, set the tag explicitly;
!! otherwise a new tag is selected automatically. Return the tag of the shell.

FUNCTION geo_AddSurfaceLoop(surfaceTags, tag) &
  & RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: surfaceTags(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
  INTEGER(I4B) :: ans
  
  cintvar = gmshModelGeoAddSurfaceLoop( &
    & surfaceTags=gmsh_cint(surfaceTags),  &
    & surfaceTags_n=INT(SIZE(surfaceTags), C_SIZE_T), &
    & tag=gmsh_cint(input(default=-1, option=tag)), &
    & ierr=ierr)
 
  ans = INT(cintvar, i4b)
END FUNCTION geo_AddsurfaceLoop

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Add a volume (a region) in the built-in CAD representation, defined by one
!! or more shells `shellTags'. The first surface loop defines the exterior
!! boundary; additional surface loop define holes. If `tag' is positive, set
!! the tag explicitly; otherwise a new tag is selected automatically. Return
!! the tag of the volume.

FUNCTION geo_AddVolume(shellTags, tag) &
  & RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: shellTags(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
  INTEGER(I4B) :: ans
  !!
  cintvar = gmshModelGeoAddVolume( &
    & shellTags=gmsh_cint(shellTags),  &
    & shellTags_n=INT(SIZE(shellTags), C_SIZE_T), &
    & tag=gmsh_cint(input(default=-1, option=tag)), &
    & ierr=ierr)
  !!
  ans = INT(cintvar, i4b)
  !!
END FUNCTION geo_AddVolume

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

FUNCTION geo_AddGeometry(geometry, numbers, &
  & strings, tag) RESULT(ans)
  !!
  CHARACTER(*), INTENT(in) :: geometry
  CLASS(*), INTENT(in), OPTIONAL :: numbers(:)
  CHARACTER(*), INTENT(in), OPTIONAL :: strings(:)
  INTEGER(i4b), INTENT(in), OPTIONAL :: tag
  INTEGER(i4b) :: ans
  !!
  CHARACTER(maxStrLen, kind=C_CHAR), ALLOCATABLE :: strings_strs(:)
  TYPE(C_PTR), ALLOCATABLE :: strings_(:)
  REAL(C_DOUBLE), ALLOCATABLE :: numbers0(:)
  !!
  CALL gmsh_GetCharArray_cPtr(strings, strings_strs, strings_)
  !
  IF (PRESENT(numbers)) THEN
    numbers0 = gmsh_cdouble(numbers)
  ELSE
    ALLOCATE (numbers0(0))
  END IF
  !
  cintvar = gmshModelGeoAddGeometry( &
    & geometry=gmsh_CString(geometry), &
    & numbers=numbers0, &
    & numbers_n=SIZE(numbers0, kind=C_SIZE_T), &
    & strings=strings_, &
    & strings_n=gmsh_size_str(strings), &
    & tag=gmsh_cint(input(default=-1, option=tag)), &
    & ierr=ierr)
  !
  ans = INT(cintvar, i4b)
  !
END FUNCTION geo_AddGeometry

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Add a point in the built-in CAD representation, at coordinates (`x', `y',
!! `z') on the geometry `geometryTag'. If `meshSize' is > 0, add a meshing
!! constraint at that point. If `tag' is positive, set the tag explicitly;
!! otherwise a new tag is selected automatically. Return the tag of the point.
!! For surface geometries, only the `x' and `y' coordinates are used.

FUNCTION geo_AddPointOnGeometry(geometryTag, x, y, z, meshSize, &
  & tag) RESULT(ans)
  INTEGER(i4b), INTENT(in) :: geometryTag
  CLASS(*), INTENT(in) :: x
  CLASS(*), INTENT(in) :: y
  CLASS(*), INTENT(in), OPTIONAL :: z
  CLASS(*), INTENT(in), OPTIONAL :: meshSize
  INTEGER(i4b), INTENT(in), OPTIONAL :: tag
  INTEGER(i4b) :: ans
  !!
  cintvar = gmshModelGeoAddPointOnGeometry( &
    & geometryTag=gmsh_cint(geometryTag), &
    & x=gmsh_cdouble(x), &
    & y=gmsh_cdouble(y), &
    & z=gmsh_opt_cdouble(default=0.0_DFP, option=z), &
    & meshSize=gmsh_opt_cdouble(default=0.0_DFP, option=meshSize), &
    & tag=gmsh_opt_cint(default=-1_I4B, option=tag), &
    & ierr=ierr)
  !!
  ans = INT(cintvar, i4b)
  !!
END FUNCTION geo_AddPointOnGeometry

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

FUNCTION geo_Extrude(dimTags, dx, dy, dz, &
  & numElements, heights, recombine) RESULT(outDimTags)
  INTEGER(I4B), INTENT(IN) :: dimTags(:, :)
  CLASS(*), INTENT(IN) :: dx, dy, dz
  INTEGER(I4B), INTENT(IN) :: numElements(:)
  CLASS(*), INTENT(IN) :: heights(:)
  LOGICAL(LGT), OPTIONAL, INTENT(IN) :: recombine
  INTEGER(I4B), ALLOCATABLE :: outDimTags(:, :)
  !!
  INTEGER(C_SIZE_T) :: outDimTags_n
  TYPE(C_PTR) :: cptr
  !!
  CALL gmshModelGeoExtrude( &
    & dimTags=gmsh_cint(dimTags), &
    & dimTags_n=INT(SIZE(dimTags), C_SIZE_T), &
    & dx=gmsh_cdouble(dx), &
    & dy=gmsh_cdouble(dy), &
    & dz=gmsh_cdouble(dz), &
    & outDimTags=cptr, &
    & outDimTags_n=outDimTags_n, &
    & numElements=gmsh_cint(numElements), &
    & numElements_n=SIZE(numElements, KIND=C_SIZE_T), &
    & heights=gmsh_cdouble(heights), &
    & heights_n=SIZE(heights, KIND=C_SIZE_T), &
    & recombine=optval_c_bool(.FALSE., recombine), &
    & ierr=ierr)
  !!
  outDimTags = gmsh_dimtag_c2f(cptr, outDimTags_n)
  !!
END FUNCTION geo_Extrude

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

FUNCTION geo_GeoRevolve(dimTags, x, y, z, ax, ay, az, &
  & angle, numElements, heights, recombine) &
  & RESULT(outDimTags)
  INTEGER(I4B), INTENT(IN) :: dimTags(:, :)
  CLASS(*), INTENT(IN) :: x, y, z, ax, ay, az, angle
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: numElements(:)
  CLASS(*), OPTIONAL, INTENT(IN) :: heights(:)
  LOGICAL(LGT), OPTIONAL, INTENT(IN) :: recombine
  INTEGER(I4B), ALLOCATABLE :: outDimTags(:, :)
  !!
  INTEGER(C_SIZE_T) :: outDimTags_n
  TYPE(C_PTR) :: cptr
  REAL(C_DOUBLE), ALLOCATABLE :: heights0(:)
  INTEGER(C_INT), ALLOCATABLE :: numElements0(:)
  !!
  IF (PRESENT(numElements)) THEN
    numElements0 = gmsh_cint(numElements)
  ELSE
    ALLOCATE (numElements0(0))
  END IF
  !!
  IF (PRESENT(heights)) THEN
    heights0 = gmsh_cdouble(heights)
  ELSE
    ALLOCATE (heights0(0))
  END IF
  !!
  CALL gmshModelGeoRevolve( &
    & dimTags=dimTags, &
    & dimTags_n=SIZE(dimTags, KIND=C_SIZE_T), &
    & x=gmsh_cdouble(x), &
    & y=gmsh_cdouble(y), &
    & z=gmsh_cdouble(z), &
    & ax=gmsh_cdouble(ax), &
    & ay=gmsh_cdouble(ay), &
    & az=gmsh_cdouble(az), &
    & angle=gmsh_cdouble(angle), &
    & outDimTags=cptr, &
    & outDimTags_n=outDimTags_n,&
    & numElements=numElements0, &
    & numElements_n=SIZE(numElements0, KIND=C_SIZE_T), &
    & heights=heights0, &
    & heights_n=SIZE(heights0, KIND=C_SIZE_T), &
    & recombine=optval_c_bool(.FALSE., recombine), &
    & ierr=ierr)
  !!
  outDimTags = gmsh_dimtag_c2f(cptr, outDimTags_n)
  !!
  DEALLOCATE (heights0, numElements0)
  !!
END FUNCTION geo_GeoRevolve

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

FUNCTION geo_Twist(dimTags, x, y, z, dx, dy, dz, ax, &
  & ay, az, angle, numElements, heights, recombine) &
  & RESULT(outDimTags)
  INTEGER(I4B), INTENT(IN) :: dimTags(:, :)
  CLASS(*), INTENT(IN) :: x, y, z, dx, dy, dz, ax, ay, az, angle
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: numElements(:)
  CLASS(*), OPTIONAL, INTENT(IN) :: heights(:)
  LOGICAL(LGT), OPTIONAL, INTENT(IN) :: recombine
  INTEGER(I4B), ALLOCATABLE :: outDimTags(:, :)
  !
  TYPE(C_PTR) :: cptr
  INTEGER(C_SIZE_T) :: outDimTags_n
  REAL(C_DOUBLE), ALLOCATABLE :: heights0(:)
  INTEGER(C_INT), ALLOCATABLE :: numElements0(:)
  !!
  IF (PRESENT(numElements)) THEN
    numElements0 = gmsh_cint(numElements)
  ELSE
    ALLOCATE (numElements0(0))
  END IF
  !!
  IF (PRESENT(heights)) THEN
    heights0 = gmsh_cdouble(heights)
  ELSE
    ALLOCATE (heights0(0))
  END IF
  !!
  CALL gmshModelGeoTwist( &
    & dimTags=gmsh_cint(dimTags), &
    & dimTags_n=SIZE(dimTags, KIND=C_SIZE_T), &
    & x=gmsh_cdouble(x), &
    & y=gmsh_cdouble(y), &
    & z=gmsh_cdouble(z), &
    & dx=gmsh_cdouble(dx), &
    & dy=gmsh_cdouble(dy), &
    & dz=gmsh_cdouble(dz), &
    & ax=gmsh_cdouble(ax), &
    & ay=gmsh_cdouble(ay), &
    & az=gmsh_cdouble(az), &
    & angle=gmsh_cdouble(angle), &
    & outDimTags=cptr, &
    & outDimTags_n=outDimTags_n, &
    & numElements=numElements0, &
    & numElements_n=SIZE(numElements0, KIND=C_SIZE_T), &
    & heights=heights0, &
    & heights_n=SIZE(heights0, KIND=C_SIZE_T), &
    & recombine=optval_c_bool(.FALSE., recombine), &
    & ierr=ierr)
  !!
  outDimTags = gmsh_dimtag_c2f(cptr, outDimTags_n)
  !!
  DEALLOCATE (heights0, numElements0)
  !!
END FUNCTION geo_Twist

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

FUNCTION geo_ExtrudeBoundaryLayer(dimTags, &
  & numElements, heights, recombine, second, viewIndex) &
  & RESULT(outDimTags)
  INTEGER(I4B), INTENT(IN) :: dimTags(:, :)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: numElements(:)
  CLASS(*), OPTIONAL, INTENT(IN) :: heights(:)
  LOGICAL(LGT), OPTIONAL, INTENT(IN) :: recombine, second, viewIndex
  INTEGER(I4B), ALLOCATABLE :: outDimTags(:, :)
  !
  TYPE(C_PTR) :: cptr
  INTEGER(C_SIZE_T) :: outDimTags_n
  REAL(C_DOUBLE), ALLOCATABLE :: heights0(:)
  INTEGER(C_INT), ALLOCATABLE :: numElements0(:)
  !!
  IF (PRESENT(numElements)) THEN
    numElements0 = gmsh_cint(numElements)
  ELSE
    ALLOCATE (numElements0(0))
  END IF
  !!
  IF (PRESENT(heights)) THEN
    heights0 = gmsh_cdouble(heights)
  ELSE
    ALLOCATE (heights0(0))
  END IF
  !!
  CALL gmshModelGeoExtrudeBoundaryLayer( &
    & dimTags=gmsh_cint(dimTags), &
    & dimTags_n=SIZE(dimTags, kind=C_SIZE_T), &
    & outDimTags=cptr, &
    & outDimTags_n=outDimTags_n, &
    & numElements=numElements0, &
    & numElements_n=SIZE(numElements0, kind=C_SIZE_T), &
    & heights=heights0, &
    & heights_n=SIZE(heights0, kind=C_SIZE_T), &
    & recombine=optval_c_bool(.FALSE., recombine), &
    & second=optval_c_bool(.FALSE., second), &
    & viewIndex=optval_c_bool(.FALSE., viewIndex), &
    & ierr=ierr)
  !!
  outDimTags = gmsh_dimtag_c2f(cptr, outDimTags_n)
  !!
  DEALLOCATE (heights0, numElements0)
END FUNCTION geo_ExtrudeBoundaryLayer

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Translate the entities `dimTags' (given as a vector of (dim, tag) pairs) in
!! the built-in CAD representation along (`dx', `dy', `dz').

FUNCTION geo_GeoTranslate(dimTags, dx, dy, dz) &
  & RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: dimTags(:, :)
  CLASS(*), INTENT(IN) :: dx, dy, dz
  INTEGER(I4B) :: ans
  !!
  CALL gmshModelGeoTranslate( &
    & dimTags=gmsh_cint(dimTags), &
    & dimTags_n=SIZE(dimTags, KIND=C_SIZE_T), &
    & dx=gmsh_cdouble(dx), &
    & dy=gmsh_cdouble(dy), &
    & dz=gmsh_cdouble(dz), &
    & ierr=ierr)
  !!
  ans = INT(ierr, I4B)
END FUNCTION geo_GeoTranslate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Rotate the entities `dimTags' (given as a vector of (dim, tag) pairs) in
!! the built-in CAD representation by `angle' radians around the axis of
!! revolution defined by the point (`x', `y', `z') and the direction (`ax',
!! `ay', `az').

FUNCTION geo_GeoRotate(dimTags, x, y, z, ax, ay, az, &
  & angle) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: dimTags(:, :)
  CLASS(*), INTENT(IN) :: x, y, z, ax, ay, az, angle
  INTEGER(I4B) :: ans
  !!
  CALL gmshModelGeoRotate( &
    & dimTags=dimTags, &
    & dimTags_n=SIZE(dimTags, KIND=C_SIZE_T), &
    & x=gmsh_cdouble(x), &
    & y=gmsh_cdouble(y), &
    & z=gmsh_cdouble(z), &
    & ax=gmsh_cdouble(ax), &
    & ay=gmsh_cdouble(ay), &
    & az=gmsh_cdouble(az), &
    & angle=gmsh_cdouble(angle), &
    & ierr=ierr)
  !!
  ans = INT(ierr, I4B)
END FUNCTION geo_GeoRotate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Scale the entities `dimTags' (given as a vector of (dim, tag) pairs) in the
!! built-in CAD representation by factors `a', `b' and `c' along the three
!! coordinate axes; use (`x', `y', `z') as the center of the homothetic
!! transformation.

FUNCTION geo_GeoDilate(dimTags, x, y, z, a, b, c) &
  & RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: dimTags(:, :)
  CLASS(*), INTENT(IN) :: x, y, z, a, b, c
  INTEGER(I4B) :: ans
  !!
  CALL gmshModelGeoDilate( &
    & dimTags=gmsh_cint(dimTags), &
    & dimTags_n=SIZE(dimTags, KIND=C_SIZE_T), &
    & x=gmsh_cdouble(x), &
    & y=gmsh_cdouble(y), &
    & z=gmsh_cdouble(z), &
    & a=gmsh_cdouble(a), &
    & b=gmsh_cdouble(b), &
    & c=gmsh_cdouble(c), &
    & ierr=ierr)
  !!
  ans = INT(ierr, I4B)
END FUNCTION geo_GeoDilate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Mirror the entities `dimTags' (given as a vector of (dim, tag) pairs) in
!! the built-in CAD representation, with respect to the plane of equation `a'
!! * x + `b' * y + `c' * z + `d' = 0.

FUNCTION geo_Mirror(dimTags, a, b, c, d) &
  & RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: dimTags(:, :)
  CLASS(*), INTENT(IN) :: a, b, c, d
  INTEGER(I4B) :: ans
  !!
  CALL gmshModelGeoMirror( &
    & dimTags=gmsh_cint(dimTags), &
    & dimTags_n=SIZE(dimTags, KIND=C_SIZE_T), &
    & a=gmsh_cdouble(a), &
    & b=gmsh_cdouble(b), &
    & c=gmsh_cdouble(c), &
    & d=gmsh_cdouble(d), &
    & ierr=ierr)
  !!
  ans = INT(ierr, I4B)
END FUNCTION geo_Mirror

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Mirror the entities `dimTags' (given as a vector of (dim, tag) pairs) in
!! the built-in CAD representation, with respect to the plane of equation `a'
!! * x + `b' * y + `c' * z + `d' = 0. (This is a synonym for `mirror', which
!! will be deprecated in a future release.)

FUNCTION geo_Symmetrize(dimTags, a, b, c, d) &
  & RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: dimTags(:, :)
  CLASS(*), INTENT(IN) :: a, b, c, d
  INTEGER(I4B) :: ans
  !!
  CALL gmshModelGeoSymmetrize( &
    & dimTags=gmsh_cint(dimTags), &
    & dimTags_n=SIZE(dimTags, KIND=C_SIZE_T), &
    & a=gmsh_cdouble(a), &
    & b=gmsh_cdouble(b), &
    & c=gmsh_cdouble(c), &
    & d=gmsh_cdouble(d), &
    & ierr=ierr)
  !!
  ans = INT(ierr, I4B)
END FUNCTION geo_Symmetrize

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Copy the entities `dimTags' (given as a vector of (dim, tag) pairs) in the
!! built-in CAD representation; the new entities are returned in `outDimTags'.

FUNCTION geo_Copy(dimTags) RESULT(outDimTags)
  INTEGER(I4B), INTENT(IN) :: dimTags(:, :)
  INTEGER(I4B), ALLOCATABLE :: outDimTags(:, :)
  ! internal
  TYPE(C_PTR) :: cptr
  INTEGER(C_SIZE_T) :: outDimTags_n
  !!
  CALL gmshModelGeoCopy( &
    & dimTags=gmsh_cint(dimTags), &
    & dimTags_n=SIZE(dimTags, KIND=C_SIZE_T), &
    & outDimTags=cptr, &
    & outDimTags_n=outDimTags_n, &
    & ierr=ierr)
  !!
  outDimTags = gmsh_dimtag_c2f(cptr, outDimTags_n)
END FUNCTION geo_Copy

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Remove the entities `dimTags' (given as a vector of (dim, tag) pairs) in
!! the built-in CAD representation, provided that they are not on the boundary
!! of higher-dimensional entities. If `recursive' is true, remove all the
!! entities on their boundaries, down to dimension 0.

FUNCTION geo_Remove(dimTags, RECURSIVE) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: dimTags(:, :)
  LOGICAL(LGT), OPTIONAL, INTENT(IN) :: RECURSIVE
  INTEGER(I4B) :: ans
  !!
  CALL gmshModelGeoRemove( &
    & dimTags=dimTags, &
    & dimTags_n=SIZE(dimTags, KIND=C_SIZE_T), &
    & RECURSIVE=optval_c_bool(.FALSE., RECURSIVE), &
    & ierr=ierr)
  !!
  ans = INT(ierr, I4B)
END FUNCTION geo_Remove

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION geo_RemoveAllDuplicates() RESULT(ans)

  INTEGER(I4B) :: ans
  CALL gmshModelGeoRemoveAllDuplicates(ierr)
  ans = INT(ierr, I4B)
END FUNCTION geo_RemoveAllDuplicates

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Split the curve of tag `tag' in the built-in CAD representation, on the
!! specified control points `pointTags'. This feature is only available for
!! lines, splines and b-splines. Return the tag(s) `curveTags' of the newly
!! created curve(s).

FUNCTION geo_SplitCurve(tag, pointTags) &
  & RESULT(curveTags)
  INTEGER(I4B), INTENT(IN) :: tag
  INTEGER(I4B), INTENT(IN) :: pointTags(:)
  INTEGER(I4B), ALLOCATABLE :: curveTags(:)
  ! internal
  TYPE(C_PTR) :: cptr
  INTEGER(C_SIZE_T) :: curveTags_n
  !
  CALL gmshModelGeoSplitCurve( &
    & tag=gmsh_cint(tag), &
    & pointTags=gmsh_cint(pointTags), &
    & pointTags_n=SIZE(pointTags, kind=C_SIZE_T), &
    & curveTags=cptr, &
    & curveTags_n=curveTags_n, &
    & ierr=ierr)
  !
  curveTags = gmsh_intvec_c2f(cptr, curveTags_n)
  !
END FUNCTION geo_SplitCurve

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Get the maximum tag of entities of dimension `dim' in the built-in CAD
!! representation.

FUNCTION geo_GetMaxTag(dim) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: dim
  INTEGER(I4B) :: ans
  cintvar = gmshModelGeoGetMaxTag(dim=gmsh_cint(dim), ierr=ierr)
  ans = INT(cintvar, i4b)
END FUNCTION geo_GetMaxTag

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Set the maximum tag `maxTag' for entities of dimension `dim' in the built-
!! in CAD representation.

FUNCTION geo_SetMaxTag(dim, maxTag) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: dim, maxTag
  INTEGER(I4B) :: ans
  CALL gmshModelGeoSetMaxTag( &
    & dim=gmsh_cint(dim), maxTag=gmsh_cint(maxTag), ierr=ierr)
  ans = INT(ierr, I4B)
END FUNCTION geo_SetMaxTag

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Add a physical group of dimension `dim', grouping the entities with tags
!! `tags' in the built-in CAD representation. Return the tag of the physical
!! group, equal to `tag' if `tag' is positive, or a new tag if `tag' < 0. Set
!! the name of the physical group if `name' is not empty.

FUNCTION geo_AddPhysicalGroup(dim, tags, tag, name) &
  & RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: dim
  INTEGER(I4B), INTENT(IN) :: tags(:)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
  CHARACTER(*), OPTIONAL, INTENT(IN) :: name
  INTEGER(I4B) :: ans
  !!
  cintvar = gmshModelGeoAddPhysicalGroup( &
    & dim=gmsh_cint(dim), &
    & tags=gmsh_cint(tags), &
    & tags_n=SIZE(tags, kind=C_SIZE_T), &
    & tag=gmsh_opt_cint(default=-1_I4B, option=tag), &
    & name=gmsh_CString(input(default="", option=name)), &
    & ierr=ierr)
  !!
  ans = INT(cintvar, i4b)
END FUNCTION geo_AddPhysicalGroup

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Remove the physical groups `dimTags' (given as a vector of (dim, tag)
!! pairs) from the built-in CAD representation. If `dimTags' is empty, remove
!! all groups.

FUNCTION geo_RemovePhysicalGroups(dimTags) &
  & RESULT(ans)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: dimTags(:, :)
  INTEGER(I4B) :: ans
  !!
  !!
  INTEGER(C_INT), ALLOCATABLE :: dimTags0(:, :)
  !!
  IF (PRESENT(dimTags)) THEN
    dimTags0 = gmsh_cint(dimTags)
  ELSE
    ALLOCATE (dimTags0(0, 0))
  END IF
  !!
  CALL gmshModelGeoRemovePhysicalGroups( &
    & dimTags=dimTags0, &
    & dimTags_n=SIZE(dimTags0, KIND=C_SIZE_T), &
    & ierr=ierr)
  !!
  ans = INT(ierr, I4B)
END FUNCTION geo_RemovePhysicalGroups

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Synchronize the built-in CAD representation with the current Gmsh model.
!! This can be called at any time, but since it involves a non trivial amount
!! of processing, the number of synchronization points should normally be
!! minimized. Without synchronization the entities in the built-in CAD
!! representation are not available to any function outside of the built-in
!! CAD kernel functions.

FUNCTION geo_Synchronize() RESULT(ans)
  INTEGER(I4B) :: ans
  CALL gmshModelGeoSynchronize(ierr)
  ans = INT(ierr, I4B)
END FUNCTION geo_Synchronize

END MODULE GmshModelGeo_Class
