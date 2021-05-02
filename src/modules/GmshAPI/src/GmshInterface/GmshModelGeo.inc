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

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------


! /* Add a geometrical point in the built-in CAD representation, at coordinates
!  * (`x', `y', `z'). If `meshSize' is > 0, add a meshing constraint at that
!  * point. If `tag' is positive, set the tag explicitly; otherwise a new tag is
!  * selected automatically. Return the tag of the point. (Note that the point
!  * will be added in the current model only after `synchronize' is called. This
!  * behavior holds for all the entities added in the geo module.) */
! GMSH_API int gmshModelGeoAddPoint(const double x,
!                                   const double y,
!                                   const double z,
!                                   const double meshSize,
!                                   const int tag,
!                                   int *ierr);

INTERFACE
FUNCTION gmshModelGeoAddPoint(x, y, z, meshSize, tag, ierr) RESULT( Ans ) &
  & BIND( C, NAME="gmshModelGeoAddPoint")
  IMPORT
  _R_V_IN_ :: x
  _R_V_IN_ :: y
  _R_V_IN_ :: z
  _R_V_IN_ :: meshSize
  _I_V_IN_ :: tag
  _I_OUT_ :: ierr
  INTEGER( I4B ) :: Ans
END FUNCTION gmshModelGeoAddPoint
END INTERFACE

PUBLIC :: gmshModelGeoAddPoint

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Add a straight line segment in the built-in CAD representation, between the
!  * two points with tags `startTag' and `endTag'. If `tag' is positive, set the
!  * tag explicitly; otherwise a new tag is selected automatically. Return the
!  * tag of the line. */
! GMSH_API int gmshModelGeoAddLine(const int startTag,
!                                  const int endTag,
!                                  const int tag,
!                                  int *ierr);

INTERFACE
FUNCTION gmshModelGeoAddLine(startTag, endTag, tag, ierr) RESULT( Ans ) &
  & BIND( C, NAME="gmshModelGeoAddLine")
  IMPORT
  _I_V_IN_ :: startTag, endTag, tag
  _I_OUT_ :: ierr
  INTEGER( I4B ) :: Ans
END FUNCTION gmshModelGeoAddLine
END INTERFACE

PUBLIC :: gmshModelGeoAddLine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Add a circle arc (strictly smaller than Pi) in the built-in CAD
!  * representation, between the two points with tags `startTag' and `endTag',
!  * and with center `centerTag'. If `tag' is positive, set the tag explicitly;
!  * otherwise a new tag is selected automatically. If (`nx', `ny', `nz') != (0,
!  * 0, 0), explicitly set the plane of the circle arc. Return the tag of the
!  * circle arc. */
! GMSH_API int gmshModelGeoAddCircleArc(const int startTag,
!                                       const int centerTag,
!                                       const int endTag,
!                                       const int tag,
!                                       const double nx,
!                                       const double ny,
!                                       const double nz,
!                                       int *ierr);

INTERFACE
FUNCTION gmshModelGeoAddCircleArc(startTag, centerTag, endTag, tag, nx, ny, &
  & nz, ierr) RESULT( Ans ) &
  & BIND( C, NAME="gmshModelGeoAddCircleArc")
  IMPORT
  _I_V_IN_ :: startTag, endTag, tag, centerTag
  _R_V_IN_ :: nx, ny, nz
  _I_OUT_ :: ierr
  INTEGER( I4B ) :: Ans
END FUNCTION gmshModelGeoAddCircleArc
END INTERFACE

PUBLIC :: gmshModelGeoAddCircleArc

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Add an ellipse arc (strictly smaller than Pi) in the built-in CAD
!  * representation, between the two points `startTag' and `endTag', and with
!  * center `centerTag' and major axis point `majorTag'. If `tag' is positive,
!  * set the tag explicitly; otherwise a new tag is selected automatically. If
!  * (`nx', `ny', `nz') != (0, 0, 0), explicitly set the plane of the circle
!  * arc. Return the tag of the ellipse arc. */
! GMSH_API int gmshModelGeoAddEllipseArc(const int startTag,
!                                        const int centerTag,
!                                        const int majorTag,
!                                        const int endTag,
!                                        const int tag,
!                                        const double nx,
!                                        const double ny,
!                                        const double nz,
!                                        int *ierr);

INTERFACE
FUNCTION gmshModelGeoAddEllipseArc(startTag, centerTag, majorTag, endTag, &
  & tag, nx, ny, nz, ierr) RESULT( Ans ) &
  & BIND( C, NAME="gmshModelGeoAddEllipseArc")
  IMPORT
  _I_V_IN_ :: startTag, centerTag, majorTag, endTag, tag
  _R_V_IN_ :: nx, ny, nz
  _I_OUT_ :: ierr
  INTEGER( I4B ) :: Ans
END FUNCTION gmshModelGeoAddEllipseArc
END INTERFACE

PUBLIC :: gmshModelGeoAddEllipseArc

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Add a spline (Catmull-Rom) curve in the built-in CAD representation, going
!  * through the points `pointTags'. If `tag' is positive, set the tag
!  * explicitly; otherwise a new tag is selected automatically. Create a
!  * periodic curve if the first and last points are the same. Return the tag of
!  * the spline curve. */
! GMSH_API int gmshModelGeoAddSpline(int *pointTags, size_t pointTags_n,
!                                    const int tag,
!                                    int *ierr);

INTERFACE
FUNCTION gmshModelGeoAddSpline(pointTags, pointTags_n, tag, ierr) &
  & RESULT( Ans ) &
  & BIND( C, NAME="gmshModelGeoAddSpline")
  IMPORT
  _ST_V_IN_ :: pointTags_n
  _I_IN_ :: pointTags(pointTags_n)
  _I_V_IN_ :: tag
  _I_OUT_ :: ierr
  INTEGER( I4B ) :: Ans
END FUNCTION gmshModelGeoAddSpline
END INTERFACE

PUBLIC :: gmshModelGeoAddSpline

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Add a cubic b-spline curve in the built-in CAD representation, with
!  * `pointTags' control points. If `tag' is positive, set the tag explicitly;
!  * otherwise a new tag is selected automatically. Creates a periodic curve if
!  * the first and last points are the same. Return the tag of the b-spline
!  * curve. */
!
! GMSH_API int gmshModelGeoAddBSpline(int *pointTags, size_t pointTags_n,
!                                     const int tag,
!                                     int *ierr);

INTERFACE
FUNCTION gmshModelGeoAddBSpline(pointTags, pointTags_n, tag, ierr) &
  & RESULT( Ans ) &
  & BIND( C, NAME="gmshModelGeoAddBSpline")
  IMPORT
  _ST_V_IN_ :: pointTags_n
  _I_IN_ :: pointTags(pointTags_n)
  _I_V_IN_ :: tag
  _I_OUT_ :: ierr
  INTEGER( I4B ) :: Ans
END FUNCTION gmshModelGeoAddBSpline
END INTERFACE

PUBLIC :: gmshModelGeoAddBSpline

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Add a Bezier curve in the built-in CAD representation, with `pointTags'
!  * control points. If `tag' is positive, set the tag explicitly; otherwise a
!  * new tag is selected automatically.  Return the tag of the Bezier curve. */
! GMSH_API int gmshModelGeoAddBezier(int *pointTags, size_t pointTags_n,
!                                    const int tag,
!                                    int *ierr);

INTERFACE
FUNCTION gmshModelGeoAddBezier(pointTags, pointTags_n, tag, ierr) &
  & RESULT( Ans ) &
  & BIND( C, NAME="gmshModelGeoAddBezier")
  IMPORT
  _ST_V_IN_ :: pointTags_n
  _I_IN_ :: pointTags(pointTags_n)
  _I_V_IN_ :: tag
  _I_OUT_ :: ierr
  INTEGER( I4B ) :: Ans
END FUNCTION gmshModelGeoAddBezier
END INTERFACE

PUBLIC :: gmshModelGeoAddBezier

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Add a polyline curve in the built-in CAD representation, going through the
!  * points `pointTags'. If `tag' is positive, set the tag explicitly; otherwise
!  * a new tag is selected automatically. Create a periodic curve if the first
!  * and last points are the same. Return the tag of the polyline curve. */
!
! GMSH_API int gmshModelGeoAddPolyline(int *pointTags, size_t pointTags_n,
!                                      const int tag,
!                                      int *ierr);

INTERFACE
FUNCTION gmshModelGeoAddPolyline(pointTags, pointTags_n, tag, ierr) &
  & RESULT( Ans ) &
  & BIND( C, NAME="gmshModelGeoAddPolyline")
  IMPORT
  _ST_V_IN_ :: pointTags_n
  _I_IN_ :: pointTags(pointTags_n)
  _I_V_IN_ :: tag
  _I_OUT_ :: ierr
  INTEGER( I4B ) :: Ans
END FUNCTION gmshModelGeoAddPolyline
END INTERFACE

PUBLIC :: gmshModelGeoAddPolyline

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Add a spline (Catmull-Rom) curve in the built-in CAD representation, going
!  * through points sampling the curves in `curveTags'. The density of sampling
!  * points on each curve is governed by `numIntervals'. If `tag' is positive,
!  * set the tag explicitly; otherwise a new tag is selected automatically.
!  * Return the tag of the spline. */
!
! GMSH_API int gmshModelGeoAddCompoundSpline(int *curveTags, size_t curveTags_n,
!                                            const int numIntervals,
!                                            const int tag,
!                                            int *ierr);

INTERFACE
FUNCTION gmshModelGeoAddCompoundSpline(curveTags, curveTags_n, numIntervals, &
  & tag, ierr) RESULT( Ans ) &
  & BIND( C, NAME="gmshModelGeoAddCompoundSpline")
  IMPORT
  _ST_V_IN_ :: curveTags_n
  _I_IN_ :: curveTags(curveTags_n)
  _I_V_IN_ :: numIntervals, tag
  _I_OUT_ :: ierr
  INTEGER( I4B ) :: Ans
END FUNCTION gmshModelGeoAddCompoundSpline
END INTERFACE

PUBLIC :: gmshModelGeoAddCompoundSpline

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Add a b-spline curve in the built-in CAD representation, with control
!  * points sampling the curves in `curveTags'. The density of sampling points
!  * on each curve is governed by `numIntervals'. If `tag' is positive, set the
!  * tag explicitly; otherwise a new tag is selected automatically. Return the
!  * tag of the b-spline. */
! GMSH_API int gmshModelGeoAddCompoundBSpline(int *curveTags, size_t curveTags_n,
!                                             const int numIntervals,
!                                             const int tag,
!                                             int *ierr);

INTERFACE
FUNCTION gmshModelGeoAddCompoundBSpline(curveTags, curveTags_n, numIntervals, &
  & tag, ierr) RESULT( Ans ) &
  & BIND( C, NAME="gmshModelGeoAddCompoundBSpline")
  IMPORT
  _ST_V_IN_ :: curveTags_n
  _I_IN_ :: curveTags(curveTags_n)
  _I_V_IN_ :: numIntervals, tag
  _I_OUT_ :: ierr
  INTEGER( I4B ) :: Ans
END FUNCTION gmshModelGeoAddCompoundBSpline
END INTERFACE

PUBLIC :: gmshModelGeoAddCompoundBSpline

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Add a curve loop (a closed wire) in the built-in CAD representation, formed
!  * by the curves `curveTags'. `curveTags' should contain (signed) tags of
!  * model entities of dimension 1 forming a closed loop: a negative tag
!  * signifies that the underlying curve is considered with reversed
!  * orientation. If `tag' is positive, set the tag explicitly; otherwise a new
!  * tag is selected automatically. If `reorient' is set, automatically reorient
!  * the curves if necessary. Return the tag of the curve loop. */
!
! GMSH_API int gmshModelGeoAddCurveLoop(int *curveTags, size_t curveTags_n,
!                                       const int tag,
!                                       const int reorient,
!                                       int *ierr);

INTERFACE
FUNCTION gmshModelGeoAddCurveLoop(curveTags, curveTags_n, tag, reorient,&
  & ierr) RESULT( Ans ) &
  & BIND( C, NAME="gmshModelGeoAddCurveLoop")
  IMPORT
  _ST_V_IN_ :: curveTags_n
  _I_IN_ :: curveTags(curveTags_n)
  _I_V_IN_ :: tag, reorient
  _I_OUT_ :: ierr
  INTEGER( I4B ) :: Ans
END FUNCTION gmshModelGeoAddCurveLoop
END INTERFACE

PUBLIC :: gmshModelGeoAddCurveLoop

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Add curve loops in the built-in CAD representation based on the curves
!  * `curveTags'. Return the `tags' of found curve loops, if any. */
!
! GMSH_API void gmshModelGeoAddCurveLoops(int *curveTags, size_t curveTags_n,
!                                         int **tags, size_t *tags_n,
!                                         int *ierr);

INTERFACE
SUBROUTINE gmshModelGeoAddCurveLoops(curveTags, curveTags_n, tags, tags_n,&
  & ierr) &
  & BIND( C, NAME="gmshModelGeoAddCurveLoops")
  IMPORT
  _ST_V_IN_ :: curveTags_n
  _I_IN_ :: curveTags(curveTags_n)
  _CPTR_IN_ :: tags
  _ST_OUT_ :: tags_n
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelGeoAddCurveLoops
END INTERFACE

PUBLIC :: gmshModelGeoAddCurveLoops

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Add a plane surface in the built-in CAD representation, defined by one or
!  * more curve loops `wireTags'. The first curve loop defines the exterior
!  * contour; additional curve loop define holes. If `tag' is positive, set the
!  * tag explicitly; otherwise a new tag is selected automatically. Return the
!  * tag of the surface. */
! GMSH_API int gmshModelGeoAddPlaneSurface(int *wireTags, size_t wireTags_n,
!                                          const int tag,
!                                          int *ierr);

INTERFACE
FUNCTION gmshModelGeoAddPlaneSurface(wireTags, wireTags_n, tag, ierr) &
  & RESULT( Ans ) &
  & BIND( C, NAME="gmshModelGeoAddPlaneSurface")
  IMPORT
  _ST_V_IN_ :: wireTags_n
  _I_IN_ :: wireTags(wireTags_n)
  _I_V_IN_ :: tag
  _I_OUT_ :: ierr
  INTEGER( I4B ) :: Ans
END FUNCTION gmshModelGeoAddPlaneSurface
END INTERFACE

PUBLIC :: gmshModelGeoAddPlaneSurface

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Add a surface in the built-in CAD representation, filling the curve loop in `wireTags' using transfinite interpolation. Currently only a single curve loop is supported; this curve loop should be composed by 3 or 4 curves only. If `tag' is positive, set the tag explicitly; otherwise a new tag is selected automatically. Return the tag of the surface.
!
! GMSH_API int gmshModelGeoAddSurfaceFilling(int *wireTags, size_t wireTags_n,
!                                            const int tag,
!                                            const int sphereCenterTag,
!                                            int *ierr);

INTERFACE
FUNCTION gmshModelGeoAddSurfaceFilling(wireTags, wireTags_n, tag, &
  & sphereCenterTag, ierr) &
  & RESULT( Ans ) &
  & BIND( C, NAME="gmshModelGeoAddSurfaceFilling")
  IMPORT
  _ST_V_IN_ :: wireTags_n
  _I_IN_ :: wireTags(wireTags_n)
  _I_V_IN_ :: tag
  _I_V_IN_ :: sphereCenterTag
  _I_OUT_ :: ierr
  INTEGER( I4B ) :: Ans
END FUNCTION gmshModelGeoAddSurfaceFilling
END INTERFACE

PUBLIC :: gmshModelGeoAddSurfaceFilling

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Add a surface loop (a closed shell) formed by `surfaceTags' in the built-in
!  * CAD representation.  If `tag' is positive, set the tag explicitly;
!  * otherwise a new tag is selected automatically. Return the tag of the shell. */
!
! GMSH_API int gmshModelGeoAddSurfaceLoop(int *surfaceTags, size_t surfaceTags_n,
!                                         const int tag,
!                                         int *ierr);

INTERFACE
FUNCTION gmshModelGeoAddSurfaceLoop(surfaceTags, surfaceTags_n, tag, ierr) &
  & RESULT( Ans ) &
  & BIND( C, NAME="gmshModelGeoAddSurfaceLoop")
  IMPORT
  _ST_V_IN_ :: surfaceTags_n
  _I_IN_ :: surfaceTags(surfaceTags_n)
  _I_V_IN_ :: tag
  _I_OUT_ :: ierr
  INTEGER( I4B ) :: Ans
END FUNCTION gmshModelGeoAddSurfaceLoop
END INTERFACE

PUBLIC :: gmshModelGeoAddSurfaceLoop

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Add a volume (a region) in the built-in CAD representation, defined by one
!  * or more shells `shellTags'. The first surface loop defines the exterior
!  * boundary; additional surface loop define holes. If `tag' is positive, set
!  * the tag explicitly; otherwise a new tag is selected automatically. Return
!  * the tag of the volume. */
! GMSH_API int gmshModelGeoAddVolume(int *shellTags, size_t shellTags_n,
!                                    const int tag,
!                                    int *ierr);

INTERFACE
FUNCTION gmshModelGeoAddVolume(shellTags, shellTags_n, tag, ierr) &
  & RESULT( Ans ) &
  & BIND( C, NAME="gmshModelGeoAddVolume")
  IMPORT
  _ST_V_IN_ :: shellTags_n
  _I_IN_ :: shellTags(shellTags_n)
  _I_V_IN_ :: tag
  _I_OUT_ :: ierr
  INTEGER( I4B ) :: Ans
END FUNCTION gmshModelGeoAddVolume
END INTERFACE

PUBLIC :: gmshModelGeoAddVolume

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Extrude the entities `dimTags' in the built-in CAD representation, using a
!  * translation along (`dx', `dy', `dz'). Return extruded entities in
!  * `outDimTags'. If `numElements' is not empty, also extrude the mesh: the
!  * entries in `numElements' give the number of elements in each layer. If
!  * `height' is not empty, it provides the (cumulative) height of the different
!  * layers, normalized to 1. If `recombine' is set, recombine the mesh in the
!  * layers. */
!
! GMSH_API void gmshModelGeoExtrude(int *dimTags, size_t dimTags_n,
!                                   const double dx,
!                                   const double dy,
!                                   const double dz,
!                                   int **outDimTags, size_t *outDimTags_n,
!                                   int *numElements, size_t numElements_n,
!                                   double *heights, size_t heights_n,
!                                   const int recombine,
!                                   int *ierr);

INTERFACE
SUBROUTINE gmshModelGeoExtrude(dimTags, dimTags_n, dx, dy, dz, outDimTags, &
  & outDimTags_n, numElements, numElements_n, heights, heights_n, recombine, &
  & ierr) &
  & BIND( C, NAME="gmshModelGeoExtrude")
  IMPORT
  _ST_V_IN_ :: dimTags_n
  _I_IN_ :: dimTags(dimTags_n)
  _R_V_IN_ :: dx, dy, dz
  _CPTR_IN_ :: outDimTags
  _ST_OUT_ :: outDimTags_n
  _ST_V_IN_ :: numElements_n
  _I_IN_ :: numElements(numElements_n)
  _ST_V_IN_ :: heights_n
  _I_IN_ :: heights(heights_n)
  _I_V_IN_ :: recombine
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelGeoExtrude
END INTERFACE

PUBLIC :: gmshModelGeoExtrude

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Extrude the entities `dimTags' in the built-in CAD representation, using a
!  * rotation of `angle' radians around the axis of revolution defined by the
!  * point (`x', `y', `z') and the direction (`ax', `ay', `az'). The angle
!  * should be strictly smaller than Pi. Return extruded entities in
!  * `outDimTags'. If `numElements' is not empty, also extrude the mesh: the
!  * entries in `numElements' give the number of elements in each layer. If
!  * `height' is not empty, it provides the (cumulative) height of the different
!  * layers, normalized to 1. If `recombine' is set, recombine the mesh in the
!  * layers. */
! GMSH_API void gmshModelGeoRevolve(int *dimTags, size_t dimTags_n,
!                                   const double x,
!                                   const double y,
!                                   const double z,
!                                   const double ax,
!                                   const double ay,
!                                   const double az,
!                                   const double angle,
!                                   int **outDimTags, size_t *outDimTags_n,
!                                   int *numElements, size_t numElements_n,
!                                   double *heights, size_t heights_n,
!                                   const int recombine,
!                                   int *ierr);

INTERFACE
SUBROUTINE gmshModelGeoRevolve(dimTags, dimTags_n, x, y, z, ax, ay, az, &
  & angle, outDimTags, outDimTags_n, numElements, numElements_n, heights, &
  & heights_n, recombine, ierr) &
  & BIND( C, NAME="gmshModelGeoRevolve")
  IMPORT
  _ST_V_IN_ :: dimTags_n
  _I_IN_ :: dimTags(dimTags_n)
  _R_V_IN_ :: x, y, z, ax, ay, az, angle
  _CPTR_IN_ :: outDimTags
  _ST_OUT_ :: outDimTags_n
  _ST_V_IN_ :: numElements_n
  _I_IN_ :: numElements(numElements_n)
  _ST_V_IN_ :: heights_n
  _I_IN_ :: heights(heights_n)
  _I_V_IN_ :: recombine
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelGeoRevolve
END INTERFACE

PUBLIC :: gmshModelGeoRevolve

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Extrude the entities `dimTags' in the built-in CAD representation, using a
!  * combined translation and rotation of `angle' radians, along (`dx', `dy',
!  * `dz') and around the axis of revolution defined by the point (`x', `y',
!  * `z') and the direction (`ax', `ay', `az'). The angle should be strictly
!  * smaller than Pi. Return extruded entities in `outDimTags'. If `numElements'
!  * is not empty, also extrude the mesh: the entries in `numElements' give the
!  * number of elements in each layer. If `height' is not empty, it provides the
!  * (cumulative) height of the different layers, normalized to 1. If
!  * `recombine' is set, recombine the mesh in the layers. */
!
! GMSH_API void gmshModelGeoTwist(int *dimTags, size_t dimTags_n,
!                                 const double x,
!                                 const double y,
!                                 const double z,
!                                 const double dx,
!                                 const double dy,
!                                 const double dz,
!                                 const double ax,
!                                 const double ay,
!                                 const double az,
!                                 const double angle,
!                                 int **outDimTags, size_t *outDimTags_n,
!                                 int *numElements, size_t numElements_n,
!                                 double *heights, size_t heights_n,
!                                 const int recombine,
!                                 int *ierr);

INTERFACE
SUBROUTINE gmshModelGeoTwist(dimTags, dimTags_n, x, y, z, dx, dy, dz, ax, &
  & ay, az, angle, outDimTags, outDimTags_n, numElements, numElements_n, &
  & heights, heights_n, recombine, ierr) &
  & BIND( C, NAME="gmshModelGeoTwist")
  IMPORT
  _ST_V_IN_ :: dimTags_n
  _I_IN_ :: dimTags(dimTags_n)
  _R_V_IN_ :: x, y, z, dx, dy, dz, ax, ay, az, angle
  _CPTR_IN_ :: outDimTags
  _ST_OUT_ :: outDimTags_n
  _ST_V_IN_ :: numElements_n
  _I_IN_ :: numElements(numElements_n)
  _ST_V_IN_ :: heights_n
  _I_IN_ :: heights(heights_n)
  _I_V_IN_ :: recombine
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelGeoTwist
END INTERFACE

PUBLIC :: gmshModelGeoTwist

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Extrude the entities `dimTags' in the built-in CAD representation along the
!  * normals of the mesh, creating discrete boundary layer entities. Return
!  * extruded entities in `outDimTags'. The entries in `numElements' give the
!  * number of elements in each layer. If `height' is not empty, it provides the
!  * height of the different layers. If `recombine' is set, recombine the mesh
!  * in the layers. A second boundary layer can be created from the same
!  * entities if `second' is set. If `viewIndex' is >= 0, use the corresponding
!  * view to either specify the normals (if the view contains a vector field) or
!  * scale the normals (if the view is scalar). */
! GMSH_API void gmshModelGeoExtrudeBoundaryLayer(int *dimTags, size_t dimTags_n,
!                                                int **outDimTags, size_t *outDimTags_n,
!                                                int *numElements, size_t numElements_n,
!                                                double *heights, size_t heights_n,
!                                                const int recombine,
!                                                const int second,
!                                                const int viewIndex,
!                                                int *ierr);

INTERFACE
SUBROUTINE gmshModelGeoExtrudeBoundaryLayer(dimTags, dimTags_n, outDimTags, &
  & outDimTags_n, numElements, numElements_n, &
  & heights, heights_n, recombine, second, viewIndex, ierr) &
  & BIND( C, NAME="gmshModelGeoExtrudeBoundaryLayer")
  IMPORT
  _ST_V_IN_ :: dimTags_n
  _I_IN_ :: dimTags(dimTags_n)
  _CPTR_IN_ :: outDimTags
  _ST_OUT_ :: outDimTags_n
  _ST_V_IN_ :: numElements_n
  _I_IN_ :: numElements(numElements_n)
  _ST_V_IN_ :: heights_n
  _I_IN_ :: heights(heights_n)
  _I_V_IN_ :: recombine, second, viewIndex
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelGeoExtrudeBoundaryLayer
END INTERFACE

PUBLIC :: gmshModelGeoExtrudeBoundaryLayer

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Translate the entities `dimTags' in the built-in CAD representation along
!  * (`dx', `dy', `dz'). */
!
! GMSH_API void gmshModelGeoTranslate(int *dimTags, size_t dimTags_n,
!                                     const double dx,
!                                     const double dy,
!                                     const double dz,
!                                     int *ierr);

INTERFACE
SUBROUTINE gmshModelGeoTranslate(dimTags, dimTags_n, dx, dy, dz, ierr) &
  & BIND( C, NAME="gmshModelGeoTranslate")
  IMPORT
  _ST_V_IN_ :: dimTags_n
  _I_IN_ :: dimTags(dimTags_n)
  _R_V_IN_ :: dx, dy, dz
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelGeoTranslate
END INTERFACE

PUBLIC :: gmshModelGeoTranslate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Rotate the entities `dimTags' in the built-in CAD representation by `angle'
!  * radians around the axis of revolution defined by the point (`x', `y', `z')
!  * and the direction (`ax', `ay', `az'). */
! GMSH_API void gmshModelGeoRotate(int *dimTags, size_t dimTags_n,
!                                  const double x,
!                                  const double y,
!                                  const double z,
!                                  const double ax,
!                                  const double ay,
!                                  const double az,
!                                  const double angle,
!                                  int *ierr);

INTERFACE
SUBROUTINE gmshModelGeoRotate(dimTags, dimTags_n, x, y, z, ax, ay, az, &
  & angle, ierr) &
  & BIND( C, NAME="gmshModelGeoRotate")
  IMPORT
  _ST_V_IN_ :: dimTags_n
  _I_IN_ :: dimTags(dimTags_n)
  _R_V_IN_ :: x, y, z, ax, ay, az, angle
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelGeoRotate
END INTERFACE

PUBLIC :: gmshModelGeoRotate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Scale the entities `dimTag' in the built-in CAD representation by factors
!  * `a', `b' and `c' along the three coordinate axes; use (`x', `y', `z') as
!  * the center of the homothetic transformation. */
! GMSH_API void gmshModelGeoDilate(int *dimTags, size_t dimTags_n,
!                                  const double x,
!                                  const double y,
!                                  const double z,
!                                  const double a,
!                                  const double b,
!                                  const double c,
!                                  int *ierr);

INTERFACE
SUBROUTINE gmshModelGeoDilate(dimTags, dimTags_n, x, y, z, a, b, c, ierr) &
  & BIND( C, NAME="gmshModelGeoDilate")
  IMPORT
  _ST_V_IN_ :: dimTags_n
  _I_IN_ :: dimTags(dimTags_n)
  _R_V_IN_ :: x, y, z, a, b, c
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelGeoDilate
END INTERFACE

PUBLIC :: gmshModelGeoDilate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Mirror the entities `dimTag' in the built-in CAD representation, with
!  * respect to the plane of equation `a' * x + `b' * y + `c' * z + `d' = 0. */
! GMSH_API void gmshModelGeoMirror(int *dimTags, size_t dimTags_n,
!                                  const double a,
!                                  const double b,
!                                  const double c,
!                                  const double d,
!                                  int *ierr);

INTERFACE
SUBROUTINE gmshModelGeoMirror(dimTags, dimTags_n, a, b, c, d, ierr) &
  & BIND( C, NAME="gmshModelGeoMirror")
  IMPORT
  _ST_V_IN_ :: dimTags_n
  _I_IN_ :: dimTags(dimTags_n)
  _R_V_IN_ :: a, b, c, d
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelGeoMirror
END INTERFACE

PUBLIC :: gmshModelGeoMirror

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Mirror the entities `dimTag' in the built-in CAD representation, with
!  * respect to the plane of equation `a' * x + `b' * y + `c' * z + `d' = 0.
!  * (This is a synonym for `mirror', which will be deprecated in a future
!  * release.) */
! GMSH_API void gmshModelGeoSymmetrize(int *dimTags, size_t dimTags_n,
!                                      const double a,
!                                      const double b,
!                                      const double c,
!                                      const double d,
!                                      int *ierr);

INTERFACE
SUBROUTINE gmshModelGeoSymmetrize(dimTags, dimTags_n, a, b, c, d, ierr) &
  & BIND( C, NAME="gmshModelGeoSymmetrize")
  IMPORT
  _ST_V_IN_ :: dimTags_n
  _I_IN_ :: dimTags(dimTags_n)
  _R_V_IN_ :: a, b, c, d
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelGeoSymmetrize
END INTERFACE

PUBLIC :: gmshModelGeoSymmetrize

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Copy the entities `dimTags' in the built-in CAD representation; the new
!  * entities are returned in `outDimTags'. */
! GMSH_API void gmshModelGeoCopy(int *dimTags, size_t dimTags_n,
!                                int **outDimTags, size_t *outDimTags_n,
!                                int *ierr);

INTERFACE
SUBROUTINE gmshModelGeoCopy(dimTags, dimTags_n, outDimTags, outDimTags_n, &
  & ierr) &
  & BIND( C, NAME="gmshModelGeoCopy")
  IMPORT
  _ST_V_IN_ :: dimTags_n
  _I_IN_ :: dimTags(dimTags_n)
  _CPTR_IN_ :: outDimTags
  _ST_OUT_ :: outDimTags_n
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelGeoCopy
END INTERFACE

PUBLIC :: gmshModelGeoCopy

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Remove the entities `dimTags' in the built-in CAD representation. If
!  * `recursive' is true, remove all the entities on their boundaries, down to
!  * dimension 0. */
!
! GMSH_API void gmshModelGeoRemove(int *dimTags, size_t dimTags_n,
!                                  const int recursive,
!                                  int *ierr);

INTERFACE
SUBROUTINE gmshModelGeoRemove(dimTags, dimTags_n, recursive, ierr) &
  & BIND( C, NAME="gmshModelGeoRemove")
  IMPORT
  _ST_V_IN_ :: dimTags_n
  _I_IN_ :: dimTags(dimTags_n)
  _I_V_IN_ :: recursive
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelGeoRemove
END INTERFACE

PUBLIC :: gmshModelGeoRemove

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Remove all duplicate entities in the built-in CAD representation (different
!  * entities at the same geometrical location). */
!
! GMSH_API void gmshModelGeoRemoveAllDuplicates(int *ierr);

INTERFACE
SUBROUTINE gmshModelGeoRemoveAllDuplicates(ierr) &
  & BIND( C, NAME="gmshModelGeoRemoveAllDuplicates")
  IMPORT
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelGeoRemoveAllDuplicates
END INTERFACE

PUBLIC :: gmshModelGeoRemoveAllDuplicates

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Split the curve of tag `tag' in the built-in CAD representation, on the
!  * control points `pointTags'. Return the tags `curveTags' of the newly
!  * created curves. */
!
! GMSH_API void gmshModelGeoSplitCurve(const int tag,
!                                      int *pointTags, size_t pointTags_n,
!                                      int **curveTags, size_t *curveTags_n,
!                                      int *ierr);

INTERFACE
SUBROUTINE gmshModelGeoSplitCurve(tag, pointTags, pointTags_n, curveTags, &
  & curveTags_n, ierr) &
  & BIND( C, NAME="gmshModelGeoSplitCurve")
  IMPORT
  _I_V_IN_ :: tag
  _ST_V_IN_ :: pointTags_n
  _I_IN_ :: pointTags(pointTags_n)
  _CPTR_IN_ :: curveTags
  _ST_OUT_ :: curveTags_n
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelGeoSplitCurve
END INTERFACE

PUBLIC :: gmshModelGeoSplitCurve

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Get the maximum tag of entities of dimension `dim' in the built-in CAD
!  * representation. */
!
! GMSH_API int gmshModelGeoGetMaxTag(const int dim,
!                                    int *ierr);

INTERFACE
FUNCTION gmshModelGeoGetMaxTag(dim, ierr) RESULT( ans ) &
  & BIND( C, NAME="gmshModelGeoGetMaxTag")
  IMPORT
  _I_V_IN_ :: dim
  _I_OUT_ :: ierr
  INTEGER( I4B ) :: ans
END FUNCTION gmshModelGeoGetMaxTag
END INTERFACE

PUBLIC :: gmshModelGeoGetMaxTag

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Set the maximum tag `maxTag' for entities of dimension `dim' in the built-
!  * in CAD representation. */
!
! GMSH_API void gmshModelGeoSetMaxTag(const int dim,
!                                     const int maxTag,
!                                     int *ierr);

INTERFACE
SUBROUTINE gmshModelGeoSetMaxTag(dim, maxTag, ierr) &
  & BIND( C, NAME="gmshModelGeoSetMaxTag")
  IMPORT
  _I_V_IN_ :: dim, maxTag
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelGeoSetMaxTag
END INTERFACE

PUBLIC :: gmshModelGeoSetMaxTag

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Add a physical group of dimension `dim', grouping the entities with tags
!  * `tags' in the built-in CAD representation. Return the tag of the physical
!  * group, equal to `tag' if `tag' is positive, or a new tag if `tag' < 0. */
!
! GMSH_API int gmshModelGeoAddPhysicalGroup(const int dim,
!                                           int *tags, size_t tags_n,
!                                           const int tag,
!                                           int *ierr);

INTERFACE
FUNCTION gmshModelGeoAddPhysicalGroup(dim, tags, tags_n, tag, ierr) &
  & RESULT( ans ) &
  & BIND( C, NAME="gmshModelGeoAddPhysicalGroup")
  IMPORT
  _I_V_IN_ :: dim
  _ST_V_IN_ :: tags_n
  _I_IN_ :: tags(tags_n)
  _I_V_IN_ :: tag
  _I_OUT_ :: ierr
  INTEGER( I4B ) :: ans
END FUNCTION gmshModelGeoAddPhysicalGroup
END INTERFACE

PUBLIC :: gmshModelGeoAddPhysicalGroup

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Remove the physical groups `dimTags' from the built-in CAD representation.
!  * If `dimTags' is empty, remove all groups. */
!
! GMSH_API void gmshModelGeoRemovePhysicalGroups(int *dimTags, size_t dimTags_n,
!                                                int *ierr);

INTERFACE
SUBROUTINE gmshModelGeoRemovePhysicalGroups(dimTags, dimTags_n, ierr) &
  & BIND( C, NAME="gmshModelGeoRemovePhysicalGroups")
  IMPORT
  _ST_V_IN_ :: dimTags_n
  _I_IN_ :: dimTags(dimTags_n)
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelGeoRemovePhysicalGroups
END INTERFACE

PUBLIC :: gmshModelGeoRemovePhysicalGroups

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Synchronize the built-in CAD representation with the current Gmsh model.
!  * This can be called at any time, but since it involves a non trivial amount
!  * of processing, the number of synchronization points should normally be
!  * minimized. Without synchronization the entities in the built-in CAD
!  * representation are not available to any function outside of the built-in
!  * CAD kernel functions. */
!
! GMSH_API void gmshModelGeoSynchronize(int *ierr);

INTERFACE
SUBROUTINE gmshModelGeoSynchronize(ierr) &
  & BIND( C, NAME="gmshModelGeoSynchronize")
  IMPORT
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelGeoSynchronize
END INTERFACE

PUBLIC :: gmshModelGeoSynchronize