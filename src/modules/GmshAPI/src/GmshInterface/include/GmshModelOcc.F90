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

! Add a geometrical point in the OpenCASCADE CAD representation, at
! coordinates (`x', `y', `z'). If `meshSize' is > 0, add a meshing constraint
! at that point. If `tag' is positive, set the tag explicitly; otherwise a
! new tag is selected automatically. Return the tag of the point. (Note that
! the point will be added in the current model only after `synchronize' is
! called. This behavior holds for all the entities added in the occ module.)
!
! GMSH_API int gmshModelOccAddPoint(const double x,
!                                   const double y,
!                                   const double z,
!                                   const double meshSize,
!                                   const int tag,
!                                   int *ierr);

INTERFACE
FUNCTION gmshModelOccAddPoint(x,y,z,meshSize,tag,ierr) RESULT( ans ) &
  & BIND(C, NAME="gmshModelOccAddPoint")
  IMPORT
  _R_V_IN_ :: x, y, z, meshSize
  _I_V_IN_ :: tag
  _I_OUT_ :: ierr
  INTEGER( I4B ) :: ans
END FUNCTION gmshModelOccAddPoint
END INTERFACE

PUBLIC :: gmshModelOccAddPoint

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Add a straight line segment in the OpenCASCADE CAD representation, between
! the two points with tags `startTag' and `endTag'. If `tag' is positive, set
! the tag explicitly; otherwise a new tag is selected automatically. Return
! the tag of the line.
!
! GMSH_API int gmshModelOccAddLine(const int startTag,
!                                  const int endTag,
!                                  const int tag,
!                                  int *ierr);

INTERFACE
FUNCTION gmshModelOccAddLine(startTag, endTag, tag, ierr) RESULT( ans ) &
  & BIND(C, NAME="gmshModelOccAddLine")
  IMPORT
  _I_V_IN_ :: startTag, endTag, tag
  _I_OUT_ :: ierr
  INTEGER( I4B ) :: ans
END FUNCTION gmshModelOccAddLine
END INTERFACE

PUBLIC :: gmshModelOccAddLine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Add a circle arc in the OpenCASCADE CAD representation, between the two
! points with tags `startTag' and `endTag', with center `centerTag'. If `tag'
! is positive, set the tag explicitly; otherwise a new tag is selected
! automatically. Return the tag of the circle arc.
!
! GMSH_API int gmshModelOccAddCircleArc(const int startTag,
!                                       const int centerTag,
!                                       const int endTag,
!                                       const int tag,
!                                       int *ierr);

INTERFACE
FUNCTION gmshModelOccAddCircleArc(startTag, centerTag, endTag, tag, ierr) &
  & RESULT( ans ) &
  & BIND(C, NAME="gmshModelOccAddCircleArc")
  IMPORT
  _I_V_IN_ :: startTag, centerTag, endTag, tag
  _I_OUT_ :: ierr
  INTEGER( I4B ) :: ans
END FUNCTION gmshModelOccAddCircleArc
END INTERFACE

PUBLIC :: gmshModelOccAddCircleArc

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Add a circle of center (`x', `y', `z') and radius `r' in the OpenCASCADE
! CAD representation. If `tag' is positive, set the tag explicitly; otherwise
! a new tag is selected automatically. If `angle1' and `angle2' are
! specified, create a circle arc between the two angles. Return the tag of
! the circle.
!
! GMSH_API int gmshModelOccAddCircle(const double x,
!                                    const double y,
!                                    const double z,
!                                    const double r,
!                                    const int tag,
!                                    const double angle1,
!                                    const double angle2,
!                                    int *ierr);

INTERFACE
FUNCTION gmshModelOccAddCircle(x, y, z, r, tag, angle1, angle2, ierr) &
  & RESULT( ans ) &
  & BIND(C, NAME="gmshModelOccAddCircle")
  IMPORT
  _I_V_IN_ :: tag
  _R_V_IN_ :: x, y, z, r, angle1, angle2
  _I_OUT_ :: ierr
  INTEGER( I4B ) :: ans
END FUNCTION gmshModelOccAddCircle
END INTERFACE

PUBLIC :: gmshModelOccAddCircle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Add an ellipse arc in the OpenCASCADE CAD representation, between the two
! points `startTag' and `endTag', and with center `centerTag' and major axis
! point `majorTag'. If `tag' is positive, set the tag explicitly; otherwise a
! new tag is selected automatically. Return the tag of the ellipse arc. Note
! that OpenCASCADE does not allow creating ellipse arcs with the major radius
! smaller than the minor radius.
!
! GMSH_API int gmshModelOccAddEllipseArc(const int startTag,
!                                        const int centerTag,
!                                        const int majorTag,
!                                        const int endTag,
!                                        const int tag,
!                                        int *ierr);

INTERFACE
FUNCTION gmshModelOccAddEllipseArc(startTag, centerTag, majorTag, endTag, &
  & tag, ierr) RESULT( ans ) &
  & BIND(C, NAME="gmshModelOccAddEllipseArc")
  IMPORT
  _I_V_IN_ :: tag, startTag, centerTag, majorTag, endTag
  _I_OUT_ :: ierr
  INTEGER( I4B ) :: ans
END FUNCTION gmshModelOccAddEllipseArc
END INTERFACE

PUBLIC :: gmshModelOccAddEllipseArc

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Add an ellipse of center (`x', `y', `z') and radii `r1' and `r2' along the
! x- and y-axes, respectively, in the OpenCASCADE CAD representation. If
! `tag' is positive, set the tag explicitly; otherwise a new tag is selected
! automatically. If `angle1' and `angle2' are specified, create an ellipse
! arc between the two angles. Return the tag of the ellipse. Note that
! OpenCASCADE does not allow creating ellipses with the major radius (along
! the x-axis) smaller than or equal to the minor radius (along the y-axis):
! rotate the shape or use `addCircle' in such cases.
!
! GMSH_API int gmshModelOccAddEllipse(const double x,
!                                     const double y,
!                                     const double z,
!                                     const double r1,
!                                     const double r2,
!                                     const int tag,
!                                     const double angle1,
!                                     const double angle2,
!                                     int *ierr);

INTERFACE
FUNCTION gmshModelOccAddEllipse(x, y, z, r1, r2, tag, angle1, angle2, ierr)&
  & RESULT( ans ) &
  & BIND(C, NAME="gmshModelOccAddEllipse")
  IMPORT
  _I_V_IN_ :: tag
  _R_V_IN_ :: x, y, z, r1, r2, angle1, angle2
  _I_OUT_ :: ierr
  INTEGER( I4B ) :: ans
END FUNCTION gmshModelOccAddEllipse
END INTERFACE

PUBLIC :: gmshModelOccAddEllipse

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Add a spline (C2 b-spline) curve in the OpenCASCADE CAD representation,
! going through the points `pointTags'. If `tag' is positive, set the tag
! explicitly; otherwise a new tag is selected automatically. Create a
! periodic curve if the first and last points are the same. Return the tag of
! the spline curve.
!
! GMSH_API int gmshModelOccAddSpline(int *pointTags, size_t pointTags_n,
!                                    const int tag,
!                                    int *ierr);

INTERFACE
FUNCTION gmshModelOccAddSpline(pointTags, pointTags_n, tag, ierr) &
  & RESULT( ans ) &
  & BIND(C, NAME="gmshModelOccAddSpline")
  IMPORT
  _ST_V_IN_ :: pointTags_n
  _I_IN_ :: pointTags( pointTags_n )
  _I_V_IN_ :: tag
  _I_OUT_ :: ierr
  INTEGER( I4B ) :: ans
END FUNCTION gmshModelOccAddSpline
END INTERFACE

PUBLIC :: gmshModelOccAddSpline

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Add a b-spline curve of degree `degree' in the OpenCASCADE CAD
! representation, with `pointTags' control points. If `weights', `knots' or
! `multiplicities' are not provided, default parameters are computed
! automatically. If `tag' is positive, set the tag explicitly; otherwise a
! new tag is selected automatically. Create a periodic curve if the first and
! last points are the same. Return the tag of the b-spline curve. */
!
! GMSH_API int gmshModelOccAddBSpline(int *pointTags, size_t pointTags_n,
!                                     const int tag,
!                                     const int degree,
!                                     double *weights, size_t weights_n,
!                                     double *knots, size_t knots_n,
!                                     int *multiplicities, size_t multiplicities_n,
!                                     int *ierr);

INTERFACE
FUNCTION gmshModelOccAddBSpline(pointTags, pointTags_n, tag, degree, &
  & weights, weights_n, knots, knots_n, multiplicities, multiplicities_n, &
  & ierr) RESULT( ans ) &
  & BIND(C, NAME="gmshModelOccAddBSpline")
  IMPORT
  _ST_V_IN_ :: pointTags_n, weights_n, multiplicities_n, knots_n
  _I_IN_ :: pointTags( pointTags_n ), multiplicities( multiplicities_n )
  _R_IN_ :: weights( weights_n ), knots( knots_n )
  _I_V_IN_ :: tag, degree
  _I_OUT_ :: ierr
  INTEGER( I4B ) :: ans
END FUNCTION gmshModelOccAddBSpline
END INTERFACE

PUBLIC :: gmshModelOccAddBSpline

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Add a Bezier curve in the OpenCASCADE CAD representation, with `pointTags'
! control points. If `tag' is positive, set the tag explicitly; otherwise a
! new tag is selected automatically. Return the tag of the Bezier curve. */
!
! GMSH_API int gmshModelOccAddBezier(int *pointTags, size_t pointTags_n,
!                                    const int tag,
!                                    int *ierr);

INTERFACE
FUNCTION gmshModelOccAddBezier(pointTags, pointTags_n, tag, ierr) &
  & RESULT( ans ) BIND(C, NAME="gmshModelOccAddBezier")
  IMPORT
  _ST_V_IN_ :: pointTags_n
  _I_IN_ :: pointTags( pointTags_n )
  _I_V_IN_ :: tag
  _I_OUT_ :: ierr
  INTEGER( I4B ) :: ans
END FUNCTION gmshModelOccAddBezier
END INTERFACE

PUBLIC :: gmshModelOccAddBezier

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Add a wire (open or closed) in the OpenCASCADE CAD representation, formed
! by the curves `curveTags'. Note that an OpenCASCADE wire can be made of
! curves that share geometrically identical (but topologically different)
! points. If `tag' is positive, set the tag explicitly; otherwise a new tag
! is selected automatically. Return the tag of the wire. */
!
! GMSH_API int gmshModelOccAddWire(int *curveTags, size_t curveTags_n,
!                                  const int tag,
!                                  const int checkClosed,
!                                  int *ierr);

INTERFACE
FUNCTION gmshModelOccAddWire(curveTags, curveTags_n, tag, checkClosed,ierr) &
  & RESULT( ans ) BIND(C, NAME="gmshModelOccAddWire")
  IMPORT
  _ST_V_IN_ :: curveTags_n
  _I_IN_ :: curveTags( curveTags_n )
  _I_V_IN_ :: tag, checkClosed
  _I_OUT_ :: ierr
  INTEGER( I4B ) :: ans
END FUNCTION gmshModelOccAddWire
END INTERFACE

PUBLIC :: gmshModelOccAddWire

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Add a curve loop (a closed wire) in the OpenCASCADE CAD representation,
! formed by the curves `curveTags'. `curveTags' should contain tags of curves
! forming a closed loop. Note that an OpenCASCADE curve loop can be made of
! curves that share geometrically identical (but topologically different)
! points. If `tag' is positive, set the tag explicitly; otherwise a new tag
! is selected automatically. Return the tag of the curve loop. */
!
! GMSH_API int gmshModelOccAddCurveLoop(int *curveTags, size_t curveTags_n,
!                                       const int tag,
!                                       int *ierr);

INTERFACE
FUNCTION gmshModelOccAddCurveLoop(curveTags, curveTags_n, tag, ierr) &
  & RESULT( ans ) BIND(C, NAME="gmshModelOccAddCurveLoop")
  IMPORT
  _ST_V_IN_ :: curveTags_n
  _I_IN_ :: curveTags( curveTags_n )
  _I_V_IN_ :: tag
  _I_OUT_ :: ierr
  INTEGER( I4B ) :: ans
END FUNCTION gmshModelOccAddCurveLoop
END INTERFACE

PUBLIC :: gmshModelOccAddCurveLoop

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Add a rectangle in the OpenCASCADE CAD representation, with lower left
! corner at (`x', `y', `z') and upper right corner at (`x' + `dx', `y' +
! `dy', `z'). If `tag' is positive, set the tag explicitly; otherwise a new
! tag is selected automatically. Round the corners if `roundedRadius' is
! nonzero. Return the tag of the rectangle.
!
! GMSH_API int gmshModelOccAddRectangle(const double x,
!                                       const double y,
!                                       const double z,
!                                       const double dx,
!                                       const double dy,
!                                       const int tag,
!                                       const double roundedRadius,
!                                       int *ierr);

INTERFACE
FUNCTION gmshModelOccAddRectangle(x, y, z, dx, dy, tag, roundedRadius, &
  & ierr) RESULT( ans ) BIND(C, NAME="gmshModelOccAddRectangle")
  IMPORT
  _R_V_IN_ :: x, y, z, dx, dy, roundedRadius
  _I_V_IN_ :: tag
  _I_OUT_ :: ierr
  INTEGER( I4B ) :: ans
END FUNCTION gmshModelOccAddRectangle
END INTERFACE

PUBLIC :: gmshModelOccAddRectangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Add a disk in the OpenCASCADE CAD representation, with center (`xc', `yc',
! `zc') and radius `rx' along the x-axis and `ry' along the y-axis. If `tag'
! is positive, set the tag explicitly; otherwise a new tag is selected
! automatically. Return the tag of the disk.
!
! GMSH_API int gmshModelOccAddDisk(const double xc,
!                                  const double yc,
!                                  const double zc,
!                                  const double rx,
!                                  const double ry,
!                                  const int tag,
!                                  int *ierr);

INTERFACE
FUNCTION gmshModelOccAddDisk(xc, yc, zc, rx, ry, tag, &
  & ierr) RESULT( ans ) BIND(C, NAME="gmshModelOccAddDisk")
  IMPORT
  _R_V_IN_ :: xc, yc, zc, rx, ry
  _I_V_IN_ :: tag
  _I_OUT_ :: ierr
  INTEGER( I4B ) :: ans
END FUNCTION gmshModelOccAddDisk
END INTERFACE

PUBLIC :: gmshModelOccAddDisk

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Add a plane surface in the OpenCASCADE CAD representation, defined by one
! or more curve loops (or closed wires) `wireTags'. The first curve loop
! defines the exterior contour; additional curve loop define holes. If `tag'
! is positive, set the tag explicitly; otherwise a new tag is selected
! automatically. Return the tag of the surface.
!
! GMSH_API int gmshModelOccAddPlaneSurface(int *wireTags, size_t wireTags_n,
!                                          const int tag,
!                                          int *ierr);

INTERFACE
FUNCTION gmshModelOccAddPlaneSurface(wireTags, wireTags_n, tag, &
  & ierr) RESULT( ans ) BIND(C, NAME="gmshModelOccAddPlaneSurface")
  IMPORT
  _ST_V_IN_ :: wireTags_n
  _I_IN_ :: wireTags( wireTags_n )
  _I_V_IN_ :: tag
  _I_OUT_ :: ierr
  INTEGER( I4B ) :: ans
END FUNCTION gmshModelOccAddPlaneSurface
END INTERFACE

PUBLIC :: gmshModelOccAddPlaneSurface

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Add a surface in the OpenCASCADE CAD representation, filling the curve loop
! `wireTag'. If `tag' is positive, set the tag explicitly; otherwise a new
! tag is selected automatically. Return the tag of the surface. If
! `pointTags' are provided, force the surface to pass through the given
! points.
!
! GMSH_API int gmshModelOccAddSurfaceFilling(const int wireTag,
!                                            const int tag,
!                                            int *pointTags, size_t pointTags_n,
!                                            int *ierr);

INTERFACE
FUNCTION gmshModelOccAddSurfaceFilling(wireTag, tag, pointTags, &
  & pointTags_n, ierr) RESULT( ans ) &
  & BIND(C, NAME="gmshModelOccAddSurfaceFilling")
  IMPORT
  _I_V_IN_ :: tag, wireTag
  _ST_V_IN_ :: pointTags_n
  _I_IN_ :: pointTags( pointTags_n )
  _I_OUT_ :: ierr
  INTEGER( I4B ) :: ans
END FUNCTION gmshModelOccAddSurfaceFilling
END INTERFACE

PUBLIC :: gmshModelOccAddSurfaceFilling

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Add a BSpline surface in the OpenCASCADE CAD representation, filling the
! curve loop `wireTag'. The curve loop should be made of 2, 3 or 4 BSpline
! curves. The optional `type' argument specifies the type of filling:
! "Stretch" creates the flattest patch, "Curved" (the default) creates the
! most rounded patch, and "Coons" creates a rounded patch with less depth
! than "Curved". If `tag' is positive, set the tag explicitly; otherwise a
! new tag is selected automatically. Return the tag of the surface.
!
! GMSH_API int gmshModelOccAddBSplineFilling(const int wireTag,
!                                            const int tag,
!                                            const char *type,
!                                            int *ierr);

INTERFACE
FUNCTION gmshModelOccAddBSplineFilling(wireTag, tag, typeOfFilling, ierr ) &
  & RESULT( ans ) &
  & BIND(C, NAME="gmshModelOccAddBSplineFilling")
  IMPORT
  _I_V_IN_ :: tag, wireTag
  _CPTR_V_IN_ :: typeOfFilling
  _I_OUT_ :: ierr
  INTEGER( I4B ) :: ans
END FUNCTION gmshModelOccAddBSplineFilling
END INTERFACE

PUBLIC :: gmshModelOccAddBSplineFilling

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Add a Bezier surface in the OpenCASCADE CAD representation, filling the
! curve loop `wireTag'. The curve loop should be made of 2, 3 or 4 Bezier
! curves. The optional `type' argument specifies the type of filling:
! "Stretch" creates the flattest patch, "Curved" (the default) creates the
! most rounded patch, and "Coons" creates a rounded patch with less depth
! than "Curved". If `tag' is positive, set the tag explicitly; otherwise a
! new tag is selected automatically. Return the tag of the surface.
!
! GMSH_API int gmshModelOccAddBezierFilling(const int wireTag,
!                                           const int tag,
!                                           const char *type,
!                                           int *ierr);

INTERFACE
FUNCTION gmshModelOccAddBezierFilling(wireTag, tag, typeOfFilling, ierr ) &
  & RESULT( ans ) &
  & BIND(C, NAME="gmshModelOccAddBezierFilling")
  IMPORT
  _I_V_IN_ :: tag, wireTag
  _CPTR_V_IN_ :: typeOfFilling
  _I_OUT_ :: ierr
  INTEGER( I4B ) :: ans
END FUNCTION gmshModelOccAddBezierFilling
END INTERFACE

PUBLIC :: gmshModelOccAddBezierFilling

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Add a b-spline surface of degree `degreeU' x `degreeV' in the OpenCASCADE
! CAD representation, with `pointTags' control points given as a single
! vector [Pu1v1, ... Pu`numPointsU'v1, Pu1v2, ...]. If `weights', `knotsU',
! `knotsV', `multiplicitiesU' or `multiplicitiesV' are not provided, default
! parameters are computed automatically. If `tag' is positive, set the tag
! explicitly; otherwise a new tag is selected automatically. If `wireTags' is
! provided, trim the b-spline patch using the provided wires: the first wire
! defines the external contour, the others define holes. If `wire3D' is set,
! consider wire curves as 3D curves and project them on the b-spline surface;
! otherwise consider the wire curves as defined in the parametric space of
! the surface. Return the tag of the b-spline surface.
!
! GMSH_API int gmshModelOccAddBSplineSurface(int *pointTags, size_t pointTags_n,
!                                            const int numPointsU,
!                                            const int tag,
!                                            const int degreeU,
!                                            const int degreeV,
!                                            double *weights, size_t weights_n,
!                                            double *knotsU, size_t knotsU_n,
!                                            double *knotsV, size_t knotsV_n,
!                                            int *multiplicitiesU, size_t multiplicitiesU_n,
!                                            int *multiplicitiesV, size_t multiplicitiesV_n,
!                                            int *wireTags, size_t wireTags_n,
!                                            const int wire3D,
!                                            int *ierr);

INTERFACE
FUNCTION gmshModelOccAddBSplineSurface( pointTags, pointTags_n, numPointsU, &
  & tag, degreeU, degreeV, weights, weights_n, knotsU, knotsU_n, &
  & knotsV, knotsV_n, multiplicitiesU, multiplicitiesU_n, &
  & multiplicitiesV, multiplicitiesV_n, wireTags, wireTags_n, wire3D, ierr ) &
  & RESULT( ans ) &
  & BIND(C, NAME="gmshModelOccAddBSplineSurface")
  IMPORT
  _ST_V_IN_ :: pointTags_n, weights_n, knotsU_n, knotsV_n, multiplicitiesU_n
  _ST_V_IN_ :: multiplicitiesV_n, wireTags_n
  _I_IN_ :: pointTags(pointTags_n)
  _I_IN_ :: multiplicitiesU(multiplicitiesU_n)
  _I_IN_ :: multiplicitiesV(multiplicitiesV_n)
  _I_IN_ :: wireTags(wireTags_n)
  _I_V_IN_ :: tag, numPointsU, degreeU, degreeV, wire3D
  _R_IN_ :: weights( weights_n ), knotsU( knotsU_n ), knotsV( knotsV_n )
  _I_OUT_ :: ierr
  INTEGER( I4B ) :: ans
END FUNCTION gmshModelOccAddBSplineSurface
END INTERFACE

PUBLIC :: gmshModelOccAddBSplineSurface

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Add a Bezier surface in the OpenCASCADE CAD representation, with
! `pointTags' control points given as a single vector [Pu1v1, ...
! Pu`numPointsU'v1, Pu1v2, ...]. If `tag' is positive, set the tag
! explicitly; otherwise a new tag is selected automatically. If `wireTags' is
! provided, trim the Bezier patch using the provided wires: the first wire
! defines the external contour, the others define holes. If `wire3D' is set,
! consider wire curves as 3D curves and project them on the Bezier surface;
! otherwise consider the wire curves as defined in the parametric space of
! the surface. Return the tag of the Bezier surface.
!
! GMSH_API int gmshModelOccAddBezierSurface(int *pointTags, size_t pointTags_n,
!                                           const int numPointsU,
!                                           const int tag,
!                                           int *wireTags, size_t wireTags_n,
!                                           const int wire3D,
!                                           int *ierr);

INTERFACE
FUNCTION gmshModelOccAddBezierSurface(pointTags, pointTags_n, numPointsU,&
  & tag, wireTags, wireTags_n, wire3D, ierr ) &
  & RESULT( ans ) &
  & BIND(C, NAME="gmshModelOccAddBezierSurface")
  IMPORT
  _ST_V_IN_ :: pointTags_n, wireTags_n
  _I_IN_ :: pointTags(pointTags_n), wireTags( wireTags_n )
  _I_V_IN_ :: tag, numPointsU, wire3D
  _I_OUT_ :: ierr
  INTEGER( I4B ) :: ans
END FUNCTION gmshModelOccAddBezierSurface
END INTERFACE

PUBLIC :: gmshModelOccAddBezierSurface

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Trim the surface `surfaceTag' with the wires `wireTags', replacing any
! existing trimming curves. The first wire defines the external contour, the
! others define holes. If `wire3D' is set, consider wire curves as 3D curves
! and project them on the surface; otherwise consider the wire curves as
! defined in the parametric space of the surface. If `tag' is positive, set
! the tag explicitly; otherwise a new tag is selected automatically. Return
! the tag of the trimmed surface.
!
! GMSH_API int gmshModelOccAddTrimmedSurface(const int surfaceTag,
!                                            int *wireTags, size_t wireTags_n,
!                                            const int wire3D,
!                                            const int tag,
!                                            int *ierr);

INTERFACE
FUNCTION gmshModelOccAddTrimmedSurface(surfaceTag, wireTags, wireTags_n,&
  & wire3D, tag, ierr) RESULT( ans ) &
  & BIND(C, NAME="gmshModelOccAddTrimmedSurface")
  IMPORT
  _ST_V_IN_ :: wireTags_n
  _I_IN_ :: wireTags( wireTags_n )
  _I_V_IN_ :: surfaceTag, tag, wire3D
  _I_OUT_ :: ierr
  INTEGER( I4B ) :: ans
END FUNCTION gmshModelOccAddTrimmedSurface
END INTERFACE

PUBLIC :: gmshModelOccAddTrimmedSurface

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Add a surface loop (a closed shell) in the OpenCASCADE CAD representation,
! formed by `surfaceTags`.  If `tag` is positive, set the tag explicitly;
! otherwise a new tag is selected automatically. Return the tag of the
! surface loop. Setting `sewing` allows to build a shell made of  cxvcvb  surfaces that share geometrically identical (but topologically different) curves.
!
! GMSH_API int gmshModelOccAddSurfaceLoop(int *surfaceTags, size_t surfaceTags_n,
!                                         const int tag,
!                                         const int sewing,
!                                         int *ierr);

INTERFACE
FUNCTION gmshModelOccAddSurfaceLoop(surfaceTags, surfaceTags_n, tag, sewing,&
  & ierr) RESULT( ans ) &
  & BIND(C, NAME="gmshModelOccAddSurfaceLoop")
  IMPORT
  _ST_V_IN_ :: surfaceTags_n
  _I_IN_ :: surfaceTags( surfaceTags_n )
  _I_V_IN_ :: tag, sewing
  _I_OUT_ :: ierr
  INTEGER( I4B ) :: ans
END FUNCTION gmshModelOccAddSurfaceLoop
END INTERFACE

PUBLIC :: gmshModelOccAddSurfaceLoop

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Add a volume (a region) in the OpenCASCADE CAD representation, defined by
! one or more surface loops `shellTags'. The first surface loop defines the
! exterior boundary; additional surface loop define holes. If `tag' is
! positive, set the tag explicitly; otherwise a new tag is selected
! automatically. Return the tag of the volume.
!
! GMSH_API int gmshModelOccAddVolume(int *shellTags, size_t shellTags_n,
!                                    const int tag,
!                                    int *ierr);

INTERFACE
FUNCTION gmshModelOccAddVolume(shellTags, shellTags_n, tag, ierr) &
  & RESULT( ans ) &
  & BIND(C, NAME="gmshModelOccAddVolume")
  IMPORT
  _ST_V_IN_ :: shellTags_n
  _I_IN_ :: shellTags( shellTags_n )
  _I_V_IN_ :: tag
  _I_OUT_ :: ierr
  INTEGER( I4B ) :: ans
END FUNCTION gmshModelOccAddVolume
END INTERFACE

PUBLIC :: gmshModelOccAddVolume

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Add a sphere of center (`xc', `yc', `zc') and radius `r' in the OpenCASCADE
! CAD representation. The optional `angle1' and `angle2' arguments define the
! polar angle opening (from -Pi/2 to Pi/2). The optional `angle3' argument
! defines the azimuthal opening (from 0 to 2*Pi). If `tag' is positive, set
! the tag explicitly; otherwise a new tag is selected automatically. Return
! the tag of the sphere.
!
! GMSH_API int gmshModelOccAddSphere(const double xc,
!                                    const double yc,
!                                    const double zc,
!                                    const double radius,
!                                    const int tag,
!                                    const double angle1,
!                                    const double angle2,
!                                    const double angle3,
!                                    int *ierr);

INTERFACE
FUNCTION gmshModelOccAddSphere(xc, yc, zc, radius, tag, angle1, angle2, &
  & angle3, ierr ) &
  & RESULT( ans ) &
  & BIND(C, NAME="gmshModelOccAddSphere")
  IMPORT
  _I_V_IN_ :: tag
  _R_V_IN_ :: xc, yc, zc, radius, angle1, angle2, angle3
  _I_OUT_ :: ierr
  INTEGER( I4B ) :: ans
END FUNCTION gmshModelOccAddSphere
END INTERFACE

PUBLIC :: gmshModelOccAddSphere

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Add a parallelepipedic box in the OpenCASCADE CAD representation, defined
! by a point (`x', `y', `z') and the extents along the x-, y- and z-axes. If
! `tag' is positive, set the tag explicitly; otherwise a new tag is selected
! automatically. Return the tag of the box.
!
! GMSH_API int gmshModelOccAddBox(const double x,
!                                 const double y,
!                                 const double z,
!                                 const double dx,
!                                 const double dy,
!                                 const double dz,
!                                 const int tag,
!                                 int *ierr);

INTERFACE
FUNCTION gmshModelOccAddBox(x, y, z, dx, dy, dz, tag, ierr) &
  & RESULT( ans ) &
  & BIND(C, NAME="gmshModelOccAddBox")
  IMPORT
  _I_V_IN_ :: tag
  _R_V_IN_ :: x, y, z, dx, dy, dz
  _I_OUT_ :: ierr
  INTEGER( I4B ) :: ans
END FUNCTION gmshModelOccAddBox
END INTERFACE

PUBLIC :: gmshModelOccAddBox

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Add a cylinder in the OpenCASCADE CAD representation, defined by the center
! (`x', `y', `z') of its first circular face, the 3 components (`dx', `dy',
! `dz') of the vector defining its axis and its radius `r'. The optional
! `angle' argument defines the angular opening (from 0 to 2*Pi). If `tag' is
! positive, set the tag explicitly; otherwise a new tag is selected
! automatically. Return the tag of the cylinder.
!
! GMSH_API int gmshModelOccAddCylinder(const double x,
!                                      const double y,
!                                      const double z,
!                                      const double dx,
!                                      const double dy,
!                                      const double dz,
!                                      const double r,
!                                      const int tag,
!                                      const double angle,
!                                      int *ierr);

INTERFACE
FUNCTION gmshModelOccAddCylinder(x, y, z, dx, dy, dz, r, tag, angle, ierr) &
  & RESULT( ans ) &
  & BIND(C, NAME="gmshModelOccAddCylinder")
  IMPORT
  _I_V_IN_ :: tag
  _R_V_IN_ :: x, y, z, dx, dy, dz, r, angle
  _I_OUT_ :: ierr
  INTEGER( I4B ) :: ans
END FUNCTION gmshModelOccAddCylinder
END INTERFACE

PUBLIC :: gmshModelOccAddCylinder

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Add a cone in the OpenCASCADE CAD representation, defined by the center
! (`x', `y', `z') of its first circular face, the 3 components of the vector
! (`dx', `dy', `dz') defining its axis and the two radii `r1' and `r2' of the
! faces (these radii can be zero). If `tag' is positive, set the tag
! explicitly; otherwise a new tag is selected automatically. `angle' defines
! the optional angular opening (from 0 to 2*Pi). Return the tag of the cone.
!
! GMSH_API int gmshModelOccAddCone(const double x,
!                                  const double y,
!                                  const double z,
!                                  const double dx,
!                                  const double dy,
!                                  const double dz,
!                                  const double r1,
!                                  const double r2,
!                                  const int tag,
!                                  const double angle,
!                                  int *ierr);

INTERFACE
FUNCTION gmshModelOccAddCone(x, y, z, dx, dy, dz, r1, r2, tag, angle, ierr) &
  & RESULT( ans ) &
  & BIND(C, NAME="gmshModelOccAddCone")
  IMPORT
  _I_V_IN_ :: tag
  _R_V_IN_ :: x, y, z, dx, dy, dz, r1, r2, angle
  _I_OUT_ :: ierr
  INTEGER( I4B ) :: ans
END FUNCTION gmshModelOccAddCone
END INTERFACE

PUBLIC :: gmshModelOccAddCone

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Add a right angular wedge in the OpenCASCADE CAD representation, defined by
! the right-angle point (`x', `y', `z') and the 3 extends along the x-, y-
! and z-axes (`dx', `dy', `dz'). If `tag' is positive, set the tag
! explicitly; otherwise a new tag is selected automatically. The optional
! argument `ltx' defines the top extent along the x-axis. Return the tag of
! the wedge.
!
! GMSH_API int gmshModelOccAddWedge(const double x,
!                                   const double y,
!                                   const double z,
!                                   const double dx,
!                                   const double dy,
!                                   const double dz,
!                                   const int tag,
!                                   const double ltx,
!                                   int *ierr);

INTERFACE
FUNCTION gmshModelOccAddWedge(x, y, z, dx, dy, dz, tag, ltx, ierr) &
  & RESULT( ans ) &
  & BIND(C, NAME="gmshModelOccAddWedge")
  IMPORT
  _I_V_IN_ :: tag
  _R_V_IN_ :: x, y, z, dx, dy, dz, ltx
  _I_OUT_ :: ierr
  INTEGER( I4B ) :: ans
END FUNCTION gmshModelOccAddWedge
END INTERFACE

PUBLIC :: gmshModelOccAddWedge

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Add a torus in the OpenCASCADE CAD representation, defined by its center
! (`x', `y', `z') and its 2 radii `r' and `r2'. If `tag' is positive, set the
! tag explicitly; otherwise a new tag is selected automatically. The optional
! argument `angle' defines the angular opening (from 0 to 2*Pi). Return the
! tag of the wedge.
!
! GMSH_API int gmshModelOccAddTorus(const double x,
!                                   const double y,
!                                   const double z,
!                                   const double r1,
!                                   const double r2,
!                                   const int tag,
!                                   const double angle,
!                                   int *ierr);

INTERFACE
FUNCTION gmshModelOccAddTorus(x, y, z, r1, r2, tag, angle, ierr) &
  & RESULT( ans ) &
  & BIND(C, NAME="gmshModelOccAddTorus")
  IMPORT
  _I_V_IN_ :: tag
  _R_V_IN_ :: x, y, z, r1, r2, angle
  _I_OUT_ :: ierr
  INTEGER( I4B ) :: ans
END FUNCTION gmshModelOccAddTorus
END INTERFACE

PUBLIC :: gmshModelOccAddTorus

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Add a volume (if the optional argument `makeSolid' is set) or surfaces in
! the OpenCASCADE CAD representation, defined through the open or closed
! wires `wireTags'. If `tag' is positive, set the tag explicitly; otherwise a
! new tag is selected automatically. The new entities are returned in
! `outDimTags'. If the optional argument `makeRuled' is set, the surfaces
! created on the boundary are forced to be ruled surfaces. If `maxDegree' is
! positive, set the maximal degree of resulting surface.
!
! GMSH_API void gmshModelOccAddThruSections(int *wireTags, size_t wireTags_n,
!                                           int **outDimTags, size_t *outDimTags_n,
!                                           const int tag,
!                                           const int makeSolid,
!                                           const int makeRuled,
!                                           const int maxDegree,
!                                           int *ierr);

INTERFACE
SUBROUTINE gmshModelOccAddThruSections(wireTags, wireTags_n, outDimTags, &
  & outDimTags_n, tag, makeSolid, makeRuled, maxDegree, ierr) &
  & BIND(C, NAME="gmshModelOccAddThruSections")
  IMPORT
  _ST_V_IN_ :: wireTags_n
  _ST_OUT_ :: outDimTags_n
  _I_IN_ :: wireTags( wireTags_n )
  _CPTR_IN_ :: outDimTags
  _I_V_IN_ :: tag, makeSolid, makeRuled, maxDegree
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelOccAddThruSections
END INTERFACE

PUBLIC :: gmshModelOccAddThruSections

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Add a hollowed volume in the OpenCASCADE CAD representation, built from an
! initial volume `volumeTag' and a set of faces from this volume
! `excludeSurfaceTags', which are to be removed. The remaining faces of the
! volume become the walls of the hollowed solid, with thickness `offset'. If
! `tag' is positive, set the tag explicitly; otherwise a new tag is selected
! automatically.
!
! GMSH_API void gmshModelOccAddThickSolid(const int volumeTag,
!                                         int *excludeSurfaceTags, size_t excludeSurfaceTags_n,
!                                         const double offset,
!                                         int **outDimTags, size_t *outDimTags_n,
!                                         const int tag,
!                                         int *ierr);

INTERFACE
SUBROUTINE gmshModelOccAddThickSolid(volumeTag, excludeSurfaceTags, &
  & excludeSurfaceTags_n, offset, outDimTags, outDimTags_n, tag, ierr) &
  & BIND(C, NAME="gmshModelOccAddThickSolid")
  IMPORT
  _ST_V_IN_ :: excludeSurfaceTags_n
  _ST_OUT_ :: outDimTags_n
  _I_IN_ :: excludeSurfaceTags( excludeSurfaceTags_n )
  _CPTR_IN_ :: outDimTags
  _I_V_IN_ :: volumeTag, tag
  _R_V_IN_ ::offset
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelOccAddThickSolid
END INTERFACE

PUBLIC :: gmshModelOccAddThickSolid

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Extrude the entities `dimTags' in the OpenCASCADE CAD representation, using
! a translation along (`dx', `dy', `dz'). Return extruded entities in
! `outDimTags'. If `numElements' is not empty, also extrude the mesh: the
! entries in `numElements' give the number of elements in each layer. If
! `height' is not empty, it provides the (cumulative) height of the different
! layers, normalized to 1. If `recombine' is set, recombine the mesh in the
! layers.
!
! GMSH_API void gmshModelOccExtrude(int *dimTags, size_t dimTags_n,
!                                   const double dx,
!                                   const double dy,
!                                   const double dz,
!                                   int **outDimTags, size_t *outDimTags_n,
!                                   int *numElements, size_t numElements_n,
!                                   double *heights, size_t heights_n,
!                                   const int recombine,
!                                   int *ierr);

INTERFACE
SUBROUTINE gmshModelOccExtrude(dimTags, dimTags_n, dx, dy, dz, outDimTags, &
  & outDimTags_n, numElements, numElements_n, heights, heights_n, &
  & recombine, ierr) &
  & BIND(C, NAME="gmshModelOccExtrude")
  IMPORT
  _ST_V_IN_ :: dimTags_n, heights_n, numElements_n
  _ST_OUT_ :: outDimTags_n
  _I_IN_ :: dimTags( dimTags_n ), numElements(numElements_n)
  _CPTR_IN_ :: outDimTags
  _I_V_IN_ :: recombine
  _R_V_IN_ :: dx, dy, dz
  _R_IN_ :: heights(heights_n)
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelOccExtrude
END INTERFACE

PUBLIC :: gmshModelOccExtrude

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Extrude the entities `dimTags' in the OpenCASCADE CAD representation, using
! a rotation of `angle' radians around the axis of revolution defined by the
! point (`x', `y', `z') and the direction (`ax', `ay', `az'). Return extruded
! entities in `outDimTags'. If `numElements' is not empty, also extrude the
! mesh: the entries in `numElements' give the number of elements in each
! layer. If `height' is not empty, it provides the (cumulative) height of the
! different layers, normalized to 1. When the mesh is extruded the angle
! should be strictly smaller than 2*Pi. If `recombine' is set, recombine the
! mesh in the layers.
!
! GMSH_API void gmshModelOccRevolve(int *dimTags, size_t dimTags_n,
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
SUBROUTINE gmshModelOccRevolve(dimTags, dimTags_n, x, y, z, ax, ay, az, &
  & angle, outDimTags, outDimTags_n, numElements, numElements_n, &
  & heights, heights_n, recombine, ierr) &
  & BIND(C, NAME="gmshModelOccRevolve")
  IMPORT
  _ST_V_IN_ :: dimTags_n, heights_n, numElements_n
  _ST_OUT_ :: outDimTags_n
  _I_IN_ :: dimTags( dimTags_n ), numElements(numElements_n)
  _CPTR_IN_ :: outDimTags
  _I_V_IN_ :: recombine
  _R_V_IN_ :: x, y, z, ax, ay, az, angle
  _R_IN_ :: heights(heights_n)
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelOccRevolve
END INTERFACE

PUBLIC :: gmshModelOccRevolve

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Add a pipe in the OpenCASCADE CAD representation, by extruding the entities
! `dimTags' along the wire `wireTag'. The type of sweep can be specified with
! `trihedron' (possible values: "DiscreteTrihedron", "CorrectedFrenet",
! "Fixed", "Frenet", "ConstantNormal", "Darboux", "GuideAC", "GuidePlan",
! "GuideACWithContact", "GuidePlanWithContact"). If `trihedron' is not
! provided, "DiscreteTrihedron" is assumed. Return the pipe in `outDimTags'.
!
! GMSH_API void gmshModelOccAddPipe(int *dimTags, size_t dimTags_n,
!                                   const int wireTag,
!                                   int **outDimTags, size_t *outDimTags_n,
!                                   const char *trihedron,
!                                   int *ierr);

INTERFACE
SUBROUTINE gmshModelOccAddPipe(dimTags, dimTags_n, wireTag, outDimTags, &
  & outDimTags_n, trihedron, ierr) &
  & BIND(C, NAME="gmshModelOccAddPipe")
  IMPORT
  _ST_V_IN_ :: dimTags_n
  _I_IN_ :: dimTags( dimTags_n )
  _I_V_IN_ :: wireTag
  _CPTR_IN_ :: outDimTags
  _ST_OUT_ :: outDimTags_n
  _CPTR_V_IN_ :: trihedron
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelOccAddPipe
END INTERFACE

PUBLIC :: gmshModelOccAddPipe

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Fillet the volumes `volumeTags' on the curves `curveTags' with radii
! `radii'. The `radii' vector can either contain a single radius, as many
! radii as `curveTags', or twice as many as `curveTags' (in which case
! different radii are provided for the begin and end points of the curves).
! Return the filleted entities in `outDimTags'. Remove the original volume if
! `removeVolume' is set.
!
! GMSH_API void gmshModelOccFillet(int *volumeTags, size_t volumeTags_n,
!                                  int *curveTags, size_t curveTags_n,
!                                  double *radii, size_t radii_n,
!                                  int **outDimTags, size_t *outDimTags_n,
!                                  const int removeVolume,
!                                  int *ierr);

INTERFACE
SUBROUTINE gmshModelOccFillet(volumeTags, volumeTags_n, curveTags, &
  & curveTags_n, radii, radii_n, outDimTags, outDimTags_n, removeVolume,&
  & ierr) &
  & BIND(C, NAME="gmshModelOccFillet")
  IMPORT
  _ST_V_IN_ :: volumeTags_n, curveTags_n, radii_n
  _I_IN_ :: volumeTags( volumeTags_n )
  _I_IN_ :: curveTags( curveTags_n )
  _R_IN_ :: radii(radii_n)
  _CPTR_IN_ :: outDimTags
  _ST_OUT_ :: outDimTags_n
  _I_V_IN_ :: removeVolume
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelOccFillet
END INTERFACE

PUBLIC :: gmshModelOccFillet

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Chamfer the volumes `volumeTags' on the curves `curveTags' with distances
! `distances' measured on surfaces `surfaceTags'. The `distances' vector can
! either contain a single distance, as many distances as `curveTags' and
! `surfaceTags', or twice as many as `curveTags' and `surfaceTags' (in which
! case the first in each pair is measured on the corresponding surface in
! `surfaceTags', the other on the other adjacent surface). Return the
! chamfered entities in `outDimTags'. Remove the original volume if
! `removeVolume' is set.
!
! GMSH_API void gmshModelOccChamfer(int *volumeTags, size_t volumeTags_n,
!                                   int *curveTags, size_t curveTags_n,
!                                   int *surfaceTags, size_t surfaceTags_n,
!                                   double *distances, size_t distances_n,
!                                   int **outDimTags, size_t *outDimTags_n,
!                                   const int removeVolume,
!                                   int *ierr);

INTERFACE
SUBROUTINE gmshModelOccChamfer(volumeTags, volumeTags_n, curveTags, &
  & curveTags_n, surfaceTags, surfaceTags_n, distances, distances_n, &
  & outDimTags, outDimTags_n, removeVolume,&
  & ierr) &
  & BIND(C, NAME="gmshModelOccChamfer")
  IMPORT
  _ST_V_IN_ :: volumeTags_n
  _I_IN_ :: volumeTags( volumeTags_n )
  _ST_V_IN_ :: curveTags_n
  _I_IN_ :: curveTags( curveTags_n )
  _ST_V_IN_ :: surfaceTags_n
  _I_IN_ :: surfaceTags( surfaceTags_n )
  _ST_V_IN_ :: distances_n
  _R_IN_ :: distances(distances_n)
  _CPTR_IN_ :: outDimTags
  _ST_OUT_ :: outDimTags_n
  _I_V_IN_ :: removeVolume
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelOccChamfer
END INTERFACE

PUBLIC :: gmshModelOccChamfer

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Compute the boolean union (the fusion) of the entities `objectDimTags' and
! `toolDimTags' in the OpenCASCADE CAD representation. Return the resulting
! entities in `outDimTags'. If `tag' is positive, try to set the tag
! explicitly (only valid if the boolean operation results in a single
! entity). Remove the object if `removeObject' is set. Remove the tool if
! `removeTool' is set.
!
! GMSH_API void gmshModelOccFuse(int *objectDimTags, size_t objectDimTags_n,
!                                int *toolDimTags, size_t toolDimTags_n,
!                                int **outDimTags, size_t *outDimTags_n,
!                                int ***outDimTagsMap, size_t **outDimTagsMap_n, size_t *outDimTagsMap_nn,
!                                const int tag,
!                                const int removeObject,
!                                const int removeTool,
!                                int *ierr);

INTERFACE
SUBROUTINE gmshModelOccFuse(objectDimTags, objectDimTags_n, toolDimTags, &
  & toolDimTags_n, outDimTags, outDimTags_n, outDimTagsMap, &
  & outDimTagsMap_n, outDimTagsMap_nn, tag, removeObject, removeTool, &
  & ierr) &
  & BIND(C, NAME="gmshModelOccFuse")
  IMPORT
  _ST_V_IN_ :: objectDimTags_n
  _I_IN_ :: objectDimTags(objectDimTags_n)
  _ST_V_IN_ :: toolDimTags_n
  _I_IN_ :: toolDimTags(toolDimTags_n)
  _CPTR_IN_ :: outDimTags
  _I_OUT_ :: outDimTags_n
  _CPTR_IN_ :: outDimTagsMap
  _CPTR_IN_ :: outDimTagsMap_n
  _ST_OUT_ :: outDimTagsMap_nn
  _I_V_IN_ :: tag
  _I_V_IN_ :: removeObject
  _I_V_IN_ :: removeTool
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelOccFuse
END INTERFACE

PUBLIC :: gmshModelOccFuse

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Compute the boolean intersection (the common parts) of the entities
! `objectDimTags' and `toolDimTags' in the OpenCASCADE CAD representation.
! Return the resulting entities in `outDimTags'. If `tag' is positive, try to
! set the tag explicitly (only valid if the boolean operation results in a
! single entity). Remove the object if `removeObject' is set. Remove the tool
! if `removeTool' is set.
!
! GMSH_API void gmshModelOccIntersect(int *objectDimTags, size_t objectDimTags_n,
!                                     int *toolDimTags, size_t toolDimTags_n,
!                                     int **outDimTags, size_t *outDimTags_n,
!                                     int ***outDimTagsMap, size_t **outDimTagsMap_n, size_t *outDimTagsMap_nn,
!                                     const int tag,
!                                     const int removeObject,
!                                     const int removeTool,
!                                     int *ierr);

INTERFACE
SUBROUTINE gmshModelOccIntersect(objectDimTags, objectDimTags_n, &
  & toolDimTags, toolDimTags_n, outDimTags, outDimTags_n, outDimTagsMap, &
  & outDimTagsMap_n, outDimTagsMap_nn, tag, removeObject, removeTool, &
  & ierr) &
  & BIND(C, NAME="gmshModelOccIntersect")
  IMPORT
  _ST_V_IN_ :: objectDimTags_n
  _I_IN_ :: objectDimTags(objectDimTags_n)
  _ST_V_IN_ :: toolDimTags_n
  _I_IN_ :: toolDimTags(toolDimTags_n)
  _CPTR_IN_ :: outDimTags
  _I_OUT_ :: outDimTags_n
  _CPTR_IN_ :: outDimTagsMap
  _CPTR_IN_ :: outDimTagsMap_n
  _ST_OUT_ :: outDimTagsMap_nn
  _I_V_IN_ :: tag
  _I_V_IN_ :: removeObject
  _I_V_IN_ :: removeTool
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelOccIntersect
END INTERFACE

PUBLIC :: gmshModelOccIntersect

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Compute the boolean difference between the entities `objectDimTags' and
! `toolDimTags' in the OpenCASCADE CAD representation. Return the resulting
! entities in `outDimTags'. If `tag' is positive, try to set the tag
! explicitly (only valid if the boolean operation results in a single
! entity). Remove the object if `removeObject' is set. Remove the tool if
! `removeTool' is set.
!
! GMSH_API void gmshModelOccCut(int *objectDimTags, size_t objectDimTags_n,
!                               int *toolDimTags, size_t toolDimTags_n,
!                               int **outDimTags, size_t *outDimTags_n,
!                               int ***outDimTagsMap, size_t **outDimTagsMap_n, size_t *outDimTagsMap_nn,
!                               const int tag,
!                               const int removeObject,
!                               const int removeTool,
!                               int *ierr);
!

INTERFACE
SUBROUTINE gmshModelOccCut(objectDimTags, objectDimTags_n, &
  & toolDimTags, toolDimTags_n, outDimTags, outDimTags_n, outDimTagsMap, &
  & outDimTagsMap_n, outDimTagsMap_nn, tag, removeObject, removeTool, &
  & ierr) &
  & BIND(C, NAME="gmshModelOccCut")
  IMPORT
  _ST_V_IN_ :: objectDimTags_n
  _I_IN_ :: objectDimTags(objectDimTags_n)
  _ST_V_IN_ :: toolDimTags_n
  _I_IN_ :: toolDimTags(toolDimTags_n)
  _CPTR_IN_ :: outDimTags
  _I_OUT_ :: outDimTags_n
  _CPTR_IN_ :: outDimTagsMap
  _CPTR_IN_ :: outDimTagsMap_n
  _ST_OUT_ :: outDimTagsMap_nn
  _I_V_IN_ :: tag
  _I_V_IN_ :: removeObject
  _I_V_IN_ :: removeTool
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelOccCut
END INTERFACE

PUBLIC :: gmshModelOccCut

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Compute the boolean fragments (general fuse) resulting from the
! intersection of the entities `objectDimTags' and `toolDimTags' in the
! OpenCASCADE CAD representation, making all iterfaces conformal. When
! applied to entities of different dimensions, the lower dimensional entities
! will be automatically embedded in the higher dimensional entities if they
! are not on their boundary. Return the resulting entities in `outDimTags'.
! If `tag' is positive, try to set the tag explicitly (only valid if the
! boolean operation results in a single entity). Remove the object if
! `removeObject' is set. Remove the tool if `removeTool' is set.
!
! GMSH_API void gmshModelOccFragment(int *objectDimTags, size_t objectDimTags_n,
!                                    int *toolDimTags, size_t toolDimTags_n,
!                                    int **outDimTags, size_t *outDimTags_n,
!                                    int ***outDimTagsMap, size_t **outDimTagsMap_n, size_t *outDimTagsMap_nn,
!                                    const int tag,
!                                    const int removeObject,
!                                    const int removeTool,
!                                    int *ierr);

INTERFACE
SUBROUTINE gmshModelOccFragment(objectDimTags, objectDimTags_n, &
  & toolDimTags, toolDimTags_n, outDimTags, outDimTags_n, outDimTagsMap, &
  & outDimTagsMap_n, outDimTagsMap_nn, tag, removeObject, removeTool, &
  & ierr) &
  & BIND(C, NAME="gmshModelOccFragment")
  IMPORT
  _ST_V_IN_ :: objectDimTags_n
  _I_IN_ :: objectDimTags(objectDimTags_n)
  _ST_V_IN_ :: toolDimTags_n
  _I_IN_ :: toolDimTags(toolDimTags_n)
  _CPTR_IN_ :: outDimTags
  _I_OUT_ :: outDimTags_n
  _CPTR_IN_ :: outDimTagsMap
  _CPTR_IN_ :: outDimTagsMap_n
  _ST_OUT_ :: outDimTagsMap_nn
  _I_V_IN_ :: tag
  _I_V_IN_ :: removeObject
  _I_V_IN_ :: removeTool
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelOccFragment
END INTERFACE

PUBLIC :: gmshModelOccFragment

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Translate the entities `dimTags' in the OpenCASCADE CAD representation
!  * along (`dx', `dy', `dz'). */
! GMSH_API void gmshModelOccTranslate(int *dimTags, size_t dimTags_n,
!                                     const double dx,
!                                     const double dy,
!                                     const double dz,
!                                     int *ierr);

INTERFACE
SUBROUTINE gmshModelOccTranslate(dimTags, dimTags_n, dx, dy, dz, ierr) &
  & BIND(C, NAME="gmshModelOccTranslate")
  IMPORT
  _ST_V_IN_ :: dimTags_n
  _I_IN_ :: dimTags( dimTags_n )
  _R_V_IN_ :: dx, dy, dz
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelOccTranslate
END INTERFACE

PUBLIC :: gmshModelOccTranslate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Rotate the entities `dimTags' in the OpenCASCADE CAD representation by
! `angle' radians around the axis of revolution defined by the point (`x',
! `y', `z') and the direction (`ax', `ay', `az').
!
! GMSH_API void gmshModelOccRotate(int *dimTags, size_t dimTags_n,
!                                  const double x,
!                                  const double y,
!                                  const double z,
!                                  const double ax,
!                                  const double ay,
!                                  const double az,
!                                  const double angle,
!                                  int *ierr);

INTERFACE
SUBROUTINE gmshModelOccRotate(dimTags, dimTags_n, x, y, z, ax, &
  & ay, az, angle, ierr) &
  & BIND(C, NAME="gmshModelOccRotate")
  IMPORT
  _ST_V_IN_ :: dimTags_n
  _I_IN_ :: dimTags( dimTags_n )
  _R_V_IN_ :: x, y, z, ax, ay, az, angle
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelOccRotate
END INTERFACE

PUBLIC :: gmshModelOccRotate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Scale the entities `dimTags' in the OpenCASCADE CAD representation by
!  * factors `a', `b' and `c' along the three coordinate axes; use (`x', `y',
!  * `z') as the center of the homothetic transformation. */
! GMSH_API void gmshModelOccDilate(int *dimTags, size_t dimTags_n,
!                                  const double x,
!                                  const double y,
!                                  const double z,
!                                  const double a,
!                                  const double b,
!                                  const double c,
!                                  int *ierr);

INTERFACE
SUBROUTINE gmshModelOccDilate(dimTags, dimTags_n, x, y, z, a, &
  & b, c, ierr) &
  & BIND(C, NAME="gmshModelOccDilate")
  IMPORT
  _ST_V_IN_ :: dimTags_n
  _I_IN_ :: dimTags( dimTags_n )
  _R_V_IN_ :: x, y, z, a, b, c
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelOccDilate
END INTERFACE

PUBLIC :: gmshModelOccDilate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Mirror the entities `dimTags' in the OpenCASCADE CAD representation, with
! respect to the plane of equation `a' * x + `b' * y + `c' * z + `d' = 0.
!
! GMSH_API void gmshModelOccMirror(int *dimTags, size_t dimTags_n,
!                                  const double a,
!                                  const double b,
!                                  const double c,
!                                  const double d,
!                                  int *ierr);

INTERFACE
SUBROUTINE gmshModelOccMirror(dimTags, dimTags_n, a, b, c, d, ierr) &
  & BIND(C, NAME="gmshModelOccMirror")
  IMPORT
  _ST_V_IN_ :: dimTags_n
  _I_IN_ :: dimTags( dimTags_n )
  _R_V_IN_ :: a,b,c,d
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelOccMirror
END INTERFACE

PUBLIC :: gmshModelOccMirror

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Mirror the entities `dimTags' in the OpenCASCADE CAD representation, with
! respect to the plane of equation `a' * x + `b' * y + `c' * z + `d' = 0.
! (This is a synonym for `mirror', which will be deprecated in a future
! release.) */
!
! GMSH_API void gmshModelOccSymmetrize(int *dimTags, size_t dimTags_n,
!                                      const double a,
!                                      const double b,
!                                      const double c,
!                                      const double d,
!                                      int *ierr);

INTERFACE
SUBROUTINE gmshModelOccSymmetrize(dimTags, dimTags_n, a, b, c, d, ierr) &
  & BIND(C, NAME="gmshModelOccSymmetrize")
  IMPORT
  _ST_V_IN_ :: dimTags_n
  _I_IN_ :: dimTags( dimTags_n )
  _R_V_IN_ :: a,b,c,d
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelOccSymmetrize
END INTERFACE

PUBLIC :: gmshModelOccSymmetrize

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Apply a general affine transformation matrix `a' (16 entries of a 4x4
! matrix, by row; only the 12 first can be provided for convenience) to the
! entities `dimTags' in the OpenCASCADE CAD representation.
!
! GMSH_API void gmshModelOccAffineTransform(int *dimTags, size_t dimTags_n,
!                                           double *a, size_t a_n,
!                                           int *ierr);

INTERFACE
SUBROUTINE gmshModelOccAffineTransform(dimTags, dimTags_n, a, b, c, d, ierr) &
  & BIND(C, NAME="gmshModelOccAffineTransform")
  IMPORT
  _ST_V_IN_ :: dimTags_n
  _I_IN_ :: dimTags( dimTags_n )
  _R_V_IN_ :: a,b,c,d
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelOccAffineTransform
END INTERFACE

PUBLIC :: gmshModelOccAffineTransform

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Copy the entities `dimTags' in the OpenCASCADE CAD representation; the new
! entities are returned in `outDimTags'.
!
! GMSH_API void gmshModelOccCopy(int *dimTags, size_t dimTags_n,
!                                int **outDimTags, size_t *outDimTags_n,
!                                int *ierr);

INTERFACE
SUBROUTINE gmshModelOccCopy(dimTags, dimTags_n, outDimTags, outDimTags_n, &
  & ierr ) &
  & BIND(C, NAME="gmshModelOccCopy")
  IMPORT
  _ST_V_IN_ :: dimTags_n
  _I_IN_ :: dimTags( dimTags_n )
  _ST_OUT_ :: outDimTags_n
  _CPTR_IN_ :: outDimTags
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelOccCopy
END INTERFACE

PUBLIC :: gmshModelOccCopy

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Remove the entities `dimTags' in the OpenCASCADE CAD representation. If
! `recursive' is true, remove all the entities on their boundaries, down to
! dimension 0.
!
! GMSH_API void gmshModelOccRemove(int *dimTags, size_t dimTags_n,
!                                  const int recursive,
!                                  int *ierr);

INTERFACE
SUBROUTINE gmshModelOccRemove(dimTags, dimTags_n, recursive, ierr) &
  & BIND(C, NAME="gmshModelOccRemove")
  IMPORT
  _ST_V_IN_ :: dimTags_n
  _I_IN_ :: dimTags( dimTags_n )
  _I_V_IN_ :: recursive
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelOccRemove
END INTERFACE

PUBLIC :: gmshModelOccRemove

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Remove all duplicate entities in the OpenCASCADE CAD representation
! (different entities at the same geometrical location) after intersecting
! (using boolean fragments) all highest dimensional entities.
!
! GMSH_API void gmshModelOccRemoveAllDuplicates(int *ierr);

INTERFACE
SUBROUTINE gmshModelOccRemoveAllDuplicates(ierr) &
  & BIND(C, NAME="gmshModelOccRemoveAllDuplicates")
  IMPORT
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelOccRemoveAllDuplicates
END INTERFACE

PUBLIC :: gmshModelOccRemoveAllDuplicates

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Apply various healing procedures to the entities `dimTags' (or to all the
! entities in the model if `dimTags' is empty) in the OpenCASCADE CAD
! representation. Return the healed entities in `outDimTags'. Available
! healing options are listed in the Gmsh reference manual. */

! GMSH_API void gmshModelOccHealShapes(int **outDimTags, size_t *outDimTags_n,
!                                      int *dimTags, size_t dimTags_n,
!                                      const double tolerance,
!                                      const int fixDegenerated,
!                                      const int fixSmallEdges,
!                                      const int fixSmallFaces,
!                                      const int sewFaces,
!                                      const int makeSolids,
!                                      int *ierr);

INTERFACE
SUBROUTINE gmshModelOccHealShapes(outDimTags, outDimTags_n, dimTags, &
  & dimTags_n, tolerance, fixDegenerated, fixSmallEdges, fixSmallFaces, &
  & sewFaces, makeSolids, ierr) &
  & BIND(C, NAME="gmshModelOccHealShapes")
  IMPORT
  _CPTR_IN_ :: outDimTags
  _ST_OUT_ :: outDimTags_n
  _ST_V_IN_ :: dimTags_n
  _I_IN_ :: dimTags(dimTags_n)
  _R_V_IN_ :: tolerance
  _I_V_IN_ :: fixDegenerated, fixSmallEdges, fixSmallFaces, &
  & sewFaces, makeSolids
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelOccHealShapes
END INTERFACE

PUBLIC :: gmshModelOccHealShapes

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Import BREP, STEP or IGES shapes from the file `fileName' in the
! OpenCASCADE CAD representation. The imported entities are returned in
! `outDimTags'. If the optional argument `highestDimOnly' is set, only import
! the highest dimensional entities in the file. The optional argument
! `format' can be used to force the format of the file (currently "brep",
! "step" or "iges")
!
! GMSH_API void gmshModelOccImportShapes(const char *fileName,
!                                        int **outDimTags, size_t *outDimTags_n,
!                                        const int highestDimOnly,
!                                        const char *format,
!                                        int *ierr);

INTERFACE
SUBROUTINE gmshModelOccImportShapes(fileName, outDimTags, outDimTags_n, &
  & highestDimOnly, format, ierr) &
  & BIND(C, NAME="gmshModelOccImportShapes")
  IMPORT
  _CPTR_V_IN_ :: fileName
  _CPTR_IN_ :: outDimTags
  _ST_OUT_ :: outDimTags_n
  _I_V_IN_ :: highestDimOnly
  _CPTR_V_IN_ :: format
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelOccImportShapes
END INTERFACE

PUBLIC :: gmshModelOccImportShapes

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Imports an OpenCASCADE `shape' by providing a pointer to a native
! OpenCASCADE `TopoDS_Shape' object (passed as a pointer to void). The
! imported entities are returned in `outDimTags'. If the optional argument
! `highestDimOnly' is set, only import the highest dimensional entities in
! `shape'. For C and C++ only. Warning: this function is unsafe, as providing
!  * an invalid pointer will lead to undefined behavior.
!
! GMSH_API void gmshModelOccImportShapesNativePointer(const void *shape,
!                                                     int **outDimTags, size_t *outDimTags_n,
!                                                     const int highestDimOnly,
!                                                     int *ierr);

INTERFACE
SUBROUTINE gmshModelOccImportShapesNativePointer(shape, outDimTags, &
  & outDimTags_n, highestDimOnly, ierr) &
  & BIND(C, NAME="gmshModelOccImportShapesNativePointer")
  IMPORT
  _CPTR_IN_ :: shape
  _CPTR_IN_ :: outDimTags
  _ST_OUT_ :: outDimTags_n
  _I_V_IN_ :: highestDimOnly
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelOccImportShapesNativePointer
END INTERFACE

PUBLIC :: gmshModelOccImportShapesNativePointer

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get all the OpenCASCADE entities. If `dim' is >= 0, return only the
! entities of the specified dimension (e.g. points if `dim' == 0). The
! entities are returned as a vector of (dim, tag) integer pairs.
!
! GMSH_API void gmshModelOccGetEntities(int **dimTags, size_t *dimTags_n,
!                                       const int dim,
!                                       int *ierr);

INTERFACE
SUBROUTINE gmshModelOccGetEntities(dimTags, dimTags_n, dim, ierr) &
  & BIND(C, NAME="gmshModelOccGetEntities")
  IMPORT
  _CPTR_IN_ :: dimTags
  _ST_OUT_ :: dimTags_n
  _I_V_IN_ :: dim
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelOccGetEntities
END INTERFACE

PUBLIC :: gmshModelOccGetEntities

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Get the OpenCASCADE entities in the bounding box defined by the two points
!  * (`xmin', `ymin', `zmin') and (`xmax', `ymax', `zmax'). If `dim' is >= 0,
!  * return only the entities of the specified dimension (e.g. points if `dim'
!  * == 0). */
! GMSH_API void gmshModelOccGetEntitiesInBoundingBox(const double xmin,
!                                                    const double ymin,
!                                                    const double zmin,
!                                                    const double xmax,
!                                                    const double ymax,
!                                                    const double zmax,
!                                                    int **tags, size_t *tags_n,
!                                                    const int dim,
!                                                    int *ierr);

INTERFACE
SUBROUTINE gmshModelOccGetEntitiesInBoundingBox(xmin, ymin, zmin, xmax, &
  & ymax, zmax, tags, tags_n, dim, ierr) &
  & BIND(C, NAME="gmshModelOccGetEntitiesInBoundingBox")
  IMPORT
  _R_V_IN_ :: xmin, ymin, zmin, xmax, ymax, zmax
  _CPTR_IN_ :: tags
  _ST_OUT_ :: tags_n
  _I_V_IN_ :: dim
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelOccGetEntitiesInBoundingBox
END INTERFACE

PUBLIC :: gmshModelOccGetEntitiesInBoundingBox

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the bounding box (`xmin', `ymin', `zmin'), (`xmax', `ymax', `zmax') of
!  * the OpenCASCADE entity of dimension `dim' and tag `tag'.
!
! GMSH_API void gmshModelOccGetBoundingBox(const int dim,
!                                          const int tag,
!                                          double *xmin,
!                                          double *ymin,
!                                          double *zmin,
!                                          double *xmax,
!                                          double *ymax,
!                                          double *zmax,
!                                          int *ierr);

INTERFACE
SUBROUTINE gmshModelOccGetBoundingBox(dim, tag, xmin, ymin, zmin, xmax, &
  & ymax, zmax, ierr) &
  & BIND(C, NAME="gmshModelOccGetBoundingBox")
  IMPORT
  _I_V_IN_ :: dim, tag
  _R_OUT_ :: xmin, ymin, zmin, xmax, ymax, zmax
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelOccGetBoundingBox
END INTERFACE

PUBLIC :: gmshModelOccGetBoundingBox

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the mass of the OpenCASCADE entity of dimension `dim' and tag `tag'.
!
! GMSH_API void gmshModelOccGetMass(const int dim,
!                                   const int tag,
!                                   double *mass,
!                                   int *ierr);

INTERFACE
SUBROUTINE gmshModelOccGetMass(dim, tag, mass, ierr) &
  & BIND(C, NAME="gmshModelOccGetMass")
  IMPORT
  _I_V_IN_ :: dim, tag
  _R_OUT_ :: mass
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelOccGetMass
END INTERFACE

PUBLIC :: gmshModelOccGetMass

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the center of mass of the OpenCASCADE entity of dimension `dim' and tag
!  * `tag'.
!
! GMSH_API void gmshModelOccGetCenterOfMass(const int dim,
!                                           const int tag,
!                                           double *x,
!                                           double *y,
!                                           double *z,
!                                           int *ierr);

INTERFACE
SUBROUTINE gmshModelOccGetCenterOfMass(dim, tag, x, y, z, ierr) &
  & BIND(C, NAME="gmshModelOccGetCenterOfMass")
  IMPORT
  _I_V_IN_ :: dim, tag
  _R_OUT_ :: x, y, z
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelOccGetCenterOfMass
END INTERFACE

PUBLIC :: gmshModelOccGetCenterOfMass

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the matrix of inertia (by row) of the OpenCASCADE entity of dimension
!  * `dim' and tag `tag'.
!
! GMSH_API void gmshModelOccGetMatrixOfInertia(const int dim,
!                                              const int tag,
!                                              double **mat, size_t *mat_n,
!                                              int *ierr);

INTERFACE
SUBROUTINE gmshModelOccGetMatrixOfInertia(dim, tag, mat, mat_n, ierr) &
  & BIND(C, NAME="gmshModelOccGetMatrixOfInertia")
  IMPORT
  _I_V_IN_ :: dim, tag
  _CPTR_IN_ :: mat
  _I_OUT_ :: mat_n
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelOccGetMatrixOfInertia
END INTERFACE

PUBLIC :: gmshModelOccGetMatrixOfInertia

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the maximum tag of entities of dimension `dim' in the OpenCASCADE CAD
!  * representation.
!
! GMSH_API int gmshModelOccGetMaxTag(const int dim,
!                                    int *ierr);

INTERFACE
SUBROUTINE gmshModelOccGetMaxTag(dim, ierr) &
  & BIND(C, NAME="gmshModelOccGetMaxTag")
  IMPORT
  _I_V_IN_ :: dim
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelOccGetMaxTag
END INTERFACE

PUBLIC :: gmshModelOccGetMaxTag

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Set the maximum tag `maxTag' for entities of dimension `dim' in the
! OpenCASCADE CAD representation.
!
! GMSH_API void gmshModelOccSetMaxTag(const int dim,
!                                     const int maxTag,
!                                     int *ierr);

INTERFACE
SUBROUTINE gmshModelOccSetMaxTag(dim, maxTag, ierr) &
  & BIND(C, NAME="gmshModelOccSetMaxTag")
  IMPORT
  _I_V_IN_ :: dim
  _I_V_IN_ :: maxTag
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelOccSetMaxTag
END INTERFACE

PUBLIC :: gmshModelOccSetMaxTag

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Synchronize the OpenCASCADE CAD representation with the current Gmsh model.
! This can be called at any time, but since it involves a non trivial amount
! of processing, the number of synchronization points should normally be
! minimized. Without synchronization the entities in the OpenCASCADE CAD
! representation are not available to any function outside of the OpenCASCADE
! CAD kernel functions.
!
! GMSH_API void gmshModelOccSynchronize(int *ierr);

INTERFACE
SUBROUTINE gmshModelOccSynchronize(ierr) &
  & BIND(C, NAME="gmshModelOccSynchronize")
  IMPORT
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelOccSynchronize
END INTERFACE

PUBLIC :: gmshModelOccSynchronize