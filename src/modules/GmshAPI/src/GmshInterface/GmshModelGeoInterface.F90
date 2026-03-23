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

MODULE GmshModelGeoInterface
USE ISO_C_BINDING, ONLY: C_INT
USE ISO_C_BINDING, ONLY: C_DOUBLE
USE ISO_C_BINDING, ONLY: C_SIZE_T
USE ISO_C_BINDING, ONLY: C_PTR
USE ISO_C_BINDING, ONLY: C_CHAR
IMPLICIT NONE
PRIVATE

PUBLIC :: gmshModelGeoAddPoint
PUBLIC :: gmshModelGeoAddLine
PUBLIC :: gmshModelGeoAddCircleArc
PUBLIC :: gmshModelGeoAddEllipseArc
PUBLIC :: gmshModelGeoAddSpline
PUBLIC :: gmshModelGeoAddBSpline
PUBLIC :: gmshModelGeoAddBezier
PUBLIC :: gmshModelGeoAddPolyline
PUBLIC :: gmshModelGeoAddCompoundSpline
PUBLIC :: gmshModelGeoAddCompoundBSpline
PUBLIC :: gmshModelGeoAddCurveLoop
PUBLIC :: gmshModelGeoAddCurveLoops
PUBLIC :: gmshModelGeoAddPlaneSurface
PUBLIC :: gmshModelGeoAddSurfaceFilling
PUBLIC :: gmshModelGeoAddSurfaceLoop
PUBLIC :: gmshModelGeoAddVolume
PUBLIC :: gmshModelGeoAddGeometry
PUBLIC :: gmshModelGeoAddPointOnGeometry
PUBLIC :: gmshModelGeoExtrude
PUBLIC :: gmshModelGeoRevolve
PUBLIC :: gmshModelGeoTwist
PUBLIC :: gmshModelGeoTranslate
PUBLIC :: gmshModelGeoRotate
PUBLIC :: gmshModelGeoDilate
PUBLIC :: gmshModelGeoMirror
PUBLIC :: gmshModelGeoSymmetrize
PUBLIC :: gmshModelGeoCopy
PUBLIC :: gmshModelGeoRemove
PUBLIC :: gmshModelGeoRemoveAllDuplicates
PUBLIC :: gmshModelGeoSplitCurve
PUBLIC :: gmshModelGeoGetMaxTag
PUBLIC :: gmshModelGeoSetMaxTag
PUBLIC :: gmshModelGeoAddPhysicalGroup
PUBLIC :: gmshModelGeoRemovePhysicalGroups
PUBLIC :: gmshModelGeoSynchronize

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION gmshModelGeoAddPoint( &
    x, y, z, meshSize, tag, ierr) RESULT(ans) &
    BIND(C, NAME="gmshModelGeoAddPoint")
    IMPORT
    REAL(C_DOUBLE), VALUE, INTENT(IN) :: x
    REAL(C_DOUBLE), VALUE, INTENT(IN) :: y
    REAL(C_DOUBLE), VALUE, INTENT(IN) :: z
    REAL(C_DOUBLE), VALUE, INTENT(IN) :: meshSize
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    INTEGER(C_INT), INTENT(OUT) :: ierr
    INTEGER(C_INT) :: ans
  END FUNCTION gmshModelGeoAddPoint
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION gmshModelGeoAddLine( &
    startTag, endTag, tag, ierr) RESULT(ans) &
    BIND(C, NAME="gmshModelGeoAddLine")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: startTag, endTag, tag
    INTEGER(C_INT), INTENT(OUT) :: ierr
    INTEGER(C_INT) :: ans
  END FUNCTION gmshModelGeoAddLine
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION gmshModelGeoAddCircleArc( &
    startTag, centerTag, endTag, tag, nx, ny, nz, ierr) RESULT(ans) &
    BIND(C, NAME="gmshModelGeoAddCircleArc")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: startTag, endTag, tag, centerTag
    REAL(C_DOUBLE), VALUE, INTENT(IN) :: nx, ny, nz
    INTEGER(C_INT), INTENT(OUT) :: ierr
    INTEGER(C_INT) :: ans
  END FUNCTION gmshModelGeoAddCircleArc
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION gmshModelGeoAddEllipseArc( &
    startTag, centerTag, majorTag, endTag, tag, nx, ny, &
    nz, ierr) RESULT(ans) BIND(C, NAME="gmshModelGeoAddEllipseArc")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: startTag, centerTag, &
                                         majorTag, endTag, tag
    REAL(C_DOUBLE), VALUE, INTENT(IN) :: nx, ny, nz
    INTEGER(C_INT), INTENT(OUT) :: ierr
    INTEGER(C_INT) :: ans
  END FUNCTION gmshModelGeoAddEllipseArc
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION gmshModelGeoAddSpline( &
    pointTags, pointTags_n, tag, ierr) RESULT(ans) &
    BIND(C, NAME="gmshModelGeoAddSpline")
    IMPORT
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: pointTags_n
    INTEGER(C_INT), INTENT(IN) :: pointTags(pointTags_n)
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    INTEGER(C_INT), INTENT(OUT) :: ierr
    INTEGER(C_INT) :: ans
  END FUNCTION gmshModelGeoAddSpline
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION gmshModelGeoAddBSpline( &
    pointTags, pointTags_n, tag, ierr) RESULT(ans) &
    BIND(C, NAME="gmshModelGeoAddBSpline")
    IMPORT
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: pointTags_n
    INTEGER(C_INT), INTENT(IN) :: pointTags(pointTags_n)
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    INTEGER(C_INT), INTENT(OUT) :: ierr
    INTEGER(C_INT) :: ans
  END FUNCTION gmshModelGeoAddBSpline
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION gmshModelGeoAddBezier( &
    pointTags, pointTags_n, tag, ierr) RESULT(ans) &
    BIND(C, NAME="gmshModelGeoAddBezier")
    IMPORT
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: pointTags_n
    INTEGER(C_INT), INTENT(IN) :: pointTags(pointTags_n)
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    INTEGER(C_INT), INTENT(OUT) :: ierr
    INTEGER(C_INT) :: ans
  END FUNCTION gmshModelGeoAddBezier
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION gmshModelGeoAddPolyline( &
    pointTags, pointTags_n, tag, ierr) RESULT(ans) &
    BIND(C, NAME="gmshModelGeoAddPolyline")
    IMPORT
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: pointTags_n
    INTEGER(C_INT), INTENT(IN) :: pointTags(pointTags_n)
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    INTEGER(C_INT), INTENT(OUT) :: ierr
    INTEGER(C_INT) :: ans
  END FUNCTION gmshModelGeoAddPolyline
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION gmshModelGeoAddCompoundSpline( &
    curveTags, curveTags_n, numIntervals, tag, ierr) RESULT(ans) &
    BIND(C, NAME="gmshModelGeoAddCompoundSpline")
    IMPORT
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: curveTags_n
    INTEGER(C_INT), INTENT(IN) :: curveTags(curveTags_n)
    INTEGER(C_INT), VALUE, INTENT(IN) :: numIntervals, tag
    INTEGER(C_INT), INTENT(OUT) :: ierr
    INTEGER(C_INT) :: ans
  END FUNCTION gmshModelGeoAddCompoundSpline
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION gmshModelGeoAddCompoundBSpline( &
    curveTags, curveTags_n, numIntervals, tag, ierr) RESULT(ans) &
    BIND(C, NAME="gmshModelGeoAddCompoundBSpline")
    IMPORT
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: curveTags_n
    INTEGER(C_INT), INTENT(IN) :: curveTags(curveTags_n)
    INTEGER(C_INT), VALUE, INTENT(IN) :: numIntervals, tag
    INTEGER(C_INT), INTENT(OUT) :: ierr
    INTEGER(C_INT) :: ans
  END FUNCTION gmshModelGeoAddCompoundBSpline
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION gmshModelGeoAddCurveLoop( &
    curveTags, curveTags_n, tag, reorient, ierr) RESULT(ans) &
    BIND(C, NAME="gmshModelGeoAddCurveLoop")
    IMPORT
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: curveTags_n
    INTEGER(C_INT), INTENT(IN) :: curveTags(curveTags_n)
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag, reorient
    INTEGER(C_INT), INTENT(OUT) :: ierr
    INTEGER(C_INT) :: ans
  END FUNCTION gmshModelGeoAddCurveLoop
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE gmshModelGeoAddCurveLoops( &
    curveTags, curveTags_n, tags, tags_n, ierr) &
    BIND(C, NAME="gmshModelGeoAddCurveLoops")
    IMPORT
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: curveTags_n
    INTEGER(C_INT), INTENT(IN) :: curveTags(curveTags_n)
    TYPE(C_PTR), INTENT(IN) :: tags
    INTEGER(C_SIZE_T), INTENT(OUT) :: tags_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGeoAddCurveLoops
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION gmshModelGeoAddPlaneSurface( &
    wireTags, wireTags_n, tag, ierr) RESULT(ans) &
    BIND(C, NAME="gmshModelGeoAddPlaneSurface")
    IMPORT
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: wireTags_n
    INTEGER(C_INT), INTENT(IN) :: wireTags(wireTags_n)
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    INTEGER(C_INT), INTENT(OUT) :: ierr
    INTEGER(C_INT) :: ans
  END FUNCTION gmshModelGeoAddPlaneSurface
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION gmshModelGeoAddSurfaceFilling( &
    wireTags, wireTags_n, tag, sphereCenterTag, ierr) RESULT(ans) &
    BIND(C, NAME="gmshModelGeoAddSurfaceFilling")
    IMPORT
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: wireTags_n
    INTEGER(C_INT), INTENT(IN) :: wireTags(wireTags_n)
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    INTEGER(C_INT), VALUE, INTENT(IN) :: sphereCenterTag
    INTEGER(C_INT), INTENT(OUT) :: ierr
    INTEGER(C_INT) :: ans
  END FUNCTION gmshModelGeoAddSurfaceFilling
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION gmshModelGeoAddSurfaceLoop( &
    surfaceTags, surfaceTags_n, tag, ierr) RESULT(ans) &
    BIND(C, NAME="gmshModelGeoAddSurfaceLoop")
    IMPORT
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: surfaceTags_n
    INTEGER(C_INT), INTENT(IN) :: surfaceTags(surfaceTags_n)
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    INTEGER(C_INT), INTENT(OUT) :: ierr
    INTEGER(C_INT) :: ans
  END FUNCTION gmshModelGeoAddSurfaceLoop
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION gmshModelGeoAddVolume( &
    shellTags, shellTags_n, tag, ierr) RESULT(ans) &
    BIND(C, NAME="gmshModelGeoAddVolume")
    IMPORT
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: shellTags_n
    INTEGER(C_INT), INTENT(IN) :: shellTags(shellTags_n)
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    INTEGER(C_INT), INTENT(OUT) :: ierr
    INTEGER(C_INT) :: ans
  END FUNCTION gmshModelGeoAddVolume
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION gmshModelGeoAddGeometry( &
    geometry, numbers, numbers_n, strings, strings_n, tag, ierr) &
    RESULT(ans) BIND(C, name="gmshModelGeoAddGeometry")
    IMPORT
    CHARACTER(len=1, kind=C_CHAR), DIMENSION(*), INTENT(in) :: geometry
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: numbers_n
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: strings_n
    REAL(C_DOUBLE), INTENT(IN) :: numbers(numbers_n)
    TYPE(C_PTR), INTENT(IN) :: strings(strings_n)
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    INTEGER(C_INT), INTENT(OUT) :: ierr
    INTEGER(C_INT) :: ans
  END FUNCTION gmshModelGeoAddGeometry
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION gmshModelGeoAddPointOnGeometry( &
    geometryTag, x, y, z, meshSize, tag, ierr) RESULT(ans) &
    BIND(C, name="gmshModelGeoAddPointOnGeometry")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: geometryTag, tag
    REAL(C_DOUBLE), VALUE, INTENT(IN) :: x, y, z, meshSize
    INTEGER(C_INT), INTENT(OUT) :: ierr
    INTEGER(C_INT) :: ans
  END FUNCTION gmshModelGeoAddPointOnGeometry
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE gmshModelGeoExtrude( &
    dimTags, dimTags_n, dx, dy, dz, outDimTags, outDimTags_n, &
    numElements, numElements_n, heights, heights_n, recombine, &
    ierr) BIND(C, NAME="gmshModelGeoExtrude")
    IMPORT
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: dimTags_n
    INTEGER(C_INT), INTENT(IN) :: dimTags(*)
    REAL(C_DOUBLE), VALUE, INTENT(IN) :: dx, dy, dz
    TYPE(C_PTR), INTENT(IN) :: outDimTags
    INTEGER(C_SIZE_T), INTENT(OUT) :: outDimTags_n
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: numElements_n
    INTEGER(C_INT), INTENT(IN) :: numElements(numElements_n)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: heights_n
    REAL(C_DOUBLE), INTENT(IN) :: heights(heights_n)
    INTEGER(C_INT), VALUE, INTENT(IN) :: recombine
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGeoExtrude
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE gmshModelGeoRevolve( &
    dimTags, dimTags_n, x, y, z, ax, ay, az, &
    angle, outDimTags, outDimTags_n, numElements, numElements_n, heights, &
    heights_n, recombine, ierr) BIND(C, NAME="gmshModelGeoRevolve")
    IMPORT
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: dimTags_n
    INTEGER(C_INT), INTENT(IN) :: dimTags(*)
    REAL(C_DOUBLE), VALUE, INTENT(IN) :: x, y, z, ax, ay, az, angle
    TYPE(C_PTR), INTENT(IN) :: outDimTags
    INTEGER(C_SIZE_T), INTENT(OUT) :: outDimTags_n
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: numElements_n
    INTEGER(C_INT), INTENT(IN) :: numElements(numElements_n)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: heights_n
    REAL(C_DOUBLE), INTENT(IN) :: heights(heights_n)
    INTEGER(C_INT), VALUE, INTENT(IN) :: recombine
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGeoRevolve
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE gmshModelGeoTwist( &
    dimTags, dimTags_n, x, y, z, dx, dy, dz, ax, &
    ay, az, angle, outDimTags, outDimTags_n, numElements, numElements_n, &
    heights, heights_n, recombine, ierr) BIND(C, NAME="gmshModelGeoTwist")
    IMPORT
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: dimTags_n
    INTEGER(C_INT), INTENT(IN) :: dimTags(*)
    REAL(C_DOUBLE), VALUE, INTENT(IN) :: x, y, z, dx, dy, dz, &
                                         ax, ay, az, angle
    TYPE(C_PTR), INTENT(IN) :: outDimTags
    INTEGER(C_SIZE_T), INTENT(OUT) :: outDimTags_n
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: numElements_n
    INTEGER(C_INT), INTENT(IN) :: numElements(numElements_n)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: heights_n
    REAL(C_DOUBLE), INTENT(IN) :: heights(heights_n)
    INTEGER(C_INT), VALUE, INTENT(IN) :: recombine
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGeoTwist
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE gmshModelGeoExtrudeBoundaryLayer( &
    dimTags, dimTags_n, outDimTags, outDimTags_n, numElements, &
    numElements_n, heights, heights_n, recombine, second, &
    viewIndex, ierr) BIND(C, NAME="gmshModelGeoExtrudeBoundaryLayer")
    IMPORT
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: dimTags_n
    INTEGER(C_INT), INTENT(IN) :: dimTags(*)
    TYPE(C_PTR), INTENT(IN) :: outDimTags
    INTEGER(C_SIZE_T), INTENT(OUT) :: outDimTags_n
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: numElements_n
    INTEGER(C_INT), INTENT(IN) :: numElements(numElements_n)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: heights_n
    REAL(C_DOUBLE), INTENT(IN) :: heights(heights_n)
    INTEGER(C_INT), VALUE, INTENT(IN) :: recombine, second, viewIndex
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGeoExtrudeBoundaryLayer
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE gmshModelGeoTranslate( &
    dimTags, dimTags_n, dx, dy, dz, ierr) &
    BIND(C, NAME="gmshModelGeoTranslate")
    IMPORT
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: dimTags_n
    INTEGER(C_INT), INTENT(IN) :: dimTags(*)
    REAL(C_DOUBLE), VALUE, INTENT(IN) :: dx, dy, dz
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGeoTranslate
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE gmshModelGeoRotate( &
    dimTags, dimTags_n, x, y, z, ax, ay, az, angle, ierr) &
    BIND(C, NAME="gmshModelGeoRotate")
    IMPORT
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: dimTags_n
    INTEGER(C_INT), INTENT(IN) :: dimTags(*)
    REAL(C_DOUBLE), VALUE, INTENT(IN) :: x, y, z, ax, ay, az, angle
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGeoRotate
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE gmshModelGeoDilate( &
    dimTags, dimTags_n, x, y, z, a, b, c, ierr) &
    BIND(C, NAME="gmshModelGeoDilate")
    IMPORT
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: dimTags_n
    INTEGER(C_INT), INTENT(IN) :: dimTags(dimTags_n)
    REAL(C_DOUBLE), VALUE, INTENT(IN) :: x, y, z, a, b, c
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGeoDilate
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE gmshModelGeoMirror( &
    dimTags, dimTags_n, a, b, c, d, ierr) BIND(C, NAME="gmshModelGeoMirror")
    IMPORT
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: dimTags_n
    INTEGER(C_INT), INTENT(IN) :: dimTags(dimTags_n)
    REAL(C_DOUBLE), VALUE, INTENT(IN) :: a, b, c, d
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGeoMirror
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE gmshModelGeoSymmetrize( &
    dimTags, dimTags_n, a, b, c, d, ierr) &
    BIND(C, NAME="gmshModelGeoSymmetrize")
    IMPORT
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: dimTags_n
    INTEGER(C_INT), INTENT(IN) :: dimTags(dimTags_n)
    REAL(C_DOUBLE), VALUE, INTENT(IN) :: a, b, c, d
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGeoSymmetrize
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE gmshModelGeoCopy( &
    dimTags, dimTags_n, outDimTags, outDimTags_n, ierr) &
    BIND(C, NAME="gmshModelGeoCopy")
    IMPORT
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: dimTags_n
    INTEGER(C_INT), INTENT(IN) :: dimTags(dimTags_n)
    TYPE(C_PTR), INTENT(IN) :: outDimTags
    INTEGER(C_SIZE_T), INTENT(OUT) :: outDimTags_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGeoCopy
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE gmshModelGeoRemove( &
    dimTags, dimTags_n, RECURSIVE, ierr) BIND(C, NAME="gmshModelGeoRemove")
    IMPORT
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: dimTags_n
    INTEGER(C_INT), INTENT(IN) :: dimTags(dimTags_n)
    INTEGER(C_INT), VALUE, INTENT(IN) :: RECURSIVE
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGeoRemove
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE gmshModelGeoRemoveAllDuplicates(ierr) &
    BIND(C, NAME="gmshModelGeoRemoveAllDuplicates")
    IMPORT
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGeoRemoveAllDuplicates
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE gmshModelGeoSplitCurve( &
    tag, pointTags, pointTags_n, curveTags, curveTags_n, ierr) &
    BIND(C, NAME="gmshModelGeoSplitCurve")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: pointTags_n
    INTEGER(C_INT), INTENT(IN) :: pointTags(pointTags_n)
    TYPE(C_PTR), INTENT(IN) :: curveTags
    INTEGER(C_SIZE_T), INTENT(OUT) :: curveTags_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGeoSplitCurve
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION gmshModelGeoGetMaxTag(dim, ierr) RESULT(ans) &
    BIND(C, NAME="gmshModelGeoGetMaxTag")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim
    INTEGER(C_INT), INTENT(OUT) :: ierr
    INTEGER(C_INT) :: ans
  END FUNCTION gmshModelGeoGetMaxTag
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE gmshModelGeoSetMaxTag( &
    dim, maxTag, ierr) BIND(C, NAME="gmshModelGeoSetMaxTag")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim, maxTag
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGeoSetMaxTag
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION gmshModelGeoAddPhysicalGroup( &
    dim, tags, tags_n, tag, name, ierr) RESULT(ans) &
    BIND(C, NAME="gmshModelGeoAddPhysicalGroup")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: tags_n
    INTEGER(C_INT), INTENT(IN) :: tags(*)
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    CHARACTER(len=1, kind=C_CHAR), INTENT(in) :: name(*)
    INTEGER(C_INT), INTENT(OUT) :: ierr
    INTEGER(C_INT) :: ans
  END FUNCTION gmshModelGeoAddPhysicalGroup
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE gmshModelGeoRemovePhysicalGroups( &
    dimTags, dimTags_n, ierr) &
    BIND(C, NAME="gmshModelGeoRemovePhysicalGroups")
    IMPORT
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: dimTags_n
    INTEGER(C_INT), INTENT(IN) :: dimTags(*)
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGeoRemovePhysicalGroups
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE gmshModelGeoSynchronize(ierr) &
    BIND(C, NAME="gmshModelGeoSynchronize")
    IMPORT
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGeoSynchronize
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE GmshModelGeoInterface
