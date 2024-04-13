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

INTERFACE
  FUNCTION gmshModelGeoAddPoint(x, y, z, meshSize, tag, ierr) RESULT(ans) &
    & BIND(C, NAME="gmshModelGeoAddPoint")
    IMPORT
    _R_V_IN_ :: x
    _R_V_IN_ :: y
    _R_V_IN_ :: z
    _R_V_IN_ :: meshSize
    _I_V_IN_ :: tag
    _I_OUT_ :: ierr
    INTEGER(C_INT) :: ans
  END FUNCTION gmshModelGeoAddPoint
END INTERFACE

PUBLIC :: gmshModelGeoAddPoint

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION gmshModelGeoAddLine(startTag, endTag, tag, ierr) RESULT(ans) &
    & BIND(C, NAME="gmshModelGeoAddLine")
    IMPORT
    _I_V_IN_ :: startTag, endTag, tag
    _I_OUT_ :: ierr
    INTEGER(C_INT) :: ans
  END FUNCTION gmshModelGeoAddLine
END INTERFACE

PUBLIC :: gmshModelGeoAddLine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION gmshModelGeoAddCircleArc(startTag, centerTag, endTag, tag, &
    & nx, ny, &
    & nz, ierr) RESULT(ans) &
    & BIND(C, NAME="gmshModelGeoAddCircleArc")
    IMPORT
    _I_V_IN_ :: startTag, endTag, tag, centerTag
    _R_V_IN_ :: nx, ny, nz
    _I_OUT_ :: ierr
    INTEGER(C_INT) :: ans
  END FUNCTION gmshModelGeoAddCircleArc
END INTERFACE

PUBLIC :: gmshModelGeoAddCircleArc

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION gmshModelGeoAddEllipseArc(startTag, centerTag, majorTag, endTag, &
    & tag, nx, ny, nz, ierr) RESULT(ans) &
    & BIND(C, NAME="gmshModelGeoAddEllipseArc")
    IMPORT
    _I_V_IN_ :: startTag, centerTag, majorTag, endTag, tag
    _R_V_IN_ :: nx, ny, nz
    _I_OUT_ :: ierr
    INTEGER(C_INT) :: ans
  END FUNCTION gmshModelGeoAddEllipseArc
END INTERFACE

PUBLIC :: gmshModelGeoAddEllipseArc

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION gmshModelGeoAddSpline(pointTags, pointTags_n, tag, ierr) &
    & RESULT(ans) &
    & BIND(C, NAME="gmshModelGeoAddSpline")
    IMPORT
    _ST_V_IN_ :: pointTags_n
    _I_IN_ :: pointTags(pointTags_n)
    _I_V_IN_ :: tag
    _I_OUT_ :: ierr
    INTEGER(C_INT) :: ans
  END FUNCTION gmshModelGeoAddSpline
END INTERFACE

PUBLIC :: gmshModelGeoAddSpline

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION gmshModelGeoAddBSpline(pointTags, pointTags_n, tag, ierr) &
    & RESULT(ans) &
    & BIND(C, NAME="gmshModelGeoAddBSpline")
    IMPORT
    _ST_V_IN_ :: pointTags_n
    _I_IN_ :: pointTags(pointTags_n)
    _I_V_IN_ :: tag
    _I_OUT_ :: ierr
    INTEGER(C_INT) :: ans
  END FUNCTION gmshModelGeoAddBSpline
END INTERFACE

PUBLIC :: gmshModelGeoAddBSpline

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION gmshModelGeoAddBezier(pointTags, pointTags_n, tag, ierr) &
    & RESULT(ans) &
    & BIND(C, NAME="gmshModelGeoAddBezier")
    IMPORT
    _ST_V_IN_ :: pointTags_n
    _I_IN_ :: pointTags(pointTags_n)
    _I_V_IN_ :: tag
    _I_OUT_ :: ierr
    INTEGER(C_INT) :: ans
  END FUNCTION gmshModelGeoAddBezier
END INTERFACE

PUBLIC :: gmshModelGeoAddBezier

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION gmshModelGeoAddPolyline(pointTags, pointTags_n, tag, ierr) &
    & RESULT(ans) &
    & BIND(C, NAME="gmshModelGeoAddPolyline")
    IMPORT
    _ST_V_IN_ :: pointTags_n
    _I_IN_ :: pointTags(pointTags_n)
    _I_V_IN_ :: tag
    _I_OUT_ :: ierr
    INTEGER(C_INT) :: ans
  END FUNCTION gmshModelGeoAddPolyline
END INTERFACE

PUBLIC :: gmshModelGeoAddPolyline

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
FUNCTION gmshModelGeoAddCompoundSpline(curveTags, curveTags_n, numIntervals, &
                        & tag, ierr) RESULT(ans) &
                        & BIND(C, NAME="gmshModelGeoAddCompoundSpline")
    IMPORT
    _ST_V_IN_ :: curveTags_n
    _I_IN_ :: curveTags(curveTags_n)
    _I_V_IN_ :: numIntervals, tag
    _I_OUT_ :: ierr
    INTEGER(C_INT) :: ans
  END FUNCTION gmshModelGeoAddCompoundSpline
END INTERFACE

PUBLIC :: gmshModelGeoAddCompoundSpline

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION gmshModelGeoAddCompoundBSpline(curveTags, curveTags_n, &
    & numIntervals, tag, ierr) RESULT(ans) &
    & BIND(C, NAME="gmshModelGeoAddCompoundBSpline")
    IMPORT
    _ST_V_IN_ :: curveTags_n
    _I_IN_ :: curveTags(curveTags_n)
    _I_V_IN_ :: numIntervals, tag
    _I_OUT_ :: ierr
    INTEGER(C_INT) :: ans
  END FUNCTION gmshModelGeoAddCompoundBSpline
END INTERFACE

PUBLIC :: gmshModelGeoAddCompoundBSpline

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION gmshModelGeoAddCurveLoop(curveTags, curveTags_n, tag, reorient,&
    & ierr) RESULT(ans) &
    & BIND(C, NAME="gmshModelGeoAddCurveLoop")
    IMPORT
    _ST_V_IN_ :: curveTags_n
    _I_IN_ :: curveTags(curveTags_n)
    _I_V_IN_ :: tag, reorient
    _I_OUT_ :: ierr
    INTEGER(C_INT) :: ans
  END FUNCTION gmshModelGeoAddCurveLoop
END INTERFACE

PUBLIC :: gmshModelGeoAddCurveLoop

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE gmshModelGeoAddCurveLoops(curveTags, curveTags_n, tags, tags_n,&
    & ierr) &
    & BIND(C, NAME="gmshModelGeoAddCurveLoops")
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

INTERFACE
  FUNCTION gmshModelGeoAddPlaneSurface(wireTags, wireTags_n, tag, ierr) &
    & RESULT(ans) &
    & BIND(C, NAME="gmshModelGeoAddPlaneSurface")
    IMPORT
    _ST_V_IN_ :: wireTags_n
    _I_IN_ :: wireTags(wireTags_n)
    _I_V_IN_ :: tag
    _I_OUT_ :: ierr
    INTEGER(C_INT) :: ans
  END FUNCTION gmshModelGeoAddPlaneSurface
END INTERFACE

PUBLIC :: gmshModelGeoAddPlaneSurface

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION gmshModelGeoAddSurfaceFilling(wireTags, wireTags_n, tag, &
    & sphereCenterTag, ierr) &
    & RESULT(ans) &
    & BIND(C, NAME="gmshModelGeoAddSurfaceFilling")
    IMPORT
    _ST_V_IN_ :: wireTags_n
    _I_IN_ :: wireTags(wireTags_n)
    _I_V_IN_ :: tag
    _I_V_IN_ :: sphereCenterTag
    _I_OUT_ :: ierr
    INTEGER(C_INT) :: ans
  END FUNCTION gmshModelGeoAddSurfaceFilling
END INTERFACE

PUBLIC :: gmshModelGeoAddSurfaceFilling

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION gmshModelGeoAddSurfaceLoop(surfaceTags, surfaceTags_n, tag, ierr) &
    & RESULT(ans) &
    & BIND(C, NAME="gmshModelGeoAddSurfaceLoop")
    IMPORT
    _ST_V_IN_ :: surfaceTags_n
    _I_IN_ :: surfaceTags(surfaceTags_n)
    _I_V_IN_ :: tag
    _I_OUT_ :: ierr
    INTEGER(C_INT) :: ans
  END FUNCTION gmshModelGeoAddSurfaceLoop
END INTERFACE

PUBLIC :: gmshModelGeoAddSurfaceLoop

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION gmshModelGeoAddVolume(shellTags, shellTags_n, tag, ierr) &
    & RESULT(ans) &
    & BIND(C, NAME="gmshModelGeoAddVolume")
    IMPORT
    _ST_V_IN_ :: shellTags_n
    _I_IN_ :: shellTags(shellTags_n)
    _I_V_IN_ :: tag
    _I_OUT_ :: ierr
    INTEGER(C_INT) :: ans
  END FUNCTION gmshModelGeoAddVolume
END INTERFACE

PUBLIC :: gmshModelGeoAddVolume

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

interface
  function gmshModelGeoAddGeometry(geometry, numbers, numbers_n, strings, &
      & strings_n, tag, ierr) result(ans) &
      & bind(C, name="gmshModelGeoAddGeometry")
    import
    character(len=1, kind=c_char), dimension(*), intent(in) :: geometry
    _ST_V_IN_ :: numbers_n
    _ST_V_IN_ :: strings_n
    _R_IN_ :: numbers(numbers_n)
    _CPTR_IN_ :: strings(strings_n)
    _I_V_IN_ :: tag
    _I_OUT_ :: ierr
    INTEGER(c_int) :: ans
  end function gmshModelGeoAddGeometry
end interface

PUBLIC :: gmshModelGeoAddGeometry

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

interface
  function gmshModelGeoAddPointOnGeometry(geometryTag, x, y, z, meshSize, &
    & tag, ierr) result(ans) &
    bind(C, name="gmshModelGeoAddPointOnGeometry")
    import
    _I_V_IN_ :: geometryTag, tag
    _R_V_IN_ :: x, y, z, meshSize
    _I_OUT_ :: ierr
    integer(c_int) :: ans
  end function gmshModelGeoAddPointOnGeometry
end interface

PUBLIC :: gmshModelGeoAddPointOnGeometry

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE gmshModelGeoExtrude(dimTags, dimTags_n, dx, dy, dz, outDimTags, &
  & outDimTags_n, numElements, numElements_n, heights, heights_n, recombine, &
    & ierr) &
    & BIND(C, NAME="gmshModelGeoExtrude")
    IMPORT
    _ST_V_IN_ :: dimTags_n
    _I_IN_ :: dimTags(*)
    _R_V_IN_ :: dx, dy, dz
    _CPTR_IN_ :: outDimTags
    _ST_OUT_ :: outDimTags_n
    _ST_V_IN_ :: numElements_n
    _I_IN_ :: numElements(numElements_n)
    _ST_V_IN_ :: heights_n
    _R_IN_ :: heights(heights_n)
    _I_V_IN_ :: recombine
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelGeoExtrude
END INTERFACE

PUBLIC :: gmshModelGeoExtrude

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE gmshModelGeoRevolve(dimTags, dimTags_n, x, y, z, ax, ay, az, &
    & angle, outDimTags, outDimTags_n, numElements, numElements_n, heights, &
    & heights_n, recombine, ierr) &
    & BIND(C, NAME="gmshModelGeoRevolve")
    IMPORT
    _ST_V_IN_ :: dimTags_n
    _I_IN_ :: dimTags(*)
    _R_V_IN_ :: x, y, z, ax, ay, az, angle
    _CPTR_IN_ :: outDimTags
    _ST_OUT_ :: outDimTags_n
    _ST_V_IN_ :: numElements_n
    _I_IN_ :: numElements(numElements_n)
    _ST_V_IN_ :: heights_n
    _R_IN_ :: heights(heights_n)
    _I_V_IN_ :: recombine
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelGeoRevolve
END INTERFACE

PUBLIC :: gmshModelGeoRevolve

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE gmshModelGeoTwist(dimTags, dimTags_n, x, y, z, dx, dy, dz, ax, &
    & ay, az, angle, outDimTags, outDimTags_n, numElements, numElements_n, &
    & heights, heights_n, recombine, ierr) &
    & BIND(C, NAME="gmshModelGeoTwist")
    IMPORT
    _ST_V_IN_ :: dimTags_n
    _I_IN_ :: dimTags(*)
    _R_V_IN_ :: x, y, z, dx, dy, dz, ax, ay, az, angle
    _CPTR_IN_ :: outDimTags
    _ST_OUT_ :: outDimTags_n
    _ST_V_IN_ :: numElements_n
    _I_IN_ :: numElements(numElements_n)
    _ST_V_IN_ :: heights_n
    _R_IN_ :: heights(heights_n)
    _I_V_IN_ :: recombine
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelGeoTwist
END INTERFACE

PUBLIC :: gmshModelGeoTwist

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE gmshModelGeoExtrudeBoundaryLayer(dimTags, dimTags_n, &
    & outDimTags, outDimTags_n, numElements, numElements_n, &
    & heights, heights_n, recombine, second, viewIndex, ierr) &
    & BIND(C, NAME="gmshModelGeoExtrudeBoundaryLayer")
    IMPORT
    _ST_V_IN_ :: dimTags_n
    _I_IN_ :: dimTags(*)
    _CPTR_IN_ :: outDimTags
    _ST_OUT_ :: outDimTags_n
    _ST_V_IN_ :: numElements_n
    _I_IN_ :: numElements(numElements_n)
    _ST_V_IN_ :: heights_n
    _R_IN_ :: heights(heights_n)
    _I_V_IN_ :: recombine, second, viewIndex
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelGeoExtrudeBoundaryLayer
END INTERFACE

PUBLIC :: gmshModelGeoExtrudeBoundaryLayer

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE gmshModelGeoTranslate(dimTags, dimTags_n, dx, dy, dz, ierr) &
    & BIND(C, NAME="gmshModelGeoTranslate")
    IMPORT
    _ST_V_IN_ :: dimTags_n
    _I_IN_ :: dimTags(*)
    _R_V_IN_ :: dx, dy, dz
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelGeoTranslate
END INTERFACE

PUBLIC :: gmshModelGeoTranslate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE gmshModelGeoRotate(dimTags, dimTags_n, x, y, z, ax, ay, az, &
    & angle, ierr) &
    & BIND(C, NAME="gmshModelGeoRotate")
    IMPORT
    _ST_V_IN_ :: dimTags_n
    _I_IN_ :: dimTags(*)
    _R_V_IN_ :: x, y, z, ax, ay, az, angle
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelGeoRotate
END INTERFACE

PUBLIC :: gmshModelGeoRotate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE gmshModelGeoDilate(dimTags, dimTags_n, x, y, z, a, b, c, ierr) &
    & BIND(C, NAME="gmshModelGeoDilate")
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

INTERFACE
  SUBROUTINE gmshModelGeoMirror(dimTags, dimTags_n, a, b, c, d, ierr) &
    & BIND(C, NAME="gmshModelGeoMirror")
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

INTERFACE
  SUBROUTINE gmshModelGeoSymmetrize(dimTags, dimTags_n, a, b, c, d, ierr) &
    & BIND(C, NAME="gmshModelGeoSymmetrize")
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

INTERFACE
  SUBROUTINE gmshModelGeoCopy(dimTags, dimTags_n, outDimTags, outDimTags_n, &
    & ierr) &
    & BIND(C, NAME="gmshModelGeoCopy")
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

INTERFACE
  SUBROUTINE gmshModelGeoRemove(dimTags, dimTags_n, recursive, ierr) &
    & BIND(C, NAME="gmshModelGeoRemove")
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

INTERFACE
  SUBROUTINE gmshModelGeoRemoveAllDuplicates(ierr) &
    & BIND(C, NAME="gmshModelGeoRemoveAllDuplicates")
    IMPORT
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelGeoRemoveAllDuplicates
END INTERFACE

PUBLIC :: gmshModelGeoRemoveAllDuplicates

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE gmshModelGeoSplitCurve(tag, pointTags, pointTags_n, curveTags, &
    & curveTags_n, ierr) &
    & BIND(C, NAME="gmshModelGeoSplitCurve")
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

INTERFACE
  FUNCTION gmshModelGeoGetMaxTag(dim, ierr) RESULT(ans) &
    & BIND(C, NAME="gmshModelGeoGetMaxTag")
    IMPORT
    _I_V_IN_ :: dim
    _I_OUT_ :: ierr
    INTEGER(C_INT) :: ans
  END FUNCTION gmshModelGeoGetMaxTag
END INTERFACE

PUBLIC :: gmshModelGeoGetMaxTag

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE gmshModelGeoSetMaxTag(dim, maxTag, ierr) &
    & BIND(C, NAME="gmshModelGeoSetMaxTag")
    IMPORT
    _I_V_IN_ :: dim, maxTag
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelGeoSetMaxTag
END INTERFACE

PUBLIC :: gmshModelGeoSetMaxTag

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  FUNCTION gmshModelGeoAddPhysicalGroup(dim, tags, tags_n, tag, name, ierr) &
    & RESULT(ans) &
    & BIND(C, NAME="gmshModelGeoAddPhysicalGroup")
    IMPORT
    _I_V_IN_ :: dim
    _ST_V_IN_ :: tags_n
    _I_IN_ :: tags(*)
    _I_V_IN_ :: tag
    character(len=1, kind=c_char), intent(in) :: name(*)
    _I_OUT_ :: ierr
    INTEGER(C_INT) :: ans
  END FUNCTION gmshModelGeoAddPhysicalGroup
END INTERFACE

PUBLIC :: gmshModelGeoAddPhysicalGroup

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE gmshModelGeoRemovePhysicalGroups(dimTags, dimTags_n, ierr) &
    & BIND(C, NAME="gmshModelGeoRemovePhysicalGroups")
    IMPORT
    _ST_V_IN_ :: dimTags_n
    _I_IN_ :: dimTags(*)
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelGeoRemovePhysicalGroups
END INTERFACE

PUBLIC :: gmshModelGeoRemovePhysicalGroups

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE gmshModelGeoSynchronize(ierr) &
    & BIND(C, NAME="gmshModelGeoSynchronize")
    IMPORT
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelGeoSynchronize
END INTERFACE

PUBLIC :: gmshModelGeoSynchronize
