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

#ifdef USE_GMSH_SDK
MODULE GmshModelGeo_Class
USE GlobalData, ONLY: DFP, I4B, LGT
USE Utility, ONLY: Reallocate, Input
USE GmshInterface
USE GmshModelGeoMesh_Class
USE ExceptionHandler_Class, ONLY: ExceptionHandler_
USE CInterface, ONLY: C_PTR_TO_INT_VEC
USE ISO_C_BINDING
IMPLICIT NONE
PRIVATE
CHARACTER( LEN = * ), PARAMETER :: modName = "GMSHOPTION_CLASS"
INTEGER( C_INT ) :: ierr
!$OMP THREADPRIVATE(ierr)
TYPE( ExceptionHandler_ ) :: e
!$OMP THREADPRIVATE(e)
INTEGER( I4B ), PARAMETER :: maxStrLen = 120

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#define _DT_ GmshModelGeo_

TYPE :: GmshModelGeo_
  TYPE( GmshModelGeoMesh_ ), POINTER :: mesh => NULL()
  CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS( obj ) :: Initiate => geo_Initiate
  PROCEDURE, PUBLIC, PASS( Obj ) :: AddPoint => geo_AddPoint
  PROCEDURE, PUBLIC, PASS( Obj ) :: AddLine => geo_AddLine
  PROCEDURE, PUBLIC, PASS( Obj ) :: AddCircleArc => geo_AddCircleArc
  PROCEDURE, PUBLIC, PASS( Obj ) :: AddEllipseArc => geo_AddEllipseArc
  PROCEDURE, PUBLIC, PASS( Obj ) :: AddSpline => geo_AddSpline
  PROCEDURE, PUBLIC, PASS( Obj ) :: AddBSpline => geo_AddBSpline
  PROCEDURE, PUBLIC, PASS( Obj ) :: AddBezier => geo_AddBezier
  PROCEDURE, PUBLIC, PASS( Obj ) :: AddPolyline => geo_AddPolyline
  PROCEDURE, PUBLIC, PASS( Obj ) :: AddCompoundSpline => geo_AddCompoundSpline
  PROCEDURE, PUBLIC, PASS( Obj ) :: AddCompoundBSpline => &
    & geo_AddCompoundBSpline
  PROCEDURE, PUBLIC, PASS( Obj ) :: AddCurveLoop => geo_AddCurveLoop
  PROCEDURE, PUBLIC, PASS( Obj ) :: AddCurveLoops => geo_AddCurveLoops
  PROCEDURE, PUBLIC, PASS( Obj ) :: AddPlaneSurface => geo_AddPlaneSurface
  PROCEDURE, PUBLIC, PASS( Obj ) :: AddSurfaceFilling => geo_AddSurfaceFilling
  PROCEDURE, PUBLIC, PASS( Obj ) :: AddSurfaceLoop => geo_AddSurfaceLoop
  PROCEDURE, PUBLIC, PASS( Obj ) :: AddVolume => geo_AddVolume
  PROCEDURE, PUBLIC, PASS( Obj ) :: Extrude => geo_Extrude
  PROCEDURE, PUBLIC, PASS( Obj ) :: Revolve => geo_GeoRevolve
  PROCEDURE, PUBLIC, PASS( Obj ) :: Twist => geo_Twist
  PROCEDURE, PUBLIC, PASS( Obj ) :: ExtrudeBoundaryLayer => &
    & geo_ExtrudeBoundaryLayer
  PROCEDURE, PUBLIC, PASS( Obj ) :: GeoTranslate => geo_GeoTranslate
  PROCEDURE, PUBLIC, PASS( Obj ) :: GeoRotate => geo_GeoRotate
  PROCEDURE, PUBLIC, PASS( Obj ) :: GeoDilate => geo_GeoDilate
  PROCEDURE, PUBLIC, PASS( Obj ) :: Mirror => geo_Mirror
  PROCEDURE, PUBLIC, PASS( Obj ) :: Symmetrize => geo_Symmetrize
  PROCEDURE, PUBLIC, PASS( Obj ) :: Copy => geo_Copy
  PROCEDURE, PUBLIC, PASS( Obj ) :: Remove => geo_Remove
  PROCEDURE, PUBLIC, PASS( Obj ) :: RemoveAllDuplicates => &
    & geo_RemoveAllDuplicates
  PROCEDURE, PUBLIC, PASS( Obj ) :: SplitCurve => geo_SplitCurve
  PROCEDURE, PUBLIC, PASS( Obj ) :: GetMaxTag => geo_GetMaxTag
  PROCEDURE, PUBLIC, PASS( Obj ) :: SetMaxTag => geo_SetMaxTag
  PROCEDURE, PUBLIC, PASS( Obj ) :: AddPhysicalGroup => geo_AddPhysicalGroup
  PROCEDURE, PUBLIC, PASS( Obj ) :: RemovePhysicalGroups => &
    & geo_RemovePhysicalGroups
  PROCEDURE, PUBLIC, PASS( Obj ) :: Synchronize => geo_Synchronize
END TYPE GmshModelGeo_

PUBLIC :: GmshModelGeo_
TYPE( GmshModelGeo_ ), PUBLIC, PARAMETER :: TypeGmshModelGeo = GmshModelGeo_()

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: GmshModelGeoPointer_
  CLASS( GmshModelGeo_ ), POINTER :: Ptr => NULL()
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
  CLASS( GmshModelGeo_ ), INTENT( INOUT ) :: obj
  !> internal var
  CHARACTER( LEN = * ), PARAMETER :: myName="geo_Initiate"
  !> main program
  IF( ASSOCIATED( obj%Mesh )  ) THEN
    CALL e%raiseError(modName//"::"//myName//" - "// &
      & "gmsh::Model::Geo::Mesh is already associated;")
  END IF
  ALLOCATE( obj%Mesh )
END SUBROUTINE geo_Initiate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION geo_AddPoint(obj, x, y, z, meshSize, tag) RESULT( ans )
  CLASS( _DT_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: x, y, z, meshSize
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: tag
  INTEGER( I4B ) :: ans
  ! Internal variable
  ans = gmshModelGeoAddPoint(x,y,z,meshSize, INPUT(default=-1, option=tag), &
    & ierr )
END FUNCTION geo_AddPoint

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION geo_AddLine(obj, startTag, endTag, tag) RESULT( Ans )
  CLASS( _DT_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: startTag, endTag
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: tag
  INTEGER( I4B ) :: Ans
  Ans = gmshModelGeoAddLine(startTag, endTag, INPUT(default=-1,option=tag), &
    & ierr)
END FUNCTION geo_AddLine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION geo_AddCircleArc(obj,startTag, centerTag, endTag, tag, nx, ny, &
  & nz ) RESULT( Ans )
  CLASS( _DT_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: startTag, endTag, centerTag
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: tag
  REAL( DFP ), INTENT( IN ) ::  nx, ny, nz
  INTEGER( I4B ) :: Ans
  Ans = gmshModelGeoAddCircleArc(startTag, centerTag, endTag, &
    & INPUT(default=-1, option=tag), nx, &
    & ny, nz, ierr)
END FUNCTION geo_AddCircleArc

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION geo_AddEllipseArc(obj, startTag, centerTag, majorTag, endTag, &
  & tag, nx, ny, nz ) RESULT( Ans )
  CLASS( _DT_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) ::  startTag, centerTag, majorTag, endTag
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: tag
  REAL( DFP ), INTENT( IN ) ::  nx, ny, nz
  INTEGER( I4B ) :: Ans
  Ans = gmshModelGeoAddEllipseArc( startTag, centerTag, majorTag, &
    & endTag, input(default=-1, option=tag), nx, ny, nz, ierr )
END FUNCTION geo_AddEllipseArc

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION geo_AddSpline(obj, pointTags, tag) &
  & RESULT( Ans )
  CLASS( _DT_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: pointTags(:)
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: tag
  INTEGER( I4B ) :: Ans
  ! internal
  Ans = gmshModelGeoAddSpline(pointTags,  &
    & INT(SIZE(pointTags), KIND=C_SIZE_T),  &
    & INPUT(default=-1,option=tag), ierr)
END FUNCTION geo_AddSpline

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION geo_AddBSpline(obj, pointTags, tag) &
  & RESULT( Ans )
  CLASS( _DT_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: pointTags(:)
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: tag
  INTEGER( I4B ) :: Ans
  Ans = gmshModelGeoAddBSpline(pointTags, &
    & INT(SIZE(pointTags),KIND=C_SIZE_T), INPUT(default=-1,option=tag), ierr)
END FUNCTION geo_AddBSpline

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION geo_AddBezier(obj, pointTags, tag) &
  & RESULT( Ans )
  CLASS( _DT_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: pointTags(:)
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: tag
  INTEGER( I4B ) :: Ans
  Ans = gmshModelGeoAddBezier(pointTags, &
    & INT(SIZE(pointTags),KIND=C_SIZE_T), INPUT(default=-1,option=tag), ierr)
END FUNCTION geo_AddBezier

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------


FUNCTION geo_AddPolyline(obj, pointTags, tag) &
  & RESULT( Ans )
  CLASS( _DT_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: pointTags(:)
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: tag
  INTEGER( I4B ) :: Ans
  Ans = gmshModelGeoAddPolyline(pointTags, &
    & INT(SIZE(pointTags),KIND=C_SIZE_T), INPUT(default=-1,option=tag), ierr)
END FUNCTION geo_AddPolyline

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION geo_AddCompoundSpline(obj,curveTags, &
  & numIntervals, tag) RESULT( Ans )
  CLASS( _DT_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: curveTags(:)
  INTEGER( I4B ), INTENT( IN ) :: numIntervals
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: tag
  INTEGER( I4B ) :: Ans
  Ans = gmshModelGeoAddCompoundSpline(curveTags, &
    & INT(SIZE(curveTags), KIND=C_SIZE_T), &
      & numIntervals, INPUT(default=-1,option=tag), ierr)
END FUNCTION geo_AddCompoundSpline

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION geo_AddCompoundBSpline(obj,curveTags, &
  & numIntervals, tag) RESULT( Ans )
  CLASS( _DT_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: curveTags(:)
  INTEGER( I4B ), INTENT( IN ) :: numIntervals
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: tag
  INTEGER( I4B ) :: Ans
  Ans = gmshModelGeoAddCompoundBSpline(curveTags, &
    & INT(SIZE(curveTags), KIND=C_SIZE_T), &
    & numIntervals, INPUT(default=-1,option=tag), ierr)
END FUNCTION geo_AddCompoundBSpline

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION geo_AddCurveLoop(obj,curveTags, tag, reorient) &
  & RESULT( Ans )
  CLASS( _DT_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: curveTags(:)
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) ::  tag
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: reorient
  INTEGER( I4B ) :: Ans
  !
  INTEGER( I4B ) :: reorient_
  reorient_ = 1; IF( PRESENT( reorient ) ) reorient_ = reorient
  Ans = gmshModelGeoAddCurveLoop(curveTags, &
    & INT(SIZE(curveTags), C_SIZE_T),  &
    & INPUT(default=-1,option=tag), reorient_, ierr)
END FUNCTION geo_AddCurveLoop

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION geo_AddCurveLoops(obj, curveTags, tags ) &
  & RESULT( ans )
  CLASS( _DT_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: curveTags(:)
  INTEGER( I4B ), ALLOCATABLE, INTENT( OUT ) :: tags(:)
  INTEGER( I4B ) :: ans
  !
  TYPE( C_PTR ) :: cptr
  INTEGER( C_SIZE_T ) :: tags_n

  CALL gmshModelGeoAddCurveLoops(curveTags, &
    & INT(SIZE(curveTags), C_SIZE_T), cptr, tags_n, ierr)
  ans = INT(ierr, I4B)
  IF( ALLOCATED( tags ) ) DEALLOCATE( tags )
  ALLOCATE( tags( tags_n ) )
  CALL C_PTR_TO_INT_VEC( vec=tags, cptr = cptr )
END FUNCTION geo_AddCurveLoops

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION geo_AddPlaneSurface(obj,wireTags, tag) &
  & RESULT( Ans )
  CLASS( _DT_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: wireTags( : )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: tag
  INTEGER( I4B ) :: Ans
  Ans = gmshModelGeoAddPlaneSurface(wireTags, &
    & INT(SIZE(wireTags), C_SIZE_T), input(default=-1, option=tag), ierr)
END FUNCTION geo_AddPlaneSurface

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION geo_AddSurfaceFilling(obj, wireTags, tag, &
  & sphereCenterTag) RESULT( Ans )
  CLASS( _DT_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: wireTags(:)
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: tag
  INTEGER( I4B ), INTENT( IN ) :: sphereCenterTag
  INTEGER( I4B ) :: Ans
  Ans = gmshModelGeoAddSurfaceFilling(wireTags,  &
    & INT(SIZE(wireTags), C_SIZE_T), &
      & input(default=-1, option=tag), sphereCenterTag, ierr)
END FUNCTION geo_AddSurfaceFilling

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION geo_AddSurfaceLoop(obj,surfaceTags, tag ) &
  & RESULT( Ans )
  CLASS( _DT_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: surfaceTags(:)
  INTEGER( I4B ), INTENT( IN ) ::  tag
  INTEGER( I4B ) :: Ans

  Ans = gmshModelGeoAddSurfaceLoop(surfaceTags,  &
    & INT(SIZE(surfaceTags), C_SIZE_T), tag, ierr)
END FUNCTION geo_AddsurfaceLoop

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION geo_AddVolume(obj, shellTags, tag) &
  & RESULT( Ans )
  CLASS( _DT_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) ::  shellTags(:)
  INTEGER( I4B ), INTENT( IN ) :: tag
  INTEGER( I4B ) :: Ans
  Ans = gmshModelGeoAddVolume(shellTags,  &
    & INT(SIZE(shellTags), C_SIZE_T), tag, ierr)
END FUNCTION geo_AddVolume

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION geo_Extrude(obj, dimTags, dx, dy, dz, &
  & outDimTags, numElements, heights, recombine ) RESULT( ans )
  CLASS( _DT_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: dimTags(:)
  REAL( DFP ), INTENT( IN ) :: dx, dy, dz
  INTEGER( I4B ), ALLOCATABLE, INTENT( OUT ) :: outDimTags( : )
  INTEGER( I4B ), INTENT( IN ) :: numElements(:)
  REAL( DFP ), INTENT( IN ) :: heights(:)
  INTEGER( I4B ), INTENT( IN ) :: recombine
  INTEGER( I4B ) :: ans

  INTEGER( C_SIZE_T ) :: outDimTags_n
  TYPE( C_PTR ) :: cptr

  CALL gmshModelGeoExtrude(dimTags, INT(SIZE(dimTags), C_SIZE_T), dx, &
    & dy, dz, cptr, outDimTags_n, numElements, INT(SIZE(numElements), &
    & C_SIZE_T), heights, INT(SIZE(heights), C_SIZE_T), recombine, &
    & ierr )
  ans = INT(ierr, I4B)
  CALL Reallocate( outDimTags, INT(outDimTags_n, I4B) )
  CALL C_PTR_TO_INT_VEC( vec = outDimTags, cptr = cptr )
END FUNCTION geo_Extrude

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION geo_GeoRevolve(obj, dimTags, x, y, z, ax, ay, az, &
  & angle, outDimTags, numElements, heights, recombine) &
  & RESULT( ans )
  CLASS( _DT_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: dimTags(:)
  REAL( DFP ), INTENT( IN ) :: x, y, z, ax, ay, az, angle
  INTEGER( I4B ), ALLOCATABLE, INTENT( OUT ) :: outDimTags( : )
  INTEGER( I4B ), INTENT( IN ) :: numElements( : )
  REAL( DFP ), INTENT( IN ) :: heights( : )
  INTEGER( I4B ), INTENT( IN ) :: recombine
  INTEGER( I4B ) :: ans

  INTEGER( C_SIZE_T ) :: outDimTags_n
  TYPE( C_PTR ) :: cptr
  CALL gmshModelGeoRevolve( dimTags, INT(SIZE(dimTags), C_SIZE_T), x, &
    & y, z, ax, ay, az, angle, cptr, outDimTags_n, numElements, &
    & INT(SIZE(numElements), C_SIZE_T), heights, &
    & INT(SIZE(heights), C_SIZE_T), recombine, ierr )
  ans = INT(ierr, I4B)
  CALL Reallocate( outDimTags, INT(outDimTags_n, I4B) )
  CALL C_PTR_TO_INT_VEC( vec=outDimTags, cptr=cptr )
END FUNCTION geo_GeoRevolve

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION geo_Twist( obj, dimTags, x, y, z, dx, dy, dz, ax, &
  & ay, az, angle, outDimTags, numElements, heights, recombine ) &
  & RESULT( ans )
  CLASS( _DT_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: dimTags( : )
  REAL( DFP ), INTENT( IN ) :: x, y, z, dx, dy, dz, ax, ay, az, angle
  INTEGER( I4B ), ALLOCATABLE, INTENT( OUT ) :: outDimTags( : )
  INTEGER( I4B ), INTENT( IN ) :: numElements(:)
  REAL( DFP ), INTENT( IN ) :: heights(:)
  INTEGER( I4B ), INTENT( IN ) :: recombine
  INTEGER( I4B ) :: ans
  !
  TYPE( C_PTR ) :: cptr
  INTEGER( C_SIZE_T ) :: outDimTags_n
  CALL gmshModelGeoTwist(dimTags, SIZE(dimTags, KIND=C_SIZE_T), x, y, &
    & z, dx, dy, dz, ax, ay, az, angle, cptr, outDimTags_n, &
    & numElements, SIZE(numElements, KIND=C_SIZE_T), heights, &
    & SIZE(heights, KIND=C_SIZE_T), recombine, ierr)
  ans = INT(ierr, I4B)
  CALL Reallocate(outDimTags, int(outDimTags_n, i4b))
  CALL C_PTR_TO_INT_VEC( vec=outDimTags, cptr=cptr )
END FUNCTION geo_Twist

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION geo_ExtrudeBoundaryLayer( obj, dimTags, &
  & outDimTags, numElements, &
  & heights, recombine, second, viewIndex ) &
  & RESULT( ans )
  CLASS( _DT_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: dimTags( : )
  INTEGER( I4B ), ALLOCATABLE, INTENT( OUT ) :: outDimTags( : )
  INTEGER( I4B ), INTENT( IN ) :: numElements(:)
  REAL( DFP ), INTENT( IN ) :: heights(:)
  INTEGER( I4B ), INTENT( IN ) :: recombine, second, viewIndex
  INTEGER( I4B ) :: ans
  !
  TYPE( C_PTR ) :: cptr
  INTEGER( C_SIZE_T ) :: outDimTags_n
  CALL gmshModelGeoExtrudeBoundaryLayer( dimTags, &
    & SIZE(dimTags, kind=c_size_t), cptr, outDimTags_n, &
    & numElements, SIZE(numElements, kind=c_size_t), &
    & heights, SIZE(heights, kind=c_size_t), recombine, second, &
    & viewIndex, ierr )
  ans = INT(ierr, I4B)
  CALL Reallocate(outDimTags, int(outDimTags_n, i4b))
  CALL C_PTR_TO_INT_VEC( vec=outDimTags, cptr=cptr )
END FUNCTION geo_ExtrudeBoundaryLayer

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION geo_GeoTranslate(obj, dimTags, dx, dy, dz) &
  & RESULT( ans )
  CLASS( _DT_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: dimTags(:)
  REAL( DFP ), INTENT( IN ) :: dx, dy, dz
  INTEGER( I4B ) :: ans
  CALL gmshModelGeoTranslate(dimTags, &
    & size(dimTags, kind=c_size_t), dx, dy, dz, ierr)
  ans = INT(ierr, I4B)
END FUNCTION geo_GeoTranslate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION geo_GeoRotate(obj, dimTags, x, y, z, ax, ay, az, &
  & angle) RESULT( ans )
  CLASS( _DT_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: dimTags(:)
  REAL( DFP ), INTENT( IN ) :: x, y, z, ax, ay, az, angle
  INTEGER( I4B ) :: ans
  CALL gmshModelGeoRotate(dimTags, size(dimTags, kind=c_size_t), x, &
    & y, z, ax, ay, az, angle, ierr)
  ans = INT(ierr, I4B)
END FUNCTION geo_GeoRotate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION geo_GeoDilate(obj, dimTags, x, y, z, a, b, c) &
  & RESULT(ans)
  CLASS( _DT_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: dimTags(:)
  REAL( DFP ), INTENT( IN ) :: x, y, z, a, b, c
  INTEGER( I4B ) :: ans

  CALL gmshModelGeoDilate(dimTags, size(dimTags, kind=c_size_t), x, &
    & y, z, a, b, c, ierr)
  ans = INT(ierr, I4B)
END FUNCTION geo_GeoDilate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION geo_Mirror(obj, dimTags, a, b, c, d) &
  & RESULT( ans )
  CLASS( _DT_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: dimTags(:)
  REAL( DFP ), INTENT( IN ) :: a, b, c, d
  INTEGER( I4B ) :: ans
  CALL gmshModelGeoMirror(dimTags, size(dimTags, kind=c_size_t), a, b, &
    & c, d, ierr)
  ans = INT(ierr, I4B)
END FUNCTION geo_Mirror

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION geo_Symmetrize(obj, dimTags, a, b, c, d) &
  & RESULT( ans )
  CLASS( _DT_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: dimTags(:)
  REAL( DFP ), INTENT( IN ) :: a, b, c, d
  INTEGER( I4B ) :: ans
  CALL gmshModelGeoSymmetrize(dimTags, size(dimTags, kind=c_size_t), a, &
    & b, c, d, ierr)
  ans = INT(ierr, I4B)
END FUNCTION geo_Symmetrize

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION geo_Copy(obj, dimTags, outDimTags ) &
  & RESULT( ans )
  CLASS( _DT_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: dimTags(:)
  INTEGER( I4B ), ALLOCATABLE, INTENT( OUT ) :: outDimTags( : )
  INTEGER( I4B ) :: ans
  ! internal
  TYPE( C_PTR ) :: cptr
  INTEGER( C_SIZE_T ) :: outDimTags_n

  CALL gmshModelGeoCopy(dimTags, size(dimTags, kind=c_size_t), cptr, &
    & outDimTags_n, ierr)
  ans = INT(ierr, I4B)
  CALL Reallocate(outDimTags, int(outDimTags_n, kind=i4b))
  CALL C_PTR_TO_INT_VEC( vec=outDimTags, cptr=cptr )
END FUNCTION geo_Copy

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION geo_Remove(obj, dimTags, recursive) &
  & RESULT( ans )
  CLASS( _DT_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: dimTags(:)
  INTEGER( I4B ), INTENT( IN ) :: recursive
  INTEGER( I4B ) :: ans
  CALL gmshModelGeoRemove(dimTags, size(dimTags, kind=c_size_t), &
    & recursive, ierr)
  ans = INT(ierr, I4B)
END FUNCTION geo_Remove

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION geo_RemoveAllDuplicates(obj) RESULT( ans )
  CLASS( _DT_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ) :: ans
  CALL gmshModelGeoRemoveAllDuplicates(ierr)
  ans = INT(ierr, I4B)
END FUNCTION geo_RemoveAllDuplicates

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION geo_SplitCurve(obj, tag, pointTags, curveTags) &
  RESULT( ans )
  CLASS( _DT_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: tag
  INTEGER( I4B ), INTENT( IN ) :: pointTags(:)
  INTEGER( I4B ), ALLOCATABLE, INTENT( OUT ) :: curveTags( : )
  INTEGER( I4B ) :: ans

  ! internal
  TYPE( C_PTR ) :: cptr
  INTEGER( C_SIZE_T ) :: curveTags_n
  CALL gmshModelGeoSplitCurve(tag, pointTags, &
    & size(pointTags, kind=c_size_t), cptr, curveTags_n, ierr)

  CALL Reallocate( curveTags, int(curveTags_n, i4b))
  CALL C_PTR_TO_INT_VEC( vec=curveTags, cptr=cptr )
END FUNCTION geo_SplitCurve

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION geo_GetMaxTag(obj,dim) RESULT( ans )
  CLASS( _DT_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: dim
  INTEGER( I4B ) :: ans
  ans = gmshModelGeoGetMaxTag( dim, ierr )
END FUNCTION geo_GetMaxTag

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION geo_SetMaxTag(obj, dim, maxTag) RESULT( ans )
  CLASS( _DT_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: dim, maxTag
  INTEGER( I4B ) :: ans
  CALL gmshModelGeoSetMaxTag( dim, maxTag, ierr )
  ans = INT( ierr, I4B )
END FUNCTION geo_SetMaxTag

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION geo_AddPhysicalGroup(obj, dim, tags, tag) &
  & RESULT( ans )
  CLASS( _DT_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: dim
  INTEGER( I4B ), INTENT( IN ) :: tags(:)
  INTEGER( I4B ), INTENT( IN ) :: tag
  INTEGER( I4B ) :: ans
  ans = gmshModelGeoAddPhysicalGroup(dim, tags, &
    & size(tags,kind=c_size_t), tag, ierr)
END FUNCTION geo_AddPhysicalGroup

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION geo_RemovePhysicalGroups(obj, dimTags) &
  & RESULT( ans )
  CLASS( _DT_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: dimTags(:)
  INTEGER( I4B ) :: ans
  CALL gmshModelGeoRemovePhysicalGroups( dimTags, &
    & size(dimTags, kind=c_size_t), ierr)
  ans = INT(ierr, I4B)
END FUNCTION geo_RemovePhysicalGroups

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION geo_Synchronize(obj) RESULT( ans )
  CLASS( _DT_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ) :: ans
  CALL gmshModelGeoSynchronize(ierr)
  ans = INT(ierr, I4B)
END FUNCTION geo_Synchronize

END MODULE GmshModelGeo_Class
#endif