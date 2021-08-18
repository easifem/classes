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
USE GlobalData, ONLY: DFP, I4B, LGT, PI
USE Utility, ONLY: Reallocate, input
USE GmshInterface
USE GmshModelOccMesh_Class
USE CInterface, ONLY: C_PTR_TO_INT_VEC
USE ISO_C_BINDING
IMPLICIT NONE
PRIVATE
CHARACTER( LEN = * ), PARAMETER :: modName = "GMSHMODELOCC_CLASS"
INTEGER( C_INT ) :: ierr
!$OMP THREADPRIVATE(ierr)
INTEGER( I4B ), PARAMETER :: maxStrLen = 256
REAL( DFP ), PARAMETER, DIMENSION(0) :: emptyReal=0
INTEGER( I4B ), PARAMETER, DIMENSION(1) :: emptyInt=0

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: GmshModelOcc_
  PRIVATE
  TYPE( GmshModelOccMesh_ ), PUBLIC, POINTER :: Mesh => NULL()
  CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS( Obj ) :: AddPoint => occ_AddPoint
  PROCEDURE, PUBLIC, PASS( Obj ) :: AddLine => occ_AddLine
  PROCEDURE, PUBLIC, PASS( Obj ) :: AddCircleArc => occ_AddCircleArc
  PROCEDURE, PUBLIC, PASS( Obj ) :: AddCircle => occ_AddCircle
  PROCEDURE, PUBLIC, PASS( Obj ) :: AddEllipseArc => occ_AddEllipseArc
  PROCEDURE, PUBLIC, PASS( Obj ) :: AddEllipse => occ_AddEllipse
  PROCEDURE, PUBLIC, PASS( Obj ) :: AddSpline => occ_AddSpline
  PROCEDURE, PUBLIC, PASS( Obj ) :: AddBSpline => occ_AddBSpline
  PROCEDURE, PUBLIC, PASS( Obj ) :: AddBezier => occ_AddBezier
  PROCEDURE, PUBLIC, PASS( Obj ) :: AddWire => occ_AddWire
  PROCEDURE, PUBLIC, PASS( Obj ) :: AddCurveLoop => occ_AddCurveLoop
  PROCEDURE, PUBLIC, PASS( Obj ) :: AddRectangle => occ_AddRectangle
  PROCEDURE, PUBLIC, PASS( Obj ) :: AddDisk => occ_AddDisk
  PROCEDURE, PUBLIC, PASS( Obj ) :: AddPlaneSurface => occ_AddPlaneSurface
  PROCEDURE, PUBLIC, PASS( Obj ) :: AddSurfaceFilling => occ_AddSurfaceFilling
  PROCEDURE, PUBLIC, PASS( Obj ) :: AddBSplineFilling => occ_AddBSplineFilling
  PROCEDURE, PUBLIC, PASS( Obj ) :: AddBezierFilling => occ_AddBezierFilling
  PROCEDURE, PUBLIC, PASS( Obj ) :: AddBSplineSurface => occ_AddBSplineSurface
  PROCEDURE, PUBLIC, PASS( Obj ) :: AddTrimmedSurface => occ_AddTrimmedSurface
  PROCEDURE, PUBLIC, PASS( Obj ) :: AddSurfaceLoop => occ_AddSurfaceLoop
  PROCEDURE, PUBLIC, PASS( Obj ) :: AddVolume => occ_AddVolume
  PROCEDURE, PUBLIC, PASS( Obj ) :: AddSphere => occ_AddSphere
  PROCEDURE, PUBLIC, PASS( Obj ) :: AddBox => occ_AddBox
  PROCEDURE, PUBLIC, PASS( Obj ) :: AddCylinder => occ_AddCylinder
  PROCEDURE, PUBLIC, PASS( Obj ) :: AddCone => occ_AddCone
  PROCEDURE, PUBLIC, PASS( Obj ) :: AddWedge => occ_AddWedge
  PROCEDURE, PUBLIC, PASS( Obj ) :: AddTorus => occ_AddTorus
  PROCEDURE, PUBLIC, PASS( Obj ) :: AddThruSections => Occ_AddThruSections



  PROCEDURE, PUBLIC, PASS( Obj ) :: Synchronize => occ_Synchronize
END TYPE GmshModelOcc_

PUBLIC :: GmshModelOcc_
TYPE( GmshModelOcc_ ), PUBLIC, PARAMETER :: TypeGmshModelOcc = GmshModelOcc_()

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: GmshModelOccPointer_
  CLASS( GmshModelOcc_ ), POINTER :: Ptr => NULL()
END TYPE

PUBLIC :: GmshModelOccPointer_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION occ_AddPoint(obj,x,y,z,meshSize,tag) &
  & RESULT( ans )
  CLASS( GmshModelOcc_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: x, y, z, meshSize
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: tag
  INTEGER( I4B ) :: ans
  ans = gmshModelOccAddPoint(x,y,z,meshSize,input(-1,tag),ierr)
END FUNCTION occ_AddPoint

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION occ_AddLine(obj, startTag, endTag, tag) &
  & RESULT( ans )
  CLASS( GmshModelOcc_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: tag
  INTEGER( I4B ), INTENT( IN ) :: startTag, endTag
  INTEGER( I4B ) :: ans
  ans = gmshModelOccAddLine(startTag, endTag, input(-1,tag), ierr)
END FUNCTION occ_AddLine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION occ_AddCircleArc(obj, startTag, centerTag, endTag, tag) &
  & RESULT( ans )
  CLASS( GmshModelOcc_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: tag
  INTEGER( I4B ), INTENT( IN ) :: startTag, centerTag, endTag
  INTEGER( I4B ) :: ans
  ans = gmshModelOccAddCircleArc(startTag, centerTag, endTag, input(-1,tag), ierr)
END FUNCTION occ_AddCircleArc

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION occ_AddCircle(obj, x, y, z, r, tag, angle1, angle2) &
  & RESULT( ans )
  CLASS( GmshModelOcc_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: tag
  REAL( DFP ), INTENT( IN ) :: x, y, z, r
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: angle1, angle2
  INTEGER( I4B ) :: ans
  ans = gmshModelOccAddCircle(x, y, z, r, input(-1,tag), &
  & input(option=angle1, default=0.0_DFP), input(option=angle2, &
  & default=2.0*PI), ierr)
END FUNCTION occ_AddCircle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION occ_AddEllipseArc(obj, startTag, centerTag, majorTag, endTag, &
  & tag) RESULT( ans )
  CLASS( GmshModelOcc_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: tag
  INTEGER( I4B ), INTENT( IN ) :: startTag, centerTag, majorTag, endTag
  INTEGER( I4B ) :: ans
  !
  ans = gmshModelOccAddEllipseArc(startTag, centerTag, majorTag, &
    & endTag, input(-1,tag), ierr)
END FUNCTION occ_AddEllipseArc

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION occ_AddEllipse(obj,x, y, z, r1, r2, tag, angle1, angle2)&
  & RESULT( ans )
  CLASS( GmshModelOcc_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: tag
  REAL( DFP ), INTENT( IN ) :: x, y, z, r1, r2, angle1, angle2
  INTEGER( I4B ) :: ans
  ans = gmshModelOccAddEllipse(x, y, z, r1, r2, input(-1,tag), angle1, &
    & angle2, ierr)
END FUNCTION occ_AddEllipse

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION occ_AddSpline(obj, pointTags, tag) &
  & RESULT( ans )
  CLASS( GmshModelOcc_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: pointTags( : )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: tag
  INTEGER( I4B ) :: ans
  ans = gmshModelOccAddSpline(pointTags, SIZE(pointTags, kind=c_size_t), &
    & input(-1,tag), ierr)
END FUNCTION occ_AddSpline

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION occ_AddBSpline(obj, pointTags, tag, degree, weights, knots, &
  & multiplicities) RESULT( ans )
  CLASS( GmshModelOcc_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: pointTags(:)
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: multiplicities(:)
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: weights(:), knots(:)
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: tag
  INTEGER( I4B ), INTENT( IN ) :: degree
  INTEGER( I4B ) :: ans
  ! Internal
  INTEGER( C_SIZE_T ) :: pointTags_n, weights_n, multiplicities_n, knots_n
  REAL( DFP ), ALLOCATABLE :: weights_( : ), knots_( : )
  INTEGER( I4B ), ALLOCATABLE :: multiplicities_( : )

  pointTags_n = SIZE( pointTags )
  IF( PRESENT( weights ) ) THEN
    weights_n = SIZE( weights )
    weights_ =weights
  ELSE
    weights_n = 0
    weights_ = emptyReal
  END IF

  IF( PRESENT( multiplicities ) ) THEN
    multiplicities_n = SIZE( multiplicities )
    multiplicities_ = multiplicities
  ELSE
    multiplicities_n = 0
    multiplicities_ = emptyInt
  END IF

  IF( PRESENT( knots ) ) THEN
    knots_n = SIZE( knots )
    knots_ = knots
  ELSE
    knots_n = 0
    knots_ = emptyReal
  END IF

  ans = gmshModelOccAddBSpline(pointTags, pointTags_n, input(-1,tag), degree, &
  & weights_, weights_n, knots_, knots_n, multiplicities_, multiplicities_n, &
  & ierr)
END FUNCTION occ_AddBSpline

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION occ_AddBezier(obj, pointTags, tag) RESULT( ans )
  CLASS( GmshModelOcc_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: tag
  INTEGER( I4B ), INTENT( IN ) :: pointTags( : )
  INTEGER( I4B ) :: ans
  ! Internal
  INTEGER( C_SIZE_T ) :: pointTags_n

  pointTags_n = SIZE( pointTags )
  ans = gmshModelOccAddBezier(pointTags, pointTags_n, input(-1,tag), ierr)
END FUNCTION occ_AddBezier

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION occ_AddWire(obj, curveTags, tag, checkClosed) &
  & RESULT( ans )
  CLASS( GmshModelOcc_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: tag
  INTEGER( I4B ), INTENT( IN ) :: curveTags( : )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: checkClosed
  INTEGER( I4B ) :: ans
  !internal
  INTEGER( C_SIZE_T ) :: curveTags_n
  curveTags_n = SIZE( curveTags )
  ans = gmshModelOccAddWire(curveTags, curveTags_n, input(-1,tag), &
    & input(option=checkClosed, default=0_I4B), ierr)
END FUNCTION occ_AddWire

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION occ_AddCurveLoop(obj, curveTags, tag) &
  & RESULT( ans )
  CLASS( GmshModelOcc_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: tag
  INTEGER( I4B ), INTENT( IN ) :: curveTags( : )
  INTEGER( I4B ) :: ans
  !internal
  INTEGER( C_SIZE_T ) :: curveTags_n
  curveTags_n = SIZE( curveTags )
  ans = gmshModelOccAddCurveLoop(curveTags, curveTags_n, input(-1,tag), ierr)
END FUNCTION occ_AddCurveLoop

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION occ_AddRectangle(obj, x, y, z, dx, dy, tag, &
  & roundedRadius) RESULT(ans)
  CLASS( GmshModelOcc_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: x, y, z, dx, dy, roundedRadius
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: tag
  INTEGER( I4B ) :: ans
  ans = gmshModelOccAddRectangle(x, y, z, dx, dy, input(-1,tag), roundedRadius, &
  & ierr)
END FUNCTION occ_AddRectangle

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION occ_AddDisk(obj, xc, yc, zc, rx, ry, tag) RESULT( ans )
  CLASS( GmshModelOcc_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: xc, yc, zc, rx, ry
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: tag
  INTEGER( I4B ) :: ans
  ans = gmshModelOccAddDisk(xc, yc, zc, rx, ry, input(-1,tag), &
  & ierr)
END FUNCTION occ_AddDisk

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION occ_AddPlaneSurface(obj, wireTags, tag) RESULT( ans )
  CLASS( GmshModelOcc_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: tag
  INTEGER( I4B ), INTENT( IN ) :: wireTags(:)
  INTEGER( I4B ) :: ans
  ! Internal
  INTEGER( C_SIZE_T ) :: wireTags_n
  wireTags_n = SIZE( wireTags )
  ans = gmshModelOccAddPlaneSurface(wireTags, wireTags_n, input(-1, tag), &
    & ierr)
END FUNCTION occ_AddPlaneSurface

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION occ_AddSurfaceFilling(obj, wireTag, tag, pointTags ) &
  & RESULT( ans )
  CLASS( GmshModelOcc_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: tag
  INTEGER( I4B ), INTENT( IN ) :: wireTag, pointTags(:)
  INTEGER( I4B ) :: ans
  ! internal
  INTEGER( C_SIZE_T ) :: pointTags_n
  pointTags_n = SIZE(pointTags)
  ans = gmshModelOccAddSurfaceFilling(wireTag, input(-1, tag), pointTags, &
    & pointTags_n, ierr)
END FUNCTION occ_AddSurfaceFilling

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION occ_AddBSplineFilling(obj, wireTag, tag, typeOfFilling) &
  & RESULT( ans )
  CLASS( GmshModelOcc_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: tag
  INTEGER( I4B ), INTENT( IN ) :: wireTag
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: typeOfFilling
  INTEGER( I4B ) :: ans
  ! internal
  CHARACTER( LEN = maxStrLen ), TARGET :: typeOfFilling_
  IF( PRESENT( typeOfFilling ) ) THEN
    typeOfFilling_ = TRIM(typeOfFilling) // C_NULL_CHAR
  ELSE
    typeOfFilling_ = "Curved" // C_NULL_CHAR
  END IF
  ans = gmshModelOccAddBSplineFilling(wireTag, input(-1_I4B,tag), C_LOC(typeOfFilling_), ierr)
END FUNCTION occ_AddBSplineFilling

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION occ_AddBezierFilling(obj, wireTag, tag, typeOfFilling ) &
  & RESULT( ans )
  CLASS( GmshModelOcc_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: tag
  INTEGER( I4B ), INTENT( IN ) :: wireTag
  CHARACTER( LEN = * ), OPTIONAL, INTENT( IN ) :: typeOfFilling
  INTEGER( I4B ) :: ans
  ! internal
  CHARACTER( LEN = maxStrLen ), TARGET :: typeOfFilling_
  IF( PRESENT( typeOfFilling ) ) THEN
    typeOfFilling_ = TRIM(typeOfFilling) // C_NULL_CHAR
  ELSE
    typeOfFilling_ = "Curved" // C_NULL_CHAR
  END IF
  ans = gmshModelOccAddBezierFilling(wireTag, input(-1_I4B,tag), C_LOC(typeOfFilling_), ierr)
END FUNCTION occ_AddBezierFilling

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION occ_AddBSplineSurface(obj, pointTags, numPointsU, &
  & tag, degreeU, degreeV, weights, knotsU, &
  & knotsV, multiplicitiesU, &
  & multiplicitiesV, wireTags, wire3D) &
  & RESULT( ans )
  CLASS( GmshModelOcc_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: pointTags(:)
  INTEGER( I4B ), INTENT( IN ) :: numPointsU, degreeU, degreeV
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: multiplicitiesU(:)
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: multiplicitiesV(:)
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: wireTags(:), wire3D
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: tag
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: weights( : )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: knotsU( : )
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: knotsV( : )
  INTEGER( I4B ) :: ans
  ! Internal variables
  INTEGER( C_SIZE_T ) :: pointTags_n, weights_n, knotsU_n, &
    & knotsV_n, multiplicitiesU_n, multiplicitiesV_n, wireTags_n
  INTEGER( I4B ), ALLOCATABLE :: multiplicitiesU_(:)
  INTEGER( I4B ), ALLOCATABLE :: multiplicitiesV_(:)
  INTEGER( I4B ), ALLOCATABLE :: wireTags_(:)
  REAL( DFP ), ALLOCATABLE :: weights_( : )
  REAL( DFP ), ALLOCATABLE :: knotsU_( : )
  REAL( DFP ), ALLOCATABLE :: knotsV_( : )

  pointTags_n = SIZE( pointTags )

  IF( PRESENT(multiplicitiesU) ) THEN
    multiplicitiesU_ = multiplicitiesU
    multiplicitiesU_n = SIZE( multiplicitiesU )
  ELSE
    multiplicitiesU_ = emptyInt
    multiplicitiesU_n = 0
  END IF
  IF( PRESENT(multiplicitiesV) ) THEN
    multiplicitiesV_ = multiplicitiesV
    multiplicitiesV_n = SIZE( multiplicitiesV )
  ELSE
    multiplicitiesV_ = emptyInt
    multiplicitiesV_n = 0
  END IF
  IF( PRESENT(wireTags) ) THEN
    wireTags_ = wireTags
    wireTags_n = SIZE( wireTags )
  ELSE
    wireTags_ = emptyInt
    wireTags_n = 0
  END IF
  IF( PRESENT(weights) ) THEN
    weights_ = weights
    weights_n = SIZE( weights )
  ELSE
    weights_ = emptyReal
    weights_n = 0
  END IF
  IF( PRESENT(knotsU) ) THEN
    knotsU_ = knotsU
    knotsU_n = SIZE( knotsU )
  ELSE
    knotsU_ = emptyReal
    knotsU_n = 0
  END IF
  IF( PRESENT(knotsV) ) THEN
    knotsV_ = knotsV
    knotsV_n = SIZE( knotsV )
  ELSE
    knotsV_ = emptyReal
    knotsV_n = 0
  END IF

  ans = gmshModelOccAddBSplineSurface(pointTags, pointTags_n, &
    & numPointsU, input(-1_I4B, tag), degreeU, degreeV, weights_, &
    & weights_n, knotsU_, &
    & knotsU_n, knotsV_, knotsV_n, multiplicitiesU_, multiplicitiesU_n, &
    & multiplicitiesV_, multiplicitiesV_n, &
    & wireTags_, wireTags_n, input(0_I4B, wire3D), ierr)

END FUNCTION occ_AddBSplineSurface

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION Occ_AddBezierSurface(obj, pointTags, numPointsU, &
  & tag, wireTags, wire3D) &
  & RESULT( ans )
  CLASS( GmshModelOcc_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: pointTags(:), numPointsU
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: tag, wireTags(:), wire3D
  INTEGER( I4B ) :: ans
  !
  INTEGER(C_SIZE_T) :: pointTags_n, wireTags_n
  INTEGER( I4B ), ALLOCATABLE :: wireTags_(:)
  pointTags_n = SIZE(pointTags)
  IF( PRESENT(wireTags) ) THEN
    wireTags_ = wireTags
    wireTags_n = SIZE(wireTags)
  ELSE
    wireTags_ = emptyInt
    wireTags_n = 0
  END IF
  ans = gmshModelOccAddBezierSurface(pointTags, pointTags_n, numPointsU,&
    & input(-1_I4B, tag), wireTags_, wireTags_n, input(0, wire3D), ierr)
END FUNCTION Occ_AddBezierSurface

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION occ_AddTrimmedSurface(obj, surfaceTag, wireTags, &
  & wire3D, tag) RESULT( ans )
  CLASS( GmshModelOcc_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: wireTags(:)
  INTEGER( I4B ), INTENT( IN ) :: surfaceTag
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: tag, wire3D
  INTEGER( I4B ) :: ans
  ! internal
  INTEGER( C_SIZE_T ) :: wireTags_n
  wireTags_n = SIZE(wireTags)
  ans = gmshModelOccAddTrimmedSurface(surfaceTag, wireTags, wireTags_n, &
    & input(0,wire3D), input(-1,tag), ierr )
END FUNCTION occ_AddTrimmedSurface

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION occ_AddSurfaceLoop(obj, surfaceTags, tag, &
  & sewing) RESULT( ans )
  CLASS( GmshModelOcc_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: surfaceTags(:)
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: tag, sewing
  INTEGER( I4B ) :: ans
  !
  INTEGER( C_SIZE_T ) :: surfaceTags_n
  surfaceTags_n = SIZE(surfaceTags)
  ans = gmshModelOccAddSurfaceLoop(surfaceTags, surfaceTags_n, input(-1,tag),&
    & input(0,sewing), ierr)
END FUNCTION occ_AddSurfaceLoop

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION occ_AddVolume(obj,shellTags, tag) &
  & RESULT( ans )
  CLASS( GmshModelOcc_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: shellTags(:)
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: tag
  INTEGER( I4B ) :: ans
  ! internal
  INTEGER( C_SIZE_T ) :: shellTags_n
  shellTags_n = SIZE( shellTags )
  ans = gmshModelOccAddVolume(shellTags, shellTags_n, input(-1,tag), ierr)
END FUNCTION occ_AddVolume

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION Occ_AddSphere(obj, xc, yc, zc, radius, tag, angle1, angle2,angle3)&
  & RESULT( ans )
  CLASS( GmshModelOcc_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: xc, yc, zc, radius
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: angle1, angle2, angle3
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: tag
  INTEGER( I4B ) :: ans
  ans = gmshModelOccAddSphere(xc, yc, zc, radius, input(-1_I4B,tag), &
    & input(-PI*0.5_DFP, angle1), input(PI*0.5_DFP, angle2), &
    & input(PI*2.0_DFP, angle3), ierr )
END FUNCTION Occ_AddSphere

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION occ_AddBox(obj, x, y, z, dx, dy, dz, tag) &
  & RESULT( ans )
  CLASS( GmshModelOcc_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: tag
  REAL( DFP ), INTENT( IN ) :: x, y, z, dx, dy, dz
  INTEGER( I4B ) :: ans
  ans = gmshModelOccAddBox(x, y, z, dx, dy, dz, input(-1_I4B, tag), ierr)
END FUNCTION occ_AddBox

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION occ_AddCylinder(obj, x, y, z, dx, dy, dz, r, tag, angle) &
  & RESULT( ans )
  CLASS( GmshModelOcc_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: tag
  REAL( DFP ), INTENT( IN ) :: x, y, z, dx, dy, dz, r
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: angle
  INTEGER( I4B ) :: ans
  ans = gmshModelOccAddCylinder(x, y, z, dx, dy, dz, r, input(-1_I4B, tag), &
    & input(2.0*PI, angle), ierr)
END FUNCTION occ_AddCylinder

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION occ_AddCone(obj,x, y, z, dx, dy, dz, r1, r2, tag, angle) &
  & RESULT( ans )
  CLASS( GmshModelOcc_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: tag
  REAL( DFP ), INTENT( IN ) :: x, y, z, dx, dy, dz, r1, r2
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: angle
  INTEGER( I4B ) :: ans
  ans = gmshModelOccAddCone(x, y, z, dx, dy, dz, r1, r2, input(-1_I4B, tag), input(2.0*PI, angle), ierr)
END FUNCTION occ_AddCone

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION Occ_AddWedge(obj, x, y, z, dx, dy, dz, tag, ltx) &
  & RESULT( ans )
  CLASS( GmshModelOcc_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: tag
  REAL( DFP ), INTENT( IN ) :: x, y, z, dx, dy, dz
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: ltx
  INTEGER( I4B ) :: ans
  !
  ans = gmshModelOccAddWedge(x, y, z, dx, dy, dz, input(-1_I4B, tag), &
    & input(0.0_DFP, ltx), ierr)
END FUNCTION Occ_AddWedge

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION Occ_AddTorus(obj, x, y, z, r1, r2, tag, angle) &
  & RESULT( ans )
  CLASS( GmshModelOcc_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: tag
  REAL( DFP ), INTENT( IN ) :: x, y, z, r1, r2
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: angle
  INTEGER( I4B ) :: ans
  ans = gmshModelOccAddTorus(x, y, z, r1, r2, input(-1_I4B, tag), &
    & input(2.0*PI, angle), ierr)
END FUNCTION Occ_AddTorus

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION Occ_AddThruSections(obj, wireTags, outDimTags, &
  & tag, makeSolid, makeRuled, maxDegree) &
  RESULT( ans )
  CLASS( GmshModelOcc_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: wireTags(:)
  INTEGER( I4B ), ALLOCATABLE, INTENT( OUT ) :: outDimTags(:)
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: tag, makeSolid, makeRuled, &
    & maxDegree
  INTEGER( I4B ) :: ans
  ! internal value
  INTEGER( C_SIZE_T ) :: wireTags_n, outDimTags_n
  TYPE( C_PTR ) :: cptr

  wireTags_n = SIZE( wireTags )
  CALL gmshModelOccAddThruSections(wireTags, wireTags_n, cptr, &
    & outDimTags_n, input(-1,tag), input(1, makeSolid), &
    & input(0,makeRuled), input(-1, maxDegree), ierr)

  CALL Reallocate(outDimTags, int(outDimTags_n,i4b))
  CALL C_PTR_TO_INT_VEC(cptr=cptr, vec=outDimTags)
END FUNCTION Occ_AddThruSections

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION occ_Synchronize(obj) &
  & RESULT( ans )
  CLASS( GmshModelOcc_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ) :: ans
  CALL gmshModelOccSynchronize(ierr)
  ans = int(ierr, i4b)
END FUNCTION occ_Synchronize

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE GmshModelOcc_Class