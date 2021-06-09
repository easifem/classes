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

MODULE GmshModelGeoMesh_Class
USE GlobalData, ONLY: DFP, I4B, LGT
USE Utility, ONLY: Reallocate
USE GmshInterface
USE C_INTERFACE_MODULE, ONLY: C_PTR_TO_INT_VEC
USE ISO_C_BINDING
IMPLICIT NONE
PRIVATE
CHARACTER( LEN = * ), PARAMETER :: modName = "GMSHMODELGEOMESH_CLASS"
INTEGER( C_INT ) :: ierr
!$OMP THREADPRIVATE(ierr)
INTEGER( I4B ), PARAMETER :: maxStrLen = 256

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: GmshModelGeoMesh_
  CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS( Obj ) :: SetSize => mesh_SetSize
  PROCEDURE, PUBLIC, PASS( Obj ) :: SetTransfiniteCurve => mesh_SetTransfiniteCurve
  PROCEDURE, PUBLIC, PASS( Obj ) :: SetTransfiniteSurface => mesh_SetTransfiniteSurface
  PROCEDURE, PUBLIC, PASS( Obj ) :: SetTransfiniteVolume => mesh_SetTransfiniteVolume
  PROCEDURE, PUBLIC, PASS( Obj ) :: SetSmoothing => mesh_SetSmoothing
  PROCEDURE, PUBLIC, PASS( Obj ) :: SetReverse => mesh_SetReverse
  PROCEDURE, PUBLIC, PASS( Obj ) :: SetAlgorithm => mesh_SetAlgorithm
  PROCEDURE, PUBLIC, PASS( Obj ) :: SetSizeFromBoundary => mesh_SetSizeFromBoundary
END TYPE GmshModelGeoMesh_

PUBLIC :: GmshModelGeoMesh_
TYPE( GmshModelGeoMesh_ ), PUBLIC, PARAMETER :: TypeGmshModelGeoMesh = GmshModelGeoMesh_()

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: GmshModelGeoMeshPointer_
  CLASS( GmshModelGeoMesh_ ), POINTER :: Ptr => NULL()
END TYPE

PUBLIC :: GmshModelGeoMeshPointer_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION mesh_SetSize(obj, dimTags, Meshsize) &
  & RESULT( ans )
  CLASS( GmshModelGeoMesh_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: dimTags( : )
  REAL( DFP ), INTENT( IN ) :: Meshsize
  INTEGER( I4B ) :: ans
  CALL gmshModelGeoMeshSetSize( dimTags, &
    & SIZE(dimTags, KIND=C_SIZE_T), Meshsize, ierr )
  ans = int( ierr, i4b )
END FUNCTION mesh_SetSize

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION mesh_SetTransfiniteCurve(obj, tag, nPoints, meshType, &
  & coef ) RESULT( ans )
  CLASS( GmshModelGeoMesh_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: tag, nPoints
  CHARACTER( LEN = * ), INTENT( IN ) :: meshType
  REAL( DFP ), INTENT( IN ) :: coef
  INTEGER( I4B ) :: ans
  !
  CHARACTER( LEN = maxStrLen ), TARGET :: meshType_
  meshType_ = TRIM( meshType ) // C_NULL_CHAR
  CALL gmshModelGeoMeshSetTransfiniteCurve(tag, nPoints, &
    & C_LOC(meshType_), coef, ierr )
  ans = int( ierr, i4b )
END FUNCTION mesh_SetTransfiniteCurve

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION mesh_SetTransfiniteSurface(obj, tag, arrangement, &
  & cornerTags ) RESULT( ans )
  CLASS( GmshModelGeoMesh_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: tag
  CHARACTER( LEN = * ), INTENT( IN ) :: arrangement
  INTEGER( I4B ), INTENT( IN ) :: cornerTags(:)
  INTEGER( I4B ) :: ans
  !
  CHARACTER( LEN = maxStrLen ), TARGET :: arrangement_
  arrangement_ = TRIM( arrangement ) // C_NULL_CHAR
  CALL gmshModelGeoMeshSetTransfiniteSurface(tag, &
    & C_LOC(arrangement_), cornerTags, &
    & SIZE(cornerTags, KIND=C_SIZE_T), ierr)
  ans = int( ierr, i4b )
END FUNCTION mesh_SetTransfiniteSurface

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION mesh_SetTransfiniteVolume(obj, tag, cornerTags) &
  & RESULT( ans )
  CLASS( GmshModelGeoMesh_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: tag
  INTEGER( I4B ), INTENT( IN ) :: cornerTags( : )
  INTEGER( I4B ) :: ans
  CALL gmshModelGeoMeshSetTransfiniteVolume(tag, cornerTags, &
  & SIZE(cornerTags, KIND=C_SIZE_T), ierr)
  ans = int(ierr, i4b)
END FUNCTION mesh_SetTransfiniteVolume

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION mesh_SetSmoothing(obj, dim, tag, val) &
  & RESULT( ans )
  CLASS( GmshModelGeoMesh_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: dim, tag, val
  INTEGER( I4B ) :: ans
  CALL gmshModelGeoMeshSetSmoothing(dim, tag, val, ierr)
  ans = int( ierr, i4b )
END FUNCTION mesh_SetSmoothing

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION mesh_SetReverse(obj, dim, tag, val) RESULT( ans )
  CLASS( GmshModelGeoMesh_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: dim, tag, val
  INTEGER( I4B ) :: ans
  CALL gmshModelGeoMeshSetReverse(dim, tag, val, ierr)
  ans = int( ierr, i4b )
END FUNCTION mesh_SetReverse

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION mesh_SetAlgorithm(obj, dim, tag, val) RESULT( ans )
  CLASS( GmshModelGeoMesh_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: dim, tag, val
  INTEGER( I4B ) :: ans
  CALL gmshModelGeoMeshSetAlgorithm(dim, tag, val, ierr)
  ans = int( ierr, i4b )
END FUNCTION mesh_SetAlgorithm

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION mesh_SetSizeFromBoundary(obj, dim, tag, val) &
  & RESULT( ans )
  CLASS( GmshModelGeoMesh_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: dim, tag, val
  INTEGER( I4B ) :: ans
  CALL gmshModelGeoMeshSetSizeFromBoundary(dim, tag, val, ierr)
  ans = int( ierr, i4b )
END FUNCTION mesh_SetSizeFromBoundary

END MODULE GmshModelGeoMesh_Class