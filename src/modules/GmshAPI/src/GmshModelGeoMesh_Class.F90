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
USE Utility, ONLY: Reallocate, input
USE GmshInterface
USE CInterface, ONLY: C_PTR_TO_INT_VEC
USE ISO_C_BINDING
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "GmshModelGeoMesh_Class"
INTEGER(C_INT) :: ierr
!$OMP THREADPRIVATE(ierr)
INTEGER(I4B), PARAMETER :: maxStrLen = 256
PUBLIC :: GmshModelGeoMesh_
PUBLIC :: GmshModelGeoMeshPointer_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: GmshModelGeoMesh_
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, NOPASS :: SetSize => mesh_SetSize
  PROCEDURE, PUBLIC, NOPASS :: SetTransfiniteCurve  &
    & => mesh_SetTransfiniteCurve
  PROCEDURE, PUBLIC, NOPASS :: SetTransfiniteSurface  &
    & => mesh_SetTransfiniteSurface
  PROCEDURE, PUBLIC, NOPASS :: SetTransfiniteVolume =>  &
    & mesh_SetTransfiniteVolume
  PROCEDURE, PUBLIC, NOPASS :: SetRecombine =>  &
    & mesh_SetRecombine
  PROCEDURE, PUBLIC, NOPASS :: SetSmoothing =>  &
    & mesh_SetSmoothing
  PROCEDURE, PUBLIC, NOPASS :: SetReverse =>  &
    & mesh_SetReverse
  PROCEDURE, PUBLIC, NOPASS :: SetAlgorithm =>  &
    & mesh_SetAlgorithm
  PROCEDURE, PUBLIC, NOPASS :: SetSizeFromBoundary =>  &
    & mesh_SetSizeFromBoundary
END TYPE GmshModelGeoMesh_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE( GmshModelGeoMesh_ ), PUBLIC, PARAMETER :: TypeGmshModelGeoMesh  &
  & = GmshModelGeoMesh_()

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: GmshModelGeoMeshPointer_
  CLASS(GmshModelGeoMesh_), POINTER :: Ptr => NULL()
END TYPE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-04
! summary: Set size
!
!# Introduction
!
! Set a mesh size constraint on the entities `dimTags' in the built-in CAD
! kernel representation. Currently only entities of dimension 0 (points) are
! handled.

FUNCTION mesh_SetSize(dimTags, Meshsize) &
  & RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: dimTags(:)
  REAL(DFP), INTENT(IN) :: Meshsize
  INTEGER(I4B) :: ans
  CALL gmshModelGeoMeshSetSize(dimTags, &
    & SIZE(dimTags, KIND=C_SIZE_T), Meshsize, ierr)
  ans = INT(ierr, i4b)
END FUNCTION mesh_SetSize

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-04
! summary:  Set transfinite curve
!
!# Introduction
!
! Set a transfinite meshing constraint on the curve `tag' in the built-in CAD
! kernel representation, with `numNodes' nodes distributed according to
! `meshType' and `coef'. Currently supported types are "Progression"
! (geometrical progression with power `coef') and "Bump" (refinement toward
! both extremities of the curve).

FUNCTION mesh_SetTransfiniteCurve(tag, nPoints, meshType, coef &
  & ) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: tag, nPoints
  CHARACTER(*), OPTIONAL, INTENT(IN) :: meshType
  !! Default value is "Progression"
  REAL(DFP), OPTIONAL, INTENT(IN) :: coef
  !! Default value is 1.0_DFP 
  INTEGER(I4B) :: ans

  ! internal variables
  CHARACTER(maxStrLen), TARGET :: meshType_

  IF (PRESENT(meshType)) THEN
    meshType_ = TRIM(meshType)//C_NULL_CHAR
  ELSE
    meshType_ = "Progression"//C_NULL_CHAR
  END IF
  CALL gmshModelGeoMeshSetTransfiniteCurve(tag, nPoints, &
    & C_LOC(meshType_), input(option=coef, default=1.0_DFP), ierr)
  ans = INT(ierr, i4b)
END FUNCTION mesh_SetTransfiniteCurve

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:   2023-11-04
! summary:  Set transfinite surface
!
!# Introduction
!
! Set a transfinite meshing constraint on the surface `tag' in the built-in
! CAD kernel representation. `arrangement' describes the arrangement of the
! triangles when the surface is not flagged as recombined: currently
! supported values are "Left", "Right", "AlternateLeft" and "AlternateRight".
! `cornerTags' can be used to specify the (3 or 4) corners of the transfinite
! interpolation explicitly; specifying the corners explicitly is mandatory if
! the surface has more that 3 or 4 points on its boundary.

FUNCTION mesh_SetTransfiniteSurface(tag, arrangement, &
  & cornerTags) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: tag
  CHARACTER(*), OPTIONAL, INTENT(IN) :: arrangement
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: cornerTags(:)
  INTEGER(I4B) :: ans
  ! internal variables
  CHARACTER(maxStrLen), TARGET :: arrangement_
  INTEGER(C_SIZE_T) :: cornerTags_n
  INTEGER(I4B), ALLOCATABLE :: cornerTags_(:)

  IF (PRESENT(arrangement)) THEN
    arrangement_ = TRIM(arrangement)//C_NULL_CHAR
  ELSE
    arrangement_ = "Left"//C_NULL_CHAR
  END IF

  IF (PRESENT(cornerTags)) THEN
    cornerTags_n = SIZE(cornerTags)
    cornerTags_ = cornerTags
  ELSE
    cornerTags_n = 0_I4B
    ALLOCATE (cornerTags_(0))
  END IF

  CALL gmshModelGeoMeshSetTransfiniteSurface(tag, &
    & C_LOC(arrangement_), cornerTags_, &
    & cornerTags_n, ierr)

  IF (ALLOCATED(cornerTags_)) DEALLOCATE (cornerTags_)

  ans = INT(ierr, i4b)
END FUNCTION mesh_SetTransfiniteSurface

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-04
! summary:  Set transfinite volume
!
!# Introduction
!
! Set a transfinite meshing constraint on the surface `tag' in the built-in
! CAD kernel representation. `cornerTags' can be used to specify the (6 or 8)
! corners of the transfinite interpolation explicitly. */

FUNCTION mesh_SetTransfiniteVolume(tag, cornerTags) &
  & RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: tag
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: cornerTags(:)
  INTEGER(I4B) :: ans
  !
  INTEGER(C_SIZE_T) :: cornerTags_n
  INTEGER(I4B), ALLOCATABLE :: cornerTags_(:)

  IF (PRESENT(cornerTags)) THEN
    cornerTags_n = SIZE(cornerTags)
    cornerTags_ = cornerTags
  ELSE
    cornerTags_n = 0_I4B
    ALLOCATE (cornerTags_(0))
  END IF

  CALL gmshModelGeoMeshSetTransfiniteVolume(tag, cornerTags_, &
  & cornerTags_n, ierr)

  IF (ALLOCATED(cornerTags_)) DEALLOCATE (cornerTags_)
  ans = INT(ierr, i4b)
END FUNCTION mesh_SetTransfiniteVolume

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-04
! summary:  Recombine mesh
!
!# Introduction
!
! Set a recombination meshing constraint on the entity of dimension `dim' and
! tag `tag' in the built-in CAD kernel representation. Currently only
! entities of dimension 2 (to recombine triangles into quadrangles) are
! supported; `angle' specifies the threshold angle for the simple
! recombination algorithm.

FUNCTION mesh_SetRecombine(dim, tag, angle) &
  & RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: dim, tag
  REAL(DFP), OPTIONAL, INTENT(IN) :: angle
  INTEGER(I4B) :: ans

  CALL gmshModelGeoMeshSetRecombine(dim, tag,  &
    & input(option=angle, default=45.0_DFP), ierr)

  ans = INT(ierr, i4b)
END FUNCTION mesh_SetRecombine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-04
! summary:  Set SetSmoothing
!
!# Introduction
!
! Set a smoothing meshing constraint on the entity of dimension `dim' and tag
! `tag' in the built-in CAD kernel representation. `val' iterations of a
! Laplace smoother are applied.

FUNCTION mesh_SetSmoothing(dim, tag, val) &
  & RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: dim, tag, val
  INTEGER(I4B) :: ans
  CALL gmshModelGeoMeshSetSmoothing(dim, tag, val, ierr)
  ans = INT(ierr, i4b)
END FUNCTION mesh_SetSmoothing

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION mesh_SetReverse(dim, tag, val) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: dim, tag, val
  INTEGER(I4B) :: ans
  CALL gmshModelGeoMeshSetReverse(dim, tag, val, ierr)
  ans = INT(ierr, i4b)
END FUNCTION mesh_SetReverse

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION mesh_SetAlgorithm(dim, tag, val) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: dim, tag, val
  INTEGER(I4B) :: ans
  CALL gmshModelGeoMeshSetAlgorithm(dim, tag, val, ierr)
  ans = INT(ierr, i4b)
END FUNCTION mesh_SetAlgorithm

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

FUNCTION mesh_SetSizeFromBoundary(dim, tag, val) &
  & RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: dim, tag, val
  INTEGER(I4B) :: ans
  CALL gmshModelGeoMeshSetSizeFromBoundary(dim, tag, val, ierr)
  ans = INT(ierr, i4b)
END FUNCTION mesh_SetSizeFromBoundary

END MODULE GmshModelGeoMesh_Class
