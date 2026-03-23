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
IMPLICIT NONE

PRIVATE
PUBLIC :: GmshModelGeoMesh_
PUBLIC :: GmshModelGeoMeshPointer_
PUBLIC :: TypeGmshModelGeoMesh

!----------------------------------------------------------------------------
!                                                          GmshModelGeoMesh_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-18
! summary: GmshModelGeoMesh_ gmsh model geo mesh
!
!# GmshModelGeoMesh_
!
! Gmsh model geo mesh.
!
TYPE :: GmshModelGeoMesh_
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, NOPASS :: SetSize => obj_SetSize
  PROCEDURE, PUBLIC, NOPASS :: SetTransfiniteCurve => &
    obj_SetTransfiniteCurve
  PROCEDURE, PUBLIC, NOPASS :: SetTransfiniteSurface => &
    obj_SetTransfiniteSurface
  PROCEDURE, PUBLIC, NOPASS :: SetTransfiniteVolume => &
    obj_SetTransfiniteVolume
  PROCEDURE, PUBLIC, NOPASS :: SetRecombine => &
    obj_SetRecombine
  PROCEDURE, PUBLIC, NOPASS :: SetSmoothing => &
    obj_SetSmoothing
  PROCEDURE, PUBLIC, NOPASS :: SetReverse => &
    obj_SetReverse
  PROCEDURE, PUBLIC, NOPASS :: SetAlgorithm => &
    obj_SetAlgorithm
  PROCEDURE, PUBLIC, NOPASS :: SetSizeFromBoundary => &
    obj_SetSizeFromBoundary
END TYPE GmshModelGeoMesh_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE(GmshModelGeoMesh_), PARAMETER :: TypeGmshModelGeoMesh = &
                                      GmshModelGeoMesh_()

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: GmshModelGeoMeshPointer_
  CLASS(GmshModelGeoMesh_), POINTER :: ptr => NULL()
END TYPE GmshModelGeoMeshPointer_

!----------------------------------------------------------------------------
!                                                                    SetSize
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-04
! summary: Set size
!
!# SetSize
!
! Set a mesh size constraint on the entities `dimTags' in the built-in CAD
! kernel representation. Currently only entities of dimension 0 (points) are
! handled.

INTERFACE
  MODULE FUNCTION obj_SetSize(dimTags, meshSize) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dimTags(:, :)
    REAL(DFP), INTENT(IN) :: meshSize
    INTEGER(I4B) :: ans
  END FUNCTION obj_SetSize
END INTERFACE

!----------------------------------------------------------------------------
!                                                        SetTransfiniteCurve
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-04
! summary:  Set transfinite curve
!
!# SetTransfiniteCurve
!
! Set a transfinite meshing constraint on the curve `tag' in the built-in CAD
! kernel representation, with `numNodes' nodes distributed according to
! `meshType' and `coef'. Currently supported types are "Progression"
! (geometrical progression with power `coef') and "Bump" (refinement toward
! both extremities of the curve).

INTERFACE
  MODULE FUNCTION obj_SetTransfiniteCurve( &
    tag, nPoints, meshType, coef) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: tag, nPoints
    CHARACTER(*), OPTIONAL, INTENT(IN) :: meshType
    !! Default value is "Progression"
    REAL(DFP), OPTIONAL, INTENT(IN) :: coef
    !! Default value is 1.0_DFP
    INTEGER(I4B) :: ans
  END FUNCTION obj_SetTransfiniteCurve
END INTERFACE

!----------------------------------------------------------------------------
!                                                      SetTransfiniteSurface
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

INTERFACE
  MODULE FUNCTION obj_SetTransfiniteSurface( &
    tag, arrangement, cornerTags) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: tag
    CHARACTER(*), OPTIONAL, INTENT(IN) :: arrangement
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cornerTags(:)
    INTEGER(I4B) :: ans
  END FUNCTION obj_SetTransfiniteSurface
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetTransfiniteVolume
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-04
! summary: Set transfinite volume
!
!# SetTransfiniteVolume
!
! Set a transfinite meshing constraint on the surface `tag' in the built-in
! CAD kernel representation. `cornerTags' can be used to specify the (6 or 8)
! corners of the transfinite interpolation explicitly.

INTERFACE
  MODULE FUNCTION obj_SetTransfiniteVolume(tag, cornerTags) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: tag
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: cornerTags(:)
    INTEGER(I4B) :: ans
  END FUNCTION obj_SetTransfiniteVolume
END INTERFACE

!----------------------------------------------------------------------------
!                                                               SetRecombine
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

INTERFACE
  MODULE FUNCTION obj_SetRecombine(dim, tag, angle) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim, tag
    REAL(DFP), OPTIONAL, INTENT(IN) :: angle
    INTEGER(I4B) :: ans
  END FUNCTION obj_SetRecombine
END INTERFACE

!----------------------------------------------------------------------------
!                                                               SetSmoothing
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-04
! summary:  Set SetSmoothing
!
!# SetSmoothing
!
! Set a smoothing meshing constraint on the entity of dimension `dim' and tag
! `tag' in the built-in CAD kernel representation. `val' iterations of a
! Laplace smoother are applied.

INTERFACE
  MODULE FUNCTION obj_SetSmoothing(dim, tag, val) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim, tag, val
    INTEGER(I4B) :: ans
  END FUNCTION obj_SetSmoothing
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 SetReverse
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-18
! summary: SetReverse
!
!# SetReverse
!
! Set reverse.
!

INTERFACE
  MODULE FUNCTION obj_SetReverse(dim, tag, val) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim, tag, val
    INTEGER(I4B) :: ans
  END FUNCTION obj_SetReverse
END INTERFACE

!----------------------------------------------------------------------------
!                                                               SetAlgorithm
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-18
! summary: Set Algorithm
!
!# SetAlgorithm
!
! Set algorithm.
!

INTERFACE
  MODULE FUNCTION obj_SetAlgorithm(dim, tag, val) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim, tag, val
    INTEGER(I4B) :: ans
  END FUNCTION obj_SetAlgorithm
END INTERFACE

!----------------------------------------------------------------------------
!                                                        SetSizeFromBoundary
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-03-18
! summary: Set size from boundary
!
!# SetSizeFromBoundary
!
! Set size from boundary.
!
INTERFACE
  MODULE FUNCTION obj_SetSizeFromBoundary(dim, tag, val) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim, tag, val
    INTEGER(I4B) :: ans
  END FUNCTION obj_SetSizeFromBoundary
END INTERFACE

END MODULE GmshModelGeoMesh_Class
