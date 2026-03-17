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

MODULE GmshModel_Class
USE GlobalData, ONLY: DFP, I4B, LGT
USE ExceptionHandler_Class, ONLY: e
USE String_Class, ONLY: String
USE Utility, ONLY: Input, Reallocate
USE GmshModelGeo_Class, ONLY: GmshModelGeo_
USE GmshModelOcc_Class, ONLY: GmshModelOcc_
USE GmshModelMesh_Class, ONLY: GmshModelMesh_
IMPLICIT NONE

PRIVATE
PUBLIC :: GmshModel_
PUBLIC :: TypeGmshModel
PUBLIC :: GmshModelPointer_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: GmshModel_
  PRIVATE
  TYPE(GmshModelGeo_), PUBLIC, POINTER :: geo => NULL()
  TYPE(GmshModelOcc_), PUBLIC, POINTER :: occ => NULL()
  TYPE(GmshModelMesh_), PUBLIC, POINTER :: mesh => NULL()
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => model_Initiate
  PROCEDURE, PUBLIC, NOPASS :: Add => model_Add
  PROCEDURE, PUBLIC, NOPASS :: Remove => model_Remove
  PROCEDURE, PUBLIC, NOPASS :: List => model_List
  PROCEDURE, PUBLIC, NOPASS :: GetCurrent => model_GetCurrent
  PROCEDURE, PUBLIC, NOPASS :: SetCurrent => model_SetCurrent
  PROCEDURE, PUBLIC, NOPASS :: GetFileName => model_GetFileName
  PROCEDURE, PUBLIC, NOPASS :: SetFileName => model_SetFileName
  PROCEDURE, PUBLIC, NOPASS :: GetEntities => model_GetEntities
  PROCEDURE, PUBLIC, NOPASS :: SetEntityName => model_SetEntityName
  PROCEDURE, PUBLIC, NOPASS :: GetEntityName => model_GetEntityName
  PROCEDURE, PUBLIC, NOPASS :: GetPhysicalGroups => model_GetPhysicalGroups
  PROCEDURE, PUBLIC, NOPASS :: GetEntitiesForPhysicalGroup => &
    model_GetEntitiesForPhysicalGroup
  PROCEDURE, PUBLIC, NOPASS :: GetPhysicalGroupsForEntity => &
    model_GetPhysicalGroupsForEntity
  PROCEDURE, PUBLIC, NOPASS :: AddPhysicalGroup => model_AddPhysicalGroup
  PROCEDURE, PUBLIC, NOPASS :: RemovePhysicalGroups => &
    model_RemovePhysicalGroups
  PROCEDURE, PUBLIC, NOPASS :: SetPhysicalName => model_SetPhysicalName
  PROCEDURE, PUBLIC, NOPASS :: RemovePhysicalName => &
    model_RemovePhysicalName
  PROCEDURE, PUBLIC, NOPASS :: GetPhysicalName => model_GetPhysicalName
  PROCEDURE, PUBLIC, NOPASS :: SetTag => model_SetTag
  PROCEDURE, PUBLIC, NOPASS :: GetBoundary => model_GetBoundary
  PROCEDURE, PUBLIC, NOPASS :: GetAdjacencies => model_GetAdjacencies
  PROCEDURE, PUBLIC, NOPASS :: GetEntitiesInBoundingBox => &
    model_GetEntitiesInBoundingBox
  PROCEDURE, PUBLIC, NOPASS :: GetBoundingBox => &
    model_GetBoundingBox
  PROCEDURE, PUBLIC, NOPASS :: GetDimension => &
    model_GetDimension
  PROCEDURE, PUBLIC, NOPASS :: AddDiscreteEntity => &
    model_AddDiscreteEntity
  PROCEDURE, PUBLIC, NOPASS :: RemoveEntities => &
    model_RemoveEntities
  PROCEDURE, PUBLIC, NOPASS :: RemoveEntityName => &
    model_RemoveEntityName
  PROCEDURE, PUBLIC, NOPASS :: GetType => &
    model_GetType
  PROCEDURE, PUBLIC, NOPASS :: GetParent => &
    model_GetParent
  PROCEDURE, PUBLIC, NOPASS :: GetNumberOfPartitions => &
    model_GetNumberOfPartitions
  PROCEDURE, PUBLIC, NOPASS :: GetPartitions => &
    model_GetPartitions
  PROCEDURE, PUBLIC, NOPASS :: GetValue => &
    model_GetValue
  PROCEDURE, PUBLIC, NOPASS :: GetDerivative => &
    model_GetDerivative
  PROCEDURE, PUBLIC, NOPASS :: GetSecondDerivative => &
    model_GetSecondDerivative
  PROCEDURE, PUBLIC, NOPASS :: GetCurvature => &
    model_GetCurvature
  PROCEDURE, PUBLIC, NOPASS :: GetPrincipalCurvatures => &
    model_GetPrincipalCurvatures
  PROCEDURE, PUBLIC, NOPASS :: GetNormal => &
    model_GetNormal
  PROCEDURE, PUBLIC, NOPASS :: GetParameterizaion => &
    model_GetParametrization
  PROCEDURE, PUBLIC, NOPASS :: GetParametrizationBounds => &
    model_GetParametrizationBounds
  PROCEDURE, PUBLIC, NOPASS :: IsInside => &
    model_IsInside
  PROCEDURE, PUBLIC, NOPASS :: GetClosestPoint => &
    model_GetClosestPoint
  PROCEDURE, PUBLIC, NOPASS :: ReparametrizeOnSurface => &
    model_ReparametrizeOnSurface
  PROCEDURE, PUBLIC, NOPASS :: SetVisibility => model_SetVisibility
  PROCEDURE, PUBLIC, NOPASS :: GetVisibility => model_GetVisibility
  PROCEDURE, PUBLIC, NOPASS :: SetVisibilityPerWindow => &
    model_SetVisibilityPerWindow
  PROCEDURE, PUBLIC, NOPASS :: SetColor => model_SetColor
  PROCEDURE, PUBLIC, NOPASS :: GetColor => model_GetColor
  PROCEDURE, PUBLIC, NOPASS :: SetCoordinates => model_SetCoordinates
  PROCEDURE, PUBLIC, NOPASS :: GetAttributeNames => model_GetAttributeNames
  PROCEDURE, PUBLIC, NOPASS :: SetAttribute => model_SetAttribute
  PROCEDURE, PUBLIC, NOPASS :: GetAttribute => model_GetAttribute
  PROCEDURE, PUBLIC, NOPASS :: RemoveAttribute => model_RemoveAttribute
END TYPE GmshModel_

!----------------------------------------------------------------------------
!                                                              TypeGmshModel
!----------------------------------------------------------------------------

TYPE(GmshModel_), PARAMETER :: TypeGmshModel = GmshModel_()

!----------------------------------------------------------------------------
!                                                          GmshModelPointer_
!----------------------------------------------------------------------------

TYPE :: GmshModelPointer_
  CLASS(GmshModel_), POINTER :: Ptr => NULL()
END TYPE GmshModelPointer_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE model_initiate(obj)
    CLASS(GmshModel_), INTENT(INOUT) :: obj
  END SUBROUTINE model_initiate
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION model_add(name) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B) :: ans
  END FUNCTION model_add
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION model_remove() RESULT(ans)
    INTEGER(I4B) :: ans
  END FUNCTION model_remove
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION model_list(names) RESULT(ans)
    TYPE(String), ALLOCATABLE, INTENT(OUT) :: names(:)
    INTEGER(I4B) :: ans
  END FUNCTION model_list
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION model_GetCurrent(name) RESULT(ans)
    CHARACTER(*), INTENT(OUT) :: name
    INTEGER(I4B) :: ans
  END FUNCTION model_GetCurrent
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION model_SetCurrent(name) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B) :: ans
  END FUNCTION model_SetCurrent
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION model_GetFileName(fileName) RESULT(ans)
    CHARACTER(*), INTENT(OUT) :: fileName
    INTEGER(I4B) :: ans
  END FUNCTION model_GetFileName
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION model_SetFileName(fileName) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: fileName
    INTEGER(I4B) :: ans
  END FUNCTION model_SetFileName
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Get all the entities in the current model.
!! If `dim` is >= 0, return only
!! the entities of the specified dimension (e.g. points if `dim' == 0).
!! The entities are returned as a vector of (dim, tag) pairs.

INTERFACE
  MODULE FUNCTION model_GetEntities(dimTags, dim) RESULT(ans)
    INTEGER(I4B), ALLOCATABLE, INTENT(OUT) :: dimTags(:, :)
    !! dimTags has two rows
    !! first row is for dim and second row is for tag
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim
    !! if present should be greater than 0
    INTEGER(I4B) :: ans
  END FUNCTION model_GetEntities
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Set the name of the entity of dimension `dim' and tag `tag'.

INTERFACE
  MODULE FUNCTION model_SetEntityName(dim, tag, name) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim, tag
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B) :: ans
  END FUNCTION model_SetEntityName
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Get the name of the entity of dimension `dim' and tag `tag'.

INTERFACE
  MODULE FUNCTION model_GetEntityName(dim, tag, name) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim, tag
    CHARACTER(*), INTENT(OUT) :: name
    INTEGER(I4B) :: ans
  END FUNCTION model_GetEntityName
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Get all the physical groups in the current model. If `dim' is >= 0, return
!! only the entities of the specified dimension (e.g. physical points if `dim'
!! == 0). The entities are returned as a vector of (dim, tag) pairs.

INTERFACE
  MODULE FUNCTION model_GetPhysicalGroups(dimTags, dim) RESULT(ans)
    INTEGER(I4B), ALLOCATABLE, INTENT(OUT) :: dimTags(:, :)
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B) :: ans
  END FUNCTION model_GetPhysicalGroups
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Get the tags of the model entities making up the physical group of
!! dimension `dim' and tag `tag'.

INTERFACE
  MODULE FUNCTION model_GetEntitiesForPhysicalGroup(dim, tag, tags) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim, tag
    INTEGER(I4B), ALLOCATABLE, INTENT(OUT) :: tags(:)
    INTEGER(I4B) :: ans
  END FUNCTION model_GetEntitiesForPhysicalGroup
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Get the tags of the model entities making up the physical group of
!! dimension `dim' and tag `tag'.

INTERFACE
  MODULE FUNCTION model_GetPhysicalGroupsForEntity(dim, tag, physicalTags) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim, tag
    INTEGER(I4B), ALLOCATABLE, INTENT(OUT) :: physicalTags(:)
    INTEGER(I4B) :: ans
  END FUNCTION model_GetPhysicalGroupsForEntity
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Add a physical group of dimension `dim', grouping the model entities with
!! tags `tags'. Return the tag of the physical group, equal to `tag' if `tag'
!! is positive, or a new tag if `tag' < 0.

INTERFACE
  MODULE FUNCTION model_AddPhysicalGroup(dim, tags, tag, name) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), INTENT(IN) :: tags(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tag
    CHARACTER(*), OPTIONAL, INTENT(IN) :: name
    INTEGER(I4B) :: ans
  END FUNCTION model_AddPhysicalGroup
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Remove the physical groups `dimTags' (given as a vector of (dim, tag)
!! pairs) from the current model. If `dimTags' is empty, remove all groups.

INTERFACE
  MODULE FUNCTION model_RemovePhysicalGroups(dimTags) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dimTags(:)
    INTEGER(I4B) :: ans
  END FUNCTION model_RemovePhysicalGroups
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Set the name of the physical group of dimension `dim' and tag `tag'.

INTERFACE
  MODULE FUNCTION model_SetPhysicalName(dim, tag, name) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim, tag
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B) :: ans
  END FUNCTION model_SetPhysicalName
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Remove the physical name `name' from the current model.

INTERFACE
  MODULE FUNCTION model_RemovePhysicalName(name) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B) :: ans
  END FUNCTION model_RemovePhysicalName
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Get the name of the physical group of dimension `dim' and tag `tag'.

INTERFACE
  MODULE FUNCTION model_GetPhysicalName(dim, tag, name) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim, tag
    CHARACTER(*), INTENT(OUT) :: name
    INTEGER(I4B) :: ans
  END FUNCTION model_GetPhysicalName
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Set the tag of the entity of dimension `dim' and tag `tag' to the new value
!! `newTag'.

INTERFACE
  MODULE FUNCTION model_SetTag(dim, tag, newTag) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim, tag, newTag
    INTEGER(I4B) :: ans
  END FUNCTION model_SetTag
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the boundary of the model entities `dimTags'. Return in `outDimTags'
! the boundary of the individual entities (if `combined' is false) or the
! boundary of the combined geometrical shape formed by all input entities (if
!  `combined' is true). Return tags multiplied by the sign of the boundary
!  entity if `oriented' is true. Apply the boundary operator recursively down
!  to dimension 0 (i.e. to points) if `recursive' is true.

INTERFACE
  MODULE FUNCTION model_GetBoundary( &
    dimTags, outDimTags, combined, oriented, RECURSIVE) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dimTags(:, :)
    INTEGER(I4B), ALLOCATABLE, INTENT(OUT) :: outDimTags(:, :)
    LOGICAL, INTENT(IN), OPTIONAL :: combined
    LOGICAL, INTENT(IN), OPTIONAL :: oriented
    LOGICAL, INTENT(IN), OPTIONAL :: RECURSIVE
    INTEGER(I4B) :: ans
  END FUNCTION model_GetBoundary
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the upward and downward adjacencies of the model entity of dimension
! `dim' and tag `tag'. The `upward' vector returns the adjacent entities of
! dimension `dim' + 1; the `downward' vector returns the adjacent entities of
! dimension `dim' - 1. */

INTERFACE
  MODULE FUNCTION model_GetAdjacencies(dim, tag, upward, downward) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), INTENT(IN) :: tag
    INTEGER(I4B), ALLOCATABLE, INTENT(OUT) :: upward(:)
    INTEGER(I4B), ALLOCATABLE, INTENT(OUT) :: downward(:)
    INTEGER(I4B) :: ans
  END FUNCTION model_GetAdjacencies
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION model_GetEntitiesInBoundingBox( &
    xmin, ymin, zmin, xmax, ymax, zmax, dimTags, dim) RESULT(ans)
    REAL(DFP), INTENT(IN) :: xmin
    REAL(DFP), INTENT(IN) :: ymin
    REAL(DFP), INTENT(IN) :: zmin
    REAL(DFP), INTENT(IN) :: xmax
    REAL(DFP), INTENT(IN) :: ymax
    REAL(DFP), INTENT(IN) :: zmax
    INTEGER(I4B), ALLOCATABLE, INTENT(OUT) :: dimTags(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim
    INTEGER(I4B) :: ans
  END FUNCTION model_GetEntitiesInBoundingBox
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION model_GetBoundingBox(dim, tag, xmin, ymin, zmin, &
                                       xmax, ymax, zmax) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), INTENT(IN) :: tag
    REAL(DFP), INTENT(OUT) :: xmin
    REAL(DFP), INTENT(OUT) :: ymin
    REAL(DFP), INTENT(OUT) :: zmin
    REAL(DFP), INTENT(OUT) :: xmax
    REAL(DFP), INTENT(OUT) :: ymax
    REAL(DFP), INTENT(OUT) :: zmax
    INTEGER(I4B) :: ans
  END FUNCTION model_GetBoundingBox
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Return the geometrical dimension of the current model.

INTERFACE
  MODULE FUNCTION model_GetDimension() RESULT(ans)
    INTEGER(I4B) :: ans
  END FUNCTION model_GetDimension
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Add a discrete model entity (defined by a mesh) of dimension `dim' in the
!! current model. Return the tag of the new discrete entity, equal to `tag' if
!! `tag' is positive, or a new tag if `tag' < 0. `boundary' specifies the tags
!! of the entities on the boundary of the discrete entity, if any. Specifying
!! `boundary' allows Gmsh to construct the topology of the overall model.

INTERFACE
  MODULE FUNCTION model_AddDiscreteEntity(dim, tag, boundary) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), INTENT(IN), OPTIONAL :: tag
    INTEGER(I4B), INTENT(IN), OPTIONAL :: boundary(:)
    INTEGER(I4B) :: ans
  END FUNCTION model_AddDiscreteEntity
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Remove the entities `dimTags' (given as a vector of (dim, tag) pairs) of
!! the current model, provided that they are not on the boundary of (or
!! embedded in) higher-dimensional entities. If `recursive' is true, remove
!! all the entities on their boundaries, down to dimension 0.

INTERFACE
  MODULE FUNCTION model_RemoveEntities(dimTags, RECURSIVE) RESULT(ans)
    INTEGER(i4b), INTENT(in) :: dimTags(:, :)
    LOGICAL, INTENT(in), OPTIONAL :: RECURSIVE
    INTEGER(I4B) :: ans
  END FUNCTION model_RemoveEntities
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Remove the entity name `name' from the current model.

INTERFACE
  MODULE FUNCTION model_RemoveEntityName(name) RESULT(ans)
    CHARACTER(len=*), INTENT(in) :: name
    INTEGER(i4b) :: ans
  END FUNCTION model_RemoveEntityName
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the type of the entity of dimension `dim' and tag `tag'.

INTERFACE
  MODULE FUNCTION model_GetType(dim, tag) RESULT(ans)
    INTEGER(i4b), INTENT(in) :: dim
    INTEGER(i4b), INTENT(in) :: tag
    CHARACTER(len=:), ALLOCATABLE :: ans
  END FUNCTION model_GetType
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> In a partitioned model, get the parent of the entity of dimension `dim' and
!! tag `tag', i.e. from which the entity is a part of, if any. `parentDim' and
!! `parentTag' are Set to -1 if the entity has no parent.

INTERFACE
  MODULE FUNCTION model_GetParent(dim, tag, parentDim, parentTag) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), INTENT(IN) :: tag
    INTEGER(I4B), INTENT(OUT) :: parentDim
    INTEGER(I4B), INTENT(OUT) :: parentTag
    INTEGER(I4B) :: ans
  END FUNCTION model_GetParent
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Return the number of partitions in the model.

INTERFACE
  MODULE FUNCTION model_GetNumberOfPartitions() RESULT(ans)
    INTEGER(I4B) :: ans
  END FUNCTION model_GetNumberOfPartitions
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! In a partitioned model, return the tags of the partition(s) to which the
!! entity belongs.

INTERFACE
  MODULE FUNCTION model_GetPartitions(dim, tag) RESULT(ans)
    INTEGER(I4B), INTENT(in) :: dim
    INTEGER(I4B), INTENT(in) :: tag
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION model_GetPartitions
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Evaluate the parametrization of the entity of dimension `dim' and tag `tag'
!! at the parametric coordinates `parametricCoord'. Only valid for `dim' equal
!! to 0 (with empty `parametricCoord'), 1 (with `parametricCoord' containing
!! parametric coordinates on the curve) or 2 (with `parametricCoord'
!! containing u, v parametric coordinates on the surface, concatenated: [p1u,
!! p1v, p2u, ...]). Return x, y, z coordinates in `coord', concatenated: [p1x,
!! p1y, p1z, p2x, ...].

INTERFACE
  MODULE FUNCTION model_GetValue(dim, tag, parametricCoord) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), INTENT(IN) :: tag
    REAL(DFP), DIMENSION(:), INTENT(IN) :: parametricCoord
    REAL(DFP), DIMENSION(:), ALLOCATABLE :: ans
  END FUNCTION model_GetValue
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Evaluate the derivative of the parametrization of the entity of dimension
!! `dim' and tag `tag' at the parametric coordinates `parametricCoord'. Only
!! valid for `dim' equal to 1 (with `parametricCoord' containing parametric
!! coordinates on the curve) or 2 (with `parametricCoord' containing u, v
!! parametric coordinates on the surface, concatenated: [p1u, p1v, p2u, ...]).
!! For `dim' equal to 1 return the x, y, z components of the derivative with
!! respect to u [d1ux, d1uy, d1uz, d2ux, ...]; for `dim' equal to 2 return the
!! x, y, z components of the derivative with respect to u and v: [d1ux, d1uy,
!! d1uz, d1vx, d1vy, d1vz, d2ux, ...].

INTERFACE
  MODULE FUNCTION model_GetDerivative(dim, tag, parametricCoord) &
    RESULT(derivatives)
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), INTENT(IN) :: tag
    REAL(DFP), DIMENSION(:), INTENT(IN) :: parametricCoord
    REAL(DFP), DIMENSION(:), ALLOCATABLE :: derivatives
  END FUNCTION model_GetDerivative
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Evaluate the second derivative of the parametrization of the entity of
!! dimension `dim' and tag `tag' at the parametric coordinates
!! `parametricCoord'. Only valid for `dim' equal to 1 (with `parametricCoord'
!! containing parametric coordinates on the curve) or 2 (with
!! `parametricCoord' containing u, v parametric coordinates on the surface,
!! concatenated: [p1u, p1v, p2u, ...]). For `dim' equal to 1 return the x, y,
!! z components of the second derivative with respect to u [d1uux, d1uuy,
!! d1uuz, d2uux, ...]; for `dim' equal to 2 return the x, y, z components of
!! the second derivative with respect to u and v, and the mixed derivative
!! with respect to u and v: [d1uux, d1uuy, d1uuz, d1vvx, d1vvy, d1vvz, d1uvx,
!! d1uvy, d1uvz, d2uux, ...].

INTERFACE
  MODULE FUNCTION model_GetSecondDerivative(dim, tag, parametricCoord) &
    RESULT(derivatives)
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), INTENT(IN) :: tag
    REAL(DFP), DIMENSION(:), INTENT(IN) :: parametricCoord
    REAL(DFP), DIMENSION(:), ALLOCATABLE :: derivatives
  END FUNCTION model_GetSecondDerivative
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Evaluate the (maximum) curvature of the entity of dimension `dim' and tag
!! `tag' at the parametric coordinates `parametricCoord'. Only valid for `dim'
!! equal to 1 (with `parametricCoord' containing parametric coordinates on the
!! curve) or 2 (with `parametricCoord' containing u, v parametric coordinates
!! on the surface, concatenated: [p1u, p1v, p2u, ...]).

INTERFACE
  MODULE FUNCTION model_GetCurvature(dim, tag, parametricCoord) &
    RESULT(curvatures)
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), INTENT(IN) :: tag
    REAL(DFP), DIMENSION(:), INTENT(IN) :: parametricCoord
    REAL(DFP), DIMENSION(:), ALLOCATABLE :: curvatures
  END FUNCTION model_GetCurvature
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Evaluate the principal curvatures of the surface with tag `tag' at the
!! parametric coordinates `parametricCoord', as well as their respective
!! directions. `parametricCoord' are given by pair of u and v coordinates,
!! concatenated: [p1u, p1v, p2u, ...].

INTERFACE
  MODULE FUNCTION model_GetPrincipalCurvatures( &
    tag, parametricCoord, curvatureMax, curvatureMin, directionMax, &
    directionMin) RESULT(ans)
    INTEGER(I4B), INTENT(in) :: tag
    REAL(DFP), INTENT(IN) :: parametricCoord(:)
    REAL(DFP), ALLOCATABLE, INTENT(OUT) :: curvatureMax(:)
    REAL(DFP), ALLOCATABLE, INTENT(OUT) :: curvatureMin(:)
    REAL(DFP), ALLOCATABLE, INTENT(OUT) :: directionMax(:)
    REAL(DFP), ALLOCATABLE, INTENT(OUT) :: directionMin(:)
    INTEGER(I4B) :: ans
  END FUNCTION model_GetPrincipalCurvatures
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Get the normal to the surface with tag `tag' at the parametric coordinates
!! `parametricCoord'. The `parametricCoord' vector should contain u and v
!! coordinates, concatenated: [p1u, p1v, p2u, ...]. `normals' are returned as
!! a vector of x, y, z components, concatenated: [n1x, n1y, n1z, n2x, ...].

INTERFACE
  MODULE FUNCTION model_GetNormal(tag, parametricCoord) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: tag
    REAL(DFP), INTENT(IN) :: parametricCoord(:)
    REAL(DFP), ALLOCATABLE :: ans(:)
    !! normals
  END FUNCTION model_GetNormal
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Get the parametric coordinates `parametricCoord' for the points `coord' on
!! the entity of dimension `dim' and tag `tag'. `coord' are given as x, y, z
!! coordinates, concatenated: [p1x, p1y, p1z, p2x, ...]. `parametricCoord'
!! returns the parametric coordinates t on the curve (if `dim' = 1) or u and v
!! coordinates concatenated on the surface (if `dim' = 2), i.e. [p1t, p2t,
!! ...] or [p1u, p1v, p2u, ...].

INTERFACE
  MODULE FUNCTION model_GetParametrization(dim, tag, coord) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), INTENT(IN) :: tag
    REAL(DFP), INTENT(IN) :: coord(:)
    REAL(DFP), ALLOCATABLE :: ans(:)
    !! parametricCoord
  END FUNCTION model_GetParametrization
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Get the `min' and `max' bounds of the parametric coordinates for the entity
!! of dimension `dim' and tag `tag'.

INTERFACE
  MODULE FUNCTION model_GetParametrizationBounds(dim, tag, min, max) &
    RESULT(ans)
    INTEGER(I4B), INTENT(in) :: dim
    INTEGER(I4B), INTENT(in) :: tag
    REAL(DFP), ALLOCATABLE, INTENT(OUT) :: MIN(:)
    REAL(DFP), ALLOCATABLE, INTENT(OUT) :: MAX(:)
    INTEGER(I4B) :: ans
  END FUNCTION model_GetParametrizationBounds
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Check if the coordinates (or the parametric coordinates if `parametric' is
!! Set) provided in `coord' correspond to points inside the entity of
!! dimension `dim' and tag `tag', and return the number of points inside. This
!! feature is only available for a subSet of entities, depending on the
!! underlying geometrical representation.

INTERFACE
  MODULE FUNCTION model_IsInside(dim, tag, coord, parametric) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), INTENT(IN) :: tag
    REAL(DFP), INTENT(IN) :: coord(:)
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: parametric
    LOGICAL(LGT) :: ans
  END FUNCTION model_IsInside
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Get the points `closestCoord' on the entity of dimension `dim' and tag
  !! `tag' to the points `coord', by orthogonal projection. `coord' and
  !! `closestCoord' are given as x, y, z coordinates, concatenated: [p1x, p1y,
  !! p1z, p2x, ...]. `parametricCoord' returns the parametric coordinates t on
  !! the curve (if `dim' = 1) or u and v coordinates concatenated on the surface
  !! (if `dim' = 2), i.e. [p1t, p2t, ...] or [p1u, p1v, p2u, ...].

INTERFACE
  MODULE FUNCTION model_GetClosestPoint(dim, tag, coord, closestCoord, &
                                        parametricCoord) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), INTENT(IN) :: tag
    REAL(DFP), INTENT(IN) :: coord(:)
    REAL(DFP), ALLOCATABLE, INTENT(OUT) :: closestCoord(:)
    REAL(DFP), ALLOCATABLE, INTENT(OUT) :: parametricCoord(:)
    INTEGER(I4B) :: ans
  END FUNCTION model_GetClosestPoint
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Reparametrize the boundary entity (point or curve, i.e. with `dim' == 0 or
!! `dim' == 1) of tag `tag' on the surface `surfaceTag'. If `dim' == 1,
!! reparametrize all the points corresponding to the parametric coordinates
!! `parametricCoord'. Multiple matches in case of periodic surfaces can be
!! selected with `which'. This feature is only available for a subSet of
!! entities, depending on the underlying geometrical representation.

INTERFACE
  MODULE FUNCTION model_ReparametrizeOnSurface( &
    dim, tag, parametricCoord, surfaceTag, which) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), INTENT(IN) :: tag
    REAL(DFP), INTENT(IN) :: parametricCoord(:)
    INTEGER(I4B), INTENT(IN) :: surfaceTag
    INTEGER(I4B), INTENT(IN), OPTIONAL :: which
    REAL(DFP), ALLOCATABLE :: ans(:)
    !! surfaceParametricCoord
  END FUNCTION model_ReparametrizeOnSurface
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Set the visibility of the model entities `dimTags' (given as a vector of
!! (dim, tag) pairs) to `value'. Apply the visibility Setting recursively if
!! `recursive' is true.

INTERFACE
  MODULE FUNCTION model_SetVisibility(dimTags, VALUE, RECURSIVE) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dimTags(:, :)
    INTEGER(I4B), INTENT(IN) :: VALUE
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: RECURSIVE
    INTEGER(I4B) :: ans
  END FUNCTION model_SetVisibility
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Get the visibility of the model entity of dimension `dim' and tag `tag'.

INTERFACE
  MODULE FUNCTION model_GetVisibility(dim, tag) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), INTENT(IN) :: tag
    INTEGER(I4B) :: ans
    !! VALUE
  END FUNCTION model_GetVisibility
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Set the global visibility of the model per window to `value', where
!! `windowIndex' identifies the window in the window list.

INTERFACE
  MODULE FUNCTION model_SetVisibilityPerWindow(VALUE, windowIndex) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: VALUE
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: windowIndex
    INTEGER(I4B) :: ans
  END FUNCTION model_SetVisibilityPerWindow
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Set the color of the model entities `dimTags' (given as a vector of (dim,
!! tag) pairs) to the RGBA value (`r', `g', `b', `a'), where `r', `g', `b' and
!! `a' should be integers between 0 and 255. Apply the color Setting
!! recursively if `recursive' is true.

INTERFACE
  MODULE FUNCTION model_SetColor(dimTags, r, g, b, a, RECURSIVE) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dimTags(:, :)
    INTEGER(I4B), INTENT(IN) :: r
    INTEGER(I4B), INTENT(IN) :: g
    INTEGER(I4B), INTENT(IN) :: b
    INTEGER(I4B), INTENT(IN), OPTIONAL :: a
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: RECURSIVE
    INTEGER(I4B) :: ans
  END FUNCTION model_SetColor
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Get the color of the model entity of dimension `dim' and tag `tag'.

INTERFACE
  MODULE FUNCTION model_GetColor(dim, tag) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), INTENT(IN) :: tag
    INTEGER(I4B) :: ans(4)
  END FUNCTION model_GetColor
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Set the `x', `y', `z' coordinates of a geometrical point.

INTERFACE
  MODULE FUNCTION model_SetCoordinates(tag, x, y, z) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: tag
    REAL(DFP), INTENT(IN) :: x
    REAL(DFP), INTENT(IN) :: y
    REAL(DFP), INTENT(IN) :: z
    INTEGER(I4B) :: ans
  END FUNCTION model_SetCoordinates
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Get the names of any optional attributes stored in the model.

INTERFACE
  MODULE FUNCTION model_GetAttributeNames() RESULT(names)
    TYPE(String), ALLOCATABLE :: names(:)
  END FUNCTION model_GetAttributeNames
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Get the values of the attribute with name `name'.

INTERFACE
  MODULE FUNCTION model_GetAttribute(name) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: name
    TYPE(String), ALLOCATABLE :: ans(:)
  END FUNCTION model_GetAttribute
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Set the values of the attribute with name `name'.

INTERFACE
  MODULE FUNCTION model_SetAttribute(name, values) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: name
    CHARACTER(*), INTENT(IN) :: values(:)
    INTEGER(I4B) :: ans
  END FUNCTION model_SetAttribute
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Remove the attribute with name `name'.

INTERFACE
  MODULE FUNCTION model_RemoveAttribute(name) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B) :: ans
  END FUNCTION model_RemoveAttribute
END INTERFACE

END MODULE GmshModel_Class

