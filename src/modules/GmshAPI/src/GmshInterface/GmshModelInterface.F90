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

MODULE GmshModelInterface
USE ISO_C_BINDING, ONLY: C_INT
USE ISO_C_BINDING, ONLY: C_DOUBLE
USE ISO_C_BINDING, ONLY: C_SIZE_T
USE ISO_C_BINDING, ONLY: C_PTR
USE ISO_C_BINDING, ONLY: C_CHAR
IMPLICIT NONE

PRIVATE
PUBLIC :: gmshModelAdd
PUBLIC :: gmshModelRemove
PUBLIC :: gmshModelList
PUBLIC :: gmshModelGetCurrent
PUBLIC :: gmshModelSetCurrent
PUBLIC :: gmshModelGetFileName
PUBLIC :: gmshModelSetFileName
PUBLIC :: gmshModelGetEntities
PUBLIC :: gmshModelSetEntityName
PUBLIC :: gmshModelGetEntityName
PUBLIC :: gmshModelGetPhysicalGroups
PUBLIC :: gmshModelGetEntitiesForPhysicalGroup
PUBLIC :: gmshModelGetPhysicalGroupsForEntity
PUBLIC :: gmshModelAddPhysicalGroup
PUBLIC :: gmshModelRemovePhysicalGroups
PUBLIC :: gmshModelSetPhysicalName
PUBLIC :: gmshModelRemovePhysicalName
PUBLIC :: gmshModelGetPhysicalName
PUBLIC :: gmshModelSetTag
PUBLIC :: gmshModelGetBoundary
PUBLIC :: gmshModelGetAdjacencies
PUBLIC :: gmshModelGetEntitiesInBoundingBox
PUBLIC :: gmshModelGetBoundingBox
PUBLIC :: gmshModelGetDimension
PUBLIC :: gmshModelAddDiscreteEntity
PUBLIC :: gmshModelRemoveEntities
PUBLIC :: gmshModelRemoveEntityName
PUBLIC :: gmshModelGetType
PUBLIC :: gmshModelGetParent
PUBLIC :: gmshModelGetNumberOfPartitions
PUBLIC :: gmshModelGetPartitions
PUBLIC :: gmshModelGetValue
PUBLIC :: gmshModelGetDerivative
PUBLIC :: gmshModelGetSecondDerivative
PUBLIC :: gmshModelGetCurvature
PUBLIC :: gmshModelGetPrincipalCurvatures
PUBLIC :: gmshModelGetNormal
PUBLIC :: gmshModelGetParametrization
PUBLIC :: gmshModelGetParametrizationBounds
PUBLIC :: gmshModelIsInside
PUBLIC :: gmshModelGetClosestPoint
PUBLIC :: gmshModelReparametrizeOnSurface
PUBLIC :: gmshModelSetVisibility
PUBLIC :: gmshModelGetVisibility
PUBLIC :: gmshModelSetVisibilityPerWindow
PUBLIC :: gmshModelSetColor
PUBLIC :: gmshModelGetColor
PUBLIC :: gmshModelSetCoordinates
PUBLIC :: gmshModelGetAttributeNames
PUBLIC :: gmshModelSetAttribute
PUBLIC :: gmshModelGetAttribute
PUBLIC :: gmshModelRemoveAttribute

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Add a new model, with name `name', and set it as the current model. */
!
! GMSH_API void gmshModelAdd(const char *name,
!                            int *ierr);

INTERFACE
  SUBROUTINE gmshModelAdd(name, ierr) BIND(C, NAME="gmshModelAdd")
    IMPORT
    TYPE(C_PTR), VALUE, INTENT(IN) :: name
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelAdd
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Remove the current model. */
!
! GMSH_API void gmshModelRemove(int *ierr);

INTERFACE
  SUBROUTINE gmshModelRemove(ierr) BIND(C, NAME="gmshModelRemove")
    IMPORT
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelRemove
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* List the names of all models. */
!
! GMSH_API void gmshModelList(char ***names, size_t *names_n,
!                             int *ierr);

INTERFACE
  SUBROUTINE gmshModelList(names, names_n, ierr) &
    & BIND(C, NAME="gmshModelList")
    IMPORT
    TYPE(C_PTR), INTENT(IN) :: names
    INTEGER(C_SIZE_T), INTENT(OUT) :: names_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelList
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Get the name of the current model. */
!
! GMSH_API void gmshModelGetCurrent(char **name,
!                                   int *ierr);

INTERFACE
  SUBROUTINE gmshModelGetCurrent(name, ierr) &
    & BIND(C, NAME="gmshModelGetCurrent")
    IMPORT
    TYPE(C_PTR), INTENT(IN) :: name
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGetCurrent
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Set the current model to the model with name `name'.
! If several models have
!
!  * the same name, select the one that was added first. */
! GMSH_API void gmshModelSetCurrent(const char *name,
!                                   int *ierr);

INTERFACE
  SUBROUTINE gmshModelSetCurrent(name, ierr) &
    & BIND(C, NAME="gmshModelSetCurrent")
    IMPORT
    TYPE(C_PTR), VALUE, INTENT(IN) :: name
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelSetCurrent
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the file name (if any) associated with the current model.
! A file name
! is associated when a model is read from a file on disk. */
!
! GMSH_API void gmshModelGetFileName(char **fileName,
!                                    int *ierr);

INTERFACE
  SUBROUTINE gmshModelGetFileName(fileName, ierr) &
    & BIND(C, NAME="gmshModelGetFileName")
    IMPORT
    TYPE(C_PTR), INTENT(IN) :: fileName
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGetFileName
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Set the file name associated with the current model. */
!
! GMSH_API void gmshModelSetFileName(const char *fileName,
!                                    int *ierr);

INTERFACE
  SUBROUTINE gmshModelSetFileName(fileName, ierr) &
    & BIND(C, NAME="gmshModelSetFileName")
    IMPORT
    TYPE(C_PTR), VALUE, INTENT(IN) :: fileName
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelSetFileName
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Get all the entities in the current model. If `dim' is >= 0, return only
!  * the entities of the specified dimension (e.g. points if `dim' == 0). The
!  * entities are returned as a vector of (dim, tag) integer pairs. */
!
! GMSH_API void gmshModelGetEntities(int **dimTags, size_t *dimTags_n,
!                                    const int dim,
!                                    int *ierr);

INTERFACE
  SUBROUTINE gmshModelGetEntities(dimTags, dimTags_n, dim, ierr) &
    & BIND(C, NAME="gmshModelGetEntities")
    IMPORT
    TYPE(C_PTR), INTENT(IN) :: dimTags
    INTEGER(C_SIZE_T), INTENT(OUT) :: dimTags_n
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGetEntities
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Set the name of the entity of dimension `dim' and tag `tag'.
!
! GMSH_API void gmshModelSetEntityName(const int dim,
!                                      const int tag,
!                                      const char *name,
!                                      int *ierr);

INTERFACE
  SUBROUTINE gmshModelSetEntityName(dim, tag, name, ierr) &
    & BIND(C, NAME="gmshModelSetEntityName")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    TYPE(C_PTR), VALUE, INTENT(IN) :: name
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelSetEntityName
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the name of the entity of dimension `dim' and tag `tag'.
!
! GMSH_API void gmshModelGetEntityName(const int dim,
!                                      const int tag,
!                                      char **name,
!                                      int *ierr);

INTERFACE
  SUBROUTINE gmshModelGetEntityName(dim, tag, name, ierr) &
    & BIND(C, NAME="gmshModelGetEntityName")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    TYPE(C_PTR), INTENT(IN) :: name
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGetEntityName
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get all the physical groups in the current model. If `dim' is >= 0,
! return
! only the entities of the specified dimension (e.g. physical points if `dim'
!  * == 0). The entities are returned as a vector of (dim, tag) integer
! pairs.
!
! GMSH_API void gmshModelGetPhysicalGroups(int **dimTags, size_t *dimTags_n,
!                                          const int dim,
!                                          int *ierr);

INTERFACE
  SUBROUTINE gmshModelGetPhysicalGroups(dimTags, dimTags_n, dim, ierr) &
    & BIND(C, NAME="gmshModelGetPhysicalGroups")
    IMPORT
    TYPE(C_PTR), INTENT(IN) :: dimTags
    INTEGER(C_SIZE_T), INTENT(OUT) :: dimTags_n
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGetPhysicalGroups
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the tags of the model entities making up the physical group of
! dimension `dim' and tag `tag'. */

INTERFACE
  SUBROUTINE gmshModelGetEntitiesForPhysicalGroup( &
    dim, tag, tags, tags_n, ierr) &
    BIND(C, NAME="gmshModelGetEntitiesForPhysicalGroup")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    TYPE(C_PTR), INTENT(IN) :: tags
    INTEGER(C_SIZE_T), INTENT(OUT) :: tags_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGetEntitiesForPhysicalGroup
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the tags of the physical groups (if any) to which the model entity of
! dimension `dim' and tag `tag' belongs.

INTERFACE
  SUBROUTINE gmshModelGetPhysicalGroupsForEntity( &
    dim, tag, physicalTags, physicalTags_n, ierr) &
    BIND(C, NAME="gmshModelGetPhysicalGroupsForEntity")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    TYPE(C_PTR), INTENT(IN) :: physicalTags
    INTEGER(C_SIZE_T), INTENT(OUT) :: physicalTags_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGetPhysicalGroupsForEntity
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Add a physical group of dimension `dim', grouping the model entities with
! * tags `tags'. Return the tag of the physical group, equal to `tag'
! if `tag' * is positive, or a new tag if `tag' < 0.

INTERFACE
  FUNCTION gmshModelAddPhysicalGroup( &
    dim, tags, tags_n, tag, name, ierr) RESULT(ans) &
    BIND(C, NAME="gmshModelAddPhysicalGroup")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: tags_n
    INTEGER(C_INT), INTENT(IN) :: tags(tags_n)
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    TYPE(C_PTR), INTENT(IN) :: name
    INTEGER(C_INT), INTENT(OUT) :: ierr
    INTEGER(C_INT) :: ans
  END FUNCTION gmshModelAddPhysicalGroup
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Remove the physical groups `dimTags' from the current model. If `dimTags'
! is empty, remove all groups.

INTERFACE
  SUBROUTINE gmshModelRemovePhysicalGroups(dimTags, dimTags_n, ierr) &
    BIND(C, NAME="gmshModelRemovePhysicalGroups")
    IMPORT
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: dimtags_n
    INTEGER(C_INT), INTENT(in) :: dimTags(dimTags_n)
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelRemovePhysicalGroups
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Set the name of the physical group of dimension `dim' and tag `tag'.
!
! GMSH_API void gmshModelSetPhysicalName(const int dim,
!                                        const int tag,
!                                        const char *name,
!                                        int *ierr);

INTERFACE
  SUBROUTINE gmshModelSetPhysicalName(dim, tag, name, ierr) &
    BIND(C, NAME="gmshModelSetPhysicalName")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim, tag
    TYPE(C_PTR), VALUE, INTENT(IN) :: name
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelSetPhysicalName
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Remove the physical name `name' from the current model. */
! GMSH_API void gmshModelRemovePhysicalName(const char *name,
!                                           int *ierr);

INTERFACE
  SUBROUTINE gmshModelRemovePhysicalName(name, ierr) &
    BIND(C, NAME="gmshModelRemovePhysicalName")
    IMPORT
    TYPE(C_PTR), VALUE, INTENT(IN) :: name
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelRemovePhysicalName
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Get the name of the physical group of dimension `dim' and tag `tag'. */
! GMSH_API void gmshModelGetPhysicalName(const int dim,
!                                        const int tag,
!                                        char **name,
!                                        int *ierr);

INTERFACE
  SUBROUTINE gmshModelGetPhysicalName(dim, tag, name, ierr) &
    BIND(C, NAME="gmshModelGetPhysicalName")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim, tag
    TYPE(C_PTR), INTENT(IN) :: name
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGetPhysicalName
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Set the tag of the entity of dimension `dim' and tag `tag' to the new
!! value `newTag`.

INTERFACE
  SUBROUTINE gmshModelSetTag(dim, tag, newTag, ierr) &
    BIND(C, name="gmshModelSetTag")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(in) :: dim
    INTEGER(C_INT), VALUE, INTENT(in) :: tag
    INTEGER(C_INT), VALUE, INTENT(in) :: newTag
    INTEGER(C_INT), INTENT(out) :: ierr
  END SUBROUTINE gmshModelSetTag
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the boundary of the model entities `dimTags'. Return in `outDimTags'
! the boundary of the individual entities (if `combined' is false) or the
! boundary of the combined geometrical shape formed by all input entities (if
!  `combined' is true). Return tags multiplied by the sign of the boundary
!  entity if `oriented' is true. Apply the boundary operator recursively down
!  to dimension 0 (i.e. to points) if `recursive' is true. */
!

INTERFACE
  SUBROUTINE gmshModelGetBoundary( &
    dimTags, dimTags_n, outDimTags, outDimTags_n, combined, &
    oriented, RECURSIVE, ierr) BIND(C, NAME="gmshModelGetBoundary")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dimTags_n
    INTEGER(C_INT), INTENT(in) :: dimTags(dimTags_n)
    TYPE(C_PTR), INTENT(IN) :: outDimTags
    INTEGER(C_SIZE_T), INTENT(OUT) :: outDimTags_n
    INTEGER(C_INT), VALUE, INTENT(IN) :: combined, oriented, RECURSIVE
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGetBoundary
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the upward and downward adjacencies of the model entity of dimension
! `dim' and tag `tag'. The `upward' vector returns the adjacent entities of
! dimension `dim' + 1; the `downward' vector returns the adjacent entities of
! dimension `dim' - 1. */

INTERFACE
  SUBROUTINE gmshModelGetAdjacencies( &
    dim, tag, upward, upward_n, downward, downward_n, ierr) &
    BIND(C, NAME="gmshModelGetAdjacencies")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    TYPE(C_PTR), INTENT(IN) :: upward, downward
    INTEGER(C_SIZE_T), INTENT(OUT) :: upward_n, downward_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGetAdjacencies
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the model entities in the bounding box defined by the two points
! (`xmin', `ymin', `zmin') and (`xmax', `ymax', `zmax'). If `dim' is >= 0,
! return only the entities of the specified dimension (e.g. points if `dim'
! == 0).

INTERFACE
  SUBROUTINE gmshModelGetEntitiesInBoundingBox( &
    xmin, ymin, zmin, xmax, ymax, zmax, tags, tags_n, dim, ierr) &
    BIND(C, NAME="gmshModelGetEntitiesInBoundingBox")
    IMPORT
    REAL(C_DOUBLE), VALUE, INTENT(IN) :: xmin, ymin, zmin, xmax, ymax, zmax
    TYPE(C_PTR), INTENT(IN) :: tags
    INTEGER(C_SIZE_T), INTENT(OUT) :: tags_n
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGetEntitiesInBoundingBox
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the bounding box (`xmin', `ymin', `zmin'), (`xmax', `ymax', `zmax') of
! the model entity of dimension `dim' and tag `tag'. If `dim' and `tag' are
! negative, get the bounding box of the whole model. */

INTERFACE
  SUBROUTINE gmshModelGetBoundingBox( &
    dim, tag, xmin, ymin, zmin, xmax, ymax, zmax, ierr) &
    BIND(C, NAME="gmshModelGetBoundingBox")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim, tag
    REAL(C_DOUBLE), INTENT(OUT) :: xmin, ymin, zmin, xmax, ymax, zmax
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGetBoundingBox
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Get the geometrical dimension of the current model. */
! GMSH_API int gmshModelGetDimension(int *ierr);

INTERFACE
  FUNCTION gmshModelGetDimension(ierr) RESULT(ans) &
    BIND(C, NAME="gmshModelGetDimension")
    IMPORT
    INTEGER(C_INT), INTENT(OUT) :: ierr
    INTEGER(C_INT) :: ans
  END FUNCTION gmshModelGetDimension
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Add a discrete model entity (defined by a mesh) of dimension `dim' in the
! current model. Return the tag of the new discrete entity, equal to `tag' if
! `tag' is positive, or a new tag if `tag' < 0. `boundary' specifies the tags
! of the entities on the boundary of the discrete entity, if any. Specifying
! `boundary' allows Gmsh to construct the topology of the overall model. */

INTERFACE
  FUNCTION gmshModelAddDiscreteEntity( &
    dim, tag, boundary, boundary_n, ierr) RESULT(ans) &
    BIND(C, NAME="gmshModelAddDiscreteEntity")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim, tag
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: boundary_n
    INTEGER(C_INT), INTENT(in) :: boundary(boundary_n)
    INTEGER(C_INT), INTENT(OUT) :: ierr
    INTEGER(C_INT) :: ans
  END FUNCTION gmshModelAddDiscreteEntity
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Remove the entities `dimTags' of the current model. If `recursive' is true,
! remove all the entities on their boundaries, down to dimension 0. */

INTERFACE
  SUBROUTINE gmshModelRemoveEntities( &
    dimTags, dimTags_n, RECURSIVE, ierr) &
    BIND(C, NAME="gmshModelRemoveEntities")
    IMPORT
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: dimTags_n
    INTEGER(C_INT), INTENT(in) :: dimTags(dimTags_n)
    INTEGER(C_INT), VALUE, INTENT(IN) :: RECURSIVE
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelRemoveEntities
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Remove the entity name `name' from the current model. */

INTERFACE
  SUBROUTINE gmshModelRemoveEntityName(name, ierr) &
    BIND(C, NAME="gmshModelRemoveEntityName")
    IMPORT
    CHARACTER(len=1, kind=C_CHAR), DIMENSION(*), INTENT(in) :: name
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelRemoveEntityName
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Get the type of the entity of dimension `dim' and tag `tag'. */

INTERFACE
  SUBROUTINE gmshModelGetType(dim, tag, entityType, ierr) &
    BIND(C, NAME="gmshModelGetType")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim, tag
    CHARACTER(kind=C_CHAR), DIMENSION(*) :: entityType
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGetType
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! In a partitioned model, get the parent of the entity of dimension `dim' and
! tag `tag', i.e. from which the entity is a part of, if any. `parentDim' and
! `parentTag' are set to -1 if the entity has no parent. */

INTERFACE
  SUBROUTINE gmshModelGetParent(dim, tag, parentDim, parentTag, ierr) &
    BIND(C, NAME="gmshModelGetParent")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim, tag
    INTEGER(C_INT), INTENT(OUT) :: parentDim, parentTag
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGetParent
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Return the number of partitions in the model.

INTERFACE
  FUNCTION gmshModelGetNumberOfPartitions(ierr) RESULT(ans) &
    BIND(C, name="gmshModelGetNumberOfPartitions")
    IMPORT
    INTEGER(C_INT), INTENT(OUT) :: ierr
    INTEGER(C_INT) :: ans
  END FUNCTION gmshModelGetNumberOfPartitions
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* In a partitioned model, return the tags of the partition(s) to which the
!  * entity belongs. */

INTERFACE
  SUBROUTINE gmshModelGetPartitions( &
    dim, tag, partitions, partitions_n, ierr) &
    BIND(C, NAME="gmshModelGetPartitions")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim, tag
    TYPE(C_PTR), INTENT(IN) :: partitions
    INTEGER(C_SIZE_T), INTENT(OUT) :: partitions_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGetPartitions
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Evaluate the parametrization of the entity of dimension `dim' and tag `tag'
! at the parametric coordinates `parametricCoord'. Only valid for `dim' equal
! to 0 (with empty `parametricCoord'), 1 (with `parametricCoord' containing
! parametric coordinates on the curve) or 2 (with `parametricCoord'
! containing
! pairs of u, v parametric coordinates on the surface, concatenated:
! [p1u, p1v, p2u, ...]). Return triplets of x, y, z coordinates in `coord',
! concatenated: [p1x, p1y, p1z, p2x, ...]. */

INTERFACE
  SUBROUTINE gmshModelGetValue( &
    dim, tag, parametricCoord, parametricCoord_n, coord, coord_n, ierr) &
    BIND(C, NAME="gmshModelGetValue")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim, tag
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: parametricCoord_n
    REAL(C_DOUBLE), INTENT(IN) :: parametricCoord(parametricCoord_n)
    TYPE(C_PTR), INTENT(IN) :: coord
    INTEGER(C_SIZE_T), INTENT(OUT) :: coord_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGetValue
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Evaluate the derivative of the parametrization of the entity of dimension
! `dim' and tag `tag' at the parametric coordinates `parametricCoord'. Only
! valid for `dim' equal to 1 (with `parametricCoord' containing parametric
! coordinates on the curve) or 2 (with `parametricCoord' containing pairs of
! u, v parametric coordinates on the surface, concatenated: [p1u, p1v, p2u,
! ...]). For `dim' equal to 1 return the x, y, z components of the derivative
! with respect to u [d1ux, d1uy, d1uz, d2ux, ...]; for `dim' equal to 2
! return the x, y, z components of the derivative with respect to u and v:
! [d1ux, d1uy, d1uz, d1vx, d1vy, d1vz, d2ux, ...].

INTERFACE
  SUBROUTINE gmshModelGetDerivative( &
    dim, tag, parametricCoord, parametricCoord_n, derivatives, &
    derivatives_n, ierr) BIND(C, NAME="gmshModelGetDerivative")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim, tag
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: parametricCoord_n
    REAL(C_DOUBLE), INTENT(IN) :: parametricCoord(parametricCoord_n)
    TYPE(C_PTR), INTENT(IN) :: derivatives
    INTEGER(C_SIZE_T), INTENT(OUT) :: derivatives_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGetDerivative
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Evaluate the second derivative of the parametrization of the entity of
! dimension `dim' and tag `tag' at the parametric coordinates
! `parametricCoord'. Only valid for `dim' equal to 1 (with `parametricCoord'
! containing parametric coordinates on the curve) or 2 (with
! `parametricCoord' containing pairs of u, v parametric coordinates on the
! surface, concatenated: [p1u, p1v, p2u, ...]). For `dim' equal to 1 return
! the x, y, z components of the second derivative with respect to u [d1uux,
! d1uuy, d1uuz, d2uux, ...]; for `dim' equal to 2 return the x, y, z
! components of the second derivative with respect to u and v, and the mixed
! derivative with respect to u and v: [d1uux, d1uuy, d1uuz, d1vvx, d1vvy,
! d1vvz, d1uvx, d1uvy, d1uvz, d2uux, ...].

INTERFACE
  SUBROUTINE gmshModelGetSecondDerivative( &
    dim, tag, parametricCoord, parametricCoord_n, derivatives, &
    derivatives_n, ierr) BIND(C, NAME="gmshModelGetSecondDerivative")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim, tag
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: parametricCoord_n
    REAL(C_DOUBLE), INTENT(IN) :: parametricCoord(parametricCoord_n)
    TYPE(C_PTR), INTENT(IN) :: derivatives
    INTEGER(C_SIZE_T), INTENT(OUT) :: derivatives_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGetSecondDerivative
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Evaluate the (maximum) curvature of the entity of dimension `dim' and tag
! `tag' at the parametric coordinates `parametricCoord'. Only valid for `dim'
! equal to 1 (with `parametricCoord' containing parametric coordinates on the
! curve) or 2 (with `parametricCoord' containing pairs of u, v parametric
! coordinates on the surface, concatenated: [p1u, p1v, p2u, ...]).

INTERFACE
  SUBROUTINE gmshModelGetCurvature( &
    dim, tag, parametricCoord, parametricCoord_n, curvatures, &
    curvatures_n, ierr) BIND(C, NAME="gmshModelGetCurvature")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim, tag
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: parametricCoord_n
    REAL(C_DOUBLE), INTENT(IN) :: parametricCoord(parametricCoord_n)
    TYPE(C_PTR), INTENT(IN) :: curvatures
    INTEGER(C_SIZE_T), INTENT(OUT) :: curvatures_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGetCurvature
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Evaluate the principal curvatures of the surface with tag `tag' at the
! parametric coordinates `parametricCoord', as well as their respective
! directions. `parametricCoord' are given by pair of u and v coordinates,
! concatenated: [p1u, p1v, p2u, ...].

INTERFACE
  SUBROUTINE gmshModelGetPrincipalCurvatures( &
    tag, parametricCoord, parametricCoord_n, curvatureMax, &
    curvatureMax_n, curvatureMin, curvatureMin_n, directionMax, &
    directionMax_n, directionMin, directionMin_n, ierr) &
    BIND(C, NAME="gmshModelGetPrincipalCurvatures")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: parametricCoord_n
    REAL(C_DOUBLE), INTENT(IN) :: parametricCoord(parametricCoord_n)
    TYPE(C_PTR), INTENT(IN) :: curvatureMax, curvatureMin, directionMax, &
                               directionMin
    INTEGER(C_SIZE_T), INTENT(OUT) :: curvatureMax_n, curvatureMin_n, &
                                      directionMax_n, directionMin_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGetPrincipalCurvatures
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the normal to the surface with tag `tag' at the parametric coordinates
! `parametricCoord'. `parametricCoord' are given by pairs of u and v
! coordinates, concatenated: [p1u, p1v, p2u, ...]. `normals' are returned as
!  triplets of x, y, z components, concatenated: [n1x, n1y, n1z, n2x, ...].

INTERFACE
  SUBROUTINE gmshModelGetNormal( &
    tag, parametricCoord, parametricCoord_n, normals, normals_n, ierr) &
    BIND(C, NAME="gmshModelGetNormal")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: parametricCoord_n
    REAL(C_DOUBLE), INTENT(IN) :: parametricCoord(parametricCoord_n)
    TYPE(C_PTR), INTENT(IN) :: normals
    INTEGER(C_SIZE_T), INTENT(OUT) :: normals_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGetNormal
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the parametric coordinates `parametricCoord' for the points `coord' on
! the entity of dimension `dim' and tag `tag'. `coord' are given as triplets
! of x, y, z coordinates, concatenated: [p1x, p1y, p1z, p2x, ...].
! `parametricCoord' returns the parametric coordinates t on the curve
! (if * `dim' = 1) or pairs of u and v coordinates concatenated on the
!  surface (if * `dim' = 2), i.e. [p1t, p2t, ...] or [p1u, p1v, p2u, ...].

INTERFACE
  SUBROUTINE gmshModelGetParametrization( &
    dim, tag, coord, coord_n, parametricCoord, &
    parametricCoord_n, ierr) BIND(C, NAME="gmshModelGetParametrization")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim, tag
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: coord_n
    REAL(C_DOUBLE), INTENT(IN) :: coord(coord_n)
    TYPE(C_PTR), INTENT(IN) :: parametricCoord
    INTEGER(C_SIZE_T), INTENT(OUT) :: parametricCoord_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGetParametrization
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the `min' and `max' bounds of the parametric coordinates for the entity
! of dimension `dim' and tag `tag'.

INTERFACE
  SUBROUTINE gmshModelGetParametrizationBounds( &
    dim, tag, min, min_n, max, max_n, ierr) &
    BIND(C, NAME="gmshModelGetParametrizationBounds")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim, tag
    TYPE(C_PTR), INTENT(IN) :: min, max
    INTEGER(C_SIZE_T), INTENT(OUT) :: min_n, max_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGetParametrizationBounds
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Check if the parametric coordinates provided in `parametricCoord'
! correspond to points inside the entitiy of dimension `dim' and tag `tag',
! and return the number of points inside. This feature is only available for
! a subset of curves and surfaces, depending on the underyling geometrical
! representation.

INTERFACE
  FUNCTION gmshModelIsInside( &
    dim, tag, coord, coord_n, parametric, ierr) RESULT(ans) &
    BIND(C, NAME="gmshModelIsInside")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim, tag
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: coord_n
    REAL(C_DOUBLE), INTENT(IN) :: coord(coord_n)
    INTEGER(C_INT), VALUE, INTENT(IN) :: parametric
    INTEGER(C_INT), INTENT(OUT) :: ierr
    INTEGER(C_INT) :: ans
  END FUNCTION gmshModelIsInside
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the points `closestCoord' on the entity of dimension `dim' and tag
! `tag' to the points `coord', by orthogonal projection. `coord' and
! `closestCoord' are given as triplets of x, y, z coordinates, concatenated:
! [p1x, p1y, p1z, p2x, ...]. `parametricCoord' returns the parametric
! coordinates t on the curve (if `dim' = 1) or pairs of u and v coordinates
! concatenated on the surface (if `dim' = 2), i.e. [p1t, p2t, ...] or [p1u,
! p1v, p2u, ...]

INTERFACE
  SUBROUTINE gmshModelGetClosestPoint( &
    dim, tag, coord, coord_n, closestCoord, closestCoord_n, &
    parametricCoord, parametricCoord_n, ierr) &
    BIND(C, NAME="gmshModelGetClosestPoint")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim, tag
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: coord_n
    REAL(C_DOUBLE), INTENT(IN) :: coord(coord_n)
    TYPE(C_PTR), INTENT(IN) :: closestCoord, parametricCoord
    INTEGER(C_SIZE_T), INTENT(OUT) :: closestCoord_n, parametricCoord_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGetClosestPoint
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Reparametrize the boundary entity (point or curve, i.e. with `dim' == 0 or
! `dim' == 1) of tag `tag' on the surface `surfaceTag'. If `dim' == 1,
! reparametrize all the points corresponding to the parametric coordinates
! `parametricCoord'. Multiple matches in case of periodic surfaces can be
! selected with `which'. This feature is only available for a subset of
! entities, depending on the underyling geometrical representation. */

INTERFACE
  SUBROUTINE gmshModelReparametrizeOnSurface( &
    dim, tag, parametricCoord, parametricCoord_n, surfaceTag, &
    surfaceParametricCoord, surfaceParametricCoord_n, which, ierr) &
    BIND(C, NAME="gmshModelReparametrizeOnSurface")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim, tag, surfaceTag, which
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: parametricCoord_n
    REAL(C_DOUBLE), INTENT(IN) :: parametricCoord(parametricCoord_n)
    TYPE(C_PTR), INTENT(IN) :: surfaceParametricCoord
    INTEGER(C_SIZE_T), INTENT(OUT) :: surfaceParametricCoord_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelReparametrizeOnSurface
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Set the visibility of the model entities `dimTags' to `value'. Apply the
! visibility setting recursively if `recursive' is true.
!
! GMSH_API void gmshModelSetVisibility(int *dimTags, size_t dimTags_n,
!                                      const int value,
!                                      const int recursive,
!                                      int *ierr);
!

INTERFACE
  SUBROUTINE gmshModelSetVisibility( &
    dimTags, dimTags_n, VALUE, RECURSIVE, ierr) &
    BIND(C, NAME="gmshModelSetVisibility")
    IMPORT
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: dimTags_n
    INTEGER(C_INT), INTENT(in) :: dimTags(dimTags_n)
    INTEGER(C_INT), VALUE, INTENT(IN) :: VALUE, RECURSIVE
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelSetVisibility
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the visibility of the model entity of dimension `dim' and tag `tag'.
! GMSH_API void gmshModelGetVisibility(const int dim,
!                                      const int tag,
!                                      int *value,
!                                      int *ierr);

INTERFACE
  SUBROUTINE gmshModelGetVisibility(dim, tag, VALUE, ierr) &
    BIND(C, NAME="gmshModelGetVisibility")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim, tag
    INTEGER(C_INT), INTENT(OUT) :: VALUE
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGetVisibility
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Set the global visibility of the model per window to `value', where
! `windowIndex' identifies the window in the window list.
!
! GMSH_API void gmshModelSetVisibilityPerWindow(const int value,
!                                               const int windowIndex,
!                                               int *ierr);

INTERFACE
  SUBROUTINE gmshModelSetVisibilityPerWindow(VALUE, windowIndex, ierr) &
    BIND(C, NAME="gmshModelSetVisibilityPerWindow")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: VALUE, windowIndex
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelSetVisibilityPerWindow
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Set the color of the model entities `dimTags' to the RGBA value (`r', `g',
! `b', `a'), where `r', `g', `b' and `a' should be integers between 0 and
! 255. Apply the color setting recursively if `recursive' is true.
!
! GMSH_API void gmshModelSetColor(int *dimTags, size_t dimTags_n,
!                                 const int r,
!                                 const int g,
!                                 const int b,
!                                 const int a,
!                                 const int recursive,
!                                 int *ierr);

INTERFACE
  SUBROUTINE gmshModelSetColor( &
    dimTags, dimTags_n, r, g, b, a, RECURSIVE, ierr) &
    BIND(C, NAME="gmshModelSetColor")
    IMPORT
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: dimTags_n
    INTEGER(C_INT), INTENT(in) :: dimTags(dimTags_n)
    INTEGER(C_INT), VALUE, INTENT(IN) :: r, g, b, a, RECURSIVE
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelSetColor
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the color of the model entity of dimension `dim' and tag `tag'.
!
! GMSH_API void gmshModelGetColor(const int dim,
!                                 const int tag,
!                                 int *r,
!                                 int *g,
!                                 int *b,
!                                 int *a,
!                                 int *ierr);

INTERFACE
  SUBROUTINE gmshModelGetColor(dim, tag, r, g, b, a, ierr) &
    BIND(C, NAME="gmshModelGetColor")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim, tag
    INTEGER(C_INT), INTENT(OUT) :: r, g, b, a
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGetColor
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Set the `x', `y', `z' coordinates of a geometrical point.
!
! GMSH_API void gmshModelSetCoordinates(const int tag,
!                                       const double x,
!                                       const double y,
!                                       const double z,
!                                       int *ierr);

INTERFACE
  SUBROUTINE gmshModelSetCoordinates(tag, x, y, z, ierr) &
    BIND(C, NAME="gmshModelSetCoordinates")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    REAL(C_DOUBLE), VALUE, INTENT(IN) :: x, y, z
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelSetCoordinates
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE gmshModelGetAttributeNames(names, names_n, ierr) &
    BIND(C, name="gmshModelGetAttributeNames")
    IMPORT
    TYPE(C_PTR), INTENT(out) :: names
    INTEGER(C_SIZE_T), INTENT(out) :: names_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGetAttributeNames
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE gmshModelSetAttribute( &
    name, values, values_n, ierr) BIND(C, name="gmshModelSetAttribute")
    IMPORT
    CHARACTER(len=1, kind=C_CHAR), DIMENSION(*), INTENT(IN) :: name
    TYPE(C_PTR), DIMENSION(*) :: values
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: values_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelSetAttribute
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  SUBROUTINE gmshModelGetAttribute(name, values, values_n, ierr) &
    BIND(C, name="gmshModelGetAttribute")
    IMPORT
    CHARACTER(len=1, kind=C_CHAR), DIMENSION(*), INTENT(in) :: name
    TYPE(C_PTR), INTENT(IN) :: values
    INTEGER(C_SIZE_T), INTENT(OUT) :: values_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelGetAttribute
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Remove the attribute with name `name'.

INTERFACE
  SUBROUTINE gmshModelRemoveAttribute(name, ierr) &
    BIND(C, name="gmshModelRemoveAttribute")
    IMPORT
    CHARACTER(len=1, kind=C_CHAR), DIMENSION(*), INTENT(in) :: name
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelRemoveAttribute
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE GmshModelInterface
