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

! /* Add a new model, with name `name', and set it as the current model. */
!
! GMSH_API void gmshModelAdd(const char *name,
!                            int *ierr);

INTERFACE
  SUBROUTINE gmshModelAdd(name, ierr) BIND(C, NAME="gmshModelAdd")
    IMPORT
    _CPTR_V_IN_ :: name
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelAdd
END INTERFACE

PUBLIC :: gmshModelAdd

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Remove the current model. */
!
! GMSH_API void gmshModelRemove(int *ierr);

INTERFACE
  SUBROUTINE gmshModelRemove(ierr) BIND(C, NAME="gmshModelRemove")
    IMPORT
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelRemove
END INTERFACE

PUBLIC :: gmshModelRemove

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
    _CPTR_IN_ :: names
    _ST_OUT_ :: names_n
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelList
END INTERFACE

PUBLIC :: gmshModelList

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
    _CPTR_IN_ :: name
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelGetCurrent
END INTERFACE

PUBLIC :: gmshModelGetCurrent

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
    _CPTR_V_IN_ :: name
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelSetCurrent
END INTERFACE

PUBLIC :: gmshModelSetCurrent

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Get the file name (if any) associated with the current model. A file name
!  * is associated when a model is read from a file on disk. */
!
! GMSH_API void gmshModelGetFileName(char **fileName,
!                                    int *ierr);

INTERFACE
  SUBROUTINE gmshModelGetFileName(fileName, ierr) &
    & BIND(C, NAME="gmshModelGetFileName")
    IMPORT
    _CPTR_IN_ :: fileName
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelGetFileName
END INTERFACE

PUBLIC :: gmshModelGetFileName

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
    _CPTR_V_IN_ :: fileName
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelSetFileName
END INTERFACE

PUBLIC :: gmshModelSetFileName

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
    _CPTR_IN_ :: dimTags
    _ST_OUT_ :: dimTags_n
    _I_V_IN_ :: dim
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelGetEntities
END INTERFACE

PUBLIC :: gmshModelGetEntities

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
    _I_V_IN_ :: dim
    _I_V_IN_ :: tag
    _CPTR_V_IN_ :: name
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelSetEntityName
END INTERFACE

PUBLIC :: gmshModelSetEntityName

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
    _I_V_IN_ :: dim
    _I_V_IN_ :: tag
    _CPTR_IN_ :: name
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelGetEntityName
END INTERFACE

PUBLIC :: gmshModelGetEntityName

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
    _CPTR_IN_ :: dimTags
    _ST_OUT_ :: dimTags_n
    _I_V_IN_ :: dim
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelGetPhysicalGroups
END INTERFACE

PUBLIC :: gmshModelGetPhysicalGroups

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the tags of the model entities making up the physical group of
! dimension `dim' and tag `tag'. */

INTERFACE
SUBROUTINE gmshModelGetEntitiesForPhysicalGroup(dim, tag, tags, tags_n, ierr)&
                        & BIND(C, NAME="gmshModelGetEntitiesForPhysicalGroup")
    IMPORT
    _I_V_IN_ :: dim
    _I_V_IN_ :: tag
    _CPTR_IN_ :: tags
    _ST_OUT_ :: tags_n
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelGetEntitiesForPhysicalGroup
END INTERFACE

PUBLIC :: gmshModelGetEntitiesForPhysicalGroup

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the tags of the physical groups (if any) to which the model entity of
! dimension `dim' and tag `tag' belongs.

INTERFACE
  SUBROUTINE gmshModelGetPhysicalGroupsForEntity(dim, tag, physicalTags, &
    & physicalTags_n, ierr) &
    & BIND(C, NAME="gmshModelGetPhysicalGroupsForEntity")
    IMPORT
    _I_V_IN_ :: dim
    _I_V_IN_ :: tag
    _CPTR_IN_ :: physicalTags
    _ST_OUT_ :: physicalTags_n
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelGetPhysicalGroupsForEntity
END INTERFACE

PUBLIC :: gmshModelGetPhysicalGroupsForEntity

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Add a physical group of dimension `dim', grouping the model entities with
!  * tags `tags'. Return the tag of the physical group, equal to `tag'
! if `tag'
!  * is positive, or a new tag if `tag' < 0. */

INTERFACE
  FUNCTION gmshModelAddPhysicalGroup(dim, tags, tags_n, &
    & tag, name, ierr) RESULT(ans) &
    & BIND(C, NAME="gmshModelAddPhysicalGroup")
    IMPORT
    _I_V_IN_ :: dim
    _ST_V_IN_ :: tags_n
    _I_IN_ :: tags(tags_n)
    _I_V_IN_ :: tag
    _CPTR_IN_ :: name
    _I_OUT_ :: ierr
    INTEGER(C_INT) :: ans
  END FUNCTION gmshModelAddPhysicalGroup
END INTERFACE

PUBLIC :: gmshModelAddPhysicalGroup

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Remove the physical groups `dimTags' from the current model. If `dimTags'
!  * is empty, remove all groups. */
!

INTERFACE
  SUBROUTINE gmshModelRemovePhysicalGroups(dimTags, dimTags_n, ierr) &
    & BIND(C, NAME="gmshModelRemovePhysicalGroups")
    IMPORT
    _ST_V_IN_ :: dimtags_n
    _I_IN_ :: dimTags(dimTags_n)
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelRemovePhysicalGroups
END INTERFACE

PUBLIC :: gmshModelRemovePhysicalGroups

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Set the name of the physical group of dimension `dim' and tag `tag'. */
!
! GMSH_API void gmshModelSetPhysicalName(const int dim,
!                                        const int tag,
!                                        const char *name,
!                                        int *ierr);

INTERFACE
  SUBROUTINE gmshModelSetPhysicalName(dim, tag, name, ierr) &
    & BIND(C, NAME="gmshModelSetPhysicalName")
    IMPORT
    _I_V_IN_ :: dim, tag
    _CPTR_V_IN_ :: name
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelSetPhysicalName
END INTERFACE

PUBLIC :: gmshModelSetPhysicalName

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Remove the physical name `name' from the current model. */
! GMSH_API void gmshModelRemovePhysicalName(const char *name,
!                                           int *ierr);

INTERFACE
  SUBROUTINE gmshModelRemovePhysicalName(name, ierr) &
    & BIND(C, NAME="gmshModelRemovePhysicalName")
    IMPORT
    _CPTR_V_IN_ :: name
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelRemovePhysicalName
END INTERFACE

PUBLIC :: gmshModelRemovePhysicalName

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
    & BIND(C, NAME="gmshModelGetPhysicalName")
    IMPORT
    _I_V_IN_ :: dim, tag
    _CPTR_IN_ :: name
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelGetPhysicalName
END INTERFACE

PUBLIC :: gmshModelGetPhysicalName

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Set the tag of the entity of dimension `dim' and tag `tag' to the new value
!! `newTag'.

interface
  subroutine gmshModelSetTag(dim, tag, newTag, ierr) &
    & bind(C, name="gmshModelSetTag")
    IMPORT
    integer(c_int), value, intent(in) :: dim
    integer(c_int), value, intent(in) :: tag
    integer(c_int), value, intent(in) :: newTag
    integer(c_int), intent(out) :: ierr
  end subroutine gmshModelSetTag
end interface

PUBLIC :: gmshModelSetTag

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
  SUBROUTINE gmshModelGetBoundary(dimTags, dimTags_n, outDimTags, &
    & outDimTags_n, combined, oriented, recursive, ierr) &
    & BIND(C, NAME="gmshModelGetBoundary")
    IMPORT
    _I_V_IN_ :: dimTags_n
    _I_IN_ :: dimTags(dimTags_n)
    _CPTR_IN_ :: outDimTags
    _ST_OUT_ :: outDimTags_n
    _I_V_IN_ :: combined, oriented, recursive
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelGetBoundary
END INTERFACE

PUBLIC :: gmshModelGetBoundary

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the upward and downward adjacencies of the model entity of dimension
! `dim' and tag `tag'. The `upward' vector returns the adjacent entities of
! dimension `dim' + 1; the `downward' vector returns the adjacent entities of
! dimension `dim' - 1. */

INTERFACE
  SUBROUTINE gmshModelGetAdjacencies(dim, tag, upward, upward_n, downward, &
    & downward_n, ierr) &
    & BIND(C, NAME="gmshModelGetAdjacencies")
    IMPORT
    _I_V_IN_ :: dim
    _I_V_IN_ :: tag
    _CPTR_IN_ :: upward, downward
    _ST_OUT_ :: upward_n, downward_n
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelGetAdjacencies
END INTERFACE

PUBLIC :: gmshModelGetAdjacencies

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the model entities in the bounding box defined by the two points
! (`xmin', `ymin', `zmin') and (`xmax', `ymax', `zmax'). If `dim' is >= 0,
! return only the entities of the specified dimension (e.g. points if `dim'
! == 0).

INTERFACE
  SUBROUTINE gmshModelGetEntitiesInBoundingBox(xmin, ymin, zmin, xmax, ymax, &
    & zmax, tags, tags_n, dim, ierr) &
    & BIND(C, NAME="gmshModelGetEntitiesInBoundingBox")
    IMPORT
    _R_V_IN_ :: xmin, ymin, zmin, xmax, ymax, zmax
    _CPTR_IN_ :: tags
    _ST_OUT_ :: tags_n
    _I_V_IN_ :: dim
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelGetEntitiesInBoundingBox
END INTERFACE

PUBLIC :: gmshModelGetEntitiesInBoundingBox

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the bounding box (`xmin', `ymin', `zmin'), (`xmax', `ymax', `zmax') of
! the model entity of dimension `dim' and tag `tag'. If `dim' and `tag' are
! negative, get the bounding box of the whole model. */

INTERFACE
  SUBROUTINE gmshModelGetBoundingBox(dim, tag, xmin, ymin, zmin, xmax, ymax, &
    & zmax, ierr) &
    & BIND(C, NAME="gmshModelGetBoundingBox")
    IMPORT
    _I_V_IN_ :: dim, tag
    _R_OUT_ :: xmin, ymin, zmin, xmax, ymax, zmax
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelGetBoundingBox
END INTERFACE

PUBLIC :: gmshModelGetBoundingBox

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Get the geometrical dimension of the current model. */
! GMSH_API int gmshModelGetDimension(int *ierr);

INTERFACE
  FUNCTION gmshModelGetDimension(ierr) RESULT(ans) &
    & BIND(C, NAME="gmshModelGetDimension")
    IMPORT
    _I_OUT_ :: ierr
    INTEGER(C_INT) :: ans
  END FUNCTION gmshModelGetDimension
END INTERFACE

PUBLIC :: gmshModelGetDimension

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Add a discrete model entity (defined by a mesh) of dimension `dim' in the
! current model. Return the tag of the new discrete entity, equal to `tag' if
! `tag' is positive, or a new tag if `tag' < 0. `boundary' specifies the tags
! of the entities on the boundary of the discrete entity, if any. Specifying
! `boundary' allows Gmsh to construct the topology of the overall model. */

INTERFACE
  FUNCTION gmshModelAddDiscreteEntity(dim, tag, boundary, boundary_n, ierr) &
    & RESULT(ans) &
    & BIND(C, NAME="gmshModelAddDiscreteEntity")
    IMPORT
    _I_V_IN_ :: dim, tag
    _ST_V_IN_ :: boundary_n
    _I_IN_ :: boundary(boundary_n)
    _I_OUT_ :: ierr
    INTEGER(C_INT) :: ans
  END FUNCTION gmshModelAddDiscreteEntity
END INTERFACE

PUBLIC :: gmshModelAddDiscreteEntity

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Remove the entities `dimTags' of the current model. If `recursive' is true,
! remove all the entities on their boundaries, down to dimension 0. */

INTERFACE
  SUBROUTINE gmshModelRemoveEntities(dimTags, dimTags_n, recursive, ierr) &
    & BIND(C, NAME="gmshModelRemoveEntities")
    IMPORT
    _ST_V_IN_ :: dimTags_n
    _I_IN_ :: dimTags(dimTags_n)
    _I_V_IN_ :: recursive
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelRemoveEntities
END INTERFACE

PUBLIC :: gmshModelRemoveEntities

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Remove the entity name `name' from the current model. */

INTERFACE
  SUBROUTINE gmshModelRemoveEntityName(name, ierr) &
    & BIND(C, NAME="gmshModelRemoveEntityName")
    IMPORT
    ! _CPTR_V_IN_ :: name
    character(len=1, kind=c_char), dimension(*), intent(in) :: name
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelRemoveEntityName
END INTERFACE

PUBLIC :: gmshModelRemoveEntityName

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Get the type of the entity of dimension `dim' and tag `tag'. */

INTERFACE
  SUBROUTINE gmshModelGetType(dim, tag, entityType, ierr) &
    & BIND(C, NAME="gmshModelGetType")
    IMPORT
    _I_V_IN_ :: dim, tag
    ! _CPTR_IN_ :: entityType
    character(kind=c_char), dimension(*) :: entityType
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelGetType
END INTERFACE

PUBLIC :: gmshModelGetType

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! In a partitioned model, get the parent of the entity of dimension `dim' and
! tag `tag', i.e. from which the entity is a part of, if any. `parentDim' and
! `parentTag' are set to -1 if the entity has no parent. */

INTERFACE
  SUBROUTINE gmshModelGetParent(dim, tag, parentDim, parentTag, ierr) &
    & BIND(C, NAME="gmshModelGetParent")
    IMPORT
    _I_V_IN_ :: dim, tag
    _I_OUT_ :: parentDim, parentTag
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelGetParent
END INTERFACE

PUBLIC :: gmshModelGetParent

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Return the number of partitions in the model.

interface
  function gmshModelGetNumberOfPartitions(ierr) result(ans) &
    & bind(C, name="gmshModelGetNumberOfPartitions")
    import
    _I_OUT_ :: ierr
    integer(c_int) :: ans
  end function gmshModelGetNumberOfPartitions
end interface

PUBLIC :: gmshModelGetNumberOfPartitions

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* In a partitioned model, return the tags of the partition(s) to which the
!  * entity belongs. */

INTERFACE
  SUBROUTINE gmshModelGetPartitions(dim, tag, partitions, partitions_n, &
    & ierr) BIND(C, NAME="gmshModelGetPartitions")
    IMPORT
    _I_V_IN_ :: dim, tag
    _CPTR_IN_ :: partitions
    _ST_OUT_ :: partitions_n
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelGetPartitions
END INTERFACE

PUBLIC :: gmshModelGetPartitions

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Evaluate the parametrization of the entity of dimension `dim' and tag `tag'
! at the parametric coordinates `parametricCoord'. Only valid for `dim' equal
! to 0 (with empty `parametricCoord'), 1 (with `parametricCoord' containing
! parametric coordinates on the curve) or 2 (with `parametricCoord' containing
! pairs of u, v parametric coordinates on the surface, concatenated:
! [p1u, p1v, p2u, ...]). Return triplets of x, y, z coordinates in `coord',
! concatenated: [p1x, p1y, p1z, p2x, ...]. */

INTERFACE
  SUBROUTINE gmshModelGetValue(dim, tag, parametricCoord, parametricCoord_n, &
    & coord, coord_n, ierr) &
    & BIND(C, NAME="gmshModelGetValue")
    IMPORT
    _I_V_IN_ :: dim, tag
    _ST_V_IN_ :: parametricCoord_n
    _R_IN_ :: parametricCoord(parametricCoord_n)
    _CPTR_IN_ :: coord
    _ST_OUT_ :: coord_n
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelGetValue
END INTERFACE

PUBLIC :: gmshModelGetValue

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
  SUBROUTINE gmshModelGetDerivative(dim, tag, parametricCoord, &
    & parametricCoord_n, derivatives, derivatives_n, ierr) &
    & BIND(C, NAME="gmshModelGetDerivative")
    IMPORT
    _I_V_IN_ :: dim, tag
    _ST_V_IN_ :: parametricCoord_n
    _R_IN_ :: parametricCoord(parametricCoord_n)
    _CPTR_IN_ :: derivatives
    _ST_OUT_ :: derivatives_n
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelGetDerivative
END INTERFACE

PUBLIC :: gmshModelGetDerivative

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
  SUBROUTINE gmshModelGetSecondDerivative(dim, tag, parametricCoord, &
    & parametricCoord_n, derivatives, derivatives_n, ierr) &
    & BIND(C, NAME="gmshModelGetSecondDerivative")
    IMPORT
    _I_V_IN_ :: dim, tag
    _ST_V_IN_ :: parametricCoord_n
    _R_IN_ :: parametricCoord(parametricCoord_n)
    _CPTR_IN_ :: derivatives
    _ST_OUT_ :: derivatives_n
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelGetSecondDerivative
END INTERFACE

PUBLIC :: gmshModelGetSecondDerivative

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Evaluate the (maximum) curvature of the entity of dimension `dim' and tag
! `tag' at the parametric coordinates `parametricCoord'. Only valid for `dim'
! equal to 1 (with `parametricCoord' containing parametric coordinates on the
! curve) or 2 (with `parametricCoord' containing pairs of u, v parametric
! coordinates on the surface, concatenated: [p1u, p1v, p2u, ...]).

INTERFACE
  SUBROUTINE gmshModelGetCurvature(dim, tag, parametricCoord, &
    & parametricCoord_n, curvatures, curvatures_n, ierr) &
    & BIND(C, NAME="gmshModelGetCurvature")
    IMPORT
    _I_V_IN_ :: dim, tag
    _ST_V_IN_ :: parametricCoord_n
    _R_IN_ :: parametricCoord(parametricCoord_n)
    _CPTR_IN_ :: curvatures
    _ST_OUT_ :: curvatures_n
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelGetCurvature
END INTERFACE

PUBLIC :: gmshModelGetCurvature

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Evaluate the principal curvatures of the surface with tag `tag' at the
! parametric coordinates `parametricCoord', as well as their respective
! directions. `parametricCoord' are given by pair of u and v coordinates,
! concatenated: [p1u, p1v, p2u, ...].

INTERFACE
  SUBROUTINE gmshModelGetPrincipalCurvatures(tag, parametricCoord, &
    & parametricCoord_n, curvatureMax, curvatureMax_n, curvatureMin, &
    & curvatureMin_n, directionMax, directionMax_n, directionMin, &
    & directionMin_n, ierr) &
    & BIND(C, NAME="gmshModelGetPrincipalCurvatures")
    IMPORT
    _I_V_IN_ :: tag
    _ST_V_IN_ :: parametricCoord_n
    _R_IN_ :: parametricCoord(parametricCoord_n)
    _CPTR_IN_ :: curvatureMax, curvatureMin, directionMax, directionMin
    _ST_OUT_ :: curvatureMax_n, curvatureMin_n, directionMax_n, directionMin_n
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelGetPrincipalCurvatures
END INTERFACE

PUBLIC :: gmshModelGetPrincipalCurvatures

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the normal to the surface with tag `tag' at the parametric coordinates
! `parametricCoord'. `parametricCoord' are given by pairs of u and v
! coordinates, concatenated: [p1u, p1v, p2u, ...]. `normals' are returned as
!  triplets of x, y, z components, concatenated: [n1x, n1y, n1z, n2x, ...].

INTERFACE
  SUBROUTINE gmshModelGetNormal(tag, parametricCoord, &
    & parametricCoord_n, normals, normals_n, ierr) &
    & BIND(C, NAME="gmshModelGetNormal")
    IMPORT
    _I_V_IN_ :: tag
    _ST_V_IN_ :: parametricCoord_n
    _R_IN_ :: parametricCoord(parametricCoord_n)
    _CPTR_IN_ :: normals
    _ST_OUT_ :: normals_n
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelGetNormal
END INTERFACE

PUBLIC :: gmshModelGetNormal

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
  SUBROUTINE gmshModelGetParametrization(dim, tag, coord, coord_n, &
    & parametricCoord, parametricCoord_n, ierr) &
    & BIND(C, NAME="gmshModelGetParametrization")
    IMPORT
    _I_V_IN_ :: dim, tag
    _ST_V_IN_ :: coord_n
    _R_IN_ :: coord(coord_n)
    _CPTR_IN_ :: parametricCoord
    _ST_OUT_ :: parametricCoord_n
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelGetParametrization
END INTERFACE

PUBLIC :: gmshModelGetParametrization

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the `min' and `max' bounds of the parametric coordinates for the entity
! of dimension `dim' and tag `tag'.

INTERFACE
  SUBROUTINE gmshModelGetParametrizationBounds(dim, tag, min, min_n, &
    & max, max_n, ierr) &
    & BIND(C, NAME="gmshModelGetParametrizationBounds")
    IMPORT
    _I_V_IN_ :: dim, tag
    _CPTR_IN_ :: min, max
    _ST_OUT_ :: min_n, max_n
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelGetParametrizationBounds
END INTERFACE

PUBLIC :: gmshModelGetParametrizationBounds

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Check if the parametric coordinates provided in `parametricCoord'
! correspond to points inside the entitiy of dimension `dim' and tag `tag',
! and return the number of points inside. This feature is only available for
! a subset of curves and surfaces, depending on the underyling geometrical
! representation.

INTERFACE
  FUNCTION gmshModelIsInside(dim, tag, coord, coord_n, parametric, &
    & ierr) RESULT(ans) &
    & BIND(C, NAME="gmshModelIsInside")
    IMPORT
    _I_V_IN_ :: dim, tag
    _ST_V_IN_ :: coord_n
    _R_IN_ :: coord(coord_n)
    _I_V_IN_ :: parametric
    _I_OUT_ :: ierr
    INTEGER(C_INT) :: ans
  END FUNCTION gmshModelIsInside
END INTERFACE

PUBLIC :: gmshModelIsInside

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
  SUBROUTINE gmshModelGetClosestPoint(dim, tag, coord, coord_n, &
      &  closestCoord, closestCoord_n, parametricCoord, &
      & parametricCoord_n, ierr) BIND(C, NAME="gmshModelGetClosestPoint")
    IMPORT
    _I_V_IN_ :: dim, tag
    _ST_V_IN_ :: coord_n
    _R_IN_ :: coord(coord_n)
    _CPTR_IN_ :: closestCoord, parametricCoord
    _ST_OUT_ :: closestCoord_n, parametricCoord_n
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelGetClosestPoint
END INTERFACE

PUBLIC :: gmshModelGetClosestPoint

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
  SUBROUTINE gmshModelReparametrizeOnSurface(dim, tag, parametricCoord, &
    & parametricCoord_n, surfaceTag, surfaceParametricCoord, &
    & surfaceParametricCoord_n, which, ierr) &
    & BIND(C, NAME="gmshModelReparametrizeOnSurface")
    IMPORT
    _I_V_IN_ :: dim, tag, surfaceTag, which
    _ST_V_IN_ :: parametricCoord_n
    _R_IN_ :: parametricCoord(parametricCoord_n)
    _CPTR_IN_ :: surfaceParametricCoord
    _ST_OUT_ :: surfaceParametricCoord_n
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelReparametrizeOnSurface
END INTERFACE

PUBLIC :: gmshModelReparametrizeOnSurface

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
  SUBROUTINE gmshModelSetVisibility(dimTags, dimTags_n, value, &
    & recursive, ierr) BIND(C, NAME="gmshModelSetVisibility")
    IMPORT
    _ST_V_IN_ :: dimTags_n
    _I_IN_ :: dimTags(dimTags_n)
    _I_V_IN_ :: value, recursive
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelSetVisibility
END INTERFACE

PUBLIC :: gmshModelSetVisibility

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the visibility of the model entity of dimension `dim' and tag `tag'.
! GMSH_API void gmshModelGetVisibility(const int dim,
!                                      const int tag,
!                                      int *value,
!                                      int *ierr);

INTERFACE
  SUBROUTINE gmshModelGetVisibility(dim, tag, value, ierr)&
    & BIND(C, NAME="gmshModelGetVisibility")
    IMPORT
    _I_V_IN_ :: dim, tag
    _I_OUT_ :: value
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelGetVisibility
END INTERFACE

PUBLIC :: gmshModelGetVisibility

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
  SUBROUTINE gmshModelSetVisibilityPerWindow(value, windowIndex, ierr) &
    & BIND(C, NAME="gmshModelSetVisibilityPerWindow")
    IMPORT
    _I_V_IN_ :: value, windowIndex
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelSetVisibilityPerWindow
END INTERFACE

PUBLIC :: gmshModelSetVisibilityPerWindow

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
  SUBROUTINE gmshModelSetColor(dimTags, dimTags_n, r, g, b, a, &
      & recursive, ierr) BIND(C, NAME="gmshModelSetColor")
    IMPORT
    _ST_V_IN_ :: dimTags_n
    _I_IN_ :: dimTags(dimTags_n)
    _I_V_IN_ :: r, g, b, a, recursive
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelSetColor
END INTERFACE

PUBLIC :: gmshModelSetColor

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
    & BIND(C, NAME="gmshModelGetColor")
    IMPORT
    _I_V_IN_ :: dim, tag
    _I_OUT_ :: r, g, b, a
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelGetColor
END INTERFACE

PUBLIC :: gmshModelGetColor

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
    & BIND(C, NAME="gmshModelSetCoordinates")
    IMPORT
    _I_V_IN_ :: tag
    _R_V_IN_ :: x, y, z
    _I_OUT_ :: ierr
  END SUBROUTINE gmshModelSetCoordinates
END INTERFACE

PUBLIC :: gmshModelSetCoordinates

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

interface
  subroutine gmshModelGetAttributeNames(names, names_n, ierr) &
    & bind(C, name="gmshModelGetAttributeNames")
    import
    type(c_ptr), intent(out) :: names
    integer(c_size_t), intent(out) :: names_n
    _I_OUT_ :: ierr
  end subroutine gmshModelGetAttributeNames
end interface

PUBLIC :: gmshModelGetAttributeNames

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

interface
  subroutine gmshModelSetAttribute(name, values, values_n, &
    & ierr) bind(C, name="gmshModelSetAttribute")
    import
    character(len=1, kind=c_char), dimension(*), intent(in) :: name
    type(c_ptr), dimension(*) :: values
    integer(c_size_t), value, intent(in) :: values_n
    _I_OUT_ :: ierr
  end subroutine gmshModelSetAttribute
end interface

PUBLIC :: gmshModelSetAttribute

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

interface
  subroutine gmshModelGetAttribute(name, values, values_n, ierr) &
    & bind(C, name="gmshModelGetAttribute")
    import
    character(len=1, kind=c_char), dimension(*), intent(in) :: name
    _CPTR_IN_ :: values
    _ST_OUT_ :: values_n
    _I_OUT_ :: ierr
  end subroutine gmshModelGetAttribute
end interface

PUBLIC :: gmshModelGetAttribute

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Remove the attribute with name `name'.

interface
  subroutine gmshModelRemoveAttribute(name, ierr) &
      & bind(C, name="gmshModelRemoveAttribute")
    import
    character(len=1, kind=c_char), dimension(*), intent(in) :: name
    _I_OUT_ :: ierr
  end subroutine gmshModelRemoveAttribute
end interface

PUBLIC :: gmshModelRemoveAttribute
