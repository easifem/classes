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

MODULE GmshModelMeshInterface
USE ISO_C_BINDING, ONLY: C_INT
USE ISO_C_BINDING, ONLY: C_DOUBLE
USE ISO_C_BINDING, ONLY: C_SIZE_T
USE ISO_C_BINDING, ONLY: C_PTR
USE ISO_C_BINDING, ONLY: C_CHAR
USE ISO_C_BINDING, ONLY: C_FUNPTR
IMPLICIT NONE
PRIVATE

PUBLIC :: gmshModelMeshGenerate
PUBLIC :: gmshModelMeshPartition
PUBLIC :: gmshModelMeshUnpartition
PUBLIC :: gmshModelMeshOptimize
PUBLIC :: gmshModelMeshRecombine
PUBLIC :: gmshModelMeshRefine
PUBLIC :: gmshModelMeshSetOrder
PUBLIC :: gmshModelMeshGetLastEntityError
PUBLIC :: gmshModelMeshGetLastNodeError
PUBLIC :: gmshModelMeshClear
PUBLIC :: gmshModelMeshGetNodes
PUBLIC :: gmshModelMeshGetNodesByElementType
PUBLIC :: gmshModelMeshGetNode
PUBLIC :: gmshModelMeshSetNode
PUBLIC :: gmshModelMeshRebuildNodeCache
PUBLIC :: gmshModelMeshRebuildElementCache
PUBLIC :: gmshModelMeshGetNodesForPhysicalGroup
PUBLIC :: gmshModelMeshReclassifyNodes
PUBLIC :: gmshModelMeshRelocateNodes
PUBLIC :: gmshModelMeshAddNodes
PUBLIC :: gmshModelMeshGetElements
PUBLIC :: gmshModelMeshGetElement

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Generate a mesh of the current model, up to dimension `dim' (0, 1, 2 or 3).
!
! GMSH_API void gmshModelMeshGenerate(const int dim,
!                                     int * ierr);

INTERFACE
  SUBROUTINE gmshModelMeshGenerate(dim, ierr) &
    BIND(C, NAME="gmshModelMeshGenerate")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim
    INTEGER(C_INT), INTENT(out) :: ierr
  END SUBROUTINE gmshModelMeshGenerate
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Partition the mesh of the current model into `numPart' partitions.
!
! GMSH_API void gmshModelMeshPartition(const int numPart,
!                                      int * ierr);

INTERFACE
  SUBROUTINE gmshModelMeshPartition(numPart, ierr) &
    BIND(C, NAME="gmshModelMeshPartition")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: numPart
    INTEGER(C_INT), INTENT(out) :: ierr
  END SUBROUTINE gmshModelMeshPartition
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Unpartition the mesh of the current model.
!
! GMSH_API void gmshModelMeshUnpartition(int * ierr)

INTERFACE
  SUBROUTINE gmshModelMeshUnpartition(ierr) &
    BIND(C, NAME="gmshModelMeshUnpartition")
    IMPORT
    INTEGER(C_INT), INTENT(out) :: ierr
  END SUBROUTINE gmshModelMeshUnpartition
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Optimize the mesh of the current model using `method' (empty for default
! tetrahedral mesh optimizer, "Netgen" for Netgen optimizer, "HighOrder" for
! direct high-order mesh optimizer, "HighOrderElastic" for high-order elastic
! smoother, "HighOrderFastCurving" for fast curving algorithm, "Laplace2D"
! for Laplace smoothing, "Relocate2D" and "Relocate3D" for node relocation).
! If `force' is set apply the optimization also to discrete entities. If
! `dimTags' is given, only apply the optimizer to the given entities. */
!
! GMSH_API void gmshModelMeshOptimize(const char * method,
!                                     const int force,
!                                     const int niter,
!                                     int * dimTags, size_t dimTags_n,
!                                     int * ierr);

INTERFACE
  SUBROUTINE gmshModelMeshOptimize( &
    method, force, niter, dimTags, dimTags_n, ierr) &
    BIND(C, NAME="gmshModelMeshOptimize")
    IMPORT
    TYPE(C_PTR), VALUE, INTENT(in) :: method
    INTEGER(C_INT), VALUE, INTENT(IN) :: force, niter
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: dimTags_n
    INTEGER(C_INT), INTENT(in) :: dimTags(dimTags_n)
    INTEGER(C_INT), INTENT(out) :: ierr
  END SUBROUTINE gmshModelMeshOptimize
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Recombine the mesh of the current model.
!
! GMSH_API void gmshModelMeshRecombine(int * ierr);

INTERFACE
  SUBROUTINE gmshModelMeshRecombine(ierr) &
    BIND(C, NAME="gmshModelMeshRecombine")
    IMPORT
    INTEGER(C_INT), INTENT(out) :: ierr
  END SUBROUTINE gmshModelMeshRecombine
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Refine the mesh of the current model by uniformly splitting the elements.
!
! GMSH_API void gmshModelMeshRefine(int * ierr);

INTERFACE
  SUBROUTINE gmshModelMeshRefine(ierr) &
    BIND(C, NAME="gmshModelMeshRefine")
    IMPORT
    INTEGER(C_INT), INTENT(out) :: ierr
  END SUBROUTINE gmshModelMeshRefine
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Set the order of the elements in the mesh of the current model to `order'.
!
! GMSH_API void gmshModelMeshSetOrder(const int order,
!                                     int * ierr);

INTERFACE
  SUBROUTINE gmshModelMeshSetOrder(order, ierr) &
    BIND(C, NAME="gmshModelMeshSetOrder")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: order
    INTEGER(C_INT), INTENT(out) :: ierr
  END SUBROUTINE gmshModelMeshSetOrder
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the last entities (if any) where a meshing error occurred. Currently
! only populated by the new 3D meshing algorithms.
!
! GMSH_API void gmshModelMeshGetLastEntityError(int ** dimTags,
! size_t * dimTags_n, int * ierr);

INTERFACE
  SUBROUTINE gmshModelMeshGetLastEntityError(dimTags, dimTags_n, ierr) &
    BIND(C, NAME="gmshModelMeshGetLastEntityError")
    IMPORT
    TYPE(C_PTR), INTENT(in) :: dimTags
    INTEGER(C_SIZE_T), INTENT(in) :: dimTags_n
    INTEGER(C_INT), INTENT(out) :: ierr
  END SUBROUTINE gmshModelMeshGetLastEntityError
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the last nodes (if any) where a meshing error occurred. Currently only
! populated by the new 3D meshing algorithms. */
!
! GMSH_API void gmshModelMeshGetLastNodeError(
! size_t ** nodeTags, size_t * nodeTags_n, int * ierr);

INTERFACE
  SUBROUTINE gmshModelMeshGetLastNodeError(nodeTags, nodeTags_n, ierr) &
    BIND(C, NAME="gmshModelMeshGetLastNodeError")
    IMPORT
    TYPE(C_PTR), INTENT(in) :: nodeTags
    INTEGER(C_SIZE_T), INTENT(in) :: nodeTags_n
    INTEGER(C_INT), INTENT(out) :: ierr
  END SUBROUTINE gmshModelMeshGetLastNodeError
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Clear the mesh, i.e. delete all the nodes and elements, for the entities
! `dimTags'. if `dimTags' is empty, clear the whole mesh. Note that the mesh
! of an entity can only be cleared if this entity is not on the boundary of
! another entity with a non-empty mesh.
!
! GMSH_API void gmshModelMeshClear(int * dimTags, size_t dimTags_n,
!                                  int * ierr);

INTERFACE
  SUBROUTINE gmshModelMeshClear(dimTags, dimTags_n, ierr) &
    BIND(C, NAME="gmshModelMeshClear")
    IMPORT
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: dimTags_n
    INTEGER(C_INT), INTENT(in) :: dimTags(dimTags_n)
    INTEGER(C_INT), INTENT(out) :: ierr
  END SUBROUTINE gmshModelMeshClear
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the nodes classified on the entity of dimension `dim' and tag `tag'. If
! `tag' < 0, get the nodes for all entities of dimension `dim'. If `dim' and
! `tag' are negative, get all the nodes in the mesh. `nodeTags' contains the
! node tags (their unique, strictly positive identification numbers). `coord'
! is a vector of length 3 times the length of `nodeTags' that contains the x,
! y, z coordinates of the nodes, concatenated: [n1x, n1y, n1z, n2x, ...]. If
! `dim' >= 0 and `returnParamtricCoord' is set, `parametricCoord' contains
! the parametric coordinates ([u1, u2, ...] or [u1, v1, u2, ...]) of the
! nodes, if available. The length of `parametricCoord' can be 0 or `dim'
! times the length of `nodeTags'. If `includeBoundary' is set, also return
! the nodes classified on the boundary of the entity (which will be
! reparametrized on the entity if `dim' >= 0 in order to compute their
! parametric coordinates).
!
! GMSH_API void gmshModelMeshGetNodes(
! size_t ** nodeTags, size_t * nodeTags_n,
! double ** coord, size_t * coord_n,
! double ** parametricCoord, size_t * parametricCoord_n,
! const int dim,
! const int tag,
! const int includeBoundary,
! const int returnParametricCoord,
! int * ierr);

INTERFACE
  SUBROUTINE gmshModelMeshGetNodes( &
    nodeTags, nodeTags_n, coord, coord_n, parametricCoord, &
    parametricCoord_n, dim, tag, includeBoundary, returnParametricCoord, &
    ierr) BIND(C, NAME="gmshModelMeshGetNodes")
    IMPORT
    TYPE(C_PTR), INTENT(in) :: nodeTags, coord, parametricCoord
    INTEGER(C_SIZE_T), INTENT(in) :: nodeTags_n, coord_n, parametricCoord_n
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim, tag, includeBoundary, &
                                         returnParametricCoord
    INTEGER(C_INT), INTENT(out) :: ierr
  END SUBROUTINE gmshModelMeshGetNodes
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the nodes classified on the entity of tag `tag', for all the elements
! of type `elementType'. The other arguments are treated as in `getNodes'. */
!
! GMSH_API void gmshModelMeshGetNodesByElementType(
! const int elementType,
! size_t ** nodeTags, size_t * nodeTags_n,
! double ** coord, size_t * coord_n,
! double ** parametricCoord, size_t * parametricCoord_n,
! const int tag,
! const int returnParametricCoord,
! int * ierr);

INTERFACE
  SUBROUTINE gmshModelMeshGetNodesByElementType( &
    elementType, nodeTags, nodeTags_n, coord, coord_n, parametricCoord, &
    parametricCoord_n, tag, returnParametricCoord, ierr) &
    BIND(C, NAME="gmshModelMeshGetNodesByElementType")
    IMPORT
    TYPE(C_PTR), INTENT(in) :: nodeTags, coord, parametricCoord
    INTEGER(C_SIZE_T), INTENT(in) :: nodeTags_n, coord_n, parametricCoord_n
    INTEGER(C_INT), VALUE, INTENT(IN) :: elementType, tag, &
                                         returnParametricCoord
    INTEGER(C_INT), INTENT(out) :: ierr
  END SUBROUTINE gmshModelMeshGetNodesByElementType
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the coordinates and the parametric coordinates (if any) of the node
! with tag `tag'. This function relies on an internal cache (a vector in case
! of dense node numbering, a map otherwise); for large meshes accessing nodes
! in bulk is often preferable.
!
! GMSH_API void gmshModelMeshGetNode(
! const size_t nodeTag,
! double ** coord, size_t * coord_n,
! double ** parametricCoord, size_t * parametricCoord_n,
! int * ierr);

INTERFACE
  SUBROUTINE gmshModelMeshGetNode( &
    nodeTag, coord, coord_n, parametricCoord, parametricCoord_n, ierr) &
    BIND(C, NAME="gmshModelMeshGetNode")
    IMPORT
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: nodeTag
    TYPE(C_PTR), INTENT(in) :: coord, parametricCoord
    INTEGER(C_SIZE_T), INTENT(in) :: coord_n, parametricCoord_n
    INTEGER(C_INT), INTENT(out) :: ierr
  END SUBROUTINE gmshModelMeshGetNode
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Set the coordinates and the parametric coordinates (if any) of the node
! with tag `tag'. This function relies on an internal cache (a vector in case
! of dense node numbering, a map otherwise); for large meshes accessing nodes
! in bulk is often preferable.
!
! GMSH_API void gmshModelMeshSetNode(
! const size_t nodeTag,
! double * coord, size_t coord_n,
! double * parametricCoord, size_t parametricCoord_n,
! int * ierr);

INTERFACE
  SUBROUTINE gmshModelMeshSetNode( &
    nodeTag, coord, coord_n, parametricCoord, parametricCoord_n, ierr) &
    BIND(C, NAME="gmshModelMeshSetNode")
    IMPORT
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: nodeTag, coord_n, &
                                            parametricCoord_n
    TYPE(C_PTR), INTENT(in) :: coord(coord_n), &
                               parametricCoord(parametricCoord_n)
    INTEGER(C_INT), INTENT(out) :: ierr
  END SUBROUTINE gmshModelMeshSetNode
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Rebuild the node cache.
!
! GMSH_API void gmshModelMeshRebuildNodeCache(const int onlyIfNecessary,
!                                             int * ierr);

INTERFACE
  SUBROUTINE gmshModelMeshRebuildNodeCache(onlyIfNecessary, ierr) &
    BIND(C, NAME="gmshModelMeshRebuildNodeCache")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: onlyIfNecessary
    INTEGER(C_INT), INTENT(out) :: ierr
  END SUBROUTINE gmshModelMeshRebuildNodeCache
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Rebuild the element cache
!
! GMSH_API void gmshModelMeshRebuildElementCache(const int onlyIfNecessary,
!                                                int * ierr);

INTERFACE
  SUBROUTINE gmshModelMeshRebuildElementCache(onlyIfNecessary, ierr) &
    BIND(C, NAME="gmshModelMeshRebuildElementCache")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: onlyIfNecessary
    INTEGER(C_INT), INTENT(out) :: ierr
  END SUBROUTINE gmshModelMeshRebuildElementCache
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the nodes from all the elements belonging to the physical group of
! dimension `dim' and tag `tag'. `nodeTags' contains the node tags; `coord'
! is a vector of length 3 times the length of `nodeTags' that contains the x,
! y, z coordinates of the nodes, concatenated: [n1x, n1y, n1z, n2x, ...]. */
!
! GMSH_API void gmshModelMeshGetNodesForPhysicalGroup(const int dim,
! const int tag,
! size_t ** nodeTags, size_t * nodeTags_n,
! double ** coord, size_t * coord_n,
! int * ierr);

INTERFACE
  SUBROUTINE gmshModelMeshGetNodesForPhysicalGroup( &
    dim, tag, nodeTags, nodeTags_n, coord, coord_n, ierr) &
    BIND(C, NAME="gmshModelMeshGetNodesForPhysicalGroup")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim, tag
    TYPE(C_PTR), INTENT(in) :: nodeTags, coord
    INTEGER(C_SIZE_T), INTENT(in) :: nodeTags_n, coord_n
    INTEGER(C_INT), INTENT(out) :: ierr
  END SUBROUTINE gmshModelMeshGetNodesForPhysicalGroup
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Add nodes classified on the model entity of dimension `dim' and tag `tag'.
! `nodeTags' contains the node tags (their unique, strictly positive
! identification numbers). `coord' is a vector of length 3 times the length
! of `nodeTags' that contains the x, y, z coordinates of the nodes,
! concatenated: [n1x, n1y, n1z, n2x, ...]. The optional `parametricCoord'
! vector contains the parametric coordinates of the nodes, if any. The length
! of `parametricCoord' can be 0 or `dim' times the length of `nodeTags'. If
! the `nodeTags' vector is empty, new tags are automatically assigned to the
! nodes. */
!
! GMSH_API void gmshModelMeshAddNodes(const int dim,
! const int tag,
! size_t * nodeTags, size_t nodeTags_n,
! double * coord, size_t coord_n,
! double * parametricCoord, size_t parametricCoord_n,
! int * ierr);

INTERFACE
  SUBROUTINE gmshModelMeshAddNodes( &
    dim, tag, nodeTags, nodeTags_n, coord, coord_n, parametricCoord, &
    parametricCoord_n, ierr) &
    BIND(C, NAME="gmshModelMeshAddNodes")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim, tag
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: nodeTags_n, coord_n, &
                                            parametricCoord_n
    INTEGER(C_SIZE_T), INTENT(IN) :: nodeTags(nodeTags_n)
    REAL(C_DOUBLE), INTENT(IN) :: coord(coord_n), &
                                  parametricCoord(parametricCoord_n)
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshAddNodes
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Reclassify all nodes on their associated model entity, based on the
! elements. Can be used when importing nodes in bulk (e.g. by associating
! them all to a single volume), to reclassify them correctly on model
! surfaces, curves, etc. after the elements have been set.
!
! GMSH_API void gmshModelMeshReclassifyNodes(int * ierr);

INTERFACE
  SUBROUTINE gmshModelMeshReclassifyNodes(ierr) &
    BIND(C, NAME="gmshModelMeshReclassifyNodes")
    IMPORT
    INTEGER(C_INT), INTENT(out) :: ierr
  END SUBROUTINE gmshModelMeshReclassifyNodes
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Relocate the nodes classified on the entity of dimension `dim' and tag
! `tag' using their parametric coordinates. If `tag' < 0, relocate the nodes
! for all entities of dimension `dim'. If `dim' and `tag' are negative,
! relocate all the nodes in the mesh.
!
! GMSH_API void gmshModelMeshRelocateNodes(
! const int dim, const int tag, int * ierr);

INTERFACE
  SUBROUTINE gmshModelMeshRelocateNodes(dim, tag, ierr) &
    BIND(C, NAME="gmshModelMeshRelocateNodes")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim, tag
    INTEGER(C_INT), INTENT(out) :: ierr
  END SUBROUTINE gmshModelMeshRelocateNodes
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the elements classified on the entity of dimension `dim' and tag `tag'.
! If `tag' < 0, get the elements for all entities of dimension `dim'. If
! `dim' and `tag' are negative, get all the elements in the mesh.
! `elementTypes' contains the MSH types of the elements (e.g. `2' for 3-node
! triangles: see `getElementProperties' to obtain the properties for a given
! element type). `elementTags' is a vector of the same length as
! `elementTypes'; each entry is a vector containing the tags (unique,
! strictly positive identifiers) of the elements of the corresponding type.
! `nodeTags' is also a vector of the same length as `elementTypes'; each
! entry is a vector of length equal to the number of elements of the given
! type times the number N of nodes for this type of element, that contains
! the node tags of all the elements of the given type, concatenated: [e1n1,
! e1n2, ..., e1nN, e2n1, ...]. */
!
! GMSH_API void gmshModelMeshGetElements(
! int ** elementTypes, size_t * elementTypes_n,
! size_t *** elementTags, size_t ** elementTags_n, size_t *elementTags_nn,
! size_t *** nodeTags, size_t ** nodeTags_n, size_t *nodeTags_nn,
! const int dim,
! const int tag,
! int * ierr);

INTERFACE
  SUBROUTINE gmshModelMeshGetElements( &
    elementTypes, elementTypes_n, elementTags, elementTags_n, &
    elementTags_nn, nodeTags, nodeTags_n, nodeTags_nn, dim, tag, ierr) &
    BIND(C, NAME="gmshModelMeshGetElements")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim, tag
    TYPE(C_PTR), INTENT(IN) :: elementTypes, elementTags, &
                               elementTags_n, nodeTags, nodeTags_n
    INTEGER(C_SIZE_T), INTENT(IN) :: elementTypes_n, &
                                     elementTags_nn, nodeTags_nn
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshGetElements
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the type and node tags of the element with tag `tag'. This function
! relies on an internal cache (a vector in case of dense element numbering, a
! map otherwise); for large meshes accessing elements in bulk is often
! preferable.
!
! GMSH_API void gmshModelMeshGetElement(const size_t elementTag,
! int * elementType,
! size_t ** nodeTags, size_t * nodeTags_n,
! int * ierr);

INTERFACE
  SUBROUTINE gmshModelMeshGetElement( &
    elementTag, elementType, nodeTags, nodeTags_n, ierr) &
    BIND(C, NAME="gmshModelMeshGetElement")
    IMPORT
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: elementTag
    INTEGER(C_INT), INTENT(out) :: elementType
    TYPE(C_PTR), INTENT(in) :: nodeTags
    INTEGER(C_SIZE_T), INTENT(in) :: nodeTags_n
    INTEGER(C_INT), INTENT(out) :: ierr
  END SUBROUTINE gmshModelMeshGetElement
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!! Search the mesh for an element located at coordinates (`x', `y', `z').
!! This function performs a search in a spatial octree.
!! If an element is found,
!! return its tag, type and node tags, as well as the local coordinates (`u',
!! `v', `w') within the reference element corresponding to search location.
!! If `dim' is >= 0, only search for elements of the given dimension.
!! If `strict' is not set, use a tolerance to find elements near
!! the search location.

INTERFACE
  SUBROUTINE gmshModelMeshGetElementByCoordinates( &
    x, y, z, elementTag, elementType, api_nodeTags_, api_nodeTags_n_, &
    u, v, w, dim, strict, ierr) &
    BIND(C, name="gmshModelMeshGetElementByCoordinates")
    IMPORT
    REAL(C_DOUBLE), VALUE, INTENT(IN) :: x
    REAL(C_DOUBLE), VALUE, INTENT(IN) :: y
    REAL(C_DOUBLE), VALUE, INTENT(IN) :: z
    INTEGER(C_SIZE_T), INTENT(inout) :: elementTag
    INTEGER(C_INT), INTENT(INOUT) :: elementType
    TYPE(C_PTR), INTENT(OUT) :: api_nodeTags_
    INTEGER(C_SIZE_T), INTENT(OUT) :: api_nodeTags_n_
    REAL(C_DOUBLE), INTENT(INOUT) :: u
    REAL(C_DOUBLE), INTENT(INOUT) :: v
    REAL(C_DOUBLE), INTENT(INOUT) :: w
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim
    INTEGER(C_INT), VALUE, INTENT(IN) :: strict
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshGetElementByCoordinates
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Search the mesh for element(s) located at coordinates (`x', `y', `z'). This
! function performs a search in a spatial octree. Return the tags of all
! found elements in `elementTags'. Additional information about the elements
! can be accessed through `getElement' and `getLocalCoordinatesInElement'. If
! `dim' is >= 0, only search for elements of the given dimension. If `strict'
! is not set, use a tolerance to find elements near the search location.

INTERFACE
  SUBROUTINE gmshModelMeshGetElementsByCoordinates( &
    x, y, z, api_elementTags_, api_elementTags_n_, dim, strict, ierr) &
    BIND(C, name="gmshModelMeshGetElementsByCoordinates")
    IMPORT
    REAL(C_DOUBLE), VALUE, INTENT(IN) :: x
    REAL(C_DOUBLE), VALUE, INTENT(IN) :: y
    REAL(C_DOUBLE), VALUE, INTENT(IN) :: z
    TYPE(C_PTR), INTENT(OUT) :: api_elementTags_
    INTEGER(C_SIZE_T), INTENT(OUT) :: api_elementTags_n_
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim
    INTEGER(C_INT), VALUE, INTENT(IN) :: strict
    INTEGER(C_INT), INTENT(OUT), OPTIONAL :: ierr
  END SUBROUTINE gmshModelMeshGetElementsByCoordinates
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Return the local coordinates (`u', `v', `w') within the element
! `elementTag' corresponding to the model coordinates (`x', `y', `z'). This
! function relies on an internal cache (a vector in case of dense element
! numbering, a map otherwise); for large meshes accessing elements in bulk is
! often preferable.

INTERFACE
  SUBROUTINE gmshModelMeshGetLocalCoordinatesInElement( &
    elementTag, x, y, z, u, v, w, ierr) &
    BIND(C, name="gmshModelMeshGetLocalCoordinatesInElement")
    IMPORT
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: elementTag
    REAL(C_DOUBLE), VALUE, INTENT(IN) :: x
    REAL(C_DOUBLE), VALUE, INTENT(IN) :: y
    REAL(C_DOUBLE), VALUE, INTENT(IN) :: z
    REAL(C_DOUBLE), INTENT(INOUT) :: u
    REAL(C_DOUBLE), INTENT(INOUT) :: v
    REAL(C_DOUBLE), INTENT(INOUT) :: w
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshGetLocalCoordinatesInElement
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the types of elements in the entity of dimension `dim' and tag `tag'.
! If `tag' < 0, get the types for all entities of dimension `dim'. If `dim'
! and `tag' are negative, get all the types in the mesh.

INTERFACE
  SUBROUTINE gmshModelMeshGetElementTypes( &
    api_elementTypes_, api_elementTypes_n_, dim, tag, ierr) &
    BIND(C, name="gmshModelMeshGetElementTypes")
    IMPORT
    TYPE(C_PTR), INTENT(OUT) :: api_elementTypes_
    INTEGER(C_SIZE_T), INTENT(OUT) :: api_elementTypes_n_
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshGetElementTypes
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Return an element type given its family name `familyName' ("Point", "Line",
! "Triangle", "Quadrangle", "Tetrahedron", "Pyramid", "Prism", "Hexahedron")
! and polynomial order `order'. If `serendip' is true, return the
! corresponding serendip element type (element without interior nodes).

INTERFACE
  FUNCTION gmshModelMeshGetElementType( &
    familyName, order, serendip, ierr) &
    BIND(C, name="gmshModelMeshGetElementType")
    IMPORT
    INTEGER(C_INT) :: gmshModelMeshGetElementType
    CHARACTER(len=1, kind=C_CHAR), INTENT(IN) :: familyName(*)
    INTEGER(C_INT), VALUE, INTENT(IN) :: order
    INTEGER(C_INT), VALUE, INTENT(IN) :: serendip
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END FUNCTION gmshModelMeshGetElementType
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the properties of an element of type `elementType': its name
! (`elementName'), dimension (`dim'), order (`order'), number of nodes
! (`numNodes'), local coordinates of the nodes in the reference element
! (`localNodeCoord' vector, of length `dim' times `numNodes') and number of
! primary (first order) nodes (`numPrimaryNodes').

INTERFACE
  SUBROUTINE gmshModelMeshGetElementProperties( &
    elementType, api_elementName_, dim, order, numNodes, &
    api_localNodeCoord_, api_localNodeCoord_n_, numPrimaryNodes, ierr) &
    BIND(C, name="gmshModelMeshGetElementProperties")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: elementType
    TYPE(C_PTR), INTENT(OUT) :: api_elementName_
    INTEGER(C_INT), INTENT(INOUT) :: dim
    INTEGER(C_INT), INTENT(INOUT) :: order
    INTEGER(C_INT), INTENT(INOUT) :: numNodes
    TYPE(C_PTR), INTENT(OUT) :: api_localNodeCoord_
    INTEGER(C_SIZE_T), INTENT(INOUT) :: api_localNodeCoord_n_
    INTEGER(C_INT), INTENT(INOUT) :: numPrimaryNodes
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshGetElementProperties
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Get the elements of type `elementType' classified on the entity of tag
! `tag'. If `tag' < 0, get the elements for all entities. `elementTags' is a
! vector containing the tags (unique, strictly positive identifiers) of the
! elements of the corresponding type. `nodeTags' is a vector of length equal
! to the number of elements of the given type times the number N of nodes for
! this type of element, that contains the node tags of all the elements of
! the given type, concatenated: [e1n1, e1n2, ..., e1nN, e2n1, ...]. If
! `numTasks' > 1, only compute and return the part of the data indexed by
! `task' (for C++ only; output vectors must be preallocated).

INTERFACE
  SUBROUTINE gmshModelMeshGetElementsByType( &
    elementType, api_elementTags_, api_elementTags_n_, api_nodeTags_, &
    api_nodeTags_n_, tag, task, numTasks, ierr) &
    BIND(C, name="gmshModelMeshGetElementsByType")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(in) :: elementType
    TYPE(C_PTR), INTENT(out) :: api_elementTags_
    INTEGER(C_SIZE_T), INTENT(out) :: api_elementTags_n_
    TYPE(C_PTR), INTENT(out) :: api_nodeTags_
    INTEGER(C_SIZE_T), INTENT(out) :: api_nodeTags_n_
    INTEGER(C_INT), VALUE, INTENT(in) :: tag
    INTEGER(C_SIZE_T), VALUE, INTENT(in) :: task
    INTEGER(C_SIZE_T), VALUE, INTENT(in) :: numTasks
    INTEGER(C_INT), INTENT(out), OPTIONAL :: ierr
  END SUBROUTINE gmshModelMeshGetElementsByType
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Get the maximum tag `maxTag' of an element in the mesh.

INTERFACE
  SUBROUTINE gmshModelMeshGetMaxElementTag(maxTag, ierr) &
    BIND(C, name="gmshModelMeshGetMaxElementTag")
    IMPORT
    INTEGER(C_SIZE_T) :: maxTag
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshGetMaxElementTag
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Preallocate data before calling `getElementsByType' with `numTasks' > 1.
! For C++ only.

INTERFACE
  SUBROUTINE gmshModelMeshPreallocateElementsByType( &
    elementType, elementTag, nodeTag, api_elementTags_, &
    api_elementTags_n_, api_nodeTags_, api_nodeTags_n_, tag, ierr) &
    BIND(C, name="gmshModelMeshPreallocateElementsByType")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: elementType
    INTEGER(C_INT), VALUE, INTENT(IN) :: elementTag
    INTEGER(C_INT), VALUE, INTENT(IN) :: nodeTag
    TYPE(C_PTR), INTENT(OUT) :: api_elementTags_
    INTEGER(C_SIZE_T), INTENT(OUT) :: api_elementTags_n_
    TYPE(C_PTR), INTENT(OUT) :: api_nodeTags_
    INTEGER(C_SIZE_T), INTENT(OUT) :: api_nodeTags_n_
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshPreallocateElementsByType
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the quality `elementQualities' of the elements with tags `elementTags'.
! `qualityType' is the requested quality measure: "minDetJac" and "maxDetJac"
! for the adaptively computed minimal and maximal Jacobian determinant,
! "minSJ" for the sampled minimal scaled jacobien, "minSICN" for the sampled
! minimal signed inverted condition number, "minSIGE" for the sampled signed
! inverted gradient error, "gamma" for the ratio of the inscribed to
! circumcribed sphere radius, "innerRadius" for the inner radius,
! "outerRadius" for the outerRadius, "minIsotropy" for the minimum isotropy
! measure, "angleShape" for the angle shape measure, "minEdge" for the
! minimum straight edge length, "maxEdge" for the maximum straight edge
! length, "volume" for the volume. If `numTasks' > 1, only compute and return
! the part of the data indexed by `task' (for C++ only; output vector must be
! preallocated).

INTERFACE
  SUBROUTINE gmshModelMeshGetElementQualities( &
    elementTags, elementTags_n, elementsQuality, &
    elementsQuality_n, qualityName, task, numTasks, ierr) &
    BIND(C, name="gmshModelMeshGetElementQualities")
    IMPORT
    INTEGER(C_SIZE_T), INTENT(INOUT) :: elementTags(*)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: elementTags_n
    TYPE(C_PTR), INTENT(OUT) :: elementsQuality
    INTEGER(C_SIZE_T), INTENT(INOUT) :: elementsQuality_n
    CHARACTER(len=1, kind=C_CHAR), OPTIONAL, INTENT(IN) :: &
      qualityName(*)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: task
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: numTasks
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshGetElementQualities
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Add elements classified on the entity of dimension `dim' and tag `tag'.
! `types' contains the MSH types of the elements (e.g. `2' for 3-node
! triangles: see the Gmsh reference manual). `elementTags' is a vector of the
! same length as `types'; each entry is a vector containing the tags (unique,
! strictly positive identifiers) of the elements of the corresponding type.
! `nodeTags' is also a vector of the same length as `types'; each entry is a
! vector of length equal to the number of elements of the given type times
! the number N of nodes per element, that contains the node tags of all the
! elements of the given type, concatenated: [e1n1, e1n2, ..., e1nN, e2n1,
! ...].

INTERFACE
  SUBROUTINE gmshModelMeshAddElements( &
    dim, tag, elementTypes, elementTypes_n, elementTags, &
    elementTags_n, elementTags_nn, nodeTags, nodeTags_n, nodeTags_nn, &
    ierr) &
    BIND(C, name="gmshModelMeshAddElements")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    INTEGER(C_INT), INTENT(INOUT) :: elementTypes(*)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: elementTypes_n
    TYPE(C_PTR), INTENT(IN) :: elementTags
    TYPE(C_PTR), INTENT(IN) :: elementTags_n
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: elementTags_nn
    TYPE(C_PTR), INTENT(IN) :: nodeTags
    TYPE(C_PTR), INTENT(IN) :: nodeTags_n
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: nodeTags_nn
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshAddElements
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Add elements of type `elementType' classified on the entity of tag `tag'.
! `elementTags' contains the tags (unique, strictly positive identifiers) of
! the elements of the corresponding type. `nodeTags' is a vector of length
! equal to the number of elements times the number N of nodes per element,
! that contains the node tags of all the elements, concatenated: [e1n1, e1n2,
! ..., e1nN, e2n1, ...]. If the `elementTag' vector is empty, new tags are
! automatically assigned to the elements.

INTERFACE
  SUBROUTINE gmshModelMeshAddElementsByType( &
    tag, elementType, elementTags, elementTags_n, nodeTags, nodeTags_n, &
    ierr) BIND(C, name="gmshModelMeshAddElementsByType")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    INTEGER(C_INT), VALUE, INTENT(IN) :: elementType
    INTEGER(C_SIZE_T), INTENT(INOUT) :: elementTags(*)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: elementTags_n
    INTEGER(C_SIZE_T), INTENT(INOUT) :: nodeTags(*)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: nodeTags_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshAddElementsByType
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the numerical quadrature information for the given element type
! `elementType' and integration rule `integrationType', where
! `integrationType' concatenates the integration rule family name with the
! desired order (e.g. "Gauss4" for a quadrature suited for integrating 4th
! order polynomials). The "CompositeGauss" family uses tensor-product rules
! based the 1D Gauss-Legendre rule; the "Gauss" family uses an economic
! scheme when available (i.e. with a minimal number of points), and falls
! back to "CompositeGauss" otherwise. Note that integration points for the
! "Gauss" family can fall outside of the reference element for high-order
! rules. `localCoord' contains the u, v, w coordinates of the G integration
! points in the reference element: [g1u, g1v, g1w, ..., gGu, gGv, gGw].
! `weights' contains the associated weights: [g1q, ..., gGq].

INTERFACE

  SUBROUTINE gmshModelMeshGetIntegrationPoints( &
    elementType, integrationType, localCoord, localCoord_n, &
    weights, weights_n, ierr) &
    BIND(C, name="gmshModelMeshGetIntegrationPoints")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: elementType
    CHARACTER(len=1, kind=C_CHAR), INTENT(IN) :: integrationType(*)
    TYPE(C_PTR), INTENT(OUT) :: localCoord
    INTEGER(C_SIZE_T), INTENT(INOUT) :: localCoord_n
    TYPE(C_PTR), INTENT(OUT) :: weights
    INTEGER(C_SIZE_T), INTENT(INOUT) :: weights_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshGetIntegrationPoints
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the Jacobians of all the elements of type `elementType' classified on
! the entity of tag `tag', at the G evaluation points `localCoord' given as
! concatenated u, v, w coordinates in the reference element [g1u, g1v, g1w,
! ..., gGu, gGv, gGw]. Data is returned by element, with elements in the same
! order as in `getElements' and `getElementsByType'. `jacobians' contains for
! each element the 9 entries of the 3x3 Jacobian matrix at each evaluation
! point. The matrix is returned by column: [e1g1Jxu, e1g1Jyu, e1g1Jzu,
! e1g1Jxv, ..., e1g1Jzw, e1g2Jxu, ..., e1gGJzw, e2g1Jxu, ...], with Jxu =
! dx/du, Jyu = dy/du, etc. `determinants' contains for each element the
! determinant of the Jacobian matrix at each evaluation point: [e1g1, e1g2,
! ... e1gG, e2g1, ...]. `coord' contains for each element the x, y, z
! coordinates of the evaluation points. If `tag' < 0, get the Jacobian data
! for all entities. If `numTasks' > 1, only compute and return the part of
! the data indexed by `task' (for C++ only; output vectors must be
! preallocated).

INTERFACE
  SUBROUTINE gmshModelMeshGetJacobians( &
    elementType, localCoord, localCoord_n, jacobians, jacobians_n, &
    determinants, determinants_n, coord, coord_n, tag, &
    task, numTasks, ierr) &
    BIND(C, name="gmshModelMeshGetJacobians")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: elementType
    REAL(C_DOUBLE), INTENT(INOUT) :: localCoord(*)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: localCoord_n
    TYPE(C_PTR), INTENT(OUT) :: jacobians
    INTEGER(C_SIZE_T), INTENT(INOUT) :: jacobians_n
    TYPE(C_PTR), INTENT(OUT) :: determinants
    INTEGER(C_SIZE_T), INTENT(INOUT) :: determinants_n
    TYPE(C_PTR), INTENT(OUT) :: coord
    INTEGER(C_SIZE_T), INTENT(INOUT) :: coord_n
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: task
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: numTasks
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshGetJacobians
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Preallocate data before calling `getJacobians' with `numTasks' > 1. For C++
! only.

INTERFACE
  SUBROUTINE gmshModelMeshPreallocateJacobians( &
    elementType, numEvaluationPoints, allocateJacobians, &
    allocateDeterminants, allocateCoord, jacobians, jacobians_n, &
    determinants, determinants_n, coord, coord_n, tag, ierr) &
    BIND(C, name="gmshModelMeshPreallocateJacobians")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: elementType
    INTEGER(C_INT), VALUE, INTENT(IN) :: numEvaluationPoints
    INTEGER(C_INT), VALUE, INTENT(IN) :: allocateJacobians
    INTEGER(C_INT), VALUE, INTENT(IN) :: allocateDeterminants
    INTEGER(C_INT), VALUE, INTENT(IN) :: allocateCoord
    TYPE(C_PTR), INTENT(OUT) :: jacobians
    INTEGER(C_SIZE_T), INTENT(INOUT) :: jacobians_n
    TYPE(C_PTR), INTENT(OUT) :: determinants
    INTEGER(C_SIZE_T), INTENT(INOUT) :: determinants_n
    TYPE(C_PTR), INTENT(OUT) :: coord
    INTEGER(C_SIZE_T), INTENT(INOUT) :: coord_n
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshPreallocateJacobians
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the Jacobian for a single element `elementTag', at the G evaluation
! points `localCoord' given as concatenated u, v, w coordinates in the
! reference element [g1u, g1v, g1w, ..., gGu, gGv, gGw]. `jacobians' contains
! the 9 entries of the 3x3 Jacobian matrix at each evaluation point. The
! matrix is returned by column: [e1g1Jxu, e1g1Jyu, e1g1Jzu, e1g1Jxv, ...,
! e1g1Jzw, e1g2Jxu, ..., e1gGJzw, e2g1Jxu, ...], with Jxu = dx/du, Jyu =
! dy/du, etc. `determinants' contains the determinant of the Jacobian matrix
! at each evaluation point. `coord' contains the x, y, z coordinates of the
! evaluation points. This function relies on an internal cache (a vector in
! case of dense element numbering, a map otherwise); for large meshes
! accessing Jacobians in bulk is often preferable.

INTERFACE
  SUBROUTINE gmshModelMeshGetJacobian( &
    elementTag, localCoord, localCoord_n, jacobians, jacobians_n, &
    determinants, determinants_n, coord, coord_n, ierr) &
    BIND(C, name="gmshModelMeshGetJacobian")
    IMPORT
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: elementTag
    REAL(C_DOUBLE), INTENT(INOUT) :: localCoord(*)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: localCoord_n
    TYPE(C_PTR), INTENT(OUT) :: jacobians
    INTEGER(C_SIZE_T), INTENT(INOUT) :: jacobians_n
    TYPE(C_PTR), INTENT(OUT) :: determinants
    INTEGER(C_SIZE_T), INTENT(INOUT) :: determinants_n
    TYPE(C_PTR), INTENT(OUT) :: coord
    INTEGER(C_SIZE_T), INTENT(INOUT) :: coord_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshGetJacobian
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the basis functions of the element of type `elementType' at the
! evaluation points `localCoord' (given as concatenated u, v, w coordinates
! in the reference element [g1u, g1v, g1w, ..., gGu, gGv, gGw]), for the
! function space `functionSpaceType'. Currently supported function spaces
! include "Lagrange" and "GradLagrange" for isoparametric Lagrange basis
! functions and their gradient in the u, v, w coordinates of the reference
! element; "LagrangeN" and "GradLagrangeN", with N = 1, 2, ..., for N-th
! order Lagrange basis functions; "H1LegendreN" and "GradH1LegendreN", with N
! = 1, 2, ..., for N-th order hierarchical H1 Legendre functions;
! "HcurlLegendreN" and "CurlHcurlLegendreN", with N = 1, 2, ..., for N-th
! order curl-conforming basis functions. `numComponents' returns the number C
! of components of a basis function (e.g. 1 for scalar functions and 3 for
! vector functions). `basisFunctions' returns the value of the N basis
! functions at the evaluation points, i.e. [g1f1, g1f2, ..., g1fN, g2f1, ...]
! when C == 1 or [g1f1u, g1f1v, g1f1w, g1f2u, ..., g1fNw, g2f1u, ...] when C
! == 3. For basis functions that depend on the orientation of the elements,
! all values for the first orientation are returned first, followed by values
! for the second, etc. `numOrientations' returns the overall number of
! orientations. If the `wantedOrientations' vector is not empty, only return
! the values for the desired orientation indices.

INTERFACE

  SUBROUTINE gmshModelMeshGetBasisFunctions( &
    elementType, localCoord, localCoord_n, functionSpaceType, &
    numComponents, basisFunctions, basisFunctions_n, numOrientations, &
    wantedOrientations, wantedOrientations_n, ierr) &
    BIND(C, name="gmshModelMeshGetBasisFunctions")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: elementType
    REAL(C_DOUBLE), INTENT(INOUT) :: localCoord(*)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: localCoord_n
    CHARACTER(len=1, kind=C_CHAR), INTENT(IN) :: &
      functionSpaceType(*)
    INTEGER(C_INT), INTENT(INOUT) :: numComponents
    TYPE(C_PTR), INTENT(OUT) :: basisFunctions
    INTEGER(C_SIZE_T), INTENT(INOUT) :: basisFunctions_n
    INTEGER(C_INT), INTENT(INOUT) :: numOrientations
    INTEGER(C_INT), OPTIONAL, INTENT(INOUT) :: wantedOrientations(*)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: wantedOrientations_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshGetBasisFunctions
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the orientation index of the elements of type `elementType' in the
! entity of tag `tag'. The arguments have the same meaning as in
! `getBasisFunctions'. `basisFunctionsOrientation' is a vector giving for
! each element the orientation index in the values returned by
! `getBasisFunctions'. For Lagrange basis functions the call is superfluous
! as it will return a vector of zeros. If `numTasks' > 1, only compute and
! return the part of the data indexed by `task' (for C++ only; output vector
! must be preallocated).

INTERFACE
  SUBROUTINE gmshModelMeshGetBasisFunctionsOrientation( &
    elementType, functionSpaceType, basisFunctionsOrientation, &
    basisFunctionsOrientation_n, tag, task, numTasks, ierr) &
    BIND(C, name="gmshModelMeshGetBasisFunctionsOrientation")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: elementType
    CHARACTER(len=1, kind=C_CHAR), INTENT(IN) :: functionSpaceType(*)
    TYPE(C_PTR), INTENT(OUT) :: basisFunctionsOrientation
    INTEGER(C_SIZE_T), INTENT(OUT) :: basisFunctionsOrientation_n
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: task
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: numTasks
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshGetBasisFunctionsOrientation
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the orientation of a single element `elementTag'.

INTERFACE
  SUBROUTINE gmshModelMeshGetBasisFunctionsOrientationForElement( &
    elementTag, functionSpaceType, basisFunctionsOrientation, ierr) &
    BIND(C, name="gmshModelMeshGetBasisFunctionsOrientationForElement")
    IMPORT
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: elementTag
    CHARACTER(len=1, kind=C_CHAR), INTENT(IN) :: functionSpaceType(*)
    INTEGER(C_INT), INTENT(INOUT) :: basisFunctionsOrientation
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshGetBasisFunctionsOrientationForElement
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the number of possible orientations for elements of type `elementType'
! and function space named `functionSpaceType'.
INTERFACE
  FUNCTION gmshModelMeshGetNumberOfOrientations( &
    elementType, functionSpaceType, ierr) &
    BIND(C, name="gmshModelMeshGetNumberOfOrientations")
    IMPORT
    INTEGER(C_INT) :: gmshModelMeshGetNumberOfOrientations
    INTEGER(C_INT), VALUE, INTENT(IN) :: elementType
    CHARACTER(len=1, kind=C_CHAR), INTENT(IN) :: &
      functionSpaceType(*)
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END FUNCTION gmshModelMeshGetNumberOfOrientations
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Preallocate data before calling `getBasisFunctionsOrientation' with
! `numTasks' > 1. For C++ only.

INTERFACE
  SUBROUTINE gmshModelMeshPreallocateBasisFunctionsOrientation( &
    elementType, basisFunctionsOrientation, &
    basisFunctionsOrientation_n, tag, ierr) &
    BIND(C, name="gmshModelMeshPreallocateBasisFunctionsOrientation")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: elementType
    TYPE(C_PTR), INTENT(OUT) :: basisFunctionsOrientation
    INTEGER(C_SIZE_T), INTENT(OUT) :: basisFunctionsOrientation_n
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshPreallocateBasisFunctionsOrientation
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the global unique mesh edge identifiers `edgeTags' and orientations
! `edgeOrientation' for an input list of node tag pairs defining these edges,
! concatenated in the vector `nodeTags'. Mesh edges are created e.g. by
! `createEdges()', `getKeys()' or `addEdges()'. The reference positive
! orientation is n1 < n2, where n1 and n2 are the tags of the two edge nodes,
! which corresponds to the local orientation of edge-based basis functions as
! well.

INTERFACE
  SUBROUTINE gmshModelMeshGetEdges( &
    nodeTags, nodeTags_n, edgeTags, edgeTags_n, edgeOrientations, &
    edgeOrientations_n, ierr) BIND(C, name="gmshModelMeshGetEdges")
    IMPORT
    INTEGER(C_SIZE_T), INTENT(INOUT) :: nodeTags(*)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: nodeTags_n
    TYPE(C_PTR), INTENT(OUT) :: edgeTags
    INTEGER(C_SIZE_T), INTENT(OUT) :: edgeTags_n
    TYPE(C_PTR), INTENT(OUT) :: edgeOrientations
    INTEGER(C_SIZE_T), INTENT(OUT) :: edgeOrientations_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshGetEdges
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the global unique mesh face identifiers `faceTags' and orientations
! `faceOrientations' for an input list of a multiple of three (if `faceType'
! == 3) or four (if `faceType' == 4) node tags defining these faces,
! concatenated in the vector `nodeTags'. Mesh faces are created e.g. by
! `createFaces()', `getKeys()' or `addFaces()'.

INTERFACE

  SUBROUTINE gmshModelMeshGetFaces( &
    faceType, nodeTags, nodeTags_n, faceTags, &
    faceTags_n, faceOrientations, faceOrientations_n, ierr) &
    BIND(C, name="gmshModelMeshGetFaces")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: faceType
    INTEGER(C_SIZE_T), INTENT(INOUT) :: nodeTags(*)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: nodeTags_n
    TYPE(C_PTR), INTENT(OUT) :: faceTags
    INTEGER(C_SIZE_T), INTENT(OUT) :: faceTags_n
    TYPE(C_PTR), INTENT(OUT) :: faceOrientations
    INTEGER(C_SIZE_T), INTENT(OUT) :: faceOrientations_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshGetFaces
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Create unique mesh edges for the entities `dimTags', given as a vector of
! (dim, tag) pairs.

INTERFACE
  SUBROUTINE gmshModelMeshCreateEdges(dimTags, dimTags_n, ierr) &
    BIND(C, name="gmshModelMeshCreateEdges")
    IMPORT
    INTEGER(C_INT), OPTIONAL, INTENT(INOUT) :: dimTags(*)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: dimTags_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshCreateEdges
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Create unique mesh faces for the entities `dimTags', given as a vector of
! (dim, tag) pairs.

INTERFACE
  SUBROUTINE gmshModelMeshCreateFaces(dimTags, dimTags_n, ierr) &
    BIND(C, name="gmshModelMeshCreateFaces")
    IMPORT
    INTEGER(C_INT), OPTIONAL, INTENT(INOUT) :: dimTags(*)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: dimTags_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshCreateFaces
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the global unique identifiers `edgeTags' and the nodes `edgeNodes' of
! the edges in the mesh. Mesh edges are created e.g. by `createEdges()',
! `getKeys()' or addEdges().

INTERFACE
  SUBROUTINE gmshModelMeshGetAllEdges( &
    edgeTags, edgeTags_n, edgeNodes, edgeNodes_n, ierr) &
    BIND(C, name="gmshModelMeshGetAllEdges")
    IMPORT
    TYPE(C_PTR), INTENT(OUT) :: edgeTags
    INTEGER(C_SIZE_T), INTENT(OUT) :: edgeTags_n
    TYPE(C_PTR), INTENT(OUT) :: edgeNodes
    INTEGER(C_SIZE_T), INTENT(OUT) :: edgeNodes_n
    INTEGER(C_INT), INTENT(OUT), OPTIONAL :: ierr
  END SUBROUTINE gmshModelMeshGetAllEdges
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the global unique identifiers `faceTags' and the nodes `faceNodes' of
! the faces of type `faceType' in the mesh. Mesh faces are created e.g. by
! `createFaces()', `getKeys()' or addFaces().

INTERFACE

  SUBROUTINE gmshModelMeshGetAllFaces( &
    faceType, faceTags, faceTags_n, faceNodes, faceNodes_n, ierr) &
    BIND(C, name="gmshModelMeshGetAllFaces")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: faceType
    TYPE(C_PTR), INTENT(OUT) :: faceTags
    INTEGER(C_SIZE_T), INTENT(OUT) :: faceTags_n
    TYPE(C_PTR), INTENT(OUT) :: faceNodes
    INTEGER(C_SIZE_T), INTENT(OUT) :: faceNodes_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshGetAllFaces
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Add mesh edges defined by their global unique identifiers `edgeTags' and
! their nodes `edgeNodes'.

INTERFACE
  SUBROUTINE gmshModelMeshAddEdges( &
    edgeTags, edgeTags_n, edgeNodes, edgeNodes_n, ierr) &
    BIND(C, name="gmshModelMeshAddEdges")
    IMPORT
    INTEGER(C_SIZE_T), INTENT(INOUT) :: edgeTags(*)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: edgeTags_n
    INTEGER(C_SIZE_T), INTENT(INOUT) :: edgeNodes(*)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: edgeNodes_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshAddEdges
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Add mesh faces of type `faceType' defined by their global unique
! identifiers `faceTags' and their nodes `faceNodes'.

INTERFACE
  SUBROUTINE gmshModelMeshAddFaces( &
    faceType, faceTags, faceTags_n, faceNodes, faceNodes_n, ierr) &
    BIND(C, name="gmshModelMeshAddFaces")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: faceType
    INTEGER(C_SIZE_T), INTENT(INOUT) :: faceTags(*)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: faceTags_n
    INTEGER(C_SIZE_T), INTENT(INOUT) :: faceNodes(*)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: faceNodes_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshAddFaces
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Generate the pair of keys for the elements of type `elementType' in the
! entity of tag `tag', for the `functionSpaceType' function space. Each pair
! (`typeKey', `entityKey') uniquely identifies a basis function in the
! function space. If `returnCoord' is set, the `coord' vector contains the x,
! y, z coordinates locating basis functions for sorting purposes. Warning:
! this is an experimental feature and will probably change in a future
! release.

INTERFACE
  SUBROUTINE gmshModelMeshGetKeys( &
    elementType, functionSpaceType, typeKeys, typeKeys_n, entityKeys, &
    entityKeys_n, coord, coord_n, tag, returnCoord, ierr) &
    BIND(C, name="gmshModelMeshGetKeys")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: elementType
    CHARACTER(len=1, kind=C_CHAR), INTENT(IN) :: functionSpaceType(*)
    TYPE(C_PTR), INTENT(OUT) :: typeKeys
    INTEGER(C_SIZE_T), INTENT(OUT) :: typeKeys_n
    TYPE(C_PTR), INTENT(OUT) :: entityKeys
    INTEGER(C_SIZE_T), INTENT(OUT) :: entityKeys_n
    TYPE(C_PTR), INTENT(OUT) :: coord
    INTEGER(C_SIZE_T), INTENT(INOUT) :: coord_n
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    INTEGER(C_INT), VALUE, INTENT(IN) :: returnCoord
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshGetKeys
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the pair of keys for a single element `elementTag'.
INTERFACE
  SUBROUTINE gmshModelMeshGetKeysForElement( &
    elementTag, functionSpaceType, typeKeys, typeKeys_n, &
    entityKeys, entityKeys_n, coord, coord_n, returnCoord, ierr) &
    BIND(C, name="gmshModelMeshGetKeysForElement")
    IMPORT
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: elementTag
    CHARACTER(len=1, kind=C_CHAR), INTENT(IN) :: functionSpaceType(*)
    TYPE(C_PTR), INTENT(OUT) :: typeKeys
    INTEGER(C_SIZE_T), INTENT(OUT) :: typeKeys_n
    TYPE(C_PTR), INTENT(OUT) :: entityKeys
    INTEGER(C_SIZE_T), INTENT(OUT) :: entityKeys_n
    TYPE(C_PTR), INTENT(OUT) :: coord
    INTEGER(C_SIZE_T), INTENT(INOUT) :: coord_n
    INTEGER(C_INT), VALUE, INTENT(IN) :: returnCoord
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshGetKeysForElement
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the number of keys by elements of type `elementType' for function space
! named `functionSpaceType'.

INTERFACE
  FUNCTION gmshModelMeshGetNumberOfKeys( &
    elementType, functionSpaceType, ierr) &
    BIND(C, name="gmshModelMeshGetNumberOfKeys")
    IMPORT
    INTEGER(C_INT) :: gmshModelMeshGetNumberOfKeys
    INTEGER(C_INT), VALUE, INTENT(IN) :: elementType
    CHARACTER(len=1, kind=C_CHAR), INTENT(IN) :: functionSpaceType(*)
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END FUNCTION gmshModelMeshGetNumberOfKeys
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get information about the pair of `keys'. `infoKeys' returns information
! about the functions associated with the pairs (`typeKeys', `entityKey').
! `infoKeys[0].first' describes the type of function (0 for  vertex function,
! 1 for edge function, 2 for face function and 3 for bubble function).
! `infoKeys[0].second' gives the order of the function associated with the
! key. Warning: this is an experimental feature and will probably change in a
! future release.

INTERFACE

  SUBROUTINE gmshModelMeshGetKeysInformation( &
    typeKeys, typeKeys_n, entityKeys, entityKeys_n, elementType, &
    functionSpaceType, infoKeys, infoKeys_n, ierr) &
    BIND(C, name="gmshModelMeshGetKeysInformation")
    IMPORT
    INTEGER(C_INT), INTENT(INOUT) :: typeKeys(*)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: typeKeys_n
    INTEGER(C_SIZE_T), INTENT(INOUT) :: entityKeys(*)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: entityKeys_n
    INTEGER(C_INT), VALUE, INTENT(IN) :: elementType
    CHARACTER(len=1, kind=C_CHAR), INTENT(IN) :: functionSpaceType(*)
    TYPE(C_PTR), INTENT(OUT) :: infoKeys
    INTEGER(C_SIZE_T), INTENT(OUT) :: infoKeys_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshGetKeysInformation
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the barycenters of all elements of type `elementType' classified on the
! entity of tag `tag'. If `primary' is set, only the primary nodes of the
! elements are taken into account for the barycenter calculation. If `fast'
! is set, the function returns the sum of the primary node coordinates
! (without normalizing by the number of nodes). If `tag' < 0, get the
! barycenters for all entities. If `numTasks' > 1, only compute and return
! the part of the data indexed by `task' (for C++ only; output vector must be
! preallocated).

INTERFACE
  SUBROUTINE gmshModelMeshGetBarycenters( &
    elementType, tag, fast, primary, barycenters, &
    barycenters_n, task, numTasks, ierr) &
    BIND(C, name="gmshModelMeshGetBarycenters")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: elementType
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    INTEGER(C_INT), VALUE, INTENT(IN) :: fast
    INTEGER(C_INT), VALUE, INTENT(IN) :: primary
    TYPE(C_PTR), INTENT(OUT) :: barycenters
    INTEGER(C_SIZE_T), INTENT(INOUT) :: barycenters_n
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: task
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: numTasks
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshGetBarycenters
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Preallocate data before calling `getBarycenters' with `numTasks' > 1. For
! C++ only.

INTERFACE

  SUBROUTINE gmshModelMeshPreallocateBarycenters( &
    elementType, barycenters, barycenters_n, tag, ierr) &
    BIND(C, name="gmshModelMeshPreallocateBarycenters")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: elementType
    TYPE(C_PTR), INTENT(OUT) :: barycenters
    INTEGER(C_SIZE_T), INTENT(INOUT) :: barycenters_n
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshPreallocateBarycenters
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the nodes on the edges of all elements of type `elementType' classified
! on the entity of tag `tag'. `nodeTags' contains the node tags of the edges
! for all the elements: [e1a1n1, e1a1n2, e1a2n1, ...]. Data is returned by
! element, with elements in the same order as in `getElements' and
! `getElementsByType'. If `primary' is set, only the primary (begin/end)
! nodes of the edges are returned. If `tag' < 0, get the edge nodes for all
! entities. If `numTasks' > 1, only compute and return the part of the data
! indexed by `task' (for C++ only; output vector must be preallocated).

INTERFACE

  SUBROUTINE gmshModelMeshGetElementEdgeNodes( &
    elementType, nodeTags, nodeTags_n, tag, primary, task, &
    numTasks, ierr) &
    BIND(C, name="gmshModelMeshGetElementEdgeNodes")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: elementType
    TYPE(C_PTR), INTENT(OUT) :: nodeTags
    INTEGER(C_SIZE_T), INTENT(OUT) :: nodeTags_n
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    INTEGER(C_INT), VALUE, INTENT(IN) :: primary
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: task
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: numTasks
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshGetElementEdgeNodes
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the nodes on the faces of type `faceType' (3 for triangular faces, 4
! for quadrangular faces) of all elements of type `elementType' classified on
! the entity of tag `tag'. `nodeTags' contains the node tags of the faces for
! all elements: [e1f1n1, ..., e1f1nFaceType, e1f2n1, ...]. Data is returned
! by element, with elements in the same order as in `getElements' and
! `getElementsByType'. If `primary' is set, only the primary (corner) nodes
! of the faces are returned. If `tag' < 0, get the face nodes for all
! entities. If `numTasks' > 1, only compute and return the part of the data
! indexed by `task' (for C++ only; output vector must be preallocated).

INTERFACE

  SUBROUTINE gmshModelMeshGetElementFaceNodes( &
    elementType, faceType, nodeTags, nodeTags_n, tag, primary, &
    task, numTasks, ierr) &
    BIND(C, name="gmshModelMeshGetElementFaceNodes")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: elementType
    INTEGER(C_INT), VALUE, INTENT(IN) :: faceType
    TYPE(C_PTR), INTENT(OUT) :: nodeTags
    INTEGER(C_SIZE_T), INTENT(OUT) :: nodeTags_n
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    INTEGER(C_INT), VALUE, INTENT(IN) :: primary
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: task
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: numTasks
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshGetElementFaceNodes
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the ghost elements `elementTags' and their associated `partitions'
! stored in the ghost entity of dimension `dim' and tag `tag'.

INTERFACE
  SUBROUTINE gmshModelMeshGetGhostElements( &
    dim, tag, elementTags, elementTags_n, partitions, &
    partitions_n, ierr) &
    BIND(C, name="gmshModelMeshGetGhostElements")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    TYPE(C_PTR), INTENT(OUT) :: elementTags
    INTEGER(C_SIZE_T), INTENT(OUT) :: elementTags_n
    TYPE(C_PTR), INTENT(OUT) :: partitions
    INTEGER(C_SIZE_T), INTENT(OUT) :: partitions_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshGetGhostElements
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Set a mesh size constraint on the model entities `dimTags', given as a
! vector of (dim, tag) pairs. Currently only entities of dimension 0 (points)
! are handled.

INTERFACE
  SUBROUTINE gmshModelMeshSetSize( &
    dimTags, dimTags_n, size, ierr) &
    BIND(C, name="gmshModelMeshSetSize")
    IMPORT
    INTEGER(C_INT), INTENT(INOUT) :: dimTags(*)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: dimTags_n
    REAL(C_DOUBLE), VALUE, INTENT(IN) :: size
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshSetSize
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the mesh size constraints (if any) associated with the model entities
! `dimTags', given as a vector of (dim, tag) pairs. A zero entry in the
! output `sizes' vector indicates that no size constraint is specified on the
! corresponding entity.

INTERFACE
  SUBROUTINE gmshModelMeshGetSizes( &
    dimTags, dimTags_n, sizes, sizes_n, ierr) &
    BIND(C, name="gmshModelMeshGetSizes")
    IMPORT
    INTEGER(C_INT), INTENT(INOUT) :: dimTags(*)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: dimTags_n
    TYPE(C_PTR), INTENT(OUT) :: sizes
    INTEGER(C_SIZE_T), INTENT(INOUT) :: sizes_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshGetSizes
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Set mesh size constraints at the given parametric points `parametricCoord'
! on the model entity of dimension `dim' and tag `tag'. Currently only
! entities of dimension 1 (lines) are handled.

INTERFACE
  SUBROUTINE gmshModelMeshSetSizeAtParametricPoints( &
    dim, tag, parametricCoord, parametricCoord_n, sizes, sizes_n, ierr) &
    BIND(C, name="gmshModelMeshSetSizeAtParametricPoints")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    REAL(C_DOUBLE), INTENT(INOUT) :: parametricCoord(*)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: parametricCoord_n
    REAL(C_DOUBLE), INTENT(INOUT) :: sizes(*)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: sizes_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshSetSizeAtParametricPoints
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Set a mesh size callback for the current model. The callback function
! should take six arguments as input (`dim', `tag', `x', `y', `z' and `lc').
! The first two integer arguments correspond to the dimension `dim' and tag
! `tag' of the entity being meshed. The next four double precision arguments
! correspond to the coordinates `x', `y' and `z' around which to prescribe
! the mesh size and to the mesh size `lc' that would be prescribed if the
! callback had not been called. The callback function should return a double
! precision number specifying the desired mesh size; returning `lc' is
! equivalent to a no-op.

INTERFACE
  SUBROUTINE gmshModelMeshSetSizeCallback(callback, ierr) &
    BIND(C, name="gmshModelMeshSetSizeCallback")
    IMPORT
    TYPE(C_FUNPTR), VALUE, INTENT(IN) :: callback
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshSetSizeCallback
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Remove the mesh size callback from the current model.

INTERFACE
  SUBROUTINE gmshModelMeshRemoveSizeCallback(ierr) &
    BIND(C, name="gmshModelMeshRemoveSizeCallback")
    IMPORT
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshRemoveSizeCallback
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Set a transfinite meshing constraint on the curve `tag', with `numNodes'
! nodes distributed according to `meshType' and `coef'. Currently supported
! types are "Progression" (geometrical progression with power `coef'), "Bump"
! (refinement toward both extremities of the curve) and "Beta" (beta law).

INTERFACE
  SUBROUTINE gmshModelMeshSetTransfiniteCurve( &
    tag, numNodes, meshType, coef, ierr) &
    BIND(C, name="gmshModelMeshSetTransfiniteCurve")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    INTEGER(C_INT), VALUE, INTENT(IN) :: numNodes
    CHARACTER(len=1, kind=C_CHAR), OPTIONAL, INTENT(IN) :: meshType(*)
    REAL(C_DOUBLE), VALUE, INTENT(IN) :: coef
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshSetTransfiniteCurve
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Set a transfinite meshing constraint on the surface `tag'. `arrangement'
! describes the arrangement of the triangles when the surface is not flagged
! as recombined: currently supported values are "Left", "Right",
! "AlternateLeft" and "AlternateRight". `cornerTags' can be used to specify
! the (3 or 4) corners of the transfinite interpolation explicitly;
! specifying the corners explicitly is mandatory if the surface has more that
! 3 or 4 points on its boundary.

INTERFACE
  SUBROUTINE gmshModelMeshSetTransfiniteSurface( &
    tag, arrangement, cornerTags, cornerTags_n, ierr) &
    BIND(C, name="gmshModelMeshSetTransfiniteSurface")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    CHARACTER(len=1, kind=C_CHAR), OPTIONAL, INTENT(IN) :: arrangement(*)
    INTEGER(C_INT), OPTIONAL, INTENT(INOUT) :: cornerTags(*)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: cornerTags_n
    INTEGER(C_INT), OPTIONAL, INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshSetTransfiniteSurface
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Set a transfinite meshing constraint on the surface `tag'. `cornerTags' can
! be used to specify the (6 or 8) corners of the transfinite interpolation
! explicitly.

INTERFACE
  SUBROUTINE gmshModelMeshSetTransfiniteVolume( &
    tag, cornerTags, cornerTags_n, ierr) &
    BIND(C, name="gmshModelMeshSetTransfiniteVolume")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    INTEGER(C_INT), OPTIONAL, INTENT(INOUT) :: cornerTags(*)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: cornerTags_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshSetTransfiniteVolume
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Set transfinite meshing constraints on the model entities in `dimTags',
! given as a vector of (dim, tag) pairs. Transfinite meshing constraints are
! added to the curves of the quadrangular surfaces and to the faces of
! 6-sided volumes. Quadragular faces with a corner angle superior to
! `cornerAngle' (in radians) are ignored. The number of points is
! automatically determined from the sizing constraints. If `dimTag' is empty,
! the constraints are applied to all entities in the model. If `recombine' is
! true, the recombine flag is automatically set on the transfinite surfaces.

INTERFACE
  SUBROUTINE gmshModelMeshSetTransfiniteAutomatic( &
    dimTags, dimTags_n, cornerAngle, recombine, ierr) &
    BIND(C, name="gmshModelMeshSetTransfiniteAutomatic")
    IMPORT
    INTEGER(C_INT), OPTIONAL, INTENT(INOUT) :: dimTags(*)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: dimTags_n
    REAL(C_DOUBLE), VALUE, INTENT(IN) :: cornerAngle
    INTEGER(C_INT), VALUE, INTENT(IN) :: recombine
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshSetTransfiniteAutomatic
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Set a recombination meshing constraint on the model entity of dimension
! `dim' and tag `tag'. Currently only entities of dimension 2 (to recombine
! triangles into quadrangles) are supported; `angle' specifies the threshold
! angle for the simple recombination algorithm..

INTERFACE
  SUBROUTINE gmshModelMeshSetRecombine(dim, tag, angle, ierr) &
    BIND(C, name="gmshModelMeshSetRecombine")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    REAL(C_DOUBLE), VALUE, INTENT(IN) :: angle
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshSetRecombine
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Set a smoothing meshing constraint on the model entity of dimension `dim'
! and tag `tag'. `val' iterations of a Laplace smoother are applied.

INTERFACE
  SUBROUTINE gmshModelMeshSetSmoothing( &
    dim, tag, val, ierr) BIND(C, name="gmshModelMeshSetSmoothing")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    INTEGER(C_INT), VALUE, INTENT(IN) :: val
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshSetSmoothing
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Set a reverse meshing constraint on the model entity of dimension `dim' and
! tag `tag'. If `val' is true, the mesh orientation will be reversed with
! respect to the natural mesh orientation (i.e. the orientation consistent
! with the orientation of the geometry). If `val' is false, the mesh is left
! as-is.

INTERFACE
  SUBROUTINE gmshModelMeshSetReverse( &
    dim, tag, val, ierr) BIND(C, name="gmshModelMeshSetReverse")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    INTEGER(C_INT), VALUE, INTENT(IN) :: val
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshSetReverse
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Set the meshing algorithm on the model entity of dimension `dim' and tag
! `tag'. Supported values are those of the `Mesh.Algorithm' option, as listed
! in the Gmsh reference manual. Currently only supported for `dim' == 2.

INTERFACE
  SUBROUTINE gmshModelMeshSetAlgorithm( &
    dim, tag, val, ierr) BIND(C, name="gmshModelMeshSetAlgorithm")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    INTEGER(C_INT), VALUE, INTENT(IN) :: val
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshSetAlgorithm
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Force the mesh size to be extended from the boundary, or not, for the model
! entity of dimension `dim' and tag `tag'. Currently only supported for `dim'
! == 2.

INTERFACE
  SUBROUTINE gmshModelMeshSetSizeFromBoundary( &
    dim, tag, val, ierr) BIND(C, name="gmshModelMeshSetSizeFromBoundary")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    INTEGER(C_INT), VALUE, INTENT(IN) :: val
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshSetSizeFromBoundary
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Set a compound meshing constraint on the model entities of dimension `dim'
! and tags `tags'. During meshing, compound entities are treated as a single
! discrete entity, which is automatically reparametrized.

INTERFACE
  SUBROUTINE gmshModelMeshSetCompound( &
    dim, tags, tags_n, ierr) BIND(C, name="gmshModelMeshSetCompound")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim
    INTEGER(C_INT), INTENT(INOUT) :: tags(*)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: tags_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshSetCompound
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Set meshing constraints on the bounding surfaces of the volume of tag `tag'
! so that all surfaces are oriented with outward pointing normals; and if a
! mesh already exists, reorient it. Currently only available with the
! OpenCASCADE kernel, as it relies on the STL triangulation.

INTERFACE
  SUBROUTINE gmshModelMeshSetOutwardOrientation(tag, ierr) &
    BIND(C, name="gmshModelMeshSetOutwardOrientation")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshSetOutwardOrientation
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Remove all meshing constraints from the model entities `dimTags', given as
! a vector of (dim, tag) pairs. If `dimTags' is empty, remove all
! constraings.

INTERFACE
  SUBROUTINE gmshModelMeshRemoveConstraints(dimTags, dimTags_n, ierr) &
    BIND(C, name="gmshModelMeshRemoveConstraints")
    IMPORT
    INTEGER(C_INT), OPTIONAL, INTENT(INOUT) :: dimTags(*)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: dimTags_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshRemoveConstraints
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Embed the model entities of dimension `dim' and tags `tags' in the
! (`inDim', `inTag') model entity. The dimension `dim' can 0, 1 or 2 and must
! be strictly smaller than `inDim', which must be either 2 or 3. The embedded
! entities should not intersect each other or be part of the boundary of the
! entity `inTag', whose mesh will conform to the mesh of the embedded
! entities. With the OpenCASCADE kernel, if the `fragment' operation is
! applied to entities of different dimensions, the lower dimensional entities
! will be automatically embedded in the higher dimensional entities if they
! are not on their boundary.

INTERFACE
  SUBROUTINE gmshModelMeshEmbed( &
    dim, tags, tags_n, inDim, inTag, ierr) &
    BIND(C, name="gmshModelMeshEmbed")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim
    INTEGER(C_INT), INTENT(INOUT) :: tags(*)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: tags_n
    INTEGER(C_INT), VALUE, INTENT(IN) :: inDim
    INTEGER(C_INT), VALUE, INTENT(IN) :: inTag
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshEmbed
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Remove embedded entities from the model entities `dimTags', given as a
! vector of (dim, tag) pairs. if `dim' is >= 0, only remove embedded entities
! of the given dimension (e.g. embedded points if `dim' == 0).

INTERFACE

  SUBROUTINE gmshModelMeshRemoveEmbedded(dimTags, dimTags_n, dim, ierr) &
    BIND(C, name="gmshModelMeshRemoveEmbedded")
    IMPORT
    INTEGER(C_INT), INTENT(INOUT) :: dimTags(*)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: dimTags_n
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshRemoveEmbedded
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the entities (if any) embedded in the model entity of dimension `dim'
! and tag `tag'.

INTERFACE
  SUBROUTINE gmshModelMeshGetEmbedded( &
    dim, tag, dimTags, dimTags_n, ierr) &
    BIND(C, name="gmshModelMeshGetEmbedded")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    TYPE(C_PTR), INTENT(OUT) :: dimTags
    INTEGER(C_SIZE_T), INTENT(OUT) :: dimTags_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshGetEmbedded
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Reorder the elements of type `elementType' classified on the entity of tag
! `tag' according to the `ordering' vector.

INTERFACE
  SUBROUTINE gmshModelMeshReorderElements( &
    elementType, tag, ordering, ordering_n, ierr) &
    BIND(C, name="gmshModelMeshReorderElements")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(in) :: elementType
    INTEGER(C_INT), VALUE, INTENT(in) :: tag
    INTEGER(C_SIZE_T), INTENT(INOUT) :: ordering(*)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: ordering_n
    INTEGER(C_INT), INTENT(out), OPTIONAL :: ierr
  END SUBROUTINE gmshModelMeshReorderElements
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Compute a renumbering vector `newTags' corresponding to the input tags
! `oldTags' for a given list of element tags `elementTags'. If `elementTags'
! is empty, compute the renumbering on the full mesh. If `method' is equal to
! "RCMK", compute a node renumering with Reverse Cuthill McKee. If `method'
! is equal to "Hilbert", compute a node renumering along a Hilbert curve. If
! `method' is equal to "Metis", compute a node renumering using Metis.
! Element renumbering is not available yet.

INTERFACE
  SUBROUTINE gmshModelMeshComputeRenumbering( &
    oldTags, oldTags_n, newTags, newTags_n, method, elementTags, &
    elementTags_n, ierr) BIND(C, name="gmshModelMeshComputeRenumbering")
    IMPORT
    TYPE(C_PTR), INTENT(OUT) :: oldTags
    INTEGER(C_SIZE_T), INTENT(OUT) :: oldTags_n
    TYPE(C_PTR), INTENT(OUT) :: newTags
    INTEGER(C_SIZE_T), INTENT(OUT) :: newTags_n
    CHARACTER(len=1, kind=C_CHAR), OPTIONAL, INTENT(IN) :: method(*)
    INTEGER(C_SIZE_T), DIMENSION(*), OPTIONAL :: elementTags
    INTEGER(C_SIZE_T), VALUE, INTENT(in) :: elementTags_n
    INTEGER(C_INT), INTENT(out), OPTIONAL :: ierr
  END SUBROUTINE gmshModelMeshComputeRenumbering
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Renumber the node tags. If no explicit renumbering is provided through the
! `oldTags' and `newTags' vectors, renumber the nodes in a continuous
! sequence, taking into account the subset of elements to be saved later on
! if the option "Mesh.SaveAll" is not set.

INTERFACE
  SUBROUTINE gmshModelMeshRenumberNodes( &
    oldTags, oldTags_n, newTags, newTags_n, ierr) &
    BIND(C, name="gmshModelMeshRenumberNodes")
    IMPORT
    INTEGER(C_SIZE_T), OPTIONAL, INTENT(INOUT) :: oldTags(*)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: oldTags_n
    INTEGER(C_SIZE_T), OPTIONAL, INTENT(INOUT) :: newTags(*)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: newTags_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshRenumberNodes
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Renumber the element tags in a continuous sequence. If no explicit
! renumbering is provided through the `oldTags' and `newTags' vectors,
! renumber the elements in a continuous sequence, taking into account the
! subset of elements to be saved later on if the option "Mesh.SaveAll" is not
! set.

INTERFACE
  SUBROUTINE gmshModelMeshRenumberElements( &
    oldTags, oldTags_n, newTags, newTags_n, ierr) &
    BIND(C, name="gmshModelMeshRenumberElements")
    IMPORT
    INTEGER(C_SIZE_T), OPTIONAL, INTENT(INOUT) :: oldTags(*)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: oldTags_n
    INTEGER(C_SIZE_T), OPTIONAL, INTENT(INOUT) :: newTags(*)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: newTags_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshRenumberElements
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Set the meshes of the entities of dimension `dim' and tag `tags' as
! periodic copies of the meshes of entities `tagsMaster', using the affine
! transformation specified in `affineTransformation' (16 entries of a 4x4
! matrix, by row). If used after meshing, generate the periodic node
! correspondence information assuming the meshes of entities `tags'
! effectively match the meshes of entities `tagsMaster' (useful for
! structured and extruded meshes). Currently only available for @code{dim} ==
! 1 and @code{dim} == 2.

INTERFACE
  SUBROUTINE gmshModelMeshSetPeriodic( &
    dim, tags, tags_n, tagsMaster, tagsMaster_n, &
    affineTransform, affineTransform_n, ierr) &
    BIND(C, name="gmshModelMeshSetPeriodic")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim
    INTEGER(C_INT), INTENT(INOUT) :: tags(*)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: tags_n
    INTEGER(C_INT), INTENT(INOUT) :: tagsMaster(*)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: tagsMaster_n
    REAL(C_DOUBLE), INTENT(INOUT) :: affineTransform(*)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: affineTransform_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshSetPeriodic
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get master entities `tagsMaster' for the entities of dimension `dim' and
! tags `tags'.

INTERFACE
  SUBROUTINE gmshModelMeshGetPeriodic( &
    dim, tags, tags_n, tagMaster, tagMaster_n, ierr) &
    BIND(C, name="gmshModelMeshGetPeriodic")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim
    INTEGER(C_INT), INTENT(INOUT) :: tags(*)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: tags_n
    TYPE(C_PTR), INTENT(OUT) :: tagMaster
    INTEGER(C_SIZE_T), INTENT(OUT) :: tagMaster_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshGetPeriodic
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the master entity `tagMaster', the node tags `nodeTags' and their
! corresponding master node tags `nodeTagsMaster', and the affine transform
! `affineTransform' for the entity of dimension `dim' and tag `tag'. If
! `includeHighOrderNodes' is set, include high-order nodes in the returned
! data.

INTERFACE
  SUBROUTINE gmshModelMeshGetPeriodicNodes( &
    dim, tag, tagMaster, nodeTags, nodeTags_n, nodeTagsMaster, &
    nodeTagsMaster_n, affineTransform, affineTransform_n, &
    includeHighOrderNodes, ierr) &
    BIND(C, name="gmshModelMeshGetPeriodicNodes")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: dim
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    INTEGER(C_INT), INTENT(INOUT) :: tagMaster
    TYPE(C_PTR), INTENT(OUT) :: nodeTags
    INTEGER(C_SIZE_T), INTENT(OUT) :: nodeTags_n
    TYPE(C_PTR), INTENT(OUT) :: nodeTagsMaster
    INTEGER(C_SIZE_T), INTENT(OUT) :: nodeTagsMaster_n
    TYPE(C_PTR), INTENT(OUT) :: affineTransform
    INTEGER(C_SIZE_T), INTENT(INOUT) :: affineTransform_n
    INTEGER(C_INT), VALUE, INTENT(IN) :: includeHighOrderNodes
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshGetPeriodicNodes
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the master entity `tagMaster' and the key pairs (`typeKeyMaster',
! `entityKeyMaster') corresponding to the entity `tag' and the key pairs
! (`typeKey', `entityKey') for the elements of type `elementType' and
! function space type `functionSpaceType'. If `returnCoord' is set, the
! `coord' and `coordMaster' vectors contain the x, y, z coordinates locating
! basis functions for sorting purposes.

INTERFACE
  SUBROUTINE gmshModelMeshGetPeriodicKeys( &
    elementType, functionSpaceType, tag, tagMaster, typeKeys, &
    typeKeys_n, typeKeysMaster, typeKeysMaster_n, entityKeys, &
    entityKeys_n, entityKeysMaster, entityKeysMaster_n, coord, &
    coord_n, coordMaster, coordMaster_n, returnCoord, ierr) &
    BIND(C, name="gmshModelMeshGetPeriodicKeys")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: elementType
    CHARACTER(len=1, kind=C_CHAR), INTENT(IN) :: functionSpaceType(*)
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    INTEGER(C_INT), INTENT(INOUT) :: tagMaster
    TYPE(C_PTR), INTENT(OUT) :: typeKeys
    INTEGER(C_SIZE_T), INTENT(OUT) :: typeKeys_n
    TYPE(C_PTR), INTENT(OUT) :: typeKeysMaster
    INTEGER(C_SIZE_T), INTENT(OUT) :: typeKeysMaster_n
    TYPE(C_PTR), INTENT(OUT) :: entityKeys
    INTEGER(C_SIZE_T), INTENT(OUT) :: entityKeys_n
    TYPE(C_PTR), INTENT(OUT) :: entityKeysMaster
    INTEGER(C_SIZE_T), INTENT(OUT) :: entityKeysMaster_n
    TYPE(C_PTR), INTENT(OUT) :: coord
    INTEGER(C_SIZE_T), INTENT(INOUT) :: coord_n
    TYPE(C_PTR), INTENT(OUT) :: coordMaster
    INTEGER(C_SIZE_T), INTENT(INOUT) :: coordMaster_n
    INTEGER(C_INT), VALUE, INTENT(IN) :: returnCoord
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshGetPeriodicKeys
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Import the model STL representation (if available) as the current mesh.

INTERFACE
  SUBROUTINE gmshModelMeshImportStl(ierr) &
    BIND(C, name="gmshModelMeshImportStl")
    IMPORT
    INTEGER(C_INT), OPTIONAL, INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshImportStl
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the `tags' of any duplicate nodes in the mesh of the entities
! `dimTags', given as a vector of (dim, tag) pairs. If `dimTags' is empty,
! consider the whole mesh.

INTERFACE
  SUBROUTINE gmshModelMeshGetDuplicateNodes( &
    tags, tags_n, dimTags, dimTags_n, ierr) &
    BIND(C, name="gmshModelMeshGetDuplicateNodes")
    IMPORT
    TYPE(C_PTR), INTENT(OUT) :: tags
    INTEGER(C_SIZE_T), INTENT(OUT) :: tags_n
    INTEGER(C_INT), OPTIONAL :: dimTags(*)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: dimTags_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshGetDuplicateNodes
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> Remove duplicate nodes in the mesh of the entities `dimTags', given as a
! vector of (dim, tag) pairs. If `dimTags' is empty, consider the whole mesh.

INTERFACE
  SUBROUTINE gmshModelMeshRemoveDuplicateNodes(dimTags, dimTags_n, ierr) &
    BIND(C, name="gmshModelMeshRemoveDuplicateNodes")
    IMPORT
    INTEGER(C_INT), OPTIONAL, INTENT(INOUT) :: dimTags(*)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: dimTags_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshRemoveDuplicateNodes
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Remove duplicate elements (defined by the same nodes, in the same entity)
! in the mesh of the entities `dimTags', given as a vector of (dim, tag)
! pairs. If `dimTags' is empty, consider the whole mesh.

INTERFACE
  SUBROUTINE gmshModelMeshRemoveDuplicateElements( &
    dimTags, dimTags_n, ierr) &
    BIND(C, name="gmshModelMeshRemoveDuplicateElements")
    IMPORT
    INTEGER(C_INT), OPTIONAL, INTENT(INOUT) :: dimTags(*)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: dimTags_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshRemoveDuplicateElements
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Split (into two triangles) all quadrangles in surface `tag' whose quality
! is lower than `quality'. If `tag' < 0, split quadrangles in all surfaces.

INTERFACE
  SUBROUTINE gmshModelMeshSplitQuadrangles(quality, tag, ierr) &
    BIND(C, name="gmshModelMeshSplitQuadrangles")
    IMPORT
    REAL(C_DOUBLE), VALUE, INTENT(IN) :: quality
    INTEGER(C_INT), VALUE, INTENT(IN) :: tag
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshSplitQuadrangles
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Set the visibility of the elements of tags `elementTags' to `value'.

INTERFACE
  SUBROUTINE gmshModelMeshSetVisibility( &
    elementTags, elementTags_n, VALUE, ierr) &
    BIND(C, name="gmshModelMeshSetVisibility")
    IMPORT
    INTEGER(C_SIZE_T), INTENT(INOUT) :: elementTags(*)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: elementTags_n
    INTEGER(C_INT), VALUE, INTENT(IN) :: VALUE
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshSetVisibility
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the visibility of the elements of tags `elementTags'.

INTERFACE
  SUBROUTINE gmshModelMeshGetVisibility( &
    elementTags, elementTags_n, values, values_n, ierr) &
    BIND(C, name="gmshModelMeshGetVisibility")
    IMPORT
    INTEGER(C_SIZE_T), INTENT(INOUT) :: elementTags(*)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: elementTags_n
    TYPE(C_PTR), INTENT(OUT) :: values
    INTEGER(C_SIZE_T), INTENT(OUT) :: values_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshGetVisibility
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Classify ("color") the surface mesh based on the angle threshold `angle'
! (in radians), and create new discrete surfaces, curves and points
! accordingly. If `boundary' is set, also create discrete curves on the
! boundary if the surface is open. If `forReparametrization' is set, create
! curves and surfaces that can be reparametrized using a single map. If
! `curveAngle' is less than Pi, also force curves to be split according to
! `curveAngle'. If `exportDiscrete' is set, clear any built-in CAD kernel
! entities and export the discrete entities in the built-in CAD kernel.

INTERFACE
  SUBROUTINE gmshModelMeshClassifySurfaces( &
    angle, boundary, forReparametrization, curveAngle, exportDiscrete, &
    ierr) BIND(C, name="gmshModelMeshClassifySurfaces")
    IMPORT
    REAL(C_DOUBLE), VALUE, INTENT(IN) :: angle
    INTEGER(C_INT), VALUE, INTENT(IN) :: boundary
    INTEGER(C_INT), VALUE, INTENT(IN) :: forReparametrization
    REAL(C_DOUBLE), VALUE, INTENT(IN) :: curveAngle
    INTEGER(C_INT), VALUE, INTENT(IN) :: exportDiscrete
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshClassifySurfaces
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Create a geometry for the discrete entities `dimTags' (given as a vector of
! (dim, tag) pairs) represented solely by a mesh (without an underlying CAD
! description), i.e. create a parametrization for discrete curves and
! surfaces, assuming that each can be parametrized with a single map. If
! `dimTags' is empty, create a geometry for all the discrete entities.

INTERFACE
  SUBROUTINE gmshModelMeshCreateGeometry(dimTags, dimTags_n, ierr) &
    BIND(C, name="gmshModelMeshCreateGeometry")
    IMPORT
    INTEGER(C_INT), OPTIONAL, INTENT(INOUT) :: dimTags(*)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: dimTags_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshCreateGeometry
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Create a boundary representation from the mesh if the model does not have
! one (e.g. when imported from mesh file formats with no BRep representation
! of the underlying model). If `makeSimplyConnected' is set, enforce simply
! connected discrete surfaces and volumes. If `exportDiscrete' is set, clear
! any built-in CAD kernel entities and export the discrete entities in the
! built-in CAD kernel.

INTERFACE
  SUBROUTINE gmshModelMeshCreateTopology( &
    makeSimplyConnected, exportDiscrete, ierr) &
    BIND(C, name="gmshModelMeshCreateTopology")
    IMPORT
    INTEGER(C_INT), VALUE, INTENT(IN) :: makeSimplyConnected
    INTEGER(C_INT), VALUE, INTENT(IN) :: exportDiscrete
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshCreateTopology
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Add a request to compute a basis representation for homology spaces (if
! `type' == "Homology") or cohomology spaces (if `type' == "Cohomology"). The
! computation domain is given in a list of physical group tags `domainTags';
! if empty, the whole mesh is the domain. The computation subdomain for
! relative (co)homology computation is given in a list of physical group tags
! `subdomainTags'; if empty, absolute (co)homology is computed. The
! dimensions of the (co)homology bases to be computed are given in the list
! `dim'; if empty, all bases are computed. Resulting basis representation
! (co)chains are stored as physical groups in the mesh. If the request is
! added before mesh generation, the computation will be performed at the end
! of the meshing pipeline.

INTERFACE
  SUBROUTINE gmshModelMeshAddHomologyRequest( &
    typeOpt, domainTags, domainTags_n, subdomainTags, subdomainTags_n, &
    dims, dims_n, ierr) &
    BIND(C, name="gmshModelMeshAddHomologyRequest")
    IMPORT
    CHARACTER(len=1, kind=C_CHAR), OPTIONAL, INTENT(IN) :: typeOpt(*)
    INTEGER(C_INT), OPTIONAL, INTENT(INOUT) :: domainTags(*)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: domainTags_n
    INTEGER(C_INT), OPTIONAL, INTENT(INOUT) :: subdomainTags(*)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: subdomainTags_n
    INTEGER(C_INT), OPTIONAL, INTENT(INOUT) :: dims(*)
    INTEGER(C_SIZE_T), VALUE, INTENT(IN) :: dims_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshAddHomologyRequest
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Clear all (co)homology computation requests.

INTERFACE
  SUBROUTINE gmshModelMeshClearHomologyRequests(ierr) &
    BIND(C, name="gmshModelMeshClearHomologyRequests")
    IMPORT
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshClearHomologyRequests
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Perform the (co)homology computations requested by addHomologyRequest().
! The newly created physical groups are returned in `dimTags' as a vector of
! (dim, tag) pairs.

INTERFACE
  SUBROUTINE gmshModelMeshComputeHomology(dimTags, dimTags_n, ierr) &
    BIND(C, name="gmshModelMeshComputeHomology")
    IMPORT
    TYPE(C_PTR), INTENT(OUT) :: dimTags
    INTEGER(C_SIZE_T), INTENT(OUT) :: dimTags_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshComputeHomology
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Compute a cross field for the current mesh. The function creates 3 views:
! the H function, the Theta function and cross directions. Return the tags of
! the views.

INTERFACE
  SUBROUTINE gmshModelMeshComputeCrossField( &
    viewTags, viewTags_n, ierr) &
    BIND(C, name="gmshModelMeshComputeCrossField")
    IMPORT
    TYPE(C_PTR), INTENT(OUT) :: viewTags
    INTEGER(C_SIZE_T), INTENT(OUT) :: viewTags_n
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelMeshComputeCrossField
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE GmshModelMeshInterface
