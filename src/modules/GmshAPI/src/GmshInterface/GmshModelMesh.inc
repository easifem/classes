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

! Generate a mesh of the current model, up to dimension `dim' (0, 1, 2 or 3).
!
! GMSH_API void gmshModelMeshGenerate(const int dim,
!                                     int * ierr);

INTERFACE
SUBROUTINE gmshModelMeshGenerate(dim, ierr) &
  & BIND( C, NAME="gmshModelMeshGenerate" )
  IMPORT
  _I_V_IN_ :: dim
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelMeshGenerate
END INTERFACE

PUBLIC :: gmshModelMeshGenerate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Partition the mesh of the current model into `numPart' partitions.
!
! GMSH_API void gmshModelMeshPartition(const int numPart,
!                                      int * ierr);

INTERFACE
SUBROUTINE gmshModelMeshPartition(numPart, ierr) &
  & BIND( C, NAME="gmshModelMeshPartition" )
  IMPORT
  _I_V_IN_ :: numPart
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelMeshPartition
END INTERFACE

PUBLIC :: gmshModelMeshPartition

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Unpartition the mesh of the current model.
!
! GMSH_API void gmshModelMeshUnpartition(int * ierr)

INTERFACE
SUBROUTINE gmshModelMeshUnpartition(ierr) &
  & BIND( C, NAME="gmshModelMeshUnpartition" )
  IMPORT
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelMeshUnpartition
END INTERFACE

PUBLIC :: gmshModelMeshUnpartition

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
SUBROUTINE gmshModelMeshOptimize(method, force, niter, dimTags, &
  & dimTags_n, ierr) &
  & BIND( C, NAME="gmshModelMeshOptimize" )
  IMPORT
  _CPTR_V_IN_ :: method
  _I_V_IN_ :: force, niter
  _ST_V_IN_ :: dimTags_n
  _I_IN_ :: dimTags( dimTags_n )
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelMeshOptimize
END INTERFACE

PUBLIC :: gmshModelMeshOptimize

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Recombine the mesh of the current model.
!
! GMSH_API void gmshModelMeshRecombine(int * ierr);

INTERFACE
SUBROUTINE gmshModelMeshRecombine(ierr) &
  & BIND( C, NAME="gmshModelMeshRecombine" )
  IMPORT
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelMeshRecombine
END INTERFACE

PUBLIC :: gmshModelMeshRecombine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Refine the mesh of the current model by uniformly splitting the elements.
!
! GMSH_API void gmshModelMeshRefine(int * ierr);

INTERFACE
SUBROUTINE gmshModelMeshRefine(ierr) &
  & BIND( C, NAME="gmshModelMeshRefine" )
  IMPORT
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelMeshRefine
END INTERFACE

PUBLIC :: gmshModelMeshRefine

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Set the order of the elements in the mesh of the current model to `order'.
!
! GMSH_API void gmshModelMeshSetOrder(const int order,
!                                     int * ierr);

INTERFACE
SUBROUTINE gmshModelMeshSetOrder(order, ierr) &
  & BIND( C, NAME="gmshModelMeshSetOrder" )
  IMPORT
  _I_V_IN_ :: order
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelMeshSetOrder
END INTERFACE

PUBLIC :: gmshModelMeshSetOrder

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the last entities (if any) where a meshing error occurred. Currently
! only populated by the new 3D meshing algorithms.
!
! GMSH_API void gmshModelMeshGetLastEntityError(int ** dimTags, size_t * dimTags_n,
!                                               int * ierr);

INTERFACE
SUBROUTINE gmshModelMeshGetLastEntityError(dimTags, dimTags_n, ierr) &
  & BIND( C, NAME="gmshModelMeshGetLastEntityError" )
  IMPORT
  _CPTR_IN_ :: dimTags
  _ST_OUT_ :: dimTags_n
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelMeshGetLastEntityError
END INTERFACE

PUBLIC :: gmshModelMeshGetLastEntityError

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the last nodes (if any) where a meshing error occurred. Currently only
! populated by the new 3D meshing algorithms. */
!
! GMSH_API void gmshModelMeshGetLastNodeError(size_t ** nodeTags, size_t * nodeTags_n,
!                                             int * ierr);

INTERFACE
SUBROUTINE gmshModelMeshGetLastNodeError(nodeTags, nodeTags_n, ierr) &
  & BIND( C, NAME="gmshModelMeshGetLastNodeError" )
  IMPORT
  _CPTR_IN_ :: nodeTags
  _ST_OUT_ :: nodeTags_n
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelMeshGetLastNodeError
END INTERFACE

PUBLIC :: gmshModelMeshGetLastNodeError

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
  & BIND( C, NAME="gmshModelMeshClear" )
  IMPORT
  _ST_V_IN_ :: dimTags_n
  _I_IN_ :: dimTags( dimTags_n )
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelMeshClear
END INTERFACE

PUBLIC :: gmshModelMeshClear

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
! GMSH_API void gmshModelMeshGetNodes(size_t ** nodeTags, size_t * nodeTags_n,
!                                     double ** coord, size_t * coord_n,
!                                     double ** parametricCoord, size_t * parametricCoord_n,
!                                     const int dim,
!                                     const int tag,
!                                     const int includeBoundary,
!                                     const int returnParametricCoord,
!                                     int * ierr);

INTERFACE
SUBROUTINE gmshModelMeshGetNodes(nodeTags, nodeTags_n, coord, &
  & coord_n, parametricCoord, parametricCoord_n, dim, tag, includeBoundary, &
  & returnParametricCoord, ierr) &
  & BIND( C, NAME="gmshModelMeshGetNodes" )
  IMPORT
  _CPTR_IN_ :: nodeTags, coord, parametricCoord
  _ST_OUT_ :: nodeTags_n, coord_n, parametricCoord_n
  _I_V_IN_ :: dim, tag, includeBoundary, returnParametricCoord
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelMeshGetNodes
END INTERFACE

PUBLIC :: gmshModelMeshGetNodes

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the nodes classified on the entity of tag `tag', for all the elements
! of type `elementType'. The other arguments are treated as in `getNodes'. */
!
! GMSH_API void gmshModelMeshGetNodesByElementType(const int elementType,
!                                                  size_t ** nodeTags, size_t * nodeTags_n,
!                                                  double ** coord, size_t * coord_n,
!                                                  double ** parametricCoord, size_t * parametricCoord_n,
!                                                  const int tag,
!                                                  const int returnParametricCoord,
!                                                  int * ierr);

INTERFACE
SUBROUTINE gmshModelMeshGetNodesByElementType(elementType, &
  & nodeTags, nodeTags_n, coord, &
  & coord_n, parametricCoord, parametricCoord_n, tag, &
  & returnParametricCoord, ierr) &
  & BIND( C, NAME="gmshModelMeshGetNodesByElementType" )
  IMPORT
  _CPTR_IN_ :: nodeTags, coord, parametricCoord
  _ST_OUT_ :: nodeTags_n, coord_n, parametricCoord_n
  _I_V_IN_ :: elementType, tag, returnParametricCoord
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelMeshGetNodesByElementType
END INTERFACE

PUBLIC :: gmshModelMeshGetNodesByElementType

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the coordinates and the parametric coordinates (if any) of the node
! with tag `tag'. This function relies on an internal cache (a vector in case
! of dense node numbering, a map otherwise); for large meshes accessing nodes
! in bulk is often preferable.
!
! GMSH_API void gmshModelMeshGetNode(const size_t nodeTag,
!                                    double ** coord, size_t * coord_n,
!                                    double ** parametricCoord, size_t * parametricCoord_n,
!                                    int * ierr);

INTERFACE
SUBROUTINE gmshModelMeshGetNode(nodeTag, coord, coord_n, parametricCoord,&
  & parametricCoord_n, ierr) &
  & BIND( C, NAME="gmshModelMeshGetNode" )
  IMPORT
  _ST_V_IN_ :: nodeTag
  _CPTR_IN_ :: coord, parametricCoord
  _ST_OUT_ :: coord_n, parametricCoord_n
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelMeshGetNode
END INTERFACE

PUBLIC :: gmshModelMeshGetNode

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Set the coordinates and the parametric coordinates (if any) of the node
! with tag `tag'. This function relies on an internal cache (a vector in case
! of dense node numbering, a map otherwise); for large meshes accessing nodes
! in bulk is often preferable.
!
! GMSH_API void gmshModelMeshSetNode(const size_t nodeTag,
!                                    double * coord, size_t coord_n,
!                                    double * parametricCoord, size_t parametricCoord_n,
!                                    int * ierr);

INTERFACE
SUBROUTINE gmshModelMeshSetNode(nodeTag, coord, coord_n, parametricCoord,&
  & parametricCoord_n, ierr) &
  & BIND( C, NAME="gmshModelMeshSetNode" )
  IMPORT
  _ST_V_IN_ :: nodeTag, coord_n, parametricCoord_n
  _CPTR_IN_ :: coord(coord_n), parametricCoord(parametricCoord_n)
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelMeshSetNode
END INTERFACE

PUBLIC :: gmshModelMeshSetNode

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Rebuild the node cache.
!
! GMSH_API void gmshModelMeshRebuildNodeCache(const int onlyIfNecessary,
!                                             int * ierr);

INTERFACE
SUBROUTINE gmshModelMeshRebuildNodeCache(onlyIfNecessary, ierr) &
  & BIND( C, NAME="gmshModelMeshRebuildNodeCache" )
  IMPORT
  _I_V_IN_ :: onlyIfNecessary
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelMeshRebuildNodeCache
END INTERFACE

PUBLIC :: gmshModelMeshRebuildNodeCache

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Rebuild the element cache
!
! GMSH_API void gmshModelMeshRebuildElementCache(const int onlyIfNecessary,
!                                                int * ierr);

INTERFACE
SUBROUTINE gmshModelMeshRebuildElementCache(onlyIfNecessary, ierr) &
  & BIND( C, NAME="gmshModelMeshRebuildElementCache" )
  IMPORT
  _I_V_IN_ :: onlyIfNecessary
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelMeshRebuildElementCache
END INTERFACE

PUBLIC :: gmshModelMeshRebuildElementCache

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the nodes from all the elements belonging to the physical group of
! dimension `dim' and tag `tag'. `nodeTags' contains the node tags; `coord'
! is a vector of length 3 times the length of `nodeTags' that contains the x,
! y, z coordinates of the nodes, concatenated: [n1x, n1y, n1z, n2x, ...]. */
!
! GMSH_API void gmshModelMeshGetNodesForPhysicalGroup(const int dim,
!                                                     const int tag,
!                                                     size_t ** nodeTags, size_t * nodeTags_n,
!                                                     double ** coord, size_t * coord_n,
!                                                     int * ierr);

INTERFACE
SUBROUTINE gmshModelMeshGetNodesForPhysicalGroup(dim, tag, nodeTags, &
  & nodeTags_n, coord, coord_n, ierr) &
  & BIND( C, NAME="gmshModelMeshGetNodesForPhysicalGroup" )
  IMPORT
  _I_V_IN_ :: dim, tag
  _CPTR_IN_ :: nodeTags, coord
  _ST_OUT_ :: nodeTags_n, coord_n
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelMeshGetNodesForPhysicalGroup
END INTERFACE

PUBLIC :: gmshModelMeshGetNodesForPhysicalGroup

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
!                                     const int tag,
!                                     size_t * nodeTags, size_t nodeTags_n,
!                                     double * coord, size_t coord_n,
!                                     double * parametricCoord, size_t parametricCoord_n,
!                                     int * ierr);

INTERFACE
SUBROUTINE gmshModelMeshAddNodes(dim, tag, nodeTags, &
  & nodeTags_n, coord, coord_n, parametricCoord, parametricCoord_n, ierr) &
  & BIND( C, NAME="gmshModelMeshAddNodes" )
  IMPORT
  _I_V_IN_ :: dim, tag
  _ST_V_IN_ :: nodeTags_n, coord_n, parametricCoord_n
  _ST_IN_ :: nodeTags( nodeTags_n )
  _R_IN_ :: coord(coord_n), parametricCoord(parametricCoord_n)
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelMeshAddNodes
END INTERFACE

PUBLIC :: gmshModelMeshAddNodes

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
  & BIND( C, NAME="gmshModelMeshReclassifyNodes" )
  IMPORT
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelMeshReclassifyNodes
END INTERFACE

PUBLIC :: gmshModelMeshReclassifyNodes

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Relocate the nodes classified on the entity of dimension `dim' and tag
! `tag' using their parametric coordinates. If `tag' < 0, relocate the nodes
! for all entities of dimension `dim'. If `dim' and `tag' are negative,
! relocate all the nodes in the mesh.
!
! GMSH_API void gmshModelMeshRelocateNodes(const int dim,
!                                          const int tag,
!                                          int * ierr);

INTERFACE
SUBROUTINE gmshModelMeshRelocateNodes(dim, tag, ierr) &
  & BIND( C, NAME="gmshModelMeshRelocateNodes" )
  IMPORT
  _I_V_IN_ :: dim, tag
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelMeshRelocateNodes
END INTERFACE

PUBLIC :: gmshModelMeshRelocateNodes

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
! GMSH_API void gmshModelMeshGetElements(int ** elementTypes, size_t * elementTypes_n,
!                                        size_t *** elementTags, size_t ** elementTags_n, size_t *elementTags_nn,
!                                        size_t *** nodeTags, size_t ** nodeTags_n, size_t *nodeTags_nn,
!                                        const int dim,
!                                        const int tag,
!                                        int * ierr);

INTERFACE
SUBROUTINE gmshModelMeshGetElements(elementTypes, elementTypes_n, &
  & elementTags, elementTags_n, elementTags_nn, nodeTags, nodeTags_n, &
  & nodeTags_nn, dim, tag, ierr) &
  & BIND( C, NAME="gmshModelMeshGetElements" )
  IMPORT
  _I_V_IN_  :: dim, tag
  _CPTR_IN_ :: elementTypes, elementTags, elementTags_n, nodeTags, &
    & nodeTags_n
  _ST_OUT_ :: elementTypes_n, elementTags_nn, nodeTags_nn
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelMeshGetElements
END INTERFACE

PUBLIC :: gmshModelMeshGetElements

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Get the type and node tags of the element with tag `tag'. This function
! relies on an internal cache (a vector in case of dense element numbering, a
! map otherwise); for large meshes accessing elements in bulk is often
! preferable.
!
! GMSH_API void gmshModelMeshGetElement(const size_t elementTag,
!                                       int * elementType,
!                                       size_t ** nodeTags, size_t * nodeTags_n,
!                                       int * ierr);

INTERFACE
SUBROUTINE gmshModelMeshGetElement(elementTag, elementType, nodeTags, &
  & nodeTags_n, ierr) &
  & BIND( C, NAME="gmshModelMeshGetElement" )
  IMPORT
  _ST_V_IN_ :: elementTag
  _I_OUT_ :: elementType
  _CPTR_IN_ :: nodeTags
  _ST_OUT_ :: nodeTags_n
  _I_OUT_ :: ierr
END SUBROUTINE gmshModelMeshGetElement
END INTERFACE

PUBLIC :: gmshModelMeshGetElement

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! /* Search the mesh for an element located at coordinates (`x', `y', `z'). This
!  * function performs a search in a spatial octree. If an element is found,
!  * return its tag, type and node tags, as well as the local coordinates (`u',
!  * `v', `w') within the reference element corresponding to search location. If
!  * `dim' is >= 0, only search for elements of the given dimension. If `strict'
!  * is not set, use a tolerance to find elements near the search location. */
! GMSH_API void gmshModelMeshGetElementByCoordinates(const double x,
!                                                    const double y,
!                                                    const double z,
!                                                    size_t * elementTag,
!                                                    int * elementType,
!                                                    size_t ** nodeTags, size_t * nodeTags_n,
!                                                    double * u,
!                                                    double * v,
!                                                    double * w,
!                                                    const int dim,
!                                                    const int strict,
!                                                    int * ierr);


! /* Search the mesh for element(s) located at coordinates (`x', `y', `z'). This
!  * function performs a search in a spatial octree. Return the tags of all
!  * found elements in `elementTags'. Additional information about the elements
!  * can be accessed through `getElement' and `getLocalCoordinatesInElement'. If
!  * `dim' is >= 0, only search for elements of the given dimension. If `strict'
!  * is not set, use a tolerance to find elements near the search location. */
! GMSH_API void gmshModelMeshGetElementsByCoordinates(const double x,
!                                                     const double y,
!                                                     const double z,
!                                                     size_t ** elementTags, size_t * elementTags_n,
!                                                     const int dim,
!                                                     const int strict,
!                                                     int * ierr);

! /* Return the local coordinates (`u', `v', `w') within the element
!  * `elementTag' corresponding to the model coordinates (`x', `y', `z'). This
!  * function relies on an internal cache (a vector in case of dense element
!  * numbering, a map otherwise); for large meshes accessing elements in bulk is
!  * often preferable. */
! GMSH_API void gmshModelMeshGetLocalCoordinatesInElement(const size_t elementTag,
!                                                         const double x,
!                                                         const double y,
!                                                         const double z,
!                                                         double * u,
!                                                         double * v,
!                                                         double * w,
!                                                         int * ierr);

! /* Get the types of elements in the entity of dimension `dim' and tag `tag'.
!  * If `tag' < 0, get the types for all entities of dimension `dim'. If `dim'
!  * and `tag' are negative, get all the types in the mesh. */
! GMSH_API void gmshModelMeshGetElementTypes(int ** elementTypes, size_t * elementTypes_n,
!                                            const int dim,
!                                            const int tag,
!                                            int * ierr);

! /* Return an element type given its family name `familyName' ("Point", "Line",
!  * "Triangle", "Quadrangle", "Tetrahedron", "Pyramid", "Prism", "Hexahedron")
!  * and polynomial order `order'. If `serendip' is true, return the
!  * corresponding serendip element type (element without interior nodes). */
! GMSH_API int gmshModelMeshGetElementType(const char * familyName,
!                                          const int order,
!                                          const int serendip,
!                                          int * ierr);

! /* Get the properties of an element of type `elementType': its name
!  * (`elementName'), dimension (`dim'), order (`order'), number of nodes
!  * (`numNodes'), local coordinates of the nodes in the reference element
!  * (`localNodeCoord' vector, of length `dim' times `numNodes') and number of
!  * primary (first order) nodes (`numPrimaryNodes'). */
! GMSH_API void gmshModelMeshGetElementProperties(const int elementType,
!                                                 char ** elementName,
!                                                 int * dim,
!                                                 int * order,
!                                                 int * numNodes,
!                                                 double ** localNodeCoord, size_t * localNodeCoord_n,
!                                                 int * numPrimaryNodes,
!                                                 int * ierr);

! /* Get the elements of type `elementType' classified on the entity of tag
!  * `tag'. If `tag' < 0, get the elements for all entities. `elementTags' is a
!  * vector containing the tags (unique, strictly positive identifiers) of the
!  * elements of the corresponding type. `nodeTags' is a vector of length equal
!  * to the number of elements of the given type times the number N of nodes for
!  * this type of element, that contains the node tags of all the elements of
!  * the given type, concatenated: [e1n1, e1n2, ..., e1nN, e2n1, ...]. If
!  * `numTasks' > 1, only compute and return the part of the data indexed by
!  * `task'. */
! GMSH_API void gmshModelMeshGetElementsByType(const int elementType,
!                                              size_t ** elementTags, size_t * elementTags_n,
!                                              size_t ** nodeTags, size_t * nodeTags_n,
!                                              const int tag,
!                                              const size_t task,
!                                              const size_t numTasks,
!                                              int * ierr);

! /* Preallocate data before calling `getElementsByType' with `numTasks' > 1.
!  * For C and C++ only. */
! GMSH_API void gmshModelMeshPreallocateElementsByType(const int elementType,
!                                                      const int elementTag,
!                                                      const int nodeTag,
!                                                      size_t ** elementTags, size_t * elementTags_n,
!                                                      size_t ** nodeTags, size_t * nodeTags_n,
!                                                      const int tag,
!                                                      int * ierr);

! /* Add elements classified on the entity of dimension `dim' and tag `tag'.
!  * `types' contains the MSH types of the elements (e.g. `2' for 3-node
!  * triangles: see the Gmsh reference manual). `elementTags' is a vector of the
!  * same length as `types'; each entry is a vector containing the tags (unique,
!  * strictly positive identifiers) of the elements of the corresponding type.
!  * `nodeTags' is also a vector of the same length as `types'; each entry is a
!  * vector of length equal to the number of elements of the given type times
!  * the number N of nodes per element, that contains the node tags of all the
!  * elements of the given type, concatenated: [e1n1, e1n2, ..., e1nN, e2n1,
!  * ...]. */
! GMSH_API void gmshModelMeshAddElements(const int dim,
!                                        const int tag,
!                                        int * elementTypes, size_t elementTypes_n,
!                                        const size_t ** elementTags, const size_t * elementTags_n, size_t elementTags_nn,
!                                        const size_t ** nodeTags, const size_t * nodeTags_n, size_t nodeTags_nn,
!                                        int * ierr);

! /* Add elements of type `elementType' classified on the entity of tag `tag'.
!  * `elementTags' contains the tags (unique, strictly positive identifiers) of
!  * the elements of the corresponding type. `nodeTags' is a vector of length
!  * equal to the number of elements times the number N of nodes per element,
!  * that contains the node tags of all the elements, concatenated: [e1n1, e1n2,
!  * ..., e1nN, e2n1, ...]. If the `elementTag' vector is empty, new tags are
!  * automatically assigned to the elements. */
! GMSH_API void gmshModelMeshAddElementsByType(const int tag,
!                                              const int elementType,
!                                              size_t * elementTags, size_t elementTags_n,
!                                              size_t * nodeTags, size_t nodeTags_n,
!                                              int * ierr);

! /* Get the numerical quadrature information for the given element type
!  * `elementType' and integration rule `integrationType' (e.g. "Gauss4" for a
!  * Gauss quadrature suited for integrating 4th order polynomials).
!  * `localCoord' contains the u, v, w coordinates of the G integration points
!  * in the reference element: [g1u, g1v, g1w, ..., gGu, gGv, gGw]. `weights'
!  * contains the associated weights: [g1q, ..., gGq]. */
! GMSH_API void gmshModelMeshGetIntegrationPoints(const int elementType,
!                                                 const char * integrationType,
!                                                 double ** localCoord, size_t * localCoord_n,
!                                                 double ** weights, size_t * weights_n,
!                                                 int * ierr);

! /* Get the Jacobians of all the elements of type `elementType' classified on
!  * the entity of tag `tag', at the G evaluation points `localCoord' given as
!  * concatenated triplets of coordinates in the reference element [g1u, g1v,
!  * g1w, ..., gGu, gGv, gGw]. Data is returned by element, with elements in the
!  * same order as in `getElements' and `getElementsByType'. `jacobians'
!  * contains for each element the 9 entries of the 3x3 Jacobian matrix at each
!  * evaluation point. The matrix is returned by column: [e1g1Jxu, e1g1Jyu,
!  * e1g1Jzu, e1g1Jxv, ..., e1g1Jzw, e1g2Jxu, ..., e1gGJzw, e2g1Jxu, ...], with
!  * Jxu=dx/du, Jyu=dy/du, etc. `determinants' contains for each element the
!  * determinant of the Jacobian matrix at each evaluation point: [e1g1, e1g2,
!  * ... e1gG, e2g1, ...]. `coord' contains for each element the x, y, z
!  * coordinates of the evaluation points. If `tag' < 0, get the Jacobian data
!  * for all entities. If `numTasks' > 1, only compute and return the part of
!  * the data indexed by `task'. */
! GMSH_API void gmshModelMeshGetJacobians(const int elementType,
!                                         double * localCoord, size_t localCoord_n,
!                                         double ** jacobians, size_t * jacobians_n,
!                                         double ** determinants, size_t * determinants_n,
!                                         double ** coord, size_t * coord_n,
!                                         const int tag,
!                                         const size_t task,
!                                         const size_t numTasks,
!                                         int * ierr);

! /* Preallocate data before calling `getJacobians' with `numTasks' > 1. For C
!  * and C++ only. */
! GMSH_API void gmshModelMeshPreallocateJacobians(const int elementType,
!                                                 const int numEvaluationPoints,
!                                                 const int allocateJacobians,
!                                                 const int allocateDeterminants,
!                                                 const int allocateCoord,
!                                                 double ** jacobians, size_t * jacobians_n,
!                                                 double ** determinants, size_t * determinants_n,
!                                                 double ** coord, size_t * coord_n,
!                                                 const int tag,
!                                                 int * ierr);

! /* Get the Jacobian for a single element `elementTag', at the G evaluation
!  * points `localCoord' given as concatenated triplets of coordinates in the
!  * reference element [g1u, g1v, g1w, ..., gGu, gGv, gGw]. `jacobians' contains
!  * the 9 entries of the 3x3 Jacobian matrix at each evaluation point. The
!  * matrix is returned by column: [e1g1Jxu, e1g1Jyu, e1g1Jzu, e1g1Jxv, ...,
!  * e1g1Jzw, e1g2Jxu, ..., e1gGJzw, e2g1Jxu, ...], with Jxu=dx/du, Jyu=dy/du,
!  * etc. `determinants' contains the determinant of the Jacobian matrix at each
!  * evaluation point. `coord' contains the x, y, z coordinates of the
!  * evaluation points. This function relies on an internal cache (a vector in
!  * case of dense element numbering, a map otherwise); for large meshes
!  * accessing Jacobians in bulk is often preferable. */
! GMSH_API void gmshModelMeshGetJacobian(const size_t elementTag,
!                                        double * localCoord, size_t localCoord_n,
!                                        double ** jacobians, size_t * jacobians_n,
!                                        double ** determinants, size_t * determinants_n,
!                                        double ** coord, size_t * coord_n,
!                                        int * ierr);

! /* Get the basis functions of the element of type `elementType' at the
!  * evaluation points `localCoord' (given as concatenated triplets of
!  * coordinates in the reference element [g1u, g1v, g1w, ..., gGu, gGv, gGw]),
!  * for the function space `functionSpaceType' (e.g. "Lagrange" or
!  * "GradLagrange" for Lagrange basis functions or their gradient, in the u, v,
!  * w coordinates of the reference element; or "H1Legendre3" or
!  * "GradH1Legendre3" for 3rd order hierarchical H1 Legendre functions).
!  * `numComponents' returns the number C of components of a basis function.
!  * `basisFunctions' returns the value of the N basis functions at the
!  * evaluation points, i.e. [g1f1, g1f2, ..., g1fN, g2f1, ...] when C == 1 or
!  * [g1f1u, g1f1v, g1f1w, g1f2u, ..., g1fNw, g2f1u, ...] when C == 3. For basis
!  * functions that depend on the orientation of the elements, all values for
!  * the first orientation are returned first, followed by values for the
!  * second, etc. `numOrientations' returns the overall number of orientations.
!  * If `wantedOrientations' is not empty, only return the values for the
!  * desired orientation indices. */
! GMSH_API void gmshModelMeshGetBasisFunctions(const int elementType,
!                                              double * localCoord, size_t localCoord_n,
!                                              const char * functionSpaceType,
!                                              int * numComponents,
!                                              double ** basisFunctions, size_t * basisFunctions_n,
!                                              int * numOrientations,
!                                              int * wantedOrientations, size_t wantedOrientations_n,
!                                              int * ierr);

! /* Get the orientation index of the elements of type `elementType' in the
!  * entity of tag `tag'. The arguments have the same meaning as in
!  * `getBasisFunctions'. `basisFunctionsOrientation' is a vector giving for
!  * each element the orientation index in the values returned by
!  * `getBasisFunctions'. For Lagrange basis functions the call is superfluous
!  * as it will return a vector of zeros. */
! GMSH_API void gmshModelMeshGetBasisFunctionsOrientationForElements(const int elementType,
!                                                                    const char * functionSpaceType,
!                                                                    int ** basisFunctionsOrientation, size_t * basisFunctionsOrientation_n,
!                                                                    const int tag,
!                                                                    const size_t task,
!                                                                    const size_t numTasks,
!                                                                    int * ierr);

! /* Get the orientation of a single element `elementTag'. */
! GMSH_API void gmshModelMeshGetBasisFunctionsOrientationForElement(const size_t elementTag,
!                                                                   const char * functionSpaceType,
!                                                                   int * basisFunctionsOrientation,
!                                                                   int * ierr);

! /* Get the number of possible orientations for elements of type `elementType'
!  * and function space named `functionSpaceType'. */
! GMSH_API int gmshModelMeshGetNumberOfOrientations(const int elementType,
!                                                   const char * functionSpaceType,
!                                                   int * ierr);

! /* Preallocate data before calling `getBasisFunctionsOrientationForElements'
!  * with `numTasks' > 1. For C and C++ only. */
! GMSH_API void gmshModelMeshPreallocateBasisFunctionsOrientationForElements(const int elementType,
!                                                                            int ** basisFunctionsOrientation, size_t * basisFunctionsOrientation_n,
!                                                                            const int tag,
!                                                                            int * ierr);

! /* Get the global unique mesh edge identifiers `edgeTags' and orientations
!  * `edgeOrientation' for an input list of node tag pairs defining these edges,
!  * concatenated in the vector `nodeTags'. */
! GMSH_API void gmshModelMeshGetEdges(size_t * nodeTags, size_t nodeTags_n,
!                                     size_t ** edgeTags, size_t * edgeTags_n,
!                                     int ** edgeOrientations, size_t * edgeOrientations_n,
!                                     int * ierr);

! /* Get the global unique mesh face identifiers `faceTags' and orientations
!  * `faceOrientations' for an input list of node tag triplets (if `faceType' ==
!  * 3) or quadruplets (if `faceType' == 4) defining these faces, concatenated
!  * in the vector `nodeTags'. */
! GMSH_API void gmshModelMeshGetFaces(const int faceType,
!                                     size_t * nodeTags, size_t nodeTags_n,
!                                     size_t ** faceTags, size_t * faceTags_n,
!                                     int ** faceOrientations, size_t * faceOrientations_n,
!                                     int * ierr);

! /* Create unique mesh edges for the entities `dimTags'. */
! GMSH_API void gmshModelMeshCreateEdges(int * dimTags, size_t dimTags_n,
!                                        int * ierr);

! /* Create unique mesh faces for the entities `dimTags'. */
! GMSH_API void gmshModelMeshCreateFaces(int * dimTags, size_t dimTags_n,
!                                        int * ierr);

! /* Get the local multipliers (to guarantee H(curl)-conformity) of the order 0
!  * H(curl) basis functions. Warning: this is an experimental feature and will
!  * probably change in a future release. */
! GMSH_API void gmshModelMeshGetLocalMultipliersForHcurl0(const int elementType,
!                                                         int ** localMultipliers, size_t * localMultipliers_n,
!                                                         const int tag,
!                                                         int * ierr);

! /* Generate the `keys' for the elements of type `elementType' in the entity of
!  * tag `tag', for the `functionSpaceType' function space. Each key uniquely
!  * identifies a basis function in the function space. If `returnCoord' is set,
!  * the `coord' vector contains the x, y, z coordinates locating basis
!  * functions for sorting purposes. Warning: this is an experimental feature
!  * and will probably change in a future release. */
! GMSH_API void gmshModelMeshGetKeysForElements(const int elementType,
!                                               const char * functionSpaceType,
!                                               int ** keys, size_t * keys_n,
!                                               double ** coord, size_t * coord_n,
!                                               const int tag,
!                                               const int returnCoord,
!                                               int * ierr);

! /* Get the keys for a single element `elementTag'. */
! GMSH_API void gmshModelMeshGetKeysForElement(const size_t elementTag,
!                                              const char * functionSpaceType,
!                                              int ** keys, size_t * keys_n,
!                                              double ** coord, size_t * coord_n,
!                                              const int returnCoord,
!                                              int * ierr);

! /* Get the number of keys by elements of type `elementType' for function space
!  * named `functionSpaceType'. */
! GMSH_API int gmshModelMeshGetNumberOfKeysForElements(const int elementType,
!                                                      const char * functionSpaceType,
!                                                      int * ierr);

! /* Get information about the `keys'. `infoKeys' returns information about the
!  * functions associated with the `keys'. `infoKeys[0].first' describes the
!  * type of function (0 for  vertex function, 1 for edge function, 2 for face
!  * function and 3 for bubble function). `infoKeys[0].second' gives the order
!  * of the function associated with the key. Warning: this is an experimental
!  * feature and will probably change in a future release. */
! GMSH_API void gmshModelMeshGetInformationForElements(int * keys, size_t keys_n,
!                                                      const int elementType,
!                                                      const char * functionSpaceType,
!                                                      int ** infoKeys, size_t * infoKeys_n,
!                                                      int * ierr);

! /* Get the barycenters of all elements of type `elementType' classified on the
!  * entity of tag `tag'. If `primary' is set, only the primary nodes of the
!  * elements are taken into account for the barycenter calculation. If `fast'
!  * is set, the function returns the sum of the primary node coordinates
!  * (without normalizing by the number of nodes). If `tag' < 0, get the
!  * barycenters for all entities. If `numTasks' > 1, only compute and return
!  * the part of the data indexed by `task'. */
! GMSH_API void gmshModelMeshGetBarycenters(const int elementType,
!                                           const int tag,
!                                           const int fast,
!                                           const int primary,
!                                           double ** barycenters, size_t * barycenters_n,
!                                           const size_t task,
!                                           const size_t numTasks,
!                                           int * ierr);

! /* Preallocate data before calling `getBarycenters' with `numTasks' > 1. For C
!  * and C++ only. */
! GMSH_API void gmshModelMeshPreallocateBarycenters(const int elementType,
!                                                   double ** barycenters, size_t * barycenters_n,
!                                                   const int tag,
!                                                   int * ierr);

! /* Get the nodes on the edges of all elements of type `elementType' classified
!  * on the entity of tag `tag'. `nodeTags' contains the node tags of the edges
!  * for all the elements: [e1a1n1, e1a1n2, e1a2n1, ...]. Data is returned by
!  * element, with elements in the same order as in `getElements' and
!  * `getElementsByType'. If `primary' is set, only the primary (begin/end)
!  * nodes of the edges are returned. If `tag' < 0, get the edge nodes for all
!  * entities. If `numTasks' > 1, only compute and return the part of the data
!  * indexed by `task'. */
! GMSH_API void gmshModelMeshGetElementEdgeNodes(const int elementType,
!                                                size_t ** nodeTags, size_t * nodeTags_n,
!                                                const int tag,
!                                                const int primary,
!                                                const size_t task,
!                                                const size_t numTasks,
!                                                int * ierr);

! /* Get the nodes on the faces of type `faceType' (3 for triangular faces, 4
!  * for quadrangular faces) of all elements of type `elementType' classified on
!  * the entity of tag `tag'. `nodeTags' contains the node tags of the faces for
!  * all elements: [e1f1n1, ..., e1f1nFaceType, e1f2n1, ...]. Data is returned
!  * by element, with elements in the same order as in `getElements' and
!  * `getElementsByType'. If `primary' is set, only the primary (corner) nodes
!  * of the faces are returned. If `tag' < 0, get the face nodes for all
!  * entities. If `numTasks' > 1, only compute and return the part of the data
!  * indexed by `task'. */
! GMSH_API void gmshModelMeshGetElementFaceNodes(const int elementType,
!                                                const int faceType,
!                                                size_t ** nodeTags, size_t * nodeTags_n,
!                                                const int tag,
!                                                const int primary,
!                                                const size_t task,
!                                                const size_t numTasks,
!                                                int * ierr);

! /* Get the ghost elements `elementTags' and their associated `partitions'
!  * stored in the ghost entity of dimension `dim' and tag `tag'. */
! GMSH_API void gmshModelMeshGetGhostElements(const int dim,
!                                             const int tag,
!                                             size_t ** elementTags, size_t * elementTags_n,
!                                             int ** partitions, size_t * partitions_n,
!                                             int * ierr);

! /* Set a mesh size constraint on the model entities `dimTags'. Currently only
!  * entities of dimension 0 (points) are handled. */
! GMSH_API void gmshModelMeshSetSize(int * dimTags, size_t dimTags_n,
!                                    const double size,
!                                    int * ierr);

! /* Set mesh size constraints at the given parametric points `parametricCoord'
!  * on the model entity of dimension `dim' and tag `tag'. Currently only
!  * entities of dimension 1 (lines) are handled. */
! GMSH_API void gmshModelMeshSetSizeAtParametricPoints(const int dim,
!                                                      const int tag,
!                                                      double * parametricCoord, size_t parametricCoord_n,
!                                                      double * sizes, size_t sizes_n,
!                                                      int * ierr);

! /* Set a global mesh size callback. The callback should take 5 arguments
!  * (`dim', `tag', `x', `y' and `z') and return the value of the mesh size at
!  * coordinates (`x', `y', `z'). */
! GMSH_API void gmshModelMeshSetSizeCallback(double (*callback)(int dim, int tag, double x, double y, double z, void * data), void * callback_data,
!                                            int * ierr);

! /* Remove the global mesh size callback. */
! GMSH_API void gmshModelMeshRemoveSizeCallback(int * ierr);

! /* Set a transfinite meshing constraint on the curve `tag', with `numNodes'
!  * nodes distributed according to `meshType' and `coef'. Currently supported
!  * types are "Progression" (geometrical progression with power `coef'), "Bump"
!  * (refinement toward both extremities of the curve) and "Beta" (beta law). */
! GMSH_API void gmshModelMeshSetTransfiniteCurve(const int tag,
!                                                const int numNodes,
!                                                const char * meshType,
!                                                const double coef,
!                                                int * ierr);

! /* Set a transfinite meshing constraint on the surface `tag'. `arrangement'
!  * describes the arrangement of the triangles when the surface is not flagged
!  * as recombined: currently supported values are "Left", "Right",
!  * "AlternateLeft" and "AlternateRight". `cornerTags' can be used to specify
!  * the (3 or 4) corners of the transfinite interpolation explicitly;
!  * specifying the corners explicitly is mandatory if the surface has more that
!  * 3 or 4 points on its boundary. */
! GMSH_API void gmshModelMeshSetTransfiniteSurface(const int tag,
!                                                  const char * arrangement,
!                                                  int * cornerTags, size_t cornerTags_n,
!                                                  int * ierr);

! /* Set a transfinite meshing constraint on the surface `tag'. `cornerTags' can
!  * be used to specify the (6 or 8) corners of the transfinite interpolation
!  * explicitly. */
! GMSH_API void gmshModelMeshSetTransfiniteVolume(const int tag,
!                                                 int * cornerTags, size_t cornerTags_n,
!                                                 int * ierr);

! /* Set transfinite meshing constraints on the model entities in `dimTag'.
!  * Transfinite meshing constraints are added to the curves of the quadrangular
!  * surfaces and to the faces of 6-sided volumes. Quadragular faces with a
!  * corner angle superior to `cornerAngle' (in radians) are ignored. The number
!  * of points is automatically determined from the sizing constraints. If
!  * `dimTag' is empty, the constraints are applied to all entities in the
!  * model. If `recombine' is true, the recombine flag is automatically set on
!  * the transfinite surfaces. */
! GMSH_API void gmshModelMeshSetTransfiniteAutomatic(int * dimTags, size_t dimTags_n,
!                                                    const double cornerAngle,
!                                                    const int recombine,
!                                                    int * ierr);

! /* Set a recombination meshing constraint on the model entity of dimension
!  * `dim' and tag `tag'. Currently only entities of dimension 2 (to recombine
!  * triangles into quadrangles) are supported. */
! GMSH_API void gmshModelMeshSetRecombine(const int dim,
!                                         const int tag,
!                                         int * ierr);

! /* Set a smoothing meshing constraint on the model entity of dimension `dim'
!  * and tag `tag'. `val' iterations of a Laplace smoother are applied. */
! GMSH_API void gmshModelMeshSetSmoothing(const int dim,
!                                         const int tag,
!                                         const int val,
!                                         int * ierr);

! /* Set a reverse meshing constraint on the model entity of dimension `dim' and
!  * tag `tag'. If `val' is true, the mesh orientation will be reversed with
!  * respect to the natural mesh orientation (i.e. the orientation consistent
!  * with the orientation of the geometry). If `val' is false, the mesh is left
!  * as-is. */
! GMSH_API void gmshModelMeshSetReverse(const int dim,
!                                       const int tag,
!                                       const int val,
!                                       int * ierr);

! /* Set the meshing algorithm on the model entity of dimension `dim' and tag
!  * `tag'. Currently only supported for `dim' == 2. */
! GMSH_API void gmshModelMeshSetAlgorithm(const int dim,
!                                         const int tag,
!                                         const int val,
!                                         int * ierr);

! /* Force the mesh size to be extended from the boundary, or not, for the model
!  * entity of dimension `dim' and tag `tag'. Currently only supported for `dim'
!  * == 2. */
! GMSH_API void gmshModelMeshSetSizeFromBoundary(const int dim,
!                                                const int tag,
!                                                const int val,
!                                                int * ierr);

! /* Set a compound meshing constraint on the model entities of dimension `dim'
!  * and tags `tags'. During meshing, compound entities are treated as a single
!  * discrete entity, which is automatically reparametrized. */
! GMSH_API void gmshModelMeshSetCompound(const int dim,
!                                        int * tags, size_t tags_n,
!                                        int * ierr);

! /* Set meshing constraints on the bounding surfaces of the volume of tag `tag'
!  * so that all surfaces are oriented with outward pointing normals. Currently
!  * only available with the OpenCASCADE kernel, as it relies on the STL
!  * triangulation. */
! GMSH_API void gmshModelMeshSetOutwardOrientation(const int tag,
!                                                  int * ierr);

! /* Remove all meshing constraints from the model entities `dimTags'. If
!  * `dimTags' is empty, remove all constraings. */
! GMSH_API void gmshModelMeshRemoveConstraints(int * dimTags, size_t dimTags_n,
!                                              int * ierr);

! /* Embed the model entities of dimension `dim' and tags `tags' in the
!  * (`inDim', `inTag') model entity. The dimension `dim' can 0, 1 or 2 and must
!  * be strictly smaller than `inDim', which must be either 2 or 3. The embedded
!  * entities should not intersect each other or be part of the boundary of the
!  * entity `inTag', whose mesh will conform to the mesh of the embedded
!  * entities. With the OpenCASCADE kernel, if the `fragment' operation is
!  * applied to entities of different dimensions, the lower dimensional entities
!  * will be automatically embedded in the higher dimensional entities if they
!  * are not on their boundary. */
! GMSH_API void gmshModelMeshEmbed(const int dim,
!                                  int * tags, size_t tags_n,
!                                  const int inDim,
!                                  const int inTag,
!                                  int * ierr);

! /* Remove embedded entities from the model entities `dimTags'. if `dim' is >=
!  * 0, only remove embedded entities of the given dimension (e.g. embedded
!  * points if `dim' == 0). */
! GMSH_API void gmshModelMeshRemoveEmbedded(int * dimTags, size_t dimTags_n,
!                                           const int dim,
!                                           int * ierr);

! /* Get the entities (if any) embedded in the model entity of dimension `dim'
!  * and tag `tag'. */
! GMSH_API void gmshModelMeshGetEmbedded(const int dim,
!                                        const int tag,
!                                        int ** dimTags, size_t * dimTags_n,
!                                        int * ierr);

! /* Reorder the elements of type `elementType' classified on the entity of tag
!  * `tag' according to `ordering'. */
! GMSH_API void gmshModelMeshReorderElements(const int elementType,
!                                            const int tag,
!                                            size_t * ordering, size_t ordering_n,
!                                            int * ierr);

! /* Renumber the node tags in a continuous sequence. */
! GMSH_API void gmshModelMeshRenumberNodes(int * ierr);

! /* Renumber the element tags in a continuous sequence. */
! GMSH_API void gmshModelMeshRenumberElements(int * ierr);

! /* Set the meshes of the entities of dimension `dim' and tag `tags' as
!  * periodic copies of the meshes of entities `tagsMaster', using the affine
!  * transformation specified in `affineTransformation' (16 entries of a 4x4
!  * matrix, by row). If used after meshing, generate the periodic node
!  * correspondence information assuming the meshes of entities `tags'
!  * effectively match the meshes of entities `tagsMaster' (useful for
!  * structured and extruded meshes). Currently only available for @code{dim} ==
!  * 1 and @code{dim} == 2. */
! GMSH_API void gmshModelMeshSetPeriodic(const int dim,
!                                        int * tags, size_t tags_n,
!                                        int * tagsMaster, size_t tagsMaster_n,
!                                        double * affineTransform, size_t affineTransform_n,
!                                        int * ierr);

! /* Get the master entity `tagMaster', the node tags `nodeTags' and their
!  * corresponding master node tags `nodeTagsMaster', and the affine transform
!  * `affineTransform' for the entity of dimension `dim' and tag `tag'. If
!  * `includeHighOrderNodes' is set, include high-order nodes in the returned
!  * data. */
! GMSH_API void gmshModelMeshGetPeriodicNodes(const int dim,
!                                             const int tag,
!                                             int * tagMaster,
!                                             size_t ** nodeTags, size_t * nodeTags_n,
!                                             size_t ** nodeTagsMaster, size_t * nodeTagsMaster_n,
!                                             double ** affineTransform, size_t * affineTransform_n,
!                                             const int includeHighOrderNodes,
!                                             int * ierr);

! /* Remove duplicate nodes in the mesh of the current model. */
! GMSH_API void gmshModelMeshRemoveDuplicateNodes(int * ierr);

! /* Split (into two triangles) all quadrangles in surface `tag' whose quality
!  * is lower than `quality'. If `tag' < 0, split quadrangles in all surfaces. */
! GMSH_API void gmshModelMeshSplitQuadrangles(const double quality,
!                                             const int tag,
!                                             int * ierr);

! /* Classify ("color") the surface mesh based on the angle threshold `angle'
!  * (in radians), and create new discrete surfaces, curves and points
!  * accordingly. If `boundary' is set, also create discrete curves on the
!  * boundary if the surface is open. If `forReparametrization' is set, create
!  * edges and surfaces that can be reparametrized using a single map. If
!  * `curveAngle' is less than Pi, also force curves to be split according to
!  * `curveAngle'. If `exportDiscrete' is set, clear any built-in CAD kernel
!  * entities and export the discrete entities in the built-in CAD kernel. */
! GMSH_API void gmshModelMeshClassifySurfaces(const double angle,
!                                             const int boundary,
!                                             const int forReparametrization,
!                                             const double curveAngle,
!                                             const int exportDiscrete,
!                                             int * ierr);

! /* Create a geometry for the discrete entities `dimTags' (represented solely
!  * by a mesh, without an underlying CAD description), i.e. create a
!  * parametrization for discrete curves and surfaces, assuming that each can be
!  * parametrized with a single map. If `dimTags' is empty, create a geometry
!  * for all the discrete entities. */
! GMSH_API void gmshModelMeshCreateGeometry(int * dimTags, size_t dimTags_n,
!                                           int * ierr);

! /* Create a boundary representation from the mesh if the model does not have
!  * one (e.g. when imported from mesh file formats with no BRep representation
!  * of the underlying model). If `makeSimplyConnected' is set, enforce simply
!  * connected discrete surfaces and volumes. If `exportDiscrete' is set, clear
!  * any built-in CAD kernel entities and export the discrete entities in the
!  * built-in CAD kernel. */
! GMSH_API void gmshModelMeshCreateTopology(const int makeSimplyConnected,
!                                           const int exportDiscrete,
!                                           int * ierr);

! /* Compute a basis representation for homology spaces after a mesh has been
!  * generated. The computation domain is given in a list of physical group tags
!  * `domainTags'; if empty, the whole mesh is the domain. The computation
!  * subdomain for relative homology computation is given in a list of physical
!  * group tags `subdomainTags'; if empty, absolute homology is computed. The
!  * dimensions homology bases to be computed are given in the list `dim'; if
!  * empty, all bases are computed. Resulting basis representation chains are
!  * stored as physical groups in the mesh. */
! GMSH_API void gmshModelMeshComputeHomology(int * domainTags, size_t domainTags_n,
!                                            int * subdomainTags, size_t subdomainTags_n,
!                                            int * dims, size_t dims_n,
!                                            int * ierr);

! /* Compute a basis representation for cohomology spaces after a mesh has been
!  * generated. The computation domain is given in a list of physical group tags
!  * `domainTags'; if empty, the whole mesh is the domain. The computation
!  * subdomain for relative cohomology computation is given in a list of
!  * physical group tags `subdomainTags'; if empty, absolute cohomology is
!  * computed. The dimensions homology bases to be computed are given in the
!  * list `dim'; if empty, all bases are computed. Resulting basis
!  * representation cochains are stored as physical groups in the mesh. */
! GMSH_API void gmshModelMeshComputeCohomology(int * domainTags, size_t domainTags_n,
!                                              int * subdomainTags, size_t subdomainTags_n,
!                                              int * dims, size_t dims_n,
!                                              int * ierr);

! /* Compute a cross field for the current mesh. The function creates 3 views:
!  * the H function, the Theta function and cross directions. Return the tags of
!  * the views. */
! GMSH_API void gmshModelMeshComputeCrossField(int ** viewTags, size_t * viewTags_n,
!                                              int * ierr);

! /* Triangulate the points given in the `coord' vector as pairs of u, v
!  * coordinates, and return the node tags (with numbering starting at 1) of the
!  * resulting triangles in `tri'. */
! GMSH_API void gmshModelMeshTriangulate(double * coord, size_t coord_n,
!                                        size_t ** tri, size_t * tri_n,
!                                        int * ierr);

! /* Tetrahedralize the points given in the `coord' vector as triplets of x, y,
!  * z coordinates, and return the node tags (with numbering starting at 1) of
!  * the resulting tetrahedra in `tetra'. */
! GMSH_API void gmshModelMeshTetrahedralize(double * coord, size_t coord_n,
!                                           size_t ** tetra, size_t * tetra_n,
!                                           int * ierr);