! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
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

MODULE AbstractMesh_Class
USE GlobalData, ONLY: LGT, I4B, DFP

USE Files, ONLY: HDF5File_, VTKFile_

USE Basetype, ONLY: BoundingBox_, CSRMatrix_

USE ExceptionHandler_Class, ONLY: e

USE CPUTime_Class, ONLY: CPUTime_

USE ElemData_Class, ONLY: ElemData_, ElemDataPointer_

USE ElemDataBinaryTree_Class, ONLY: ElemDataBinaryTree_

USE ElemDataList_Class, ONLY: ElemDataList_

USE NodeData_Class, ONLY: NodeData_, NodeDataPointer_

USE NodeDataList_Class, ONLY: NodeDataList_

USE NodeDataBinaryTree_Class, ONLY: NodeDataBinaryTree_

USE FacetData_Class, ONLY: FacetData_

USE AbstractMeshParam, ONLY: PARAM_MAX_NODE_TO_NODE, &
                             PARAM_MAX_NODE_TO_ELEM, &
                             PARAM_MAX_CONNECTIVITY_SIZE, &
                             PARAM_MAX_NNE

USE Kdtree2_Module, ONLY: Kdtree2_, Kdtree2Result_

IMPLICIT NONE

PRIVATE

PUBLIC :: AbstractMesh_
PUBLIC :: AbstractMeshPointer_
PUBLIC :: AbstractMeshDeallocate
PUBLIC :: AbstractMeshDisplay
PUBLIC :: AbstractMeshGetParam
PUBLIC :: AbstractMeshImport
PUBLIC :: AbstractMeshGetFacetConnectivity
PUBLIC :: AbstractMeshPointerDeallocate
PUBLIC :: PARAM_MAX_NODE_TO_NODE
PUBLIC :: PARAM_MAX_NODE_TO_ELEM
PUBLIC :: PARAM_MAX_CONNECTIVITY_SIZE
PUBLIC :: PARAM_MAX_NNE

CHARACTER(*), PARAMETER :: modName = "AbstractMesh_Class"

!----------------------------------------------------------------------------
!                                                             AbstractMesh_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary:  Abstract class for mesh

TYPE, ABSTRACT :: AbstractMesh_
  PRIVATE
  LOGICAL(LGT) :: isInitiated = .FALSE.
  !! logical flag denoting for whether mesh data is Initiated or not
  LOGICAL(LGT) :: showTime = .FALSE.
  !! If true, then we show the time taken by various mesh operations
  !! This is for checking the performance of a subclass
  LOGICAL(LGT) :: readFromFile = .TRUE.
  !! True if the mesh is read from a file
  LOGICAL(LGT) :: isNodeToElementsInitiated = .FALSE.
  !! Node to elements mapping
  LOGICAL(LGT) :: isNodeToNodesInitiated = .FALSE.
  !! Node to nodes mapping
  LOGICAL(LGT) :: isExtraNodeToNodesInitiated = .FALSE.
  !! Node to nodes mapping
  LOGICAL(LGT) :: isElementToElementsInitiated = .FALSE.
  !! Element to elements mapping
  LOGICAL(LGT) :: isEdgeConnectivityInitiated = .FALSE.
  !! This is Set to true when edge connectivity is initiated
  !! See InitiateEdgeConnectivity method
  LOGICAL(LGT) :: isFaceConnectivityInitiated = .FALSE.
  !! This is Set to true when face connectivity is initiated
  !! See InitiateFaceConnectivity method
  LOGICAL(LGT) :: isBoundaryDataInitiated = .FALSE.
  !! Boundary data
  LOGICAL(LGT) :: isFacetDataInitiated = .FALSE.
  !! FacetData
  INTEGER(I4B) :: uid = 0
  !! Unique id of the mesh
  !! In case of Mesh_ it is entityNumber of the mesh
  INTEGER(I4B) :: tElements_topology_wise(8) = 0
  !! point, line, triangle, quadrangle, tetrahedron, hexahedron, prism,
  !! pyramid (it is calculated in the postprocessing step)
  INTEGER(I4B) :: tElemTopologies = 0, elemTopologies(8) = 0
  !! total element topologies, name of element topologies are stored in
  !! elemTopologies(1:tElemTopologies)
  !! this info is computed in a postprocessing step
  INTEGER(I4B) :: maxNNE = 0
  !! maximum number of nodes in element
  INTEGER(I4B) :: nsd = 0
  !! number of spatial dimension of the mesh
  INTEGER(I4B) :: xidim = 0
  !! xidimension of elements present inside the mesh
  !! for point xidim = 0
  !! for line/curve xidim = 1
  !! for surface xidim = 2
  !! for volume xidim = 3
  INTEGER(I4B) :: maxNptrs = 0
  !! largest node number present inside the mesh
  INTEGER(I4B) :: minNptrs = 0
  !! minimum node number present inside the mesh
  INTEGER(I4B) :: maxElemNum = 0
  !! largest element number present inside the mesh
  INTEGER(I4B) :: minElemNum = 0
  !! minimum element number present inside the mesh
  INTEGER(I4B) :: tNodes = 0
  !! total number of nodes present inside the mesh
  INTEGER(I4B) :: tEdges = 0
  !! total number of internal nodes inside the mesh
  INTEGER(I4B) :: tFaces = 0
  !! total number of internal nodes inside the mesh
  INTEGER(I4B) :: tElements = 0
  !! total number of elements present inside the mesh
  !! It is the size of elemNumber vector
  REAL(DFP) :: minX = 0.0
  !! minimum value of x coordinate
  REAL(DFP) :: maxX = 0.0
  !! maximum value of x coordinate
  REAL(DFP) :: minY = 0.0
  !! minimum value of y coordinate
  REAL(DFP) :: maxY = 0.0
  !! maximum value of y coordinate
  REAL(DFP) :: minZ = 0.0
  !! minimum value of z coordinate
  REAL(DFP) :: maxZ = 0.0
  !! maximum value of z coordinate
  REAL(DFP) :: x = 0.0
  !! x coorindate of centroid
  REAL(DFP) :: y = 0.0
  !! y coordinate of centroid
  REAL(DFP) :: z = 0.0
  !! z coordinate of centroid

  INTEGER(I4B), ALLOCATABLE :: boundingEntity(:)
  !! Bounding entity numbers of the current entity

  INTEGER(I4B), ALLOCATABLE :: local_elemNumber(:)
  !! List of local element numbers, the lowerbound is `minElemNum`
  !! and upper bound is `maxElemNum`. In this way, local_elemNumber(iel)
  !! returns the local element number of global element number iel.

  INTEGER(I4B), ALLOCATABLE :: local_Nptrs(:)
  !! Returns local node number from a global node number
  !! Its length is from 1 to maxNptrs
  !! Helpul in finding if a global node is present inside the mesh or not

  REAL(DFP), ALLOCATABLE :: quality(:, :)
  !! number of rows are meshquality
  !! number of columns are elements

  INTEGER(I4B), ALLOCATABLE :: facetElementType(:, :)
  !! Number of rows of this array is same as the total number of
  !! facets present in the mesh-reference elements
  !! Number of columns of this array is equal to the total number of
  !! elements inside the mesh
  !! facetElementType(ii, iel) can be
  !! INTERNAL_ELEMENT, BOUNDARY_ELEMENT, DOMAIN_BOUNDARY_ELEMENT
  !! If the face is a part of the mesh boundary then it will be called
  !! the BOUNDARY_ELEMENT

  TYPE(NodeDataPointer_), ALLOCATABLE :: nodeData(:)
  !! Node data

  TYPE(ElemDataPointer_), ALLOCATABLE :: elementData(:)
  !! element data

  TYPE(FacetData_), ALLOCATABLE :: facetData(:)
  !! facet data

  TYPE(ElemDataList_) :: elementDataList
  !! ElemData list

  TYPE(ElemDataBinaryTree_) :: elementDataBinaryTree
  !! ElemData binary tree

  TYPE(NodeDataList_) :: nodeDataList
  !! NodeData list

  TYPE(NodeDataBinaryTree_) :: nodeDataBinaryTree
  !! NodeData binary tree

  TYPE(Kdtree2_), POINTER :: kdtree => NULL()
  !!

  TYPE(Kdtree2Result_), ALLOCATABLE :: kdresult(:)

CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods

  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  !!  Read the mesh by reading a hdf5 file
  !! The hdf5 file format depends upon the mesh engine

  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Deallocate memory occupied by the mesh instance

  PROCEDURE, PUBLIC, PASS(obj) :: isEmpty => obj_isEmpty
  !! Returns true if the mesh is empty.

  PROCEDURE, PUBLIC, PASS(obj) :: InitiateDynamicDataStructure => &
    obj_InitiateDynamicDataStructure
  !! Initiate DynamicDataStructure of mesh from static data

  PROCEDURE, PUBLIC, PASS(obj) :: DeallocateKdtree => obj_DeallocateKdtree
  !! Deallocate the kdtree

  ! Set:
  ! @NodeDataMethods

  PROCEDURE, PUBLIC, PASS(obj) :: InitiateKdtree => obj_InitiateKdtree
  !! Initiate Kdtree

  PROCEDURE, PUBLIC, PASS(obj) :: InitiateNodeToElements => &
    obj_InitiateNodeToElements
  !! Initiate node to element data (mapping)

  PROCEDURE, PUBLIC, PASS(obj) :: InitiateNodeToNodes => &
    obj_InitiateNodetoNodes
  !! Initiate Node to nodes data

  PROCEDURE, PUBLIC, PASS(obj) :: InitiateExtraNodeToNodes => &
    obj_InitiateExtraNodetoNodes
  !! Initiate Node to nodes mapping (used in jump based FEM)

  ! Set:
  ! @ElementDataMethods

  PROCEDURE, PUBLIC, PASS(obj) :: InitiateElementToElements => &
    obj_InitiateElementToElements
  !! Initiate element to elements mapping

  ! Set:
  ! @BoundaryDataMethods
  PROCEDURE, PUBLIC, PASS(obj) :: InitiateBoundaryData => &
    obj_InitiateBoundaryData
  !! Initiate the boundary data

  ! Set:
  ! @EdgeDataMethods
  PROCEDURE, PUBLIC, PASS(obj) :: InitiateEdgeConnectivity => &
    obj_InitiateEdgeConnectivity

  ! Set:
  ! @FaceDataMethods
  PROCEDURE, PUBLIC, PASS(obj) :: InitiateFaceConnectivity => &
    obj_InitiateFaceConnectivity

  ! Set:
  ! @FacetDataMethods
  PROCEDURE, PUBLIC, PASS(obj) :: InitiateFacetElements => &
    obj_InitiateFacetElements
  !! Initiate boundary data

  ! GET:
  ! @GetMethods

  PROCEDURE, PUBLIC, PASS(obj) :: GetElemType => obj_GetElemType
  !! Get the element name

  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalElementsTopologyWise => &
    obj_GetTotalElementsTopologyWise
  !! get total elements topology wise

  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalTopology => obj_GetTotalTopology
  !! Get total topology

  PROCEDURE, PASS(obj) :: GetElemTopology1 => obj_GetElemTopology1
  !! Get all the unique element topology stored in the mesh
  PROCEDURE, PASS(obj) :: GetElemTopology2 => obj_GetElemTopology2
  !! Get the name element topology name of an element
  GENERIC, PUBLIC :: GetElemTopology => GetElemTopology1, GetElemTopology2
  !! Generic method to get the element topology name

  PROCEDURE, PUBLIC, PASS(obj) :: GetElemTopologyIndx => &
    obj_GetElemTopologyIndx
  !! Get the index of element topology

  PROCEDURE, PUBLIC, PASS(obj) :: GetElemData => obj_GetElemData
  !! Get the element data

  PROCEDURE, PUBLIC, PASS(obj) :: GetElemDataPointer => &
    obj_GetElemDataPointer
  !! Get pointer to an element data

  PROCEDURE, PUBLIC, PASS(obj) :: GetNNE => obj_GetNNE
  !! Get number of nodes in an element

  PROCEDURE, PUBLIC, PASS(obj) :: GetMaxNNE => obj_GetMaxNNE
  !! Get maximum number of nodes in an element

  PROCEDURE, PUBLIC, PASS(obj) :: Size => obj_size
  !! Returns the size of the mesh (total number of elements)

  PROCEDURE, PASS(obj) :: GetElemNum1 => obj_GetElemNum1
  !! Returns global element number in the mesh
  PROCEDURE, PASS(obj) :: GetElemNum2 => obj_GetElemNum2
  !! Returne global or local element number in mesh with meshid
  GENERIC, PUBLIC :: GetElemNum => GetElemNum1, GetElemNum2
  !! Generic method to get list of local or global element number in mesh

  PROCEDURE, PASS(obj) :: GetElemNum1_ => obj_GetElemNum1_
  !! Returns global element number in the mesh
  PROCEDURE, PASS(obj) :: GetElemNum2_ => obj_GetElemNum2_
  !! Returne global or local element number in mesh with meshid
  PROCEDURE, PASS(obj) :: GetElemNum3_ => obj_GetElemNum3_
  !! Returne global or local element number in mesh with meshid
  GENERIC, PUBLIC :: GetElemNum_ => GetElemNum1_, GetElemNum2_, GetElemNum3_
  !! Generic method to get list of local or global element number in mesh

  PROCEDURE, PUBLIC, PASS(obj) :: GetBoundingEntity => obj_GetBoundingEntity
  !! Returns the nodal coordinates

  PROCEDURE, PASS(obj) :: GetNptrs1 => obj_GetNptrs1
  !! Returns the node number of mesh
  PROCEDURE, PASS(obj) :: GetNptrs2 => obj_GetNptrs2
  !! Get node number of mesh of meshid
  GENERIC, PUBLIC :: GetNptrs => GetNptrs1, GetNptrs2

  PROCEDURE, PASS(obj) :: GetNptrs1_ => obj_GetNptrs1_
  !! This is a subroutine which returns the node number of mesh
  PROCEDURE, PASS(obj) :: GetNptrs2_ => obj_GetNptrs2_
  !! Get node number of mesh of given meshid
  PROCEDURE, PASS(obj) :: GetNptrs3_ => obj_GetNptrs3_
  !! Get node number of several elements

  GENERIC, PUBLIC :: GetNptrs_ => GetNptrs1_, GetNptrs2_, GetNptrs3_
  !! Get node number of mesh

  PROCEDURE, PUBLIC, PASS(obj) :: GetNptrsInBox => obj_GetNptrsInBox
  !! Get node number in a box

  PROCEDURE, PUBLIC, PASS(obj) :: GetNptrsInBox_ => obj_GetNptrsInBox_
  !! Get node number in a box without allocation

  PROCEDURE, PUBLIC, PASS(obj) :: GetInternalNptrs => obj_GetInternalNptrs
  !! Returns a vector of internal node numbers

  PROCEDURE, PUBLIC, PASS(obj) :: GetInternalNptrs_ => obj_GetInternalNptrs_
  !! Returns a vector of internal node numbers
  !! subroutine version (no allocation)

  PROCEDURE, PUBLIC, PASS(obj) :: GetBoundaryNptrs => obj_GetBoundaryNptrs
  !! Returns a vector of boundary node numbers

  PROCEDURE, PUBLIC, PASS(obj) :: isBoundaryNode => obj_isBoundaryNode
  !! Returns true if a given global node number is a boundary node

  PROCEDURE, PASS(obj) :: isNodePresent1 => obj_isNodePresent1
  !! Returns true if a node number is present
  PROCEDURE, PASS(obj) :: isNodePresent2 => obj_isNodePresent2
  !! Returns true if a node number is present
  GENERIC, PUBLIC :: isNodePresent => isNodePresent1, isNodePresent2

  PROCEDURE, PUBLIC, PASS(obj) :: GetNodeMask => obj_GetNodeMask
  !! returns the mask for the present of node

  PROCEDURE, PUBLIC, PASS(obj) :: isAnyNodePresent => obj_isAnyNodePresent
  !! Returns true if any of the node number is present

  PROCEDURE, PUBLIC, PASS(obj) :: isAllNodePresent => obj_isAllNodePresent
  !! Returns true if All of the node number is present

  PROCEDURE, PUBLIC, PASS(obj) :: isElementPresent => obj_isElementPresent
  !! Returns true if a given element number is present

  PROCEDURE, PUBLIC, PASS(obj) :: isBoundaryElement => obj_isBoundaryElement
  !! Returns true if a given global element number is a boundary element

  PROCEDURE, PUBLIC, PASS(obj) :: isDomainBoundaryElement => &
    obj_isDomainBoundaryElement
  !! Returns true if a given global element number is a boundary element

  PROCEDURE, PUBLIC, PASS(obj) :: isDomainFacetElement => &
    obj_isDomainFacetElement
  !! Returns true if a given global element number is a boundary element

  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalInternalNodes => &
    obj_GetTotalInternalNodes
  !! Returns the total number of internal nodes

  PROCEDURE, PASS(obj) :: GetTotalNodes1 => obj_GetTotalNodes1
  !! Returns the total number of nodes
  PROCEDURE, PASS(obj) :: GetTotalNodes2 => obj_GetTotalNodes2
  !! Returns total nodes of meshid
  PROCEDURE, PASS(obj) :: GetTotalNodes3 => obj_GetTotalNodes3
  !! Returns total nodes from a list of element number
  GENERIC, PUBLIC :: GetTotalNodes => GetTotalNodes1, GetTotalNodes2, &
    GetTotalNodes3
  !! get total nodes

  PROCEDURE, PASS(obj) :: GetTotalVertexNodes1 => obj_GetTotalVertexNodes1
  !! Returns the total number of vertex nodes
  PROCEDURE, PASS(obj) :: GetTotalVertexNodes2 => obj_GetTotalVertexNodes2
  !! Returns total vertex of meshid
  PROCEDURE, PASS(obj) :: GetTotalVertexNodes3 => obj_GetTotalVertexNodes3
  !! Returns total vertex from a list of element number
  PROCEDURE, PASS(obj) :: GetTotalVertexNodes4 => obj_GetTotalVertexNodes4
  !! Return total vertex nodes of an element
  GENERIC, PUBLIC :: GetTotalVertexNodes => GetTotalVertexNodes1, &
    GetTotalVertexNodes2, GetTotalVertexNodes3, GetTotalVertexNodes4
  !! get total vetex nodes
  !! vetex nodes means the nodes in linear mesh. When
  !! mesh is higher order, then TotalVertexNodes wiill
  !! be less than TotalNodes

  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalFaces => obj_GetTotalFaces
  !! Returns the total number of faces in the mesh (obj%tFaces)

  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalEdges => obj_GetTotalEdges
  !! Returns the total number of edges in the mesh (obj%tEdges)

  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalCells => obj_size
  !! Returns the total number of cells in the mesh (obj%tElements)

  PROCEDURE, PASS(obj) :: GetTotalElements1 => obj_size
  !! Returns the size of the mesh

  PROCEDURE, PASS(obj) :: GetTotalElements2 => obj_GetTotalElements2
  !! Returns total number of elements of given meshid

  PROCEDURE, PASS(obj) :: GetTotalElements3 => obj_GetTotalElements3
  !! Returns total number of elements of given meshid

  GENERIC, PUBLIC :: GetTotalElements => GetTotalElements1, &
    GetTotalElements2, GetTotalElements3
  !! Generic method for getting the total number of elements

  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalBoundaryElements => &
    obj_GetTotalBoundaryElements
  !! Returns the total number of boundary element

  PROCEDURE, PASS(obj) :: GetBoundingBox1 => obj_GetBoundingBox1
  !! Returns the bounding box of the mesh
  PROCEDURE, PASS(obj) :: GetBoundingBox2 => obj_GetBoundingBox2
  !! Return the bounding box from the given nodes, and local_nptrs
  GENERIC, PUBLIC :: GetBoundingBox => GetBoundingBox1, &
    GetBoundingBox2
  !! Return the bounding box

  PROCEDURE, PUBLIC, PASS(obj) :: GetConnectivity => &
    obj_GetConnectivity
  !! Returns  node numbers in an element (this is vertex connectivity)

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: GetConnectivity1_ => &
    obj_GetConnectivity1_
  !! Get connectivity of an element in a single vector
  !! you can specify opt="A, V, E, F, C" for all, vertex, edge, face, cell

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: GetConnectivity2_ => &
    obj_GetConnectivity2_
  !! Get connectivity of an element into separate vectors
  !! you can get cell, face, and edge connectivity

  GENERIC, PUBLIC :: GetConnectivity_ => GetConnectivity1_, &
    GetConnectivity2_
  !! Generic method for getting the connectivity of an element

  PROCEDURE, PUBLIC, PASS(obj) :: GetOrientation => obj_GetOrientation
  !! Get the orientation of the element

  PROCEDURE, PUBLIC, PASS(obj) :: GetNodeConnectivity => &
    obj_GetNodeConnectivity
  !! Returns all the node connectivity of the mesh elements

  PROCEDURE, PASS(obj) :: GetLocalNodeNumber1 => &
    obj_GetLocalNodeNumber1
  !! Returns the local node number of a glocal node number
  PROCEDURE, PASS(obj) :: GetLocalNodeNumber2 => &
    obj_GetLocalNodeNumber2
  !! Returns the local node number of a global node number
  GENERIC, PUBLIC :: GetLocalNodeNumber => GetLocalNodeNumber1, &
    GetLocalNodeNumber2
  !! Returns the local node number of a global node number

  PROCEDURE, PUBLIC, PASS(obj) :: GetLocalNodeNumber_ => &
    obj_GetLocalNodeNumber1_
  !! get local node number without allocation

  PROCEDURE, PASS(obj) :: GetGlobalNodeNumber1 => &
    obj_GetGlobalNodeNumber1
  !! Returns the global node number of a local node number
  PROCEDURE, PASS(obj) :: GetGlobalNodeNumber2 => &
    obj_GetGlobalNodeNumber2
  !! Returns the global node number of a local node number
  GENERIC, PUBLIC :: GetGlobalNodeNumber => GetGlobalNodeNumber1, &
    GetGlobalNodeNumber2

  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalBoundaryNodes => &
    obj_GetTotalBoundaryNodes
  !! Returns the total number of boundary nodes

  PROCEDURE, PASS(obj) :: GetGlobalElemNumber1 => obj_GetGlobalElemNumber1
  PROCEDURE, PASS(obj) :: GetGlobalElemNumber2 => obj_GetGlobalElemNumber2
  GENERIC, PUBLIC :: GetGlobalElemNumber => &
    GetGlobalElemNumber1, GetGlobalElemNumber2

  !! Returns the global element number for a local element number
  PROCEDURE, PASS(obj) :: GetLocalElemNumber1 => obj_GetLocalElemNumber1
  PROCEDURE, PASS(obj) :: GetLocalElemNumber2 => obj_GetLocalElemNumber2
  GENERIC, PUBLIC :: GetLocalElemNumber => &
    GetLocalElemNumber1, GetLocalElemNumber2
  !! Returns the local element number of a global element number

  PROCEDURE, PASS(obj) :: GetNodeToElements1 => obj_GetNodeToElements1
  !! Get list of elements surrounding a single nodes
  PROCEDURE, PASS(obj) :: GetNodeToElements2 => obj_GetNodeToElements2
  !! Get list of elements surrounding several nodes
  GENERIC, PUBLIC :: GetNodeToElements => &
    GetNodeToElements1, GetNodeToElements2
  !! Generic method to get elements around node or nodes

  PROCEDURE, PASS(obj) :: GetNodeToElements1_ => obj_GetNodeToElements1_
  !! Get list of elements surrounding a single nodes (no alloc)
  PROCEDURE, PASS(obj) :: GetNodeToElements2_ => obj_GetNodeToElements2_
  !! Get list of elements surrounding several nodes (no alloc)
  GENERIC, PUBLIC :: GetNodeToElements_ => &
    & GetNodeToElements1_, GetNodeToElements2_
  !! Generic method to get elements around node or nodes (no alloc)

  PROCEDURE, PASS(obj) :: GetNodeToNodes1 => obj_GetNodeToNodes1
  !! Returns global node number connected to a given global node
  PROCEDURE, PASS(obj) :: GetNodeToNodes2 => obj_GetNodeToNodes2
  !! Returns global node numbers connected to given global node numbers
  GENERIC, PUBLIC :: GetNodeToNodes => &
    GetNodeToNodes1, GetNodeToNodes2
  !! Returns nodes connected to a given node number

  PROCEDURE, PASS(obj) :: GetNodeToNodes1_ => obj_GetNodeToNodes1_
  !! Returns global node number connected to a given global node
  PROCEDURE, PASS(obj) :: GetNodeToNodes2_ => obj_GetNodeToNodes2_
  !! Returns global node numbers connected to given global node numbers
  GENERIC, PUBLIC :: GetNodeToNodes_ => &
    GetNodeToNodes1_, GetNodeToNodes2_
  !! Returns nodes connected to a given node number

  PROCEDURE, PUBLIC, PASS(obj) :: GetElementToElements => &
    obj_GetElementToElements
  !! Returns local element number connected to a given local
  !! element number, it also gives information about the local
  !! facet number

  PROCEDURE, PASS(obj) :: GetElementToElements1_ => &
    obj_GetElementToElements1_
  !! Get element to elements mapping
  PROCEDURE, PASS(obj) :: GetElementToElements2_ => &
    obj_GetElementToElements2_
  !! Get element to elements mapping
  GENERIC, PUBLIC :: GetElementToElements_ => GetElementToElements1_, &
    GetElementToElements2_

  PROCEDURE, PUBLIC, PASS(obj) :: GetBoundaryElementData => &
    obj_GetBoundaryElementData
  !! Returns boundary element data

  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalFacetElements => &
    obj_GetTotalFacetElements
  !! Returns the total number of facet elements in the mesh

  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalBoundaryFacetElements => &
    obj_GetTotalBoundaryFacetElements
  !! Returns the total number of boundary facet elements

  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalInternalFacetElements => &
    obj_GetTotalInternalFacetElements
  !! Returns the total number of internal facet elements

  PROCEDURE, PUBLIC, PASS(obj) :: GetMasterCellNumber => &
    obj_GetMasterCellNumber
  !! Returns the master cell number of a facet element

  PROCEDURE, PUBLIC, PASS(obj) :: GetSlaveCellNumber => &
    obj_GetSlaveCellNumber
  !! Returns the slave cell number of a facet element

  PROCEDURE, PUBLIC, PASS(obj) :: GetCellNumber => &
    obj_GetCellNumber
  !! Returns the master and slave cell number of a facet element

  PROCEDURE, PUBLIC, PASS(obj) :: GetLocalFacetID => &
    obj_GetLocalFacetID
  !! Return the local facet id, so that we can Get reference element of
  !! the facet element

  PROCEDURE, PUBLIC, PASS(obj) :: GetGlobalFaceNumber => &
    obj_GetGlobalFaceNumber
  !! Get global face number from global element and localFacenumber

  PROCEDURE, PUBLIC, PASS(obj) :: GetGlobalEdgeNumber => &
    obj_GetGlobalEdgeNumber
  !! Get global Edge number from global element and localEdgenumber

  PROCEDURE, PUBLIC, PASS(obj) :: FindFace => obj_FindFace
  !! Find a face in a cell

  PROCEDURE, PUBLIC, PASS(obj) :: FindEdge => obj_FindEdge
  !! Find a edge in a cell (only for 3D)

  PROCEDURE, PUBLIC, PASS(obj) :: GetFacetConnectivity => &
    obj_GetFacetConnectivity
  !! Generic method to Get the connectivity of a facet element

  PROCEDURE, PUBLIC, PASS(obj) :: GetFacetElementType => &
    obj_GetFacetElementType
  !! Returns the facet element type of a given cell element number

  PROCEDURE, PASS(obj) :: GetOrder1 => obj_GetOrder1
  !! Returns the order ofthe element of mesh
  !! This method will be deprecated in future
  PROCEDURE, PASS(obj) :: GetOrder2 => obj_GetOrder2
  !! get order of an element
  GENERIC, PUBLIC :: GetOrder => GetOrder1, GetOrder2

  PROCEDURE, PUBLIC, PASS(obj) :: GetNSD => &
    obj_GetNSD
  !! Return the NSD

  PROCEDURE, PUBLIC, PASS(obj) :: GetXidimension => &
    obj_GetXidimension
  !! Return the NSD

  PROCEDURE, PUBLIC, PASS(obj) :: GetMaterial => obj_GetMaterial1
  !! returns the material id of a given medium

  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalMaterial1 => obj_GetTotalMaterial1
  !! returns the total materials in an element
  GENERIC, PUBLIC :: GetTotalMaterial => GetTotalMaterial1

  PROCEDURE, PUBLIC, PASS(obj) :: GetParam => obj_GetParam
  !! Get parameter of mesh

  PROCEDURE, PUBLIC, PASS(obj) :: GetMinElemNumber => obj_GetMinElemNumber
  !! Get minimum element number
  PROCEDURE, PUBLIC, PASS(obj) :: GetMaxElemNumber => obj_GetMaxElemNumber
  !! get maximum element number
  PROCEDURE, PUBLIC, PASS(obj) :: GetMinNodeNumber => obj_GetMinNodeNumber
  !! get minimum element number
  PROCEDURE, PUBLIC, PASS(obj) :: GetMaxNodeNumber => obj_GetMaxNodeNumber
  !! Get maximum node number

  PROCEDURE, PUBLIC, PASS(obj) :: isInit => obj_isInit
  !! Returns obj%isInitiated
  PROCEDURE, PUBLIC, PASS(obj) :: isNodeToElements => obj_isNodeToElements
  PROCEDURE, PUBLIC, PASS(obj) :: isNodeToNodes => obj_isNodeToNodes
  PROCEDURE, PUBLIC, PASS(obj) :: isExtraNodeToNodes => obj_isExtraNodeToNodes
  PROCEDURE, PUBLIC, PASS(obj) :: isElementToElements => &
    obj_isElementToElements
  PROCEDURE, PUBLIC, PASS(obj) :: isEdgeConnectivity => obj_isEdgeConnectivity
  PROCEDURE, PUBLIC, PASS(obj) :: isFaceConnectivity => obj_isFaceConnectivity
  PROCEDURE, PUBLIC, PASS(obj) :: isBoundaryData => obj_isBoundaryData
  PROCEDURE, PUBLIC, PASS(obj) :: isFacetData => obj_isFacetData
  PROCEDURE, PUBLIC, PASS(obj) :: isElementActive => obj_isElementActive

  PROCEDURE, PUBLIC, PASS(obj) :: GetFacetParam => obj_GetFacetParam
  !! Get the parameters of facet elements

  PROCEDURE, PASS(obj) :: GetTotalEntities1 => obj_GetTotalEntities1
  !! Get total entities in an element
  PROCEDURE, PASS(obj) :: GetTotalEntities2 => obj_GetTotalEntities2
  !! Get total entities (VEFC) in the mesh
  GENERIC, PUBLIC :: GetTotalEntities => GetTotalEntities1, GetTotalEntities2
  !! Generic method for getting total enttiies in mesh and an element

  PROCEDURE, PASS(obj) :: GetNodeCoord1 => obj_GetNodeCoord1
  !! Get node coord from the HDF5File
  PROCEDURE, PASS(obj) :: GetNodeCoord2 => obj_GetNodeCoord2
  !! Get node coord in a 2D array, stored inside nodedata
  PROCEDURE, PASS(obj) :: GetNodeCoord3 => obj_GetNodeCoord3
  !! Get node coord of an element
  PROCEDURE, PASS(obj) :: GetNodeCoord4 => obj_GetNodeCoord4
  !! Get node coord of specified nodes
  PROCEDURE, PASS(obj) :: GetNodeCoord5 => obj_GetNodeCoord5
  !! Get node coord of a single node

  GENERIC, PUBLIC :: GetNodeCoord => GetNodeCoord1, GetNodeCoord2, &
    GetNodeCoord3, GetNodeCoord4, GetNodeCoord5

  PROCEDURE, PASS(obj) :: GetNearestNode1 => obj_GetNearestNode1
  PROCEDURE, PASS(obj) :: GetNearestNode2 => obj_GetNearestNode2
  GENERIC, PUBLIC :: GetNearestNode => GetNearestNode1, GetNearestNode2

  PROCEDURE, PUBLIC, PASS(obj) :: GetMaxNodeToElements => &
    obj_GetMaxNodeToElements
  !! Get maximum number of node to elements

  PROCEDURE, PASS(obj) :: GetMaxElementToElements => &
    obj_GetMaxElementToElements
  !! Get maximum number of element to elements

  PROCEDURE, PASS(obj) :: GetMaxNodeToNodes => &
    obj_GetMaxNodeToNodes
  !! Get maximum number of node to nodes

  ! SET:
  ! @SetMethods

  PROCEDURE, PUBLIC, PASS(obj) :: SetShowTime => obj_SetShowTime
  !! Set showTime option

  PROCEDURE, PASS(obj) :: SetBoundingBox1 => obj_SetBoundingBox1
  !! Set the bounding box of the mesh
  PROCEDURE, PASS(obj) :: SetBoundingBox2 => obj_SetBoundingBox2
  !! Set the bounding box from the given nodes, and local_nptrs
  GENERIC, PUBLIC :: SetBoundingBox => SetBoundingBox1, &
    SetBoundingBox2
  !! Set the bounding box

  PROCEDURE, PASS(obj) :: SetSparsity1 => obj_SetSparsity1
  !! Set the sparsity of sparse matrix
  PROCEDURE, PASS(obj) :: SetSparsity2 => obj_SetSparsity2
  !! Set the sparsity of sparse matrix
  PROCEDURE, PASS(obj) :: SetSparsity3 => obj_SetSparsity3
  !! Set the sparsity of sparse matrix
  PROCEDURE, PASS(obj) :: SetSparsity4 => obj_SetSparsity4
  !! Set the sparsity of sparse matrix
  GENERIC, PUBLIC :: SetSparsity => SetSparsity1, SetSparsity2, &
    SetSparsity3, SetSparsity4
  !! Generic method for Setting the sparsity

  PROCEDURE, PASS(obj) :: SetTotalMaterial1 => obj_SetTotalMaterial1
  !! Adding a material ID of a medium which is mapped to the mesh
  PROCEDURE, PASS(obj) :: SetTotalMaterial2 => obj_SetTotalMaterial2
  !! Adding a material ID of a medium which is mapped to the mesh
  GENERIC, PUBLIC :: SetTotalMaterial => SetTotalMaterial1, SetTotalMaterial2
  !! Generic method

  PROCEDURE, PASS(obj) :: SetMaterial1 => obj_SetMaterial1
  !! Adding a material ID of a medium which is mapped to the mesh
  PROCEDURE, PASS(obj) :: SetMaterial2 => obj_SetMaterial2
  !! Adding a material ID of a medium which is mapped to the mesh
  !! This is for backward compatibility only
  PROCEDURE, PASS(obj) :: SetMaterial3 => obj_SetMaterial3
  !! Set material to an element
  GENERIC, PUBLIC :: SetMaterial => SetMaterial1, SetMaterial2, &
    SetMaterial3

  PROCEDURE, PUBLIC, PASS(obj) :: SetFacetElementType => &
    obj_SetFacetElementType
  !! Set the facet element type of a given cell number
  PROCEDURE, PUBLIC, PASS(obj) :: SetQuality => obj_SetQuality
    !! Set mesh quality

  PROCEDURE, PUBLIC, PASS(obj) :: SetParam => obj_SetParam
  !! set parameters of mesh

  PROCEDURE, PUBLIC, PASS(obj) :: SetFacetParam => obj_SetFacetParam
  !! Set the parametersof facet element

  ! IO:
  ! @IOMethods

  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  !! Read mesh from hdf5 file

  PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export
  !! Export mesh to an hdf5 file

  PROCEDURE, PUBLIC, PASS(obj) :: ExportToVTK => obj_ExportToVTK
  !! Export mesh to a VTKfile

  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_display
  !! Display the mesh

  PROCEDURE, PUBLIC, PASS(obj) :: DisplayNodeData => &
    obj_DisplayNodeData
  !! Display node data

  PROCEDURE, PUBLIC, PASS(obj) :: DisplayElementData => &
    obj_DisplayElementData
  !! Display element data

  PROCEDURE, PUBLIC, PASS(obj) :: DisplayFacetData => &
    obj_DisplayFacetData
  !! Display  facet data

  PROCEDURE, PUBLIC, PASS(obj) :: DisplayInternalFacetData => &
    obj_DisplayInternalFacetData
  !! Display internal facet data

  PROCEDURE, PUBLIC, PASS(obj) :: DisplayBoundaryFacetData => &
    obj_DisplayBoundaryFacetData
  !! Display mesh facet data

  PROCEDURE, PUBLIC, PASS(obj) :: DisplayFacetElements => &
    obj_DisplayFacetElements
  !! Display facet element shape data

  PROCEDURE, PUBLIC, PASS(obj) :: DisplayMeshInfo => &
    obj_DisplayMeshInfo
  !! Display mesh statistics

END TYPE AbstractMesh_

!----------------------------------------------------------------------------
!                                                   AbstractMeshPointer_
!----------------------------------------------------------------------------

TYPE :: AbstractMeshPointer_
  CLASS(AbstractMesh_), POINTER :: ptr => NULL()
END TYPE AbstractMeshPointer_

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-03-18
! summary: Read the mesh from the HDF5File_ see import method

INTERFACE
  MODULE SUBROUTINE obj_Initiate(obj, hdf5, group, dim, entities)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! Mesh file in hdf5 file format
    CHARACTER(*), OPTIONAL, INTENT(IN) :: group
    !! location in HDF5 file
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim
    !! dimension of the mesh
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: entities(:)
    !! entity number
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Free up the memory stored in [[obj_]]

INTERFACE AbstractMeshDeallocate
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE AbstractMeshDeallocate

!----------------------------------------------------------------------------
!                                                 isEmpty@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-31
! summary:  Returns true if the mesh is empty

INTERFACE
  MODULE FUNCTION obj_isEmpty(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_isEmpty
END INTERFACE

!----------------------------------------------------------------------------
!                           InitiateDynamicDataStructure@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-31
! summary:  Initiate dynamic data structure from static data structure

INTERFACE
  MODULE SUBROUTINE obj_InitiateDynamicDataStructure(obj)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_InitiateDynamicDataStructure
END INTERFACE

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-10
! summary: Deallocate kdtree related data

INTERFACE
  MODULE SUBROUTINE obj_DeallocateKdtree(obj)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! AbstractDomain object
  END SUBROUTINE obj_DeallocateKdtree
END INTERFACE

!----------------------------------------------------------------------------
!                                               Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-16
! summary: Free up the memory stored in [[obj_]]

INTERFACE
  MODULE SUBROUTINE AbstractMeshPointerDeallocate(obj)
    TYPE(AbstractMeshPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
  END SUBROUTINE AbstractMeshPointerDeallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
!> date: 2024-03-18
!> summary: Imports mesh data from an HDF5 file
!
!# Description
!
! This method reads mesh data from an HDF5 file and initializes the AbstractMesh_
! object. It serves as a wrapper around the `Initiate` method, providing the
! same functionality with a more intuitive name for importing mesh data.
!
! The method reads the following data:
! - Mesh ID (uid)
! - Spatial dimensions (xidim, nsd)
! - Element types and topology
! - Bounding box information (minX, minY, minZ, maxX, maxY, maxZ)
! - Node coordinates (x, y, z)
! - Element counts (tElements, tNodes)
! - Connectivity information
! - Boundary entities
!
! The routine also initializes:
! - Local node numbering (local_nptrs)
! - Node data arrays
! - Global-to-local node mapping
!
!# Arguments
!
! - `obj`: The AbstractMesh_ object to be initialized
! - `hdf5`: HDF5File_ object containing mesh data
! - `group` (optional): Location in the HDF5 file where mesh data is stored
! - `dim` (optional): Dimension of the mesh to read
! - `entities` (optional): Entity numbers to be included in the mesh
!
!# Usage
!
!```fortran
! type(HDF5File_) :: h5file
! type(Mesh_) :: mesh
!
! call h5file%open("mesh.h5", "r")
! call mesh%Import(h5file, group="/mesh")
! call h5file%close()
!```
!
!# Notes
!
! - If `group` is provided, mesh data from that group is read
! - If `dim` is provided, all entities of that dimension are read
! - If both `dim` and `entities` are provided, the method constructs groups
!   based on the dimension and entities to create the mesh
! - After reading the data, the mesh is marked as initiated (`isInitiated = .TRUE.`)
! - This method is equivalent to calling the `Initiate` method

INTERFACE AbstractMeshImport
  MODULE SUBROUTINE obj_Import(obj, hdf5, group, dim, entities)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), OPTIONAL, INTENT(IN) :: group
    !! Group name
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim
    !! dimension
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: entities(:)
    !! entityNum
  END SUBROUTINE obj_Import
END INTERFACE AbstractMeshImport

!----------------------------------------------------------------------------
!                                                    GetNodeCoord@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Reads hdf5File for nodecoord of the mesh
!
!# Introduction
!
! This routine reads [[HDFFile_]] instance for constructing nodeCoord of mesh
!
! - Rows of `nodeCoord` represents the spatial component
! - Columns of `nodeCoord` retpresents the node number
! - Total number of columns in `nodeCoord` is equal to the number of
! nodes present in the mesh object.
!
!@note
! The nodeCoord returned by this routine should be used by the mesh object
! itself. This is because, in nodeCoords the nodes are arranged locally.
! However, if you wish to use nodeCoord, then Get the localNodeNumber of a
! global node by calling the mesh methods, and use this localNodeNumber to
! extract the coordinates.
!@endnote

INTERFACE
  MODULE SUBROUTINE obj_GetNodeCoord1(obj, nodeCoord, hdf5, group)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: nodeCoord(:, :)
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_GetNodeCoord1
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Export@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: This routine exports the mesh to a hdf5 file
!
!# Introduction

INTERFACE
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ExportToVTK@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Export mesh to a VTK file
!
!
!# Introduction
!
! - If `filename` is present then call [[VTKFile_:InitiateVTKFile]] method
! - If `nodeCoord` is present then write Points by calling
! [[VTKFile_:WritePoints]] method
! - If `content` is present then write cell data by calling
! [[VTKFile_:WriteCells]] methods
! - If openTag is true then write piece info
! - If cloSetag is true then close the piece

INTERFACE
  MODULE SUBROUTINE obj_ExportToVTK(obj, vtk, nodeCoord, filename, &
                                    opentag, content, closetag)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    TYPE(VTKFile_), INTENT(INOUT) :: vtk
    REAL(DFP), OPTIONAL, INTENT(IN) :: nodeCoord(:, :)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: opentag
    !! Default is true
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: closetag
    !! Default is true
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: content
    !! Default is true
  END SUBROUTINE obj_ExportToVTK
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Displays the content of [[obj_]] datatype
!
!# Introduction
!
! This routine displays the content of [[obj_]] datatype
!
!### Usage
!
!```fortran
! call display( obj, 'mesh', stdout )
! call obj%display( 'mesh', stdout )
!```

INTERFACE AbstractMeshDisplay
  MODULE SUBROUTINE obj_Display(obj, msg, unitno)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
    CHARACTER(*), INTENT(IN) :: msg
    !! message on screen
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
    !! unit number of ouput file
  END SUBROUTINE obj_Display
END INTERFACE AbstractMeshDisplay

!----------------------------------------------------------------------------
!                                              DisplayNodeData@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Displays the Node data

INTERFACE
  MODULE SUBROUTINE obj_DisplayNodeData(obj, msg, unitno)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE obj_DisplayNodeData
END INTERFACE

!----------------------------------------------------------------------------
!                                              DisplayElementData@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Displays the element data

INTERFACE
  MODULE SUBROUTINE obj_DisplayElementData(obj, msg, unitno, globalElement, &
                                           islocal)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! object
    CHARACTER(*), INTENT(IN) :: msg
    !! message
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
    !! unit number
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: globalElement
    !! global element number
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! if true then globalElement is local
  END SUBROUTINE obj_DisplayElementData
END INTERFACE

!----------------------------------------------------------------------------
!                                              DisplayFacetData@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Displays the internal facet data

INTERFACE
  MODULE SUBROUTINE obj_DisplayInternalFacetData(obj, msg, unitno)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE obj_DisplayInternalFacetData
END INTERFACE

!----------------------------------------------------------------------------
!                                              DisplayFacetData@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Displays the facet data

INTERFACE
  MODULE SUBROUTINE obj_DisplayFacetData(obj, msg, unitno)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE obj_DisplayFacetData
END INTERFACE

!----------------------------------------------------------------------------
!                                        DisplayBoundaryFacetData@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Displays the boundary facet data

INTERFACE
  MODULE SUBROUTINE obj_DisplayBoundaryFacetData(obj, msg, unitno)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE obj_DisplayBoundaryFacetData
END INTERFACE

!----------------------------------------------------------------------------
!                                              DisplayFacetElements@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Display the facet elements

INTERFACE
  MODULE SUBROUTINE obj_DisplayFacetElements(obj, msg, unitno)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE obj_DisplayFacetElements
END INTERFACE

!----------------------------------------------------------------------------
!                                               DisplayMeshInfo@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-31
! summary:  Display mesh statistics

INTERFACE
  MODULE SUBROUTINE obj_DisplayMeshInfo(obj, msg, unitno)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE obj_DisplayMeshInfo
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetNodeCoord2@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-05-24
! summary: Get All the NodeCoord of  the mesh

INTERFACE
  MODULE SUBROUTINE obj_GetNodeCoord2(obj, nodeCoord, nrow, ncol)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! Abstract mesh object
    REAL(DFP), INTENT(INOUT) :: nodeCoord(:, :)
    !! All node coordinates of the mesh in xiJ format
    !! The nodes are arranged in local order
    !! So to get the global node coord, first you need to convert
    !! the global node number to local node number. Then, use this local
    !! node number to extract the node coordinates
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! number of rows written in nodecoord
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! number of columns written in nodecoord
  END SUBROUTINE obj_GetNodeCoord2
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetNodeCoord@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-05-24
! summary: Get the nodecoord of an element

INTERFACE
  MODULE SUBROUTINE obj_GetNodeCoord3(obj, nodeCoord, nrow, &
                                      ncol, globalElement, islocal)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! Abstract mesh object
    REAL(DFP), INTENT(INOUT) :: nodeCoord(:, :)
    !! node coordinates of an element in xiJ format
    !! The nodes are arranged in local order
    !! The global nodes can be obtianed by calling the GetVertexConnectivity
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written in nodecoord
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global or local element
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! if true then globalElement is local
  END SUBROUTINE obj_GetNodeCoord3
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetNodeCoord@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-05-24
! summary: Get the nodecoord of an element

INTERFACE
  MODULE SUBROUTINE obj_GetNodeCoord4(obj, nodeCoord, nrow, ncol, &
                                      globalNode, islocal)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! Abstract mesh object
    REAL(DFP), INTENT(INOUT) :: nodeCoord(:, :)
    !! node coordinates of an element in xiJ format
    !! The nodes are arranged in local order
    !! The global nodes can be obtianed by calling the GetVertexConnectivity
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written in nodecoord
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node numbers
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! if true then globalNode is local
  END SUBROUTINE obj_GetNodeCoord4
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetNodeCoord@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-05-24
! summary: Get the nodecoord of a single node

INTERFACE
  MODULE SUBROUTINE obj_GetNodeCoord5(obj, nodeCoord, tsize, &
                                      globalNode, islocal)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! Abstract mesh object
    REAL(DFP), INTENT(INOUT) :: nodeCoord(:)
    !! node coordinates of an element in xiJ format
    !! The nodes are arranged in local order
    !! The global nodes can be obtianed by calling the GetVertexConnectivity
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of rows and columns written in nodecoord
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global or local node numbers
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! if true then globalNode is local
  END SUBROUTINE obj_GetNodeCoord5
END INTERFACE

!----------------------------------------------------------------------------
!                                                  GetNearestNode@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-06-10
! summary:  Get nearest node

INTERFACE
  MODULE SUBROUTINE obj_GetNearestNode1(obj, qv, x, globalNode)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: qv(:)
    !! Query vector
    REAL(DFP), INTENT(INOUT) :: x(:)
    !! node coord of nearest node
    INTEGER(I4B), INTENT(OUT) :: globalNode
    !! globalNode number
  END SUBROUTINE obj_GetNearestNode1
END INTERFACE

!----------------------------------------------------------------------------
!                                                  GetNearestNode@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-04-11
! summary:  Get nearest node

INTERFACE
  MODULE SUBROUTINE obj_GetNearestNode2(obj, qv, x, globalNode, nn)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: qv(:)
    !! Query vector
    REAL(DFP), INTENT(INOUT) :: x(:, :)
    !! node coord of nearest node
    !! the size(x, 2) should be atleast nn
    INTEGER(I4B), INTENT(INOUT) :: globalNode(:)
    !! globalNode number, size of globalNode should be atleast nn
    INTEGER(I4B), INTENT(IN) :: nn
    !! number of nearest points
  END SUBROUTINE obj_GetNearestNode2
END INTERFACE

!----------------------------------------------------------------------------
!                                    GetTotalElementsTopologyWise@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-12
! summary:  REturns the total elements topology wise

INTERFACE
  MODULE PURE FUNCTION obj_GetTotalElementsTopologyWise(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans(8)
  !! Total number of points, lines, triangles, quadrangles, tetrahedrons,
  !! hexahedrons, prisms, pyramids
  END FUNCTION obj_GetTotalElementsTopologyWise
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetTotalTopology@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-12
! summary:  Returns the total number of topology present in the mesh

INTERFACE
  MODULE PURE FUNCTION obj_GetTotalTopology(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalTopology
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetElementTopology@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-12
! summary:  Get the name of element topology present in the mesh

INTERFACE
  MODULE FUNCTION obj_GetElemTopology1(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans(8)
  END FUNCTION obj_GetElemTopology1
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetElementTopology@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-12
! summary:  Get the name of element topology present in the mesh

INTERFACE
  MODULE FUNCTION obj_GetElemTopology2(obj, globalElement, islocal) &
    RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global or local element number
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! if true then global element is local element
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetElemTopology2
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetElemTopologyIndx@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:
! summary:  Get the element topology index
!
!# Introduction
!
! Point 1
! Line 2
! Triangle 3
! Quadrangle
! Tetrahedron
! Hexahedron
! Prism
! Pyramid

INTERFACE
  MODULE FUNCTION obj_GetElemTopologyIndx(obj, globalElement, islocal) &
    RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global or local element number
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! if true then global element is local element
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetElemTopologyIndx
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetElemType@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-13
! summary:  get element type from the element data

INTERFACE
  MODULE FUNCTION obj_GetElemType(obj, globalElement, islocal) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetElemType
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetElemData@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-04-18
! summary:  Get teh element data (hardcopoy)

INTERFACE
  MODULE SUBROUTINE obj_GetElemData(obj, elemdata, globalElement, islocal)
    CLASS(AbstractMesh_), INTENT(in) :: obj
    TYPE(ElemData_), INTENT(INOUT) :: elemdata
    INTEGER(I4B), INTENT(IN) :: globalElement
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
  END SUBROUTINE obj_GetElemData
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetElemDataPointer@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-04-18
! summary:  Get teh element data (hardcopoy)

INTERFACE
  MODULE FUNCTION obj_GetElemDataPointer(obj, globalElement, islocal) &
    RESULT(ans)
    CLASS(AbstractMesh_), TARGET, INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    TYPE(ElemData_), POINTER :: ans
  END FUNCTION obj_GetElemDataPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetNNE@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-25
! summary:  Get number of nodes in element

INTERFACE
  MODULE FUNCTION obj_GetNNE(obj, globalElement, islocal) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetNNE
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetNNE@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-25
! summary:  Get number of nodes in element

INTERFACE
  MODULE FUNCTION obj_GetMaxNNE(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetMaxNNE
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetTotalElements@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns total elements in the mesh

INTERFACE
  MODULE FUNCTION obj_Size(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B) :: ans
  END FUNCTION obj_Size
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetTotalElements@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-17
! summary:  Get total number of elements of given meshid

INTERFACE
  MODULE FUNCTION obj_GetTotalElements2(obj, meshid) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: meshid
    !! mesh id
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalElements2
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetTotalElements@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-20
! summary:  Get total elements

INTERFACE
  MODULE FUNCTION obj_GetTotalElements3(obj, meshid) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: meshid(:)
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalElements3
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetElemNum@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns the global element numbers present in the mesh

INTERFACE
  MODULE FUNCTION obj_GetElemNum1(obj, islocal) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_GetElemNum1
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetElemNum@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-07-17
! summary: Get global element number for a given meshid

INTERFACE
  MODULE FUNCTION obj_GetElemNum2(obj, meshid, islocal) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: meshid
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_GetElemNum2
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetElemNum_@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_GetElemNum1_(obj, islocal, ans, tsize)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    LOGICAL(LGT), INTENT(IN) :: islocal
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE obj_GetElemNum1_
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetElemNum_@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_GetElemNum2_(obj, meshid, islocal, ans, tsize)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: meshid
    LOGICAL(LGT), INTENT(IN) :: islocal
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE obj_GetElemNum2_
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetElemNum_@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_GetElemNum3_(obj, meshid, islocal, ans, tsize)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: meshid(:)
    LOGICAL(LGT), INTENT(IN) :: islocal
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE obj_GetElemNum3_
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetBoundingEntity@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns bounding entity of the mesh

INTERFACE
  MODULE FUNCTION obj_GetBoundingEntity(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_GetBoundingEntity
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetNptrs@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns the vector of global node numbers

INTERFACE
  MODULE FUNCTION obj_GetNptrs1(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans(obj%tNodes)
  END FUNCTION obj_GetNptrs1
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetNptrs@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns the vector of global node numbers

INTERFACE
  MODULE FUNCTION obj_GetNptrs2(obj, meshid) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: meshid
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_GetNptrs2
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetNptrs@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-03-29
! summary: Returns the vector of global node numbers

INTERFACE
  MODULE SUBROUTINE obj_GetNptrs1_(obj, ans, tsize)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE obj_GetNptrs1_
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetNptrs@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_GetNptrs2_(obj, meshid, ans, tsize)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: meshid
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
  END SUBROUTINE obj_GetNptrs2_
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetNptrs@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_GetNptrs3_(obj, globalElement, ans, tsize, islocal)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement(:)
    !! global or local element number
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! if true then globalElement is local
  END SUBROUTINE obj_GetNptrs3_
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetNptrs@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-06-10
! summary: this routine returns the global node number in a box

INTERFACE
  MODULE SUBROUTINE obj_GetNptrsInBox(obj, box, nptrs, isStrict)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
      !! If Kdtree is not init then we init it
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: nptrs(:)
    TYPE(BoundingBox_), INTENT(IN) :: box
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isStrict
    !! Default is true
    !! If it is true the returned points are strictly inside or on the
    !! box, but not outside of it
    !! This is because we use radius of bounding box to find the points
    !! this is over estimation.
  END SUBROUTINE obj_GetNptrsInBox
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetNptrs@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-06-10
! summary: this routine returns the global node number in a box

INTERFACE
  MODULE SUBROUTINE obj_GetNptrsInBox_(obj, box, nptrs, tnodes, isStrict)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
      !! If Kdtree is not init then we init it
    TYPE(BoundingBox_), INTENT(IN) :: box
    INTEGER(I4B), INTENT(INOUT) :: nptrs(:)
    !! it should allocated, size of nptrs should be .ge. tnodes
    INTEGER(I4B), INTENT(INOUT) :: tnodes
    !! total nodes found
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isStrict
    !! Default is true
    !! If it is true the returned points are strictly inside or on the
    !! box, but not outside of it
    !! This is because we use radius of bounding box to find the points
    !! this is over estimation.
  END SUBROUTINE obj_GetNptrsInBox_
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetInternalNptrs@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns the vector of global node numbers of internal nodes

INTERFACE
  MODULE FUNCTION obj_GetInternalNptrs(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_GetInternalNptrs
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetInternalNptrs_@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-03-29
! summary: Returns the vector of global node numbers of internal nodes

INTERFACE
  MODULE SUBROUTINE obj_GetInternalNptrs_(obj, nptrs)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(INOUT) :: nptrs(:)
  END SUBROUTINE obj_GetInternalNptrs_
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetBoundaryNptrs@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns the vector of global node numbers of boundary nodes

INTERFACE
  MODULE FUNCTION obj_GetBoundaryNptrs(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_GetBoundaryNptrs
END INTERFACE

!----------------------------------------------------------------------------
!                                                 isBoundaryNode@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: This function returns true if given global node is a boundary node

INTERFACE
  MODULE FUNCTION obj_isBoundaryNode(obj, globalNode, islocal) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    LOGICAL(LGT) :: ans
  END FUNCTION obj_isBoundaryNode
END INTERFACE

!----------------------------------------------------------------------------
!                                                  isNodePresent@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns  TRUE if a given global node number is present

INTERFACE
  MODULE FUNCTION obj_isNodePresent1(obj, globalNode, islocal) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    LOGICAL(LGT) :: ans
  END FUNCTION obj_isNodePresent1
END INTERFACE

!----------------------------------------------------------------------------
!                                                  isNodePresent@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns  TRUE if a given global node number is present

INTERFACE
  MODULE FUNCTION obj_isNodePresent2(obj, globalNode, islocal) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    LOGICAL(LGT) :: ans(SIZE(globalNode))
  END FUNCTION obj_isNodePresent2
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetNodeMask@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-04-09
! summary:  Returns a mask vector for presence of nodes

INTERFACE
  MODULE SUBROUTINE obj_GetNodeMask(obj, mask, local_nptrs)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    LOGICAL(LGT), INTENT(INOUT) :: mask(:)
    !! the size of mask should be more than or equal to  the maxNptrs
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: local_nptrs(:)
    !! additional mapping
  END SUBROUTINE obj_GetNodeMask
END INTERFACE

!----------------------------------------------------------------------------
!                                                isAnyNodePresent@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns TRUE if any global node number is present

INTERFACE
  MODULE FUNCTION obj_isAnyNodePresent(obj, globalNode, islocal) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    LOGICAL(LGT) :: ans
  END FUNCTION obj_isAnyNodePresent
END INTERFACE

!----------------------------------------------------------------------------
!                                                isAllNodePresent@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns TRUE if any global node number is present

INTERFACE
  MODULE FUNCTION obj_isAllNodePresent(obj, globalNode, islocal) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    LOGICAL(LGT) :: ans
  END FUNCTION obj_isAllNodePresent
END INTERFACE

!----------------------------------------------------------------------------
!                                                 isElementPresent@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns  TRUE if a given global Element number is present

INTERFACE
  MODULE FUNCTION obj_isElementPresent(obj, globalElement, islocal) &
    RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    LOGICAL(LGT) :: ans
  END FUNCTION obj_isElementPresent
END INTERFACE

!----------------------------------------------------------------------------
!                                               isBoundaryElement@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns true if an global element number is a boundary element
!
!# Introduction
! This routine returns true if a global element number is a boundary element.
! A boundary element is one which contains a boundary node.

INTERFACE
  MODULE FUNCTION obj_isBoundaryElement(obj, globalElement, islocal) &
    & RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    LOGICAL(LGT) :: ans
  END FUNCTION obj_isBoundaryElement
END INTERFACE

!----------------------------------------------------------------------------
!                                         isDomainBoundaryElement@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns true if an global element number is domain boundary element
!
!# Introduction
! This routine returns true if a global element number is domain
! boundary element.
! A boundary element is one which contains a boundary node.
! A domain boundary element is a boundary element with
! no connection with the other mesh.

INTERFACE
  MODULE FUNCTION obj_isDomainBoundaryElement(obj, globalElement, islocal) &
    & RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    LOGICAL(LGT) :: ans
  END FUNCTION obj_isDomainBoundaryElement
END INTERFACE

!----------------------------------------------------------------------------
!                                            isDomainFacetElement@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns true if an global element number is domain boundary element
!
!# Introduction
! This routine returns true if a global element number is domain
! boundary element.
! A boundary element is one which contains a boundary node.
! A domain boundary element is a boundary element with
! no connection with the other mesh.

INTERFACE
  MODULE FUNCTION obj_isDomainFacetElement(obj, facetElement) &
    & RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: facetElement
    LOGICAL(LGT) :: ans
  END FUNCTION obj_isDomainFacetElement
END INTERFACE

!----------------------------------------------------------------------------
!                                           GetTotalInternalNodes@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns total number of internal nodes inside the mesh

INTERFACE
  MODULE FUNCTION obj_GetTotalInternalNodes(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalInternalNodes
END INTERFACE

!----------------------------------------------------------------------------
!                                         GetTotalVertexNodes@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2025-06-05
! summary: Returns total number of vertex nodes in the mesh

INTERFACE
  MODULE FUNCTION obj_GetTotalVertexNodes1(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalVertexNodes1
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetTotalVertexNodes@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-05
! summary:  Get total nodes of a given meshid

INTERFACE
  MODULE FUNCTION obj_GetTotalVertexNodes2(obj, meshid) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: meshid
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalVertexNodes2
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetTotalVertexNodes@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-05
! summary:  Get total vertex nodes in a collection of elements

INTERFACE
  MODULE FUNCTION obj_GetTotalVertexNodes3(obj, globalElement, islocal) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! abstrract mesh
    INTEGER(I4B), INTENT(IN) :: globalElement(:)
    !! global or local element number
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! if true then global element is local element
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalVertexNodes3
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetTotalVertexNodes@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-05
! summary:  Get total vertex nodes in an element

INTERFACE
  MODULE FUNCTION obj_GetTotalVertexNodes4(obj, globalElement, islocal) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! abstrract mesh
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global or local element number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global element is local element
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalVertexNodes4
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetTotalNodes@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: returns total number of nodes in the mesh

INTERFACE
  MODULE FUNCTION obj_GetTotalNodes1(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalNodes1
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetTotalNodes@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-17
! summary:  Get total nodes of in a given meshid

INTERFACE
  MODULE FUNCTION obj_GetTotalNodes2(obj, meshid) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: meshid
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalNodes2
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetTotalNodes@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-17
! summary:  Get total nodes in a collection of elements

INTERFACE
  MODULE FUNCTION obj_GetTotalNodes3(obj, globalElement, islocal) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! abstrract mesh
    INTEGER(I4B), INTENT(IN) :: globalElement(:)
    !! global or local element number
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! if true then global element is local element
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalNodes3
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetTotalFaces@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: returns total number of Faces in the mesh

INTERFACE
  MODULE PURE FUNCTION obj_GetTotalFaces(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalFaces
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetTotalEdges@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: returns total number of Edges in the mesh

INTERFACE
  MODULE PURE FUNCTION obj_GetTotalEdges(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalEdges
END INTERFACE

!----------------------------------------------------------------------------
!                                     GetTotalBoundaryNodes@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: returns total number of boundary nodes in the mesh

INTERFACE
  MODULE FUNCTION obj_GetTotalBoundaryNodes(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalBoundaryNodes
END INTERFACE

!----------------------------------------------------------------------------
!                                  GetTotalBoundaryElements@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: returns total number of boundary elements

INTERFACE
  MODULE FUNCTION obj_GetTotalBoundaryElements(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalBoundaryElements
END INTERFACE

!----------------------------------------------------------------------------
!                                                  GetBoundingBox@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: returns bounding box of the mesh

INTERFACE
  MODULE FUNCTION obj_GetBoundingBox1(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    TYPE(BoundingBox_) :: ans
  END FUNCTION obj_GetBoundingBox1
END INTERFACE

!----------------------------------------------------------------------------
!                                            GetBoundingBox@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: returns bounding box of the mesh

INTERFACE
  MODULE FUNCTION obj_GetBoundingBox2(obj, nodes, local_nptrs) &
    RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: nodes(:, :)
    !! Nodal coordinates in XiJ format
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: local_nptrs(:)
    TYPE(BoundingBox_) :: ans
  END FUNCTION obj_GetBoundingBox2
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetConnectivity@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: This routine returns global node numbers in a given global elem
!
!# Introduction
!
! This returns the vertex connectivity

INTERFACE
  MODULE FUNCTION obj_GetConnectivity(obj, globalElement, islocal, opt) &
    RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: opt
    !! Vertex, Edge, Face, Cell, All
    !! Default is Vertex
  END FUNCTION obj_GetConnectivity
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetConnectivity@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-12
! summary: This routine returns global node numbers in a given global elem

INTERFACE
  MODULE SUBROUTINE obj_GetConnectivity1_(obj, globalElement, ans, tsize, &
                                          islocal, opt)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! vector of connectivity
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! total size of data written to ans
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! if islocal is present and true then globalElement is a local element
    CHARACTER(*), OPTIONAL, INTENT(IN) :: opt
    !! Vertex, Edge, Face, Cell
    !! Default is Vertex
  END SUBROUTINE obj_GetConnectivity1_
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetConnectivity@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-14
! summary:  Get connectivity

INTERFACE
  MODULE SUBROUTINE obj_GetConnectivity2_(obj, cellCon, faceCon, edgeCon, &
      nodeCon, tCellCon, tFaceCon, tEdgeCon, tNodeCon, globalElement, islocal)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(INOUT) :: cellCon(:)
    !! cell connectivity of element
    INTEGER(I4B), INTENT(INOUT) :: faceCon(:)
    !! face connectivity of element
    INTEGER(I4B), INTENT(INOUT) :: edgeCon(:)
    !! edge connectivity of element
    INTEGER(I4B), INTENT(INOUT) :: nodeCon(:)
    !! node connectivity of element
    INTEGER(I4B), INTENT(OUT) :: tCellCon
    !! size of data written in cellCon
    INTEGER(I4B), INTENT(OUT) :: tFaceCon
    !! size of data written in faceCon
    INTEGER(I4B), INTENT(OUT) :: tEdgeCon
    !! size of data written in edgecon
    INTEGER(I4B), INTENT(OUT) :: tnodeCon
    !! size of data written in nodecon
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global or local element number
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! if true then global element is local element
  END SUBROUTINE obj_GetConnectivity2_
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetOrientation@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-14
! summary:  Get the orientation flags of an element

INTERFACE
  MODULE SUBROUTINE obj_GetOrientation(obj, cellOrient, faceOrient, &
           edgeOrient, tCellOrient, tFaceOrient, tEdgeOrient, globalElement, &
                                       islocal)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(INOUT) :: cellOrient(:)
    !! cell connectivity of element
    INTEGER(I4B), INTENT(INOUT) :: faceOrient(:, :)
    !! face connectivity of element
    INTEGER(I4B), INTENT(INOUT) :: edgeOrient(:)
    !! edge connectivity of element
    INTEGER(I4B), INTENT(OUT) :: tCellOrient
    !! size of data written in cellCon
    INTEGER(I4B), INTENT(OUT) :: tFaceOrient(2)
    !! size of data written in faceCon
    !! tFaceOrient(1) is the number of rows in faceOrient
    !! tFaceOrient(2) is the number of columns in faceOrient
    INTEGER(I4B), INTENT(OUT) :: tEdgeOrient
    !! size of data written in edgecon
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global or local element number
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! if true then global element is local element
  END SUBROUTINE obj_GetOrientation
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetNodeConnectivity@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: This routine returns global node numbers in a given global elem
!
!# Introduction
!
! This routine returns the global node numbers (Vertex) connectivity
! of all elements of the mesh

INTERFACE
  MODULE SUBROUTINE obj_GetNodeConnectivity(obj, VALUE)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(INOUT) :: VALUE(:, :)
    !! The number of columns are equal to the total number of elements
    !! in the mesh, the number of rows equal to the maximum number of
    !! nodes in the elements of mesh
  END SUBROUTINE obj_GetNodeConnectivity
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetLocalNodeNumber@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: This function returns the local node number from global node num
!
!# Introduction
!
! This function returns the local node numbers from global node numbers.

INTERFACE
  MODULE FUNCTION obj_GetLocalNodeNumber1(obj, globalNode, islocal) &
    RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    INTEGER(I4B) :: ans(SIZE(globalNode))
  END FUNCTION obj_GetLocalNodeNumber1
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetLocalNoddeNumber_@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_GetLocalNodeNumber1_(obj, globalNode, ans, islocal)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
  END SUBROUTINE obj_GetLocalNodeNumber1_
END INTERFACE

!----------------------------------------------------------------------------
!                                         GetLocalNodeNumber@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: This routine returns the local node number from a global node number

INTERFACE
  MODULE FUNCTION obj_GetLocalNodeNumber2(obj, globalNode, islocal) &
    RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetLocalNodeNumber2
END INTERFACE

!----------------------------------------------------------------------------
!                                          GetGlobalNptrs@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: This function returns the Global node number from local node num

INTERFACE
  MODULE FUNCTION obj_GetGlobalNodeNumber1(obj, localNode) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: localNode(:)
    INTEGER(I4B) :: ans(SIZE(localNode))
  END FUNCTION obj_GetGlobalNodeNumber1
END INTERFACE

!----------------------------------------------------------------------------
!                                          GetGlobalNptrs@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns the Global node number from a local node number

INTERFACE
  MODULE FUNCTION obj_GetGlobalNodeNumber2(obj, localNode) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: localNode
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetGlobalNodeNumber2
END INTERFACE

!----------------------------------------------------------------------------
!                                       GetGlobalElemNumber@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: This function returns the Global node number from local node num

INTERFACE
  MODULE FUNCTION obj_GetGlobalElemNumber1(obj, LocalElement) &
    & RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: LocalElement(:)
    INTEGER(I4B) :: ans(SIZE(LocalElement))
  END FUNCTION obj_GetGlobalElemNumber1
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetGlobalElemNumber@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns the Global node number from a local node number

INTERFACE
  MODULE FUNCTION obj_GetGlobalElemNumber2(obj, LocalElement) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: LocalElement
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetGlobalElemNumber2
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetLocalElemNumber@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: This function returns the local element number

INTERFACE
  MODULE FUNCTION obj_GetLocalElemNumber1(obj, globalElement, islocal) &
    RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    INTEGER(I4B) :: ans(SIZE(globalElement))
  END FUNCTION obj_GetLocalElemNumber1
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetLocalElemNumber@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: This function returns the local element number

INTERFACE
  MODULE FUNCTION obj_GetLocalElemNumber2(obj, globalElement, islocal) &
    RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetLocalElemNumber2
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetNodeToElements@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns element numbers connected to the a global node
!
!# Introduction
!
! This function returns the global element numbers which are
! connected to the global node number `GlobalNode`
!
!@note
! If the node number `globalNode` is not present inside the mesh then
! the returned vector of integer has size 0
!@endnote

INTERFACE
  MODULE FUNCTION obj_GetNodeToElements1(obj, globalNode, islocal) &
    & RESULT(ans)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh data
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global node number
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !! A vector of local element number
  END FUNCTION obj_GetNodeToElements1
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetNodeToElements@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns element numbers which are connected to a global node
!
!# Introduction
!
! - This function returns the elements containing the global node number
! `globalNode`
! - The element number are global element numbers
! - Duplicate entries are removed
!
!@note
! If `globalNode` is not present inside the mesh then
! the returned vector of integer has size 0
!@endnote

INTERFACE
  MODULE FUNCTION obj_GetNodeToElements2(obj, globalNode, islocal) &
    RESULT(ans)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh data
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global node number
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !! A vector of local element number
  END FUNCTION obj_GetNodeToElements2
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetNodeToElements@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-03-28
! summary: returns the elements connected to a node

INTERFACE
  MODULE SUBROUTINE obj_GetNodeToElements1_(obj, ans, tsize, &
                                            globalNode, islocal)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! node to elements, it should be atleast tsize long
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! actual size of ans, it is returned by this routine
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global node number
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! is true it means globalNode is actually local node
  END SUBROUTINE obj_GetNodeToElements1_
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetNodeToElements@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-03-28
! summary: returns the elements connected to a node

INTERFACE
  MODULE SUBROUTINE obj_GetNodeToElements2_(obj, ans, tsize, &
                                            globalNode, islocal)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! node to elements, it should be atleast tsize long
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! actual size of ans, it is returned by this routine
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global node number
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! is true it means globalNode is actually local node
  END SUBROUTINE obj_GetNodeToElements2_
END INTERFACE

!----------------------------------------------------------------------------
!                                                  GetNodeToNodes@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary:         Returns the node surrounding a node
!
!# Introduction
! This fucntion returns the vector of node numbers which surrounds a given
! node number `globalNode`.
! - If `includeSelf` is true then, in the returned vector of integer,
! node number globalNode is also present
!- If `includeSelf` is false then, in the returned vector of integer,
! node number `globalNode` is not present
!
!@note
!  If the node number `globalNode` is not present in the mesh then the
! returned vector of integer has zero length
!@endnote

INTERFACE
  MODULE FUNCTION obj_GetNodeToNodes1(obj, globalNode, includeSelf, &
    & islocal) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    LOGICAL(LGT), INTENT(IN) :: includeSelf
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_GetNodeToNodes1
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetNodeToNodes@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns the node surrounding a node
!
!# Introduction
!
! This function returns the vector of node numbers which surrounds a given
! node number `globalNode`.
! - If `includeSelf` is true then, in the returned vector of integer,
! node number globalNode is also present
!- If `includeSelf` is false then, in the returned vector of integer,
! node number `globalNode` is not present
!
!@note
!  If the node number `globalNode` is not present in the mesh then the
! returned vector of integer has zero length
!@endnote

INTERFACE
  MODULE FUNCTION obj_GetNodeToNodes2(obj, globalNode, includeSelf,  &
    & islocal) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    LOGICAL(LGT), INTENT(IN) :: includeSelf
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_GetNodeToNodes2
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetNodeToNodes_@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-13
! summary: Returns the node surrounding a node
!
!# Introduction
! This fucntion returns the vector of node numbers which surrounds a given
! node number `globalNode`.
! - If `includeSelf` is true then, in the returned vector of integer,
! node number globalNode is also present
!- If `includeSelf` is false then, in the returned vector of integer,
! node number `globalNode` is not present
!
!@note
!  If the node number `globalNode` is not present in the mesh then the
! returned vector of integer has zero length
!@endnote

INTERFACE
  MODULE SUBROUTINE obj_GetNodeToNodes1_(obj, globalNode, includeSelf, &
    & ans, tsize, islocal)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    LOGICAL(LGT), INTENT(IN) :: includeSelf
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
  END SUBROUTINE obj_GetNodeToNodes1_
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetNodeToNodes@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns the node surrounding a node
!
!# Introduction
!
! This function returns the vector of node numbers which surrounds a given
! node number `globalNode`.
! - If `includeSelf` is true then, in the returned vector of integer,
! node number globalNode is also present
!- If `includeSelf` is false then, in the returned vector of integer,
! node number `globalNode` is not present
!
!@note
!  If the node number `globalNode` is not present in the mesh then the
! returned vector of integer has zero length
!@endnote

INTERFACE
  MODULE SUBROUTINE obj_GetNodeToNodes2_(obj, globalNode, includeSelf,  &
    & ans, tsize, islocal)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    LOGICAL(LGT), INTENT(IN) :: includeSelf
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
  END SUBROUTINE obj_GetNodeToNodes2_
END INTERFACE

!----------------------------------------------------------------------------
!                                      GetElementToElements@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns element to element connectivity information
!
!# Introduction
! This routine returns element to element connectivity information
! for a given global element number `globalElement`
!
! If `OnlyElements` is absent or it is Set to FALSE then, this routine
! returns the **full information** about elements surrounding the global
! element `globalElement`. In this case,
!
! - Each Row of `ans` denotes the element to which `globalElement` is
! connected to
! - Column-1 of `ans` denotes global element number of the neighbour
! - Column-2 denotes the local face number of element `globalElement`
! - Column-3 denotes the local face number of global element given by
! the column number 1 (same row)
!
! If `OnlyElements` is present and it is TRUE then, this routine returns
! only the global element numbers surrouding
! the given element `globalElement`
!

INTERFACE
  MODULE FUNCTION obj_GetElementToElements(obj, globalElement, &
                                           onlyElements, islocal) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh data
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! Global element number
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: onlyElements
    !! If onlyElements is absent or it is FALSE then full information
    !! about the elements connected to element iel is given
    !! If onlyElements is present and it is TRUE then only the
    !! information about the elements connected to element iel is given
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
    !! list of elements surrounding elements
  END FUNCTION obj_GetElementToElements
END INTERFACE

!----------------------------------------------------------------------------
!                                      GetElementToElements@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-05-22
! summary: Returns element to element connectivity information
!
!# Introduction
!
! This routine returns element to element connectivity information
! for a given global element number `globalElement`
!
! This routine returns only the global element numbers surrouding
! the given element `globalElement`

INTERFACE
  MODULE SUBROUTINE obj_GetElementToElements1_(obj, ans, tsize, &
                                               globalElement, islocal)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! list of elements surrounding elements
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! Size of data written to ans
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! Global element number
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! If islocal is present and true then globalElement is a local element
  END SUBROUTINE obj_GetElementToElements1_
END INTERFACE

!----------------------------------------------------------------------------
!                                      GetElementToElements@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-05-22
! summary: Returns element to element connectivity information
!
!# Introduction
!
! This routine returns element to element connectivity information
! for a given global element number `globalElement`
!
! It returns the **full information** about elements surrounding the global
! element `globalElement`. In this case,
!
! - Each Row of `ans` denotes the element to which `globalElement` is
! connected to
! - Column-1 of `ans` denotes global element number of the neighbour
! - Column-2 denotes the local face number of element `globalElement`
! - Column-3 denotes the local face number of global element given by
! the column number 1 (same row)

INTERFACE
  MODULE SUBROUTINE obj_GetElementToElements2_(obj, ans, nrow, ncol, &
                                               globalElement, islocal)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh
    INTEGER(I4B), INTENT(INOUT) :: ans(:, :)
    !! list of elements surrounding elements
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! number of rows written to ans
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! number of columns written to ans
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! Global element number
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! If islocal is present and true then globalElement is a local element
  END SUBROUTINE obj_GetElementToElements2_
END INTERFACE

!----------------------------------------------------------------------------
!                                          GetBoundaryElementData@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: This routine returns the boundary element data
!
!# Introduction
!
! If the given element number is a boundary element, then this routine
! returns the boundry element data.
! It contains the local index of facet element which is a boundary.
!
! For example:
! - If `iel` is a boundary element, then boudnaryData contains
! the local facet number of iel which concides with the mesh boundary.
! - If an element contains the boundary node then it is considered
! as a boundary element.
! - It may happen that a boundary element has no boundary face, in which case
! boundaryData will have zero size
!
!@note
! If a given globalElement is not a boundary element, then the returned
! array has zero size.
!@endnote

INTERFACE
  MODULE FUNCTION obj_GetBoundaryElementData(obj, globalElement, islocal) &
    & RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_GetBoundaryElementData
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetOrder@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns the order of reference element

INTERFACE
  MODULE FUNCTION obj_GetOrder1(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetOrder1
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetOrder@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-29
! summary:  Get the order of element

INTERFACE
  MODULE FUNCTION obj_GetOrder2(obj, globalElement, islocal) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global or local element number
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! if true then global element is local element
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetOrder2
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetNSD@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns the spatial dimension of the mesh

INTERFACE
  MODULE FUNCTION obj_GetNSD(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetNSD
END INTERFACE

!----------------------------------------------------------------------------
!                                                  GetXidimension@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns the xidimension of the mesh

INTERFACE
  MODULE FUNCTION obj_GetXidimension(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetXidimension
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetMaterial@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns the materials id of a given medium

INTERFACE
  MODULE FUNCTION obj_GetMaterial1(obj, medium, globalElement, islocal) &
    RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: medium
    INTEGER(I4B), INTENT(IN) :: globalElement
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetMaterial1
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetMaterial@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns the materials id of a given medium

INTERFACE
  MODULE FUNCTION obj_GetTotalMaterial1(obj, globalElement, islocal) &
    RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalMaterial1
END INTERFACE

!----------------------------------------------------------------------------
!                                           GetTotalFacetElements@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns the total number of facet elements
!
!# Introduction
!
! This function returns the total number of facet element in the mesh.
! It includes
! - InternalFacet Elements
! - DomainFacet Elements
! - MeshFacet Elements

INTERFACE
  MODULE FUNCTION obj_GetTotalFacetElements(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalFacetElements
END INTERFACE

!----------------------------------------------------------------------------
!                                   GetTotalInternalFacetElements@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns the total number of internal facets element in mesh

INTERFACE
  MODULE FUNCTION obj_GetTotalInternalFacetElements(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalInternalFacetElements
END INTERFACE

!----------------------------------------------------------------------------
!                                   GetTotalBoundaryFacetElements@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_GetTotalBoundaryFacetElements(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalBoundaryFacetElements
END INTERFACE

!----------------------------------------------------------------------------
!                                            GetMasterCellNumber@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns the master cell number of a given facet
!
!# Introduction
!
! - This routine returns the master cell number of a given facet element
! - The facetElement is the local facet element
! - The master cell number is the global element number

INTERFACE
  MODULE FUNCTION obj_GetMasterCellNumber(obj, facetElement, elementType) &
    RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: facetElement
    INTEGER(I4B), INTENT(IN) :: elementType
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetMasterCellNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                            GetSlaveCellNumber@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns the slave cell number of a given facet
!
!# Introduction
!
! - This routine returns the slave cell number of a given facet element
! - The facetElement is the local facet element
! - The slave cell number is the global element number

INTERFACE
  MODULE FUNCTION obj_GetSlaveCellNumber(obj, facetElement, elementType) &
    RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: facetElement
    INTEGER(I4B), INTENT(IN) :: elementType
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetSlaveCellNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetCellNumber@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns the  cell number of a given facet
!
!# Introduction
!
! - This routine returns the  cell number of a given facet element
! - The facetElement is the local facet element
! - The  cell number is the global element number
! - ans(1)  contains the master-cell number
! - ans(2)  contains the slave cell number

INTERFACE
  MODULE FUNCTION obj_GetCellNumber(obj, facetElement, elementType) &
    RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: facetElement
    INTEGER(I4B), INTENT(IN) :: elementType
    INTEGER(I4B) :: ans(2)
  END FUNCTION obj_GetCellNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetLocalFacetID@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns the local facet id

INTERFACE
  MODULE FUNCTION obj_GetLocalFacetID(obj, facetElement, elementType, &
                                      isMaster) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: facetElement
    INTEGER(I4B), INTENT(IN) :: elementType
    LOGICAL(LGT), INTENT(IN) :: isMaster
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetLocalFacetID
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetGlobalFaceNumber@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-21
! summary:  Get global face number from globalElement and local face number

INTERFACE
MODULE FUNCTION obj_GetGlobalFaceNumber(obj, globalElement, localFaceNumber, &
                                          islocal) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! local or global element number
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! local face number in global element
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! if true then global element is local element
    INTEGER(I4B) :: ans
    !! global face number
  END FUNCTION obj_GetGlobalFaceNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetGlobalEdgeNumber@GetMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION obj_GetGlobalEdgeNumber(obj, globalElement, localEdgeNumber, &
                                          islocal) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! local or global element number
    INTEGER(I4B), INTENT(IN) :: localEdgeNumber
    !! local Edge number in global element
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! if true then global element is local element
    INTEGER(I4B) :: ans
    !! global Edge number
  END FUNCTION obj_GetGlobalEdgeNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                                       FindFace@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-20
! summary:  Find a face in a given element

INTERFACE
  MODULE SUBROUTINE obj_FindFace(obj, globalElement, faceCon, isFace, &
                                localFaceNumber, onlyBoundaryElement, islocal)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! abstract mesh
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global or local elem number
    INTEGER(I4B), INTENT(IN) :: faceCon(:)
    !! vertex connectivity of face
    LOGICAL(LGT), INTENT(OUT) :: isFace
    !! if faceCon is a face of globalElement then it is true, else false
    INTEGER(I4B), INTENT(OUT) :: localFaceNumber
    !! local face number if found, else 0
    LOGICAL(LGT), INTENT(IN) :: onlyBoundaryElement
    !! if true then we will search if the element is boundary element
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! if true then global element is local element
  END SUBROUTINE obj_FindFace
END INTERFACE

!----------------------------------------------------------------------------
!                                                       FindEdge@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:   2024-07-20
! summary:  Find edge in a cell
!
!# Introduction
!
! Call this routine only for 3D mesh

INTERFACE
  MODULE SUBROUTINE obj_FindEdge(obj, globalElement, edgeCon, isEdge, &
                                localEdgeNumber, onlyBoundaryElement, islocal)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! abstract mesh
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global or local elem number
    INTEGER(I4B), INTENT(IN) :: edgeCon(:)
    !! vertex connectivity of Edge
    LOGICAL(LGT), INTENT(OUT) :: isEdge
    !! if EdgeCon is a Edge of globalElement then it is true, else false
    INTEGER(I4B), INTENT(OUT) :: localEdgeNumber
    !! local Edge number if found, else 0
    LOGICAL(LGT), INTENT(IN) :: onlyBoundaryElement
    !! if true then we will search if the element is boundary element
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! if true then global element is local element
  END SUBROUTINE obj_FindEdge
END INTERFACE

!----------------------------------------------------------------------------
!                                           GetFacetConnectivity@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns the connectivity of a facet element
!
!# Introduction
!
! - Returns the connectivity of a given facet element
! - facetElement is local facet element number

INTERFACE
  MODULE FUNCTION AbstractMeshGetFacetConnectivity(obj, facetElement, &
                                            elementType, isMaster) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: facetElement
    INTEGER(I4B), INTENT(IN) :: elementType
    LOGICAL(LGT), INTENT(IN) :: isMaster
      !! if isMaster is true then connectivity of facet in master-cell
      !! is returned, otherwise connectivity of facet in slave-cell
      !! is returned. This is only applicable for internal facet element
      !! because for domain facet we do not have slave-cell.
      !! Currently, we do not support slave-cell for meshFacet because
      !! the slave of meshFacet lives in different instance of obj_
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION AbstractMeshGetFacetConnectivity
END INTERFACE

!----------------------------------------------------------------------------
!                                           GetFacetConnectivity@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns the connectivity of a facet element of a cellElement
!
!# Introduction
!
! - Returns the connectivity of a given facet element of a cellElement
! - globalElement is global element number of cell number
! - iface is the local face number in globalElement

INTERFACE
  MODULE FUNCTION obj_GetFacetConnectivity(obj, globalElement, &
                                           iface, islocal) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    INTEGER(I4B), INTENT(IN) :: iface
    LOGICAL(I4B), OPTIONAL, INTENT(IN) :: islocal
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_GetFacetConnectivity
END INTERFACE

!----------------------------------------------------------------------------
!                                            GetFacetElementType@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns the facet element type of the cell element number

INTERFACE
  MODULE FUNCTION obj_GetFacetElementType(obj, globalElement, islocal) &
    RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_GetFacetElementType
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetQuery@GetMethods
!----------------------------------------------------------------------------

INTERFACE AbstractMeshGetParam
  MODULE SUBROUTINE obj_GetParam(obj, &
             isInitiated, isNodeToElementsInitiated, isNodeToNodesInitiated, &
                  isExtraNodeToNodesInitiated, isElementToElementsInitiated, &
                         isBoundaryDataInitiated, isFacetDataInitiated, uid, &
                                 xidim, elemType, nsd, maxNptrs, minNptrs, &
                                 maxElemNum, minElemNum, tNodes, tElements, &
                                 minX, minY, minZ, maxX, maxY, maxZ, &
            x, y, z, tElements_topology_wise, tElemTopologies, elemTopologies)

    CLASS(AbstractMesh_), INTENT(IN) :: obj
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isInitiated, &
                          isNodeToElementsInitiated, isNodeToNodesInitiated, &
                  isExtraNodeToNodesInitiated, isElementToElementsInitiated, &
                                 isBoundaryDataInitiated, isFacetDataInitiated

    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: uid, xidim, elemType, nsd, &
                         maxNptrs, minNptrs, maxElemNum, minElemNum, tNodes, &
     tElements, tElements_topology_wise(8), tElemTopologies, elemTopologies(8)

    REAL(DFP), OPTIONAL, INTENT(OUT) :: minX, minY, minZ, maxX, maxY, maxZ, &
                                        x, y, z
  END SUBROUTINE obj_GetParam
END INTERFACE AbstractMeshGetParam

!----------------------------------------------------------------------------
!                                                GetMinElemNumber@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_GetMinElemNumber(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetMinElemNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetMaxElemNumber@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_GetMaxElemNumber(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetMaxElemNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetMinNodeNumber@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_GetMinNodeNumber(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetMinNodeNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetMaxNodeNumber@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_GetMaxNodeNumber(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetMaxNodeNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                                         isInit@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_isInit(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_isInit
END INTERFACE

!----------------------------------------------------------------------------
!                                                         isInit@GetMethods
!----------------------------------------------------------------------------

! function for obj_isNodeToElements

INTERFACE
  MODULE FUNCTION obj_isNodeToElements(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_isNodeToElements
END INTERFACE

!----------------------------------------------------------------------------
!                                                         isInit@GetMethods
!----------------------------------------------------------------------------
! function obj_isNodeToNodes
INTERFACE
  MODULE FUNCTION obj_isNodeToNodes(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_isNodeToNodes
END INTERFACE

!----------------------------------------------------------------------------
!                                                         isInit@GetMethods
!----------------------------------------------------------------------------

! function obj_isExtraNodeToNodes
INTERFACE
  MODULE FUNCTION obj_isExtraNodeToNodes(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_isExtraNodeToNodes
END INTERFACE

!----------------------------------------------------------------------------
!                                                         isInit@GetMethods
!----------------------------------------------------------------------------

! function obj_isElementToElements
INTERFACE
  MODULE FUNCTION obj_isElementToElements(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(in) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_isElementToElements
END INTERFACE

!----------------------------------------------------------------------------
!                                                         isInit@GetMethods
!----------------------------------------------------------------------------

! function obj_isEdgeConnectivity
INTERFACE
  MODULE FUNCTION obj_isEdgeConnectivity(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(in) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_isEdgeConnectivity
END INTERFACE

!----------------------------------------------------------------------------
!                                                         isInit@GetMethods
!----------------------------------------------------------------------------
!function obj_isFaceConnectivity
INTERFACE
  MODULE FUNCTION obj_isFaceConnectivity(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(in) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_isFaceConnectivity
END INTERFACE

!----------------------------------------------------------------------------
!                                                         isInit@GetMethods
!----------------------------------------------------------------------------
! function obj_boundaryData
INTERFACE
  MODULE FUNCTION obj_isBoundaryData(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(in) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_isBoundaryData
END INTERFACE

!----------------------------------------------------------------------------
!                                                         isInit@GetMethods
!----------------------------------------------------------------------------
! function obj_isfacetData
INTERFACE
  MODULE FUNCTION obj_isFacetData(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(in) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_isFacetData
END INTERFACE

!----------------------------------------------------------------------------
!                                                 isElementActive@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_isElementActive(obj, globalElement, islocal) &
    RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    LOGICAL(LGT) :: ans
  END FUNCTION obj_isElementActive
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetFacetParam@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-04-14
! summary:  Get the parameters of facet elements

INTERFACE
  MODULE SUBROUTINE obj_GetFacetParam(obj, facetElement, elementType)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: facetElement
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: elementType
  END SUBROUTINE obj_GetFacetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetTotalEntities@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-15
! summary: Get total entities of a globalElement
!
! # Introduction
!
! ans(1) =  total number of nodes in element
! ans(2) =  total number of edges in element
! ans(3) =  total number of faces in element
! ans(4) = 1

INTERFACE
  MODULE FUNCTION obj_GetTotalEntities1(obj, globalElement, islocal) &
    RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    INTEGER(I4B) :: ans(4)
  END FUNCTION obj_GetTotalEntities1
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetTotalEntities@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-15
! summary: Get total entities of the mesh
!
!# Introduction
!
! ans(1) = total number of nodes
! ans(2) = total number of edges
! ans(3) = total number of faces
! ans(4) = total number of cell elements

INTERFACE
  MODULE FUNCTION obj_GetTotalEntities2(obj) &
    RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans(4)
  END FUNCTION obj_GetTotalEntities2
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetMaxNodeToElements@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-17
! summary: Return maximum size of node to elements

INTERFACE
  MODULE FUNCTION obj_GetMaxNodeToElements(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetMaxNodeToElements
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetMaxNodeToNodes@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-17
! summary: Return maximum size of node to nodes

INTERFACE
  MODULE FUNCTION obj_GetMaxNodeToNodes(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetMaxNodeToNodes
END INTERFACE

!----------------------------------------------------------------------------
!                                         GetMaxElementToElements@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-17
! summary: Return maximum size of element to elements

INTERFACE
  MODULE FUNCTION obj_GetMaxElementToElements(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetMaxElementToElements
END INTERFACE

!----------------------------------------------------------------------------
!                                           InitiateKdtree@NodeDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-10
! summary: Initiate the kd tree

INTERFACE
  MODULE SUBROUTINE obj_InitiateKdtree(obj)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_InitiateKdtree
END INTERFACE

!----------------------------------------------------------------------------
!                                     InitiateNodeToElements@NodeDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: generate Elements surrounding a node mapping
!
!# Introduction
!
! - This subroutine generates elements surrounding a node mapping, in other
! words it generates node to element
! - Element numbers returned by this routine are global element number.
! - This mapping is stored inside `obj%nodeData` array
! - For a local node number `ii`, `obj%nodeData(ii)%globalElements(:)`
! contains the global element numbers.
!
!@note
! Always use method called `GetNodeToElements()` to access this information.
! This methods requires global Node number
!@endnote
!
!@warning
! Always use the mapping between global node number and local node number to
! avoid segmentation fault
!@endwarning

INTERFACE
  MODULE SUBROUTINE obj_InitiateNodeToElements(obj)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_InitiateNodeToElements
END INTERFACE

!----------------------------------------------------------------------------
!                                         InitiateNodeToNode@NodeDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Initiate node to node connectivity data
!
!# Introduction
!
!- This routine generate the node to nodes mapping
!- In other words, it generates info of node-numbers in mesh
! surrounding a node number
!- This mapping is stored inside `obj%nodeData%globalNode`
!- For a local node number i, obj%nodeData(i)%globalNode denotes the
! global node data surrounding the local node number.
!- This list does not include self node.
!- This methods needs node-to-elements data, therefore if this data
! is not Initiated, then this method calls `InitiateNodeToElements()`
!

INTERFACE
  MODULE SUBROUTINE obj_InitiateNodetoNodes(obj)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh data
  END SUBROUTINE obj_InitiateNodetoNodes
END INTERFACE

!----------------------------------------------------------------------------
!                                   InitiateExtraNodeToNode@NodeDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Initiate node to node connectivity data
!
!# Introduction
!
!- This routine generate the node to nodes mapping
!- This mapping is stored inside `obj%nodeData%extraGlobalNodeNum`
!- For a local node number i,`obj%nodeData(i)%ExtraGlobalNodeNum` denotes the
! global node data surrounding the local node number used for edge-based
!  stabilization. This list does not include self node.
!
!- This methods needs information about `nodeToNodes`, `nodeToElements`,
! and `elementToElements`. Therefore,
!- If `nodeToNodes` is not Initiated, then this method initiates it.
!- If `nodeToElements` is not Initiated, then this method initiates it.
!- If `elementToElements` is not Initiated, then this method initiates it.

INTERFACE
  MODULE SUBROUTINE obj_InitiateExtraNodetoNodes(obj)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh data
  END SUBROUTINE obj_InitiateExtraNodetoNodes
END INTERFACE

!----------------------------------------------------------------------------
!                               InitiateElementToElements@ElementDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:  2024-01-27
! summary: Initiate element to element data
!
!# Introduction
!
!- This routine creates the element surrounding a given element data
!- For 1D element it calls InitiateElementToElements1D,
!- For 2D and 3D element it calls InitiateElementToElements3D
!- In the end it calls MarkInternalNodes routine, to identify internal nodes
!- If ElementToElements is already initiated then it does nothing
!
! In the case of 2D and 3D mesh, this routine will also form the
! the globalFaces of element data.

INTERFACE
  MODULE SUBROUTINE obj_InitiateElementToElements(obj)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh data
  END SUBROUTINE obj_InitiateElementToElements
END INTERFACE

!----------------------------------------------------------------------------
!                                  InitiateBoundaryData@BoundaryDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Initiate boundary data
!
!# Introduction
!
! This method construct the boundary element data.
! It marks elements of mesh as BOUNDARY_ELEMENT and INTERNAL_ELEMENT
! In this case boundary elements are those which contains the boundary node.
! Boundary node information is available by reading the mesh file, see
! mesh import method.
! It also forms `obj%elementData(ii)%boundaryData`
!
! This methods needs following information:
!
!- `ElementToElements`
!- `refelem` to construct the FacetElements

INTERFACE
  MODULE SUBROUTINE obj_InitiateBoundaryData(obj)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh data
  END SUBROUTINE obj_InitiateBoundaryData
END INTERFACE

!----------------------------------------------------------------------------
!                                   InitiateEdgeConnectivity@EdgeDataMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-03-07
! summary:  Initiate edge data in elemData

INTERFACE
  MODULE SUBROUTINE obj_InitiateEdgeConnectivity(obj)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh data
  END SUBROUTINE obj_InitiateEdgeConnectivity
END INTERFACE

!----------------------------------------------------------------------------
!                                   InitiateFaceConnectivity@FaceDataMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-03-07
! summary:  Initiate edge data in elemData

INTERFACE
  MODULE SUBROUTINE obj_InitiateFaceConnectivity(obj)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh data
  END SUBROUTINE obj_InitiateFaceConnectivity
END INTERFACE

!----------------------------------------------------------------------------
!                                     InitiateFacetElements@FacetDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Compute the total number of facet elements in the mesh
!
!# Introduction
!
! This routine needs following information:
!
!- `ElementToElements`
!- `BoundaryData`
!
! It makes following data
!
!- `InternalFacetData`
!- `BoundaryFacetData`
!- `FacetElementType`
!
! Note that at this point all boundaryFacet element are of type
! `DOMAIN_BOUNDARY_ELEMENT`. This information can be corrected only when
! we call SetFacetElementType from Domain_ class. This is because,
! at this point we only know that a boundary facet is a domain boundary
! element, as we have no information about the neighbouring mesh.

INTERFACE
  MODULE SUBROUTINE obj_InitiateFacetElements(obj)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_InitiateFacetElements
END INTERFACE

!----------------------------------------------------------------------------
!                                                   SetShowTime@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-31
! summary:  Set the showTime

INTERFACE
  MODULE SUBROUTINE obj_SetShowTime(obj, VALUE)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    LOGICAL(LGT), INTENT(IN) :: VALUE
  END SUBROUTINE obj_SetShowTime
END INTERFACE

!----------------------------------------------------------------------------
!                                                  SetBoundingBox@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Sets the bounding box information in the mesh

INTERFACE
  MODULE SUBROUTINE obj_SetBoundingBox1(obj, box)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    TYPE(BoundingBox_), INTENT(IN) :: box
  END SUBROUTINE obj_SetBoundingBox1
END INTERFACE

!----------------------------------------------------------------------------
!                                                  SetBoundingBox@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Sets the bounding box information in the mesh

INTERFACE
  MODULE SUBROUTINE obj_SetBoundingBox2(obj, nodes, local_nptrs)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: nodes(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: local_nptrs(:)
  END SUBROUTINE obj_SetBoundingBox2
END INTERFACE

!----------------------------------------------------------------------------
!                                                      SetSparsity@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: This routine Set the sparsity pattern in [[CSRMatrix_]] object
!
!# Introduction
!
! This routine Sets the sparsity pattern in [[CSRMatrix_]] object.

INTERFACE
  MODULE SUBROUTINE obj_SetSparsity1(obj, mat, localNodeNumber, lbound, &
                                     ubound)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! [[Mesh_]] class
    TYPE(CSRMatrix_), INTENT(INOUT) :: mat
    !! [[CSRMatrix_]] object
    INTEGER(I4B), INTENT(IN) :: lbound
    !! lower bound of localNodeNumber
    INTEGER(I4B), INTENT(IN) :: ubound
    !! upper bound of localNodeNumber
    INTEGER(I4B), INTENT(IN) :: localNodeNumber(lbound:ubound)
    !! Global to local node number map
  END SUBROUTINE obj_SetSparsity1
END INTERFACE

!----------------------------------------------------------------------------
!                                                SetSparsity@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: This routine Set the sparsity pattern in [[CSRMatrix_]] object

INTERFACE
  MODULE SUBROUTINE obj_SetSparsity2(obj, mat)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! Mesh_ class
    TYPE(CSRMatrix_), INTENT(INOUT) :: mat
    !! CSRMatrix object
  END SUBROUTINE obj_SetSparsity2
END INTERFACE

!----------------------------------------------------------------------------
!                                                      SetSparsity@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: This routine Set the sparsity pattern in [[CSRMatrix_]] object
!
!# Introduction
!
! This routine Sets the sparsity pattern in [[CSRMatrix_]] object.

INTERFACE
  MODULE SUBROUTINE obj_SetSparsity3(obj, colMesh, nodeToNode, mat, &
    & ivar, jvar)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! Abstract mesh class
    CLASS(AbstractMesh_), INTENT(INOUT) :: colMesh
    !! Abstract mesh class
    INTEGER(I4B), INTENT(IN) :: nodeToNode(:)
    !! Node to node connectivity between obj and colMesh
    TYPE(CSRMatrix_), INTENT(INOUT) :: mat
    !! [[CSRMatrix_]] object
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable in row
    INTEGER(I4B), INTENT(IN) :: jvar
    !! physical variable in column
  END SUBROUTINE obj_SetSparsity3
END INTERFACE

!----------------------------------------------------------------------------
!                                                      SetSparsity@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: This routine Set the sparsity pattern in [[CSRMatrix_]] object
!
!# Introduction
!
! This routine Sets the sparsity pattern in [[CSRMatrix_]] object.

INTERFACE
  MODULE SUBROUTINE obj_SetSparsity4(obj, colMesh, nodeToNode, mat, &
     rowGlobalToLocalNodeNum, rowLBOUND, rowUBOUND, colGlobalToLocalNodeNum, &
                                     colLBOUND, colUBOUND, ivar, jvar)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! [[AbstractMesh_]] class
    CLASS(AbstractMesh_), INTENT(INOUT) :: colMesh
    !! [[AbstractMesh_]] class
    INTEGER(I4B), INTENT(IN) :: nodeToNode(:)
    !! node to node connectivity between obj and colMesh
    TYPE(CSRMatrix_), INTENT(INOUT) :: mat
    !! [[CSRMatrix_]] object
    INTEGER(I4B), INTENT(IN) :: rowLBOUND
    INTEGER(I4B), INTENT(IN) :: rowUBOUND
    !! lower bound of rowGlobalToLocalNodeNum
    INTEGER(I4B), INTENT(IN) :: rowGlobalToLocalNodeNum( &
                                rowLBOUND:rowUBOUND)
    !! Global to local node number map
    INTEGER(I4B), INTENT(IN) :: colLBOUND
    INTEGER(I4B), INTENT(IN) :: colUBOUND
    INTEGER(I4B), INTENT(IN) :: colGlobalToLocalNodeNum( &
      & colLBOUND:colUBOUND)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
  END SUBROUTINE obj_SetSparsity4
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetMaterial@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Set the materials id of a given medium

INTERFACE
  MODULE SUBROUTINE obj_SetTotalMaterial1(obj, n, globalElement, islocal)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: n
    INTEGER(I4B), INTENT(IN) :: globalElement
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
  END SUBROUTINE obj_SetTotalMaterial1
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetMaterial@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Set the materials id of a given medium

INTERFACE
  MODULE SUBROUTINE obj_SetTotalMaterial2(obj, n)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: n
  END SUBROUTINE obj_SetTotalMaterial2
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetMaterial@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-12-09
! update: 2021-12-09
! summary: Set the materials id of a given medium
!
!# Introduction
!
! if obj%elementData(ii)%ptr%meshID is equal to entityNum then
! set %material(medium) = material

INTERFACE
  MODULE SUBROUTINE obj_SetMaterial1(obj, entityNum, medium, material)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: entityNum
    !! entity number
    INTEGER(I4B), INTENT(IN) :: medium
    !! medium number (like soil, water)
    INTEGER(I4B), INTENT(IN) :: material
    !! type of medium like clay, sand, water1, water2
  END SUBROUTINE obj_SetMaterial1
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetMaterial@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Set the materials id of a given medium

INTERFACE
  MODULE SUBROUTINE obj_SetMaterial2(obj, medium, material)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: medium
    INTEGER(I4B), INTENT(IN) :: material
  END SUBROUTINE obj_SetMaterial2
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetMaterial@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-12-09
! update: 2021-12-09
! summary: Set the materials id of a given medium

INTERFACE
  MODULE SUBROUTINE obj_SetMaterial3(obj, medium, material, globalElement, &
                                     islocal)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: medium
    !! medium number (like soil, water)
    INTEGER(I4B), INTENT(IN) :: material
    !! type of medium like clay, sand, water1, water2
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! is global element local
    !! we cannot keep it optional for unique interface
  END SUBROUTINE obj_SetMaterial3
END INTERFACE

!----------------------------------------------------------------------------
!                                            SetFacetElementType@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Set the facet element type of a given cell number

INTERFACE
  MODULE SUBROUTINE obj_SetFacetElementType(obj, globalElement, &
                                            iface, facetElementType, islocal)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    INTEGER(I4B), INTENT(IN) :: iface
    INTEGER(I4B), INTENT(IN) :: facetElementType
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
  END SUBROUTINE obj_SetFacetElementType
END INTERFACE

!----------------------------------------------------------------------------
!                                                   SetQuality@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-27
! summary:  Set mesh quality

INTERFACE
  MODULE SUBROUTINE obj_SetQuality(obj, measures, max_measures, &
    & min_measures, nodeCoord, local_nptrs)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: measures(:)
    REAL(DFP), INTENT(OUT) :: max_measures(:)
    REAL(DFP), INTENT(OUT) :: min_measures(:)
    REAL(DFP), INTENT(IN) :: nodeCoord(:, :)
    INTEGER(I4B), INTENT(IN) :: local_nptrs(:)
  END SUBROUTINE obj_SetQuality
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetParam@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-21
! summary: Set param

INTERFACE
  MODULE SUBROUTINE obj_SetParam(obj, &
             isInitiated, isNodeToElementsInitiated, isNodeToNodesInitiated, &
                  isExtraNodeToNodesInitiated, isElementToElementsInitiated, &
                         isBoundaryDataInitiated, isFacetDataInitiated, uid, &
                                 xidim, elemType, nsd, maxNptrs, minNptrs, &
                                 maxElemNum, minElemNum, tNodes, tElements, &
                                 minX, minY, minZ, maxX, maxY, maxZ, &
            x, y, z, tElements_topology_wise, tElemTopologies, elemTopologies)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isInitiated, &
      & isNodeToElementsInitiated, isNodeToNodesInitiated, &
      & isExtraNodeToNodesInitiated, isElementToElementsInitiated, &
      & isBoundaryDataInitiated, isFacetDataInitiated

    INTEGER(I4B), OPTIONAL, INTENT(IN) :: uid, &
      & xidim, elemType, nsd, maxNptrs, minNptrs, &
      & maxElemNum, minElemNum, tNodes, &
      & tElements, tElements_topology_wise(8), tElemTopologies,  &
      & elemTopologies(8)

    REAL(DFP), OPTIONAL, INTENT(IN) :: minX, &
      & minY, minZ, maxX, maxY, maxZ, &
      & x, y, z
  END SUBROUTINE obj_SetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                          SetBoundaryFacetParam@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-04-14
! summary:  Set the parameters of facet elements

INTERFACE
  MODULE SUBROUTINE obj_SetFacetParam(obj, facetElement, elementType)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: facetElement
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: elementType
  END SUBROUTINE obj_SetFacetParam
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE AbstractMesh_Class
