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

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Abstract mesh class
!
! This module defines AbstractMesh_, the abstract class for finite element
! meshes, and AbstractMeshPointer_.

MODULE AbstractMesh_Class
USE GlobalData, ONLY: LGT, I4B, DFP
USE Files, ONLY: HDF5File_
USE files, ONLY: VTKFile_
USE Basetype, ONLY: BoundingBox_
USE BaseType, ONLY: CSRMatrix_
USE BaseType, ONLY: TypeMeshOpt
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE CPUTime_Class, ONLY: CPUTime_
USE ElemData_Class, ONLY: ElemData_
USE ElemData_Class, ONLY: ElemDataPointer_
USE ElemDataBinaryTree_Class, ONLY: ElemDataBinaryTree_
USE ElemDataList_Class, ONLY: ElemDataList_
USE NodeData_Class, ONLY: NodeData_, NodeDataPointer_
USE NodeDataList_Class, ONLY: NodeDataList_
USE NodeDataBinaryTree_Class, ONLY: NodeDataBinaryTree_
USE FacetData_Class, ONLY: FacetData_
USE String_Class, ONLY: String
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

!----------------------------------------------------------------------------
!                                                             AbstractMesh_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Abstract class for meshes
!
! AbstractMesh_ holds the elements of one dimension (points, curves, surfaces
! or volumes) of a mesh, read from a HDF5 mesh file. FEMesh_ is a concrete
! mesh.
!
! Numbering:
!
! - Global element and node numbers are the numbers in the mesh file.
! - Local node numbers follow the order of the global node numbers;
!   local_Nptrs(global) gives the local number.
! - Local element numbers follow the order of the elements in the file;
!   local_elemNumber(global) gives the local number.
! - Most methods take a global number; with islocal = .TRUE. they take a local
!   number.
!
! Data:
!
! - nodeData(:) stores, for each local node, its global number, type (internal
!   or boundary), coordinates, and the node to elements and node to nodes
!   data.
! - elementData(:) stores, for each local element, its global number,
!   connectivity, mesh id, neighbours, boundary data, edges, faces and
!   materials.
! - facetData(:) stores the facet elements.
!
! Initiate reads the mesh and initiates the element to elements data, which
! marks boundary elements and boundary nodes and numbers the faces. The other
! data is initiated on demand by the Initiate... methods (node to elements,
! node to nodes, extra node to nodes, boundary data, edges, facet elements,
! kdtree).
!
! See also: [[obj_Initiate]]

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
  INTEGER(I4B) :: maxElemOrder = 0
  !! largest order of the elements in the mesh
  !! It is computed and stored by GetOrder()
  LOGICAL(LGT) :: isMaxElemOrderSet = .FALSE.
  !! True if maxElemOrder has been computed
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
  !! total number of edges inside the mesh
  INTEGER(I4B) :: tFaces = 0
  !! total number of faces inside the mesh
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
  !! kdtree of the node coordinates

  TYPE(Kdtree2Result_), ALLOCATABLE :: kdresult(:)
  !! results of kdtree searches

CONTAINS
  ! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  !! Initiate the mesh from a HDF5 mesh file
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Deallocate the data of the mesh
  PROCEDURE, PUBLIC, PASS(obj) :: isEmpty => obj_isEmpty
  !! Returns true if the mesh is empty
  PROCEDURE, PUBLIC, PASS(obj) :: InitiateDynamicDataStructure => &
    obj_InitiateDynamicDataStructure
  !! Initiate the dynamic data structure of the mesh
  PROCEDURE, PUBLIC, PASS(obj) :: DeallocateKdtree => obj_DeallocateKdtree
  !! Deallocate the kdtree of the mesh

  ! @NodeDataMethods
  PROCEDURE, PUBLIC, PASS(obj) :: InitiateKdtree => obj_InitiateKdtree
  !! Initiate the kdtree of the mesh
  PROCEDURE, PUBLIC, PASS(obj) :: InitiateNodeToElements => &
    obj_InitiateNodeToElements
  !! Initiate the node to elements data
  PROCEDURE, PUBLIC, PASS(obj) :: InitiateNodeToNodes => &
    obj_InitiateNodetoNodes
  !! Initiate the node to nodes data
  PROCEDURE, PUBLIC, PASS(obj) :: InitiateExtraNodeToNodes => &
    obj_InitiateExtraNodetoNodes
  !! Initiate the extra node to nodes data

  ! @ElementDataMethods
  PROCEDURE, PUBLIC, PASS(obj) :: InitiateElementToElements => &
    obj_InitiateElementToElements
  !! Initiate the element to elements data

  ! @BoundaryDataMethods
  PROCEDURE, PUBLIC, PASS(obj) :: InitiateBoundaryData => &
    obj_InitiateBoundaryData
  !! Initiate the boundary data

  ! @EdgeDataMethods
  PROCEDURE, PUBLIC, PASS(obj) :: InitiateEdgeConnectivity => &
    obj_InitiateEdgeConnectivity
  !! Initiate the edge connectivity

  ! @FaceDataMethods
  PROCEDURE, PUBLIC, PASS(obj) :: InitiateFaceConnectivity => &
    obj_InitiateFaceConnectivity
  !! Initiate the face connectivity

  ! @FacetDataMethods
  PROCEDURE, PUBLIC, PASS(obj) :: InitiateFacetElements => &
    obj_InitiateFacetElements
  !! Initiate the facet elements

  ! @GetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: GetElemType => obj_GetElemType
  !! Get the element type of an element
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalElementsTopologyWise => &
    obj_GetTotalElementsTopologyWise
  !! Get the total number of elements of each topology
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalTopology => obj_GetTotalTopology
  !! Get the number of topologies in the mesh
  PROCEDURE, PASS(obj) :: GetElemTopology1 => obj_GetElemTopology1
  !! Get the topologies present in the mesh
  PROCEDURE, PASS(obj) :: GetElemTopology2 => obj_GetElemTopology2
  !! Get the topology of an element
  GENERIC, PUBLIC :: GetElemTopology => GetElemTopology1, GetElemTopology2
  !! Get the element topology
  PROCEDURE, PUBLIC, PASS(obj) :: GetElemTopologyIndx => &
    obj_GetElemTopologyIndx
  !! Get the topology index of an element
  PROCEDURE, PUBLIC, PASS(obj) :: GetElemData => obj_GetElemData
  !! Get a copy of the element data
  PROCEDURE, PUBLIC, PASS(obj) :: GetElemDataPointer => &
    obj_GetElemDataPointer
  !! Get a pointer to the element data
  PROCEDURE, PUBLIC, PASS(obj) :: GetNNE => obj_GetNNE
  !! Get the number of nodes in an element
  PROCEDURE, PUBLIC, PASS(obj) :: GetMaxNNE => obj_GetMaxNNE
  !! Get the maximum number of nodes in an element
  PROCEDURE, PUBLIC, PASS(obj) :: Size => obj_Size
  !! Get the total number of elements
  PROCEDURE, PASS(obj) :: GetElemNum1 => obj_GetElemNum1
  !! Get the element numbers of the mesh
  PROCEDURE, PASS(obj) :: GetElemNum2 => obj_GetElemNum2
  !! Get the element numbers of a mesh id
  GENERIC, PUBLIC :: GetElemNum => GetElemNum1, GetElemNum2
  !! Get the element numbers
  PROCEDURE, PASS(obj) :: GetElemNum1_ => obj_GetElemNum1_
  !! Get the element numbers without allocation
  PROCEDURE, PASS(obj) :: GetElemNum2_ => obj_GetElemNum2_
  !! Get the element numbers of a mesh id
  PROCEDURE, PASS(obj) :: GetElemNum3_ => obj_GetElemNum3_
  !! Get the element numbers of mesh ids
  GENERIC, PUBLIC :: GetElemNum_ => GetElemNum1_, GetElemNum2_, GetElemNum3_
  !! Get the element numbers without allocation
  PROCEDURE, PUBLIC, PASS(obj) :: GetBoundingEntity => obj_GetBoundingEntity
  !! Get the bounding entities of the mesh
  PROCEDURE, PASS(obj) :: GetNptrs1 => obj_GetNptrs1
  !! Get the global node numbers of the mesh
  PROCEDURE, PASS(obj) :: GetNptrs2 => obj_GetNptrs2
  !! Get the global node numbers of a mesh id
  GENERIC, PUBLIC :: GetNptrs => GetNptrs1, GetNptrs2
  !! Get the global node numbers
  PROCEDURE, PASS(obj) :: GetNptrs1_ => obj_GetNptrs1_
  !! Get the global node numbers without allocation
  PROCEDURE, PASS(obj) :: GetNptrs2_ => obj_GetNptrs2_
  !! Get the global node numbers of a mesh id
  PROCEDURE, PASS(obj) :: GetNptrs3_ => obj_GetNptrs3_
  !! Get the global node numbers of a list of elements
  GENERIC, PUBLIC :: GetNptrs_ => GetNptrs1_, GetNptrs2_, GetNptrs3_
  !! Get the global node numbers without allocation
  PROCEDURE, PUBLIC, PASS(obj) :: GetNptrsInBox => obj_GetNptrsInBox
  !! Get the global nodes in a bounding box
  PROCEDURE, PUBLIC, PASS(obj) :: GetNptrsInBox_ => obj_GetNptrsInBox_
  !! Get the global nodes in a bounding box without allocation
  PROCEDURE, PUBLIC, PASS(obj) :: &
    GetElemNumberInBox => obj_GetElemNumberInBox
  !! Get the elements around nodes in a box
  PROCEDURE, PUBLIC, PASS(obj) :: &
    GetElemNumberInBox_ => obj_GetElemNumberInBox_
  !! Get the elements around nodes in a box without allocation
  PROCEDURE, PUBLIC, PASS(obj) :: GetInternalNptrs => obj_GetInternalNptrs
  !! Get the global numbers of internal nodes
  PROCEDURE, PUBLIC, PASS(obj) :: GetInternalNptrs_ => obj_GetInternalNptrs_
  !! Get the global numbers of internal nodes without allocation
  PROCEDURE, PUBLIC, PASS(obj) :: GetBoundaryNptrs => obj_GetBoundaryNptrs
  !! Get the global numbers of boundary nodes
  PROCEDURE, PUBLIC, PASS(obj) :: isBoundaryNode => obj_isBoundaryNode
  !! Returns true if a node is a boundary node
  PROCEDURE, PASS(obj) :: isNodePresent1 => obj_isNodePresent1
  !! Returns true if a node is present in the mesh
  PROCEDURE, PASS(obj) :: isNodePresent2 => obj_isNodePresent2
  !! Returns true for each node present in the mesh
  GENERIC, PUBLIC :: isNodePresent => isNodePresent1, isNodePresent2
  !! Returns true if nodes are present
  PROCEDURE, PUBLIC, PASS(obj) :: GetNodeMask => obj_GetNodeMask
  !! Get a mask of the nodes present in the mesh
  PROCEDURE, PUBLIC, PASS(obj) :: isAnyNodePresent => obj_isAnyNodePresent
  !! Returns true if any of the nodes is present
  PROCEDURE, PUBLIC, PASS(obj) :: isAllNodePresent => obj_isAllNodePresent
  !! Returns true if all the nodes are present
  PROCEDURE, PUBLIC, PASS(obj) :: isElementPresent => obj_isElementPresent
  !! Returns true if an element is present
  PROCEDURE, PUBLIC, PASS(obj) :: isBoundaryElement => obj_isBoundaryElement
  !! Returns true if an element is a boundary element
  PROCEDURE, PUBLIC, PASS(obj) :: isDomainBoundaryElement => &
    obj_isDomainBoundaryElement
  !! Returns true if an element is a domain boundary element
  PROCEDURE, PUBLIC, PASS(obj) :: isDomainFacetElement => &
    obj_isDomainFacetElement
  !! Returns true if a facet element is a domain boundary facet
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalInternalNodes => &
    obj_GetTotalInternalNodes
  !! Get the number of internal nodes
  PROCEDURE, PASS(obj) :: GetTotalNodes1 => obj_GetTotalNodes1
  !! Get the number of nodes in the mesh
  PROCEDURE, PASS(obj) :: GetTotalNodes2 => obj_GetTotalNodes2
  !! Get the number of nodes of a mesh id
  PROCEDURE, PASS(obj) :: GetTotalNodes3 => obj_GetTotalNodes3
  !! Get the number of nodes of a list of elements
  GENERIC, PUBLIC :: GetTotalNodes => GetTotalNodes1, GetTotalNodes2, &
    GetTotalNodes3
  !! Get the number of nodes
  PROCEDURE, PASS(obj) :: GetTotalVertexNodes1 => obj_GetTotalVertexNodes1
  !! Get the number of vertex nodes
  PROCEDURE, PASS(obj) :: GetTotalVertexNodes2 => obj_GetTotalVertexNodes2
  !! Get the number of vertex nodes of a mesh id
  PROCEDURE, PASS(obj) :: GetTotalVertexNodes3 => obj_GetTotalVertexNodes3
  !! Get the number of vertex nodes of a list of elements
  PROCEDURE, PASS(obj) :: GetTotalVertexNodes4 => obj_GetTotalVertexNodes4
  !! Get the number of vertex nodes of an element
  GENERIC, PUBLIC :: GetTotalVertexNodes => GetTotalVertexNodes1, &
    GetTotalVertexNodes2, GetTotalVertexNodes3, GetTotalVertexNodes4
  !! Get the number of vertex nodes
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalFaces => obj_GetTotalFaces
  !! Get the number of faces in the mesh
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalEdges => obj_GetTotalEdges
  !! Get the number of edges in the mesh
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalCells => obj_Size
  !! Get the number of cells (elements)
  PROCEDURE, PASS(obj) :: GetTotalElements1 => obj_Size
  !! Get the total number of elements
  PROCEDURE, PASS(obj) :: GetTotalElements2 => obj_GetTotalElements2
  !! Get the number of elements of a mesh id
  PROCEDURE, PASS(obj) :: GetTotalElements3 => obj_GetTotalElements3
  !! Get the number of elements of mesh ids
  GENERIC, PUBLIC :: GetTotalElements => GetTotalElements1, &
    GetTotalElements2, GetTotalElements3
  !! Get the number of elements
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalBoundaryElements => &
    obj_GetTotalBoundaryElements
  !! Get the number of boundary elements
  PROCEDURE, PASS(obj) :: GetBoundingBox1 => obj_GetBoundingBox1
  !! Get the bounding box of the mesh
  PROCEDURE, PASS(obj) :: GetBoundingBox2 => obj_GetBoundingBox2
  !! Get the bounding box of node coordinates
  GENERIC, PUBLIC :: GetBoundingBox => GetBoundingBox1, &
    GetBoundingBox2
  !! Get the bounding box
  PROCEDURE, PUBLIC, PASS(obj) :: GetConnectivity => &
    obj_GetConnectivity
  !! Get the connectivity of an element
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: GetConnectivity1_ => &
    obj_GetConnectivity1_
  !! Get the connectivity of an element
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: GetConnectivity2_ => &
    obj_GetConnectivity2_
  !! Get all connectivities of an element
  GENERIC, PUBLIC :: GetConnectivity_ => GetConnectivity1_, &
    GetConnectivity2_
  !! Get the connectivity without allocation
  PROCEDURE, PUBLIC, PASS(obj) :: GetOrientation => obj_GetOrientation
  !! Get the orientation flags of an element
  PROCEDURE, PUBLIC, PASS(obj) :: GetCellOrientation => obj_GetCellOrientation
  !! Get the cell orientation of an element
  PROCEDURE, PUBLIC, PASS(obj) :: GetFaceOrientation => obj_GetFaceOrientation
  !! Get the face orientation of an element
  PROCEDURE, PUBLIC, PASS(obj) :: GetEdgeOrientation => obj_GetEdgeOrientation
  !! Get the edge orientation of an element
  PROCEDURE, PUBLIC, PASS(obj) :: GetNodeConnectivity => &
    obj_GetNodeConnectivity
  !! Get the vertex connectivity of all elements
  PROCEDURE, PASS(obj) :: GetLocalNodeNumber1 => &
    obj_GetLocalNodeNumber1
  !! Get the local numbers of global nodes
  PROCEDURE, PASS(obj) :: GetLocalNodeNumber2 => &
    obj_GetLocalNodeNumber2
  !! Get the local number of a global node
  GENERIC, PUBLIC :: GetLocalNodeNumber => GetLocalNodeNumber1, &
    GetLocalNodeNumber2
  !! Get local node numbers
  PROCEDURE, PUBLIC, PASS(obj) :: GetLocalNodeNumber_ => &
    obj_GetLocalNodeNumber1_
  !! Get the local numbers of global nodes without allocation
  PROCEDURE, PASS(obj) :: GetGlobalNodeNumber1 => &
    obj_GetGlobalNodeNumber1
  !! Get the global numbers of local nodes
  PROCEDURE, PASS(obj) :: GetGlobalNodeNumber2 => &
    obj_GetGlobalNodeNumber2
  !! Get the global number of a local node
  GENERIC, PUBLIC :: GetGlobalNodeNumber => GetGlobalNodeNumber1, &
    GetGlobalNodeNumber2
  !! Get global node numbers
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalBoundaryNodes => &
    obj_GetTotalBoundaryNodes
  !! Get the number of boundary nodes
  PROCEDURE, PASS(obj) :: GetGlobalElemNumber1 => obj_GetGlobalElemNumber1
  !! Get the global numbers of local elements
  PROCEDURE, PASS(obj) :: GetGlobalElemNumber2 => obj_GetGlobalElemNumber2
  !! Get the global number of a local element
  GENERIC, PUBLIC :: GetGlobalElemNumber => &
    GetGlobalElemNumber1, GetGlobalElemNumber2
  !! Get global element numbers
  PROCEDURE, PASS(obj) :: GetLocalElemNumber1 => obj_GetLocalElemNumber1
  !! Get the local numbers of global elements
  PROCEDURE, PASS(obj) :: GetLocalElemNumber2 => obj_GetLocalElemNumber2
  !! Get the local number of a global element
  GENERIC, PUBLIC :: GetLocalElemNumber => &
    GetLocalElemNumber1, GetLocalElemNumber2
  !! Get local element numbers
  PROCEDURE, PASS(obj) :: GetNodeToElements1 => obj_GetNodeToElements1
  !! Get the elements around a node
  PROCEDURE, PASS(obj) :: GetNodeToElements2 => obj_GetNodeToElements2
  !! Get the elements around a list of nodes
  GENERIC, PUBLIC :: GetNodeToElements => &
    GetNodeToElements1, GetNodeToElements2
  !! Get the elements around nodes
  PROCEDURE, PASS(obj) :: GetNodeToElements1_ => obj_GetNodeToElements1_
  !! Get the elements around a node without allocation
  PROCEDURE, PASS(obj) :: GetNodeToElements2_ => obj_GetNodeToElements2_
  !! Get the elements around nodes without allocation
  GENERIC, PUBLIC :: GetNodeToElements_ => &
    GetNodeToElements1_, GetNodeToElements2_
  !! Get the elements around nodes without allocation
  PROCEDURE, PASS(obj) :: GetNodeToNodes1 => obj_GetNodeToNodes1
  !! Get the nodes connected to a node
  PROCEDURE, PASS(obj) :: GetNodeToNodes2 => obj_GetNodeToNodes2
  !! Get the nodes connected to a list of nodes
  GENERIC, PUBLIC :: GetNodeToNodes => &
    GetNodeToNodes1, GetNodeToNodes2
  !! Get the nodes connected to nodes
  PROCEDURE, PASS(obj) :: GetNodeToNodes1_ => obj_GetNodeToNodes1_
  !! Get the nodes connected to a node without allocation
  PROCEDURE, PASS(obj) :: GetNodeToNodes2_ => obj_GetNodeToNodes2_
  !! Get the nodes connected to nodes without allocation
  GENERIC, PUBLIC :: GetNodeToNodes_ => &
    GetNodeToNodes1_, GetNodeToNodes2_
  !! Get the nodes connected to nodes without allocation
  PROCEDURE, PUBLIC, PASS(obj) :: GetElementToElements => &
    obj_GetElementToElements
  !! Get the elements around an element
  PROCEDURE, PASS(obj) :: GetElementToElements1_ => &
    obj_GetElementToElements1_
  !! Get the neighbour elements without allocation
  PROCEDURE, PASS(obj) :: GetElementToElements2_ => &
    obj_GetElementToElements2_
  !! Get the elements around an element without allocation
  GENERIC, PUBLIC :: GetElementToElements_ => GetElementToElements1_, &
    GetElementToElements2_
  !! Get the neighbours without allocation
  PROCEDURE, PUBLIC, PASS(obj) :: GetBoundaryElementData => &
    obj_GetBoundaryElementData
  !! Get the boundary facets of an element
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalFacetElements => &
    obj_GetTotalFacetElements
  !! Get the number of facet elements
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalBoundaryFacetElements => &
    obj_GetTotalBoundaryFacetElements
  !! Get the number of boundary facet elements
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalInternalFacetElements => &
    obj_GetTotalInternalFacetElements
  !! Get the number of internal facet elements
  PROCEDURE, PUBLIC, PASS(obj) :: GetMasterCellNumber => &
    obj_GetMasterCellNumber
  !! Get the master cell of a facet element
  PROCEDURE, PUBLIC, PASS(obj) :: GetSlaveCellNumber => &
    obj_GetSlaveCellNumber
  !! Get the slave cell of a facet element
  PROCEDURE, PUBLIC, PASS(obj) :: GetCellNumber => &
    obj_GetCellNumber
  !! Get the master and slave cells of a facet
  PROCEDURE, PUBLIC, PASS(obj) :: GetLocalFacetID => &
    obj_GetLocalFacetID
  !! Get the local facet ID of a facet element
  PROCEDURE, PUBLIC, PASS(obj) :: GetGlobalFaceNumber => &
    obj_GetGlobalFaceNumber
  !! Get the global face number of a local face
  PROCEDURE, PUBLIC, PASS(obj) :: GetGlobalEdgeNumber => &
    obj_GetGlobalEdgeNumber
  !! Get the global edge number of a local edge
  PROCEDURE, PUBLIC, PASS(obj) :: FindFace => obj_FindFace
  !! Find a face in an element
  PROCEDURE, PUBLIC, PASS(obj) :: FindEdge => obj_FindEdge
  !! Find an edge in an element
  PROCEDURE, PUBLIC, PASS(obj) :: GetFacetConnectivity => &
    obj_GetFacetConnectivity
  !! Get the connectivity of a facet of an element
  PROCEDURE, PUBLIC, PASS(obj) :: GetFacetConnectivity_ => &
    obj_GetFacetConnectivity_
  !! Get the connectivity of a facet without allocation
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalFacetConnectivity => &
    obj_GetTotalFacetConnectivity
  !! Get the number of nodes of a facet
  PROCEDURE, PASS(obj) :: GetOrder1 => obj_GetOrder1
  !! Get the largest order of the elements in the mesh
  PROCEDURE, PASS(obj) :: GetOrder2 => obj_GetOrder2
  !! Get the order of an element
  GENERIC, PUBLIC :: GetOrder => GetOrder1, GetOrder2
  !! Get the order of elements
  PROCEDURE, PUBLIC, PASS(obj) :: GetOrder_ => obj_GetOrder_
  !! Get the order of all elements
  PROCEDURE, PUBLIC, PASS(obj) :: GetNSD => obj_GetNSD
  !! Get the number of spatial dimensions
  PROCEDURE, PUBLIC, PASS(obj) :: GetXidimension => &
    obj_GetXidimension
  !! Get the dimension of the elements
  PROCEDURE, PUBLIC, PASS(obj) :: GetMaterial => obj_GetMaterial1
  !! Get the material id of a medium of an element
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalMaterial1 => obj_GetTotalMaterial1
  !! Get the number of media of an element
  GENERIC, PUBLIC :: GetTotalMaterial => GetTotalMaterial1
  !! Get the number of media of an element
  PROCEDURE, PUBLIC, PASS(obj) :: GetParam => obj_GetParam
  !! Get the parameters of the mesh
  PROCEDURE, PUBLIC, PASS(obj) :: GetMinElemNumber => obj_GetMinElemNumber
  !! Get the minimum element number
  PROCEDURE, PUBLIC, PASS(obj) :: GetMaxElemNumber => obj_GetMaxElemNumber
  !! Get the maximum element number
  PROCEDURE, PUBLIC, PASS(obj) :: GetMinNodeNumber => obj_GetMinNodeNumber
  !! Get the minimum node number
  PROCEDURE, PUBLIC, PASS(obj) :: GetMaxNodeNumber => obj_GetMaxNodeNumber
  !! Get the maximum node number

  PROCEDURE, PUBLIC, PASS(obj) :: isInit => obj_isInit
  !! Returns true if the mesh is initiated
  PROCEDURE, PUBLIC, PASS(obj) :: isNodeToElements => obj_isNodeToElements
  !! Returns true if node to elements data is initiated
  PROCEDURE, PUBLIC, PASS(obj) :: isNodeToNodes => obj_isNodeToNodes
  !! Returns true if node to nodes data is initiated
  PROCEDURE, PUBLIC, PASS(obj) :: isExtraNodeToNodes => obj_isExtraNodeToNodes
  !! Returns true if extra node to nodes data is initiated
  PROCEDURE, PUBLIC, PASS(obj) :: isElementToElements => &
    obj_isElementToElements
  !! Returns true if element to elements data is initiated
  PROCEDURE, PUBLIC, PASS(obj) :: isEdgeConnectivity => obj_isEdgeConnectivity
  !! Returns true if the edge connectivity is initiated
  PROCEDURE, PUBLIC, PASS(obj) :: isFaceConnectivity => obj_isFaceConnectivity
  !! Returns true if the face connectivity is initiated
  PROCEDURE, PUBLIC, PASS(obj) :: isBoundaryData => obj_isBoundaryData
  !! Returns true if the boundary data is initiated
  PROCEDURE, PUBLIC, PASS(obj) :: isFacetData => obj_isFacetData
  !! Returns true if the facet data is initiated
  PROCEDURE, PUBLIC, PASS(obj) :: isElementActive => obj_isElementActive
  !! Returns true if an element is active
  PROCEDURE, PUBLIC, PASS(obj) :: GetFacetParam => obj_GetFacetParam
  !! Get the parameters of a facet element
  PROCEDURE, PASS(obj) :: GetTotalEntities1 => obj_GetTotalEntities1
  !! Get the number of entities of an element
  PROCEDURE, PASS(obj) :: GetTotalEntities2 => obj_GetTotalEntities2
  !! Get the number of entities of the mesh
  GENERIC, PUBLIC :: GetTotalEntities => GetTotalEntities1, GetTotalEntities2
  !! Get the number of entities
  PROCEDURE, PASS(obj) :: GetNodeCoord1 => obj_GetNodeCoord1
  !! Get node coordinates
  PROCEDURE, PASS(obj) :: GetNodeCoord2 => obj_GetNodeCoord2
  !! Get the coordinates of all nodes
  PROCEDURE, PASS(obj) :: GetNodeCoord3 => obj_GetNodeCoord3
  !! Get the coordinates of the nodes of an element
  PROCEDURE, PASS(obj) :: GetNodeCoord4 => obj_GetNodeCoord4
  !! Get the coordinates of a list of nodes
  PROCEDURE, PASS(obj) :: GetNodeCoord5 => obj_GetNodeCoord5
  !! Get the coordinates of a node

  GENERIC, PUBLIC :: GetNodeCoord => GetNodeCoord1, GetNodeCoord2, &
    GetNodeCoord3, GetNodeCoord4, GetNodeCoord5
  !! Get node coordinates
  PROCEDURE, PASS(obj) :: GetNearestNode1 => obj_GetNearestNode1
  !! Get the node nearest to a point
  PROCEDURE, PASS(obj) :: GetNearestNode2 => obj_GetNearestNode2
  !! Get the nn nodes nearest to a point
  GENERIC, PUBLIC :: GetNearestNode => GetNearestNode1, GetNearestNode2
  !! Get the nearest nodes to a point
  PROCEDURE, PUBLIC, PASS(obj) :: GetMaxNodeToElements => &
    obj_GetMaxNodeToElements
  !! Get the maximum number of elements around a node
  PROCEDURE, PUBLIC, PASS(obj) :: GetMaxElementToElements => &
    obj_GetMaxElementToElements
  !! Get the maximum number of elements around an element
  PROCEDURE, PUBLIC, PASS(obj) :: GetMaxNodeToNodes => &
    obj_GetMaxNodeToNodes
  !! Get the maximum number of nodes around a node

  ! @SetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: SetShowTime => obj_SetShowTime
  !! Set the showTime option
  PROCEDURE, PASS(obj) :: SetBoundingBox1 => obj_SetBoundingBox1
  !! Set the bounding box parameters of the mesh
  PROCEDURE, PASS(obj) :: SetBoundingBox2 => obj_SetBoundingBox2
  !! Set the bounding box parameters from node coordinates
  GENERIC, PUBLIC :: SetBoundingBox => SetBoundingBox1, &
    SetBoundingBox2
  !! Set the bounding box parameters
  PROCEDURE, PASS(obj) :: SetSparsity1 => obj_SetSparsity1
  !! Set the sparsity pattern of a matrix
  PROCEDURE, PASS(obj) :: SetSparsity2 => obj_SetSparsity2
  !! Set the sparsity pattern of a matrix
  PROCEDURE, PASS(obj) :: SetSparsity3 => obj_SetSparsity3
  !! Set the sparsity pattern between two meshes
  PROCEDURE, PASS(obj) :: SetSparsity4 => obj_SetSparsity4
  !! Set the sparsity pattern between two meshes with number maps
  GENERIC, PUBLIC :: SetSparsity => SetSparsity1, SetSparsity2, &
    SetSparsity3, SetSparsity4
  !! Set the sparsity pattern of a matrix
  PROCEDURE, PASS(obj) :: SetTotalMedium1 => obj_SetTotalMedium1
  !! Set the number of media of an element
  PROCEDURE, PASS(obj) :: SetTotalMedium2 => obj_SetTotalMedium2
  !! Set the number of media of all elements
  GENERIC, PUBLIC :: SetTotalMedium => SetTotalMedium1, SetTotalMedium2
  !! Set the number of media of elements
  PROCEDURE, PASS(obj) :: SetMaterial1 => obj_SetMaterial1
  !! Set the material of a medium in an entity
  PROCEDURE, PASS(obj) :: SetMaterial2 => obj_SetMaterial2
  !! Set the material of a medium in all elements
  PROCEDURE, PASS(obj) :: SetMaterial3 => obj_SetMaterial3
  !! Set the material of a medium in an element
  GENERIC, PUBLIC :: SetMaterial => SetMaterial1, SetMaterial2, &
    SetMaterial3
  !! Set the material of a medium
  PROCEDURE, PUBLIC, PASS(obj) :: SetFacetElementType => &
    obj_SetFacetElementType
  !! Set the element type of an element
  PROCEDURE, PUBLIC, PASS(obj) :: SetQuality => obj_SetQuality
  !! Set the quality of the mesh
  PROCEDURE, PUBLIC, PASS(obj) :: SetParam => obj_SetParam
  !! Set the parameters of the mesh
  PROCEDURE, PUBLIC, PASS(obj) :: SetFacetParam => obj_SetFacetParam
  !! Set the parameters of a facet element

  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  !! Import the mesh from a HDF5 mesh file
  PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export
  !! Export the mesh to a HDF5 file
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the content of the mesh
  PROCEDURE, PUBLIC, PASS(obj) :: DisplayNodeData => &
    obj_DisplayNodeData
  !! Display the node data
  PROCEDURE, PUBLIC, PASS(obj) :: DisplayElementData => &
    obj_DisplayElementData
  !! Display the element data
  PROCEDURE, PUBLIC, PASS(obj) :: DisplayFacetData => &
    obj_DisplayFacetData
  !! Display the facet data
  PROCEDURE, PUBLIC, PASS(obj) :: DisplayInternalFacetData => &
    obj_DisplayInternalFacetData
  !! Display the internal facet data
  PROCEDURE, PUBLIC, PASS(obj) :: DisplayBoundaryFacetData => &
    obj_DisplayBoundaryFacetData
  !! Display the boundary facet data
  PROCEDURE, PUBLIC, PASS(obj) :: DisplayMeshInfo => &
    obj_DisplayMeshInfo
  !! Display the statistics of the mesh

  ! @VTKMethods
  PROCEDURE, PUBLIC, PASS(obj) :: ExportToVTK => obj_ExportToVTK
  !! Export the mesh to a VTK file
  PROCEDURE, PUBLIC, PASS(obj) :: WriteData_vtk => obj_WriteData_vtk
  !! Write the mesh in a VTK file (.vtu)
  PROCEDURE, PUBLIC, PASS(obj) :: WriteData_vtk_r1 => obj_WriteData_vtk_r1
  !! Write the mesh and a scalar nodal data
  PROCEDURE, PUBLIC, PASS(obj) :: WriteData_vtk_r2 => obj_WriteData_vtk_r2
  !! Write the mesh and nodal data
  PROCEDURE, PUBLIC, PASS(obj) :: WriteData_vtk_r3 => obj_WriteData_vtk_r3
  !! Write the mesh and vector nodal data
  GENERIC, PUBLIC :: WriteData => WriteData_vtk, WriteData_vtk_r1, &
    WriteData_vtk_r2, WriteData_vtk_r3
  !! Write the mesh (and data) in a VTK file
END TYPE AbstractMesh_

!----------------------------------------------------------------------------
!                                                   AbstractMeshPointer_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Pointer to a mesh
!
! ptr points to an AbstractMesh_.
!
! See also: [[AbstractMeshPointerDeallocate]]

TYPE :: AbstractMeshPointer_
  CLASS(AbstractMesh_), POINTER :: ptr => NULL()
END TYPE AbstractMeshPointer_

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-18
! summary: Initiate the mesh from a HDF5 mesh file
!
! Deallocate obj and read the mesh with Import. Then mark the mesh as
! initiated.
!
! - If only group is given, the mesh is read from group.
! - If dim is given, all entities of dimension dim are read.
! - If dim and entities are given, only these entities are read.
!
! Global element and node numbers are the numbers in the file. Local node
! numbers follow the order of the global node numbers, local element numbers
! follow the order of the elements in the file. Initiate also initiates the
! element to elements data (see InitiateElementToElements), which marks the
! boundary elements and boundary nodes.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!   hdf5 : TYPE(HDF5File_), intent(inout) - mesh file, open with read access
!   group : CHARACTER(*), optional, intent(in) - group in the mesh file
!   dim : INTEGER(I4B), optional, intent(in) - dimension of the entities: 0
!     points, 1 curves, 2 surfaces, 3 volumes
!   entities(:) : INTEGER(I4B), optional, intent(in) - entity numbers of
!     dimension dim
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!
!   CALL OK(obj%IsInit(), "IsInit")
!   CALL OK(.NOT. obj%IsEmpty(), "IsEmpty")
!   CALL OK(obj%GetNSD() .EQ. 2_I4B, "GetNSD")
!   CALL OK(obj%GetXidimension() .EQ. 2_I4B, "GetXidimension")
!   CALL OK(obj%GetTotalNodes() .EQ. 9_I4B, "GetTotalNodes")
!   CALL OK(obj%GetTotalElements() .EQ. 4_I4B, "GetTotalElements")
!   CALL OK(obj%GetMaxNNE() .EQ. 4_I4B, "GetMaxNNE")
!   CALL OK(obj%GetMinElemNumber() .EQ. 13_I4B, "GetMinElemNumber")
!   CALL OK(obj%GetMaxElemNumber() .EQ. 16_I4B, "GetMaxElemNumber")
!   CALL OK(obj%GetMinNodeNumber() .EQ. 1_I4B, "GetMinNodeNumber")
!   CALL OK(obj%GetMaxNodeNumber() .EQ. 9_I4B, "GetMaxNodeNumber")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!   INTEGER(I4B), ALLOCATABLE :: ivec(:)
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!
!   CALL obj%Initiate(hdf5=meshfile, DIM=1, entities=[1_I4B, 3_I4B])
!   CALL OK(obj%GetTotalElements() .EQ. 4_I4B, "GetTotalElements")
!   CALL OK(obj%GetTotalNodes() .EQ. 5_I4B, "GetTotalNodes")
!   ivec = obj%GetElemNum()
!   CALL OK(ALL(ivec .EQ. [5, 6, 9, 10]), "GetElemNum")
!   ivec = obj%GetNptrs()
!   CALL OK(ALL(ivec .EQ. [1, 2, 3, 5, 7]), "GetNptrs")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Import]], [[obj_Deallocate]],
!   [[obj_InitiateElementToElements]]

INTERFACE
  MODULE SUBROUTINE obj_Initiate(obj, hdf5, group, dim, entities)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! mesh file, open with read access
    CHARACTER(*), INTENT(IN), OPTIONAL :: group
    !! group in the mesh file
    INTEGER(I4B), INTENT(IN), OPTIONAL :: dim
    !! dimension of the entities: 0 points, 1 curves, 2 surfaces, 3 volumes
    INTEGER(I4B), INTENT(IN), OPTIONAL :: entities(:)
    !! entity numbers of dimension dim
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Deallocate the data of the mesh
!
! Free all data of obj. After Deallocate, IsEmpty is true and IsInit is false.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!
!   CALL OK(.NOT. obj%IsEmpty(), "IsEmpty before Deallocate")
!   CALL obj%DEALLOCATE()
!   CALL OK(obj%IsEmpty(), "IsEmpty after Deallocate")
!   CALL OK(.NOT. obj%IsInit(), "IsInit after Deallocate")
!   CALL OK(obj%GetTotalNodes() .EQ. 0_I4B, "GetTotalNodes")
!
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!   CALL OK(obj%GetTotalNodes() .EQ. 9_I4B, "Initiate again")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]], [[obj_isEmpty]]

INTERFACE AbstractMeshDeallocate
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
  END SUBROUTINE obj_Deallocate
END INTERFACE AbstractMeshDeallocate

!----------------------------------------------------------------------------
!                                                 isEmpty@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-31
! summary: Returns true if the mesh is empty
!
! The mesh is empty when it has no elements or no nodes.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!
! Returns:
!   ans : LOGICAL(LGT) - true if the mesh has no elements or no nodes
!
! See also: [[obj_Deallocate]], [[obj_isInit]]

INTERFACE
  MODULE FUNCTION obj_isEmpty(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    LOGICAL(LGT) :: ans
    !! true if the mesh has no elements or no nodes
  END FUNCTION obj_isEmpty
END INTERFACE

!----------------------------------------------------------------------------
!                           InitiateDynamicDataStructure@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-31
! summary: Initiate the dynamic data structure of the mesh
!
! Put the element data and node data of the mesh in linked lists and binary
! trees. The mesh must be initiated first; the static data is not changed.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!
!   CALL obj%InitiateDynamicDataStructure()
!   CALL OK(obj%GetTotalElements() .EQ. 4_I4B, "GetTotalElements")
!   CALL OK(obj%GetTotalNodes() .EQ. 9_I4B, "GetTotalNodes")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE obj_InitiateDynamicDataStructure(obj)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
  END SUBROUTINE obj_InitiateDynamicDataStructure
END INTERFACE

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-10
! summary: Deallocate the kdtree of the mesh
!
! Free the kdtree and the search results. Methods which need the kdtree
! initiate it again.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!
! See also: [[obj_InitiateKdtree]], [[obj_GetNearestNode1]]

INTERFACE
  MODULE SUBROUTINE obj_DeallocateKdtree(obj)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
  END SUBROUTINE obj_DeallocateKdtree
END INTERFACE

!----------------------------------------------------------------------------
!                                               Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-16
! summary: Deallocate an array of mesh pointers
!
! Call Deallocate on every associated mesh of obj, then deallocate obj. The
! mesh objects themselves are not freed.
!
! Arguments:
!   obj(:) : TYPE(AbstractMeshPointer_), allocatable, intent(inout) - array of
!     mesh pointers
!
! Examples
!
!```fortran
! PROGRAM main
!   USE AbstractMesh_Class, ONLY: AbstractMeshPointer_
!   USE AbstractMesh_Class, ONLY: AbstractMeshPointerDeallocate
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE FEMesh_Class, ONLY: FEMesh_Pointer
!   USE FEMesh_Class, ONLY: FEMeshPointer_
!   USE FEMesh_Class, ONLY: FEMeshPointerDeallocate
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CLASS(FEMesh_), POINTER :: ptr
!   TYPE(FEMeshPointer_), ALLOCATABLE :: fptrs(:)
!   TYPE(AbstractMeshPointer_), ALLOCATABLE :: aptrs(:)
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   ptr => FEMesh_Pointer()
!   CALL OK(ASSOCIATED(ptr), "FEMesh_Pointer")
!   CALL OK(ptr%IsEmpty(), "new mesh is empty")
!
!   ALLOCATE(fptrs(2))
!   fptrs(1)%ptr => FEMesh_Pointer()
!   CALL FEMeshPointerDeallocate(fptrs)
!   CALL OK(.NOT. ALLOCATED(fptrs), "FEMeshPointerDeallocate")
!
!   ALLOCATE(aptrs(1))
!   aptrs(1)%ptr => ptr
!   CALL AbstractMeshPointerDeallocate(aptrs)
!   CALL OK(.NOT. ALLOCATED(aptrs), "AbstractMeshPointerDeallocate")
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE AbstractMeshPointerDeallocate(obj)
    TYPE(AbstractMeshPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    !! array of mesh pointers
  END SUBROUTINE AbstractMeshPointerDeallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-18
! summary: Import the mesh from a HDF5 mesh file
!
! Read the mesh data (see Initiate for the meaning of group, dim and entities)
! and initiate the element to elements data. Import does not mark the mesh as
! initiated; use Initiate for that.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!   hdf5 : TYPE(HDF5File_), intent(inout) - mesh file, open with read access
!   group : CHARACTER(*), optional, intent(in) - group in the mesh file;
!     default is the root
!   dim : INTEGER(I4B), optional, intent(in) - dimension of the entities: 0
!     points, 1 curves, 2 surfaces, 3 volumes
!   entities(:) : INTEGER(I4B), optional, intent(in) - entity numbers of
!     dimension dim
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!
!   CALL obj%IMPORT(hdf5=meshfile, DIM=2_I4B)
!   CALL OK(obj%GetTotalElements() .EQ. 4_I4B, "GetTotalElements")
!   CALL OK(obj%GetTotalNodes() .EQ. 9_I4B, "GetTotalNodes")
!   CALL OK(obj%IsElementToElements(), "element to elements data")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]]

INTERFACE AbstractMeshImport
  MODULE SUBROUTINE obj_Import(obj, hdf5, group, dim, entities)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! mesh file, open with read access
    CHARACTER(*), INTENT(IN), OPTIONAL :: group
    !! group in the mesh file; default is the root
    INTEGER(I4B), INTENT(IN), OPTIONAL :: dim
    !! dimension of the entities: 0 points, 1 curves, 2 surfaces, 3 volumes
    INTEGER(I4B), INTENT(IN), OPTIONAL :: entities(:)
    !! entity numbers of dimension dim
  END SUBROUTINE obj_Import
END INTERFACE AbstractMeshImport

!----------------------------------------------------------------------------
!                                                    GetNodeCoord@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Get node coordinates
!
! GetNodeCoord is a generic method:
!
! - with hdf5 and group, read the coordinates of all nodes from the mesh file;
! - with nrow and ncol, return the coordinates of all nodes;
! - with globalElement, return the coordinates of the nodes of an element;
! - with globalNode(:), return the coordinates of a list of nodes;
! - with globalNode and tsize, return the coordinates of one node.
!
! Coordinates are in xiJ format: three rows (x, y, z) and one column per node.
! For all nodes the columns are in local node order.
!
! This routine reads nodeCoord from the mesh file. nodeCoord is reallocated to
! (3, number of nodes); column i is local node i. The dataset is searched in
! group, group/nodeCoord, and the parent group of group.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   nodeCoord(:,:) : REAL(DFP), allocatable, intent(inout) - node coordinates
!     in xiJ format, it is reallocated
!   hdf5 : TYPE(HDF5File_), intent(inout) - mesh file, open with read access
!   group : CHARACTER(*), intent(in) - group in the mesh file
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: DFP
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!   REAL(DFP), PARAMETER :: tol = 1.0E-8_DFP
!   REAL(DFP), ALLOCATABLE :: xij(:, :)
!   REAL(DFP) :: xx(3, 20), x(3)
!   INTEGER(I4B) :: nrow, ncol, tsize
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!
!   CALL obj%GetNodeCoord(nodeCoord=xij, hdf5=meshfile, group="")
!   CALL OK(ALL(SHAPE(xij) .EQ. [3, 9]), "from the mesh file")
!
!   CALL obj%GetNodeCoord(nodeCoord=xx, nrow=nrow, ncol=ncol)
!   CALL OK(nrow .EQ. 3_I4B .AND. ncol .EQ. 9_I4B, "all nodes")
!   CALL OK(ALL(ABS(xx(1:2, 9) - 0.5_DFP) .LT. tol), "node 9")
!
!   CALL obj%GetNodeCoord(nodeCoord=xx, nrow=nrow, ncol=ncol, &
!     globalElement=16_I4B)
!   CALL OK(ncol .EQ. 4_I4B, "element 16: ncol")
!   CALL OK(ALL(ABS(xx(1:2, 3) - 1.0_DFP) .LT. tol), "element 16: node 4")
!
!   CALL obj%GetNodeCoord(nodeCoord=xx, nrow=nrow, ncol=ncol, &
!     globalNode=[4_I4B, 1_I4B])
!   CALL OK(ALL(ABS(xx(1:2, 2)) .LT. tol), "nodes 4 and 1")
!
!   CALL obj%GetNodeCoord(nodeCoord=x, tsize=tsize, globalNode=6_I4B)
!   CALL OK(ALL(ABS(x(1:2) - [0.5_DFP, 1.0_DFP]) .LT. tol), "node 6")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetNodeCoord2]], [[obj_GetNodeCoord3]],
!   [[obj_GetNodeCoord4]], [[obj_GetNodeCoord5]]

INTERFACE
  MODULE SUBROUTINE obj_GetNodeCoord1(obj, nodeCoord, hdf5, group)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: nodeCoord(:, :)
    !! node coordinates in xiJ format, it is reallocated
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! mesh file, open with read access
    CHARACTER(*), INTENT(IN) :: group
    !! group in the mesh file
  END SUBROUTINE obj_GetNodeCoord1
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Export@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Export the mesh to a HDF5 file
!
! TODO: This routine needs to be implemented. At present it only raises
! an error. When implemented, it should write the mesh to the group of
! the HDF5 file in the format that [[obj_Import]] reads.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   hdf5 : TYPE(HDF5File_), intent(inout) - HDF5 file, open with write access
!   group : CHARACTER(*), intent(in) - group in the HDF5 file

INTERFACE
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! HDF5 file, open with write access
    CHARACTER(*), INTENT(IN) :: group
    !! group in the HDF5 file
  END SUBROUTINE obj_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ExportToVTK@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Export the mesh to a VTK file
!
! Write the mesh in vtk.
!
! - If vtk is not initiated, filename must be given; a new unstructured grid
!   file is created.
! - If opentag is true (default), the Piece tag is written.
! - The Points are always written: nodeCoord if given, else the node
!   coordinates of the mesh.
! - If content is true (default), the Cells are written.
! - If closetag is true (default), the Piece tag is closed.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   vtk : TYPE(VTKFile_), intent(inout) - VTK file
!   nodeCoord(:,:) : REAL(DFP), optional, intent(in) - node coordinates in xiJ
!     format, SIZE(nodeCoord) = [3, number of nodes]
!   filename : CHARACTER(*), optional, intent(in) - name of the VTK file,
!     needed if vtk is not initiated
!   opentag : LOGICAL(LGT), optional, intent(in) - if true (default), write
!     the Piece tag
!   content : LOGICAL(LGT), optional, intent(in) - if true (default), write
!     the cells
!   closetag : LOGICAL(LGT), optional, intent(in) - if true (default), close
!     the Piece tag
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: DFP
!   USE GlobalData, ONLY: I4B
!   USE GlobalData, ONLY: LGT
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE String_Class, ONLY: String
!   USE Test_Method, ONLY: OK
!   USE VTKFile_Class, ONLY: VTKFile_
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!   TYPE(VTKFile_) :: vtk
!   REAL(DFP) :: dval(9)
!   LOGICAL(LGT) :: isok
!   INTEGER(I4B) :: ii
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!
!   CALL obj%WriteData(filename="WriteData_test_1.vtu")
!   INQUIRE(FILE="WriteData_test_1.vtu", EXIST=isok)
!   CALL OK(isok, "WriteData")
!
!   dval = [(REAL(ii, DFP), ii=1, 9)]
!   CALL obj%WriteData(filename="WriteData_test_1_data.vtu", &
!     DATA=dval, dataName=String("u"))
!   INQUIRE(FILE="WriteData_test_1_data.vtu", EXIST=isok)
!   CALL OK(isok, "WriteData with data")
!
!   CALL obj%ExportToVTK(vtk=vtk, filename="WriteData_test_1_export.vtu")
!   CALL vtk%DEALLOCATE()
!   INQUIRE(FILE="WriteData_test_1_export.vtu", EXIST=isok)
!   CALL OK(isok, "ExportToVTK")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_WriteData_vtk]]

INTERFACE
  MODULE SUBROUTINE obj_ExportToVTK(obj, vtk, nodeCoord, filename, &
                                    opentag, content, closetag)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    TYPE(VTKFile_), INTENT(INOUT) :: vtk
    !! VTK file
    REAL(DFP), INTENT(IN), OPTIONAL :: nodeCoord(:, :)
    !! node coordinates in xiJ format, SIZE(nodeCoord) = [3, number of nodes]
    CHARACTER(*), INTENT(IN), OPTIONAL :: filename
    !! name of the VTK file, needed if vtk is not initiated
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: opentag
    !! if true (default), write the Piece tag
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: closetag
    !! if true (default), close the Piece tag
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: content
    !! if true (default), write the cells
  END SUBROUTINE obj_ExportToVTK
END INTERFACE

!----------------------------------------------------------------------------
!                                                       WriteData@VTKMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-04
! summary: Write the mesh in a VTK file (.vtu)
!
! WriteData is a generic method. It creates a new unstructured grid VTK file
! (binary appended) and writes the mesh with ExportToVTK. With DATA the nodal
! data is written too:
!
! - DATA(:): one scalar per node, named dataName;
! - DATA(:, :): row i is data i (named dataNames(i)), or with isSingleData one
!   data with SIZE(DATA, 1) components;
! - DATA(:, :, :): DATA(i, :, :) is data i with SIZE(DATA, 2) components.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   filename : CHARACTER(*), optional, intent(in) - name of the VTK file
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: DFP
!   USE GlobalData, ONLY: I4B
!   USE GlobalData, ONLY: LGT
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE String_Class, ONLY: String
!   USE Test_Method, ONLY: OK
!   USE VTKFile_Class, ONLY: VTKFile_
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!   TYPE(VTKFile_) :: vtk
!   REAL(DFP) :: dval(9)
!   LOGICAL(LGT) :: isok
!   INTEGER(I4B) :: ii
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!
!   CALL obj%WriteData(filename="WriteData_test_1.vtu")
!   INQUIRE(FILE="WriteData_test_1.vtu", EXIST=isok)
!   CALL OK(isok, "WriteData")
!
!   dval = [(REAL(ii, DFP), ii=1, 9)]
!   CALL obj%WriteData(filename="WriteData_test_1_data.vtu", &
!     DATA=dval, dataName=String("u"))
!   INQUIRE(FILE="WriteData_test_1_data.vtu", EXIST=isok)
!   CALL OK(isok, "WriteData with data")
!
!   CALL obj%ExportToVTK(vtk=vtk, filename="WriteData_test_1_export.vtu")
!   CALL vtk%DEALLOCATE()
!   INQUIRE(FILE="WriteData_test_1_export.vtu", EXIST=isok)
!   CALL OK(isok, "ExportToVTK")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_WriteData_vtk_r1]], [[obj_WriteData_vtk_r2]],
!   [[obj_WriteData_vtk_r3]], [[obj_ExportToVTK]]

INTERFACE
  MODULE SUBROUTINE obj_WriteData_vtk(obj, filename)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    CHARACTER(*), INTENT(IN), OPTIONAL :: filename
    !! name of the VTK file
  END SUBROUTINE obj_WriteData_vtk
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Write the mesh and a scalar nodal data
!
! See WriteData_vtk.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   filename : CHARACTER(*), optional, intent(in) - name of the VTK file
!   DATA(:) : REAL(DFP), intent(in) - nodal data, SIZE(DATA) = number of nodes
!   dataName : TYPE(String), intent(in) - name of the data
!
! See also: [[obj_WriteData_vtk]]

INTERFACE
  MODULE SUBROUTINE obj_WriteData_vtk_r1(obj, filename, DATA, dataName)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    CHARACTER(*), INTENT(IN), OPTIONAL :: filename
    !! name of the VTK file
    REAL(DFP), INTENT(IN) :: DATA(:)
    !! nodal data, SIZE(DATA) = number of nodes
    TYPE(String), INTENT(IN) :: dataName
    !! name of the data
  END SUBROUTINE obj_WriteData_vtk_r1
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Write the mesh and nodal data
!
! See WriteData_vtk.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   filename : CHARACTER(*), optional, intent(in) - name of the VTK file
!   DATA(:,:) : REAL(DFP), intent(in) - nodal data, rows are data (or
!     components), columns are nodes
!   dataNames(:) : TYPE(String), intent(in) - names of the data
!   isSingleData : LOGICAL(LGT), optional, intent(in) - if true, DATA is one
!     data with SIZE(DATA, 1) components; default is false
!
! See also: [[obj_WriteData_vtk]]

INTERFACE
  MODULE SUBROUTINE obj_WriteData_vtk_r2(obj, filename, DATA, dataNames, &
                                         isSingleData)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    CHARACTER(*), INTENT(IN), OPTIONAL :: filename
    !! name of the VTK file
    REAL(DFP), INTENT(IN) :: DATA(:, :)
    !! nodal data, rows are data (or components), columns are nodes
    ! row: number of data or number of components of data
    ! column: number of nodes in mesh
    TYPE(String), INTENT(IN) :: dataNames(:)
    !! names of the data
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: isSingleData
    !! if true, DATA is one data with SIZE(DATA, 1) components; default is
    !! false
  END SUBROUTINE obj_WriteData_vtk_r2
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Write the mesh and vector nodal data
!
! See WriteData_vtk.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   filename : CHARACTER(*), optional, intent(in) - name of the VTK file
!   DATA(:,:,:) : REAL(DFP), intent(in) - nodal data, DATA(i, :, :) is data i
!     with SIZE(DATA, 2) components
!   dataNames(:) : TYPE(String), intent(in) - names of the data
!
! See also: [[obj_WriteData_vtk]]

INTERFACE
  MODULE SUBROUTINE obj_WriteData_vtk_r3(obj, filename, DATA, dataNames)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    CHARACTER(*), INTENT(IN), OPTIONAL :: filename
    !! name of the VTK file
    REAL(DFP), INTENT(IN) :: DATA(:, :, :)
    !! nodal data, DATA(i, :, :) is data i with SIZE(DATA, 2) components
    ! dim1: number of data
    ! dim2: number of components of each data
    ! dim3: number of nodes in mesh
    TYPE(String), INTENT(IN) :: dataNames(:)
    !! names of the data
  END SUBROUTINE obj_WriteData_vtk_r3
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Display the content of the mesh
!
! Display the status flags and parameters of the mesh.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!   msg : CHARACTER(*), intent(in) - message printed first
!   unitno : INTEGER(I4B), optional, intent(in) - unit number of the output;
!     default is stdout
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!
!   CALL obj%Display("mesh:")
!   CALL obj%DisplayMeshInfo("mesh info:")
!   CALL obj%DisplayNodeData("node data:")
!   CALL obj%DisplayElementData("element 13:", globalElement=13_I4B)
!   CALL obj%InitiateFacetElements()
!   CALL obj%DisplayFacetData("facet data:")
!   CALL obj%DisplayInternalFacetData("internal facet data:")
!   CALL obj%DisplayBoundaryFacetData("boundary facet data:")
!   CALL OK(obj%GetTotalElements() .EQ. 4_I4B, "mesh is not changed")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_DisplayMeshInfo]], [[obj_DisplayNodeData]],
!   [[obj_DisplayElementData]], [[obj_DisplayFacetData]]

INTERFACE AbstractMeshDisplay
  MODULE SUBROUTINE obj_Display(obj, msg, unitno)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
    CHARACTER(*), INTENT(IN) :: msg
    !! message printed first
    INTEGER(I4B), INTENT(IN), OPTIONAL :: unitno
    !! unit number of the output; default is stdout
  END SUBROUTINE obj_Display
END INTERFACE AbstractMeshDisplay

!----------------------------------------------------------------------------
!                                              DisplayNodeData@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Display the node data
!
! Display the data of every node.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!   msg : CHARACTER(*), intent(in) - message printed first
!   unitno : INTEGER(I4B), optional, intent(in) - unit number of the output;
!     default is stdout
!
! See also: [[obj_Display]]

INTERFACE
  MODULE SUBROUTINE obj_DisplayNodeData(obj, msg, unitno)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
    CHARACTER(*), INTENT(IN) :: msg
    !! message printed first
    INTEGER(I4B), INTENT(IN), OPTIONAL :: unitno
    !! unit number of the output; default is stdout
  END SUBROUTINE obj_DisplayNodeData
END INTERFACE

!----------------------------------------------------------------------------
!                                              DisplayElementData@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Display the element data
!
! Display the data of one element, or of all elements if globalElement is
! absent.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!   msg : CHARACTER(*), intent(in) - message printed first
!   unitno : INTEGER(I4B), optional, intent(in) - unit number of the output;
!     default is stdout
!   globalElement : INTEGER(I4B), optional, intent(in) - global element number
!     (local if islocal is true); if absent, all elements are displayed
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! See also: [[obj_Display]]

INTERFACE
  MODULE SUBROUTINE obj_DisplayElementData(obj, msg, unitno, globalElement, &
                                           islocal)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
    CHARACTER(*), INTENT(IN) :: msg
    !! message printed first
    INTEGER(I4B), INTENT(IN), OPTIONAL :: unitno
    !! unit number of the output; default is stdout
    INTEGER(I4B), INTENT(IN), OPTIONAL :: globalElement
    !! global element number (local if islocal is true); if absent, all
    !! elements are displayed
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
  END SUBROUTINE obj_DisplayElementData
END INTERFACE

!----------------------------------------------------------------------------
!                                              DisplayFacetData@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Display the internal facet data
!
! Display the facet data of the internal facets.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!   msg : CHARACTER(*), intent(in) - message printed first
!   unitno : INTEGER(I4B), optional, intent(in) - unit number of the output;
!     default is stdout
!
! See also: [[obj_Display]]

INTERFACE
  MODULE SUBROUTINE obj_DisplayInternalFacetData(obj, msg, unitno)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
    CHARACTER(*), INTENT(IN) :: msg
    !! message printed first
    INTEGER(I4B), INTENT(IN), OPTIONAL :: unitno
    !! unit number of the output; default is stdout
  END SUBROUTINE obj_DisplayInternalFacetData
END INTERFACE

!----------------------------------------------------------------------------
!                                              DisplayFacetData@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Display the facet data
!
! Display the data of every facet element.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!   msg : CHARACTER(*), intent(in) - message printed first
!   unitno : INTEGER(I4B), optional, intent(in) - unit number of the output;
!     default is stdout
!
! See also: [[obj_Display]]

INTERFACE
  MODULE SUBROUTINE obj_DisplayFacetData(obj, msg, unitno)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
    CHARACTER(*), INTENT(IN) :: msg
    !! message printed first
    INTEGER(I4B), INTENT(IN), OPTIONAL :: unitno
    !! unit number of the output; default is stdout
  END SUBROUTINE obj_DisplayFacetData
END INTERFACE

!----------------------------------------------------------------------------
!                                        DisplayBoundaryFacetData@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Display the boundary facet data
!
! Display the facet data of the boundary facets.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!   msg : CHARACTER(*), intent(in) - message printed first
!   unitno : INTEGER(I4B), optional, intent(in) - unit number of the output;
!     default is stdout
!
! See also: [[obj_Display]]

INTERFACE
  MODULE SUBROUTINE obj_DisplayBoundaryFacetData(obj, msg, unitno)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
    CHARACTER(*), INTENT(IN) :: msg
    !! message printed first
    INTEGER(I4B), INTENT(IN), OPTIONAL :: unitno
    !! unit number of the output; default is stdout
  END SUBROUTINE obj_DisplayBoundaryFacetData
END INTERFACE

!----------------------------------------------------------------------------
!                                               DisplayMeshInfo@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-31
! summary: Display the statistics of the mesh
!
! Display the number of nodes, elements and other statistics.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!   msg : CHARACTER(*), intent(in) - message printed first
!   unitno : INTEGER(I4B), optional, intent(in) - unit number of the output;
!     default is stdout
!
! See also: [[obj_Display]]

INTERFACE
  MODULE SUBROUTINE obj_DisplayMeshInfo(obj, msg, unitno)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
    CHARACTER(*), INTENT(IN) :: msg
    !! message printed first
    INTEGER(I4B), INTENT(IN), OPTIONAL :: unitno
    !! unit number of the output; default is stdout
  END SUBROUTINE obj_DisplayMeshInfo
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetNodeCoord2@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-24
! summary: Get the coordinates of all nodes
!
! Column i of nodeCoord is local node i. See GetNodeCoord.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   nodeCoord(:,:) : REAL(DFP), intent(inout) - node coordinates in xiJ
!     format, SIZE(nodeCoord, 2) must be at least the number of nodes
!   nrow : INTEGER(I4B), intent(out) - number of rows written
!   ncol : INTEGER(I4B), intent(out) - number of columns written
!
! See also: [[obj_GetNodeCoord1]]

INTERFACE
  MODULE SUBROUTINE obj_GetNodeCoord2(obj, nodeCoord, nrow, ncol)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    REAL(DFP), INTENT(INOUT) :: nodeCoord(:, :)
    !! node coordinates in xiJ format, SIZE(nodeCoord, 2) must be at least the
    !! number of nodes
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! number of rows written
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! number of columns written
  END SUBROUTINE obj_GetNodeCoord2
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetNodeCoord@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-24
! summary: Get the coordinates of the nodes of an element
!
! Column i of nodeCoord is node i of the element connectivity. See
! GetNodeCoord.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   nodeCoord(:,:) : REAL(DFP), intent(inout) - node coordinates in xiJ format
!   nrow : INTEGER(I4B), intent(out) - number of rows written
!   ncol : INTEGER(I4B), intent(out) - number of columns written
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! See also: [[obj_GetNodeCoord1]]

INTERFACE
  MODULE SUBROUTINE obj_GetNodeCoord3(obj, nodeCoord, nrow, &
                                      ncol, globalElement, islocal)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    REAL(DFP), INTENT(INOUT) :: nodeCoord(:, :)
    !! node coordinates in xiJ format
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! nrow: number of rows written
    !! ncol: number of columns written
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
  END SUBROUTINE obj_GetNodeCoord3
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetNodeCoord@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-24
! summary: Get the coordinates of a list of nodes
!
! Column i of nodeCoord is node globalNode(i). See GetNodeCoord.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   nodeCoord(:,:) : REAL(DFP), intent(inout) - node coordinates in xiJ format
!   nrow : INTEGER(I4B), intent(out) - number of rows written
!   ncol : INTEGER(I4B), intent(out) - number of columns written
!   globalNode(:) : INTEGER(I4B), intent(in) - global node numbers (local if
!     islocal is true)
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! See also: [[obj_GetNodeCoord1]]

INTERFACE
  MODULE SUBROUTINE obj_GetNodeCoord4(obj, nodeCoord, nrow, ncol, &
                                      globalNode, islocal)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    REAL(DFP), INTENT(INOUT) :: nodeCoord(:, :)
    !! node coordinates in xiJ format
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! nrow: number of rows written
    !! ncol: number of columns written
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global node numbers (local if islocal is true)
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
  END SUBROUTINE obj_GetNodeCoord4
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetNodeCoord@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-24
! summary: Get the coordinates of a node
!
! See GetNodeCoord.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   nodeCoord(:) : REAL(DFP), intent(inout) - coordinates of the node
!   tsize : INTEGER(I4B), intent(out) - number of coordinates written
!   globalNode : INTEGER(I4B), intent(in) - global node number, or local node
!     number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! See also: [[obj_GetNodeCoord1]]

INTERFACE
  MODULE SUBROUTINE obj_GetNodeCoord5(obj, nodeCoord, tsize, &
                                      globalNode, islocal)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    REAL(DFP), INTENT(INOUT) :: nodeCoord(:)
    !! coordinates of the node
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of coordinates written
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global node number, or local node number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
  END SUBROUTINE obj_GetNodeCoord5
END INTERFACE

!----------------------------------------------------------------------------
!                                                  GetNearestNode@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-10
! summary: Get the node nearest to a point
!
! GetNearestNode returns the global node number and the coordinates of the
! node nearest to qv. The kdtree is initiated if needed. With nn, the nn
! nearest nodes are returned; the kdtree is not sorted, so their order is not
! fixed.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!   qv(:) : REAL(DFP), intent(in) - coordinates of the query point, SIZE(qv)
!     >= nsd
!   x(:) : REAL(DFP), intent(inout) - coordinates of the nearest node, SIZE(x)
!     >= 3
!   globalNode : INTEGER(I4B), intent(out) - global node number of the nearest
!     node
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: DFP
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!   REAL(DFP), PARAMETER :: tol = 1.0E-8_DFP
!   REAL(DFP) :: x(3), xn(3, 2)
!   INTEGER(I4B) :: node, nodes(2)
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!
!   ! nearest node to (0.9, 0.9) is the corner node 4 at (1, 1)
!   CALL obj%GetNearestNode(qv=[0.9_DFP, 0.9_DFP], x=x, globalNode=node)
!   CALL OK(node .EQ. 4_I4B, "nearest node to (0.9, 0.9)")
!   CALL OK(ALL(ABS(x(1:2) - [1.0_DFP, 1.0_DFP]) .LT. tol), "coordinates")
!
!   ! nearest node to (0.45, 0.55) is the center node 9
!   CALL obj%GetNearestNode(qv=[0.45_DFP, 0.55_DFP], x=x, globalNode=node)
!   CALL OK(node .EQ. 9_I4B, "nearest node to (0.45, 0.55)")
!
!   ! two nearest nodes to (0.1, 0.0): node 1 and node 5
!   CALL obj%GetNearestNode(qv=[0.1_DFP, 0.0_DFP], x=xn, &
!     globalNode=nodes, nn=2_I4B)
!   CALL OK(ANY(nodes .EQ. 1_I4B) .AND. ANY(nodes .EQ. 5_I4B), &
!     "two nearest nodes")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetNearestNode2]], [[obj_InitiateKdtree]]

INTERFACE
  MODULE SUBROUTINE obj_GetNearestNode1(obj, qv, x, globalNode)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
    REAL(DFP), INTENT(IN) :: qv(:)
    !! coordinates of the query point, SIZE(qv) >= nsd
    REAL(DFP), INTENT(INOUT) :: x(:)
    !! coordinates of the nearest node, SIZE(x) >= 3
    INTEGER(I4B), INTENT(OUT) :: globalNode
    !! global node number of the nearest node
  END SUBROUTINE obj_GetNearestNode1
END INTERFACE

!----------------------------------------------------------------------------
!                                                  GetNearestNode@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-11
! summary: Get the nn nodes nearest to a point
!
! See GetNearestNode.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!   qv(:) : REAL(DFP), intent(in) - coordinates of the query point, SIZE(qv)
!     >= nsd
!   x(:,:) : REAL(DFP), intent(inout) - coordinates of the nearest nodes,
!     SIZE(x, 2) >= nn
!   globalNode(:) : INTEGER(I4B), intent(inout) - global node numbers,
!     SIZE(globalNode) >= nn
!   nn : INTEGER(I4B), intent(in) - number of nearest nodes
!
! See also: [[obj_GetNearestNode1]]

INTERFACE
  MODULE SUBROUTINE obj_GetNearestNode2(obj, qv, x, globalNode, nn)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
    REAL(DFP), INTENT(IN) :: qv(:)
    !! coordinates of the query point, SIZE(qv) >= nsd
    REAL(DFP), INTENT(INOUT) :: x(:, :)
    !! coordinates of the nearest nodes, SIZE(x, 2) >= nn
    INTEGER(I4B), INTENT(INOUT) :: globalNode(:)
    !! global node numbers, SIZE(globalNode) >= nn
    INTEGER(I4B), INTENT(IN) :: nn
    !! number of nearest nodes
  END SUBROUTINE obj_GetNearestNode2
END INTERFACE

!----------------------------------------------------------------------------
!                                    GetTotalElementsTopologyWise@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-12
! summary: Get the total number of elements of each topology
!
! The topologies are, in this order: Point, Line, Triangle, Quadrangle,
! Tetrahedron, Hexahedron, Prism, Pyramid.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!
! Returns:
!   ans(8) : INTEGER(I4B) - number of elements of each topology
!
! See also: [[obj_GetElemType]]

INTERFACE
  MODULE PURE FUNCTION obj_GetTotalElementsTopologyWise(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B) :: ans(8)
    !! number of elements of each topology
  END FUNCTION obj_GetTotalElementsTopologyWise
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetTotalTopology@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-12
! summary: Get the number of topologies in the mesh
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!
! Returns:
!   ans : INTEGER(I4B) - number of different element topologies
!
! See also: [[obj_GetElemType]]

INTERFACE
  MODULE PURE FUNCTION obj_GetTotalTopology(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B) :: ans
    !! number of different element topologies
  END FUNCTION obj_GetTotalTopology
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetElementTopology@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-12
! summary: Get the topologies present in the mesh
!
! Entries after GetTotalTopology() are zero.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!
! Returns:
!   ans(8) : INTEGER(I4B) - element topologies (for example Quadrangle)
!     present in the mesh
!
! See also: [[obj_GetElemType]]

INTERFACE
  MODULE FUNCTION obj_GetElemTopology1(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B) :: ans(8)
    !! element topologies (for example Quadrangle) present in the mesh
  END FUNCTION obj_GetElemTopology1
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetElementTopology@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-12
! summary: Get the topology of an element
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans : INTEGER(I4B) - element topology, for example Quadrangle
!
! See also: [[obj_GetElemType]]

INTERFACE
  MODULE FUNCTION obj_GetElemTopology2(obj, globalElement, islocal) &
    RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    INTEGER(I4B) :: ans
    !! element topology, for example Quadrangle
  END FUNCTION obj_GetElemTopology2
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetElemTopologyIndx@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the topology index of an element
!
! The index is the position of the topology in: Point, Line, Triangle,
! Quadrangle, Tetrahedron, Hexahedron, Prism, Pyramid.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans : INTEGER(I4B) - index of the topology of the element
!
! See also: [[obj_GetElemType]]

INTERFACE
  MODULE FUNCTION obj_GetElemTopologyIndx(obj, globalElement, islocal) &
    RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    INTEGER(I4B) :: ans
    !! index of the topology of the element
  END FUNCTION obj_GetElemTopologyIndx
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetElemType@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-13
! summary: Get the element type of an element
!
! Return the element name, for example Quadrangle4. GetElemTopology returns
! the topology (Quadrangle), and GetElemTopologyIndx its index.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans : INTEGER(I4B) - element name, for example Quadrangle4
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE BaseType, ONLY: TypeElemNameOpt
!   USE BaseType, ONLY: TypeQuadrangleNameOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!   INTEGER(I4B) :: topo(8)
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!
!   CALL OK(obj%GetElemType(13_I4B) .EQ. &
!     TypeQuadrangleNameOpt%quadrangle4, "GetElemType")
!   CALL OK(obj%GetElemTopology(13_I4B) .EQ. TypeElemNameOpt%quadrangle, &
!     "GetElemTopology")
!   CALL OK(obj%GetElemTopologyIndx(13_I4B) .EQ. 4_I4B, &
!     "GetElemTopologyIndx")
!   CALL OK(obj%GetTotalTopology() .EQ. 1_I4B, "GetTotalTopology")
!   topo = obj%GetElemTopology()
!   CALL OK(topo(1) .EQ. TypeElemNameOpt%quadrangle, "topologies")
!   topo = obj%GetTotalElementsTopologyWise()
!   CALL OK(ALL(topo .EQ. [0, 0, 0, 4, 0, 0, 0, 0]), &
!     "GetTotalElementsTopologyWise")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetElemTopology1]], [[obj_GetElemTopology2]],
!   [[obj_GetElemTopologyIndx]], [[obj_GetTotalTopology]],
!   [[obj_GetTotalElementsTopologyWise]]

INTERFACE
  MODULE FUNCTION obj_GetElemType(obj, globalElement, islocal) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    INTEGER(I4B) :: ans
    !! element name, for example Quadrangle4
  END FUNCTION obj_GetElemType
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetElemData@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-18
! summary: Get a copy of the element data
!
! Copy the element data of an element in elemdata.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   elemdata : TYPE(ElemData_), intent(inout) - copy of the element data
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ElemData_Class, ONLY: ElemData_
!   USE ElemData_Class, ONLY: Elemdata_globalElemNum
!   USE ElemData_Class, ONLY: Elemdata_localElemNum
!   USE ElemData_Class, ONLY: Elemdata_Meshid
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!   TYPE(ElemData_) :: elemdata
!   TYPE(ElemData_), POINTER :: ptr
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!
!   CALL obj%GetElemData(elemdata=elemdata, globalElement=15_I4B)
!   CALL OK(Elemdata_globalElemNum(elemdata) .EQ. 15_I4B, "global number")
!   CALL OK(Elemdata_localElemNum(elemdata) .EQ. 3_I4B, "local number")
!   CALL OK(Elemdata_Meshid(elemdata) .EQ. 1_I4B, "mesh id")
!
!   ptr => obj%GetElemDataPointer(globalElement=3_I4B, islocal=.TRUE.)
!   CALL OK(Elemdata_globalElemNum(ptr) .EQ. 15_I4B, "pointer")
!   ptr => NULL()
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetElemDataPointer]]

INTERFACE
  MODULE SUBROUTINE obj_GetElemData(obj, elemdata, globalElement, islocal)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    TYPE(ElemData_), INTENT(INOUT) :: elemdata
    !! copy of the element data
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
  END SUBROUTINE obj_GetElemData
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetElemDataPointer@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-18
! summary: Get a pointer to the element data
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans : TYPE(ElemData_), pointer - pointer to the element data of the
!     element
!
! See also: [[obj_GetElemData]]

INTERFACE
  MODULE FUNCTION obj_GetElemDataPointer(obj, globalElement, islocal) &
    RESULT(ans)
    CLASS(AbstractMesh_), TARGET, INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    TYPE(ElemData_), POINTER :: ans
    !! pointer to the element data of the element
  END FUNCTION obj_GetElemDataPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetNNE@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-25
! summary: Get the number of nodes in an element
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans : INTEGER(I4B) - number of nodes of the element
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!
!   CALL OK(obj%GetNNE(13_I4B) .EQ. 4_I4B, "global element 13")
!   CALL OK(obj%GetNNE(1_I4B, islocal=.TRUE.) .EQ. 4_I4B, &
!     "local element 1")
!   CALL OK(obj%GetMaxNNE() .EQ. 4_I4B, "GetMaxNNE")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetMaxNNE]]

INTERFACE
  MODULE FUNCTION obj_GetNNE(obj, globalElement, islocal) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    INTEGER(I4B) :: ans
    !! number of nodes of the element
  END FUNCTION obj_GetNNE
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetNNE@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-25
! summary: Get the maximum number of nodes in an element
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!
! Returns:
!   ans : INTEGER(I4B) - largest number of nodes of an element of the mesh
!
! See also: [[obj_GetNNE]]

INTERFACE
  MODULE FUNCTION obj_GetMaxNNE(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B) :: ans
    !! largest number of nodes of an element of the mesh
  END FUNCTION obj_GetMaxNNE
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetTotalElements@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Get the total number of elements
!
! GetTotalElements is a generic method: without arguments it returns the
! number of elements of the mesh (also Size and GetTotalCells); with meshid it
! returns the number of active elements of these mesh ids.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!
! Returns:
!   ans : INTEGER(I4B) - number of elements of the mesh
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!
!   CALL OK(obj%GetTotalElements() .EQ. 4_I4B, "whole mesh")
!   CALL OK(obj%SIZE() .EQ. 4_I4B, "Size")
!   CALL OK(obj%GetTotalCells() .EQ. 4_I4B, "GetTotalCells")
!   CALL OK(obj%GetTotalElements(meshid=1_I4B) .EQ. 4_I4B, "meshid=1")
!   CALL OK(obj%GetTotalElements(meshid=[1_I4B]) .EQ. 4_I4B, &
!     "meshid=[1]")
!   CALL OK(obj%GetTotalElements(meshid=2_I4B) .EQ. 0_I4B, "meshid=2")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetTotalElements2]], [[obj_GetTotalElements3]]

INTERFACE
  MODULE FUNCTION obj_Size(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B) :: ans
    !! number of elements of the mesh
  END FUNCTION obj_Size
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetTotalElements@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-17
! summary: Get the number of elements of a mesh id
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   meshid : INTEGER(I4B), intent(in) - mesh id (entity number) of the
!     elements
!
! Returns:
!   ans : INTEGER(I4B) - number of active elements with mesh id meshid
!
! See also: [[obj_Size]]

INTERFACE
  MODULE FUNCTION obj_GetTotalElements2(obj, meshid) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: meshid
    !! mesh id (entity number) of the elements
    INTEGER(I4B) :: ans
    !! number of active elements with mesh id meshid
  END FUNCTION obj_GetTotalElements2
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetTotalElements@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-20
! summary: Get the number of elements of mesh ids
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   meshid(:) : INTEGER(I4B), intent(in) - mesh ids (entity numbers)
!
! Returns:
!   ans : INTEGER(I4B) - number of active elements with a mesh id in meshid
!
! See also: [[obj_Size]]

INTERFACE
  MODULE FUNCTION obj_GetTotalElements3(obj, meshid) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: meshid(:)
    !! mesh ids (entity numbers)
    INTEGER(I4B) :: ans
    !! number of active elements with a mesh id in meshid
  END FUNCTION obj_GetTotalElements3
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetElemNum@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Get the element numbers of the mesh
!
! GetElemNum returns the global (or local) element numbers of all elements, or
! of the elements of a mesh id. GetElemNum_ does the same without allocation
! (also for a list of mesh ids).
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, local element
!     numbers are returned; default is false
!
! Returns:
!   ans(:) : INTEGER(I4B), allocatable - element numbers
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!   INTEGER(I4B), ALLOCATABLE :: elems(:)
!   INTEGER(I4B) :: buf(10), tsize
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!
!   elems = obj%GetElemNum()
!   CALL OK(ALL(elems .EQ. [13, 14, 15, 16]), "global")
!   elems = obj%GetElemNum(islocal=.TRUE.)
!   CALL OK(ALL(elems .EQ. [1, 2, 3, 4]), "local")
!   elems = obj%GetElemNum(meshid=1_I4B)
!   CALL OK(ALL(elems .EQ. [13, 14, 15, 16]), "meshid")
!
!   CALL obj%GetElemNum_(islocal=.FALSE., ans=buf, tsize=tsize)
!   CALL OK(ALL(buf(1:tsize) .EQ. [13, 14, 15, 16]), "GetElemNum_")
!   CALL obj%GetElemNum_(meshid=1_I4B, islocal=.TRUE., ans=buf, &
!     tsize=tsize)
!   CALL OK(ALL(buf(1:tsize) .EQ. [1, 2, 3, 4]), "GetElemNum_ meshid")
!   CALL obj%GetElemNum_(meshid=[1_I4B], islocal=.FALSE., ans=buf, &
!     tsize=tsize)
!   CALL OK(tsize .EQ. 4_I4B, "GetElemNum_ meshid(:)")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetElemNum2]], [[obj_GetElemNum1_]], [[obj_GetElemNum2_]],
!   [[obj_GetElemNum3_]]

INTERFACE
  MODULE FUNCTION obj_GetElemNum1(obj, islocal) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, local element numbers are returned; default is false
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !! element numbers
  END FUNCTION obj_GetElemNum1
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetElemNum@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-17
! summary: Get the element numbers of a mesh id
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   meshid : INTEGER(I4B), intent(in) - mesh id (entity number) of the
!     elements
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, local element
!     numbers are returned; default is false
!
! Returns:
!   ans(:) : INTEGER(I4B), allocatable - element numbers
!
! See also: [[obj_GetElemNum1]]

INTERFACE
  MODULE FUNCTION obj_GetElemNum2(obj, meshid, islocal) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: meshid
    !! mesh id (entity number) of the elements
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, local element numbers are returned; default is false
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !! element numbers
  END FUNCTION obj_GetElemNum2
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetElemNum_@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the element numbers without allocation
!
! See GetElemNum.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   islocal : LOGICAL(LGT), intent(in) - if true, local element numbers are
!     returned
!   ans(:) : INTEGER(I4B), intent(inout) - element numbers, size must be at
!     least tsize
!   tsize : INTEGER(I4B), intent(out) - number of entries written in ans
!
! See also: [[obj_GetElemNum1]]

INTERFACE
  MODULE SUBROUTINE obj_GetElemNum1_(obj, islocal, ans, tsize)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, local element numbers are returned
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! element numbers, size must be at least tsize
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written in ans
  END SUBROUTINE obj_GetElemNum1_
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetElemNum_@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the element numbers of a mesh id
!
! See GetElemNum.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   meshid : INTEGER(I4B), intent(in) - mesh id (entity number)
!   islocal : LOGICAL(LGT), intent(in) - if true, local element numbers are
!     returned
!   ans(:) : INTEGER(I4B), intent(inout) - element numbers, size must be at
!     least tsize
!   tsize : INTEGER(I4B), intent(out) - number of entries written in ans
!
! See also: [[obj_GetElemNum1]]

INTERFACE
  MODULE SUBROUTINE obj_GetElemNum2_(obj, meshid, islocal, ans, tsize)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: meshid
    !! mesh id (entity number)
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, local element numbers are returned
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! element numbers, size must be at least tsize
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written in ans
  END SUBROUTINE obj_GetElemNum2_
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetElemNum_@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the element numbers of mesh ids
!
! See GetElemNum.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   meshid(:) : INTEGER(I4B), intent(in) - mesh ids (entity numbers)
!   islocal : LOGICAL(LGT), intent(in) - if true, local element numbers are
!     returned
!   ans(:) : INTEGER(I4B), intent(inout) - element numbers, size must be at
!     least tsize
!   tsize : INTEGER(I4B), intent(out) - number of entries written in ans
!
! See also: [[obj_GetElemNum1]]

INTERFACE
  MODULE SUBROUTINE obj_GetElemNum3_(obj, meshid, islocal, ans, tsize)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: meshid(:)
    !! mesh ids (entity numbers)
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, local element numbers are returned
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! element numbers, size must be at least tsize
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written in ans
  END SUBROUTINE obj_GetElemNum3_
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetBoundingEntity@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Get the bounding entities of the mesh
!
! Return the entities of dimension xidim - 1 which bound the entities of the
! mesh, sorted by entity number. The sign gives the orientation.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!
! Returns:
!   ans(:) : INTEGER(I4B), allocatable - bounding entity numbers
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!   INTEGER(I4B), ALLOCATABLE :: ent(:)
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!
!   ent = obj%GetBoundingEntity()
!   CALL OK(ALL(ent .EQ. [1, -2, -3, 4]), "GetBoundingEntity")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```

INTERFACE
  MODULE FUNCTION obj_GetBoundingEntity(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !! bounding entity numbers
  END FUNCTION obj_GetBoundingEntity
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetNptrs@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Get the global node numbers of the mesh
!
! GetNptrs returns the global node numbers of all nodes (in local node order,
! which is sorted), or of the elements of a mesh id (in the order of the
! connectivity). GetNptrs_ does the same without allocation, and can also
! return the nodes of a list of elements (in the order of the connectivity,
! without duplicates).
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!
! Returns:
!   ans(obj%tNodes) : INTEGER(I4B) - global node numbers, SIZE(ans) = number
!     of nodes
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!   INTEGER(I4B), ALLOCATABLE :: nodes(:)
!   INTEGER(I4B) :: buf(20), tsize
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!
!   nodes = obj%GetNptrs()
!   CALL OK(ALL(nodes .EQ. [1, 2, 3, 4, 5, 6, 7, 8, 9]), "whole mesh")
!   nodes = obj%GetNptrs(meshid=1_I4B)
!   CALL OK(ALL(nodes .EQ. [1, 5, 9, 7, 6, 3, 2, 8, 4]), "meshid")
!
!   CALL obj%GetNptrs_(ans=buf, tsize=tsize)
!   CALL OK(tsize .EQ. 9_I4B, "GetNptrs_")
!   CALL obj%GetNptrs_(meshid=1_I4B, ans=buf, tsize=tsize)
!   CALL OK(tsize .EQ. 9_I4B, "GetNptrs_ meshid")
!   CALL obj%GetNptrs_(globalElement=[13_I4B, 16_I4B], ans=buf, &
!     tsize=tsize)
!   CALL OK(ALL(buf(1:tsize) .EQ. [1, 5, 9, 7, 8, 4, 6]), &
!     "GetNptrs_ elements")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetNptrs2]], [[obj_GetNptrs1_]], [[obj_GetNptrs2_]],
!   [[obj_GetNptrs3_]]

INTERFACE
  MODULE FUNCTION obj_GetNptrs1(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B) :: ans(obj%tNodes)
    !! global node numbers, SIZE(ans) = number of nodes
  END FUNCTION obj_GetNptrs1
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetNptrs@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Get the global node numbers of a mesh id
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   meshid : INTEGER(I4B), intent(in) - mesh id (entity number) of the
!     elements
!
! Returns:
!   ans(:) : INTEGER(I4B), allocatable - global node numbers
!
! See also: [[obj_GetNptrs1]]

INTERFACE
  MODULE FUNCTION obj_GetNptrs2(obj, meshid) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: meshid
    !! mesh id (entity number) of the elements
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !! global node numbers
  END FUNCTION obj_GetNptrs2
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetNptrs@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-29
! summary: Get the global node numbers without allocation
!
! See GetNptrs.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   ans(:) : INTEGER(I4B), intent(inout) - global node numbers, size must be
!     at least tsize
!   tsize : INTEGER(I4B), intent(out) - number of entries written in ans
!
! See also: [[obj_GetNptrs1]]

INTERFACE
  MODULE SUBROUTINE obj_GetNptrs1_(obj, ans, tsize)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! global node numbers, size must be at least tsize
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written in ans
  END SUBROUTINE obj_GetNptrs1_
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetNptrs@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the global node numbers of a mesh id
!
! See GetNptrs.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   meshid : INTEGER(I4B), intent(in) - mesh id (entity number) of the
!     elements
!   ans(:) : INTEGER(I4B), intent(inout) - global node numbers, size must be
!     at least tsize
!   tsize : INTEGER(I4B), intent(out) - number of entries written in ans
!
! See also: [[obj_GetNptrs1]]

INTERFACE
  MODULE SUBROUTINE obj_GetNptrs2_(obj, meshid, ans, tsize)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: meshid
    !! mesh id (entity number) of the elements
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! global node numbers, size must be at least tsize
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written in ans
  END SUBROUTINE obj_GetNptrs2_
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetNptrs@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the global node numbers of a list of elements
!
! See GetNptrs.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   globalElement(:) : INTEGER(I4B), intent(in) - global element numbers
!     (local if islocal is true)
!   ans(:) : INTEGER(I4B), intent(inout) - global node numbers, size must be
!     at least tsize
!   tsize : INTEGER(I4B), intent(out) - number of entries written in ans
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! See also: [[obj_GetNptrs1]]

INTERFACE
  MODULE SUBROUTINE obj_GetNptrs3_(obj, globalElement, ans, tsize, islocal)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: globalElement(:)
    !! global element numbers (local if islocal is true)
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! global node numbers, size must be at least tsize
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written in ans
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
  END SUBROUTINE obj_GetNptrs3_
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetNptrs@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-10
! summary: Get the global nodes in a bounding box
!
! The kdtree is initiated if needed. The nodes within the radius of the box
! (from its center) are found first. If isStrict is true (default) only the
! nodes inside or on the box are kept. GetElemNumberInBox returns the elements
! around the nodes in the box.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!   box : TYPE(BoundingBox_), intent(in) - bounding box
!   nptrs(:) : INTEGER(I4B), allocatable, intent(inout) - global node numbers
!     in the box, it is reallocated
!   isStrict : LOGICAL(LGT), optional, intent(in) - if true (default), only
!     nodes inside or on the box are returned
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: BoundingBox_
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE BoundingBox_Method, ONLY: BoundingBoxInitiate => Initiate
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: DFP
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE SortUtility, ONLY: Sort
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!   TYPE(BoundingBox_) :: box
!   INTEGER(I4B), ALLOCATABLE :: nodes(:), elems(:)
!   INTEGER(I4B) :: buf(20), buf2(20), tnodes, telems
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!
!   CALL BoundingBoxInitiate(obj=box, nsd=3_I4B, &
!     lim=[-0.1_DFP, 0.6_DFP, -0.1_DFP, &
!     0.6_DFP, -0.1_DFP, 0.1_DFP])
!
!   CALL obj%GetNptrsInBox(box=box, nptrs=nodes)
!   CALL OK(ALL(Sort(nodes) .EQ. [1, 5, 7, 9]), "GetNptrsInBox")
!   CALL obj%GetNptrsInBox_(box=box, nptrs=buf, tnodes=tnodes)
!   CALL OK(tnodes .EQ. 4_I4B, "GetNptrsInBox_")
!
!   CALL obj%GetElemNumberInBox(box=box, elemNumber=elems)
!   CALL OK(ALL(elems .EQ. [13, 14, 15, 16]), "GetElemNumberInBox")
!   CALL obj%GetElemNumberInBox_(box=box, elemNumber=buf2, &
!     tElements=telems, nptrs=buf, &
!     tnodes=tnodes)
!   CALL OK(telems .EQ. 4_I4B .AND. tnodes .EQ. 4_I4B, &
!     "GetElemNumberInBox_")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetNptrsInBox_]], [[obj_GetElemNumberInBox]],
!   [[obj_GetElemNumberInBox_]]

INTERFACE
  MODULE SUBROUTINE obj_GetNptrsInBox(obj, box, nptrs, isStrict)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: nptrs(:)
    !! global node numbers in the box, it is reallocated
    TYPE(BoundingBox_), INTENT(IN) :: box
    !! bounding box
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: isStrict
    !! if true (default), only nodes inside or on the box are returned
  END SUBROUTINE obj_GetNptrsInBox
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetNptrs@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-10
! summary: Get the global nodes in a bounding box without allocation
!
! See GetNptrsInBox.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!   box : TYPE(BoundingBox_), intent(in) - bounding box
!   nptrs(:) : INTEGER(I4B), intent(inout) - global node numbers in the box,
!     size must be at least the number of nodes of the mesh
!   tnodes : INTEGER(I4B), intent(inout) - number of nodes found
!   isStrict : LOGICAL(LGT), optional, intent(in) - if true (default), only
!     nodes inside or on the box are returned
!
! See also: [[obj_GetNptrsInBox]]

INTERFACE
  MODULE SUBROUTINE obj_GetNptrsInBox_(obj, box, nptrs, tnodes, isStrict)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
    TYPE(BoundingBox_), INTENT(IN) :: box
    !! bounding box
    INTEGER(I4B), INTENT(INOUT) :: nptrs(:)
    !! global node numbers in the box, size must be at least the number of
    !! nodes of the mesh
    INTEGER(I4B), INTENT(INOUT) :: tnodes
    !! number of nodes found
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: isStrict
    !! if true (default), only nodes inside or on the box are returned
  END SUBROUTINE obj_GetNptrsInBox_
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetNptrs@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-03
! summary: Get the elements around nodes in a box
!
! Return the global elements around the nodes in the box (sorted, without
! duplicates). See GetNptrsInBox.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!   box : TYPE(BoundingBox_), intent(in) - bounding box
!   elemNumber(:) : INTEGER(I4B), allocatable, intent(inout) - global element
!     numbers, it is reallocated
!
! See also: [[obj_GetNptrsInBox]]

INTERFACE
  MODULE SUBROUTINE obj_GetElemNumberInBox(obj, box, elemNumber)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: elemNumber(:)
    !! global element numbers, it is reallocated
    TYPE(BoundingBox_), INTENT(IN) :: box
    !! bounding box
  END SUBROUTINE obj_GetElemNumberInBox
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetNptrs@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-03
! summary: Get the elements around nodes in a box without allocation
!
! See GetElemNumberInBox.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!   box : TYPE(BoundingBox_), intent(in) - bounding box
!   elemNumber(:) : INTEGER(I4B), intent(inout) - global element numbers, size
!     must be at least tElements
!   tElements : INTEGER(I4B), intent(out) - number of elements found
!   nptrs(:) : INTEGER(I4B), intent(inout) - global node numbers in the box,
!     size must be at least the number of nodes of the mesh
!   tNodes : INTEGER(I4B), intent(out) - number of nodes found
!
! See also: [[obj_GetNptrsInBox]]

INTERFACE
  MODULE SUBROUTINE obj_GetElemNumberInBox_(obj, box, elemNumber, &
                                            tElements, nptrs, tNodes)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
    TYPE(BoundingBox_), INTENT(IN) :: box
    !! bounding box
    INTEGER(I4B), INTENT(INOUT) :: elemNumber(:)
    !! global element numbers, size must be at least tElements
    INTEGER(I4B), INTENT(OUT) :: tElements
    !! number of elements found
    INTEGER(I4B), INTENT(INOUT) :: nptrs(:)
    !! global node numbers in the box, size must be at least the number of
    !! nodes of the mesh
    INTEGER(I4B), INTENT(OUT) :: tNodes
    !! number of nodes found
  END SUBROUTINE obj_GetElemNumberInBox_
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetInternalNptrs@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Get the global numbers of internal nodes
!
! Internal nodes are nodes which are not boundary nodes.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!
! Returns:
!   ans(:) : INTEGER(I4B), allocatable - global numbers of the internal nodes
!
! See also: [[obj_isBoundaryNode]], [[obj_GetInternalNptrs_]]

INTERFACE
  MODULE FUNCTION obj_GetInternalNptrs(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !! global numbers of the internal nodes
  END FUNCTION obj_GetInternalNptrs
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetInternalNptrs_@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-29
! summary: Get the global numbers of internal nodes without allocation
!
! See GetInternalNptrs.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   nptrs(:) : INTEGER(I4B), intent(inout) - global numbers of internal nodes,
!     size must be at least GetTotalInternalNodes()
!
! See also: [[obj_GetInternalNptrs]]

INTERFACE
  MODULE SUBROUTINE obj_GetInternalNptrs_(obj, nptrs)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(INOUT) :: nptrs(:)
    !! global numbers of internal nodes, size must be at least
    !! GetTotalInternalNodes()
  END SUBROUTINE obj_GetInternalNptrs_
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetBoundaryNptrs@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Get the global numbers of boundary nodes
!
! Boundary nodes are all nodes which are not internal nodes.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!
! Returns:
!   ans(:) : INTEGER(I4B), allocatable - global numbers of the boundary nodes
!
! See also: [[obj_isBoundaryNode]]

INTERFACE
  MODULE FUNCTION obj_GetBoundaryNptrs(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !! global numbers of the boundary nodes
  END FUNCTION obj_GetBoundaryNptrs
END INTERFACE

!----------------------------------------------------------------------------
!                                                 isBoundaryNode@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns true if a node is a boundary node
!
! The nodes of the boundary facets are boundary nodes. They are marked when
! the element to elements data is initiated (by Initiate). All other nodes are
! internal nodes.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   globalNode : INTEGER(I4B), intent(in) - global node number, or local node
!     number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans : LOGICAL(LGT) - true if the node is not an internal node
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!   INTEGER(I4B), ALLOCATABLE :: nodes(:)
!   INTEGER(I4B) :: buf(1)
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!
!   CALL OK(obj%IsBoundaryNode(1_I4B), "node 1")
!   CALL OK(.NOT. obj%IsBoundaryNode(9_I4B), "node 9")
!   CALL OK(obj%GetTotalBoundaryNodes() .EQ. 8_I4B, &
!     "GetTotalBoundaryNodes")
!   CALL OK(obj%GetTotalInternalNodes() .EQ. 1_I4B, &
!     "GetTotalInternalNodes")
!   nodes = obj%GetBoundaryNptrs()
!   CALL OK(ALL(nodes .EQ. [1, 2, 3, 4, 5, 6, 7, 8]), "GetBoundaryNptrs")
!   nodes = obj%GetInternalNptrs()
!   CALL OK(ALL(nodes .EQ. [9]), "GetInternalNptrs")
!   CALL obj%GetInternalNptrs_(nptrs=buf)
!   CALL OK(buf(1) .EQ. 9_I4B, "GetInternalNptrs_")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetBoundaryNptrs]], [[obj_GetInternalNptrs]],
!   [[obj_GetTotalBoundaryNodes]], [[obj_GetTotalInternalNodes]]

INTERFACE
  MODULE FUNCTION obj_isBoundaryNode(obj, globalNode, islocal) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global node number, or local node number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    LOGICAL(LGT) :: ans
    !! true if the node is not an internal node
  END FUNCTION obj_isBoundaryNode
END INTERFACE

!----------------------------------------------------------------------------
!                                                  isNodePresent@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns true if a node is present in the mesh
!
! IsNodePresent checks one node or a list of nodes. GetNodeMask,
! IsAnyNodePresent and IsAllNodePresent also check the presence of nodes.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   globalNode : INTEGER(I4B), intent(in) - global node number, or local node
!     number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans : LOGICAL(LGT) - true if the node is present
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: I4B
!   USE GlobalData, ONLY: LGT
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!   LOGICAL(LGT) :: mask(10)
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!
!   CALL OK(obj%IsNodePresent(9_I4B), "node 9")
!   CALL OK(.NOT. obj%IsNodePresent(10_I4B), "node 10")
!   CALL OK(ALL(obj%IsNodePresent([1_I4B, 10_I4B]) .EQV. &
!     [.TRUE., .FALSE.]), "nodes 1 and 10")
!   CALL OK(obj%IsAnyNodePresent([10_I4B, 11_I4B, 2_I4B]), &
!     "IsAnyNodePresent")
!   CALL OK(.NOT. obj%IsAllNodePresent([1_I4B, 2_I4B, 10_I4B]), &
!     "IsAllNodePresent")
!   CALL obj%GetNodeMask(mask=mask)
!   CALL OK(ALL(mask(1:9)) .AND. .NOT. mask(10), "GetNodeMask")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_isNodePresent2]], [[obj_GetNodeMask]],
!   [[obj_isAnyNodePresent]], [[obj_isAllNodePresent]]

INTERFACE
  MODULE FUNCTION obj_isNodePresent1(obj, globalNode, islocal) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global node number, or local node number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    LOGICAL(LGT) :: ans
    !! true if the node is present
  END FUNCTION obj_isNodePresent1
END INTERFACE

!----------------------------------------------------------------------------
!                                                  isNodePresent@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns true for each node present in the mesh
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   globalNode(:) : INTEGER(I4B), intent(in) - global node numbers (local if
!     islocal is true)
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans(SIZE(globalNode)) : LOGICAL(LGT) - ans(i) is true if globalNode(i) is
!     present
!
! See also: [[obj_isNodePresent1]]

INTERFACE
  MODULE FUNCTION obj_isNodePresent2(obj, globalNode, islocal) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global node numbers (local if islocal is true)
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    LOGICAL(LGT) :: ans(SIZE(globalNode))
    !! ans(i) is true if globalNode(i) is present
  END FUNCTION obj_isNodePresent2
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetNodeMask@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-09
! summary: Get a mask of the nodes present in the mesh
!
! Set mask(i) to true for every global node i of the mesh, and false
! otherwise. With local_nptrs, mask(local_nptrs(i)) is set instead.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   mask(:) : LOGICAL(LGT), intent(inout) - mask, SIZE(mask) must be at least
!     GetMaxNodeNumber()
!   local_nptrs(:) : INTEGER(I4B), optional, intent(in) - map from a global
!     node number to a mask entry
!
! See also: [[obj_isNodePresent1]]

INTERFACE
  MODULE SUBROUTINE obj_GetNodeMask(obj, mask, local_nptrs)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    LOGICAL(LGT), INTENT(INOUT) :: mask(:)
    !! mask, SIZE(mask) must be at least GetMaxNodeNumber()
    INTEGER(I4B), INTENT(IN), OPTIONAL :: local_nptrs(:)
    !! map from a global node number to a mask entry
  END SUBROUTINE obj_GetNodeMask
END INTERFACE

!----------------------------------------------------------------------------
!                                                isAnyNodePresent@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns true if any of the nodes is present
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   globalNode(:) : INTEGER(I4B), intent(in) - global node numbers (local if
!     islocal is true)
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans : LOGICAL(LGT) - true if at least one node of globalNode is present
!
! See also: [[obj_isNodePresent1]]

INTERFACE
  MODULE FUNCTION obj_isAnyNodePresent(obj, globalNode, islocal) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global node numbers (local if islocal is true)
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    LOGICAL(LGT) :: ans
    !! true if at least one node of globalNode is present
  END FUNCTION obj_isAnyNodePresent
END INTERFACE

!----------------------------------------------------------------------------
!                                                isAllNodePresent@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns true if all the nodes are present
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   globalNode(:) : INTEGER(I4B), intent(in) - global node numbers (local if
!     islocal is true)
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans : LOGICAL(LGT) - true if every node of globalNode is present
!
! See also: [[obj_isNodePresent1]]

INTERFACE
  MODULE FUNCTION obj_isAllNodePresent(obj, globalNode, islocal) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global node numbers (local if islocal is true)
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    LOGICAL(LGT) :: ans
    !! true if every node of globalNode is present
  END FUNCTION obj_isAllNodePresent
END INTERFACE

!----------------------------------------------------------------------------
!                                                 isElementPresent@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns true if an element is present
!
! A local element number is present if it is in 1 to the number of elements.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans : LOGICAL(LGT) - true if the element is present
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!
!   CALL OK(obj%IsElementPresent(13_I4B), "element 13")
!   CALL OK(.NOT. obj%IsElementPresent(5_I4B), "element 5")
!   CALL OK(.NOT. obj%IsElementPresent(17_I4B), "element 17")
!   CALL OK(obj%IsElementPresent(4_I4B, islocal=.TRUE.), "local 4")
!   CALL OK(.NOT. obj%IsElementPresent(5_I4B, islocal=.TRUE.), &
!     "local 5")
!   CALL OK(obj%IsElementActive(14_I4B), "IsElementActive")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_isElementActive]]

INTERFACE
  MODULE FUNCTION obj_isElementPresent(obj, globalElement, islocal) &
    RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    LOGICAL(LGT) :: ans
    !! true if the element is present
  END FUNCTION obj_isElementPresent
END INTERFACE

!----------------------------------------------------------------------------
!                                               isBoundaryElement@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns true if an element is a boundary element
!
! A boundary element has a facet on the boundary of the mesh. Boundary
! elements are marked when the element to elements data is initiated.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans : LOGICAL(LGT) - true if the element is a boundary or domain boundary
!     element
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!
!   CALL OK(obj%IsBoundaryElement(13_I4B), "IsBoundaryElement")
!   CALL OK(obj%IsDomainBoundaryElement(16_I4B), &
!     "IsDomainBoundaryElement")
!   CALL OK(obj%GetTotalBoundaryElements() .EQ. 4_I4B, &
!     "GetTotalBoundaryElements")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_isDomainBoundaryElement]], [[obj_GetTotalBoundaryElements]]

INTERFACE
  MODULE FUNCTION obj_isBoundaryElement(obj, globalElement, islocal) &
    RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    LOGICAL(LGT) :: ans
    !! true if the element is a boundary or domain boundary element
  END FUNCTION obj_isBoundaryElement
END INTERFACE

!----------------------------------------------------------------------------
!                                         isDomainBoundaryElement@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns true if an element is a domain boundary element
!
! A domain boundary element is a boundary element which is not connected to
! another mesh. In a single mesh every boundary element is a domain boundary
! element.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans : LOGICAL(LGT) - true if the element type is DOMAIN_BOUNDARY_ELEMENT
!
! See also: [[obj_isBoundaryElement]]

INTERFACE
  MODULE FUNCTION obj_isDomainBoundaryElement(obj, globalElement, islocal) &
    RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    LOGICAL(LGT) :: ans
    !! true if the element type is DOMAIN_BOUNDARY_ELEMENT
  END FUNCTION obj_isDomainBoundaryElement
END INTERFACE

!----------------------------------------------------------------------------
!                                            isDomainFacetElement@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns true if a facet element is a domain boundary facet
!
! The facet elements must be initiated (InitiateFacetElements).
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   facetElement : INTEGER(I4B), intent(in) - facet element number
!
! Returns:
!   ans : LOGICAL(LGT) - true if the facet element type is
!     DOMAIN_BOUNDARY_ELEMENT
!
! See also: [[obj_GetCellNumber]]

INTERFACE
  MODULE FUNCTION obj_isDomainFacetElement(obj, facetElement) &
    RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: facetElement
    !! facet element number
    LOGICAL(LGT) :: ans
    !! true if the facet element type is DOMAIN_BOUNDARY_ELEMENT
  END FUNCTION obj_isDomainFacetElement
END INTERFACE

!----------------------------------------------------------------------------
!                                           GetTotalInternalNodes@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Get the number of internal nodes
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!
! Returns:
!   ans : INTEGER(I4B) - number of internal nodes
!
! See also: [[obj_isBoundaryNode]]

INTERFACE
  MODULE FUNCTION obj_GetTotalInternalNodes(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B) :: ans
    !! number of internal nodes
  END FUNCTION obj_GetTotalInternalNodes
END INTERFACE

!----------------------------------------------------------------------------
!                                         GetTotalVertexNodes@MeshDataMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-05
! summary: Get the number of vertex nodes
!
! Vertex nodes are the nodes of the linear element (the corners). For a linear
! mesh all nodes are vertex nodes. GetTotalVertexNodes works for the whole
! mesh, a mesh id, a list of elements, or one element.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!
! Returns:
!   ans : INTEGER(I4B) - number of vertex nodes of the mesh
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!
!   CALL OK(obj%GetTotalVertexNodes() .EQ. 9_I4B, "whole mesh")
!   CALL OK(obj%GetTotalVertexNodes(meshid=1_I4B) .EQ. 9_I4B, "meshid")
!   CALL OK(obj%GetTotalVertexNodes(globalElement=[13_I4B, 14_I4B]) &
!     .EQ. 6_I4B, "elements 13 and 14")
!   CALL OK(obj%GetTotalVertexNodes(globalElement=13_I4B, &
!     islocal=.FALSE.) .EQ. 4_I4B, &
!     "element 13")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/small_tri6_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!   INTEGER(I4B) :: iel
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!
!   CALL OK(obj%GetTotalNodes() .EQ. 37_I4B, "GetTotalNodes")
!   CALL OK(obj%GetTotalVertexNodes() .EQ. 12_I4B, "GetTotalVertexNodes")
!   iel = obj%GetGlobalElemNumber(1_I4B)
!   CALL OK(obj%GetNNE(iel) .EQ. 6_I4B, "GetNNE")
!   CALL OK(obj%GetTotalVertexNodes(globalElement=iel, islocal=.FALSE.) &
!     .EQ. 3_I4B, "vertex nodes of an element")
!   CALL OK(obj%GetOrder(globalElement=iel) .EQ. 2_I4B, "GetOrder")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetTotalVertexNodes2]], [[obj_GetTotalVertexNodes3]],
!   [[obj_GetTotalVertexNodes4]], [[obj_GetTotalNodes1]]

INTERFACE
  MODULE FUNCTION obj_GetTotalVertexNodes1(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B) :: ans
    !! number of vertex nodes of the mesh
  END FUNCTION obj_GetTotalVertexNodes1
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetTotalVertexNodes@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-05
! summary: Get the number of vertex nodes of a mesh id
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   meshid : INTEGER(I4B), intent(in) - mesh id (entity number) of the
!     elements
!
! Returns:
!   ans : INTEGER(I4B) - number of vertex nodes in the elements of the mesh id
!
! See also: [[obj_GetTotalVertexNodes1]]

INTERFACE
  MODULE FUNCTION obj_GetTotalVertexNodes2(obj, meshid) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: meshid
    !! mesh id (entity number) of the elements
    INTEGER(I4B) :: ans
    !! number of vertex nodes in the elements of the mesh id
  END FUNCTION obj_GetTotalVertexNodes2
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetTotalVertexNodes@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-05
! summary: Get the number of vertex nodes of a list of elements
!
! Shared nodes are counted once.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   globalElement(:) : INTEGER(I4B), intent(in) - global element numbers
!     (local if islocal is true)
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans : INTEGER(I4B) - number of vertex nodes of the elements
!
! See also: [[obj_GetTotalVertexNodes1]]

INTERFACE
  MODULE FUNCTION obj_GetTotalVertexNodes3(obj, globalElement, islocal) &
    RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: globalElement(:)
    !! global element numbers (local if islocal is true)
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    INTEGER(I4B) :: ans
    !! number of vertex nodes of the elements
  END FUNCTION obj_GetTotalVertexNodes3
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetTotalVertexNodes@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-05
! summary: Get the number of vertex nodes of an element
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   islocal : LOGICAL(LGT), intent(in) - if true, globalElement is a local
!     element number
!
! Returns:
!   ans : INTEGER(I4B) - number of vertex nodes of the element
!
! See also: [[obj_GetTotalVertexNodes1]]

INTERFACE
  MODULE FUNCTION obj_GetTotalVertexNodes4(obj, globalElement, islocal) &
    RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, globalElement is a local element number
    INTEGER(I4B) :: ans
    !! number of vertex nodes of the element
  END FUNCTION obj_GetTotalVertexNodes4
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetTotalNodes@MeshDataMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Get the number of nodes in the mesh
!
! GetTotalNodes returns the number of nodes of the mesh, of the elements of a
! mesh id, or of a list of elements. Shared nodes are counted once.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!
! Returns:
!   ans : INTEGER(I4B) - number of nodes of the mesh
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!
!   CALL OK(obj%GetTotalNodes() .EQ. 9_I4B, "whole mesh")
!   CALL OK(obj%GetTotalNodes(meshid=1_I4B) .EQ. 9_I4B, "meshid=1")
!
!   ! nodes in elements 13 [1,5,9,7] and 14 [7,9,6,3]
!   CALL OK(obj%GetTotalNodes(globalElement=[13_I4B, 14_I4B]) &
!     .EQ. 6_I4B, "global elements 13 and 14")
!   CALL OK(obj%GetTotalNodes(globalElement=[1_I4B, 2_I4B], &
!     islocal=.TRUE.) .EQ. 6_I4B, &
!     "local elements 1 and 2")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetTotalNodes2]], [[obj_GetTotalNodes3]],
!   [[obj_GetTotalVertexNodes1]]

INTERFACE
  MODULE FUNCTION obj_GetTotalNodes1(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B) :: ans
    !! number of nodes of the mesh
  END FUNCTION obj_GetTotalNodes1
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetTotalNodes@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-17
! summary: Get the number of nodes of a mesh id
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   meshid : INTEGER(I4B), intent(in) - mesh id (entity number) of the
!     elements
!
! Returns:
!   ans : INTEGER(I4B) - number of nodes in the active elements of the mesh id
!
! See also: [[obj_GetTotalNodes1]]

INTERFACE
  MODULE FUNCTION obj_GetTotalNodes2(obj, meshid) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: meshid
    !! mesh id (entity number) of the elements
    INTEGER(I4B) :: ans
    !! number of nodes in the active elements of the mesh id
  END FUNCTION obj_GetTotalNodes2
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetTotalNodes@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-17
! summary: Get the number of nodes of a list of elements
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   globalElement(:) : INTEGER(I4B), intent(in) - global element numbers
!     (local if islocal is true)
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans : INTEGER(I4B) - number of nodes of the active elements
!
! See also: [[obj_GetTotalNodes1]]

INTERFACE
  MODULE FUNCTION obj_GetTotalNodes3(obj, globalElement, islocal) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: globalElement(:)
    !! global element numbers (local if islocal is true)
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    INTEGER(I4B) :: ans
    !! number of nodes of the active elements
  END FUNCTION obj_GetTotalNodes3
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetTotalFaces@MeshDataMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Get the number of faces in the mesh
!
! The faces are numbered by Initiate (volume and surface meshes) or
! InitiateFaceConnectivity.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!
! Returns:
!   ans : INTEGER(I4B) - number of faces
!
! See also: [[obj_InitiateFaceConnectivity]]

INTERFACE
  MODULE PURE FUNCTION obj_GetTotalFaces(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B) :: ans
    !! number of faces
  END FUNCTION obj_GetTotalFaces
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetTotalEdges@MeshDataMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Get the number of edges in the mesh
!
! The edges are numbered by InitiateEdgeConnectivity; before that the number
! of edges is zero.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!
! Returns:
!   ans : INTEGER(I4B) - number of edges
!
! See also: [[obj_InitiateEdgeConnectivity]]

INTERFACE
  MODULE PURE FUNCTION obj_GetTotalEdges(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B) :: ans
    !! number of edges
  END FUNCTION obj_GetTotalEdges
END INTERFACE

!----------------------------------------------------------------------------
!                                     GetTotalBoundaryNodes@MeshDataMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Get the number of boundary nodes
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!
! Returns:
!   ans : INTEGER(I4B) - number of nodes which are not internal nodes
!
! See also: [[obj_isBoundaryNode]]

INTERFACE
  MODULE FUNCTION obj_GetTotalBoundaryNodes(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B) :: ans
    !! number of nodes which are not internal nodes
  END FUNCTION obj_GetTotalBoundaryNodes
END INTERFACE

!----------------------------------------------------------------------------
!                                  GetTotalBoundaryElements@MeshDataMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Get the number of boundary elements
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!
! Returns:
!   ans : INTEGER(I4B) - number of boundary and domain boundary elements
!
! See also: [[obj_isBoundaryElement]]

INTERFACE
  MODULE FUNCTION obj_GetTotalBoundaryElements(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B) :: ans
    !! number of boundary and domain boundary elements
  END FUNCTION obj_GetTotalBoundaryElements
END INTERFACE

!----------------------------------------------------------------------------
!                                                  GetBoundingBox@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Get the bounding box of the mesh
!
! GetBoundingBox returns the smallest box which contains all nodes of the
! mesh. With nodes, the box of the given node coordinates (only the columns of
! the nodes of the mesh) is returned.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!
! Returns:
!   ans : TYPE(BoundingBox_) - bounding box of the nodes of the mesh
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: BoundingBox_
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE BoundingBox_Method, ONLY: OPERATOR(.Xmax.)
!   USE BoundingBox_Method, ONLY: OPERATOR(.Xmin.)
!   USE BoundingBox_Method, ONLY: OPERATOR(.Ymax.)
!   USE BoundingBox_Method, ONLY: OPERATOR(.Ymin.)
!   USE BoundingBox_Method, ONLY: OPERATOR(.Zmax.)
!   USE BoundingBox_Method, ONLY: OPERATOR(.Zmin.)
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: DFP
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!   REAL(DFP), PARAMETER :: tol = 1.0E-8_DFP
!   TYPE(BoundingBox_) :: box
!   REAL(DFP), ALLOCATABLE :: xij(:, :)
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!
!   box = obj%GetBoundingBox()
!   CALL OK(ABS(.Xmin.box) .LT. tol, "Xmin")
!   CALL OK(ABS((.Xmax.box) - 1.0_DFP) .LT. tol, "Xmax")
!   CALL OK(ABS(.Ymin.box) .LT. tol, "Ymin")
!   CALL OK(ABS((.Ymax.box) - 1.0_DFP) .LT. tol, "Ymax")
!   CALL OK(ABS(.Zmin.box) .LT. tol, "Zmin")
!   CALL OK(ABS(.Zmax.box) .LT. tol, "Zmax")
!
!   CALL obj%GetNodeCoord(nodeCoord=xij, hdf5=meshfile, group="")
!   box = obj%GetBoundingBox(nodes=xij)
!   CALL OK(ABS((.Xmax.box) - 1.0_DFP) .LT. tol, "box of nodes")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetBoundingBox2]], [[obj_SetBoundingBox1]]

INTERFACE
  MODULE FUNCTION obj_GetBoundingBox1(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    TYPE(BoundingBox_) :: ans
    !! bounding box of the nodes of the mesh
  END FUNCTION obj_GetBoundingBox1
END INTERFACE

!----------------------------------------------------------------------------
!                                            GetBoundingBox@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Get the bounding box of node coordinates
!
! See GetBoundingBox.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   nodes(:,:) : REAL(DFP), intent(in) - node coordinates in xiJ format,
!     column i is global node i (or local_nptrs(i))
!   local_nptrs(:) : INTEGER(I4B), optional, intent(in) - map from a global
!     node number to a column of nodes
!
! Returns:
!   ans : TYPE(BoundingBox_) - bounding box of the nodes of the mesh
!
! See also: [[obj_GetBoundingBox1]]

INTERFACE
  MODULE FUNCTION obj_GetBoundingBox2(obj, nodes, local_nptrs) &
    RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    REAL(DFP), INTENT(IN) :: nodes(:, :)
    !! node coordinates in xiJ format, column i is global node i (or
    !! local_nptrs(i))
    INTEGER(I4B), INTENT(IN), OPTIONAL :: local_nptrs(:)
    !! map from a global node number to a column of nodes
    TYPE(BoundingBox_) :: ans
    !! bounding box of the nodes of the mesh
  END FUNCTION obj_GetBoundingBox2
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetConnectivity@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Get the connectivity of an element
!
! Return the vertex connectivity (global node numbers) of an element. With opt
! the connectivity can be the edges (E), faces (F), cell (C), or all of them
! (A, in the order V, E, F, C). Edges are numbered by
! InitiateEdgeConnectivity, faces by Initiate.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!   opt : CHARACTER(*), optional, intent(in) - connectivity: V vertex
!     (default), E edges, F faces, C cell, A all
!
! Returns:
!   ans(:) : INTEGER(I4B), allocatable - connectivity of the element
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!   INTEGER(I4B), ALLOCATABLE :: con(:)
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!
!   con = obj%GetConnectivity(globalElement=13_I4B)
!   CALL OK(SIZE(con) .EQ. 4_I4B, "size of connectivity")
!   CALL OK(ALL(con .EQ. [1, 5, 9, 7]), "global element 13")
!
!   ! local element 2 is global element 14
!   con = obj%GetConnectivity(globalElement=2_I4B, islocal=.TRUE.)
!   CALL OK(ALL(con .EQ. [7, 9, 6, 3]), "local element 2")
!
!   ! opt="V" (vertex) is the default
!   con = obj%GetConnectivity(globalElement=16_I4B, opt="V")
!   CALL OK(ALL(con .EQ. [9, 8, 4, 6]), "opt=V")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetConnectivity1_]], [[obj_GetConnectivity2_]]

INTERFACE
  MODULE FUNCTION obj_GetConnectivity(obj, globalElement, islocal, opt) &
    RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !! connectivity of the element
    CHARACTER(*), INTENT(IN), OPTIONAL :: opt
    !! connectivity: V vertex (default), E edges, F faces, C cell, A all
  END FUNCTION obj_GetConnectivity
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetConnectivity@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-12
! summary: Get the connectivity of an element
!
! GetConnectivity_ is a generic method. With ans and tsize it works like
! GetConnectivity without allocation. With cellCon, faceCon, edgeCon and
! nodeCon it returns all connectivities in separate vectors.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   ans(:) : INTEGER(I4B), intent(inout) - connectivity, size must be at least
!     tsize
!   tsize : INTEGER(I4B), intent(out) - number of entries written in ans
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!   opt : CHARACTER(*), optional, intent(in) - connectivity: V vertex
!     (default), E edges, F faces, C cell, A all
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!   INTEGER(I4B), ALLOCATABLE :: con(:)
!   INTEGER(I4B) :: buf(20), cellCon(2), faceCon(10), edgeCon(10)
!   INTEGER(I4B) :: nodeCon(10), tsize, tcell, tface, tedge, tnode
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!
!   CALL obj%InitiateEdgeConnectivity()
!   con = obj%GetConnectivity(globalElement=13_I4B, opt="E")
!   CALL OK(SIZE(con) .EQ. 4_I4B, "opt=E")
!   con = obj%GetConnectivity(globalElement=13_I4B, opt="F")
!   CALL OK(SIZE(con) .EQ. 4_I4B, "opt=F")
!   con = obj%GetConnectivity(globalElement=13_I4B, opt="C")
!   CALL OK(ALL(con .EQ. [13]), "opt=C")
!   con = obj%GetConnectivity(globalElement=13_I4B, opt="A")
!   CALL OK(SIZE(con) .EQ. 13_I4B, "opt=A")
!   CALL OK(ALL(con(1:4) .EQ. [1, 5, 9, 7]) .AND. con(13) .EQ. 13_I4B, &
!     "opt=A: vertices first, cell last")
!
!   CALL obj%GetConnectivity_(globalElement=13_I4B, ans=buf, &
!     tsize=tsize, opt="V")
!   CALL OK(ALL(buf(1:tsize) .EQ. [1, 5, 9, 7]), "GetConnectivity_")
!
!   CALL obj%GetConnectivity_(cellCon=cellCon, faceCon=faceCon, &
!     edgeCon=edgeCon, nodeCon=nodeCon, &
!     tCellCon=tcell, tFaceCon=tface, &
!     tEdgeCon=tedge, tNodeCon=tnode, &
!     globalElement=13_I4B)
!   CALL OK(tcell .EQ. 1_I4B .AND. tface .EQ. 4_I4B .AND. &
!     tedge .EQ. 4_I4B .AND. tnode .EQ. 4_I4B, "sizes")
!   CALL OK(cellCon(1) .EQ. 13_I4B, "cellCon")
!   CALL OK(ALL(nodeCon(1:4) .EQ. [1, 5, 9, 7]), "nodeCon")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetConnectivity]], [[obj_GetConnectivity2_]]

INTERFACE
  MODULE SUBROUTINE obj_GetConnectivity1_(obj, globalElement, ans, tsize, &
                                          islocal, opt)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! connectivity, size must be at least tsize
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written in ans
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    CHARACTER(*), INTENT(IN), OPTIONAL :: opt
    !! connectivity: V vertex (default), E edges, F faces, C cell, A all
  END SUBROUTINE obj_GetConnectivity1_
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetConnectivity@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-14
! summary: Get all connectivities of an element
!
! See GetConnectivity_.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   cellCon(:) : INTEGER(I4B), intent(inout) - cell number of the element
!   faceCon(:) : INTEGER(I4B), intent(inout) - global face numbers of the
!     element
!   edgeCon(:) : INTEGER(I4B), intent(inout) - global edge numbers of the
!     element
!   nodeCon(:) : INTEGER(I4B), intent(inout) - global node numbers of the
!     element
!   tCellCon : INTEGER(I4B), intent(out) - number of entries written in
!     cellCon
!   tFaceCon : INTEGER(I4B), intent(out) - number of entries written in
!     faceCon
!   tEdgeCon : INTEGER(I4B), intent(out) - number of entries written in
!     edgeCon
!   tnodeCon : INTEGER(I4B), intent(out) - number of entries written in
!     nodeCon
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! See also: [[obj_GetConnectivity1_]]

INTERFACE
  MODULE SUBROUTINE obj_GetConnectivity2_(obj, cellCon, faceCon, edgeCon, &
                                          nodeCon, tCellCon, tFaceCon, &
                                          tEdgeCon, tnodeCon, globalElement, &
                                          islocal)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(INOUT) :: cellCon(:)
    !! cell number of the element
    INTEGER(I4B), INTENT(INOUT) :: faceCon(:)
    !! global face numbers of the element
    INTEGER(I4B), INTENT(INOUT) :: edgeCon(:)
    !! global edge numbers of the element
    INTEGER(I4B), INTENT(INOUT) :: nodeCon(:)
    !! global node numbers of the element
    INTEGER(I4B), INTENT(OUT) :: tCellCon
    !! number of entries written in cellCon
    INTEGER(I4B), INTENT(OUT) :: tFaceCon
    !! number of entries written in faceCon
    INTEGER(I4B), INTENT(OUT) :: tEdgeCon
    !! number of entries written in edgeCon
    INTEGER(I4B), INTENT(OUT) :: tnodeCon
    !! number of entries written in nodeCon
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
  END SUBROUTINE obj_GetConnectivity2_
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetOrientation@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-14
! summary: Get the orientation flags of an element
!
! Return the cell, face and edge orientation of an element.
!
! The orientation of an edge is 1 when its first node has the lower global
! node number, else -1. The edge orientation needs the edge connectivity
! (InitiateEdgeConnectivity).
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   cellOrient(:) : INTEGER(I4B), intent(inout) - cell orientation
!   faceOrient(:,:) : INTEGER(I4B), intent(inout) - face orientation, one
!     column per face
!   edgeOrient(:) : INTEGER(I4B), intent(inout) - edge orientation
!   tCellOrient : INTEGER(I4B), intent(out) - number of entries written in
!     cellOrient
!   tFaceOrient(2) : INTEGER(I4B), intent(out) - number of rows and columns
!     written in faceOrient
!   tEdgeOrient : INTEGER(I4B), intent(out) - number of entries written in
!     edgeOrient
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/small_hexa8_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!   INTEGER(I4B) :: buf(20), face(5, 10), cell(5), tface(2)
!   INTEGER(I4B) :: tsize, nrow, ncol, tcell
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=3)
!
!   CALL obj%InitiateEdgeConnectivity()
!   CALL obj%GetEdgeOrientation(ans=buf, tsize=tsize, &
!     globalElement=36_I4B)
!   CALL OK(tsize .EQ. 12_I4B, "number of edges")
!   CALL OK(ALL(buf(1:12) .EQ. [-1, 1, 1, 1, 1, 1, 1, 1, -1, 1, 1, 1]), &
!     "GetEdgeOrientation")
!
!   CALL obj%GetCellOrientation(ans=buf, tsize=tsize, &
!     globalElement=36_I4B)
!   CALL OK(ALL(buf(1:tsize) .EQ. [1, 1, 1]), "GetCellOrientation")
!
!   CALL obj%GetFaceOrientation(ans=face, nrow=nrow, ncol=ncol, &
!     globalElement=36_I4B)
!   CALL OK(nrow .EQ. 3_I4B .AND. ncol .EQ. 6_I4B, "GetFaceOrientation")
!
!   CALL obj%GetOrientation(cellOrient=cell, faceOrient=face, &
!     edgeOrient=buf, tCellOrient=tcell, &
!     tFaceOrient=tface, tEdgeOrient=tsize, &
!     globalElement=36_I4B)
!   CALL OK(tcell .EQ. 3_I4B .AND. ALL(tface .EQ. [3, 6]) .AND. &
!     tsize .EQ. 12_I4B, "GetOrientation")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetCellOrientation]], [[obj_GetFaceOrientation]],
!   [[obj_GetEdgeOrientation]]

INTERFACE
  MODULE SUBROUTINE obj_GetOrientation(obj, cellOrient, faceOrient, &
                                       edgeOrient, tCellOrient, tFaceOrient, &
                                       tEdgeOrient, globalElement, islocal)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(INOUT) :: cellOrient(:)
    !! cell orientation
    INTEGER(I4B), INTENT(INOUT) :: faceOrient(:, :)
    !! face orientation, one column per face
    INTEGER(I4B), INTENT(INOUT) :: edgeOrient(:)
    !! edge orientation
    INTEGER(I4B), INTENT(OUT) :: tCellOrient
    !! number of entries written in cellOrient
    INTEGER(I4B), INTENT(OUT) :: tFaceOrient(2)
    !! number of rows and columns written in faceOrient
    INTEGER(I4B), INTENT(OUT) :: tEdgeOrient
    !! number of entries written in edgeOrient
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
  END SUBROUTINE obj_GetOrientation
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetOrientation@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-02
! summary: Get the cell orientation of an element
!
! See GetOrientation.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   ans(:) : INTEGER(I4B), intent(inout) - cell orientation
!   tsize : INTEGER(I4B), intent(out) - number of entries written in ans
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! See also: [[obj_GetOrientation]]

INTERFACE
  MODULE SUBROUTINE obj_GetCellOrientation(obj, ans, tsize, &
                                           globalElement, islocal)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! cell orientation
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written in ans
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
  END SUBROUTINE obj_GetCellOrientation
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetOrientation@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-14
! summary: Get the face orientation of an element
!
! See GetOrientation.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   ans(:,:) : INTEGER(I4B), intent(inout) - face orientation, one column per
!     face
!   nrow : INTEGER(I4B), intent(out) - number of rows written
!   ncol : INTEGER(I4B), intent(out) - number of columns written
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! See also: [[obj_GetOrientation]]

INTERFACE
  MODULE SUBROUTINE obj_GetFaceOrientation(obj, ans, nrow, ncol, &
                                           globalElement, islocal)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(INOUT) :: ans(:, :)
    !! face orientation, one column per face
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! nrow: number of rows written
    !! ncol: number of columns written
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
  END SUBROUTINE obj_GetFaceOrientation
END INTERFACE

!----------------------------------------------------------------------------
!                                            GetEdgeOrientation@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-02
! summary: Get the edge orientation of an element
!
! The orientation of an edge is 1 when its first node has the lower global
! node number, else -1. The edge orientation needs the edge connectivity
! (InitiateEdgeConnectivity).
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   ans(:) : INTEGER(I4B), intent(inout) - edge orientation
!   tsize : INTEGER(I4B), intent(out) - number of entries written in ans
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! See also: [[obj_GetOrientation]]

INTERFACE
  MODULE SUBROUTINE obj_GetEdgeOrientation(obj, ans, tsize, globalElement, &
                                           islocal)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! edge orientation
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written in ans
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
  END SUBROUTINE obj_GetEdgeOrientation
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetNodeConnectivity@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Get the vertex connectivity of all elements
!
! Column i of VALUE is the vertex connectivity of local element i.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   VALUE(:,:) : INTEGER(I4B), intent(inout) - connectivity, SIZE(VALUE) =
!     [GetMaxNNE(), number of elements]
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!   INTEGER(I4B) :: con(4, 4)
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!
!   CALL obj%GetNodeConnectivity(VALUE=con)
!   CALL OK(ALL(con(:, 1) .EQ. [1, 5, 9, 7]), "element 13")
!   CALL OK(ALL(con(:, 4) .EQ. [9, 8, 4, 6]), "element 16")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetConnectivity]]

INTERFACE
  MODULE SUBROUTINE obj_GetNodeConnectivity(obj, VALUE)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(INOUT) :: VALUE(:, :)
    !! connectivity, SIZE(VALUE) = [GetMaxNNE(), number of elements]
  END SUBROUTINE obj_GetNodeConnectivity
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetLocalNodeNumber@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Get the local numbers of global nodes
!
! Local node numbers follow the order of the global node numbers. Local
! element numbers follow the order of the elements in the mesh file.
!
! GetLocalNodeNumber, GetGlobalNodeNumber, GetLocalElemNumber and
! GetGlobalElemNumber convert one number or a list of numbers.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   globalNode(:) : INTEGER(I4B), intent(in) - global node numbers (local if
!     islocal is true)
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans(SIZE(globalNode)) : INTEGER(I4B) - local node numbers
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!   INTEGER(I4B) :: buf(2)
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!
!   CALL OK(obj%GetLocalNodeNumber(7_I4B) .EQ. 7_I4B, &
!     "GetLocalNodeNumber")
!   CALL OK(ALL(obj%GetLocalNodeNumber([2_I4B, 9_I4B]) .EQ. [2, 9]), &
!     "GetLocalNodeNumber(:)")
!   CALL obj%GetLocalNodeNumber_(globalNode=[5_I4B, 6_I4B], ans=buf)
!   CALL OK(ALL(buf(1:2) .EQ. [5, 6]), "GetLocalNodeNumber_")
!   CALL OK(obj%GetGlobalNodeNumber(7_I4B) .EQ. 7_I4B, &
!     "GetGlobalNodeNumber")
!   CALL OK(ALL(obj%GetGlobalNodeNumber([2_I4B, 9_I4B]) .EQ. [2, 9]), &
!     "GetGlobalNodeNumber(:)")
!
!   CALL OK(obj%GetLocalElemNumber(15_I4B) .EQ. 3_I4B, &
!     "GetLocalElemNumber")
!   CALL OK(ALL(obj%GetLocalElemNumber([13_I4B, 16_I4B]) .EQ. [1, 4]), &
!     "GetLocalElemNumber(:)")
!   CALL OK(obj%GetGlobalElemNumber(3_I4B) .EQ. 15_I4B, &
!     "GetGlobalElemNumber")
!   CALL OK(ALL(obj%GetGlobalElemNumber([1_I4B, 4_I4B]) .EQ. [13, 16]), &
!     "GetGlobalElemNumber(:)")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetLocalNodeNumber2]], [[obj_GetLocalNodeNumber1_]],
!   [[obj_GetGlobalNodeNumber1]], [[obj_GetLocalElemNumber1]],
!   [[obj_GetGlobalElemNumber1]]

INTERFACE
  MODULE FUNCTION obj_GetLocalNodeNumber1(obj, globalNode, islocal) &
    RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global node numbers (local if islocal is true)
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    INTEGER(I4B) :: ans(SIZE(globalNode))
    !! local node numbers
  END FUNCTION obj_GetLocalNodeNumber1
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetLocalNoddeNumber_@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the local numbers of global nodes without allocation
!
! See GetLocalNodeNumber.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   globalNode(:) : INTEGER(I4B), intent(in) - global node numbers (local if
!     islocal is true)
!   ans(:) : INTEGER(I4B), intent(inout) - local node numbers, SIZE(ans) >=
!     SIZE(globalNode)
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! See also: [[obj_GetLocalNodeNumber1]]

INTERFACE
  MODULE SUBROUTINE obj_GetLocalNodeNumber1_(obj, globalNode, ans, islocal)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global node numbers (local if islocal is true)
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! local node numbers, SIZE(ans) >= SIZE(globalNode)
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
  END SUBROUTINE obj_GetLocalNodeNumber1_
END INTERFACE

!----------------------------------------------------------------------------
!                                         GetLocalNodeNumber@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Get the local number of a global node
!
! See GetLocalNodeNumber.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   globalNode : INTEGER(I4B), intent(in) - global node number, or local node
!     number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans : INTEGER(I4B) - local node number
!
! See also: [[obj_GetLocalNodeNumber1]]

INTERFACE
  MODULE FUNCTION obj_GetLocalNodeNumber2(obj, globalNode, islocal) &
    RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global node number, or local node number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    INTEGER(I4B) :: ans
    !! local node number
  END FUNCTION obj_GetLocalNodeNumber2
END INTERFACE

!----------------------------------------------------------------------------
!                                          GetGlobalNptrs@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Get the global numbers of local nodes
!
! See GetLocalNodeNumber.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   localNode(:) : INTEGER(I4B), intent(in) - local node numbers
!
! Returns:
!   ans(SIZE(localNode)) : INTEGER(I4B) - global node numbers
!
! See also: [[obj_GetLocalNodeNumber1]]

INTERFACE
  MODULE FUNCTION obj_GetGlobalNodeNumber1(obj, localNode) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: localNode(:)
    !! local node numbers
    INTEGER(I4B) :: ans(SIZE(localNode))
    !! global node numbers
  END FUNCTION obj_GetGlobalNodeNumber1
END INTERFACE

!----------------------------------------------------------------------------
!                                          GetGlobalNptrs@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Get the global number of a local node
!
! See GetLocalNodeNumber.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   localNode : INTEGER(I4B), intent(in) - local node number
!
! Returns:
!   ans : INTEGER(I4B) - global node number
!
! See also: [[obj_GetLocalNodeNumber1]]

INTERFACE
  MODULE FUNCTION obj_GetGlobalNodeNumber2(obj, localNode) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: localNode
    !! local node number
    INTEGER(I4B) :: ans
    !! global node number
  END FUNCTION obj_GetGlobalNodeNumber2
END INTERFACE

!----------------------------------------------------------------------------
!                                       GetGlobalElemNumber@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Get the global numbers of local elements
!
! See GetLocalNodeNumber.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   LocalElement(:) : INTEGER(I4B), intent(in) - local element numbers
!
! Returns:
!   ans(SIZE(LocalElement)) : INTEGER(I4B) - global element numbers
!
! See also: [[obj_GetLocalNodeNumber1]]

INTERFACE
  MODULE FUNCTION obj_GetGlobalElemNumber1(obj, LocalElement) &
    RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: LocalElement(:)
    !! local element numbers
    INTEGER(I4B) :: ans(SIZE(LocalElement))
    !! global element numbers
  END FUNCTION obj_GetGlobalElemNumber1
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetGlobalElemNumber@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Get the global number of a local element
!
! See GetLocalNodeNumber.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   LocalElement : INTEGER(I4B), intent(in) - local element number
!
! Returns:
!   ans : INTEGER(I4B) - global element number
!
! See also: [[obj_GetLocalNodeNumber1]]

INTERFACE
  MODULE FUNCTION obj_GetGlobalElemNumber2(obj, LocalElement) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: LocalElement
    !! local element number
    INTEGER(I4B) :: ans
    !! global element number
  END FUNCTION obj_GetGlobalElemNumber2
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetLocalElemNumber@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Get the local numbers of global elements
!
! See GetLocalNodeNumber.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   globalElement(:) : INTEGER(I4B), intent(in) - global element numbers
!     (local if islocal is true)
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans(SIZE(globalElement)) : INTEGER(I4B) - local element numbers
!
! See also: [[obj_GetLocalNodeNumber1]]

INTERFACE
  MODULE FUNCTION obj_GetLocalElemNumber1(obj, globalElement, islocal) &
    RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: globalElement(:)
    !! global element numbers (local if islocal is true)
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    INTEGER(I4B) :: ans(SIZE(globalElement))
    !! local element numbers
  END FUNCTION obj_GetLocalElemNumber1
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetLocalElemNumber@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Get the local number of a global element
!
! See GetLocalNodeNumber.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans : INTEGER(I4B) - local element number
!
! See also: [[obj_GetLocalNodeNumber1]]

INTERFACE
  MODULE FUNCTION obj_GetLocalElemNumber2(obj, globalElement, islocal) &
    RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    INTEGER(I4B) :: ans
    !! local element number
  END FUNCTION obj_GetLocalElemNumber2
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetNodeToElements@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Get the elements around a node
!
! Return the global element numbers around a node, in local element order. The
! node to elements data is initiated if needed. For a list of nodes the result
! is sorted and has no duplicates. If the node is not present, an error is
! raised in debug mode.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!   globalNode : INTEGER(I4B), intent(in) - global node number, or local node
!     number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans(:) : INTEGER(I4B), allocatable - global element numbers around the
!     node
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!   INTEGER(I4B), ALLOCATABLE :: elems(:)
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!
!   ! corner node 1 belongs to element 13 only
!   elems = obj%GetNodeToElements(globalNode=1_I4B)
!   CALL OK(SIZE(elems) .EQ. 1_I4B, "node 1: size")
!   CALL OK(elems(1) .EQ. 13_I4B, "node 1: element")
!   CALL OK(obj%IsNodeToElements(), "IsNodeToElements")
!
!   ! edge node 5 belongs to elements 13 and 15
!   elems = obj%GetNodeToElements(globalNode=5_I4B)
!   CALL OK(ALL(elems .EQ. [13_I4B, 15_I4B]), "node 5")
!
!   ! center node 9 belongs to all elements
!   elems = obj%GetNodeToElements(globalNode=9_I4B)
!   CALL OK(ALL(elems .EQ. [13_I4B, 14_I4B, 15_I4B, 16_I4B]), "node 9")
!
!   ! several nodes: union of elements, sorted
!   elems = obj%GetNodeToElements(globalNode=[4_I4B, 1_I4B])
!   CALL OK(ALL(elems .EQ. [13_I4B, 16_I4B]), "nodes 4 and 1")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetNodeToElements2]], [[obj_GetNodeToElements1_]],
!   [[obj_InitiateNodeToElements]]

INTERFACE
  MODULE FUNCTION obj_GetNodeToElements1(obj, globalNode, islocal) &
    RESULT(ans)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global node number, or local node number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !! global element numbers around the node
  END FUNCTION obj_GetNodeToElements1
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetNodeToElements@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Get the elements around a list of nodes
!
! See GetNodeToElements.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!   globalNode(:) : INTEGER(I4B), intent(in) - global node numbers (local if
!     islocal is true)
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans(:) : INTEGER(I4B), allocatable - global element numbers, sorted and
!     without duplicates
!
! See also: [[obj_GetNodeToElements1]]

INTERFACE
  MODULE FUNCTION obj_GetNodeToElements2(obj, globalNode, islocal) &
    RESULT(ans)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global node numbers (local if islocal is true)
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !! global element numbers, sorted and without duplicates
  END FUNCTION obj_GetNodeToElements2
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetNodeToElements@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-28
! summary: Get the elements around a node without allocation
!
! GetNodeToElements_ works like GetNodeToElements without allocation.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!   ans(:) : INTEGER(I4B), intent(inout) - global element numbers, size must
!     be at least tsize
!   tsize : INTEGER(I4B), intent(out) - number of entries written in ans
!   globalNode : INTEGER(I4B), intent(in) - global node number, or local node
!     number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!   INTEGER(I4B) :: elems(10), tsize
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!
!   CALL obj%GetNodeToElements_(ans=elems, tsize=tsize, globalNode=7_I4B)
!   CALL OK(tsize .EQ. 2_I4B, "node 7: tsize")
!   CALL OK(ALL(elems(1:tsize) .EQ. [13_I4B, 14_I4B]), "node 7")
!
!   CALL obj%GetNodeToElements_(ans=elems, tsize=tsize, &
!     globalNode=[7_I4B, 8_I4B])
!   CALL OK(ALL(elems(1:tsize) .EQ. [13_I4B, 14_I4B, 15_I4B, 16_I4B]), &
!     "nodes 7 and 8")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetNodeToElements1]]

INTERFACE
  MODULE SUBROUTINE obj_GetNodeToElements1_(obj, ans, tsize, &
                                            globalNode, islocal)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! global element numbers, size must be at least tsize
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written in ans
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global node number, or local node number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
  END SUBROUTINE obj_GetNodeToElements1_
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetNodeToElements@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-28
! summary: Get the elements around nodes without allocation
!
! See GetNodeToElements_.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!   ans(:) : INTEGER(I4B), intent(inout) - global element numbers, size must
!     be at least tsize
!   tsize : INTEGER(I4B), intent(out) - number of entries written in ans
!   globalNode(:) : INTEGER(I4B), intent(in) - global node numbers (local if
!     islocal is true)
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! See also: [[obj_GetNodeToElements1_]]

INTERFACE
  MODULE SUBROUTINE obj_GetNodeToElements2_(obj, ans, tsize, &
                                            globalNode, islocal)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! global element numbers, size must be at least tsize
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written in ans
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global node numbers (local if islocal is true)
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
  END SUBROUTINE obj_GetNodeToElements2_
END INTERFACE

!----------------------------------------------------------------------------
!                                                  GetNodeToNodes@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! update:
!   - 2026-10-23: The node to nodes data is initiated if needed
! summary: Get the nodes connected to a node
!
! Return the global node numbers connected to a node through the elements
! around it. With includeSelf the node itself is the first entry. For a list
! of nodes the result is sorted and has no duplicates. The node to nodes data
! is initiated if needed (see [[obj_InitiateNodetoNodes]]). If the extra node
! to nodes data is initiated, the extra nodes are included.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!   globalNode : INTEGER(I4B), intent(in) - global node number, or local node
!     number if islocal is true
!   includeSelf : LOGICAL(LGT), intent(in) - if true, globalNode itself is the
!     first entry
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans(:) : INTEGER(I4B), allocatable - global node numbers connected to the
!     node
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE SortUtility, ONLY: Sort
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!   INTEGER(I4B), ALLOCATABLE :: nodes(:)
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!
!   CALL obj%InitiateNodeToNodes()
!
!   nodes = obj%GetNodeToNodes(globalNode=1_I4B, includeSelf=.FALSE.)
!   CALL OK(SIZE(nodes) .EQ. 3_I4B, "node 1: size")
!   CALL OK(ALL([5, 7, 9] .EQ. Sort(nodes)), "node 1")
!
!   nodes = obj%GetNodeToNodes(globalNode=1_I4B, includeSelf=.TRUE.)
!   CALL OK(nodes(1) .EQ. 1_I4B, "includeSelf: first entry")
!   CALL OK(SIZE(nodes) .EQ. 4_I4B, "includeSelf: size")
!
!   nodes = obj%GetNodeToNodes(globalNode=[1_I4B, 2_I4B], &
!     includeSelf=.FALSE.)
!   CALL OK(ALL(nodes .EQ. [5, 7, 8, 9]), "nodes 1 and 2")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetNodeToNodes2]], [[obj_GetNodeToNodes1_]],
!   [[obj_InitiateNodetoNodes]]

INTERFACE
  MODULE FUNCTION obj_GetNodeToNodes1(obj, globalNode, includeSelf, &
                                      islocal) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global node number, or local node number if islocal is true
    LOGICAL(LGT), INTENT(IN) :: includeSelf
    !! if true, globalNode itself is the first entry
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !! global node numbers connected to the node
  END FUNCTION obj_GetNodeToNodes1
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetNodeToNodes@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! update:
!   - 2026-10-23: The node to nodes data is initiated if needed
! summary: Get the nodes connected to a list of nodes
!
! See GetNodeToNodes.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!   globalNode(:) : INTEGER(I4B), intent(in) - global node numbers (local if
!     islocal is true)
!   includeSelf : LOGICAL(LGT), intent(in) - if true, the nodes of globalNode
!     are included
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans(:) : INTEGER(I4B), allocatable - global node numbers, sorted and
!     without duplicates
!
! See also: [[obj_GetNodeToNodes1]]

INTERFACE
  MODULE FUNCTION obj_GetNodeToNodes2(obj, globalNode, includeSelf, &
                                      islocal) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global node numbers (local if islocal is true)
    LOGICAL(LGT), INTENT(IN) :: includeSelf
    !! if true, the nodes of globalNode are included
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !! global node numbers, sorted and without duplicates
  END FUNCTION obj_GetNodeToNodes2
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetNodeToNodes_@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-13
! update:
!   - 2026-10-23: The node to nodes data is initiated if needed
! summary: Get the nodes connected to a node without allocation
!
! GetNodeToNodes_ works like GetNodeToNodes without allocation. The node to
! nodes data is initiated if needed.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!   globalNode : INTEGER(I4B), intent(in) - global node number, or local node
!     number if islocal is true
!   includeSelf : LOGICAL(LGT), intent(in) - if true, globalNode itself is the
!     first entry
!   ans(:) : INTEGER(I4B), intent(inout) - global node numbers, size must be
!     at least tsize
!   tsize : INTEGER(I4B), intent(out) - number of entries written in ans
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!   INTEGER(I4B) :: nodes(20), tsize
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!
!   CALL obj%InitiateNodeToNodes()
!
!   CALL obj%GetNodeToNodes_(globalNode=2_I4B, includeSelf=.TRUE., &
!     ans=nodes, tsize=tsize)
!   CALL OK(tsize .EQ. 4_I4B, "node 2: tsize")
!   CALL OK(nodes(1) .EQ. 2_I4B, "node 2: first entry is node 2")
!
!   CALL obj%GetNodeToNodes_(globalNode=[2_I4B, 3_I4B], &
!     includeSelf=.FALSE., ans=nodes, tsize=tsize)
!   CALL OK(ALL(nodes(1:tsize) .EQ. [5, 6, 7, 8, 9]), "nodes 2 and 3")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetNodeToNodes1]]

INTERFACE
  MODULE SUBROUTINE obj_GetNodeToNodes1_(obj, globalNode, includeSelf, &
                                         ans, tsize, islocal)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global node number, or local node number if islocal is true
    LOGICAL(LGT), INTENT(IN) :: includeSelf
    !! if true, globalNode itself is the first entry
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! global node numbers, size must be at least tsize
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written in ans
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
  END SUBROUTINE obj_GetNodeToNodes1_
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetNodeToNodes@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! update:
!   - 2026-10-23: The node to nodes data is initiated if needed
! summary: Get the nodes connected to nodes without allocation
!
! See GetNodeToNodes_.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!   globalNode(:) : INTEGER(I4B), intent(in) - global node numbers (local if
!     islocal is true)
!   includeSelf : LOGICAL(LGT), intent(in) - if true, the nodes of globalNode
!     are included
!   ans(:) : INTEGER(I4B), intent(inout) - global node numbers, size must be
!     at least tsize
!   tsize : INTEGER(I4B), intent(out) - number of entries written in ans
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! See also: [[obj_GetNodeToNodes1_]]

INTERFACE
  MODULE SUBROUTINE obj_GetNodeToNodes2_(obj, globalNode, includeSelf, &
                                         ans, tsize, islocal)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global node numbers (local if islocal is true)
    LOGICAL(LGT), INTENT(IN) :: includeSelf
    !! if true, the nodes of globalNode are included
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! global node numbers, size must be at least tsize
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written in ans
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
  END SUBROUTINE obj_GetNodeToNodes2_
END INTERFACE

!----------------------------------------------------------------------------
!                                      GetElementToElements@MeshDataMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Get the elements around an element
!
! Each row of the result is a neighbour of the element: column 1 is the global
! element number of the neighbour, column 2 the local facet number in the
! element, and column 3 the local facet number in the neighbour.
!
! If onlyElements is true, only the first column is returned.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   onlyElements : LOGICAL(LGT), optional, intent(in) - if true, only the
!     neighbour element numbers are returned; default is false
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans(:,:) : INTEGER(I4B), allocatable - neighbours of the element,
!     SHAPE(ans) = [neighbours, 3] (or [neighbours, 1])
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!   INTEGER(I4B), ALLOCATABLE :: e2e(:, :)
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!
!   ! element 13 shares its facet 2 with facet 4 of element 15,
!   ! and its facet 3 with facet 1 of element 14
!   e2e = obj%GetElementToElements(globalElement=13_I4B)
!   CALL OK(ALL(SHAPE(e2e) .EQ. [2, 3]), "shape")
!   CALL OK(ALL(e2e(1, :) .EQ. [15, 2, 4]), "neighbour 15")
!   CALL OK(ALL(e2e(2, :) .EQ. [14, 3, 1]), "neighbour 14")
!
!   e2e = obj%GetElementToElements(globalElement=13_I4B, &
!     onlyElements=.TRUE.)
!   CALL OK(ALL(SHAPE(e2e) .EQ. [2, 1]), "onlyElements: shape")
!   CALL OK(ALL(e2e(:, 1) .EQ. [15, 14]), "onlyElements")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetElementToElements1_]], [[obj_GetElementToElements2_]],
!   [[obj_InitiateElementToElements]]

INTERFACE
  MODULE FUNCTION obj_GetElementToElements(obj, globalElement, &
                                           onlyElements, islocal) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: onlyElements
    !! if true, only the neighbour element numbers are returned; default is
    !! false
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
    !! neighbours of the element, SHAPE(ans) = [neighbours, 3] (or
    !! [neighbours, 1])
  END FUNCTION obj_GetElementToElements
END INTERFACE

!----------------------------------------------------------------------------
!                                      GetElementToElements@MeshDataMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-22
! summary: Get the neighbour elements without allocation
!
! GetElementToElements_ is a generic method. With ans(:) it returns the global
! element numbers of the neighbours, with ans(:, :) the full information (see
! GetElementToElements).
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   ans(:) : INTEGER(I4B), intent(inout) - neighbour element numbers, size
!     must be at least tsize
!   tsize : INTEGER(I4B), intent(out) - number of entries written in ans
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!   INTEGER(I4B) :: elems(10), e2e(10, 3), tsize, nrow, ncol
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!
!   CALL obj%GetElementToElements_(ans=elems, tsize=tsize, &
!     globalElement=16_I4B)
!   CALL OK(ALL(elems(1:tsize) .EQ. [15, 14]), "neighbours of 16")
!
!   CALL obj%GetElementToElements_(ans=e2e, nrow=nrow, ncol=ncol, &
!     globalElement=4_I4B, islocal=.TRUE.)
!   CALL OK(nrow .EQ. 2_I4B .AND. ncol .EQ. 3_I4B, "nrow and ncol")
!   CALL OK(ALL(e2e(1, 1:3) .EQ. [15, 1, 3]), "first neighbour")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetElementToElements]], [[obj_GetElementToElements2_]]

INTERFACE
  MODULE SUBROUTINE obj_GetElementToElements1_(obj, ans, tsize, &
                                               globalElement, islocal)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! neighbour element numbers, size must be at least tsize
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written in ans
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
  END SUBROUTINE obj_GetElementToElements1_
END INTERFACE

!----------------------------------------------------------------------------
!                                      GetElementToElements@MeshDataMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-22
! summary: Get the elements around an element without allocation
!
! Each row of the result is a neighbour of the element: column 1 is the global
! element number of the neighbour, column 2 the local facet number in the
! element, and column 3 the local facet number in the neighbour.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   ans(:,:) : INTEGER(I4B), intent(inout) - neighbours, rows are neighbours,
!     SIZE(ans, 2) >= 3
!   nrow : INTEGER(I4B), intent(out) - number of rows written
!   ncol : INTEGER(I4B), intent(out) - number of columns written
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! See also: [[obj_GetElementToElements1_]]

INTERFACE
  MODULE SUBROUTINE obj_GetElementToElements2_(obj, ans, nrow, ncol, &
                                               globalElement, islocal)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(INOUT) :: ans(:, :)
    !! neighbours, rows are neighbours, SIZE(ans, 2) >= 3
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! number of rows written
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! number of columns written
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
  END SUBROUTINE obj_GetElementToElements2_
END INTERFACE

!----------------------------------------------------------------------------
!                                          GetBoundaryElementData@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Get the boundary facets of an element
!
! Return the local facet numbers of an element which are on the boundary of
! the mesh. The array has size zero for an internal element.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans(:) : INTEGER(I4B), allocatable - local facet numbers on the boundary
!
! See also: [[obj_InitiateBoundaryData]]

INTERFACE
  MODULE FUNCTION obj_GetBoundaryElementData(obj, globalElement, islocal) &
    RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !! local facet numbers on the boundary
  END FUNCTION obj_GetBoundaryElementData
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetOrder@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! update:
!   - 2026-10-23: Implemented; the result is stored in the mesh
! summary: Get the order of the mesh
!
! Returns the largest order of the elements in the mesh.
!
! The first call computes the order from the elements (see
! [[obj_GetOrder2]]), stores it in obj%maxElemOrder and sets
! obj%isMaxElemOrderSet to true. The later calls return the stored value.
! Deallocate resets both components. A mesh with no elements has order 0.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!
! Returns:
!   ans : INTEGER(I4B) - largest order of the elements
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, dim=2)
!
!   CALL OK(obj%GetOrder() .EQ. 1_I4B, "GetOrder (computed)")
!   CALL OK(obj%GetOrder() .EQ. 1_I4B, "GetOrder (stored)")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetOrder2]], [[obj_GetOrder_]]

INTERFACE
  MODULE FUNCTION obj_GetOrder1(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
    INTEGER(I4B) :: ans
    !! largest order of the elements
  END FUNCTION obj_GetOrder1
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetOrder@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-29
! summary: Get the order of an element
!
! GetOrder_ returns the order of all elements.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans : INTEGER(I4B) - order of the element
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!   INTEGER(I4B) :: order(10), tsize
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!
!   CALL OK(obj%GetOrder(globalElement=13_I4B) .EQ. 1_I4B, "GetOrder")
!   CALL obj%GetOrder_(order=order, tsize=tsize)
!   CALL OK(tsize .EQ. 4_I4B .AND. ALL(order(1:4) .EQ. 1), "GetOrder_")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetOrder_]]

INTERFACE
  MODULE FUNCTION obj_GetOrder2(obj, globalElement, islocal) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    INTEGER(I4B) :: ans
    !! order of the element
  END FUNCTION obj_GetOrder2
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetOrder@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-24
! summary: Get the order of all elements
!
! Order of element i is written in order(i).
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   order(:) : INTEGER(I4B), intent(inout) - order of the elements, size must
!     be at least the number of elements
!   tsize : INTEGER(I4B), intent(out) - number of entries written in order
!
! See also: [[obj_GetOrder2]]

INTERFACE
  MODULE SUBROUTINE obj_GetOrder_(obj, order, tsize)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(INOUT) :: order(:)
    !! order of the elements, size must be at least the number of elements
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written in order
  END SUBROUTINE obj_GetOrder_
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetNSD@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Get the number of spatial dimensions
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!
! Returns:
!   ans : INTEGER(I4B) - number of spatial dimensions of the mesh
!
! See also: [[obj_Initiate]]

INTERFACE
  MODULE FUNCTION obj_GetNSD(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B) :: ans
    !! number of spatial dimensions of the mesh
  END FUNCTION obj_GetNSD
END INTERFACE

!----------------------------------------------------------------------------
!                                                  GetXidimension@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Get the dimension of the elements
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!
! Returns:
!   ans : INTEGER(I4B) - xidimension of the elements: 0 point, 1 curve, 2
!     surface, 3 volume
!
! See also: [[obj_Initiate]]

INTERFACE
  MODULE FUNCTION obj_GetXidimension(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B) :: ans
    !! xidimension of the elements: 0 point, 1 curve, 2 surface, 3 volume
  END FUNCTION obj_GetXidimension
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetMaterial@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Get the material id of a medium of an element
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   medium : INTEGER(I4B), intent(in) - medium number
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans : INTEGER(I4B) - material id of the medium
!
! See also: [[obj_SetMaterial1]]

INTERFACE
  MODULE FUNCTION obj_GetMaterial1(obj, medium, globalElement, islocal) &
    RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: medium
    !! medium number
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    INTEGER(I4B) :: ans
    !! material id of the medium
  END FUNCTION obj_GetMaterial1
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetMaterial@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Get the number of media of an element
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans : INTEGER(I4B) - number of media of the element (zero if none)
!
! See also: [[obj_SetMaterial1]]

INTERFACE
  MODULE FUNCTION obj_GetTotalMaterial1(obj, globalElement, islocal) &
    RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    INTEGER(I4B) :: ans
    !! number of media of the element (zero if none)
  END FUNCTION obj_GetTotalMaterial1
END INTERFACE

!----------------------------------------------------------------------------
!                                           GetTotalFacetElements@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Get the number of facet elements
!
! The facet elements are internal facets and boundary facets. They are made by
! InitiateFacetElements.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!
! Returns:
!   ans : INTEGER(I4B) - number of facet elements
!
! See also: [[obj_InitiateFacetElements]]

INTERFACE
  MODULE FUNCTION obj_GetTotalFacetElements(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B) :: ans
    !! number of facet elements
  END FUNCTION obj_GetTotalFacetElements
END INTERFACE

!----------------------------------------------------------------------------
!                                   GetTotalInternalFacetElements@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Get the number of internal facet elements
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!
! Returns:
!   ans : INTEGER(I4B) - number of internal facet elements
!
! See also: [[obj_InitiateFacetElements]]

INTERFACE
  MODULE FUNCTION obj_GetTotalInternalFacetElements(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B) :: ans
    !! number of internal facet elements
  END FUNCTION obj_GetTotalInternalFacetElements
END INTERFACE

!----------------------------------------------------------------------------
!                                   GetTotalBoundaryFacetElements@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the number of boundary facet elements
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!
! Returns:
!   ans : INTEGER(I4B) - number of boundary and domain boundary facet elements
!
! See also: [[obj_InitiateFacetElements]]

INTERFACE
  MODULE FUNCTION obj_GetTotalBoundaryFacetElements(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B) :: ans
    !! number of boundary and domain boundary facet elements
  END FUNCTION obj_GetTotalBoundaryFacetElements
END INTERFACE

!----------------------------------------------------------------------------
!                                            GetMasterCellNumber@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Get the master cell of a facet element
!
! A facet element has a master cell and, if it is an internal facet, a slave
! cell. Cell numbers are global element numbers; the local facet ID is the
! local facet number in the cell. The facet elements must be initiated
! (InitiateFacetElements). elementType is not used.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   facetElement : INTEGER(I4B), intent(in) - facet element number
!   elementType : INTEGER(I4B), intent(in) - facet element type; it is not
!     used
!
! Returns:
!   ans : INTEGER(I4B) - global element number of the master cell
!
! See also: [[obj_GetCellNumber]]

INTERFACE
  MODULE FUNCTION obj_GetMasterCellNumber(obj, facetElement, elementType) &
    RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: facetElement
    !! facet element number
    INTEGER(I4B), INTENT(IN) :: elementType
    !! facet element type; it is not used
    INTEGER(I4B) :: ans
    !! global element number of the master cell
  END FUNCTION obj_GetMasterCellNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                            GetSlaveCellNumber@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Get the slave cell of a facet element
!
! A facet element has a master cell and, if it is an internal facet, a slave
! cell. Cell numbers are global element numbers; the local facet ID is the
! local facet number in the cell. The facet elements must be initiated
! (InitiateFacetElements). elementType is not used.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   facetElement : INTEGER(I4B), intent(in) - facet element number
!   elementType : INTEGER(I4B), intent(in) - facet element type; it is not
!     used
!
! Returns:
!   ans : INTEGER(I4B) - global element number of the slave cell, zero if none
!
! See also: [[obj_GetCellNumber]]

INTERFACE
  MODULE FUNCTION obj_GetSlaveCellNumber(obj, facetElement, elementType) &
    RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: facetElement
    !! facet element number
    INTEGER(I4B), INTENT(IN) :: elementType
    !! facet element type; it is not used
    INTEGER(I4B) :: ans
    !! global element number of the slave cell, zero if none
  END FUNCTION obj_GetSlaveCellNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetCellNumber@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Get the master and slave cells of a facet
!
! A facet element has a master cell and, if it is an internal facet, a slave
! cell. Cell numbers are global element numbers; the local facet ID is the
! local facet number in the cell. The facet elements must be initiated
! (InitiateFacetElements). elementType is not used.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   facetElement : INTEGER(I4B), intent(in) - facet element number
!   elementType : INTEGER(I4B), intent(in) - facet element type; it is not
!     used
!
! Returns:
!   ans(2) : INTEGER(I4B) - ans(1) master cell, ans(2) slave cell (zero if
!     none)
!
! Examples
!
!```fortran
! PROGRAM main
!   USE AbstractMesh_Class, ONLY: AbstractMeshGetFacetConnectivity
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ElemData_Class, ONLY: DOMAIN_BOUNDARY_ELEMENT
!   USE ElemData_Class, ONLY: INTERNAL_ELEMENT
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!   INTEGER(I4B) :: etype
!   INTEGER(I4B), ALLOCATABLE :: con(:)
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!
!   CALL obj%InitiateFacetElements()
!
!   CALL obj%GetFacetParam(facetElement=2_I4B, elementType=etype)
!   CALL OK(etype .EQ. INTERNAL_ELEMENT, "GetFacetParam")
!   CALL OK(ALL(obj%GetCellNumber(2_I4B, etype) .EQ. [13, 15]), &
!     "GetCellNumber")
!   CALL OK(obj%GetMasterCellNumber(2_I4B, etype) .EQ. 13_I4B, &
!     "GetMasterCellNumber")
!   CALL OK(obj%GetSlaveCellNumber(2_I4B, etype) .EQ. 15_I4B, &
!     "GetSlaveCellNumber")
!   CALL OK(obj%GetLocalFacetID(2_I4B, etype, .TRUE.) .EQ. 2_I4B, &
!     "GetLocalFacetID master")
!   CALL OK(obj%GetLocalFacetID(2_I4B, etype, .FALSE.) .EQ. 4_I4B, &
!     "GetLocalFacetID slave")
!   con = AbstractMeshGetFacetConnectivity(obj, 2_I4B, etype, .TRUE.)
!   CALL OK(ALL(con .EQ. [5, 9]), "AbstractMeshGetFacetConnectivity")
!   CALL OK(.NOT. obj%IsDomainFacetElement(2_I4B), "internal facet")
!
!   CALL obj%GetFacetParam(facetElement=1_I4B, elementType=etype)
!   CALL OK(etype .EQ. DOMAIN_BOUNDARY_ELEMENT, "boundary facet")
!   CALL OK(obj%IsDomainFacetElement(1_I4B), "IsDomainFacetElement")
!   CALL OK(ALL(obj%GetCellNumber(1_I4B, etype) .EQ. [13, 0]), &
!     "no slave cell")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetMasterCellNumber]], [[obj_GetSlaveCellNumber]],
!   [[obj_GetLocalFacetID]], [[AbstractMeshGetFacetConnectivity]],
!   [[obj_GetFacetParam]], [[obj_isDomainFacetElement]]

INTERFACE
  MODULE FUNCTION obj_GetCellNumber(obj, facetElement, elementType) &
    RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: facetElement
    !! facet element number
    INTEGER(I4B), INTENT(IN) :: elementType
    !! facet element type; it is not used
    INTEGER(I4B) :: ans(2)
    !! ans(1) master cell, ans(2) slave cell (zero if none)
  END FUNCTION obj_GetCellNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetLocalFacetID@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Get the local facet ID of a facet element
!
! A facet element has a master cell and, if it is an internal facet, a slave
! cell. Cell numbers are global element numbers; the local facet ID is the
! local facet number in the cell. The facet elements must be initiated
! (InitiateFacetElements). elementType is not used.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   facetElement : INTEGER(I4B), intent(in) - facet element number
!   elementType : INTEGER(I4B), intent(in) - facet element type; it is not
!     used
!   isMaster : LOGICAL(LGT), intent(in) - if true, the master cell is used,
!     else the slave cell
!
! Returns:
!   ans : INTEGER(I4B) - local facet number in the master (or slave) cell
!
! See also: [[obj_GetCellNumber]]

INTERFACE
  MODULE FUNCTION obj_GetLocalFacetID(obj, facetElement, elementType, &
                                      isMaster) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: facetElement
    !! facet element number
    INTEGER(I4B), INTENT(IN) :: elementType
    !! facet element type; it is not used
    LOGICAL(LGT), INTENT(IN) :: isMaster
    !! if true, the master cell is used, else the slave cell
    INTEGER(I4B) :: ans
    !! local facet number in the master (or slave) cell
  END FUNCTION obj_GetLocalFacetID
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetGlobalFaceNumber@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-21
! summary: Get the global face number of a local face
!
! The faces are numbered by Initiate (volume and surface meshes).
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   localFaceNumber : INTEGER(I4B), intent(in) - local face number in the
!     element
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans : INTEGER(I4B) - global face number
!
! See also: [[obj_InitiateFaceConnectivity]], [[obj_GetGlobalEdgeNumber]]

INTERFACE
  MODULE FUNCTION obj_GetGlobalFaceNumber(obj, globalElement, &
                                          localFaceNumber, &
                                          islocal) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! local face number in the element
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    INTEGER(I4B) :: ans
    !! global face number
  END FUNCTION obj_GetGlobalFaceNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetGlobalEdgeNumber@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the global edge number of a local edge
!
! The edges are numbered by InitiateEdgeConnectivity.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   localEdgeNumber : INTEGER(I4B), intent(in) - local edge number in the
!     element
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans : INTEGER(I4B) - global edge number
!
! See also: [[obj_InitiateEdgeConnectivity]], [[obj_GetGlobalFaceNumber]]

INTERFACE
  MODULE FUNCTION obj_GetGlobalEdgeNumber(obj, globalElement, &
                                          localEdgeNumber, &
                                          islocal) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    INTEGER(I4B), INTENT(IN) :: localEdgeNumber
    !! local edge number in the element
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    INTEGER(I4B) :: ans
    !! global edge number
  END FUNCTION obj_GetGlobalEdgeNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                                       FindFace@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-20
! summary: Find a face in an element
!
! Check if faceCon (the vertex nodes of a face, in any order) is a face of an
! element, and return its local face number.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   faceCon(:) : INTEGER(I4B), intent(in) - global vertex node numbers of the
!     face
!   isFace : LOGICAL(LGT), intent(out) - true if faceCon is a face of the
!     element
!   localFaceNumber : INTEGER(I4B), intent(out) - local face number, zero if
!     not found
!   onlyBoundaryElement : LOGICAL(LGT), intent(in) - if true, the element is
!     searched only if it is a boundary element
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: I4B
!   USE GlobalData, ONLY: LGT
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/small_hexa8_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!   LOGICAL(LGT) :: isFace
!   INTEGER(I4B) :: lfn
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=3)
!
!   CALL obj%FindFace(globalElement=36_I4B, faceCon=[9, 10, 12, 11], &
!     isFace=isFace, localFaceNumber=lfn, &
!     onlyBoundaryElement=.FALSE.)
!   CALL OK(isFace .AND. lfn .EQ. 3_I4B, "shared face")
!   CALL obj%FindFace(globalElement=36_I4B, faceCon=[11, 12, 10, 9], &
!     isFace=isFace, localFaceNumber=lfn, &
!     onlyBoundaryElement=.FALSE.)
!   CALL OK(isFace .AND. lfn .EQ. 3_I4B, "any node order")
!   CALL obj%FindFace(globalElement=36_I4B, faceCon=[1, 9, 11, 5], &
!     isFace=isFace, localFaceNumber=lfn, &
!     onlyBoundaryElement=.FALSE.)
!   CALL OK(.NOT. isFace .AND. lfn .EQ. 0_I4B, "not a face of 36")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_FindEdge]]

INTERFACE
  MODULE SUBROUTINE obj_FindFace(obj, globalElement, faceCon, isFace, &
                                 localFaceNumber, onlyBoundaryElement, &
                                 islocal)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    INTEGER(I4B), INTENT(IN) :: faceCon(:)
    !! global vertex node numbers of the face
    LOGICAL(LGT), INTENT(OUT) :: isFace
    !! true if faceCon is a face of the element
    INTEGER(I4B), INTENT(OUT) :: localFaceNumber
    !! local face number, zero if not found
    LOGICAL(LGT), INTENT(IN) :: onlyBoundaryElement
    !! if true, the element is searched only if it is a boundary element
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
  END SUBROUTINE obj_FindFace
END INTERFACE

!----------------------------------------------------------------------------
!                                                       FindEdge@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-20
! summary: Find an edge in an element
!
! Check if edgeCon (the nodes of an edge, in any order) is an edge of an
! element, and return its local edge number. Use it only for volume meshes.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   edgeCon(:) : INTEGER(I4B), intent(in) - global node numbers of the edge
!   isEdge : LOGICAL(LGT), intent(out) - true if edgeCon is an edge of the
!     element
!   localEdgeNumber : INTEGER(I4B), intent(out) - local edge number, zero if
!     not found
!   onlyBoundaryElement : LOGICAL(LGT), intent(in) - if true, the element is
!     searched only if it is a boundary element
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: I4B
!   USE GlobalData, ONLY: LGT
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/small_hexa8_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!   LOGICAL(LGT) :: isEdge
!   INTEGER(I4B) :: len_
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=3)
!
!   CALL obj%FindEdge(globalElement=35_I4B, edgeCon=[9, 10], &
!     isEdge=isEdge, localEdgeNumber=len_, &
!     onlyBoundaryElement=.FALSE.)
!   CALL OK(isEdge .AND. len_ .EQ. 4_I4B, "edge 9-10")
!   CALL obj%FindEdge(globalElement=35_I4B, edgeCon=[2, 4], &
!     isEdge=isEdge, localEdgeNumber=len_, &
!     onlyBoundaryElement=.FALSE.)
!   CALL OK(.NOT. isEdge .AND. len_ .EQ. 0_I4B, "not an edge of 35")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_FindFace]]

INTERFACE
  MODULE SUBROUTINE obj_FindEdge(obj, globalElement, edgeCon, isEdge, &
                                 localEdgeNumber, onlyBoundaryElement, &
                                 islocal)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    INTEGER(I4B), INTENT(IN) :: edgeCon(:)
    !! global node numbers of the edge
    LOGICAL(LGT), INTENT(OUT) :: isEdge
    !! true if edgeCon is an edge of the element
    INTEGER(I4B), INTENT(OUT) :: localEdgeNumber
    !! local edge number, zero if not found
    LOGICAL(LGT), INTENT(IN) :: onlyBoundaryElement
    !! if true, the element is searched only if it is a boundary element
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
  END SUBROUTINE obj_FindEdge
END INTERFACE

!----------------------------------------------------------------------------
!                                           GetFacetConnectivity@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Get the connectivity of a facet element
!
! A facet element has a master cell and, if it is an internal facet, a slave
! cell. Cell numbers are global element numbers; the local facet ID is the
! local facet number in the cell. The facet elements must be initiated
! (InitiateFacetElements). elementType is not used.
!
! Return the global node numbers of the facet in the master (or slave) cell.
! The array has size zero if the cell does not exist.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   facetElement : INTEGER(I4B), intent(in) - facet element number
!   elementType : INTEGER(I4B), intent(in) - facet element type; it is not
!     used
!   isMaster : LOGICAL(LGT), intent(in) - if true, the master cell is used,
!     else the slave cell
!
! Returns:
!   ans(:) : INTEGER(I4B), allocatable - global node numbers of the facet
!
! See also: [[obj_GetCellNumber]]

INTERFACE
  MODULE FUNCTION AbstractMeshGetFacetConnectivity(obj, facetElement, &
                                                   elementType, &
                                                   isMaster) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: facetElement
    !! facet element number
    INTEGER(I4B), INTENT(IN) :: elementType
    !! facet element type; it is not used
    LOGICAL(LGT), INTENT(IN) :: isMaster
    !! if true, the master cell is used, else the slave cell
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !! global node numbers of the facet
  END FUNCTION AbstractMeshGetFacetConnectivity
END INTERFACE

!----------------------------------------------------------------------------
!                                           GetFacetConnectivity@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Get the connectivity of a facet of an element
!
! Return the global node numbers of the local facet iface of an element.
! GetFacetConnectivity_ does the same without allocation, and
! GetTotalFacetConnectivity returns the number of nodes of the facet.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   iface : INTEGER(I4B), intent(in) - local facet number in the element
!   islocal : LOGICAL(I4B), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans(:) : INTEGER(I4B), allocatable - global node numbers of the facet
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!   INTEGER(I4B), ALLOCATABLE :: con(:)
!   INTEGER(I4B) :: buf(10), tsize
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!
!   con = obj%GetFacetConnectivity(globalElement=13_I4B, iface=1_I4B)
!   CALL OK(ALL(con .EQ. [1, 5]), "facet 1 of 13")
!   con = obj%GetFacetConnectivity(globalElement=13_I4B, iface=2_I4B)
!   CALL OK(ALL(con .EQ. [5, 9]), "facet 2 of 13")
!   CALL obj%GetFacetConnectivity_(globalElement=14_I4B, iface=2_I4B, &
!     ans=buf, tsize=tsize)
!   CALL OK(ALL(buf(1:tsize) .EQ. [9, 6]), "GetFacetConnectivity_")
!   CALL OK(obj%GetTotalFacetConnectivity(globalElement=13_I4B, &
!     iface=1_I4B) .EQ. 2_I4B, &
!     "GetTotalFacetConnectivity")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetFacetConnectivity_]], [[obj_GetTotalFacetConnectivity]]

INTERFACE
  MODULE FUNCTION obj_GetFacetConnectivity(obj, globalElement, &
                                           iface, islocal) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    INTEGER(I4B), INTENT(IN) :: iface
    !! local facet number in the element
    LOGICAL(I4B), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !! global node numbers of the facet
  END FUNCTION obj_GetFacetConnectivity
END INTERFACE

!----------------------------------------------------------------------------
!                                           GetFacetConnectivity_@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Get the connectivity of a facet without allocation
!
! See GetFacetConnectivity.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   iface : INTEGER(I4B), intent(in) - local facet number in the element
!   ans(:) : INTEGER(I4B), intent(inout) - global node numbers of the facet,
!     size must be at least tsize
!   tsize : INTEGER(I4B), intent(out) - number of entries written in ans
!   islocal : LOGICAL(I4B), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! See also: [[obj_GetFacetConnectivity]]

INTERFACE
  MODULE SUBROUTINE obj_GetFacetConnectivity_(obj, globalElement, &
                                              iface, ans, tsize, islocal)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    INTEGER(I4B), INTENT(IN) :: iface
    !! local facet number in the element
    LOGICAL(I4B), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! global node numbers of the facet, size must be at least tsize
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written in ans
  END SUBROUTINE obj_GetFacetConnectivity_
END INTERFACE

!----------------------------------------------------------------------------
!                                           GetFacetConnectivity_@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Get the number of nodes of a facet
!
! See GetFacetConnectivity.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   iface : INTEGER(I4B), intent(in) - local facet number in the element
!   islocal : LOGICAL(I4B), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans : INTEGER(I4B) - number of nodes of the facet
!
! See also: [[obj_GetFacetConnectivity]]

INTERFACE
  MODULE FUNCTION obj_GetTotalFacetConnectivity(obj, globalElement, &
                                                iface, islocal) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    INTEGER(I4B), INTENT(IN) :: iface
    !! local facet number in the element
    LOGICAL(I4B), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    INTEGER(I4B) :: ans
    !! number of nodes of the facet
  END FUNCTION obj_GetTotalFacetConnectivity
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetQuery@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the parameters of the mesh
!
! Each present argument returns the component of the same name (see
! AbstractMesh_). SetParam sets them, and SetBoundingBox sets the bounding box
! parameters.
!
! Arguments:
!   obj : CLASS(AbstractMesh_) - mesh object
!   isInitiated, isNodeToElementsInitiated, isNodeToNodesInitiated,
!     isExtraNodeToNodesInitiated, isElementToElementsInitiated,
!     isBoundaryDataInitiated, isFacetDataInitiated : LOGICAL(LGT), optional -
!     status flags
!   uid, xidim, elemType, nsd, maxNptrs, minNptrs, maxElemNum, minElemNum,
!     tNodes, tElements, tElemTopologies : INTEGER(I4B), optional - scalar
!     parameters
!   tElements_topology_wise(8), elemTopologies(8) : INTEGER(I4B), optional -
!     elements and names of each topology
!   minX, minY, minZ, maxX, maxY, maxZ, x, y, z : REAL(DFP), optional -
!     bounding box and centroid
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: BoundingBox_
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE BoundingBox_Method, ONLY: BoundingBoxInitiate => Initiate
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: DFP
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!   REAL(DFP), PARAMETER :: tol = 1.0E-8_DFP
!   TYPE(BoundingBox_) :: box
!   REAL(DFP), ALLOCATABLE :: xij(:, :)
!   REAL(DFP) :: maxX, minX, maxY
!   INTEGER(I4B) :: tNodes, tElements, nsd, uid
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!
!   CALL obj%GetParam(tNodes=tNodes, tElements=tElements, nsd=nsd, &
!     uid=uid, maxX=maxX)
!   CALL OK(tNodes .EQ. 9_I4B .AND. tElements .EQ. 4_I4B, "counts")
!   CALL OK(nsd .EQ. 2_I4B .AND. uid .EQ. 1_I4B, "nsd and uid")
!   CALL OK(ABS(maxX - 1.0_DFP) .LT. tol, "maxX")
!
!   CALL obj%SetParam(uid=42_I4B)
!   CALL obj%GetParam(uid=uid)
!   CALL OK(uid .EQ. 42_I4B, "SetParam")
!
!   CALL BoundingBoxInitiate(obj=box, nsd=3_I4B, &
!     lim=[-1.0_DFP, 2.0_DFP, -3.0_DFP, &
!     4.0_DFP, 0.0_DFP, 0.0_DFP])
!   CALL obj%SetBoundingBox(box=box)
!   CALL obj%GetParam(minX=minX, maxY=maxY)
!   CALL OK(ABS(minX + 1.0_DFP) .LT. tol .AND. &
!     ABS(maxY - 4.0_DFP) .LT. tol, "SetBoundingBox(box)")
!
!   CALL obj%GetNodeCoord(nodeCoord=xij, hdf5=meshfile, group="")
!   CALL obj%SetBoundingBox(nodes=xij)
!   CALL obj%GetParam(minX=minX, maxY=maxY)
!   CALL OK(ABS(minX) .LT. tol .AND. ABS(maxY - 1.0_DFP) .LT. tol, &
!     "SetBoundingBox(nodes)")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_SetParam]], [[obj_SetBoundingBox1]], [[obj_SetBoundingBox2]]

INTERFACE AbstractMeshGetParam
  MODULE SUBROUTINE obj_GetParam(obj, isInitiated, &
                                 isNodeToElementsInitiated, &
                                 isNodeToNodesInitiated, &
                                 isExtraNodeToNodesInitiated, &
                                 isElementToElementsInitiated, &
                                 isBoundaryDataInitiated, &
                                 isFacetDataInitiated, uid, xidim, elemType, &
                                 nsd, maxNptrs, minNptrs, maxElemNum, &
                                 minElemNum, tNodes, tElements, minX, minY, &
                                 minZ, maxX, maxY, maxZ, x, y, z, &
                                 tElements_topology_wise, tElemTopologies, &
                                 elemTopologies)

    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    LOGICAL(LGT), INTENT(OUT), OPTIONAL :: isInitiated, &
      isNodeToElementsInitiated, &
      isNodeToNodesInitiated, &
      isExtraNodeToNodesInitiated, &
      isElementToElementsInitiated, &
      isBoundaryDataInitiated, &
      isFacetDataInitiated
    !! status flags of the mesh
    INTEGER(I4B), INTENT(OUT), OPTIONAL :: uid, xidim, elemType, nsd, &
      maxNptrs, minNptrs, maxElemNum, &
      minElemNum, tNodes, tElements, &
      tElements_topology_wise(8), &
      tElemTopologies, elemTopologies(8)
    !! scalar parameters of the mesh
    REAL(DFP), INTENT(OUT), OPTIONAL :: minX, minY, minZ, maxX, maxY, maxZ, &
      x, y, z
    !! bounding box and centroid of the mesh
  END SUBROUTINE obj_GetParam
END INTERFACE AbstractMeshGetParam

!----------------------------------------------------------------------------
!                                                GetMinElemNumber@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the minimum element number
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!
! Returns:
!   ans : INTEGER(I4B) - smallest global element number
!
! See also: [[obj_Initiate]]

INTERFACE
  MODULE FUNCTION obj_GetMinElemNumber(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B) :: ans
    !! smallest global element number
  END FUNCTION obj_GetMinElemNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetMaxElemNumber@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the maximum element number
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!
! Returns:
!   ans : INTEGER(I4B) - largest global element number
!
! See also: [[obj_Initiate]]

INTERFACE
  MODULE FUNCTION obj_GetMaxElemNumber(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B) :: ans
    !! largest global element number
  END FUNCTION obj_GetMaxElemNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetMinNodeNumber@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the minimum node number
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!
! Returns:
!   ans : INTEGER(I4B) - smallest global node number
!
! See also: [[obj_Initiate]]

INTERFACE
  MODULE FUNCTION obj_GetMinNodeNumber(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B) :: ans
    !! smallest global node number
  END FUNCTION obj_GetMinNodeNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetMaxNodeNumber@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the maximum node number
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!
! Returns:
!   ans : INTEGER(I4B) - largest global node number
!
! See also: [[obj_Initiate]]

INTERFACE
  MODULE FUNCTION obj_GetMaxNodeNumber(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B) :: ans
    !! largest global node number
  END FUNCTION obj_GetMaxNodeNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                                         isInit@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Returns true if the mesh is initiated
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!
! Returns:
!   ans : LOGICAL(LGT) - status flag
!
! See also: [[obj_Initiate]]

INTERFACE
  MODULE FUNCTION obj_isInit(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    LOGICAL(LGT) :: ans
    !! status flag
  END FUNCTION obj_isInit
END INTERFACE

!----------------------------------------------------------------------------
!                                                         isInit@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Returns true if node to elements data is initiated
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!
! Returns:
!   ans : LOGICAL(LGT) - status flag
!
! See also: [[obj_InitiateNodeToElements]]

INTERFACE
  MODULE FUNCTION obj_isNodeToElements(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    LOGICAL(LGT) :: ans
    !! status flag
  END FUNCTION obj_isNodeToElements
END INTERFACE

!----------------------------------------------------------------------------
!                                                         isInit@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Returns true if node to nodes data is initiated
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!
! Returns:
!   ans : LOGICAL(LGT) - status flag
!
! See also: [[obj_InitiateNodetoNodes]]

INTERFACE
  MODULE FUNCTION obj_isNodeToNodes(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    LOGICAL(LGT) :: ans
    !! status flag
  END FUNCTION obj_isNodeToNodes
END INTERFACE

!----------------------------------------------------------------------------
!                                                         isInit@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Returns true if extra node to nodes data is initiated
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!
! Returns:
!   ans : LOGICAL(LGT) - status flag
!
! See also: [[obj_InitiateExtraNodetoNodes]]

INTERFACE
  MODULE FUNCTION obj_isExtraNodeToNodes(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    LOGICAL(LGT) :: ans
    !! status flag
  END FUNCTION obj_isExtraNodeToNodes
END INTERFACE

!----------------------------------------------------------------------------
!                                                         isInit@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Returns true if element to elements data is initiated
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!
! Returns:
!   ans : LOGICAL(LGT) - status flag
!
! See also: [[obj_InitiateElementToElements]]

INTERFACE
  MODULE FUNCTION obj_isElementToElements(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    LOGICAL(LGT) :: ans
    !! status flag
  END FUNCTION obj_isElementToElements
END INTERFACE

!----------------------------------------------------------------------------
!                                                         isInit@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Returns true if the edge connectivity is initiated
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!
! Returns:
!   ans : LOGICAL(LGT) - status flag
!
! See also: [[obj_InitiateEdgeConnectivity]]

INTERFACE
  MODULE FUNCTION obj_isEdgeConnectivity(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    LOGICAL(LGT) :: ans
    !! status flag
  END FUNCTION obj_isEdgeConnectivity
END INTERFACE

!----------------------------------------------------------------------------
!                                                         isInit@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Returns true if the face connectivity is initiated
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!
! Returns:
!   ans : LOGICAL(LGT) - status flag
!
! See also: [[obj_InitiateFaceConnectivity]]

INTERFACE
  MODULE FUNCTION obj_isFaceConnectivity(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    LOGICAL(LGT) :: ans
    !! status flag
  END FUNCTION obj_isFaceConnectivity
END INTERFACE

!----------------------------------------------------------------------------
!                                                         isInit@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Returns true if the boundary data is initiated
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!
! Returns:
!   ans : LOGICAL(LGT) - status flag
!
! See also: [[obj_InitiateBoundaryData]]

INTERFACE
  MODULE FUNCTION obj_isBoundaryData(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    LOGICAL(LGT) :: ans
    !! status flag
  END FUNCTION obj_isBoundaryData
END INTERFACE

!----------------------------------------------------------------------------
!                                                         isInit@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Returns true if the facet data is initiated
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!
! Returns:
!   ans : LOGICAL(LGT) - status flag
!
! See also: [[obj_InitiateFacetElements]]

INTERFACE
  MODULE FUNCTION obj_isFacetData(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    LOGICAL(LGT) :: ans
    !! status flag
  END FUNCTION obj_isFacetData
END INTERFACE

!----------------------------------------------------------------------------
!                                                 isElementActive@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Returns true if an element is active
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans : LOGICAL(LGT) - true if the element is active
!
! See also: [[obj_isElementPresent]]

INTERFACE
  MODULE FUNCTION obj_isElementActive(obj, globalElement, islocal) &
    RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    LOGICAL(LGT) :: ans
    !! true if the element is active
  END FUNCTION obj_isElementActive
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetFacetParam@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-14
! summary: Get the parameters of a facet element
!
! Return the facet element type (INTERNAL_ELEMENT, BOUNDARY_ELEMENT or
! DOMAIN_BOUNDARY_ELEMENT).
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   facetElement : INTEGER(I4B), intent(in) - facet element number
!   elementType : INTEGER(I4B), optional, intent(out) - facet element type
!
! See also: [[obj_SetFacetParam]], [[obj_GetCellNumber]]

INTERFACE
  MODULE SUBROUTINE obj_GetFacetParam(obj, facetElement, elementType)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: facetElement
    !! facet element number
    INTEGER(I4B), INTENT(OUT), OPTIONAL :: elementType
    !! facet element type
  END SUBROUTINE obj_GetFacetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetTotalEntities@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-15
! summary: Get the number of entities of an element
!
! GetTotalEntities returns, for an element, the number of nodes, edges, faces
! and 1 (the cell). Without an element it returns the number of nodes, edges,
! faces and cells of the mesh. In a surface mesh the faces are the edges. The
! number of edges of the mesh is zero until InitiateEdgeConnectivity is
! called.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans(4) : INTEGER(I4B) - nodes, edges, faces, 1
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!   INTEGER(I4B) :: ent(4)
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!
!   CALL obj%InitiateEdgeConnectivity()
!   ent = obj%GetTotalEntities()
!   CALL OK(ALL(ent .EQ. [9, 12, 12, 4]), "mesh")
!   ent = obj%GetTotalEntities(globalElement=13_I4B)
!   CALL OK(ALL(ent .EQ. [4, 4, 4, 1]), "element 13")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetTotalEntities2]]

INTERFACE
  MODULE FUNCTION obj_GetTotalEntities1(obj, globalElement, islocal) &
    RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    INTEGER(I4B) :: ans(4)
    !! nodes, edges, faces, 1
  END FUNCTION obj_GetTotalEntities1
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetTotalEntities@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-15
! summary: Get the number of entities of the mesh
!
! See GetTotalEntities.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(in) - mesh object
!
! Returns:
!   ans(4) : INTEGER(I4B) - nodes, edges, faces, cells
!
! See also: [[obj_GetTotalEntities1]]

INTERFACE
  MODULE FUNCTION obj_GetTotalEntities2(obj) &
    RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B) :: ans(4)
    !! nodes, edges, faces, cells
  END FUNCTION obj_GetTotalEntities2
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetMaxNodeToElements@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-17
! summary: Get the maximum number of elements around a node
!
! The node to elements data is initiated if needed.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!
! Returns:
!   ans : INTEGER(I4B) - largest number of elements around a node
!
! See also: [[obj_InitiateNodeToElements]]

INTERFACE
  MODULE FUNCTION obj_GetMaxNodeToElements(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
    INTEGER(I4B) :: ans
    !! largest number of elements around a node
  END FUNCTION obj_GetMaxNodeToElements
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetMaxNodeToNodes@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-17
! summary: Get the maximum number of nodes around a node
!
! The node to nodes data is initiated if needed. The node itself is not
! counted.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!
! Returns:
!   ans : INTEGER(I4B) - largest number of nodes connected to a node
!
! See also: [[obj_InitiateNodetoNodes]]

INTERFACE
  MODULE FUNCTION obj_GetMaxNodeToNodes(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
    INTEGER(I4B) :: ans
    !! largest number of nodes connected to a node
  END FUNCTION obj_GetMaxNodeToNodes
END INTERFACE

!----------------------------------------------------------------------------
!                                         GetMaxElementToElements@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-17
! summary: Get the maximum number of elements around an element
!
! The element to elements data is initiated if needed.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!
! Returns:
!   ans : INTEGER(I4B) - largest number of neighbours of an element
!
! See also: [[obj_InitiateElementToElements]]

INTERFACE
  MODULE FUNCTION obj_GetMaxElementToElements(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
    INTEGER(I4B) :: ans
    !! largest number of neighbours of an element
  END FUNCTION obj_GetMaxElementToElements
END INTERFACE

!----------------------------------------------------------------------------
!                                           InitiateKdtree@NodeDataMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-10
! summary: Initiate the kdtree of the mesh
!
! Make a kdtree of the node coordinates (the first nsd coordinates). It is
! used by GetNearestNode and GetNptrsInBox, which initiate it when needed. The
! tree is not sorted.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: DFP
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!   REAL(DFP) :: x(3)
!   INTEGER(I4B) :: node
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!
!   CALL obj%InitiateKdtree()
!   CALL obj%GetNearestNode(qv=[0.9_DFP, 0.1_DFP], x=x, globalNode=node)
!   CALL OK(node .EQ. 2_I4B, "nearest node")
!
!   CALL obj%DeallocateKdtree()
!   CALL obj%GetNearestNode(qv=[0.9_DFP, 0.1_DFP], x=x, globalNode=node)
!   CALL OK(node .EQ. 2_I4B, "nearest node after DeallocateKdtree")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_DeallocateKdtree]], [[obj_GetNearestNode1]],
!   [[obj_GetNptrsInBox]]

INTERFACE
  MODULE SUBROUTINE obj_InitiateKdtree(obj)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
  END SUBROUTINE obj_InitiateKdtree
END INTERFACE

!----------------------------------------------------------------------------
!                                     InitiateNodeToElements@NodeDataMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Initiate the node to elements data
!
! For every node, store the global element numbers around it. Nothing is done
! if the data is already initiated. Use GetNodeToElements to read it.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!
!   CALL OK(.NOT. obj%IsNodeToElements(), "not initiated by Initiate")
!   CALL obj%InitiateNodeToElements()
!   CALL OK(obj%IsNodeToElements(), "IsNodeToElements")
!   CALL OK(obj%GetMaxNodeToElements() .EQ. 4_I4B, &
!     "GetMaxNodeToElements")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetNodeToElements1]], [[obj_isNodeToElements]],
!   [[obj_GetMaxNodeToElements]]

INTERFACE
  MODULE SUBROUTINE obj_InitiateNodeToElements(obj)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
  END SUBROUTINE obj_InitiateNodeToElements
END INTERFACE

!----------------------------------------------------------------------------
!                                         InitiateNodeToNode@NodeDataMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Initiate the node to nodes data
!
! For every node, store the nodes of the elements around it (without the node
! itself). The node to elements data is initiated if needed. Use
! GetNodeToNodes to read it.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!   INTEGER(I4B), ALLOCATABLE :: nodes(:)
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!
!   CALL obj%InitiateNodeToNodes()
!   CALL OK(obj%IsNodeToNodes(), "IsNodeToNodes")
!   CALL OK(obj%IsNodeToElements(), "node to elements is initiated too")
!   nodes = obj%GetNodeToNodes(globalNode=9_I4B, includeSelf=.FALSE.)
!   CALL OK(SIZE(nodes) .EQ. 8_I4B, "center node has 8 neighbours")
!   CALL OK(obj%GetMaxNodeToNodes() .EQ. 8_I4B, "GetMaxNodeToNodes")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetNodeToNodes1]], [[obj_isNodeToNodes]],
!   [[obj_GetMaxNodeToNodes]]

INTERFACE
  MODULE SUBROUTINE obj_InitiateNodetoNodes(obj)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
  END SUBROUTINE obj_InitiateNodetoNodes
END INTERFACE

!----------------------------------------------------------------------------
!                                   InitiateExtraNodeToNode@NodeDataMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Initiate the extra node to nodes data
!
! For every node, store extra nodes used by edge based stabilization methods.
! The node to nodes, node to elements and element to elements data are
! initiated if needed.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!
!   CALL obj%InitiateExtraNodeToNodes()
!   CALL OK(obj%IsExtraNodeToNodes(), "IsExtraNodeToNodes")
!   CALL OK(obj%IsNodeToNodes(), "IsNodeToNodes")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_InitiateNodetoNodes]], [[obj_isExtraNodeToNodes]]

INTERFACE
  MODULE SUBROUTINE obj_InitiateExtraNodetoNodes(obj)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
  END SUBROUTINE obj_InitiateExtraNodetoNodes
END INTERFACE

!----------------------------------------------------------------------------
!                               InitiateElementToElements@ElementDataMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Initiate the element to elements data
!
! For every element, store the neighbour elements and the shared facets. Then
! mark the boundary elements and the boundary nodes. In surface and volume
! meshes the faces are numbered too. Initiate calls this method; nothing is
! done if the data is already initiated.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!
!   CALL OK(obj%IsElementToElements(), "IsElementToElements")
!   CALL obj%InitiateElementToElements()
!   CALL OK(obj%GetMaxElementToElements() .EQ. 2_I4B, &
!     "GetMaxElementToElements")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetElementToElements]], [[obj_isElementToElements]],
!   [[obj_GetMaxElementToElements]]

INTERFACE
  MODULE SUBROUTINE obj_InitiateElementToElements(obj)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
  END SUBROUTINE obj_InitiateElementToElements
END INTERFACE

!----------------------------------------------------------------------------
!                                  InitiateBoundaryData@BoundaryDataMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Initiate the boundary data
!
! For every boundary element, store the local facets which are on the boundary
! of the mesh. The element to elements data is needed.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!   INTEGER(I4B), ALLOCATABLE :: bdata(:)
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!
!   CALL obj%InitiateBoundaryData()
!   CALL OK(obj%IsBoundaryData(), "IsBoundaryData")
!   CALL OK(obj%GetTotalBoundaryElements() .EQ. 4_I4B, &
!     "GetTotalBoundaryElements")
!   ! element 13 = [1, 5, 9, 7]: facets 1-5 and 7-1 are on the boundary
!   bdata = obj%GetBoundaryElementData(globalElement=13_I4B)
!   CALL OK(ALL(bdata .EQ. [1, 4]), "element 13")
!   bdata = obj%GetBoundaryElementData(globalElement=16_I4B)
!   CALL OK(ALL(bdata .EQ. [2, 3]), "element 16")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetBoundaryElementData]], [[obj_isBoundaryData]]

INTERFACE
  MODULE SUBROUTINE obj_InitiateBoundaryData(obj)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
  END SUBROUTINE obj_InitiateBoundaryData
END INTERFACE

!----------------------------------------------------------------------------
!                                   InitiateEdgeConnectivity@EdgeDataMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-07
! summary: Initiate the edge connectivity
!
! Number the edges of the mesh. An edge shared by elements has the same global
! edge number in all of them. GetTotalEdges returns the number of edges.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!
!   CALL OK(obj%GetTotalEdges() .EQ. 0_I4B, "no edges before")
!   CALL obj%InitiateEdgeConnectivity()
!   CALL OK(obj%IsEdgeConnectivity(), "IsEdgeConnectivity")
!   CALL OK(obj%GetTotalEdges() .EQ. 12_I4B, "GetTotalEdges")
!
!   ! edge [7, 9] is edge 2 of element 13 and edge 1 of element 14
!   CALL OK(obj%GetGlobalEdgeNumber(globalElement=13_I4B, &
!     localEdgeNumber=2_I4B) .EQ. &
!     obj%GetGlobalEdgeNumber(globalElement=14_I4B, &
!     localEdgeNumber=1_I4B), "edge 7-9")
!   ! edge [5, 9] is edge 4 of element 13 and edge 3 of element 15
!   CALL OK(obj%GetGlobalEdgeNumber(globalElement=13_I4B, &
!     localEdgeNumber=4_I4B) .EQ. &
!     obj%GetGlobalEdgeNumber(globalElement=15_I4B, &
!     localEdgeNumber=3_I4B), "edge 5-9")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetGlobalEdgeNumber]], [[obj_GetTotalEdges]],
!   [[obj_isEdgeConnectivity]]

INTERFACE
  MODULE SUBROUTINE obj_InitiateEdgeConnectivity(obj)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
  END SUBROUTINE obj_InitiateEdgeConnectivity
END INTERFACE

!----------------------------------------------------------------------------
!                                   InitiateFaceConnectivity@FaceDataMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-07
! summary: Initiate the face connectivity
!
! Number the faces of a surface (xidim = 2) or volume (xidim = 3) mesh. A
! shared face has the same global face number in both elements. For other
! meshes an error is raised.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/small_hexa8_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=3)
!
!   CALL obj%InitiateFaceConnectivity()
!   CALL OK(obj%IsFaceConnectivity(), "IsFaceConnectivity")
!   CALL OK(obj%GetTotalFaces() .EQ. 11_I4B, "GetTotalFaces")
!   ! face [9, 10, 12, 11] is face 4 of 35 and face 3 of 36
!   CALL OK(obj%GetGlobalFaceNumber(globalElement=35_I4B, &
!     localFaceNumber=4_I4B) .EQ. &
!     obj%GetGlobalFaceNumber(globalElement=36_I4B, &
!     localFaceNumber=3_I4B), &
!     "shared face")
!   CALL obj%InitiateEdgeConnectivity()
!   CALL OK(obj%GetTotalEdges() .EQ. 20_I4B, "GetTotalEdges")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetGlobalFaceNumber]], [[obj_GetTotalFaces]],
!   [[obj_isFaceConnectivity]]

INTERFACE
  MODULE SUBROUTINE obj_InitiateFaceConnectivity(obj)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
  END SUBROUTINE obj_InitiateFaceConnectivity
END INTERFACE

!----------------------------------------------------------------------------
!                                     InitiateFacetElements@FacetDataMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Initiate the facet elements
!
! Make the facet data: one facet element for every internal facet and every
! boundary facet. The boundary data is initiated if needed. All boundary
! facets are DOMAIN_BOUNDARY_ELEMENT; a domain can change this when it knows
! the neighbouring meshes. Point and curve meshes (xidim <= 1) have no facet
! elements; nothing is done for them.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!
!   CALL obj%InitiateFacetElements()
!   CALL OK(obj%IsFacetData(), "IsFacetData")
!   CALL OK(obj%IsBoundaryData(), "IsBoundaryData")
!   CALL OK(obj%GetTotalFacetElements() .EQ. 12_I4B, &
!     "GetTotalFacetElements")
!   CALL OK(obj%GetTotalInternalFacetElements() .EQ. 4_I4B, &
!     "GetTotalInternalFacetElements")
!   CALL OK(obj%GetTotalBoundaryFacetElements() .EQ. 8_I4B, &
!     "GetTotalBoundaryFacetElements")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetTotalFacetElements]], [[obj_GetCellNumber]],
!   [[obj_isFacetData]]

INTERFACE
  MODULE SUBROUTINE obj_InitiateFacetElements(obj)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
  END SUBROUTINE obj_InitiateFacetElements
END INTERFACE

!----------------------------------------------------------------------------
!                                                   SetShowTime@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-31
! summary: Set the showTime option
!
! If showTime is true, the time taken by mesh operations is printed on stdout.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!   VALUE : LOGICAL(LGT), intent(in) - value of showTime
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!
!   CALL obj%SetShowTime(.TRUE.)
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!   CALL obj%InitiateNodeToNodes()
!   CALL obj%SetShowTime(.FALSE.)
!   CALL OK(obj%IsNodeToNodes(), "mesh is not changed")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE obj_SetShowTime(obj, VALUE)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
    LOGICAL(LGT), INTENT(IN) :: VALUE
    !! value of showTime
  END SUBROUTINE obj_SetShowTime
END INTERFACE

!----------------------------------------------------------------------------
!                                                  SetBoundingBox@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Set the bounding box parameters of the mesh
!
! Set minX, maxX, minY, maxY, minZ and maxZ from box (or from node
! coordinates, see GetBoundingBox).
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!   box : TYPE(BoundingBox_), intent(in) - bounding box
!
! See also: [[obj_SetBoundingBox2]], [[obj_GetParam]], [[obj_GetBoundingBox1]]

INTERFACE
  MODULE SUBROUTINE obj_SetBoundingBox1(obj, box)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
    TYPE(BoundingBox_), INTENT(IN) :: box
    !! bounding box
  END SUBROUTINE obj_SetBoundingBox1
END INTERFACE

!----------------------------------------------------------------------------
!                                                  SetBoundingBox@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Set the bounding box parameters from node coordinates
!
! See SetBoundingBox.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!   nodes(:,:) : REAL(DFP), intent(in) - node coordinates in xiJ format,
!     column i is global node i (or local_nptrs(i))
!   local_nptrs(:) : INTEGER(I4B), optional, intent(in) - map from a global
!     node number to a column of nodes
!
! See also: [[obj_SetBoundingBox1]]

INTERFACE
  MODULE SUBROUTINE obj_SetBoundingBox2(obj, nodes, local_nptrs)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
    REAL(DFP), INTENT(IN) :: nodes(:, :)
    !! node coordinates in xiJ format, column i is global node i (or
    !! local_nptrs(i))
    INTEGER(I4B), INTENT(IN), OPTIONAL :: local_nptrs(:)
    !! map from a global node number to a column of nodes
  END SUBROUTINE obj_SetBoundingBox2
END INTERFACE

!----------------------------------------------------------------------------
!                                                      SetSparsity@SetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Set the sparsity pattern of a matrix
!
! Add an entry (i, j) to the sparsity pattern of mat for every node j
! connected to node i (and for j = i). The node to nodes data is initiated if
! needed. After all calls, call SetSparsity(mat) of the matrix to finish the
! pattern.
!
! Row and column numbers are localNodeNumber(global node); nodes with
! localNodeNumber zero are skipped.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!   mat : TYPE(CSRMatrix_), intent(inout) - sparse matrix
!   localNodeNumber(lbound:ubound) : INTEGER(I4B), intent(in) - map from
!     global node number to row number
!   lbound : INTEGER(I4B), intent(in) - lower bound of localNodeNumber
!   ubound : INTEGER(I4B), intent(in) - upper bound of localNodeNumber
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: CSRMatrix_
!   USE BaseType, ONLY: DOF_
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE CSRMatrix_Method, ONLY: CSRMatrixInitiate => Initiate
!   USE CSRMatrix_Method, ONLY: CSRMatrixSetSparsity => SetSparsity
!   USE CSRMatrix_Method, ONLY: GetNNZ
!   USE DOF_Method, ONLY: DOFInitiate => Initiate
!   USE DOF_Method, ONLY: DOFSize => SIZE
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: I4B
!   USE GlobalData, ONLY: NODES_FMT
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!   TYPE(DOF_) :: dofobj
!   TYPE(CSRMatrix_) :: mat
!   INTEGER(I4B) :: nmap(9)
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!
!   CALL DOFInitiate(obj=dofobj, tNodes=[obj%GetTotalNodes()], &
!     names=["K"], spacecompo=[1_I4B], &
!     timecompo=[1_I4B], storagefmt=NODES_FMT)
!   CALL CSRMatrixInitiate(obj=mat, ncol=DOFSize(dofobj), &
!     nrow=DOFSize(dofobj), idof=dofobj, &
!     jdof=dofobj)
!   nmap = [1, 2, 3, 4, 5, 6, 7, 8, 9]
!   CALL obj%SetSparsity(mat=mat, localNodeNumber=nmap, LBOUND=1_I4B, &
!     UBOUND=9_I4B)
!   CALL CSRMatrixSetSparsity(mat)
!   CALL OK(GetNNZ(mat) .EQ. 49_I4B, "number of nonzeros")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_SetSparsity2]]

INTERFACE
  MODULE SUBROUTINE obj_SetSparsity1(obj, mat, localNodeNumber, lbound, &
                                     ubound)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
    TYPE(CSRMatrix_), INTENT(INOUT) :: mat
    !! sparse matrix
    INTEGER(I4B), INTENT(IN) :: lbound
    !! lower bound of localNodeNumber
    INTEGER(I4B), INTENT(IN) :: ubound
    !! upper bound of localNodeNumber
    INTEGER(I4B), INTENT(IN) :: localNodeNumber(lbound:ubound)
    !! map from global node number to row number
  END SUBROUTINE obj_SetSparsity1
END INTERFACE

!----------------------------------------------------------------------------
!                                                SetSparsity@MeshDataMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Set the sparsity pattern of a matrix
!
! Add an entry (i, j) to the sparsity pattern of mat for every node j
! connected to node i (and for j = i). The node to nodes data is initiated if
! needed. After all calls, call SetSparsity(mat) of the matrix to finish the
! pattern. Row and column numbers are global node numbers.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!   mat : TYPE(CSRMatrix_), intent(inout) - sparse matrix
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: CSRMatrix_
!   USE BaseType, ONLY: DOF_
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE CSRMatrix_Method, ONLY: CSRMatrixInitiate => Initiate
!   USE CSRMatrix_Method, ONLY: CSRMatrixSetSparsity => SetSparsity
!   USE CSRMatrix_Method, ONLY: GetNNZ
!   USE DOF_Method, ONLY: DOFInitiate => Initiate
!   USE DOF_Method, ONLY: DOFSize => SIZE
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: I4B
!   USE GlobalData, ONLY: NODES_FMT
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!   TYPE(DOF_) :: dofobj
!   TYPE(CSRMatrix_) :: mat
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!
!   CALL DOFInitiate(obj=dofobj, tNodes=[obj%GetTotalNodes()], &
!     names=["K"], spacecompo=[1_I4B], &
!     timecompo=[1_I4B], storagefmt=NODES_FMT)
!   CALL CSRMatrixInitiate(obj=mat, ncol=DOFSize(dofobj), &
!     nrow=DOFSize(dofobj), idof=dofobj, &
!     jdof=dofobj)
!   CALL obj%SetSparsity(mat=mat)
!   CALL CSRMatrixSetSparsity(mat)
!   CALL OK(GetNNZ(mat) .EQ. 49_I4B, "number of nonzeros")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_SetSparsity1]]

INTERFACE
  MODULE SUBROUTINE obj_SetSparsity2(obj, mat)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
    TYPE(CSRMatrix_), INTENT(INOUT) :: mat
    !! sparse matrix
  END SUBROUTINE obj_SetSparsity2
END INTERFACE

!----------------------------------------------------------------------------
!                                                      SetSparsity@SetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Set the sparsity pattern between two meshes
!
! Rows are the nodes of obj, columns the nodes of colMesh: for node i of obj,
! the nodes nodeToNode(j) of colMesh (j connected to i) are added in the block
! (ivar, jvar) of mat.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!   colMesh : CLASS(AbstractMesh_), intent(inout) - mesh of the columns
!   nodeToNode(:) : INTEGER(I4B), intent(in) - map from a node of obj to a
!     node of colMesh, SIZE(nodeToNode) = GetMaxNodeNumber()
!   mat : TYPE(CSRMatrix_), intent(inout) - sparse matrix
!   ivar : INTEGER(I4B), intent(in) - physical variable of the rows
!   jvar : INTEGER(I4B), intent(in) - physical variable of the columns
!
! See also: [[obj_SetSparsity4]]

INTERFACE
  MODULE SUBROUTINE obj_SetSparsity3(obj, colMesh, nodeToNode, mat, &
                                     ivar, jvar)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
    CLASS(AbstractMesh_), INTENT(INOUT) :: colMesh
    !! mesh of the columns
    INTEGER(I4B), INTENT(IN) :: nodeToNode(:)
    !! map from a node of obj to a node of colMesh, SIZE(nodeToNode) =
    !! GetMaxNodeNumber()
    TYPE(CSRMatrix_), INTENT(INOUT) :: mat
    !! sparse matrix
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable of the rows
    INTEGER(I4B), INTENT(IN) :: jvar
    !! physical variable of the columns
  END SUBROUTINE obj_SetSparsity3
END INTERFACE

!----------------------------------------------------------------------------
!                                                      SetSparsity@SetMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Set the sparsity pattern between two meshes with number maps
!
! Like SetSparsity3; rows and columns are mapped by rowGlobalToLocalNodeNum
! and colGlobalToLocalNodeNum.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!   colMesh : CLASS(AbstractMesh_), intent(inout) - mesh of the columns
!   nodeToNode(:) : INTEGER(I4B), intent(in) - map from a node of obj to a
!     node of colMesh
!   mat : TYPE(CSRMatrix_), intent(inout) - sparse matrix
!   rowGlobalToLocalNodeNum(rowLBOUND:rowUBOUND) : INTEGER(I4B), intent(in) -
!     map from global node to row number
!   rowLBOUND : INTEGER(I4B), intent(in) - lower bound of
!     rowGlobalToLocalNodeNum
!   rowUBOUND : INTEGER(I4B), intent(in) - upper bound of
!     rowGlobalToLocalNodeNum
!   colGlobalToLocalNodeNum(colLBOUND:colUBOUND) : INTEGER(I4B), intent(in) -
!     map from global node to column number
!   colLBOUND : INTEGER(I4B), intent(in) - lower bound of
!     colGlobalToLocalNodeNum
!   colUBOUND : INTEGER(I4B), intent(in) - upper bound of
!     colGlobalToLocalNodeNum
!   ivar : INTEGER(I4B), intent(in) - physical variable of the rows
!   jvar : INTEGER(I4B), intent(in) - physical variable of the columns
!
! See also: [[obj_SetSparsity3]]

INTERFACE
  MODULE SUBROUTINE obj_SetSparsity4(obj, colMesh, nodeToNode, mat, &
                                     rowGlobalToLocalNodeNum, rowLBOUND, &
                                     rowUBOUND, colGlobalToLocalNodeNum, &
                                     colLBOUND, colUBOUND, ivar, jvar)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
    CLASS(AbstractMesh_), INTENT(INOUT) :: colMesh
    !! mesh of the columns
    INTEGER(I4B), INTENT(IN) :: nodeToNode(:)
    !! map from a node of obj to a node of colMesh
    TYPE(CSRMatrix_), INTENT(INOUT) :: mat
    !! sparse matrix
    INTEGER(I4B), INTENT(IN) :: rowLBOUND
    !! lower bound of rowGlobalToLocalNodeNum
    INTEGER(I4B), INTENT(IN) :: rowUBOUND
    !! upper bound of rowGlobalToLocalNodeNum
    INTEGER(I4B), INTENT(IN) :: rowGlobalToLocalNodeNum( &
                                                         rowLBOUND:rowUBOUND)
    !! map from global node to row number
    INTEGER(I4B), INTENT(IN) :: colLBOUND
    !! lower bound of colGlobalToLocalNodeNum
    INTEGER(I4B), INTENT(IN) :: colUBOUND
    !! upper bound of colGlobalToLocalNodeNum
    INTEGER(I4B), INTENT(IN) :: colGlobalToLocalNodeNum( &
                                                         colLBOUND:colUBOUND)
    !! map from global node to column number
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable of the rows
    INTEGER(I4B), INTENT(IN) :: jvar
    !! physical variable of the columns
  END SUBROUTINE obj_SetSparsity4
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetMaterial@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! update:
!   - 2026-10-23: Set the number of media to n (it was increased by n)
! summary: Set the number of media of an element
!
! SetTotalMedium sets the number of media in the elements to n. The material
! ids of the first MIN(n, old number) media are kept, and new media get
! material id 0. SetMaterial sets the material id of a medium in all
! elements, in the elements of an entity, or in one element. GetMaterial and
! GetTotalMaterial read them.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!   n : INTEGER(I4B), intent(in) - number of media
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, dim=2)
!
!   CALL obj%SetTotalMedium(n=2_I4B)
!   CALL obj%SetMaterial(medium=1_I4B, material=7_I4B)
!   CALL obj%SetMaterial(medium=2_I4B, material=4_I4B)
!
!   ! more media in element 14
!   CALL obj%SetTotalMedium(n=3_I4B, globalElement=14_I4B)
!   CALL OK(obj%GetTotalMaterial(14_I4B) .EQ. 3_I4B, "element 14: 3 media")
!   CALL OK(obj%GetTotalMaterial(13_I4B) .EQ. 2_I4B, "element 13: 2 media")
!   CALL OK(obj%GetMaterial(medium=1_I4B, globalElement=14_I4B) &
!           .EQ. 7_I4B, "medium 1 is kept")
!   CALL OK(obj%GetMaterial(medium=2_I4B, globalElement=14_I4B) &
!           .EQ. 4_I4B, "medium 2 is kept")
!   CALL OK(obj%GetMaterial(medium=3_I4B, globalElement=14_I4B) &
!           .EQ. 0_I4B, "new medium 3 is 0")
!
!   ! the same n again changes nothing
!   CALL obj%SetTotalMedium(n=2_I4B)
!   CALL OK(obj%GetTotalMaterial(13_I4B) .EQ. 2_I4B, "same n: 2 media")
!   CALL OK(obj%GetMaterial(medium=2_I4B, globalElement=13_I4B) &
!           .EQ. 4_I4B, "same n: medium 2 is kept")
!
!   ! a smaller n removes the last media
!   CALL obj%SetTotalMedium(n=1_I4B)
!   CALL OK(obj%GetTotalMaterial(14_I4B) .EQ. 1_I4B, "smaller n: 1 medium")
!   CALL OK(obj%GetMaterial(medium=1_I4B, globalElement=14_I4B) &
!           .EQ. 7_I4B, "smaller n: medium 1 is kept")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_SetMaterial1]]

INTERFACE
  MODULE SUBROUTINE obj_SetTotalMedium1(obj, n, globalElement, islocal)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: n
    !! number of media
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
  END SUBROUTINE obj_SetTotalMedium1
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetMaterial@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! update:
!   - 2026-10-23: Set the number of media to n (it was increased by n)
! summary: Set the number of media of all elements
!
! SetTotalMedium sets the number of media in the elements to n. The material
! ids of the first MIN(n, old number) media are kept, and new media get
! material id 0. SetMaterial sets the material id of a medium in all
! elements, in the elements of an entity, or in one element. GetMaterial and
! GetTotalMaterial read them.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!   n : INTEGER(I4B), intent(in) - number of media
!
! See also: [[obj_SetTotalMedium1]], [[obj_SetMaterial1]]

INTERFACE
  MODULE SUBROUTINE obj_SetTotalMedium2(obj, n)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: n
    !! number of media
  END SUBROUTINE obj_SetTotalMedium2
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetMaterial@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-09
! summary: Set the material of a medium in an entity
!
! SetTotalMedium sets the number of media in the elements to n. The material
! ids of the first MIN(n, old number) media are kept, and new media get
! material id 0. SetMaterial sets the material id of a medium in all
! elements, in the elements of an entity, or in one element. GetMaterial and
! GetTotalMaterial read them. This routine sets the material in the elements
! of entityNum.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!   entityNum : INTEGER(I4B), intent(in) - entity number (mesh id) of the
!     elements
!   medium : INTEGER(I4B), intent(in) - medium number (for example soil,
!     water)
!   material : INTEGER(I4B), intent(in) - material id of the medium
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, DIM=2)
!
!   CALL obj%SetTotalMedium(n=2_I4B)
!   CALL OK(obj%GetTotalMaterial(13_I4B) .EQ. 2_I4B, "GetTotalMaterial")
!
!   ! all elements
!   CALL obj%SetMaterial(medium=1_I4B, material=7_I4B)
!   ! elements of entity 1
!   CALL obj%SetMaterial(entityNum=1_I4B, medium=2_I4B, material=3_I4B)
!   ! element 16 only
!   CALL obj%SetMaterial(medium=2_I4B, material=5_I4B, &
!     globalElement=16_I4B, islocal=.FALSE.)
!
!   CALL OK(obj%GetMaterial(medium=1_I4B, globalElement=13_I4B) &
!     .EQ. 7_I4B, "medium 1")
!   CALL OK(obj%GetMaterial(medium=2_I4B, globalElement=13_I4B) &
!     .EQ. 3_I4B, "medium 2 of element 13")
!   CALL OK(obj%GetMaterial(medium=2_I4B, globalElement=16_I4B) &
!     .EQ. 5_I4B, "medium 2 of element 16")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_SetMaterial2]], [[obj_SetMaterial3]],
!   [[obj_SetTotalMedium1]], [[obj_GetMaterial1]],
!   [[obj_GetTotalMaterial1]]

INTERFACE
  MODULE SUBROUTINE obj_SetMaterial1(obj, entityNum, medium, material)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: entityNum
    !! entity number (mesh id) of the elements
    INTEGER(I4B), INTENT(IN) :: medium
    !! medium number (for example soil, water)
    INTEGER(I4B), INTENT(IN) :: material
    !! material id of the medium
  END SUBROUTINE obj_SetMaterial1
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetMaterial@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Set the material of a medium in all elements
!
! See SetMaterial.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!   medium : INTEGER(I4B), intent(in) - medium number
!   material : INTEGER(I4B), intent(in) - material id
!
! See also: [[obj_SetMaterial1]]

INTERFACE
  MODULE SUBROUTINE obj_SetMaterial2(obj, medium, material)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: medium
    !! medium number
    INTEGER(I4B), INTENT(IN) :: material
    !! material id
  END SUBROUTINE obj_SetMaterial2
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetMaterial@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-09
! summary: Set the material of a medium in an element
!
! See SetMaterial.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!   medium : INTEGER(I4B), intent(in) - medium number
!   material : INTEGER(I4B), intent(in) - material id
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   islocal : LOGICAL(LGT), intent(in) - if true, globalElement is a local
!     element number
!
! See also: [[obj_SetMaterial1]]

INTERFACE
  MODULE SUBROUTINE obj_SetMaterial3(obj, medium, material, globalElement, &
                                     islocal)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: medium
    !! medium number
    INTEGER(I4B), INTENT(IN) :: material
    !! material id
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, globalElement is a local element number
  END SUBROUTINE obj_SetMaterial3
END INTERFACE

!----------------------------------------------------------------------------
!                                            SetFacetElementType@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Set the element type of an element
!
! Set the element type of an element to facetElementType (INTERNAL_ELEMENT,
! BOUNDARY_ELEMENT or DOMAIN_BOUNDARY_ELEMENT). IsBoundaryElement,
! IsDomainBoundaryElement and GetTotalBoundaryElements read it. After Initiate
! every boundary element is a DOMAIN_BOUNDARY_ELEMENT. iface is kept for
! compatibility; it is not used.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   iface : INTEGER(I4B), intent(in) - local facet number; not used
!   facetElementType : INTEGER(I4B), intent(in) - element type:
!     INTERNAL_ELEMENT, BOUNDARY_ELEMENT or DOMAIN_BOUNDARY_ELEMENT
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ElemData_Class, ONLY: BOUNDARY_ELEMENT
!   USE ElemData_Class, ONLY: INTERNAL_ELEMENT
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "./meshdata/very_small_quad4_mesh.h5"
!   TYPE(FEMesh_) :: obj
!   TYPE(HDF5File_) :: meshfile
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, dim=2)
!
!   CALL OK(obj%IsDomainBoundaryElement(13_I4B), "after Initiate")
!
!   CALL obj%SetFacetElementType(globalElement=13_I4B, iface=1_I4B, &
!                                facetElementType=BOUNDARY_ELEMENT)
!   CALL OK(obj%IsBoundaryElement(13_I4B), "BOUNDARY_ELEMENT")
!   CALL OK(.NOT. obj%IsDomainBoundaryElement(13_I4B), &
!           "not a domain boundary element")
!
!   CALL obj%SetFacetElementType(globalElement=1_I4B, iface=1_I4B, &
!                                facetElementType=INTERNAL_ELEMENT, &
!                                islocal=.TRUE.)
!   CALL OK(.NOT. obj%IsBoundaryElement(13_I4B), "INTERNAL_ELEMENT")
!   CALL OK(obj%GetTotalBoundaryElements() .EQ. 3_I4B, &
!           "GetTotalBoundaryElements")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_isBoundaryElement]], [[obj_isDomainBoundaryElement]]

INTERFACE
  MODULE SUBROUTINE obj_SetFacetElementType(obj, globalElement, &
                                            iface, facetElementType, islocal)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    INTEGER(I4B), INTENT(IN) :: iface
    !! local facet number; not used
    INTEGER(I4B), INTENT(IN) :: facetElementType
    !! element type: INTERNAL_ELEMENT, BOUNDARY_ELEMENT or
    !! DOMAIN_BOUNDARY_ELEMENT
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
  END SUBROUTINE obj_SetFacetElementType
END INTERFACE

!----------------------------------------------------------------------------
!                                                   SetQuality@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Set the quality of the mesh
!
! TODO: This routine needs to be implemented. At present it raises an
! error in debug builds (DEBUG_VER) and does nothing otherwise; the output
! arguments are not set.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!   measures(:) : INTEGER(I4B), intent(in) - quality measures
!   max_measures(:) : REAL(DFP), intent(out) - maximum value of each measure
!   min_measures(:) : REAL(DFP), intent(out) - minimum value of each measure
!   nodeCoord(:,:) : REAL(DFP), intent(in) - node coordinates in xiJ format
!   local_nptrs(:) : INTEGER(I4B), intent(in) - map from global node number to
!     a column of nodeCoord

INTERFACE
  MODULE SUBROUTINE obj_SetQuality(obj, measures, max_measures, &
                                   min_measures, nodeCoord, local_nptrs)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: measures(:)
    !! quality measures
    REAL(DFP), INTENT(OUT) :: max_measures(:)
    !! maximum value of each measure
    REAL(DFP), INTENT(OUT) :: min_measures(:)
    !! minimum value of each measure
    REAL(DFP), INTENT(IN) :: nodeCoord(:, :)
    !! node coordinates in xiJ format
    INTEGER(I4B), INTENT(IN) :: local_nptrs(:)
    !! map from global node number to a column of nodeCoord
  END SUBROUTINE obj_SetQuality
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetParam@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-05-21
! summary: Set the parameters of the mesh
!
! Each present argument sets the component of the same name. See GetParam.
!
! Arguments:
!   obj : CLASS(AbstractMesh_) - mesh object
!   isInitiated, isNodeToElementsInitiated, isNodeToNodesInitiated,
!     isExtraNodeToNodesInitiated, isElementToElementsInitiated,
!     isBoundaryDataInitiated, isFacetDataInitiated : LOGICAL(LGT), optional -
!     status flags
!   uid, xidim, elemType, nsd, maxNptrs, minNptrs, maxElemNum, minElemNum,
!     tNodes, tElements, tElemTopologies : INTEGER(I4B), optional - scalar
!     parameters
!   tElements_topology_wise(8), elemTopologies(8) : INTEGER(I4B), optional -
!     elements and names of each topology
!   minX, minY, minZ, maxX, maxY, maxZ, x, y, z : REAL(DFP), optional -
!     bounding box and centroid
!
! See also: [[obj_GetParam]]

INTERFACE
  MODULE SUBROUTINE obj_SetParam(obj, isInitiated, &
                                 isNodeToElementsInitiated, &
                                 isNodeToNodesInitiated, &
                                 isExtraNodeToNodesInitiated, &
                                 isElementToElementsInitiated, &
                                 isBoundaryDataInitiated, &
                                 isFacetDataInitiated, uid, xidim, elemType, &
                                 nsd, maxNptrs, minNptrs, maxElemNum, &
                                 minElemNum, tNodes, tElements, minX, minY, &
                                 minZ, maxX, maxY, maxZ, x, y, z, &
                                 tElements_topology_wise, tElemTopologies, &
                                 elemTopologies)

    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: isInitiated, &
      isNodeToElementsInitiated, &
      isNodeToNodesInitiated, &
      isExtraNodeToNodesInitiated, &
      isElementToElementsInitiated, &
      isBoundaryDataInitiated, &
      isFacetDataInitiated
    !! status flags of the mesh
    INTEGER(I4B), INTENT(IN), OPTIONAL :: uid, xidim, elemType, nsd, &
      maxNptrs, minNptrs, maxElemNum, &
      minElemNum, tNodes, tElements, &
      tElements_topology_wise(8), &
      tElemTopologies, elemTopologies(8)
    !! scalar parameters of the mesh
    REAL(DFP), INTENT(IN), OPTIONAL :: minX, minY, minZ, maxX, maxY, maxZ, &
      x, y, z
    !! bounding box and centroid of the mesh
  END SUBROUTINE obj_SetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                          SetBoundaryFacetParam@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-14
! summary: Set the parameters of a facet element
!
! Set the facet element type of a facet element.
!
! Arguments:
!   obj : CLASS(AbstractMesh_), intent(inout) - mesh object
!   facetElement : INTEGER(I4B), intent(in) - facet element number
!   elementType : INTEGER(I4B), optional, intent(in) - facet element type
!
! See also: [[obj_GetFacetParam]]

INTERFACE
  MODULE SUBROUTINE obj_SetFacetParam(obj, facetElement, elementType)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
    INTEGER(I4B), INTENT(IN) :: facetElement
    !! facet element number
    INTEGER(I4B), INTENT(IN), OPTIONAL :: elementType
    !! facet element type
  END SUBROUTINE obj_SetFacetParam
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE AbstractMesh_Class
