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

!> authors: Vikas Sharma, Ph. D.
! date: 25 March 2021
! summary: [[Mesh_Class]] module contains data type for handling the mesh.

MODULE Mesh_Class
USE GlobalData
USE BaseType
USE String_Class, ONLY: String
USE ElementFactory
USE ExceptionHandler_Class, ONLY: e
USE FPL, ONLY: ParameterList_
USE HDF5File_Class
USE VTKFile_Class
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "Mesh_Class"
INTEGER(I4B), PARAMETER, PUBLIC :: INTERNAL_NODE = 1
INTEGER(I4B), PARAMETER, PUBLIC :: BOUNDARY_NODE = -1
INTEGER(I4B), PARAMETER, PUBLIC :: DOMAIN_BOUNDARY_NODE = -2
INTEGER(I4B), PARAMETER, PUBLIC :: GHOST_NODE = -3
INTEGER(I4B), PARAMETER, PUBLIC :: INTERNAL_ELEMENT = 1
INTEGER(I4B), PARAMETER, PUBLIC :: BOUNDARY_ELEMENT = -1
INTEGER(I4B), PARAMETER, PUBLIC :: DOMAIN_BOUNDARY_ELEMENT = -2
INTEGER(I4B), PARAMETER, PUBLIC :: GHOST_ELEMENT = -4

!----------------------------------------------------------------------------
!                                                                 NodeData_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 May 2022
! summary: Data type for storing node data

TYPE :: NodeData_
  INTEGER(I4B) :: globalNodeNum = 0
    !! Global node number
  INTEGER(I4B) :: localNodeNum = 0
    !! local node number
  INTEGER(I4B) :: nodeType = INTERNAL_NODE
    !! node type; INTERNAL_NODE, BOUNDARY_NODE, DOMAIN_BOUNDARY_NODE
  INTEGER(I4B), ALLOCATABLE :: globalNodes(:)
    !! It contains the global node number surrouding the node
    !! It does not contain self global node number
  INTEGER(I4B), ALLOCATABLE :: globalElements(:)
    !! It contains the global element number surrounding the node
  INTEGER(I4B), ALLOCATABLE :: extraGlobalNodes(:)
    !! These global nodes required in facet-element-data
CONTAINS
  PROCEDURE, PUBLIC, PASS(obj) :: Display => nodeData_Display
    !! Display the content of an instance of NodeData
END TYPE NodeData_

!----------------------------------------------------------------------------
!                                                                 ElemData_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 May 2022
! summary: Data type for storing element data

TYPE :: ElemData_
  INTEGER(I4B) :: globalElemNum = 0
    !! global element number
  INTEGER(I4B) :: localElemNum = 0
    !! local element number
  INTEGER(I4B) :: elementType = INTERNAL_ELEMENT
    !! BOUNDARY_ELEMENT: If the element contqains the boundary node
    !! it will be called the boundary element
    !! INTERNAL_ELEMENT: If the element does not contain the boundary node
    !! then it will be called the internal element
  INTEGER(I4B), ALLOCATABLE :: globalNodes(:)
    !! nodes contained in the element, connectivity
  INTEGER(I4B), ALLOCATABLE :: globalElements(:)
    !! Contains the information about the element surrounding an element
    !! Lets us say that `globalElem1`, `globalElem2`, `globalElem3`
    !! surrounds a local element ielem (its global element number is
    !! globalElem), then globalElements( [1,2,3] ) contains globalElem1,
    !! pFace, nFace, globalElements( [4,5,6] ) contains globalElem2,
    !! pFace, nFace, globalElements( [7,8,9] ) contains globalElem3,
    !! pFace, nFace.
    !! Here, pFace is the local facet number of parent element
    !! globalElem (ielem) which is connected to the nFace of the neighbor
    !! element
    !! All element numbers are global element number
  INTEGER(I4B), ALLOCATABLE :: boundaryData(:)
    !! If `iel` is boundary element, then boudnaryData contains
    !! the local facet number of iel which concides with the
    !! mesh boundary.
    !! If an element contains the boundary node then it is considered
    !! as a boundary element.
    !! It may happen that a boundary element has no boundary face, in which
    !! case boundaryData will have zero size
CONTAINS
  !!
  !! Contains
  !!
  PROCEDURE, PUBLIC, PASS(obj) :: Display => elemData_Display
  !!
END TYPE ElemData_

!----------------------------------------------------------------------------
!                                                         InternalFacetData_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 May 2022
! summary: Data storage for internal facets of mesh

TYPE InternalFacetData_
  INTEGER(I4B) :: masterCellNumber = 0
    !! master cell nubmer
  INTEGER(I4B) :: slaveCellNumber = 0
    !! slave cell number
  INTEGER(I4B) :: masterLocalFacetID = 0
    !! local facet ID in master cell
  INTEGER(I4B) :: slaveLocalFacetID = 0
    !! slave facet ID in slave cell
CONTAINS
  !!
  !! Contains
  !!
  PROCEDURE, PUBLIC, PASS(obj) :: Display => InternalFacetData_Display
  !! Display the content of an instance of InternalFacetData_
END TYPE InternalFacetData_

!----------------------------------------------------------------------------
!                                                                 FacetData_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 May 2022
! summary: Data storage for Domain facet data
!
!# Introduction
!
! DomainFacet elements are those boundary elements which are located
! on the boundary of the domain. Ofcourse domainFacet elements are
! located on the mesh Boundary with only difference that these elements do
! not have slaveCellNumber

TYPE BoundaryFacetData_
  INTEGER(I4B) :: masterCellNumber = 0
  INTEGER(I4B) :: masterLocalFacetID = 0
  INTEGER(I4B) :: elementType = 0
CONTAINS
  !!
  !! Contains
  !!
  PROCEDURE, PUBLIC, PASS(obj) :: Display => BoundaryFacetData_Display
  !!
END TYPE BoundaryFacetData_

! PUBLIC :: BoundaryFacetData_

!----------------------------------------------------------------------------
!                                                                     Mesh_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 13 June 2021
! summary: This datatype contains the meta data of a mesh
!
!{!pages/Mesh_.md!}

TYPE :: Mesh_
  PRIVATE
  LOGICAL(LGT) :: readFromFile = .TRUE.
    !! True if the mesh is read from a file
  LOGICAL(LGT) :: isInitiated = .FALSE.
    !! logical flag denoting for whether mesh data is initiated or not
  LOGICAL(LGT) :: isNodeToElementsInitiated = .FALSE.
    !! Node to elements mapping
  LOGICAL(LGT) :: isNodeToNodesInitiated = .FALSE.
    !! Node to nodes mapping
  LOGICAL(LGT) :: isExtraNodeToNodesInitiated = .FALSE.
    !! Node to nodes mapping
  LOGICAL(LGT) :: isElementToElementsInitiated = .FALSE.
    !! Element to elements mapping
  LOGICAL(LGT) :: isBoundaryDataInitiated = .FALSE.
    !! Boundary data
  LOGICAL(LGT), PUBLIC :: isFacetDataInitiated = .FALSE.
    !! FacetData
  INTEGER(I4B) :: uid = 0
    !! Unique id of the mesh
  INTEGER(I4B) :: xidim = 0
    !! xidimension of elements present inside the mesh
  INTEGER(I4B) :: elemType = 0
    !! type of element present inside the mesh
  INTEGER(I4B) :: nsd = 0
    !! number of spatial dimension of the mesh
  INTEGER(I4B), PUBLIC :: maxNptrs = 0
    !! largest node number present inside the mesh
  INTEGER(I4B), PUBLIC :: minNptrs = 0
    !! minimum node number present inside the mesh
  INTEGER(I4B), PUBLIC :: maxElemNum = 0
    !! largest element number present inside the mesh
  INTEGER(I4B), PUBLIC :: minElemNum = 0
    !! minimum element number present inside the mesh
  INTEGER(I4B) :: tNodes = 0
    !! total number of nodes present inside the mesh
  INTEGER(I4B) :: tIntNodes = 0
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
  REAL(DFP) :: X = 0.0
    !! x coorindate of centroid
  REAL(DFP) :: Y = 0.0
    !! y coordinate of centroid
  REAL(DFP) :: Z = 0.0
    !! z coordinate of centroid
  INTEGER(I4B), ALLOCATABLE :: physicalTag(:)
    !! Physical entities associated with the current entity (mesh)
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
  INTEGER(I4B), ALLOCATABLE :: material(:)
    !! materials mapped to the mesh
    !! material(1) is the material id of medium 1
    !! material(2) is the material id of medium 2
    !! ...
    !! material(n) is the material id of medium n
    !!
    !! For example, soil is a porous medium n = 1,
    !! fluid is a medium n =2
    !! then material(1) denotes the type of soil => clay, sand, silt
    !! and material(2) denotes the type of fluid, water, oil, air
  TYPE(ReferenceElement_), PUBLIC, ALLOCATABLE :: facetElements(:)
    !! Facet Elements in the reference element
  INTEGER(I4B), ALLOCATABLE :: facetElementType(:, :)
  !! Number of rows of this array is same as the total number of
  !! facets present in the mesh-reference elements
  !! Number of columns of this array is equal to the total number of
  !! elements inside the mesh
  !! facetElementType(ii, iel) can be
  !! INTERNAL_ELEMENT, BOUNDARY_ELEMENT, DOMAIN_BOUNDARY_ELEMENT
  !! If the face is a part of the mesh boundary then it will be called
  !! the BOUNDARY_ELEMENT
  TYPE(NodeData_), ALLOCATABLE :: nodeData(:)
    !! Node data
  TYPE(ElemData_), ALLOCATABLE :: elementData(:)
    !! element data
  TYPE(InternalFacetData_), PUBLIC, ALLOCATABLE :: internalFacetData(:)
    !! Internal facet data
  TYPE(BoundaryFacetData_), PUBLIC, ALLOCATABLE :: boundaryFacetData(:)
    !! Domain Facet Data
  CLASS(ReferenceElement_), PUBLIC, POINTER :: refelem => NULL()
    !! Reference element of the mesh (spatial)
  INTEGER(I4B), PUBLIC :: ipType = Equidistance
    !! interpolation point type
  !!
  !! Following variables are required during processing.
  !!
  !! time
  !!
  TYPE(QuadraturePoint_), PUBLIC :: quadForTime
    !! quadrature point for time domain #STFEM
  TYPE(ElemshapeData_), PUBLIC :: linTimeElemSD
    !! Element shape data on linear time element #STFEM
  TYPE(ElemshapeData_), PUBLIC :: timeElemSD
    !! Element shape data on time element #STFEM
  TYPE(String) :: quadTypeForTime
    !! quadrature type for time
  TYPE(String) :: continuityTypeForTime
    !! continuity of base function for time
  TYPE(String) :: interpolTypeForTime
    !! interpolation of base function for time
  INTEGER(I4B) :: orderTime
    !! order for time
  !!
  !! space (cell)
  !!
  TYPE(QuadraturePoint_), PUBLIC :: quadForSpace
    !! quadrature point for space
  TYPE(ElemshapeData_), PUBLIC :: linSpaceElemSD
    !! Element shape data on linear space (simplex) element
  TYPE(ElemshapeData_), PUBLIC :: spaceElemSD
    !! Element shape data on space element
  TYPE(STElemshapeData_), ALLOCATABLE, PUBLIC :: stelemsd(:)
    !! Element shape data on space element
  TYPE(String) :: quadTypeForSpace
    !! quadrature type for space
  TYPE(String) :: continuityTypeForSpace
    !! continuity of base function for space
  TYPE(String) :: interpolTypeForSpace
    !! interoplation type of base function for space
  INTEGER(I4B) :: orderSpace
    !! order for space
  !!
  !! space (facets)
  !!
  TYPE(QuadraturePoint_), ALLOCATABLE, PUBLIC :: quadForFacet(:)
    !! quadrature point for facet elements
  TYPE(QuadraturePoint_), ALLOCATABLE, PUBLIC :: quadForFacetCell(:)
    !! quadrature point for facet-cell elements
  TYPE(ElemshapeData_), ALLOCATABLE, PUBLIC :: linFacetElemSD(:)
    !! Element shape data on linear facet (simplex) element
  TYPE(ElemshapeData_), ALLOCATABLE, PUBLIC :: linFacetCellElemSD(:)
    !! Element shape data on linear facet (simplex) cell element
  TYPE(ElemshapeData_), ALLOCATABLE, PUBLIC :: facetElemSD(:)
    !! Element shape data on facet element
  TYPE(ElemshapeData_), ALLOCATABLE, PUBLIC :: facetCellElemSD(:)
    !! Element shape data on facet cell element
  TYPE(String) :: quadTypeForFacet
    !! quadrature type for facet element
  TYPE(String) :: continuityTypeForFacet
    !! continuity of base function for facet element
  TYPE(String) :: interpolTypeForFacet
    !! interoplation type of base function for facet element
  INTEGER(I4B) :: orderFacet
    !! order for facet element
  TYPE(STElemshapeData_), ALLOCATABLE, PUBLIC :: facetSTelemsd(:, :)
    !! Element shape data on facet element
  !!
CONTAINS
  PRIVATE
  !!
  !! @ConstructorMethods
  !!
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => mesh_initiate
    !! Allocate size of a mesh
  FINAL :: mesh_final
    !! mesh finalizer
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => mesh_Deallocate
    !! Deallocate memory occupied by the mesh instance
  !!
  !! @IOMethods
  !!
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => mesh_Import
    !! Read mesh from hdf5 file
  PROCEDURE, PUBLIC, PASS(obj) :: getNodeCoord => mesh_getNodeCoord
    !! Read the nodeCoords from the hdf5file
  PROCEDURE, PUBLIC, PASS(obj) :: Export => mesh_Export
    !! Export mesh to an hdf5 file
  PROCEDURE, PUBLIC, PASS(obj) :: ExportToVTK => mesh_ExportToVTK
    !! Export mesh to a VTKfile
  PROCEDURE, PUBLIC, PASS(obj) :: Display => mesh_display
    !! Display the mesh
  PROCEDURE, PUBLIC, PASS(obj) :: DisplayNodeData => &
  & mesh_DisplayNodeData
  !! Display node data
  PROCEDURE, PUBLIC, PASS(obj) :: DisplayElementData => &
    & mesh_DisplayElementData
    !! Display element data
  PROCEDURE, PUBLIC, PASS(obj) :: DisplayInternalFacetData => &
    & mesh_DisplayInternalFacetData
    !! Display internal facet data
  PROCEDURE, PUBLIC, PASS(obj) :: DisplayBoundaryFacetData => &
    & mesh_DisplayBoundaryFacetData
    !! Display mesh facet data
  PROCEDURE, PUBLIC, PASS(obj) :: DisplayFacetElemSD => &
    & mesh_DisplayFacetElemSD
    !! Display facet element shape data
  PROCEDURE, PUBLIC, PASS(obj) :: DisplayFacetElements => &
    & mesh_DisplayFacetElements
  !
  !@NodeDataMethods
  !
  PROCEDURE, PUBLIC, PASS(obj) :: InitiateNodeToElements => &
    & mesh_InitiateNodeToElements
  !! Initiate node to node data
  PROCEDURE, PUBLIC, PASS(obj) :: InitiateNodeToNodes => &
    & mesh_InitiateNodetoNodes
  !! Initiate Node to nodes mapping
  PROCEDURE, PUBLIC, PASS(obj) :: InitiateExtraNodeToNodes => &
    & mesh_InitiateExtraNodetoNodes
  !! Initiate Node to nodes mapping
  !
  !@ElementDataMethods
  !
  PROCEDURE, PUBLIC, PASS(obj) :: InitiateElementToElements => &
    & mesh_InitiateElementToElements
  !! Initiate element to elements mapping
  !
  !@BoundaryDataMethods
  !
  PROCEDURE, PUBLIC, PASS(obj) :: InitiateBoundaryData => &
    & mesh_InitiateBoundaryData
  !
  !@FacetDataMethods
  !
  !! Initiate boundary data
  PROCEDURE, PUBLIC, PASS(obj) :: InitiateFacetElements => &
    & mesh_InitiateFacetElements
  !!
  PROCEDURE, PUBLIC, PASS(obj) :: isBoundaryNode => &
    & mesh_isBoundaryNode
  !! Returns true if a given global node number is a boundary node
  PROCEDURE, PUBLIC, PASS(obj) :: isBoundaryElement => &
    & mesh_isBoundaryElement
  !! Returns true if a given global element number is a boundary element
  PROCEDURE, PUBLIC, PASS(obj) :: isDomainBoundaryElement => &
    & mesh_isDomainBoundaryElement
  !! Returns true if a given global element number is a boundary element
  PROCEDURE, PUBLIC, PASS(obj) :: isDomainFacetElement => &
    & mesh_isDomainFacetElement
  !! Returns true if a given global element number is a boundary element
  PROCEDURE, PUBLIC, PASS(obj) :: isNodePresent => &
    & mesh_isNodePresent
  !! Returns true if a node number is present
  PROCEDURE, PUBLIC, PASS(obj) :: isAnyNodePresent => &
    & mesh_isAnyNodePresent
  !! Returns true if any of the node number is present
  PROCEDURE, PUBLIC, PASS(obj) :: isAllNodePresent => &
    & mesh_isAllNodePresent
  !! Returns true if All of the node number is present
  PROCEDURE, PUBLIC, PASS(obj) :: isElementPresent => &
    & mesh_isElementPresent
  !! Returns true if a given element number is present
  PROCEDURE, PUBLIC, PASS(obj) :: Size => mesh_size
  !! Returns the size of the mesh (total number of elements)
  PROCEDURE, PUBLIC, PASS(obj) :: getElemNum => mesh_getElemNum
  !! returns global element number in the mesh
  PROCEDURE, PUBLIC, PASS(obj) :: getRefElemPointer =>  &
    & mesh_getRefElemPointer
  !! Returns pointer to the reference element
  PROCEDURE, PUBLIC, PASS(obj) :: getTotalElements => mesh_size
  !! Returns the size of the mesh
  PROCEDURE, PUBLIC, PASS(obj) :: getBoundingEntity => &
    & mesh_getBoundingEntity
  !! Returns the nodal coordinates
  PROCEDURE, PUBLIC, PASS(obj) :: getNptrs => mesh_getNptrs
  !! Returns the node number of mesh
  PROCEDURE, PUBLIC, PASS(obj) :: getInternalNptrs => &
    & mesh_getInternalNptrs
  !! Returns a vector of internal node numbers
  PROCEDURE, PUBLIC, PASS(obj) :: getBoundaryNptrs => &
    & mesh_getBoundaryNptrs
  !! Returns a vector of boundary node numbers
  PROCEDURE, PUBLIC, PASS(obj) :: getTotalInternalNodes => &
    & mesh_getTotalInternalNodes
  !! Returns the total number of internal nodes
  PROCEDURE, PUBLIC, PASS(obj) :: getTotalNodes => mesh_getTotalNodes
  !! Returns the total number of nodes
  PROCEDURE, PUBLIC, PASS(obj) :: getTotalBoundaryNodes =>  &
    & mesh_getTotalBoundaryNodes
  !! Returns the total number of boundary nodes
  PROCEDURE, PUBLIC, PASS(obj) :: getTotalBoundaryElements => &
    & mesh_getTotalBoundaryElements
  !! Returns the total number of boundary element
  PROCEDURE, PASS(obj) :: getBoundingBox1 => mesh_getBoundingBox1
  !! Returns the bounding box of the mesh
  PROCEDURE, PASS(obj) :: getBoundingBox2 => mesh_getBoundingBox2
  !! Return the bounding box from the given nodes, and local_nptrs
  GENERIC, PUBLIC :: getBoundingBox => getBoundingBox1,  &
    & getBoundingBox2
  !! Return the bounding box
  PROCEDURE, PUBLIC, PASS(obj) :: getConnectivity => &
    & mesh_getConnectivity
  !! Returns  node numbers in an element
  PROCEDURE, PASS(obj) :: mesh_getLocalNodeNumber1
  !! Returns the local node number of a glocal node number
  PROCEDURE, PASS(obj) :: mesh_getLocalNodeNumber2
  !! Returns the local node number of a global node number
  GENERIC, PUBLIC :: getLocalNodeNumber => mesh_getLocalNodeNumber1, &
    & mesh_getLocalNodeNumber2
  !! Returns the local node number of a global node number
  PROCEDURE, PASS(obj) :: mesh_getGlobalNodeNumber1
  !! Returns the global node number of a local node number
  PROCEDURE, PASS(obj) :: mesh_getGlobalNodeNumber2
  !! Returns the global node number of a local node number
  GENERIC, PUBLIC :: getGlobalNodeNumber => mesh_getGlobalNodeNumber1, &
    & mesh_getGlobalNodeNumber2
  PROCEDURE, PASS(obj) :: mesh_getGlobalElemNumber1
  PROCEDURE, PASS(obj) :: mesh_getGlobalElemNumber2
  GENERIC, PUBLIC :: getGlobalElemNumber => &
    & mesh_getGlobalElemNumber1, mesh_getGlobalElemNumber2
  !! Returns the global element number for a local element number
  PROCEDURE, PASS(obj) :: mesh_getLocalElemNumber1
  PROCEDURE, PASS(obj) :: mesh_getLocalElemNumber2
  GENERIC, PUBLIC :: getLocalElemNumber => &
    & mesh_getLocalElemNumber1, mesh_getLocalElemNumber2
  !! Returns the local element number of a global element number
  PROCEDURE, PASS(obj) :: mesh_getNodeToElements1
  PROCEDURE, PASS(obj) :: mesh_getNodeToElements2
  GENERIC, PUBLIC :: getNodeToElements => &
    & mesh_getNodeToElements1, &
    & mesh_getNodeToElements2
  !! Returns the element attached to a given global node number
  PROCEDURE, PASS(obj) :: mesh_getNodeToNodes1
  !! Returns global node number connected to a given global node
  PROCEDURE, PASS(obj) :: mesh_getNodeToNodes2
  !! Returns global node numbers connected to given global node numbers
  GENERIC, PUBLIC :: getNodeToNodes => &
    & mesh_getNodeToNodes1, &
    & mesh_getNodeToNodes2
  !! Returns nodes connected to a given node number
  PROCEDURE, PUBLIC, PASS(obj) :: getElementToElements => &
    & mesh_getElementToElements
  !! Returns local element number connected to a given local
  !! element number, it also gives information about the local
  !! facet number
  PROCEDURE, PUBLIC, PASS(obj) :: getBoundaryElementData => &
    & mesh_getBoundaryElementData
  !! Returns boundary element data
  PROCEDURE, PUBLIC, PASS(obj) :: getTotalFacetElements => &
    & mesh_getTotalFacetElements
  !! Returns the total number of facet elements in the mesh
  PROCEDURE, PUBLIC, PASS(obj) :: getTotalBoundaryFacetElements => &
    & mesh_getTotalBoundaryFacetElements
  !! Returns the total number of boundary facet elements
  PROCEDURE, PUBLIC, PASS(obj) :: getTotalInternalFacetElements => &
    & mesh_getTotalInternalFacetElements
  !! Returns the total number of internal facet elements
  PROCEDURE, PUBLIC, PASS(obj) :: getMasterCellNumber => &
    & mesh_getMasterCellNumber
  !! Returns the master cell number of a facet element
  PROCEDURE, PUBLIC, PASS(obj) :: getSlaveCellNumber => &
    & mesh_getSlaveCellNumber
  !! Returns the slave cell number of a facet element
  PROCEDURE, PUBLIC, PASS(obj) :: getCellNumber => &
    & mesh_getCellNumber
  !! Returns the master and slave cell number of a facet element
  PROCEDURE, PUBLIC, PASS(obj) :: getLocalFacetID => &
    & mesh_getLocalFacetID
  !! Return the local facet id, so that we can get reference element of
  !! the facet element
  PROCEDURE, PASS(obj) :: mesh_getFacetConnectivity1
  !! Return the node nubmers in the facet element
  PROCEDURE, PASS(obj) :: mesh_getFacetConnectivity2
  !! Return the node nubmers in the facet element of a cellElement
  GENERIC, PUBLIC :: getFacetConnectivity => &
    & mesh_getFacetConnectivity1, &
    & mesh_getFacetConnectivity2
  !! Generic method to get the connectivity of a facet element
  PROCEDURE, PUBLIC, PASS(obj) :: getFacetElementType => &
    & mesh_getFacetElementType
  !! Returns the facet element type of a given cell element number
  PROCEDURE, PUBLIC, PASS(obj) :: getOrder => &
    & mesh_getOrder
  !! Returns the order ofthe element of mesh
  PROCEDURE, PUBLIC, PASS(obj) :: getNSD => &
    & mesh_getNSD
  !! Return the NSD
  PROCEDURE, PUBLIC, PASS(obj) :: getXidimension => &
    & mesh_getXidimension
  !! Return the NSD
  PROCEDURE, PUBLIC, PASS(obj) :: getMaterial => mesh_getMaterial
  !! returns the material id of a given medium
  !
  ! @SetMethods
  !
  !! Returns the order of reference element
  PROCEDURE, PASS(obj) :: setBoundingBox1 => mesh_setBoundingBox1
  !! Set the bounding box of the mesh
  PROCEDURE, PASS(obj) :: setBoundingBox2 => mesh_setBoundingBox2
  !! Set the bounding box from the given nodes, and local_nptrs
  GENERIC, PUBLIC :: setBoundingBox => setBoundingBox1,  &
    & setBoundingBox2
  !! Set the bounding box
  PROCEDURE, PRIVATE, PASS(obj) :: setSparsity1 => mesh_setSparsity1
  PROCEDURE, PRIVATE, PASS(obj) :: setSparsity2 => mesh_setSparsity2
  PROCEDURE, PRIVATE, PASS(obj) :: setSparsity3 => mesh_setSparsity3
  PROCEDURE, PRIVATE, PASS(obj) :: setSparsity4 => mesh_setSparsity4
  GENERIC, PUBLIC :: setSparsity => setSparsity1, setSparsity2,  &
    & setSparsity3, setSparsity4
  PROCEDURE, PUBLIC, PASS(obj) :: setTotalMaterial => mesh_setTotalMaterial
  !! Adding a material ID of a medium which is mapped to the mesh
  PROCEDURE, PUBLIC, PASS(obj) :: setMaterial => mesh_setMaterial
  !! Adding a material ID of a medium which is mapped to the mesh
  PROCEDURE, PUBLIC, PASS(obj) :: setFacetElementType => &
    & mesh_setFacetElementType
  !! Set the facet element type of a given cell number
  !
  ! @ShapeDataMethods
  !
  PROCEDURE, PASS(obj) :: initiateElemSD1 => mesh_initiateElemSD1
  PROCEDURE, PASS(obj) :: initiateElemSD2 => mesh_initiateElemSD2
  PROCEDURE, PASS(obj) :: initiateElemSD3 => mesh_initiateElemSD3
  PROCEDURE, PASS(obj) :: initiateElemSD4 => mesh_initiateElemSD4
  GENERIC, PUBLIC :: initiateElemSD => &
    & initiateElemSD1, &
    & initiateElemSD2, &
    & initiateElemSD3, &
    & initiateElemSD4
  PROCEDURE, PASS(obj) :: initiateFacetElemSD1 => mesh_initiateFacetElemSD1
  PROCEDURE, PASS(obj) :: initiateFacetElemSD2 => mesh_initiateFacetElemSD2
  PROCEDURE, PASS(obj) :: initiateFacetElemSD3 => mesh_initiateFacetElemSD3
  GENERIC, PUBLIC :: initiateFacetElemSD => &
    & initiateFacetElemSD1, &
    & initiateFacetElemSD2, &
    & initiateFacetElemSD3
  !! Initiating local shape data for mesh
END TYPE Mesh_

!----------------------------------------------------------------------------
!                                                                     Mesh_
!----------------------------------------------------------------------------

PUBLIC :: Mesh_
! TYPE( Mesh_ ), PARAMETER, PUBLIC :: TypeMesh = Mesh_( )

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 Sept 2021
! summary: Userdefine datatype which contains the pointer to a mesh

TYPE :: MeshPointer_
  TYPE(Mesh_), POINTER :: Ptr => NULL()
END TYPE MeshPointer_

PUBLIC :: MeshPointer_

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 March 2021
! summary: Allocate the size of the mesh
!
!# Introduction
!
! This routine initiate the the mesh by reading the data stored inside
! the HDF5 file.
! It calls following routines
!
! - obj%import()
! - obj%InitiateLocalNptrs()
! - obj%InitiateLocalElementNumbers()
! - obj%InitiateNodeToElements()
! - obj%InitiateNodeToNodes()
! - obj%InitiateElementToElements()
! - obj%InitiateBoundaryData()

INTERFACE
  MODULE SUBROUTINE mesh_initiate(obj, hdf5, group)
    CLASS(Mesh_), INTENT(INOUT) :: obj
    !! mesh object
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! Mesh file
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE mesh_initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Mesh@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 March 2021
! summary: Function for constructing an instance of [[Mesh_]]

INTERFACE
  MODULE FUNCTION Mesh_Constructor1(hdf5, group) RESULT(ans)
    TYPE(Mesh_) :: ans
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END FUNCTION Mesh_Constructor1
END INTERFACE

!>
! Generic function for constructing [[mesh_]]
INTERFACE Mesh
  MODULE PROCEDURE Mesh_Constructor1
END INTERFACE Mesh

PUBLIC :: Mesh

!----------------------------------------------------------------------------
!                                           Mesh_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 June 2021
! summary: This function returns a pointer to an instance of mesh_ object

INTERFACE
  MODULE FUNCTION Mesh_Constructor_1(hdf5, group) RESULT(ans)
    CLASS(Mesh_), POINTER :: ans
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END FUNCTION Mesh_Constructor_1
END INTERFACE

INTERFACE Mesh_Pointer
  MODULE PROCEDURE Mesh_Constructor_1
END INTERFACE Mesh_Pointer

PUBLIC :: Mesh_Pointer

!----------------------------------------------------------------------------
!                                                    Deallocate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 March 2021
! summary: Free up the memory stored in [[mesh_]]
!
!# Introduction
!
! Free up the memory stored in [[mesh_]] data type
!
!### Usage
!
!```fortran
!call obj%Deallocate( )
!```end fortran

INTERFACE
  MODULE SUBROUTINE mesh_Deallocate(obj)
    CLASS(Mesh_), INTENT(INOUT) :: obj
  END SUBROUTINE mesh_Deallocate
END INTERFACE

INTERFACE DEALLOCATE
  MODULE PROCEDURE mesh_Deallocate
END INTERFACE DEALLOCATE

PUBLIC :: DEALLOCATE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE mesh_final(obj)
    TYPE(Mesh_), INTENT(INOUT) :: obj
  END SUBROUTINE mesh_final
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 June 2021
! summary: This routine reads the mesh from a meshFile which is an hdf5
! file
!
!# Introduction
!
! This routine reads the following
!
! meshdata%uid,  meshdata%xidim, meshData%elemType, meshData%minX,
! meshData%minY, meshData%minZ, meshData%maxX, meshData%maxY, meshData%maxZ,
! meshData%X,meshData%Y, meshData%Z, meshData%tElements, meshData%tIntNodes,
! meshData%physicalTag, meshData%InternalNptrs, meshData%elemNumber,
! meshData%connectivity, meshData%boundingEntity
!
! This routine initiate the local_nptrs data in mesh.
! This routine also sets the number of nodes in the mesh (tNodes)
! This routine allocate obj%nodeData
! This routine set localNodeNum and globalNodeNum data inside the
! nodeData

INTERFACE
  MODULE SUBROUTINE mesh_Import(obj, hdf5, group)
    CLASS(Mesh_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE mesh_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                     getNodeCoord@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Sept 2021
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
! However, if you wish to use nodeCoord, then get the localNodeNumber of a
! global node by calling the mesh methods, and use this localNodeNumber to
! extract the coordinates.
!@endnote

INTERFACE
  MODULE SUBROUTINE mesh_getNodeCoord(obj, nodeCoord, hdf5, group)
    CLASS(Mesh_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: nodeCoord(:, :)
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE mesh_getNodeCoord
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Export@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 June 2021
! summary: This routine exports the mesh to a hdf5 file
!
!# Introduction

INTERFACE
  MODULE SUBROUTINE mesh_Export(obj, hdf5, group)
    CLASS(Mesh_), INTENT(IN) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE mesh_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ExportToVTK@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 Sept 2021
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
! - If closeTag is true then close the piece

INTERFACE
  MODULE SUBROUTINE mesh_ExportToVTK(obj, vtkFile, nodeCoord, filename, &
    & OpenTag, Content, CloseTag)
    CLASS(Mesh_), INTENT(IN) :: obj
    TYPE(VTKFile_), INTENT(INOUT) :: vtkFile
    REAL(DFP), OPTIONAL, INTENT(IN) :: nodeCoord(:, :)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: OpenTag
    !! Default is true
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: CloseTag
    !! Default is true
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: Content
    !! Default is true
  END SUBROUTINE mesh_ExportToVTK
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 June 2021
! summary: Displays the content of [[mesh_]] datatype
!
!# Introduction
!
! This routine displays the content of [[mesh_]] datatype
!
!### Usage
!
!```fortran
! call display( obj, 'mesh', stdout )
! call obj%display( 'mesh', stdout )
!```

INTERFACE
  MODULE SUBROUTINE mesh_display(obj, msg, UnitNo)
    CLASS(Mesh_), INTENT(INOUT) :: obj
    !! mesh object
    CHARACTER(*), INTENT(IN) :: msg
    !! message on screen
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: UnitNo
    !! unit number of ouput file
  END SUBROUTINE mesh_display
END INTERFACE

!>
! generic routine to display content of mesh
INTERFACE Display
  MODULE PROCEDURE mesh_display
END INTERFACE Display

PUBLIC :: Display

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 13 April 2022
! summary: Display a single instance of NodeData_

INTERFACE
  MODULE SUBROUTINE nodeData_Display(obj, msg, unitno)
    CLASS(NodeData_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE nodeData_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 13 April 2022
! summary: Display a single instance of element data

INTERFACE
  MODULE SUBROUTINE elemData_Display(obj, msg, unitno)
    CLASS(ElemData_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE elemData_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 13 April 2022
! summary: Display the instance of InternalFacetData

INTERFACE
  MODULE SUBROUTINE InternalFacetData_Display(obj, msg, unitno)
    CLASS(InternalFacetData_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE InternalFacetData_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 13 April 2022
! summary: Display the instance of DomainFacetData_

INTERFACE
  MODULE SUBROUTINE BoundaryFacetData_Display(obj, msg, unitno)
    CLASS(BoundaryFacetData_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE BoundaryFacetData_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                              DisplayNodeData@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 13 April 2022
! summary: Displays the Node data

INTERFACE
  MODULE SUBROUTINE mesh_DisplayNodeData(obj, msg, unitno)
    CLASS(Mesh_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE mesh_DisplayNodeData
END INTERFACE

!----------------------------------------------------------------------------
!                                              DisplayElementData@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 13 April 2022
! summary: Displays the element data

INTERFACE
  MODULE SUBROUTINE mesh_DisplayElementData(obj, msg, unitno)
    CLASS(Mesh_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE mesh_DisplayElementData
END INTERFACE

!----------------------------------------------------------------------------
!                                              DisplayFacetData@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 13 April 2022
! summary: Displays the element data

INTERFACE
  MODULE SUBROUTINE mesh_DisplayInternalFacetData(obj, msg, unitno)
    CLASS(Mesh_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE mesh_DisplayInternalFacetData
END INTERFACE

!----------------------------------------------------------------------------
!                                        DisplayBoundaryFacetData@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 13 April 2022
! summary: Displays the element data

INTERFACE
  MODULE SUBROUTINE mesh_DisplayBoundaryFacetData(obj, msg, unitno)
    CLASS(Mesh_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE mesh_DisplayBoundaryFacetData
END INTERFACE

!----------------------------------------------------------------------------
!                                              DisplayFacetElements@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 May 2022
! summary: Display the facet elements

INTERFACE
  MODULE SUBROUTINE mesh_DisplayFacetElements(obj, msg, unitno)
    CLASS(Mesh_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE mesh_DisplayFacetElements
END INTERFACE

!----------------------------------------------------------------------------
!                                              DisplayFacetElemSD@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 May 2022
! summary: Display the facet element shape data.

INTERFACE
  MODULE SUBROUTINE mesh_DisplayFacetElemSD(obj, msg, unitno)
    CLASS(Mesh_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE mesh_DisplayFacetElemSD
END INTERFACE

!----------------------------------------------------------------------------
!                                                 getTotalElements@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 June 2021
! summary: Returns total elements in the mesh

INTERFACE
  MODULE PURE FUNCTION mesh_size(obj) RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    !! mesh object
    INTEGER(I4B) :: ans
  END FUNCTION mesh_size
END INTERFACE

!----------------------------------------------------------------------------
!                                                       getElemNum@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-14
! update: 2021-11-14
! summary: Returns the global element numbers present in the mesh

INTERFACE
  MODULE FUNCTION mesh_getElemNum(obj) RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION mesh_getElemNum
END INTERFACE

!----------------------------------------------------------------------------
!                                               getRefElemPointer@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: Returns the pointer to the reference element

INTERFACE
  MODULE FUNCTION mesh_getRefElemPointer(obj) RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    CLASS(ReferenceElement_), POINTER :: ans
  END FUNCTION mesh_getRefElemPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                               getBoundingEntity@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 June 2021
! summary: Returns bounding entity of the mesh

INTERFACE
  MODULE PURE FUNCTION mesh_getBoundingEntity(obj) RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION mesh_getBoundingEntity
END INTERFACE

!----------------------------------------------------------------------------
!                                                        getNptrs@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 June 2021
! summary: Returns the vector of global node numbers

INTERFACE
  MODULE PURE FUNCTION mesh_getNptrs(obj) RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION mesh_getNptrs
END INTERFACE

!----------------------------------------------------------------------------
!                                                getInternalNptrs@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 June 2021
! summary: Returns the vector of global node numbers of internal nodes

INTERFACE
  MODULE PURE FUNCTION mesh_getInternalNptrs(obj) RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION mesh_getInternalNptrs
END INTERFACE

!----------------------------------------------------------------------------
!                                                getBoundaryNptrs@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 June 2021
! summary: Returns the vector of global node numbers of boundary nodes

INTERFACE
  MODULE PURE FUNCTION mesh_getBoundaryNptrs(obj) RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION mesh_getBoundaryNptrs
END INTERFACE

!----------------------------------------------------------------------------
!                                                 isBoundaryNode@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 June 2021
! summary: This function returns true if given global node is a boundary node

INTERFACE
  MODULE ELEMENTAL FUNCTION mesh_isBoundaryNode(obj, globalNode) RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    LOGICAL(LGT) :: ans
  END FUNCTION mesh_isBoundaryNode
END INTERFACE

!----------------------------------------------------------------------------
!                                                  isNodePresent@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 June 2021
! summary: Returns  TRUE if a given global node number is present

INTERFACE
  MODULE ELEMENTAL FUNCTION mesh_isNodePresent(obj, globalNode) RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    LOGICAL(LGT) :: ans
  END FUNCTION mesh_isNodePresent
END INTERFACE

!----------------------------------------------------------------------------
!                                                isAnyNodePresent@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 April 2022
! summary: Returns TRUE if any global node number is present

INTERFACE
  MODULE PURE FUNCTION mesh_isAnyNodePresent(obj, globalNode) RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    LOGICAL(LGT) :: ans
  END FUNCTION mesh_isAnyNodePresent
END INTERFACE

!----------------------------------------------------------------------------
!                                                isAllNodePresent@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 April 2022
! summary: Returns TRUE if any global node number is present

INTERFACE
  MODULE PURE FUNCTION mesh_isAllNodePresent(obj, globalNode) RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    LOGICAL(LGT) :: ans
  END FUNCTION mesh_isAllNodePresent
END INTERFACE

!----------------------------------------------------------------------------
!                                                 isElementPresent@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 June 2021
! summary: Returns  TRUE if a given global Element number is present

INTERFACE
  MODULE ELEMENTAL FUNCTION mesh_isElementPresent(obj, GlobalElement) &
    & RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: GlobalElement
    LOGICAL(LGT) :: ans
  END FUNCTION mesh_isElementPresent
END INTERFACE

!----------------------------------------------------------------------------
!                                               isBoundaryElement@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 June 2021
! summary: Returns true if an global element number is a boundary element
!
!# Introduction
! This routine returns true if a global element number is a boundary element.
! A boundary element is one which contains a boundary node.

INTERFACE
  MODULE ELEMENTAL FUNCTION mesh_isBoundaryElement(obj, globalElement) &
    & RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    LOGICAL(LGT) :: ans
  END FUNCTION mesh_isBoundaryElement
END INTERFACE

!----------------------------------------------------------------------------
!                                        isDomainBoundaryElement@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 June 2021
! summary: Returns true if an global element number is domain boundary element
!
!# Introduction
! This routine returns true if a global element number is domain
! boundary element.
! A boundary element is one which contains a boundary node.
! A domain boundary element is a boundary element with
! no connection with the other mesh.

INTERFACE
  MODULE ELEMENTAL FUNCTION mesh_isDomainBoundaryElement(obj, globalElement) &
    & RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    LOGICAL(LGT) :: ans
  END FUNCTION mesh_isDomainBoundaryElement
END INTERFACE

!----------------------------------------------------------------------------
!                                        isDomainFacetElement@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 June 2021
! summary: Returns true if an global element number is domain boundary element
!
!# Introduction
! This routine returns true if a global element number is domain
! boundary element.
! A boundary element is one which contains a boundary node.
! A domain boundary element is a boundary element with
! no connection with the other mesh.

INTERFACE
  MODULE ELEMENTAL FUNCTION mesh_isDomainFacetElement(obj, facetElement) &
    & RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: facetElement
    LOGICAL(LGT) :: ans
  END FUNCTION mesh_isDomainFacetElement
END INTERFACE

!----------------------------------------------------------------------------
!                                           getTotalInternalNodes@GetMethodss
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 July 2021
! summary: Returns total number of internal nodes inside the mesh

INTERFACE
  MODULE PURE FUNCTION mesh_getTotalInternalNodes(obj) RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION mesh_getTotalInternalNodes
END INTERFACE

!----------------------------------------------------------------------------
!                                             getTotalNodes@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 July 2021
! summary: returns total number of nodes in the mesh

INTERFACE
  MODULE PURE FUNCTION mesh_getTotalNodes(obj) RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION mesh_getTotalNodes
END INTERFACE

!----------------------------------------------------------------------------
!                                     getTotalBoundaryNodes@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 July 2021
! summary: returns total number of boundary nodes in the mesh

INTERFACE
  MODULE PURE FUNCTION mesh_getTotalBoundaryNodes(obj) RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION mesh_getTotalBoundaryNodes
END INTERFACE

!----------------------------------------------------------------------------
!                                  getTotalBoundaryElements@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 July 2021
! summary: returns total number of boundary elements

INTERFACE
  MODULE PURE FUNCTION mesh_getTotalBoundaryElements(obj) RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION mesh_getTotalBoundaryElements
END INTERFACE

!----------------------------------------------------------------------------
!                                            getBoundingBox@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 July 2021
! summary: returns bounding box of the mesh

INTERFACE
  MODULE PURE FUNCTION mesh_getBoundingBox1(obj) RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    TYPE(BoundingBox_) :: ans
  END FUNCTION mesh_getBoundingBox1
END INTERFACE

!----------------------------------------------------------------------------
!                                            getBoundingBox@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 July 2021
! summary: returns bounding box of the mesh

INTERFACE
  MODULE PURE FUNCTION mesh_getBoundingBox2(obj, nodes, local_nptrs)  &
    & RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    REAL(DFP), INTENT(IN) :: nodes(:, :)
    !! Nodal coordinates in XiJ format
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: local_nptrs(:)
    TYPE(BoundingBox_) :: ans
  END FUNCTION mesh_getBoundingBox2
END INTERFACE

!----------------------------------------------------------------------------
!                                                 getConnectivity@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 June 2021
! summary: This routine returns global node numbers in a given global elem

INTERFACE
  MODULE PURE FUNCTION mesh_getConnectivity(obj, globalElement) RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION mesh_getConnectivity
END INTERFACE

!----------------------------------------------------------------------------
!                                             getLocalNodeNumber@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 June 2021
! summary: This function returns the local node number from global node num
!
!# Introduction
!
! This function returns the local node numbers from global node numbers.

INTERFACE
  MODULE PURE FUNCTION mesh_getLocalNodeNumber1(obj, globalNode) RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B) :: ans(SIZE(globalNode))
  END FUNCTION mesh_getLocalNodeNumber1
END INTERFACE

!----------------------------------------------------------------------------
!                                         getLocalNodeNumber@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 June 2021
! summary: This routine returns the local node number from a global node number

INTERFACE
  MODULE PURE FUNCTION mesh_getLocalNodeNumber2(obj, globalNode) RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B) :: ans
  END FUNCTION mesh_getLocalNodeNumber2
END INTERFACE

!----------------------------------------------------------------------------
!                                          getGlobalNptrs@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 June 2021
! summary: This function returns the Global node number from local node num

INTERFACE
  MODULE PURE FUNCTION mesh_getGlobalNodeNumber1(obj, localNode) RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: localNode(:)
    INTEGER(I4B) :: ans(SIZE(localNode))
  END FUNCTION mesh_getGlobalNodeNumber1
END INTERFACE

!----------------------------------------------------------------------------
!                                          getGlobalNptrs@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 June 2021
! summary: This routine returns the Global node number from a local node number

INTERFACE
  MODULE PURE FUNCTION mesh_getGlobalNodeNumber2(obj, localNode) RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: localNode
    INTEGER(I4B) :: ans
  END FUNCTION mesh_getGlobalNodeNumber2
END INTERFACE

!----------------------------------------------------------------------------
!                                       getGlobalElemNumber@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 June 2021
! summary: This function returns the Global node number from local node num

INTERFACE
  MODULE PURE FUNCTION mesh_getGlobalElemNumber1(obj, LocalElement) &
    & RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: LocalElement(:)
    INTEGER(I4B) :: ans(SIZE(LocalElement))
  END FUNCTION mesh_getGlobalElemNumber1
END INTERFACE

!----------------------------------------------------------------------------
!                                             getGlobalElemNumber@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 June 2021
! summary: This routine returns the Global node number from a local node number

INTERFACE
 MODULE PURE FUNCTION mesh_getGlobalElemNumber2(obj, LocalElement) RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: LocalElement
    INTEGER(I4B) :: ans
  END FUNCTION mesh_getGlobalElemNumber2
END INTERFACE

!----------------------------------------------------------------------------
!                                              getLocalElemNumber@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 June 2021
! summary: This function returns the local element number

INTERFACE
 MODULE PURE FUNCTION mesh_getLocalElemNumber1(obj, GlobalElement) RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: GlobalElement(:)
    INTEGER(I4B) :: ans(SIZE(GlobalElement))
  END FUNCTION mesh_getLocalElemNumber1
END INTERFACE

!----------------------------------------------------------------------------
!                                              getLocalElemNumber@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 June 2021
! summary: This function returns the local element number

INTERFACE
 MODULE PURE FUNCTION mesh_getLocalElemNumber2(obj, GlobalElement) RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: GlobalElement
    INTEGER(I4B) :: ans
  END FUNCTION mesh_getLocalElemNumber2
END INTERFACE

!----------------------------------------------------------------------------
!                                               getNodeToElements@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 June 2021
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
  MODULE PURE FUNCTION mesh_getNodeToElements1(obj, globalNode) RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    !! mesh data
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global node number
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !! A vector of local element number
  END FUNCTION mesh_getNodeToElements1
END INTERFACE

!----------------------------------------------------------------------------
!                                               getNodeToElements@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 June 2021
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
  MODULE PURE FUNCTION mesh_getNodeToElements2(obj, globalNode) RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    !! mesh data
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global node number
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !! A vector of local element number
  END FUNCTION mesh_getNodeToElements2
END INTERFACE

!----------------------------------------------------------------------------
!                                                  getNodeToNodes@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 June 2021
! summary:         Returns the node surrounding a node
!
!# Introduction
! This fucntion returns the vector of node numbers which surrounds a given
! node number `globalNode`.
! - If `IncludeSelf` is true then, in the returned vector of integer,
! node number globalNode is also present
!- If `IncludeSelf` is false then, in the returned vector of integer,
! node number `globalNode` is not present
!
!@note
!  If the node number `globalNode` is not present in the mesh then the
! returned vector of integer has zero length
!@endnote

INTERFACE
  MODULE FUNCTION mesh_getNodeToNodes1(obj, globalNode, IncludeSelf) &
    & RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    LOGICAL(LGT), INTENT(IN) :: IncludeSelf
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION mesh_getNodeToNodes1
END INTERFACE

!----------------------------------------------------------------------------
!                                                 getNodeToNodes@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 11 April 2022
! summary: Returns the node surrounding a node
!
!# Introduction
!
! This function returns the vector of node numbers which surrounds a given
! node number `globalNode`.
! - If `IncludeSelf` is true then, in the returned vector of integer,
! node number globalNode is also present
!- If `IncludeSelf` is false then, in the returned vector of integer,
! node number `globalNode` is not present
!
!@note
!  If the node number `globalNode` is not present in the mesh then the
! returned vector of integer has zero length
!@endnote

INTERFACE
  MODULE FUNCTION mesh_getNodeToNodes2(obj, globalNode, IncludeSelf) &
    & RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    LOGICAL(LGT), INTENT(IN) :: IncludeSelf
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION mesh_getNodeToNodes2
END INTERFACE

!----------------------------------------------------------------------------
!                                      getElementToElements@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 June 2021
! summary: Returns element to element connectivity information
!
!# Introduction
! This routine returns element to element connectivity information
! for a given global element number `globalElement`
!
! If `OnlyElements` is absent or it is set to FALSE then, this routine
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
  MODULE PURE FUNCTION mesh_getElementToElements(obj, globalElement, &
    & onlyElements) RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    !! mesh data
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! Global element number
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: onlyElements
    !! If onlyElements is absent or it is FALSE then full information
    !! about the elements connected to element iel is given
    !! If onlyElements is present and it is TRUE then only the
    !! information about the elements connected to element iel is given
    INTEGER(I4B), ALLOCATABLE :: ans(:, :)
    !! list of elements surrounding elements
  END FUNCTION mesh_getElementToElements
END INTERFACE

!----------------------------------------------------------------------------
!                                          getBoundaryElementData@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 22 July 2021
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
  MODULE PURE FUNCTION mesh_getBoundaryElementData(obj, globalElement) &
    & RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION mesh_getBoundaryElementData
END INTERFACE

!----------------------------------------------------------------------------
!                                                        getOrder@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-12-08
! update: 2021-12-08
! summary: Returns the order of reference element

INTERFACE
  MODULE PURE FUNCTION mesh_getOrder(obj) RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION mesh_getOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                                         getNSD@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 April 2022
! summary: Returns the spatial dimension of the mesh

INTERFACE
  MODULE PURE FUNCTION mesh_getNSD(obj) RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION mesh_getNSD
END INTERFACE

!----------------------------------------------------------------------------
!                                                  getXidimension@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 6 April 2022
! summary: Returns the xidimension of the mesh

INTERFACE
  MODULE PURE FUNCTION mesh_getXidimension(obj) RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION mesh_getXidimension
END INTERFACE

!----------------------------------------------------------------------------
!                                                     getMaterial@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-12-09
! update: 2021-12-09
! summary: Returns the materials id of a given medium

INTERFACE
  MODULE PURE FUNCTION mesh_getMaterial(obj, medium) RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: medium
    INTEGER(I4B) :: ans
  END FUNCTION mesh_getMaterial
END INTERFACE

!----------------------------------------------------------------------------
!                                           getTotalFacetElements@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 May 2022
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
  MODULE PURE FUNCTION mesh_getTotalFacetElements(obj) RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION mesh_getTotalFacetElements
END INTERFACE

!----------------------------------------------------------------------------
!                                   getTotalInternalFacetElements@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 May 2022
! summary: Returns the total number of internal facets element in mesh

INTERFACE
  MODULE PURE FUNCTION mesh_getTotalInternalFacetElements(obj) RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION mesh_getTotalInternalFacetElements
END INTERFACE

!----------------------------------------------------------------------------
!                                   getTotalBoundaryFacetElements@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION mesh_getTotalBoundaryFacetElements(obj) RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION mesh_getTotalBoundaryFacetElements
END INTERFACE

!----------------------------------------------------------------------------
!                                            getMasterCellNumber@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 11 April 2022
! summary: Returns the master cell number of a given facet
!
!# Introduction
!
! - This routine returns the master cell number of a given facet element
! - The facetElement is the local facet element
! - The master cell number is the global element number

INTERFACE
  MODULE PURE FUNCTION mesh_getMasterCellNumber(obj, facetElement, &
    & elementType)&
    & RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: facetElement
    INTEGER(I4B), INTENT(IN) :: elementType
    INTEGER(I4B) :: ans
  END FUNCTION mesh_getMasterCellNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                            getSlaveCellNumber@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 11 April 2022
! summary: Returns the slave cell number of a given facet
!
!# Introduction
!
! - This routine returns the slave cell number of a given facet element
! - The facetElement is the local facet element
! - The slave cell number is the global element number

INTERFACE
  MODULE PURE FUNCTION mesh_getSlaveCellNumber(obj, facetElement, &
    & elementType) RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: facetElement
    INTEGER(I4B), INTENT(IN) :: elementType
    INTEGER(I4B) :: ans
  END FUNCTION mesh_getSlaveCellNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                                   getCellNumber@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 11 April 2022
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
  MODULE PURE FUNCTION mesh_getCellNumber(obj, facetElement, &
    & elementType) RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: facetElement
    INTEGER(I4B), INTENT(IN) :: elementType
    INTEGER(I4B) :: ans(2)
  END FUNCTION mesh_getCellNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                                getLocalFacetID@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 11 April 2022
! summary: Returns the local facet id

INTERFACE
  MODULE PURE FUNCTION mesh_getLocalFacetID(obj, facetElement, &
    & elementType, isMaster) RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: facetElement
    INTEGER(I4B), INTENT(IN) :: elementType
    LOGICAL(LGT), INTENT(IN) :: isMaster
    INTEGER(I4B) :: ans
  END FUNCTION mesh_getLocalFacetID
END INTERFACE

!----------------------------------------------------------------------------
!                                           getFacetConnectivity@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 11 April 2022
! summary: Returns the connectivity of a facet element
!
!# Introduction
!
! - Returns the connectivity of a given facet element
! - facetElement is local facet element number

INTERFACE
  MODULE PURE FUNCTION mesh_getFacetConnectivity1(obj, facetElement, &
    & elementType, isMaster) RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: facetElement
    INTEGER(I4B), INTENT(IN) :: elementType
    LOGICAL(LGT), INTENT(IN) :: isMaster
      !! if isMaster is true then connectivity of facet in master-cell
      !! is returned, otherwise connectivity of facet in slave-cell
      !! is returned. This is only applicable for internal facet element
      !! because for domain facet we do not have slave-cell.
      !! Currently, we do not support slave-cell for meshFacet because
      !! the slave of meshFacet lives in different instance of mesh_
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION mesh_getFacetConnectivity1
END INTERFACE

!----------------------------------------------------------------------------
!                                           getFacetConnectivity@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 April 2022
! summary: Returns the connectivity of a facet element of a cellElement
!
!# Introduction
!
! - Returns the connectivity of a given facet element of a cellElement
! - globalElement is global element number of cell number
! - iface is the local face number in globalElement

INTERFACE
  MODULE PURE FUNCTION mesh_getFacetConnectivity2(obj, globalElement, &
    & iface) RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    INTEGER(I4B), INTENT(IN) :: iface
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION mesh_getFacetConnectivity2
END INTERFACE

!----------------------------------------------------------------------------
!                                            getFacetElementType@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 April 2022
! summary: Returns the facet element type of the cell element number

INTERFACE
  MODULE PURE FUNCTION mesh_getFacetElementType(obj, globalElement) &
    & RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION mesh_getFacetElementType
END INTERFACE

!----------------------------------------------------------------------------
!                                     InitiateNodeToElements@NodeDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 June 2021
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
! Always use method called `getNodeToElements()` to access this information.
! This methods requires global Node number
!@endnote
!
!@warning
! Always use the mapping between global node number and local node number to
! avoid segmentation fault
!@endwarning

INTERFACE
  MODULE SUBROUTINE mesh_InitiateNodeToElements(obj)
    CLASS(Mesh_), INTENT(INOUT) :: obj
  END SUBROUTINE mesh_InitiateNodeToElements
END INTERFACE

!----------------------------------------------------------------------------
!                                         InitiateNodeToNode@NodeDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 June 2021
! summary: Initiate node to node connectivity data
!
!# Introduction
!
!- This routine generate the node to nodes mapping
!- In other words, it generates info of node-numbers in mesh
! surrounding a node number
!- This mapping is stored inside `obj%nodeData%globalNodeNum`
!- For a local node number i, obj%nodeData(i)%globalNodeNum denotes the
! global node data surrounding the local node number.
!- This list does not include self node.
!- This methods needs node-to-elements data, therefore if this data
! is not initiated, then this method calls `InitiateNodeToElements()`
!

INTERFACE
  MODULE SUBROUTINE mesh_InitiateNodetoNodes(obj)
    CLASS(Mesh_), INTENT(INOUT) :: obj
    !! mesh data
  END SUBROUTINE mesh_InitiateNodetoNodes
END INTERFACE

!----------------------------------------------------------------------------
!                                   InitiateExtraNodeToNode@NodeDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 June 2021
! summary: Initiate node to node connectivity data
!
!# Introduction
!
!- This routine generate the node to nodes mapping
!- This mapping is stored inside `obj%nodeData%extraGlobalNodeNum`
!- For a local node number i, `obj%nodeData(i)%ExtraGlobalNodeNum` denotes the
! global node data surrounding the local node number used for edge-based
!  stabilization. This list does not include self node.
!
!- This methods needs information about `nodeToNodes`, `nodeToElements`,
! and `elementToElements`. Therefore,
!- If `nodeToNodes` is not initiated, then this method initiates it.
!- If `nodeToElements` is not initiated, then this method initiates it.
!- If `elementToElements` is not initiated, then this method initiates it.

INTERFACE
  MODULE SUBROUTINE mesh_InitiateExtraNodetoNodes(obj)
    CLASS(Mesh_), INTENT(INOUT) :: obj
    !! mesh data
  END SUBROUTINE mesh_InitiateExtraNodetoNodes
END INTERFACE

!----------------------------------------------------------------------------
!                               InitiateElementToElements@ElementDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:  15 June 2021
! summary: Initiate element to element data
!
!# Introduction
!
!- This routine creates the element surrounding a given element data
!- Before calling this routine make sure the `refelem` in mesh is allocated.
!- By using `refelem`, this routine forms the FacetElements.
!- This routine needs `nodeToElements` information, therefore, if
! `nodeToElements` is not initiated then it calls `initiateNodeToelements`
! method
!- This method forms the following data:
!- `obj%elementData(ielem)%globalElements`
!- It also identifies those elements which are boundary element of mesh, and
!- set `obj%elementData(ielem)%elementType=BOUNDARY_ELEMENT` for those element
!- Note that at this point boundary element are those which has at least
! one orphan face.
!- Note that at this point these boundary element can be interface element
! between two mesh, or domain-boundary element.

INTERFACE
  MODULE SUBROUTINE mesh_InitiateElementToElements(obj)
    CLASS(Mesh_), INTENT(INOUT) :: obj
    !! mesh data
  END SUBROUTINE mesh_InitiateElementToElements
END INTERFACE

!----------------------------------------------------------------------------
!                                  InitiateBoundaryData@BoundaryDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 June 2021
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
  MODULE SUBROUTINE mesh_InitiateBoundaryData(obj)
    CLASS(Mesh_), INTENT(INOUT) :: obj
    !! mesh data
  END SUBROUTINE mesh_InitiateBoundaryData
END INTERFACE

!----------------------------------------------------------------------------
!                                     InitiateFacetElements@FacetDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 11 April 2022
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
  MODULE SUBROUTINE mesh_InitiateFacetElements(obj)
    CLASS(Mesh_), INTENT(INOUT) :: obj
  END SUBROUTINE mesh_InitiateFacetElements
END INTERFACE

!----------------------------------------------------------------------------
!                                                  setBoundingBox@setMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 July 2021
! summary: Sets the bounding box information in the mesh

INTERFACE
  MODULE PURE SUBROUTINE mesh_setBoundingBox1(obj, box)
    CLASS(Mesh_), INTENT(INOUT) :: obj
    TYPE(BoundingBox_), INTENT(IN) :: box
  END SUBROUTINE mesh_setBoundingBox1
END INTERFACE

!----------------------------------------------------------------------------
!                                                  setBoundingBox@setMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 July 2021
! summary: Sets the bounding box information in the mesh

INTERFACE
  MODULE PURE SUBROUTINE mesh_setBoundingBox2(obj, nodes, local_nptrs)
    CLASS(Mesh_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: nodes(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: local_nptrs(:)
  END SUBROUTINE mesh_setBoundingBox2
END INTERFACE

!----------------------------------------------------------------------------
!                                                      setSparsity@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine set the sparsity pattern in [[CSRMatrix_]] object
!
!# Introduction
!
! This routine sets the sparsity pattern in [[CSRMatrix_]] object.

INTERFACE
  MODULE SUBROUTINE mesh_setSparsity1(obj, mat, localNodeNumber, lbound, &
    & ubound)
    CLASS(Mesh_), INTENT(INOUT) :: obj
    !! [[Mesh_]] class
    TYPE(CSRMatrix_), INTENT(INOUT) :: mat
    !! [[CSRMatrix_]] object
    INTEGER(I4B), INTENT(IN) :: lbound
    INTEGER(I4B), INTENT(IN) :: ubound
    INTEGER(I4B), INTENT(IN) :: localNodeNumber(lbound:ubound)
    !! Global to local node number map
  END SUBROUTINE mesh_setSparsity1
END INTERFACE

!----------------------------------------------------------------------------
!                                                setSparsity@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 Oct 2021
! summary: This routine set the sparsity pattern in [[CSRMatrix_]] object

INTERFACE
  MODULE SUBROUTINE mesh_setSparsity2(obj, mat)
    CLASS(Mesh_), INTENT(INOUT) :: obj
    !! Mesh_ class
    TYPE(CSRMatrix_), INTENT(INOUT) :: mat
    !! CSRMatrix object
  END SUBROUTINE mesh_setSparsity2
END INTERFACE

!----------------------------------------------------------------------------
!                                                      setSparsity@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Oct 2021
! summary: This routine set the sparsity pattern in [[CSRMatrix_]] object
!
!# Introduction
!
! This routine sets the sparsity pattern in [[CSRMatrix_]] object.

INTERFACE
  MODULE SUBROUTINE mesh_setSparsity3(obj, colMesh, nodeToNode, mat, &
    & ivar, jvar)
    CLASS(Mesh_), INTENT(INOUT) :: obj
  !! [[Mesh_]] class
    CLASS(Mesh_), INTENT(INOUT) :: colMesh
  !! [[Mesh_]] class
    INTEGER(I4B), INTENT(IN) :: nodeToNode(:)
  !! Node to node connectivity between obj and colMesh
    TYPE(CSRMatrix_), INTENT(INOUT) :: mat
  !! [[CSRMatrix_]] object
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
  END SUBROUTINE mesh_setSparsity3
END INTERFACE

!----------------------------------------------------------------------------
!                                                      setSparsity@setMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Oct 2021
! summary: This routine set the sparsity pattern in [[CSRMatrix_]] object
!
!# Introduction
!
! This routine sets the sparsity pattern in [[CSRMatrix_]] object.

INTERFACE
  MODULE SUBROUTINE mesh_setSparsity4(obj, colMesh, nodeToNode, mat, &
  & rowGlobalToLocalNodeNum, rowLBOUND, rowUBOUND, colGlobalToLocalNodeNum, &
  & colLBOUND, colUBOUND, ivar, jvar)
    CLASS(Mesh_), INTENT(INOUT) :: obj
    !! [[Mesh_]] class
    CLASS(Mesh_), INTENT(INOUT) :: colMesh
    !! [[Mesh_]] class
    INTEGER(I4B), INTENT(IN) :: nodeToNode(:)
    !! node to node connectivity between obj and colMesh
    TYPE(CSRMatrix_), INTENT(INOUT) :: mat
    !! [[CSRMatrix_]] object
    INTEGER(I4B), INTENT(IN) :: rowLBOUND
    INTEGER(I4B), INTENT(IN) :: rowUBOUND
    INTEGER(I4B), INTENT(IN) :: rowGlobalToLocalNodeNum( &
      & rowLBOUND:rowUBOUND)
    !! Global to local node number map
    INTEGER(I4B), INTENT(IN) :: colLBOUND
    INTEGER(I4B), INTENT(IN) :: colUBOUND
    INTEGER(I4B), INTENT(IN) :: colGlobalToLocalNodeNum( &
      & colLBOUND:colUBOUND)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
  END SUBROUTINE mesh_setSparsity4
END INTERFACE

!----------------------------------------------------------------------------
!                                                     setMaterial@setMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-12-09
! update: 2021-12-09
! summary: Set the materials id of a given medium

INTERFACE
  MODULE PURE SUBROUTINE mesh_setTotalMaterial(obj, n)
    CLASS(Mesh_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: n
  END SUBROUTINE mesh_setTotalMaterial
END INTERFACE

!----------------------------------------------------------------------------
!                                                     setMaterial@setMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-12-09
! update: 2021-12-09
! summary: Set the materials id of a given medium

INTERFACE
  MODULE PURE SUBROUTINE mesh_setMaterial(obj, medium, material)
    CLASS(Mesh_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: medium
    INTEGER(I4B), INTENT(IN) :: material
  END SUBROUTINE mesh_setMaterial
END INTERFACE

!----------------------------------------------------------------------------
!                                            setFacetElementType@setMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 April 2022
! summary: Set the facet element type of a given cell number

INTERFACE
  MODULE PURE SUBROUTINE mesh_setFacetElementType(obj, globalElement, &
    & iface, facetElementType)
    CLASS(Mesh_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    INTEGER(I4B), INTENT(IN) :: iface
    INTEGER(I4B), INTENT(IN) :: facetElementType
  END SUBROUTINE mesh_setFacetElementType
END INTERFACE

!----------------------------------------------------------------------------
!                                            InitiateElemSD@ShapeDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-12-09
! update: 2021-12-09
! summary: sets the local shape data for the mesh
!
!# Introduction
!
! This routine set the local shape data in space (linSpaceElemSD and
! spaceElemSD) for the mesh. It also creates the quadrature points in space.

INTERFACE
  MODULE SUBROUTINE mesh_initiateElemSD1(obj, &
    & orderSpace,  &
    & linSpaceElem, &
    & spaceElem, &
    & quadTypeForSpace, &
    & continuityTypeForSpace, &
    & interpolTypeForSpace)
    CLASS(Mesh_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: orderSpace
      !! integrand order in space
    CLASS(ReferenceElement_), TARGET, INTENT(IN) :: linSpaceElem
      !! linear (simplex) space element
    CLASS(ReferenceElement_), TARGET, INTENT(IN) :: spaceElem
      !! space element
    CHARACTER(*), INTENT(IN) :: quadTypeForSpace
      !! quadrature for space
    CHARACTER(*), INTENT(IN) :: continuityTypeForSpace
      !! continuity for base in space
    CHARACTER(*), INTENT(IN) :: interpolTypeForSpace
      !! interpolation type for base in space
  END SUBROUTINE mesh_initiateElemSD1
END INTERFACE

!----------------------------------------------------------------------------
!                                            InitiateElemSD@ShapeDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-12-09
! update: 2021-12-09
! summary: sets the local shape data for the mesh

INTERFACE
  MODULE SUBROUTINE mesh_initiateElemSD2(obj, &
    & orderSpace,  &
    & linSpaceElem, &
    & spaceElem, &
    & quadTypeForSpace, &
    & continuityTypeForSpace, &
    & interpolTypeForSpace, &
    & orderTime, &
    & linTimeElem, &
    & timeElem, &
    & quadTypeForTime, &
    & continuityTypeForTime, &
    & interpolTypeForTime, &
    & tvec)
    CLASS(Mesh_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: orderSpace
      !! integrand order in space
    CLASS(ReferenceElement_), TARGET, INTENT(IN) :: linSpaceElem
      !! linear space element
    CLASS(ReferenceElement_), TARGET, INTENT(IN) :: spaceElem
      !! space element
    CHARACTER(*), INTENT(IN) :: quadTypeForSpace
      !! quadrature type for space
    CHARACTER(*), INTENT(IN) :: continuityTypeForSpace
      !! continuity type of base in space
    CHARACTER(*), INTENT(IN) :: interpolTypeForSpace
      !! interpol type of base in space
    INTEGER(I4B), INTENT(IN) :: orderTime
      !! integrand order in time
    TYPE(ReferenceLine_), INTENT(IN) :: linTimeElem
      !! linear time element
    TYPE(ReferenceLine_), INTENT(IN) :: timeElem
      !! time element
    CHARACTER(*), INTENT(IN) :: quadTypeForTime
      !! quadrature type of base in time
    CHARACTER(*), INTENT(IN) :: continuityTypeForTime
      !! continuity type of base in time
    CHARACTER(*), INTENT(IN) :: interpolTypeForTime
      !! interpol type of base in time
    REAL(DFP), INTENT(IN) :: tvec(:)
  END SUBROUTINE mesh_initiateElemSD2
END INTERFACE

!----------------------------------------------------------------------------
!                                            InitiateElemSD@ShapeDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-12-09
! update: 2021-12-09
! summary: sets the local shape data for the mesh

INTERFACE
  MODULE SUBROUTINE mesh_initiateElemSD3(obj, &
    & orderSpace,  &
    & linSpaceElem, &
    & spaceElem, &
    & quadTypeForSpace, &
    & continuityTypeForSpace, &
    & interpolTypeForSpace, &
    & orderTime, &
    & linTimeElem, &
    & timeElem, &
    & quadTypeForTime, &
    & continuityTypeForTime, &
    & interpolTypeForTime)
    CLASS(Mesh_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: orderSpace
      !! integrand order in space
    CLASS(ReferenceElement_), TARGET, INTENT(IN) :: linSpaceElem
      !! linear space element
    CLASS(ReferenceElement_), TARGET, INTENT(IN) :: spaceElem
      !! space element
    CHARACTER(*), INTENT(IN) :: quadTypeForSpace
      !! quadrature type of base in space
    CHARACTER(*), INTENT(IN) :: continuityTypeForSpace
      !! continuity type of base in space
    CHARACTER(*), INTENT(IN) :: interpolTypeForSpace
      !! interpolation type of base in space
    INTEGER(I4B), INTENT(IN) :: orderTime
      !! integrand order in time
    TYPE(ReferenceLine_), INTENT(IN) :: linTimeElem
      !! linear time element
    TYPE(ReferenceLine_), INTENT(IN) :: timeElem
      !! time element
    CHARACTER(*), INTENT(IN) :: quadTypeForTime
      !! quadrature type of base in time
    CHARACTER(*), INTENT(IN) :: continuityTypeForTime
      !! continuity type of base in time
    CHARACTER(*), INTENT(IN) :: interpolTypeForTime
      !! interpolation type of base in time
  END SUBROUTINE mesh_initiateElemSD3
END INTERFACE

!----------------------------------------------------------------------------
!                                            InitiateElemSD@ShapeDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-12-09
! update: 2021-12-09
! summary: sets the local shape data for the mesh

INTERFACE
  MODULE SUBROUTINE mesh_initiateElemSD4(obj, tvec)
    CLASS(Mesh_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: tvec(:)
  END SUBROUTINE mesh_initiateElemSD4
END INTERFACE

!----------------------------------------------------------------------------
!                                            InitiateElemSD@ShapeDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-12-09
! summary: sets the local shape data for the mesh

INTERFACE
  MODULE SUBROUTINE mesh_initiateFacetElemSD1(obj, &
    & orderSpace,  &
    & linSpaceElem, &
    & spaceElem, &
    & quadTypeForSpace, &
    & continuityTypeForSpace, &
    & interpolTypeForSpace)
    CLASS(Mesh_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: orderSpace
      !! integrand order in space
    CLASS(ReferenceElement_), TARGET, INTENT(IN) :: linSpaceElem(:)
      !! linear (simplex) space element for each face
    CLASS(ReferenceElement_), TARGET, INTENT(IN) :: spaceElem(:)
      !! space element for each face
    CHARACTER(*), INTENT(IN) :: quadTypeForSpace
      !! quadrature for space
    CHARACTER(*), INTENT(IN) :: continuityTypeForSpace
      !! continuity for base in space
    CHARACTER(*), INTENT(IN) :: interpolTypeForSpace
      !! interpolation type for base in space
  END SUBROUTINE mesh_initiateFacetElemSD1
END INTERFACE

!----------------------------------------------------------------------------
!                                            InitiateElemSD@ShapeDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 May 2022
! summary: sets the local shape data for the mesh

INTERFACE
  MODULE SUBROUTINE mesh_initiateFacetElemSD2(obj, &
    & orderSpace,  &
    & linSpaceElem, &
    & spaceElem, &
    & quadTypeForSpace, &
    & continuityTypeForSpace, &
    & interpolTypeForSpace, &
    & orderTime, &
    & linTimeElem, &
    & timeElem, &
    & quadTypeForTime, &
    & continuityTypeForTime, &
    & interpolTypeForTime, &
    & tvec)
    CLASS(Mesh_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: orderSpace
      !! integrand order in space
    CLASS(ReferenceElement_), TARGET, INTENT(IN) :: linSpaceElem(:)
      !! linear space element for each face
    CLASS(ReferenceElement_), TARGET, INTENT(IN) :: spaceElem(:)
      !! space element for each face
    CHARACTER(*), INTENT(IN) :: quadTypeForSpace
      !! quadrature type for space
    CHARACTER(*), INTENT(IN) :: continuityTypeForSpace
      !! continuity type of base in space
    CHARACTER(*), INTENT(IN) :: interpolTypeForSpace
      !! interpol type of base in space
    INTEGER(I4B), INTENT(IN) :: orderTime
      !! integrand order in time
    TYPE(ReferenceLine_), INTENT(IN) :: linTimeElem
      !! linear time element
    TYPE(ReferenceLine_), INTENT(IN) :: timeElem
      !! time element
    CHARACTER(*), INTENT(IN) :: quadTypeForTime
      !! quadrature type of base in time
    CHARACTER(*), INTENT(IN) :: continuityTypeForTime
      !! continuity type of base in time
    CHARACTER(*), INTENT(IN) :: interpolTypeForTime
      !! interpol type of base in time
    REAL(DFP), INTENT(IN) :: tvec(:)
  END SUBROUTINE mesh_initiateFacetElemSD2
END INTERFACE

!----------------------------------------------------------------------------
!                                            InitiateElemSD@ShapeDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-12-09
! update: 2021-12-09
! summary: sets the local shape data for the mesh

INTERFACE
  MODULE SUBROUTINE mesh_initiateFacetElemSD3(obj, tvec)
    CLASS(Mesh_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: tvec(:)
  END SUBROUTINE mesh_initiateFacetElemSD3
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END MODULE Mesh_Class
