! This program is a part of EASIFEM library
! Copyright (C) 2020~  Vikas Sharma, Ph.D
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
USE BaseType, ONLY: BoundingBox_, CSRMatrix_
USE ExceptionHandler_Class, ONLY: e
USE NodeData_Class, ONLY: NodeData_, INTERNAL_NODE, BOUNDARY_NODE,  &
  & DOMAIN_BOUNDARY_NODE, GHOST_NODE, TypeNode, NodeData_Display
USE ElemData_Class, ONLY: ElemData_, INTERNAL_ELEMENT, BOUNDARY_ELEMENT,  &
  & DOMAIN_BOUNDARY_ELEMENT, GHOST_ELEMENT, TypeElem,  &
  & ElemData_Display => Display
USE FacetData_Class, ONLY: InternalFacetData_, BoundaryFacetData_,  &
  & InternalFacetData_Display, BoundaryFacetData_Display
USE CPUTime_Class, ONLY: CPUTime_
IMPLICIT NONE

PRIVATE
PUBLIC :: AbstractMesh_
PUBLIC :: AbstractMeshDeallocate
PUBLIC :: AbstractMeshDisplay
PUBLIC :: AbstractMeshGetQuery
PUBLIC :: AbstractMeshImport

CHARACTER(*), PARAMETER :: modName = "AbstractMesh_Class"

!----------------------------------------------------------------------------
!                                                             AbstractMesh_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary:  Abstract class for mesh

TYPE, ABSTRACT :: AbstractMesh_
  LOGICAL(LGT) :: showTime = .FALSE.
    !! If true, then we show the time taken by various mesh operations
    !! This is for checking the performance of a subclass
  LOGICAL(LGT) :: readFromFile = .TRUE.
    !! True if the mesh is read from a file
  LOGICAL(LGT) :: isInitiated = .FALSE.
    !! logical flag denoting for whether mesh data is Initiated or not
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
  LOGICAL(LGT) :: isFacetDataInitiated = .FALSE.
    !! FacetData
  INTEGER(I4B) :: uid = 0
    !! Unique id of the mesh
  INTEGER(I4B) :: tElements_topology_wise(8) = 0
    !! point, line, triangle, quadrangle, tetrahedron, hexahedron, prism,
    !! pyramid (it is calculated in the postprocessing step)
  INTEGER(I4B) :: tElemTopologies = 0, elemTopologies(8) = 0
    !! total element topologies, name of element topologies are stored in
    !! elemTopologies(1:tElemTopologies)
    !! this info is computed in a postprocessing step
  INTEGER(I4B) :: nsd = 0
    !! number of spatial dimension of the mesh
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
  REAL(DFP) :: x = 0.0
    !! x coorindate of centroid
  REAL(DFP) :: y = 0.0
    !! y coordinate of centroid
  REAL(DFP) :: z = 0.0
    !! z coordinate of centroid
  INTEGER(I4B), ALLOCATABLE :: physicalTag(:)
    !! Physical entities associated with the current entity (mesh)
    !! physical tags can be thought of as the material numbers
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

  TYPE(NodeData_), ALLOCATABLE :: nodeData(:)
    !! Node data

  TYPE(ElemData_), ALLOCATABLE :: elementData(:)
    !! element data

  TYPE(InternalFacetData_), ALLOCATABLE :: internalFacetData(:)
    !! Internal facet data
    !! INFO: This data is initiated by InitiateFacetElements method

  TYPE(BoundaryFacetData_), ALLOCATABLE :: boundaryFacetData(:)
    !! Domain Facet Data
    !! INFO: This data is initiated by InitiateFacetElements method

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

  ! IO:
  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
    !! Read mesh from hdf5 file
  PROCEDURE, PUBLIC, PASS(obj) :: GetNodeCoord => obj_GetNodeCoord
    !! Read the nodeCoords from the hdf5file
  PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export
    !! Export mesh to an hdf5 file
  PROCEDURE, PUBLIC, PASS(obj) :: ExportToVTK => obj_ExportToVTK
    !! Export mesh to a VTKfile
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_display
    !! Display the mesh
  PROCEDURE, PUBLIC, PASS(obj) :: DisplayNodeData => &
    & obj_DisplayNodeData
  !! Display node data
  PROCEDURE, PUBLIC, PASS(obj) :: DisplayElementData => &
    & obj_DisplayElementData
    !! Display element data
  PROCEDURE, PUBLIC, PASS(obj) :: DisplayInternalFacetData => &
    & obj_DisplayInternalFacetData
    !! Display internal facet data
  PROCEDURE, PUBLIC, PASS(obj) :: DisplayBoundaryFacetData => &
    & obj_DisplayBoundaryFacetData
    !! Display mesh facet data
  PROCEDURE, PUBLIC, PASS(obj) :: DisplayFacetElements => &
    & obj_DisplayFacetElements
    !! Display facet element shape data
  PROCEDURE, PUBLIC, PASS(obj) :: DisplayMeshInfo => &
    & obj_DisplayMeshInfo
    !! Display mesh statistics

  ! SET:
  ! @NodeDataMethods
  PROCEDURE, PUBLIC, PASS(obj) :: InitiateNodeToElements => &
    & obj_InitiateNodeToElements
  !! Initiate node to element data (mapping)
  PROCEDURE, PUBLIC, PASS(obj) :: InitiateNodeToNodes => &
    & obj_InitiateNodetoNodes
  !! Initiate Node to nodes data
  PROCEDURE, PUBLIC, PASS(obj) :: InitiateExtraNodeToNodes => &
    & obj_InitiateExtraNodetoNodes
  !! Initiate Node to nodes mapping (used in jump based FEM)

  ! SET:
  ! @ElementDataMethods
  PROCEDURE, PUBLIC, PASS(obj) :: InitiateElementToElements => &
    & obj_InitiateElementToElements
  !! Initiate element to elements mapping

  ! SET:
  ! @BoundaryDataMethods
  PROCEDURE, PUBLIC, PASS(obj) :: InitiateBoundaryData => &
    & obj_InitiateBoundaryData
  !! Initiate the boundary data

  ! SET:
  ! @FacetDataMethods
  PROCEDURE, PUBLIC, PASS(obj) :: InitiateFacetElements => &
    & obj_InitiateFacetElements
  !! Initiate boundary data

  !  GET:
  ! @GetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: GetNNE => obj_GetNNE
  !! Get number of nodes in an element

  PROCEDURE, PUBLIC, PASS(obj) :: GetMaxNNE => obj_GetMaxNNE
  !! Get maximum number of nodes in an element

  PROCEDURE, PUBLIC, PASS(obj) :: Size => obj_size
  !! Returns the size of the mesh (total number of elements)

  PROCEDURE, PUBLIC, PASS(obj) :: GetElemNum => obj_GetElemNum
  !! Returns global element number in the mesh

  PROCEDURE, PUBLIC, PASS(obj) :: GetBoundingEntity => obj_GetBoundingEntity
  !! Returns the nodal coordinates

  PROCEDURE, PUBLIC, PASS(obj) :: GetNptrs => obj_GetNptrs
  !! Returns the node number of mesh

  PROCEDURE, PUBLIC, PASS(obj) :: GetInternalNptrs => obj_GetInternalNptrs
  !! Returns a vector of internal node numbers

  PROCEDURE, PUBLIC, PASS(obj) :: GetBoundaryNptrs => obj_GetBoundaryNptrs
  !! Returns a vector of boundary node numbers

  PROCEDURE, PUBLIC, PASS(obj) :: isBoundaryNode => obj_isBoundaryNode
  !! Returns true if a given global node number is a boundary node

  PROCEDURE, PASS(obj) :: isNodePresent1 => obj_isNodePresent1
  !! Returns true if a node number is present
  PROCEDURE, PASS(obj) :: isNodePresent2 => obj_isNodePresent2
  !! Returns true if a node number is present
  GENERIC, PUBLIC :: isNodePresent => isNodePresent1, isNodePresent2

  PROCEDURE, PUBLIC, PASS(obj) :: isAnyNodePresent => obj_isAnyNodePresent
  !! Returns true if any of the node number is present

  PROCEDURE, PUBLIC, PASS(obj) :: isAllNodePresent => obj_isAllNodePresent
  !! Returns true if All of the node number is present

  PROCEDURE, PUBLIC, PASS(obj) :: isElementPresent => obj_isElementPresent
  !! Returns true if a given element number is present

  PROCEDURE, PUBLIC, PASS(obj) :: isBoundaryElement => obj_isBoundaryElement
  !! Returns true if a given global element number is a boundary element

  PROCEDURE, PUBLIC, PASS(obj) :: isDomainBoundaryElement => &
    & obj_isDomainBoundaryElement
  !! Returns true if a given global element number is a boundary element

  PROCEDURE, PUBLIC, PASS(obj) :: isDomainFacetElement => &
    & obj_isDomainFacetElement
  !! Returns true if a given global element number is a boundary element

  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalInternalNodes => &
    & obj_GetTotalInternalNodes
  !! Returns the total number of internal nodes

  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalNodes => obj_GetTotalNodes
  !! Returns the total number of nodes

  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalElements => obj_size
  !! Returns the size of the mesh

  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalBoundaryElements => &
    & obj_GetTotalBoundaryElements
  !! Returns the total number of boundary element

  PROCEDURE, PASS(obj) :: GetBoundingBox1 => obj_GetBoundingBox1
  !! Returns the bounding box of the mesh
  PROCEDURE, PASS(obj) :: GetBoundingBox2 => obj_GetBoundingBox2
  !! Return the bounding box from the given nodes, and local_nptrs
  GENERIC, PUBLIC :: GetBoundingBox => GetBoundingBox1,  &
    & GetBoundingBox2
  !! Return the bounding box

  PROCEDURE, PUBLIC, PASS(obj) :: GetConnectivity => &
    & obj_GetConnectivity
  !! Returns  node numbers in an element

  PROCEDURE, PUBLIC, PASS(obj) :: GetNodeConnectivity => &
    & obj_GetNodeConnectivity
  !! Returns the node connectivity of the mesh elements

  PROCEDURE, PASS(obj) :: GetLocalNodeNumber1 =>  &
  & obj_GetLocalNodeNumber1
  !! Returns the local node number of a glocal node number
  PROCEDURE, PASS(obj) :: GetLocalNodeNumber2 =>  &
  & obj_GetLocalNodeNumber2
  !! Returns the local node number of a global node number
  GENERIC, PUBLIC :: GetLocalNodeNumber => GetLocalNodeNumber1, &
    & GetLocalNodeNumber2
  !! Returns the local node number of a global node number

  PROCEDURE, PASS(obj) :: GetGlobalNodeNumber1 =>  &
    & obj_GetGlobalNodeNumber1
  !! Returns the global node number of a local node number
  PROCEDURE, PASS(obj) :: GetGlobalNodeNumber2 =>  &
    & obj_GetGlobalNodeNumber2
  !! Returns the global node number of a local node number
  GENERIC, PUBLIC :: GetGlobalNodeNumber => GetGlobalNodeNumber1, &
    & GetGlobalNodeNumber2

  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalBoundaryNodes =>  &
    & obj_GetTotalBoundaryNodes
  !! Returns the total number of boundary nodes

  PROCEDURE, PASS(obj) :: GetGlobalElemNumber1 => obj_GetGlobalElemNumber1
  PROCEDURE, PASS(obj) :: GetGlobalElemNumber2 => obj_GetGlobalElemNumber2
  GENERIC, PUBLIC :: GetGlobalElemNumber => &
    & GetGlobalElemNumber1, GetGlobalElemNumber2

  !! Returns the global element number for a local element number
  PROCEDURE, PASS(obj) :: GetLocalElemNumber1 => obj_GetLocalElemNumber1
  PROCEDURE, PASS(obj) :: GetLocalElemNumber2 => obj_GetLocalElemNumber2
  GENERIC, PUBLIC :: GetLocalElemNumber => &
    & GetLocalElemNumber1, GetLocalElemNumber2
  !! Returns the local element number of a global element number

  PROCEDURE, PASS(obj) :: GetNodeToElements1 => obj_GetNodeToElements1
  PROCEDURE, PASS(obj) :: GetNodeToElements2 => obj_GetNodeToElements2
  GENERIC, PUBLIC :: GetNodeToElements => &
    & GetNodeToElements1, GetNodeToElements2
  !! Returns the element attached to a given global node number

  PROCEDURE, PASS(obj) :: GetNodeToNodes1 => obj_GetNodeToNodes1
  !! Returns global node number connected to a given global node
  PROCEDURE, PASS(obj) :: GetNodeToNodes2 => obj_GetNodeToNodes2
  !! Returns global node numbers connected to given global node numbers
  GENERIC, PUBLIC :: GetNodeToNodes => &
    & GetNodeToNodes1, GetNodeToNodes2
  !! Returns nodes connected to a given node number

  PROCEDURE, PUBLIC, PASS(obj) :: GetElementToElements => &
    & obj_GetElementToElements
  !! Returns local element number connected to a given local
  !! element number, it also gives information about the local
  !! facet number

  PROCEDURE, PUBLIC, PASS(obj) :: GetBoundaryElementData => &
    & obj_GetBoundaryElementData
  !! Returns boundary element data

  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalFacetElements => &
    & obj_GetTotalFacetElements
  !! Returns the total number of facet elements in the mesh

  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalBoundaryFacetElements => &
    & obj_GetTotalBoundaryFacetElements
  !! Returns the total number of boundary facet elements

  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalInternalFacetElements => &
    & obj_GetTotalInternalFacetElements
  !! Returns the total number of internal facet elements

  PROCEDURE, PUBLIC, PASS(obj) :: GetMasterCellNumber => &
    & obj_GetMasterCellNumber
  !! Returns the master cell number of a facet element

  PROCEDURE, PUBLIC, PASS(obj) :: GetSlaveCellNumber => &
    & obj_GetSlaveCellNumber
  !! Returns the slave cell number of a facet element

  PROCEDURE, PUBLIC, PASS(obj) :: GetCellNumber => &
    & obj_GetCellNumber
  !! Returns the master and slave cell number of a facet element

  PROCEDURE, PUBLIC, PASS(obj) :: GetLocalFacetID => &
    & obj_GetLocalFacetID
  !! Return the local facet id, so that we can Get reference element of
  !! the facet element

  PROCEDURE, PASS(obj) :: obj_GetFacetConnectivity1
  !! Return the node nubmers in the facet element
  PROCEDURE, PASS(obj) :: obj_GetFacetConnectivity2
  !! Return the node nubmers in the facet element of a cellElement
  GENERIC, PUBLIC :: GetFacetConnectivity => &
    & obj_GetFacetConnectivity1, &
    & obj_GetFacetConnectivity2
  !! Generic method to Get the connectivity of a facet element

  PROCEDURE, PUBLIC, PASS(obj) :: GetFacetElementType => &
    & obj_GetFacetElementType
  !! Returns the facet element type of a given cell element number

  PROCEDURE, PUBLIC, PASS(obj) :: GetOrder => &
    & obj_GetOrder
  !! Returns the order ofthe element of mesh

  PROCEDURE, PUBLIC, PASS(obj) :: GetNSD => &
    & obj_GetNSD
  !! Return the NSD

  PROCEDURE, PUBLIC, PASS(obj) :: GetXidimension => &
    & obj_GetXidimension
  !! Return the NSD

  PROCEDURE, PUBLIC, PASS(obj) :: GetMaterial => obj_GetMaterial
  !! returns the material id of a given medium

  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalMaterial => obj_GetTotalMaterial
  !! returns the total material

  PROCEDURE, PUBLIC, PASS(obj) :: GetQuery => obj_GetQuery
  !! Please use GetParam instead of GetQuery.
  !! They are the same. But I like the name GetParam

  PROCEDURE, PUBLIC, PASS(obj) :: GetParam => obj_GetQuery
  !! Get parameter of mesh

  PROCEDURE, PUBLIC, PASS(obj) :: GetMinElemNumber => obj_GetMinElemNumber
  !! Get minimum element number
  PROCEDURE, PUBLIC, PASS(obj) :: GetMaxElemNumber => obj_GetMaxElemNumber
  !! get maximum element number
  PROCEDURE, PUBLIC, PASS(obj) :: GetMinNodeNumber => obj_GetMinNodeNumber
  !! get minimum element number
  PROCEDURE, PUBLIC, PASS(obj) :: GetMaxNodeNumber => obj_GetMaxNodeNumber
  !! Get maximum node number

  ! SET:
  ! @SetMethods

  PROCEDURE, PUBLIC, PASS(obj) :: SetShowTime => obj_SetShowTime
  !! Set showTime option

  PROCEDURE, PASS(obj) :: SetBoundingBox1 => obj_setBoundingBox1
  !! Set the bounding box of the mesh
  PROCEDURE, PASS(obj) :: SetBoundingBox2 => obj_setBoundingBox2
  !! Set the bounding box from the given nodes, and local_nptrs
  GENERIC, PUBLIC :: SetBoundingBox => setBoundingBox1,  &
    & SetBoundingBox2
  !! Set the bounding box

  PROCEDURE, PASS(obj) :: SetSparsity1 => obj_setSparsity1
  !! Set the sparsity of sparse matrix
  PROCEDURE, PASS(obj) :: SetSparsity2 => obj_setSparsity2
  !! Set the sparsity of sparse matrix
  PROCEDURE, PASS(obj) :: SetSparsity3 => obj_setSparsity3
  !! Set the sparsity of sparse matrix
  PROCEDURE, PASS(obj) :: SetSparsity4 => obj_setSparsity4
  !! Set the sparsity of sparse matrix
  GENERIC, PUBLIC :: SetSparsity => setSparsity1, setSparsity2,  &
    & SetSparsity3, setSparsity4
  !! Generic method for setting the sparsity

  PROCEDURE, PUBLIC, PASS(obj) :: SetTotalMaterial => obj_SetTotalMaterial
  !! Adding a material ID of a medium which is mapped to the mesh
  PROCEDURE, PUBLIC, PASS(obj) :: SetMaterial => obj_setMaterial
  !! Adding a material ID of a medium which is mapped to the mesh
  PROCEDURE, PUBLIC, PASS(obj) :: SetFacetElementType => &
    & obj_SetFacetElementType
  !! Set the facet element type of a given cell number
  PROCEDURE, PUBLIC, PASS(obj) :: SetQuality => obj_setQuality
    !! Set mesh quality

END TYPE AbstractMesh_

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Allocate the size of the mesh

INTERFACE
  MODULE SUBROUTINE obj_Initiate(obj, hdf5, group)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! mesh object
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! Mesh file in hdf5 file format
    CHARACTER(*), INTENT(IN) :: group
    !! location in HDF5 file
  END SUBROUTINE obj_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                    Deallocate@Constructor
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
!                                                         isEmpty@Constructor
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_isEmpty(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_isEmpty
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
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
! This routine Initiate the local_nptrs data in mesh.
! This routine also Sets the number of nodes in the mesh (tNodes)
! This routine allocate obj%nodeData
! This routine Set localNodeNum and globalNodeNum data inside the
! nodeData

INTERFACE AbstractMeshImport
  MODULE SUBROUTINE obj_Import(obj, hdf5, group)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
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
  MODULE SUBROUTINE obj_GetNodeCoord(obj, nodeCoord, hdf5, group)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: nodeCoord(:, :)
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_GetNodeCoord
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
  MODULE SUBROUTINE obj_ExportToVTK(obj, vtkFile, nodeCoord, filename, &
    & OpenTag, Content, CloSetag)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    TYPE(VTKFile_), INTENT(INOUT) :: vtkFile
    REAL(DFP), OPTIONAL, INTENT(IN) :: nodeCoord(:, :)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: OpenTag
    !! Default is true
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: CloSetag
    !! Default is true
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: Content
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
  MODULE SUBROUTINE obj_DisplayElementData(obj, msg, unitno)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE obj_DisplayElementData
END INTERFACE

!----------------------------------------------------------------------------
!                                              DisplayFacetData@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Displays the element data

INTERFACE
  MODULE SUBROUTINE obj_DisplayInternalFacetData(obj, msg, unitno)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE obj_DisplayInternalFacetData
END INTERFACE

!----------------------------------------------------------------------------
!                                        DisplayBoundaryFacetData@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Displays the element data

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
!                                                         GetNNE@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-25
! summary:  Get number of nodes in element

INTERFACE
  MODULE FUNCTION obj_GetNNE(obj, globalElement) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
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
!                                                       GetElemNum@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns the global element numbers present in the mesh

INTERFACE
  MODULE FUNCTION obj_GetElemNum(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_GetElemNum
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
  MODULE FUNCTION obj_GetNptrs(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_GetNptrs
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
  MODULE FUNCTION obj_isBoundaryNode(obj, globalNode) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
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
  MODULE FUNCTION obj_isNodePresent1(obj, globalNode) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
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
  MODULE FUNCTION obj_isNodePresent2(obj, globalNode) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    LOGICAL(LGT) :: ans(SIZE(globalNode))
  END FUNCTION obj_isNodePresent2
END INTERFACE

!----------------------------------------------------------------------------
!                                                isAnyNodePresent@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns TRUE if any global node number is present

INTERFACE
  MODULE FUNCTION obj_isAnyNodePresent(obj, globalNode) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
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
  MODULE FUNCTION obj_isAllNodePresent(obj, globalNode) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
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
  MODULE FUNCTION obj_isElementPresent(obj, globalElement) &
    & RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
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
  MODULE FUNCTION obj_isBoundaryElement(obj, globalElement) &
    & RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    LOGICAL(LGT) :: ans
  END FUNCTION obj_isBoundaryElement
END INTERFACE

!----------------------------------------------------------------------------
!                                        isDomainBoundaryElement@GetMethods
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
  MODULE FUNCTION obj_isDomainBoundaryElement(obj, globalElement) &
    & RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    LOGICAL(LGT) :: ans
  END FUNCTION obj_isDomainBoundaryElement
END INTERFACE

!----------------------------------------------------------------------------
!                                        isDomainFacetElement@GetMethods
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
!                                           GetTotalInternalNodes@GetMethodss
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
!                                             GetTotalNodes@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: returns total number of nodes in the mesh

INTERFACE
  MODULE FUNCTION obj_GetTotalNodes(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalNodes
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
!                                            GetBoundingBox@MeshDataMethods
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
  MODULE FUNCTION obj_GetBoundingBox2(obj, nodes, local_nptrs)  &
    & RESULT(ans)
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

INTERFACE
  MODULE FUNCTION obj_GetConnectivity(obj, globalElement) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_GetConnectivity
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetNodeConnectivity@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: This routine returns global node numbers in a given global elem

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
  MODULE FUNCTION obj_GetLocalNodeNumber1(obj, globalNode) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B) :: ans(SIZE(globalNode))
  END FUNCTION obj_GetLocalNodeNumber1
END INTERFACE

!----------------------------------------------------------------------------
!                                         GetLocalNodeNumber@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: This routine returns the local node number from a global node number

INTERFACE
  MODULE FUNCTION obj_GetLocalNodeNumber2(obj, globalNode) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
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
! summary: This routine returns the Global node number from a local node number

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
! summary: This routine returns the Global node number from a local node number

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
  MODULE FUNCTION obj_GetLocalElemNumber1(obj, globalElement)  &
    & RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement(:)
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
  MODULE FUNCTION obj_GetLocalElemNumber2(obj, globalElement)  &
    & RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
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
  MODULE FUNCTION obj_GetNodeToElements1(obj, globalNode) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh data
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global node number
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
  MODULE FUNCTION obj_GetNodeToElements2(obj, globalNode) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    !! mesh data
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global node number
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !! A vector of local element number
  END FUNCTION obj_GetNodeToElements2
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
  MODULE FUNCTION obj_GetNodeToNodes1(obj, globalNode, IncludeSelf) &
    & RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    LOGICAL(LGT), INTENT(IN) :: IncludeSelf
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
  MODULE FUNCTION obj_GetNodeToNodes2(obj, globalNode, IncludeSelf) &
    & RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    LOGICAL(LGT), INTENT(IN) :: IncludeSelf
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_GetNodeToNodes2
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
    & onlyElements) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
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
  END FUNCTION obj_GetElementToElements
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
  MODULE FUNCTION obj_GetBoundaryElementData(obj, globalElement) &
    & RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
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
  MODULE FUNCTION obj_GetOrder(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetOrder
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
  MODULE FUNCTION obj_GetMaterial(obj, medium) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: medium
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetMaterial
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetMaterial@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns the materials id of a given medium

INTERFACE
  MODULE FUNCTION obj_GetTotalMaterial(obj) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalMaterial
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
  MODULE FUNCTION obj_GetMasterCellNumber(obj, facetElement, &
    & elementType)&
    & RESULT(ans)
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
  MODULE FUNCTION obj_GetSlaveCellNumber(obj, facetElement, &
    & elementType) RESULT(ans)
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
  MODULE FUNCTION obj_GetCellNumber(obj, facetElement, &
    & elementType) RESULT(ans)
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
  MODULE FUNCTION obj_GetLocalFacetID(obj, facetElement, &
    & elementType, isMaster) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: facetElement
    INTEGER(I4B), INTENT(IN) :: elementType
    LOGICAL(LGT), INTENT(IN) :: isMaster
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetLocalFacetID
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
  MODULE FUNCTION obj_GetFacetConnectivity1(obj, facetElement, &
    & elementType, isMaster) RESULT(ans)
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
  END FUNCTION obj_GetFacetConnectivity1
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
  MODULE FUNCTION obj_GetFacetConnectivity2(obj, globalElement, &
    & iface) RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    INTEGER(I4B), INTENT(IN) :: iface
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_GetFacetConnectivity2
END INTERFACE

!----------------------------------------------------------------------------
!                                            GetFacetElementType@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns the facet element type of the cell element number

INTERFACE
  MODULE FUNCTION obj_GetFacetElementType(obj, globalElement) &
    & RESULT(ans)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_GetFacetElementType
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetQuery@GetMethods
!----------------------------------------------------------------------------

INTERFACE AbstractMeshGetQuery
  MODULE SUBROUTINE obj_GetQuery(obj, &
    & isInitiated, isNodeToElementsInitiated, isNodeToNodesInitiated, &
    & isExtraNodeToNodesInitiated, isElementToElementsInitiated, &
    & isBoundaryDataInitiated, isFacetDataInitiated, uid, &
    & xidim, elemType, nsd, maxNptrs, minNptrs, &
    & maxElemNum, minElemNum, tNodes, tIntNodes, tElements, &
    & minX, minY, minZ, maxX, maxY, maxZ, &
    & x, y, z, tElements_topology_wise, tElemTopologies, elemTopologies)
    CLASS(AbstractMesh_), INTENT(IN) :: obj
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isInitiated, &
      & isNodeToElementsInitiated, isNodeToNodesInitiated, &
      & isExtraNodeToNodesInitiated, isElementToElementsInitiated, &
      & isBoundaryDataInitiated, isFacetDataInitiated

    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: uid, &
      & xidim, elemType, nsd, maxNptrs, minNptrs, &
      & maxElemNum, minElemNum, tNodes, tIntNodes, &
      & tElements, tElements_topology_wise(8), tElemTopologies,  &
      & elemTopologies(8)

    REAL(DFP), OPTIONAL, INTENT(OUT) :: minX, &
      & minY, minZ, maxX, maxY, maxZ, &
      & x, y, z
  END SUBROUTINE obj_GetQuery
END INTERFACE AbstractMeshGetQuery

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
!- This mapping is stored inside `obj%nodeData%globalNodeNum`
!- For a local node number i, obj%nodeData(i)%globalNodeNum denotes the
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
!- Before calling this routine make sure the `refelem` in mesh is allocated.
!- By using `refelem`, this routine forms the FacetElements.
!- This routine needs `nodeToElements` information, therefore, if
! `nodeToElements` is not Initiated then it calls `initiateNodeToelements`
! method
!- This method forms the following data:
!- `obj%elementData(ielem)%globalElements`
!- It also identifies those elements which are boundary element of mesh, and
!- Set `obj%elementData(ielem)%elementType=BOUNDARY_ELEMENT` for those element
!- Note that at this point boundary element are those which has at least
! one orphan face.
!- Note that at this point these boundary element can be interface element
! between two mesh, or domain-boundary element.

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
!                                                      SetSparsity@setMethod
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
    & ubound)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! [[Mesh_]] class
    TYPE(CSRMatrix_), INTENT(INOUT) :: mat
    !! [[CSRMatrix_]] object
    INTEGER(I4B), INTENT(IN) :: lbound
    INTEGER(I4B), INTENT(IN) :: ubound
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
!                                                      SetSparsity@setMethod
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
    !! [[Mesh_]] class
    CLASS(AbstractMesh_), INTENT(INOUT) :: colMesh
    !! [[Mesh_]] class
    INTEGER(I4B), INTENT(IN) :: nodeToNode(:)
    !! Node to node connectivity between obj and colMesh
    TYPE(CSRMatrix_), INTENT(INOUT) :: mat
    !! [[CSRMatrix_]] object
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
  END SUBROUTINE obj_SetSparsity3
END INTERFACE

!----------------------------------------------------------------------------
!                                                      SetSparsity@setMethod
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
  & rowGlobalToLocalNodeNum, rowLBOUND, rowUBOUND, colGlobalToLocalNodeNum, &
  & colLBOUND, colUBOUND, ivar, jvar)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    !! [[Mesh_]] class
    CLASS(AbstractMesh_), INTENT(INOUT) :: colMesh
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
  END SUBROUTINE obj_SetSparsity4
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetMaterial@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Set the materials id of a given medium

INTERFACE
  MODULE SUBROUTINE obj_SetTotalMaterial(obj, n)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: n
  END SUBROUTINE obj_SetTotalMaterial
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetMaterial@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Set the materials id of a given medium

INTERFACE
  MODULE SUBROUTINE obj_SetMaterial(obj, medium, material)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: medium
    INTEGER(I4B), INTENT(IN) :: material
  END SUBROUTINE obj_SetMaterial
END INTERFACE

!----------------------------------------------------------------------------
!                                            SetFacetElementType@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Set the facet element type of a given cell number

INTERFACE
  MODULE SUBROUTINE obj_SetFacetElementType(obj, globalElement, &
    & iface, facetElementType)
    CLASS(AbstractMesh_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    INTEGER(I4B), INTENT(IN) :: iface
    INTEGER(I4B), INTENT(IN) :: facetElementType
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
!
!----------------------------------------------------------------------------

END MODULE AbstractMesh_Class
