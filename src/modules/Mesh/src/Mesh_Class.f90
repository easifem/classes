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
! summary: `Mesh_Class` module contains data type for handling the mesh.
!
!{!pages/MeshData.md}
!{!pages/Mesh.md}

MODULE Mesh_Class
USE BaseType
USE GlobalData
USE ElementFactory
USE ExceptionHandler_Class
USE FPL, ONLY: ParameterList_
USE HDF5File_Class
IMPLICIT NONE
PRIVATE
CHARACTER( LEN = * ), PARAMETER :: modName = "MESH_CLASS"
TYPE( ExceptionHandler_ ), PUBLIC :: eMesh
INTEGER( I4B ), PARAMETER :: eUnitNo = 1003
CHARACTER( LEN = * ), PARAMETER :: eLogFile = "MESH_CLASS_EXCEPTION.txt"
  !! Exception handler

!----------------------------------------------------------------------------
!                                                                 Mesh_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	13 June 2021
! summary: 	This datatype contains the meta data of a mesh
!
!{!pages/Mesh.md}

TYPE :: Mesh_
  PRIVATE
    !! A dynamic vector of element pointer
  LOGICAL( LGT ) :: readFromFile = .TRUE.
    !! True if the mesh is read from a file
  INTEGER( I4B ) :: uid = 0
    !! Unique id of the mesh
  INTEGER( I4B ) :: xidim = 0
    !! xidimension of elements present inside the mesh
  INTEGER( I4B ) :: elemType = 0
    !! type of element present inside the mesh
  INTEGER( I4B ) :: nsd = 0
    !! number of spatial dimension
  INTEGER( I4B ) :: maxNptrs = 0
    !! largest node number present inside the mesh
  INTEGER( I4B ) :: minNptrs = 0
    !! minimum node number present inside the mesh
  INTEGER( I4B ) :: maxElemNum = 0
    !! largest element number present inside the mesh
  INTEGER( I4B ) :: minElemNum = 0
    !! minimum element number present inside the mesh
  INTEGER( I4B ) :: tNodes = 0
    !! total number of nodes present iside the mesh
  INTEGER( I4B ) :: tIntNodes = 0
    !! total number of internal nodes inside the mesh
  INTEGER( I4B ) :: tElements = 0
    !! total number of elements present inside the mesh
  REAL( DFP ) :: minX = 0.0
    !! minimum value of x coordinate
  REAL( DFP ) :: maxX = 0.0
    !! maximum value of x coordinate
  REAL( DFP ) :: minY = 0.0
    !! minimum value of y coordinate
  REAL( DFP ) :: maxY = 0.0
    !! maximum value of y coordinate
  REAL( DFP ) :: minZ = 0.0
    !! minimum value of z coordinate
  REAL( DFP ) :: maxZ = 0.0
    !! maximum value of z coordinate
  REAL( DFP ) :: X = 0.0
    !! x coorindate of centroid
  REAL( DFP ) :: Y = 0.0
    !! y coordinate of centroid
  REAL( DFP ) :: Z = 0.0
    !! z coordinate of centroid
  LOGICAL( LGT ) :: isInitiated = .FALSE.
    !! logical flag denoting for whether mesh data is initiated or not
  CLASS( ReferenceElement_ ), POINTER :: refelem => NULL()
    !! Reference element of the mesh
  TYPE( ReferenceElement_ ), ALLOCATABLE :: FacetElements( : )
    !! Facet Elements in the reference element
  INTEGER( I4B ), ALLOCATABLE :: local_elemNumber( : )
    !! Local element number
  REAL( DFP ), ALLOCATABLE :: nodeCoord( :, : )
    !! Nodal coordinates
  REAL( DFP ), ALLOCATABLE :: nodeVelocity( :, : )
    !! Nodal velocity of nodes
  REAL( DFP ), ALLOCATABLE :: nodeAcc( :, : )
    !! NodalAcceleration
  INTEGER( I4B ), ALLOCATABLE :: physicalTag( : )
    !! physical entities associated with the entity
  INTEGER( I4B ), ALLOCATABLE :: elemNumber( : )
    !! element number present inside the mesh
  INTEGER( I4B ), ALLOCATABLE :: boundingEntity( : )
    !! uid of bounding entity
  INTEGER( I4B ), ALLOCATABLE :: connectivity( :, : )
    !! connectivity matrix for the mesh
    !! each column of connectivity matrix denotes the node number

  INTEGER( I4B ), ALLOCATABLE :: LBndyIndex( : )
    !! For a given element if `LBndyIndex(iel) .eq. 0` then `iel` is not a
    !! a boundary element else it a boundary element which represents the
    !! index of `iel` in `BoundaryData()`.

  INTEGER( I4B ), ALLOCATABLE :: Nptrs( :, : )
    !! Node number of mesh `Nptrs( 1 : tNodes, 2 )`
    !! The first column contains the global node number
    !! The second column contains the indicator, >0 or -1
    !! If >zero then it is an internal node, and it contains the
    !! information of local node number
    !! if <zero, then it is an boundary node, and it contains the
    !! information of local node number
    !! The local node number information converts a given global node into
    !! local-node which can be used for accessing data inside `NodeToElem,
    !! NodeToNode`

  INTEGER( I4B ), ALLOCATABLE :: Local_Nptrs( : )
    !! Returns local node number from a global node number
    !! Its length is from 1 to maxNptrs
    !! Helpul in finding if a global node is present inside the mesh or not

  INTEGER( I4B ), ALLOCATABLE :: BoundaryNptrs( : )
    !! Node number of boundary of mesh
  INTEGER( I4B ), ALLOCATABLE :: InternalNptrs( : )
    !! Node number of internal nodes
  TYPE( IntVector_ ), ALLOCATABLE :: NodeToElem( : )
    !! `NodeToElem( iLocalNode )` denotes indx of elems in mesh which are
    !! directly connected to node `GlobalNptrs( iLocalNode )`
  TYPE( IntVector_ ), ALLOCATABLE :: ElemToElem( : )
    !! `ElemToElem( iel )` denotes data of elements
    !! connected to the element`iel`
  TYPE( IntVector_ ), ALLOCATABLE :: NTN( : )
    !! `NTN( iLocalNode )` denotes global node-ids that are connected
    !! to a `GlobalNode( iLocalNode )`
  TYPE( IntVector_ ), ALLOCATABLE :: BoundaryData( : )
    !! If `iel` is boundary element;
    !! then `Vec=BoundaryData( LBndyIndex(iel) ) `
    !! contains boundary data, where `vec(1)` is equal to `iel`, and
    !! `vec(2:)` are ids of local facets which are boundaries of mesh
  TYPE( IntVector_ ), ALLOCATABLE :: InternalBndyElemNum( : )
    !! To do
  TYPE( IntVector_ ), ALLOCATABLE :: InternalBoundaryData( : )
    !! To do
  CONTAINS
    PRIVATE
    PROCEDURE, PASS( obj ) :: Import => mesh_Import
      !! Read mesh from mesh file
    PROCEDURE, PUBLIC, PASS( obj ) :: Initiate => mesh_initiate
      !! Allocate size of a mesh
    FINAL :: mesh_final
    PROCEDURE, PUBLIC, PASS( obj ) :: DeallocateData => mesh_DeallocateData
      !! Deallocate data
    PROCEDURE, PUBLIC, PASS( obj ) :: Display => mesh_display
      !! Display the mesh
    PROCEDURE, PUBLIC, PASS( obj ) :: Size => mesh_size
      !! Returns the size of the mesh
    PROCEDURE, PUBLIC, PASS( Obj ) :: getBoundingEntity => &
      & mesh_getBoundingEntity
    PROCEDURE, PASS( Obj ) :: mesh_getNodeCoord_1
      !! Returns the nodal coordinates
    PROCEDURE, PASS( Obj ) :: mesh_getNodeCoord_2
      !! Returns the nodal coordinates
    PROCEDURE, PASS( Obj ) :: mesh_getNodeCoord_3
      !! Returns the nodal coordinates
    GENERIC, PUBLIC :: getNodeCoord => mesh_getNodeCoord_1, &
      & mesh_getNodeCoord_2, mesh_getNodeCoord_3
      !! Returns the nodal coordinates
    PROCEDURE, PASS( Obj ) :: mesh_setNodeCoord_1
      !! set the nodal coordinates
    PROCEDURE, PASS( Obj ) :: mesh_setNodeCoord_2
      !! set the nodal coordinates
    PROCEDURE, PASS( Obj ) :: mesh_setNodeCoord_3
      !! set the nodal coordinates
    GENERIC, PUBLIC :: setNodeCoord => &
      & mesh_setNodeCoord_1, mesh_setNodeCoord_2, &
      & mesh_setNodeCoord_3
    PROCEDURE, PUBLIC, PASS( Obj ) :: getNptrs => mesh_getNptrs
      !! Returns the node number of mesh

    !> Mesh data related
    PROCEDURE, PUBLIC, PASS( obj ) :: InitiateLocalNptrs => mesh_InitiateLocalNptrs
    PROCEDURE, PUBLIC, PASS( obj ) :: InitiateLocalElementNumbers => &
      & mesh_InitiateLocalElementNumbers
      !! returns `.true.` if `InternalBoundaryData` array is allocated
    PROCEDURE, PUBLIC, PASS( obj ) :: InitiateNodeToElements => &
      & mesh_InitiateNodeToElements
      !! Initiate node to node data
    PROCEDURE, PUBLIC, PASS( obj ) :: InitiateNodeToNodes => &
      & mesh_InitiateNodetoNodes
      !! Initiate Node to nodes mapping
    PROCEDURE, PUBLIC, PASS( obj ) :: InitiateElementToElements => &
      & mesh_InitiateElementToElements
      !! Initiate element to elements mapping
    PROCEDURE, PUBLIC, PASS( Obj ) :: InitiateBoundaryData => &
      & mesh_InitiateBoundaryData
      !! Initiate boundary data
    PROCEDURE, PUBLIC, PASS( obj ) :: InitiateInternalNptrs => &
      & mesh_InitiateInternalNptrs
      !! Initiate internal node numbers

    PROCEDURE, PUBLIC, PASS( Obj ) :: isBoundaryNode => &
      & mesh_isBoundaryNode
      !! Returns true if a given global node number is a boundary node
    PROCEDURE, PUBLIC, PASS( Obj ) :: isLocalElementNumbersInitiated => &
      & mesh_isLocalElementNumbersInitiated
      !! Returns true if vector related to local element num is initiated
    PROCEDURE, PUBLIC, PASS( obj ) :: isNodePresent => &
      & mesh_isNodePresent
      !! Returns true if a node number is present
    PROCEDURE, PUBLIC, PASS( obj ) :: isNodeToNodesInitiated => &
      & mesh_isNodeToNodesInitiated
      !! returns `.true.` if `NToN` array is allocated
    PROCEDURE, PUBLIC, PASS( obj ) :: isNodeToElementsInitiated => &
      & mesh_isNodeToElementsInitiated
      !! returns `.true.` if `NodeToElem` array is allocated
    PROCEDURE, PUBLIC, PASS( obj ) :: isElementToElementsInitiated => &
      & mesh_isElementToElementsInitiated
      !! returns `.true.` if `ElemToElem` array is allocated
    PROCEDURE, PUBLIC, PASS( obj ) :: isConnectivityInitiated => &
      & mesh_isConnectivityInitiated
      !! returns `.true.` if `Connectivity` array is allocated
    PROCEDURE, PUBLIC, PASS( obj ) :: isBoundaryDataInitiated => &
      & mesh_isBoundaryDataInitiated
      !! returns `.true.` if `BoundaryData` array is allocated
    PROCEDURE, PUBLIC, PASS( obj ) :: isInternalNptrsInitiated => &
      & mesh_isInternalNptrsInitiated
      !! returns `.true.` if `InternalNptrs` array is allocated
    PROCEDURE, PUBLIC, PASS( obj ) :: isBoundaryNptrsInitiated => &
      & mesh_isBoundaryNptrsInitiated
    PROCEDURE, PUBLIC, PASS( obj ) :: isLocalNptrsInitiated => &
      & mesh_isLocalNptrsInitiated
      !! returns `.true.` if `Local_Nptrs` array is allocated
    PROCEDURE, PUBLIC, PASS( obj ) :: isInternalBoundaryDataInitiated => &
      & mesh_isInternalBoundaryDataInitiated
    PROCEDURE, PUBLIC, PASS( Obj ) :: isBoundaryElement => &
      & mesh_isBoundaryElement
      !! Returns true, if an element is a boundary element

    PROCEDURE, PUBLIC, PASS( obj ) :: getTotalInternalNodes => mesh_getTotalInternalNodes
      !! Returns the total number of internal nodes
    PROCEDURE, PUBLIC, PASS( obj ) :: getTotalNodes => mesh_getTotalNodes
      !! Returns the total number of nodes
    PROCEDURE, PUBLIC, PASS( obj ) :: getTotalBoundaryNodes => mesh_getTotalBoundaryNodes
      !! Returns the total number of boundary nodes
    PROCEDURE, PUBLIC, PASS( obj ) :: getTotalBoundaryElements => mesh_getTotalBoundaryElements
      !! Returns the total number of boundary element
    PROCEDURE, PUBLIC, PASS( obj ) :: getBoundingBox => mesh_getBoundingBox
      !! Returns the bounding box of the mesh
    PROCEDURE, PUBLIC, PASS( obj ) :: getConnectivity => &
      & mesh_getConnectivity
      !! Returns  node numbers in an element
    PROCEDURE, PASS( obj ) :: mesh_getLocalNodeNumber1
      !! Returns the local node number of a glocal node number
    PROCEDURE, PASS( obj ) :: mesh_getLocalNodeNumber2
      !! Returns the local node number of a global node number
    GENERIC, PUBLIC :: getLocalNptrs => mesh_getLocalNodeNumber1, &
      & mesh_getLocalNodeNumber2
      !! Returns the local node number of a global node number
    PROCEDURE, PASS( obj ) :: mesh_getGlobalNodeNumber1
      !! Returns the global node number of a local node number
    PROCEDURE, PASS( obj ) :: mesh_getGlobalNodeNumber2
      !! Returns the global node number of a local node number
    GENERIC, PUBLIC :: getGlobalNptrs => mesh_getGlobalNodeNumber1, &
      & mesh_getGlobalNodeNumber2
    PROCEDURE, PASS( obj ) :: mesh_getGlobalElemNumber_1
    PROCEDURE, PASS( obj ) :: mesh_getGlobalElemNumber_2
    GENERIC, PUBLIC :: getGlobalElemNumber => &
      & mesh_getGlobalElemNumber_1, mesh_getGlobalElemNumber_2
      !! Returns the global element number for a local element number
    PROCEDURE, PASS( obj ) :: mesh_getLocalElemNumber_1
    PROCEDURE, PASS( obj ) :: mesh_getLocalElemNumber_2
    GENERIC, PUBLIC :: getLocalElemNumber => &
      & mesh_getLocalElemNumber_1, mesh_getLocalElemNumber_2
      !! Returns the local element number of a global element number

    PROCEDURE, PUBLIC, PASS( obj ) :: getNodeToElements => &
      & mesh_getNodeToElements
      !! Returns the element attached to a given global node number
    PROCEDURE, PUBLIC, PASS( obj ) :: getNodeToNodes => &
      & mesh_getNodeToNodes
      !! Returns nodes connected to a given node number
    PROCEDURE, PUBLIC, PASS( Obj ) :: getElementToElements => &
      & mesh_getElementToElements
      !! Returns local element number connected to a given local
      !! element number, it also gives information about the local
      !! facet number
    PROCEDURE, PUBLIC, PASS( Obj ) :: getBoundaryElementData => &
      & mesh_getBoundaryElementData
      !! Returns boundary element data
    PROCEDURE, PUBLIC, PASS( Obj ) :: getInternalNptrs => &
      & mesh_getInternalNptrs
      !! Returns internal node number
END TYPE Mesh_

!----------------------------------------------------------------------------
!                                                                     Mesh
!----------------------------------------------------------------------------

PUBLIC :: Mesh_
! TYPE( Mesh_ ), PARAMETER, PUBLIC :: TypeMesh = Mesh_( )

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!>
! `MeshPointer_` is a userdefine datatype which contains the pointer to
! a mesh
TYPE :: MeshPointer_
  TYPE( Mesh_ ), POINTER :: Ptr => NULL( )
END TYPE MeshPointer_

PUBLIC :: MeshPointer_

!----------------------------------------------------------------------------
!                                                          Read@MeshMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	18 June 2021
! summary: This routine reads the mesh from a meshFile which is an hdf5
! file
!
!### Introduction
! The xidimension denotes the topology of the mesh element
!
! - xidim = 0, implies mesh is made of point elements
! - xidim = 1, implies mesh is made of line elements
! - xidim = 2, implies mesh is made of surface elements
! - xidim = 3, implies mesh is made of volume elements
!
! `id` is the unique id of the mesh
!
! This routine reads the following
! - meshdata%uid
! - meshdata%xidim
! - meshData%elemType
! - meshData%minX
! - meshData%minY
! - meshData%minZ
! - meshData%maxX
! - meshData%maxY
! - meshData%maxZ
! - meshData%X
! - meshData%Y
! - meshData%Z
! - meshData%tElements
! - meshData%tIntNodes
! - meshData%nodeCoord
! - meshData%physicalTag
! - meshData%InternalNptrs
! - meshData%elemNumber
! - meshData%connectivity
! - meshData%boundingEntity

INTERFACE
MODULE SUBROUTINE mesh_Import( obj, meshFile, xidim, id )
  CLASS( Mesh_ ), INTENT( INOUT ) :: obj
  TYPE( HDF5File_ ), INTENT( INOUT ) :: meshFile
  INTEGER( I4B ), INTENT( IN ) :: xidim
  INTEGER( I4B ), INTENT( IN ) :: id
END SUBROUTINE mesh_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Initiate@MeshMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 March 2021
! summary: Allocate the size of the mesh
!
!### Introduction
!
! This routine initiate the the mesh by reading the data stored inside
! the HDF5 file meshFile

INTERFACE
MODULE SUBROUTINE mesh_initiate( obj, meshFile, xidim, id )
  CLASS( Mesh_ ), INTENT( INOUT) :: obj
    !! mesh object
  TYPE( HDF5File_ ), INTENT( INOUT ) :: meshFile
    !! Mesh file
  INTEGER( I4B ), INTENT( IN ) :: xidim
    !! Xidimension of the mesh
  INTEGER( I4B ), INTENT( IN ) :: id
    !! Id of mesh
END SUBROUTINE mesh_initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Mesh@MeshMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 March 2021
! summary: Function for constructing an instance of [[Mesh_]]

INTERFACE
MODULE FUNCTION Mesh_Constructor1( meshFile, xidim, id ) RESULT( ans )
  TYPE( Mesh_ ) :: ans
  TYPE( HDF5File_ ), INTENT( INOUT ) :: meshFile
  INTEGER( I4B ), INTENT( IN ) :: xidim
  INTEGER( I4B ), INTENT( IN ) :: id
END FUNCTION Mesh_Constructor1
END INTERFACE

!>
! Generic function for constructing [[mesh_]]
INTERFACE Mesh
  MODULE PROCEDURE Mesh_Constructor1
END INTERFACE Mesh

PUBLIC :: Mesh

!----------------------------------------------------------------------------
!                                                   Mesh_Pointer@MeshMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 June 2021
! summary: This function returns a pointer to an instance of mesh_ object

INTERFACE
MODULE FUNCTION Mesh_Constructor_1( meshFile, xidim, id ) RESULT( ans )
  CLASS( Mesh_ ), POINTER :: ans
  TYPE( HDF5File_ ), INTENT( INOUT ) :: meshFile
  INTEGER( I4B ), INTENT( IN ) :: xidim
  INTEGER( I4B ), INTENT( IN ) :: id
END FUNCTION Mesh_Constructor_1
END INTERFACE

INTERFACE Mesh_Pointer
  MODULE PROCEDURE Mesh_Constructor_1
END INTERFACE Mesh_Pointer

PUBLIC :: Mesh_Pointer

!----------------------------------------------------------------------------
!                                                 DeallocateData@MeshMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	25 March 2021
! summary: Free up the memory stored in [[mesh_]]
!
!### Introduction
!
! Free up the memory stored in [[mesh_]] data type
!
!### Usage
!
!```fortran
!call obj%deallocateData( )
!```end fortran

INTERFACE
MODULE SUBROUTINE mesh_DeallocateData( obj )
  CLASS( Mesh_ ), INTENT( INOUT) :: obj
END SUBROUTINE mesh_DeallocateData
END INTERFACE

INTERFACE DeallocateData
  MODULE PROCEDURE mesh_DeallocateData
END INTERFACE DeallocateData

PUBLIC :: DeallocateData

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE mesh_final( obj )
  TYPE( Mesh_ ), INTENT( INOUT ) :: obj
END SUBROUTINE mesh_final
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Display@MeshMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	12 June 2021
! summary: 	Displays the content of [[mesh_]] datatype
!
!### Introduction
! 	This routine displays the content of [[mesh_]] datatype
!
!### Usage
!
!```fortran
!	call display( obj, 'mesh', stdout )
!	call obj%display( 'mesh', stdout )
!```

INTERFACE
MODULE SUBROUTINE mesh_display( obj, msg, UnitNo )
  CLASS( Mesh_ ), INTENT( INOUT ) :: obj
    !! mesh object
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
    !! message on screen
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: UnitNo
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
!                                              getTotalElements@MeshMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 June 2021
! summary: Returns total elements in the mesh
!
!### Introduction
!
! Returns the total number of elements in the mesh.
!
!### Usage
!
!```fortran
!	telem = obj % SIZE( obj )
!```

INTERFACE
MODULE FUNCTION mesh_size( obj ) RESULT( ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
    !! mesh object
  INTEGER( I4B ) :: ans
END FUNCTION mesh_size
END INTERFACE

!----------------------------------------------------------------------------
!                                             getBoundingEntity@MeshMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 June 2021
! summary: Returns bounding entity of the mesh

INTERFACE
MODULE PURE FUNCTION mesh_getBoundingEntity( obj ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
END FUNCTION mesh_getBoundingEntity
END INTERFACE

!----------------------------------------------------------------------------
!                                                   getNodeCoord@MeshMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 June 2021
! summary: Returns nodal coordinates

INTERFACE
MODULE PURE FUNCTION mesh_getNodeCoord_1( obj, GlobalNode ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: GlobalNode
  REAL( DFP ) :: ans( 3 )
END FUNCTION mesh_getNodeCoord_1
END INTERFACE

!----------------------------------------------------------------------------
!                                                   getNodeCoord@MeshMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 June 2021
! summary: Returns nodal coordinates

INTERFACE
MODULE PURE FUNCTION mesh_getNodeCoord_2( obj, GlobalNode ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: GlobalNode( : )
  REAL( DFP ) :: ans( 3, SIZE( GlobalNode ) )
END FUNCTION mesh_getNodeCoord_2
END INTERFACE

!----------------------------------------------------------------------------
!                                                   getNodeCoord@MeshMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 June 2021
! summary: Returns nodal coordinates

INTERFACE
MODULE PURE FUNCTION mesh_getNodeCoord_3( obj ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  REAL( DFP ), ALLOCATABLE :: ans( :, : )
END FUNCTION mesh_getNodeCoord_3
END INTERFACE

!----------------------------------------------------------------------------
!                                                  setNodeCoord@MeshMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 June 2021
! summary: set the nodal coordinates

INTERFACE
MODULE PURE SUBROUTINE mesh_setNodeCoord_1( obj, GlobalNode, nodeCoord )
  CLASS( Mesh_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: GlobalNode
  REAL( DFP ), INTENT( IN ) :: nodeCoord( 3 )
END SUBROUTINE mesh_setNodeCoord_1
END INTERFACE

!----------------------------------------------------------------------------
!                                                  setNodeCoord@MeshMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 June 2021
! summary: set the nodal coordinates

INTERFACE
MODULE PURE SUBROUTINE mesh_setNodeCoord_2( obj, GlobalNode, nodeCoord )
  CLASS( Mesh_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: GlobalNode( : )
  REAL( DFP ), INTENT( IN ) :: nodeCoord( 3, SIZE( GlobalNode ) )
END SUBROUTINE mesh_setNodeCoord_2
END INTERFACE

!----------------------------------------------------------------------------
!                                                  setNodeCoord@MeshMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 June 2021
! summary: set the nodal coordinates

INTERFACE
MODULE PURE SUBROUTINE mesh_setNodeCoord_3( obj, nodeCoord )
  CLASS( Mesh_ ), INTENT( INOUT ) :: obj
  REAL( DFP ), INTENT( IN ) :: nodeCoord( :, : )
END SUBROUTINE mesh_setNodeCoord_3
END INTERFACE

!----------------------------------------------------------------------------
!                                                   getNptrs@MeshMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 June 2021
! summary: Returns nodal coordinates

INTERFACE
MODULE PURE FUNCTION mesh_getNptrs( obj ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), ALLOCATABLE :: ans( :, : )
END FUNCTION mesh_getNptrs
END INTERFACE

!----------------------------------------------------------------------------
!                                        InitiateLocalNptrs@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE mesh_InitiateLocalNptrs( obj )
  CLASS( Mesh_ ), INTENT( INOUT ) :: obj
END SUBROUTINE mesh_InitiateLocalNptrs
END INTERFACE

!----------------------------------------------------------------------------
!                               InitiateLocalElementNumbers@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 June 2021
! summary: Initiate local element numbers

INTERFACE
MODULE SUBROUTINE mesh_InitiateLocalElementNumbers( obj )
  CLASS( Mesh_ ), INTENT( INOUT ) :: obj
END SUBROUTINE mesh_InitiateLocalElementNumbers
END INTERFACE

!----------------------------------------------------------------------------
!                                      initiateNodeToElements@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	15 June 2021
! summary: 	generate Elements surrounding a node mapping
!
!### Introduction
!
! This subroutine generate Elements surrounding a node mapping
! - The mapping is stored in the array called `NodeToElem`
! - `NodeToElem` is a vector of type [[IntVector_]]
! - The size of `NodeToElem` vector is same as `obj%Nptrs`, that is total number of nodes in the mesh
! - In `NodeToElem(i)`, i denotes the local node number, and it will give the local element number surrounding the local node `i`
! - Always use method called `getNodeToElements()` to access this information
!
! @warning
! Always use the mapping between global node number and local node number to
! avoid segmentation fault
! @endwarning
!
!### Usage
!
! ```fortran
!	call obj%initiateNodeToElements()
! ```

INTERFACE
MODULE SUBROUTINE mesh_InitiateNodeToElements( obj )
  CLASS( Mesh_ ), INTENT( INOUT ) :: obj
    !! mesh data
END SUBROUTINE mesh_InitiateNodeToElements
END INTERFACE

!----------------------------------------------------------------------------
!                                        InitiateNodeToNode@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	15 June 2021
! summary: 	Initiate node to node connectivity data

INTERFACE
MODULE SUBROUTINE mesh_InitiateNodetoNodes( obj )
  CLASS( Mesh_ ), INTENT( INOUT ) :: obj
    !! mesh data
END SUBROUTINE mesh_InitiateNodetoNodes
END INTERFACE

!----------------------------------------------------------------------------
!                                 InitiateElementToElements@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	15 June 2021
! summary: 	Initiate boundary data

INTERFACE
MODULE SUBROUTINE mesh_InitiateElementToElements( obj )
  CLASS( Mesh_ ), INTENT( INOUT ) :: obj
    !! mesh data
END SUBROUTINE mesh_InitiateElementToElements
END INTERFACE

!----------------------------------------------------------------------------
!                                 InitiateBoundaryData@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	15 June 2021
! summary: 	Initiate boundary data

INTERFACE
MODULE SUBROUTINE mesh_InitiateBoundaryData( obj )
  CLASS( Mesh_ ), INTENT( INOUT ) :: obj
    !! mesh data
END SUBROUTINE mesh_InitiateBoundaryData
END INTERFACE

!----------------------------------------------------------------------------
!                                 InitiateInternalNptrs@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	15 June 2021
! summary: 	Initiate internal node numbers

INTERFACE
MODULE SUBROUTINE mesh_InitiateInternalNptrs( obj )
  CLASS( Mesh_ ), INTENT( INOUT ) :: obj
    !! mesh data
END SUBROUTINE mesh_InitiateInternalNptrs
END INTERFACE

!----------------------------------------------------------------------------
!                                                 isBoundaryNode@MeshData
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 June 2021
! summary: This function returns true if given global node is boundary node

INTERFACE
MODULE PURE FUNCTION mesh_isBoundaryNode( obj, GlobalNode ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: GlobalNode
  LOGICAL( LGT ) :: ans
END FUNCTION mesh_isBoundaryNode
END INTERFACE

!----------------------------------------------------------------------------
!                             isLocalElementNumbersInitiated@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 June 2021
! summary: Returns true if LocalElementNumbers data initiated

INTERFACE
MODULE PURE FUNCTION mesh_isLocalElementNumbersInitiated( obj ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ) :: ans
END FUNCTION mesh_isLocalElementNumbersInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                                             isNodePresent@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 June 2021
! summary: Returns  true if given global node number is present

INTERFACE
MODULE PURE FUNCTION mesh_isNodePresent( obj, GlobalNode ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: GlobalNode
  LOGICAL( LGT ) :: ans
END FUNCTION mesh_isNodePresent
END INTERFACE

!----------------------------------------------------------------------------
!                                    isNodeToNodesInitiated@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION mesh_isNodeToNodesInitiated( obj ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ) :: ans
END FUNCTION mesh_isNodeToNodesInitiated
END INTERFACE


!----------------------------------------------------------------------------
!                                 isNodeToElementsInitiated@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION mesh_isNodeToElementsInitiated( obj ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ) :: ans
END FUNCTION mesh_isNodeToElementsInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                                 isElementToElementsInitiated@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION mesh_isElementToElementsInitiated( obj ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ) :: ans
END FUNCTION mesh_isElementToElementsInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                                   isConnectivityInitiated@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION mesh_isConnectivityInitiated( obj ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ) :: ans
END FUNCTION mesh_isConnectivityInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                                 isBoundaryDataInitiated@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION mesh_isBoundaryDataInitiated( obj ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ) :: ans
END FUNCTION mesh_isBoundaryDataInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                                 isInternalNptrsInitiated@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION mesh_isInternalNptrsInitiated( obj ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ) :: ans
END FUNCTION mesh_isInternalNptrsInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                                 isLocalNptrsInitiated@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION mesh_isLocalNptrsInitiated( obj ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ) :: ans
END FUNCTION mesh_isLocalNptrsInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                            isInternalBoundaryDataInitiated@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION mesh_isInternalBoundaryDataInitiated( obj ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ) :: ans
END FUNCTION mesh_isInternalBoundaryDataInitiated
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 June 2021
! summary: Returns true if an element is boundary element

INTERFACE
MODULE PURE FUNCTION mesh_isBoundaryElement( obj, iel ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: iel
  LOGICAL( LGT ) :: ans
END FUNCTION mesh_isBoundaryElement
END INTERFACE

!----------------------------------------------------------------------------
!                                      getTotalInternalNodes@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION mesh_getTotalInternalNodes( obj ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: ans
END FUNCTION mesh_getTotalInternalNodes
END INTERFACE

!----------------------------------------------------------------------------
!                                             getTotalNodes@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION mesh_getTotalNodes( obj ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: ans
END FUNCTION mesh_getTotalNodes
END INTERFACE

!----------------------------------------------------------------------------
!                                     getTotalBoundaryNodes@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION mesh_getTotalBoundaryNodes( obj ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: ans
END FUNCTION mesh_getTotalBoundaryNodes
END INTERFACE

!----------------------------------------------------------------------------
!                                  getTotalBoundaryElements@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION mesh_getTotalBoundaryElements( obj ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: ans
END FUNCTION mesh_getTotalBoundaryElements
END INTERFACE

!----------------------------------------------------------------------------
!                                            getBoundingBox@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION mesh_getBoundingBox( obj ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  TYPE( BoundingBox_ ) :: ans
END FUNCTION mesh_getBoundingBox
END INTERFACE

!----------------------------------------------------------------------------
!                                          getConnectivity@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 June 2021
! summary: This routine returns node number in an element

INTERFACE
MODULE PURE FUNCTION mesh_getConnectivity( obj, iel ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: iel
  INTEGER( I4B ), ALLOCATABLE :: Ans( : )
END FUNCTION mesh_getConnectivity
END INTERFACE

!----------------------------------------------------------------------------
!                                          getLocalNptrs@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 June 2021
! summary: This function returns the local node number from global node num
!
!### Introduction
!
! This function returns the local node numbers from global node numbers.

INTERFACE
MODULE PURE FUNCTION mesh_getLocalNodeNumber1( obj, GlobalNode ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: GlobalNode( : )
  INTEGER( I4B ) :: Ans( SIZE( GlobalNode ) )
END FUNCTION mesh_getLocalNodeNumber1
END INTERFACE

!----------------------------------------------------------------------------
!                                          getLocalNptrs@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 June 2021
! summary: This routine returns the local node number from a global node number

INTERFACE
MODULE PURE FUNCTION mesh_getLocalNodeNumber2( obj, GlobalNode ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: GlobalNode
  INTEGER( I4B ) :: Ans
END FUNCTION mesh_getLocalNodeNumber2
END INTERFACE

!----------------------------------------------------------------------------
!                                          getGlobalNptrs@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 June 2021
! summary: This function returns the Global node number from local node num

INTERFACE
MODULE PURE FUNCTION mesh_getGlobalNodeNumber1( obj, LocalNode ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: LocalNode( : )
  INTEGER( I4B ) :: Ans( SIZE( LocalNode ) )
END FUNCTION mesh_getGlobalNodeNumber1
END INTERFACE

!----------------------------------------------------------------------------
!                                          getGlobalNptrs@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 June 2021
! summary: This routine returns the Global node number from a local node number

INTERFACE
MODULE PURE FUNCTION mesh_getGlobalNodeNumber2( obj, LocalNode ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: LocalNode
  INTEGER( I4B ) :: Ans
END FUNCTION mesh_getGlobalNodeNumber2
END INTERFACE

!----------------------------------------------------------------------------
!                                       getGlobalElemNumber@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 June 2021
! summary: This function returns the Global node number from local node num

INTERFACE
MODULE PURE FUNCTION mesh_getGlobalElemNumber_1( obj, LocalElem ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: LocalElem( : )
  INTEGER( I4B ) :: Ans( SIZE( LocalElem ) )
END FUNCTION mesh_getGlobalElemNumber_1
END INTERFACE

!----------------------------------------------------------------------------
!                                       getGlobalElemNumber@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 June 2021
! summary: This routine returns the Global node number from a local node number

INTERFACE
MODULE PURE FUNCTION mesh_getGlobalElemNumber_2( obj, LocalElem ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: LocalElem
  INTEGER( I4B ) :: Ans
END FUNCTION mesh_getGlobalElemNumber_2
END INTERFACE

!----------------------------------------------------------------------------
!                                       getLocalElemNumber@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 June 2021
! summary: This function returns the local element number

INTERFACE
MODULE PURE FUNCTION mesh_getLocalElemNumber_1( obj, GlobalElem ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: GlobalElem( : )
  INTEGER( I4B ) :: Ans( SIZE( GlobalElem ) )
END FUNCTION mesh_getLocalElemNumber_1
END INTERFACE

!----------------------------------------------------------------------------
!                                       getLocalElemNumber@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 June 2021
! summary: This function returns the local element number

INTERFACE
MODULE PURE FUNCTION mesh_getLocalElemNumber_2( obj, GlobalElem ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: GlobalElem
  INTEGER( I4B ) :: Ans
END FUNCTION mesh_getLocalElemNumber_2
END INTERFACE

!----------------------------------------------------------------------------
!                                 isBoundaryNptrsInitiated@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION mesh_isBoundaryNptrsInitiated( obj ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ) :: ans
END FUNCTION mesh_isBoundaryNptrsInitiated
END INTERFACE


!----------------------------------------------------------------------------
!                                         getNodeToElements@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	15 June 2021
! summary: Returns the element numbers which are connected to the a global node
!
!### Introduction
! This function returns the elements containing the global node number `GlobalNode`
!
!@note
! 	If the node number `GlobalNode` is not present inside the mesh then
! the returned vector of integer has size 0
!@endnote

INTERFACE
MODULE PURE FUNCTION mesh_getNodeToElements( obj, GlobalNode ) RESULT( ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
    !! mesh data
  INTEGER( I4B ), INTENT( IN ) :: GlobalNode
    !! global node number
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
    !! A vector of local element number
END FUNCTION mesh_getNodeToElements
END INTERFACE

!----------------------------------------------------------------------------
!                                           getNodeToNodes@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 June 2021
! summary: 	Returns the node surrounding a node
!
!### Introduction
! This fucntion returns the vector of node numbers which surrounds a given
! node number `GlobalNode`.
! - If `IncludeSelf` is true then, in the returned vector of integer, node number GlobalNode is also present
!- If `IncludeSelf` is false then, in the returned vector of integer, node number `GlobalNode` is not present
!
!@note
! 	If the node number `GlobalNode` is not present in the mesh then the
! returned vector of integer has zero length
!@endnote

INTERFACE
MODULE PURE FUNCTION mesh_getNodeToNodes( obj, GlobalNode, IncludeSelf ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: GlobalNode
  LOGICAL( LGT ), INTENT( IN ) :: IncludeSelf
  INTEGER( I4B ), ALLOCATABLE :: Ans( : )
END FUNCTION mesh_getNodeToNodes
END INTERFACE

!----------------------------------------------------------------------------
!                                      getElementToElements@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 June 2021
! summary: Returns element to element connectivity information
!
!### Introduction
! This routine returns element to element connectivity information for a given local element number `iel`
!
! If OnlyElements is absent or it is set to FALSE then, this routine returns
! **full information** about elements surrounding
! element `iel`
! - Each Row of `ans` denotes the element to which `iel` is connected to
! - Column-1 of `ans` denotes element number
! - Column-2 denotes the local face number of element `iel`, and
! - Column-3 denotes the local face number of element whose element number is
! given in the column-1
!
! If `OnlyElements` is present and it is TRUE then, this routine returns only the information of elements connectved ot an element.
!

INTERFACE
MODULE PURE FUNCTION mesh_getElementToElements( obj, iel, onlyElements ) RESULT( ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
    !! mesh data
  INTEGER( I4B ), INTENT( IN ) :: iel
    !! element number
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: onlyElements
    !! If onlyElements is absent or it is FALSE then full information about the elements connected to element iel is given
    !! If onlyElements is present and it is TRUE then only the information about the elements connected to element iel is given
  INTEGER( I4B ), ALLOCATABLE :: ans( :, : )
    !! list of elements surrounding elements
END FUNCTION mesh_getElementToElements
END INTERFACE

!----------------------------------------------------------------------------
!                                     getBoundaryElementData@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION mesh_getBoundaryElementData( obj, iel ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: iel
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
END FUNCTION mesh_getBoundaryElementData
END INTERFACE

!----------------------------------------------------------------------------
!                                     getInternalNptrs@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION mesh_getInternalNptrs( obj ) RESULT( Ans )
  CLASS( Mesh_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
END FUNCTION mesh_getInternalNptrs
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END MODULE Mesh_Class
