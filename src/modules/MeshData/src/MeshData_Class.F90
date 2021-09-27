MODULE MeshData_Class
  !! This module defines Mesh data class
USE BaseType
USE GlobalData
USE Mesh_Class
USE FE
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                                 MeshData_
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! [[MeshData_]] contains data related to a mesh
!
! @note
! In **EASIFEM** mesh-data is separated from the `Mesh_` mainly because
! very often we only require connectivity information which is already
! stored inside the `Mesh_`.
! @endnote

TYPE :: MeshData_
  !! [[MeshData_]] is contains mesh realated data
  INTEGER( I4B ) :: MaxNptrs
    !! Largest node number present inside mesh
  INTEGER( I4B ) :: MinNptrs
    !! Smallest node number present inside mesh
  INTEGER( I4B ) :: tNodes
    !! Total number of nodes in mesh
  LOGICAL( LGT ) :: isInitiated
    !! `.True.` if `Meshobj_` is initiated
  INTEGER( I4B ), ALLOCATABLE :: LBndyIndex( : )
    !! For a given element if `LBndyIndex(iel) .eq. 0` then `iel` is not a
    !! a boundary element else it a boundary element which represents the
    !! index of `iel` in `BoundaryData()`.
  INTEGER( I4B ), ALLOCATABLE :: Nptrs( : )
    !! Node number of mesh `Nptrs( minNptrs : maxNptrs )`
  INTEGER( I4B ), ALLOCATABLE :: BoundaryNptrs( : )
    !! Node number of boundary of mesh
  INTEGER( I4B ), ALLOCATABLE :: InternalNptrs( : )
    !! Node number of internal nodes
  INTEGER( I4B ), ALLOCATABLE :: Local_Nptrs( : )
    !! This converts a given global node into local-node which can be
    !! used for accessing data inside `NodeToElem, NodeToNode`
  TYPE( ReferenceElement_ ), POINTER :: refelem=>NULL()
    !! Reference element
  TYPE( IntVector_ ), ALLOCATABLE :: NodeToElem( : )
    !! `NodeToElem( iLocalNode )` denotes indx of elems in mesh which are
    !! directly connected to node `GlobalNptrs( iLocalNode )`
  TYPE( IntVector_ ), ALLOCATABLE :: ElemToElem( : )
    !! `ElemToElem( iel )` denotes data of elements
    !! connected to the element`iel`
  TYPE( IntVector_ ), ALLOCATABLE :: NTN( : )
    !! `NTN( iLocalNode )` denotes node-ids that are connected to a node
    !! `GlobalNode( iLocalNode )`
  TYPE( IntVector_ ), ALLOCATABLE :: ElemToNode( : )
    !! ElemToNode( iel ) denotes the node numbers in element `iel`
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
    PROCEDURE, PUBLIC, PASS( obj ) :: DeallocateData => meshData_DeallocateData
      !! Deallocate mesh data
    PROCEDURE, PUBLIC, PASS( obj ) :: Initiate => meshData_Initiate
      !! Initiate mesh data
    PROCEDURE, PUBLIC, PASS( obj ) :: TotalNodes => get_total_nodes
      !! Returns total number of nodes in mesh
    PROCEDURE, PUBLIC, PASS( obj ) :: TotalBoundaryNodes => get_tbndy_nodes
      !! Returns total number of boundary nodes in mesh
    PROCEDURE, PUBLIC, PASS( obj ) :: TotalInternalNodes => get_tint_nodes
      !! Return total number of internal nodes in mesh
    PROCEDURE, PUBLIC, PASS( obj ) :: TotalBoundaryElements =>get_tbndy_elems
      !! Rertuns total boundary elements in mesh
    PROCEDURE, PUBLIC, PASS( obj ) :: BoundingBox => get_bBox
      !! return boundingbox of mesh
    PROCEDURE, PUBLIC, PASS( obj ) :: local_from_global
      !! return local node number of a given global node number
    PROCEDURE, PUBLIC, PASS( obj ) :: local_from_global_scalar
      !! return local node number of a given global node number
    GENERIC, PUBLIC :: LocalNptrs =>  local_from_global, &
                                      & local_from_global_scalar
      !! return local node number of a given global node number
    PROCEDURE, PUBLIC, PASS( obj ) :: global_from_local
      !! return global node nuber of a given local node number
    PROCEDURE, PUBLIC, PASS( obj ) :: global_from_local_scalar
      !! return global node nuber of a given local node number
    GENERIC, PUBLIC :: GlobalNptrs => global_from_local, &
                                      & global_from_local_scalar
      !! return global node nuber of a given local node number
    PROCEDURE, PUBLIC, PASS( obj ) :: isNodePresent => is_node_present
      !! returns `.true.` if a given node is present in the mesh
    PROCEDURE, PUBLIC, PASS( obj ) :: isNodeToNodesInitiated => &
                                      & is_node_nodes_initiated
      !! returns `.true.` if `NToN` array is allocated
    PROCEDURE, PUBLIC, PASS( obj ) :: isNodeToElementsInitiated => &
                                      & is_node_elements_initiated
      !! returns `.true.` if `NodeToElem` array is allocated
    PROCEDURE, PUBLIC, PASS( obj ) :: isElementToElementsInitiated => &
                                      & is_element_elements_initiated
      !! returns `.true.` if `ElemToElem` array is allocated
    PROCEDURE, PUBLIC, PASS( obj ) :: isElementToNodesInitiated => &
                                      & is_element_nodes_initiated
      !! returns `.true.` if `ElemToNode` array is allocated
    PROCEDURE, PUBLIC, PASS( obj ) :: isBoundaryDataInitiated => &
                                      & is_boundarydata
      !! returns `.true.` if `BoundaryData` array is allocated
    PROCEDURE, PUBLIC, PASS( obj ) :: isInternalNptrsInitiated => &
                                      & is_internalnptrs
      !! returns `.true.` if `InternalNptrs` array is allocated
    PROCEDURE, PUBLIC, PASS( obj ) :: isBoundaryNptrsInitiated => &
                                      & is_bndy_nptrs
    PROCEDURE, PUBLIC, PASS( obj ) :: isLocalNptrsInitiated => &
                                      & is_local_nptrs
      !! returns `.true.` if `Local_Nptrs` array is allocated
    PROCEDURE, PUBLIC, PASS( obj ) :: isInternalBoundaryDataInitiated => &
                                      & is_int_bndy_data
      !! returns `.true.` if `InternalBoundaryData` array is allocated
    PROCEDURE, PUBLIC, PASS( obj ) :: InitiateNodeToElements => &
                                      & init_node_elements
      !! construct node to element connectivity data
    PROCEDURE, PUBLIC, PASS( obj ) :: InitiateNodeToNodes => &
                                      & init_node_nodes
      !! construct node to node connectivity data
    PROCEDURE, PUBLIC, PASS( obj ) :: InitiateElementToElements => &
                                      & init_element_elements
      !! construct element to element connectivity data
    PROCEDURE, PUBLIC, PASS( obj ) :: InitiateElementToNodes => &
                                      & init_elem_nodes
      !! construct element to node connectivity data
    PROCEDURE, PUBLIC, PASS( obj ) :: InitiateBoundaryData => init_bndy_data
      !! construct boundary data information
    PROCEDURE, PUBLIC, PASS( obj ) :: InitiateInternalNptrs => init_int_nptrs
      !! construct details about the internal boundary
    PROCEDURE, PUBLIC, PASS( obj ) :: InitiateInternalBoundaryData => &
      & init_int_bndydata
      !! construct details about the internal boundary
    PROCEDURE, PUBLIC, PASS( obj ) :: NodeToElements => node_elements
      !! return node to element connectivity data for `GlobalIndx`
    PROCEDURE, PUBLIC, PASS( obj ) :: NodeToNodes => get_node_nodes
      !! return node to node connectivity data for `GlobalIndx`
    PROCEDURE, PUBLIC, PASS( obj ) :: get_elem_elems_1
      !! return element to element connectivity data for `iel` element
    PROCEDURE, PUBLIC, PASS( obj ) :: get_elem_elems_2
      !! return element to element connectivity data for `iel` element
    GENERIC, PUBLIC :: ElementToElements => get_elem_elems_1, get_elem_elems_2
      !! return element to element connectivity data for `iel` element
    PROCEDURE, PUBLIC, PASS( obj ) :: ElementToNodes => get_elem_nodes
      !! return element to node connectivity data for `iel` element
    PROCEDURE, PUBLIC, PASS( obj ) :: isBoundaryElement => is_bndy_elem
      !! return `.true.` if element `iel` is a boundary element
    PROCEDURE, PUBLIC, PASS( obj ) :: BoundaryElementData => get_bndy_elem
      !! return boundary element data of a boundary element `iel` if it is
      !! boundary element
    PROCEDURE, PUBLIC, PASS( obj ) :: setSparsity => setSparsity_1
      !! set sparsity in sparseMatrix_
    PROCEDURE, PUBLIC, PASS( mdobj ) :: getFacetElements => get_facet_elements
      !! return the boundary elements
    PROCEDURE, PUBLIC, PASS( CellMeshData ) :: connectFacetToCell => &
      & mc_connect_facet_cell
      !! connect facet to cell
    PROCEDURE, PUBLIC, PASS( obj ) :: MeshQuality => md_quality
      !! return mesh quality
    PROCEDURE, PUBLIC, PASS( obj ) :: FindElement => md_findElement
END TYPE MeshData_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PUBLIC :: MeshData_

TYPE(  MeshData_ ), PUBLIC, PARAMETER :: TypeMeshData = &
  & MeshData_( &
  & MaxNptrs = 0, MinNptrs = 0, tNodes = 0, &
  & isInitiated = .FALSE., &
  & LBndyIndex = NULL( ), Nptrs = NULL( ), &
  & BoundaryNptrs = NULL( ), &
  & InternalNptrs = NULL( ), Local_Nptrs = NULL( ), &
  & NodeToElem = NULL( ), &
  & ElemToElem = NULL( ), &
  & NTN = NULL( ), &
  & ElemToNode = NULL( ), &
  & BoundaryData = NULL( ), &
  & InternalBndyElemNum = NULL( ), &
  & InternalBoundaryData = NULL( ) )

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!>
! Contains [[MeshData_]] pointer as its field
TYPE :: MeshDataPointer_
  CLASS( MeshData_ ), POINTER :: Ptr => NULL( )
END TYPE MeshDataPointer_

PUBLIC :: MeshDataPointer_

!----------------------------------------------------------------------------
!                                                    Initiate@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
!! Initiate [[meshdata_]] object


!> authors: Dr. Vikas Sharma
!
! Initiate [[meshdata_]] object
!
!### Usage
!
! ```fortran
!	call obj % initiate( meshobj )
! ```

MODULE PURE SUBROUTINE meshData_Initiate( obj, Meshobj )
  CLASS( MeshData_ ), INTENT( INOUT ) :: obj
    !! mesh data container
  CLASS( Mesh_ ), INTENT( INOUT ) :: Meshobj
    !! mesh object
END SUBROUTINE meshData_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE Initiate
  MODULE PROCEDURE meshData_Initiate
END INTERFACE Initiate

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                                   MeshData@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
!! `MeshData()` function to construct mesh data

!> authors: Dr. Vikas Sharma
!
! `MeshData()` function to construct mesh data
!
!### Usage
!
! ```fortran
!	obj = meshData( Meshobj )
! ```

MODULE FUNCTION meshdata_1( Meshobj ) RESULT( ans )
  TYPE( MeshData_ ) :: ans
    !! Meshdata object
  CLASS( Mesh_ ), INTENT( INOUT ) :: Meshobj
    !! Mesh object
END FUNCTION meshdata_1
END INTERFACE

!>
! Generic function to construct [[meshdata_]]
INTERFACE MeshData
  MODULE PROCEDURE meshdata_1
END INTERFACE MeshData

PUBLIC :: MeshData

!----------------------------------------------------------------------------
!                                           MeshData_Pointer@MeshDataMethods
!----------------------------------------------------------------------------

!>
! Generic function that Returns pointer to [[meshdata_]] object
INTERFACE MeshData_Pointer
  MODULE PROCEDURE meshdata_ptr_1
END INTERFACE MeshData_Pointer

PUBLIC :: MeshData_Pointer

!----------------------------------------------------------------------------
!                                             DeallocateData@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
!! Deallocate data stored inside [[meshdata_]]

!> authors: Dr. Vikas Sharma
!
! Deallocate data stored inside [[meshdata_]]
!
!### Usage
!
! ```fortran
!	call deallocateData( obj )
! ```

MODULE SUBROUTINE meshData_DeallocateData( obj )
  CLASS( MeshData_ ), INTENT( INOUT ) :: obj
    !! mesh data object
END SUBROUTINE meshData_DeallocateData
END INTERFACE

!>
! Generic subroutine to deallocate data stored inside [[MeshData_]]
INTERFACE DeallocateData
  MODULE PROCEDURE meshData_DeallocateData
END INTERFACE DeallocateData

PUBLIC :: DeallocateData

!----------------------------------------------------------------------------
!                                                 TotalNodes@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
!! Return total number of nodes

!> authors: Dr. Vikas Sharma
!
! This function will Return the total number of nodes present in meshdata
!
!### Usage
!
! ```fortran
!	tnodes = obj % TotalNodes( )
! ```

MODULE PURE FUNCTION get_total_nodes( obj ) RESULT( ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
    !! Mesh data object
  INTEGER( I4B ) :: ans
    !! Total number of nodes
END FUNCTION get_total_nodes
END INTERFACE

!----------------------------------------------------------------------------
!                                      TotalBoundaryNodes@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
!! Return total number of boundary nodes

!> authors: Dr. Vikas Sharma
!
! Return total number of boundary nodes
!
!### Usage
!
! ```fortran
!	tnodes = obj % totalBoundaryNodes( )
! ```

MODULE PURE FUNCTION get_tbndy_nodes( obj ) RESULT( ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
    !! mesh data
  INTEGER( I4B ) :: ans
    !! total boundary nodes
END FUNCTION get_tbndy_nodes
END INTERFACE

!----------------------------------------------------------------------------
!                                      TotalInternalNodes@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
!! Returns total number of internal bounary nodes

!> authors: Dr. Vikas Sharma
!
! Returns total number of internal bounary nodes
!
!### Usage
!
! ```fortran
!	tnodes = obj % totalInternalNodes( )
! ```

MODULE PURE FUNCTION get_tint_nodes( obj ) RESULT( ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
    !! mesh data
  INTEGER( I4B ) :: ans
    !! total number of internal boundary nodes
END FUNCTION get_tint_nodes
END INTERFACE

!----------------------------------------------------------------------------
!                                     TotalBoundaryElements@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
!! This function returns the total boundary elements in mesh

!> authors: Dr. Vikas Sharma
!
! This function returns the total bounadry elements in mesh

MODULE PURE FUNCTION get_tbndy_elems( obj ) RESULT( ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: ans
END FUNCTION get_tbndy_elems
END INTERFACE

!----------------------------------------------------------------------------
!                                           getBoundaringBox@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
!! Return the boundinb box of mesh

!> authors: Dr. Vikas Sharma
!
! Return the bounding box of mesh
!
!### Usage
!
! ```fortran
!	bbox = BoundingBox( obj, nodes )
! ```

MODULE PURE FUNCTION get_Bbox( obj, nodes ) RESULT( ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
    !! mesh data object
  REAL( DFP ), INTENT( IN ) :: nodes( :, : )
    !! nodal coordinates in xiJ format
  TYPE( BoundingBox_ ) :: ans
    !! bounding box for mesh
END FUNCTION get_Bbox
END INTERFACE

!> Generic method for obtaining bounding box for a mesh
INTERFACE BoundingBox
  MODULE PROCEDURE get_bBox
END INTERFACE BoundingBox

PUBLIC :: BoundingBox

!----------------------------------------------------------------------------
!                                              getLocalNptrs@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
!! Convert global node number to local node  number

!> authors: Dr. Vikas Sharma
!
! Convert global node number to local node  number
!
!### Usage
!
! ```fortran
!	indx = obj % localNptrs( globalIndx )
! ```

MODULE PURE FUNCTION local_from_global( obj, GlobalIndx ) &
  & RESULT( ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
    !! mesh data
  INTEGER( I4B ), INTENT( IN ) :: GlobalIndx( : )
    !! vec of global node numbers
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
    !! vec of local node number
END FUNCTION local_from_global
END INTERFACE

!----------------------------------------------------------------------------
!                                               getLocalNptrs@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
!! Convert global node numbr to local node number

!> authors: Dr. Vikas Sharma
!
! Convert global node numbr to local node number
!
!### Usage
!
! ```fortran
!	indx = obj % localNptrs( GlobalIndx )
! ```

MODULE PURE FUNCTION local_from_global_scalar( obj, GlobalIndx ) &
  & RESULT( ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
    !! mesh data
  INTEGER( I4B ), INTENT( IN ) :: GlobalIndx
    !! global node number
  INTEGER( I4B ) :: ans
    !! local node number
END FUNCTION local_from_global_scalar
END INTERFACE

!----------------------------------------------------------------------------
!                                               GlobalNptrs@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
!! Convert local node number to global node number

!> authors: Dr. Vikas Sharma
!
! Convert local node number to global node number
!
!### Usage
!
! ```fortran
!	indx = obj % globalNptrs( localIndx )
! ```

MODULE PURE FUNCTION global_from_local( obj, LocalIndx ) &
  & RESULT( ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
    !! mesh data
  INTEGER( I4B ), INTENT( IN ) :: LocalIndx( : )
    !! vec of local node number
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
    !! vec of global node number
END FUNCTION global_from_local
END INTERFACE

!----------------------------------------------------------------------------
!                                                GlobalNptrs@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
!! Convert local node number to global node number

!> authors: Dr. Vikas Sharma
!
! Convert local node number to global node number
!
!### Usage
!
! ```fortran
!	indx = obj % globalNptrs( localIndx )
! ```

MODULE PURE FUNCTION global_from_local_scalar( obj, LocalIndx ) RESULT( ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
    !! mesh data
  INTEGER( I4B ), INTENT( IN ) :: LocalIndx
    !! local node number
  INTEGER( I4B ) :: ans
    !! global node number
END FUNCTION global_from_local_scalar
END INTERFACE

!----------------------------------------------------------------------------
!                                             isNodePresent@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
!! Returns `.true.` if the a global node is present inside the mesh-data

!> authors: Dr. Vikas Sharma
!
! Returns `.true.` if the a global node is present inside the mesh-data
!
!### Usage
!
! ```fortran
!	is = obj % isNodePresent( Nptrs )
! ```

MODULE PURE FUNCTION is_node_present( obj, Nptrs ) RESULT( ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
    !! mesh data
  INTEGER( I4B ), INTENT( IN ) :: Nptrs
    !! global node number
  LOGICAL( LGT ) :: ans
    !! Returns true if present
END FUNCTION is_node_present
END INTERFACE

!----------------------------------------------------------------------------
!                                     isNodeToNodesInitiated@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
!! Returns `.true.` if node to node connectivity data is initiated.

! Returns `.true.` if node to node connectivity data is initiated.

MODULE PURE FUNCTION is_node_nodes_initiated( obj ) RESULT( ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ) :: ans
END FUNCTION is_node_nodes_initiated
END INTERFACE

!----------------------------------------------------------------------------
!                                  isNodeToElementsInitiated@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
!! Returns `.true.` if node to elem connectivity data is initiated.

!> authors: Dr. Vikas Sharma
!
! Returns `.true.` if node to elem connectivity data is initiated.

MODULE PURE FUNCTION is_node_elements_initiated( obj ) RESULT( ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ) :: ans
END FUNCTION is_node_elements_initiated
END INTERFACE

!----------------------------------------------------------------------------
!                               isElementToElementsInitiated@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
!! Returns `.true.` if elem to elem connectivity data is initiated.

!> authors: Dr. Vikas Sharma
!
! Returns `.true.` if elem to elem connectivity data is initiated.

MODULE PURE FUNCTION is_element_elements_initiated( obj ) RESULT( ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ) :: ans
END FUNCTION is_element_elements_initiated
END INTERFACE

!----------------------------------------------------------------------------
!                                 isElementToNodesInitiated@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
!! Returns `.true.` if elem to node connectivity data is initiated.

!> authors: Dr. Vikas Sharma
!
! Returns `.true.` if elem to node connectivity data is initiated.

MODULE PURE FUNCTION is_element_nodes_initiated( obj ) RESULT( ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ) :: ans
END FUNCTION is_element_nodes_initiated
END INTERFACE

!----------------------------------------------------------------------------
!                                   isBoundaryDataInitiated@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
!! Returns `.true.` if boundary data is initiated.

!> authors: Dr. Vikas Sharma
!
! Returns `.true.` if boundary data is initiated.

MODULE PURE FUNCTION is_boundarydata( obj ) RESULT( ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ) :: ans
END FUNCTION is_boundarydata
END INTERFACE

!----------------------------------------------------------------------------
!                                   isInternalNptrsInitiated@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
!! Returns `.true.` if data related to internal nptrs is initiated.

!> authors: Dr. Vikas Sharma
!
! Returns `.true.` if data related to internal nptrs is initiated.

MODULE PURE FUNCTION is_internalnptrs( obj ) RESULT( ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ) :: ans
END FUNCTION is_internalnptrs
END INTERFACE

!----------------------------------------------------------------------------
!                                   isBoundaryNptrsInitiated@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
!! Returns `.true.` if data related to boundary nptrs is initiated.

!> authors: Dr. Vikas Sharma
!
! Returns `.true.` if data related to boundary nptrs is initiated.

MODULE PURE FUNCTION is_bndy_nptrs( obj ) RESULT( ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ) :: ans
END FUNCTION is_bndy_nptrs
END INTERFACE

!----------------------------------------------------------------------------
!                                      isLocalNptrsInitiated@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
!! Returns `.true.` if data  `local_nptrs` array is initiated.

!> authors: Dr. Vikas Sharma
!
! Returns `.true.` if data  `local_nptrs` array is initiated.

MODULE PURE FUNCTION is_local_nptrs( obj  ) RESULT( ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ) :: ans
END FUNCTION is_local_nptrs
END INTERFACE

!----------------------------------------------------------------------------
!                            isInternalBoundaryDataInitiated@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
!! Returns `.true.` if internal bounary data is initiated

!> authors: Dr. Vikas Sharma
!
! Returns `.true.` if internal bounary data is initiated

MODULE PURE FUNCTION is_int_bndy_data( obj ) RESULT( ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
  LOGICAL( LGT ) :: ans
END FUNCTION is_int_bndy_data
END INTERFACE

!----------------------------------------------------------------------------
!                                      initiateNodeToElements@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
!! generate Elements surrounding a node mapping

!> authors: Dr. Vikas Sharma
!
! This subroutine generate Elements surrounding a node mapping
! - The mapping is stored in the array called `NodeToElem`
! - The size of `NodeToElem` array is same as `obj % Nptrs`
! - Always use method called `NodeToElements()` to access this information
!
! @warning
! Always use the mapping between global node number and local node number to
! avoid segmentation fault
! @endwarning
!
!### Usage
!
! ```fortran
!	call obj % initiateNodeToElements( Meshobj )
! ```

MODULE PURE SUBROUTINE init_node_elements( obj, Meshobj )
  CLASS( MeshData_ ), INTENT( INOUT ) :: obj
    !! mesh data
  CLASS( Mesh_ ), INTENT( INOUT) :: Meshobj
    !! mesh object
END SUBROUTINE init_node_elements
END INTERFACE

!----------------------------------------------------------------------------
!                                             NodeToElements@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
!! Returns the element numbers which are connected to the a global node

!> authors: Dr. Vikas Sharma
!
! Returns the element numbers which are connected to the a global node
!
!### Usage
!
! ```fortran
! val = obj % NodeToElements( GlobalPt )
! ```
MODULE PURE FUNCTION node_elements( obj, GlobalPt ) RESULT( ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
    !! mesh data
  INTEGER( I4B ), INTENT( IN ) :: GlobalPt
    !! global node number
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
    !! vec of element number
END FUNCTION node_elements
END INTERFACE

!----------------------------------------------------------------------------
!                                        InitiateNodeToNode@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
!! Initiate node to node connectivity data

!> authors: Dr. Vikas Sharma
!
! Initiate node to node connectivity data
!
!### Usage
!
! ```fortran
!	call obj % initiateNodeToNode( meshobj )
! ```

MODULE PURE SUBROUTINE init_node_nodes( obj, Meshobj )
  CLASS( MeshData_ ), INTENT( INOUT ) :: obj
    !! mesh data
  CLASS( Mesh_ ),INTENT( INOUT) :: Meshobj
    !! mesh object
END SUBROUTINE init_node_nodes
END INTERFACE

!----------------------------------------------------------------------------
!                                                NodeToNodes@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
!! Return a list of nodes surrounding a given global node number

!> authors: Dr. Vikas Sharma
!
! - Return a list of nodes surrounding a given global node number
! - `IndcludeSelf` is a logical parameter; if it is true self number is
!     is also returned, otherwise only surrounding node number is returned

MODULE PURE FUNCTION get_node_nodes( obj, GlobalNode, IncludeSelf )&
  & RESULT( ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
    !! mesh data
  INTEGER( I4B ), INTENT( IN ) :: GlobalNode
    !! global node number
  LOGICAL( LGT ), INTENT( IN ) :: IncludeSelf
    !! logical variable
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
    !! vec of node number surrounding `GlobalNode`

END FUNCTION get_node_nodes
END INTERFACE

!----------------------------------------------------------------------------
!                                       init_element_elements@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
!! Initiate element to element connectivity data

!> authors: Dr. Vikas Sharma
!
! Initiate element to element connectivity data
!
!### Usage
!
! ```fortran
!	call obj % initiateElementToElement( Meshobj )
! ```

MODULE PURE SUBROUTINE init_element_elements( obj, Meshobj )
  CLASS( MeshData_ ), INTENT( INOUT ) :: obj
    !! mesh data
  CLASS( Mesh_ ), INTENT( INOUT) :: Meshobj
    !! mesh object
END SUBROUTINE init_element_elements
END INTERFACE

!----------------------------------------------------------------------------
!                                         ElementToElements@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
!! Return element to element connectivity information

!> authors: Dr. Vikas Sharma
!
! Return element to element connectivity information for a given element
! number `iel`
! - This routine return **full information** about elements surrounding
! element `iel`
! - Rows of `ans` denote the element to which `iel` is connected to
! - Column-1 of `ans` denotes element number
! - Column-2 denotes the local face number of element `iel`, and
! - Column-3 denotes the local face number of element whose element number is
! given in the column-1
!
!### Usage
!
! ```fortran
!	val = obj % ElementToElements( iel )
! ```

MODULE PURE FUNCTION get_elem_elems_1( obj, iel ) RESULT( ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
    !! mesh data
  INTEGER( I4B ), INTENT( IN ) :: iel
    !! element number
  INTEGER( I4B ), ALLOCATABLE :: ans( :, : )
    !! list of elements surrounding elements
END FUNCTION get_elem_elems_1
END INTERFACE

!----------------------------------------------------------------------------
!                                         ElementToElements@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
!! Return element to element connectivity information

!> authors: Dr. Vikas Sharma
!
! Return element to element connectivity information for a given element
! number `iel`
! - `iel( 2 )` denotes the extra options
!     0 ---> only Return the element numbers
!     1 ---> Return the full information
! - This routine return **full information** when `iel(2)=1`
! about elements surrounding element `iel`
! - Rows of `ans` denote the element to which `iel` is connected to
! - Column-1 of `ans` denotes element number
! - Column-2 denotes the local face number of element `iel`, and
! - Column-3 denotes the local face number of element whose element number is
! given in the column-1
!
!### Usage
!
! ```fortran
!	val = obj % ElementToElements( iel )
! ```

MODULE PURE FUNCTION get_elem_elems_2( obj, iel ) RESULT( ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: iel( 2 )
  INTEGER( I4B ), ALLOCATABLE :: ans( :, : )
END FUNCTION get_elem_elems_2
END INTERFACE

!----------------------------------------------------------------------------
!                                     InitiateElementToNodes@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
!! Initiate element to node data

!> authors: Dr. Vikas Sharma
!
! Initiate element to node data
!
!### Usage
!
! ```fortran
!	call obj % elementToNodes( Meshobj )
! ```

MODULE PURE SUBROUTINE init_elem_nodes( obj, Meshobj )
  CLASS( MeshData_ ), INTENT( INOUT ) :: obj
    !! mesh data
  CLASS( Mesh_ ), INTENT( INOUT) :: Meshobj
    !! mesh object
END SUBROUTINE init_elem_nodes
END INTERFACE

!----------------------------------------------------------------------------
!                                             ElementToNodes@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
!! Return element to nodes data

!> authors: Dr. Vikas Sharma
!
! Return element to nodes data
!
!### Usage
!
! ```fortran
!	val = obj % elementToNodes( iel )
! ```

MODULE PURE FUNCTION get_elem_nodes( obj, iel ) RESULT( ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
    !! mesh data
  INTEGER( I4B ), INTENT( IN ) :: iel
    !! element number
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
    !! list of elements surrounding the nodes
END FUNCTION get_elem_nodes
END INTERFACE

!----------------------------------------------------------------------------
!                                                       InitiateBoundaryData
!----------------------------------------------------------------------------

INTERFACE
!! Initiate boundary data of mesh

!> authors: Dr. Vikas Sharma
!
! Initiate boundary data of mesh
!
!### Usage
!
! ```fortran
!	call obj % initiateBoundaryData( Meshobj )
! ```

MODULE PURE SUBROUTINE init_bndy_data( obj, Meshobj )
  CLASS( MeshData_ ), INTENT( INOUT) :: obj
    !! mesh data
  CLASS( Mesh_ ), INTENT( INOUT) :: Meshobj
    !! mesh object
END SUBROUTINE init_bndy_data
END INTERFACE

!----------------------------------------------------------------------------
!                                                         isBoundaryElement
!----------------------------------------------------------------------------

INTERFACE
!! Returns `.true.` if a given element number `iel` is boundary element

!> authors: Dr. Vikas Sharma
!
! Returns `.true.` if a given element number `iel` is boundary element
!
!### Usage
!
! ```fortran
!	is = obj % isBoundaryElement( iel )
! ```

MODULE PURE FUNCTION is_bndy_elem( obj, iel ) RESULT( ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
    !! mesh data
  INTEGER( I4B ), INTENT( IN ) :: iel
    !! element number
  LOGICAL( LGT ) :: ans
    !! `.true.` if `iel` is a boundary element
END FUNCTION is_bndy_elem
END INTERFACE

!----------------------------------------------------------------------------
!                                                        BoundaryElementData
!----------------------------------------------------------------------------

INTERFACE
!! Returns boundary element data

!> authors: Dr. Vikas Sharma
!
! If element is a boundary element then it Returns a integer vector
! containing the id of local facets which is boundary of mesh
! otherwise it will return `[0]`
!
!### Usage
!
! ```fortran
!	val = obj % boundaryElementData(iel)
! ```

MODULE PURE FUNCTION get_bndy_elem( obj, iel ) RESULT( ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
    !! mesh data
  INTEGER( I4B ), INTENT( IN ) :: iel
    !! element number
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
    !! boundary data
END FUNCTION get_bndy_elem
END INTERFACE

!----------------------------------------------------------------------------
!                                                      InitiateInternalNptrs
!----------------------------------------------------------------------------

INTERFACE
!! Initiate internal node numbers

!> authors: Dr. Vikas Sharma
!
! Initiate internal node numbers
!
!### Usage
!
! ```fortran
!	call obj % initiateInternalNptrs( Meshobj )
! ```

MODULE PURE SUBROUTINE init_int_nptrs( obj, Meshobj )
  CLASS( MeshData_ ), INTENT( INOUT) :: obj
    !! mesh data
  CLASS( Mesh_ ), INTENT( INOUT ) :: Meshobj
    !! mesh object
END SUBROUTINE init_int_nptrs
END INTERFACE

!----------------------------------------------------------------------------
!                                               InitiateInternalBoundaryData
!----------------------------------------------------------------------------

INTERFACE
!! Initiate internal boundary data

MODULE PURE SUBROUTINE init_int_bndydata( obj, Meshobj )
  CLASS( MeshData_ ), INTENT( INOUT) :: obj
  CLASS( Mesh_ ), INTENT( INOUT) :: Meshobj
END SUBROUTINE init_int_bndydata
END INTERFACE

!----------------------------------------------------------------------------
!                                                setSparsity@MeshDataMethods
!----------------------------------------------------------------------------
INTERFACE
!! This routine set the sparsity pattern in `SparseMatrix_` object

!> authors: Dr. Vikas Sharma
!
! This routine set the sparsity pattern in `SparseMatrix_` object
!
!### Usage
!
! ```fortran
!	call setSparsity( obj, Meshobj, Mat )
! ```

MODULE PURE SUBROUTINE setSparsity_1( obj, Meshobj, Mat, map )
  CLASS( MeshData_ ), INTENT( INOUT) :: obj
    !! mesh data
  TYPE( Mesh_ ), INTENT( INOUT) :: Meshobj
    !! mesh object
  TYPE( SparseMatrix_ ), INTENT( INOUT ) :: Mat
    !! sparsematrix object
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: map( : )
    !! Global to local node number map
END SUBROUTINE setSparsity_1
END INTERFACE

!>
! generic interfac for setting sparsity pattern in [[SparseMatrix_]].
INTERFACE setSparsity
  MODULE PROCEDURE setSparsity_1
END INTERFACE setSparsity

PUBLIC :: setSparsity

!----------------------------------------------------------------------------
!                                                           getFacetElements
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine returns the mesh of facet/boundary elements of mesh obj

!> authors: Dr. Vikas Sharma
!
! This subroutine returns the mesh of boundary/facet elements of parent mesh
! `obj`

MODULE SUBROUTINE get_facet_elements( mdobj, obj, facetmesh, feobj )
  CLASS( MeshData_ ), INTENT( INOUT) :: mdobj
    !! Mesh data of parent mesh
  CLASS( Mesh_ ), INTENT( INOUT) :: obj
    !! Parent mesh
  CLASS( Mesh_ ), INTENT( INOUT) :: facetmesh
    !! facet mesh to be constructed
  CLASS( Element_ ), INTENT( IN ) :: feobj
    !! Finite element
END SUBROUTINE get_facet_elements
END INTERFACE

!----------------------------------------------------------------------------
!                                             ConnectFacetToCell@MeshMethods
!----------------------------------------------------------------------------

INTERFACE
!! Connect facet to cell elements

!> authors: Dr. Vikas Sharma
!
! This subroutine connects the mesh of facet elements to the mesh of
! cell elements.
!
!### Usage
!
! ```fortran
!	call ConnectFacetToCell( CellMesh, FacetMesh, CellMeshData )
! ```

MODULE PURE SUBROUTINE mc_connect_facet_cell(CellMeshData,CellMesh,FacetMesh)
  CLASS( MeshData_ ), INTENT( INOUT) :: CellMeshData
    !! Mesh data of cell mesh
  CLASS( Mesh_ ), INTENT( INOUT ), TARGET :: CellMesh
    !! Mesh of  cell elements
  CLASS( Mesh_ ), INTENT( INOUT ), TARGET :: FacetMesh
    !! Mesh of facet elements
END SUBROUTINE mc_connect_facet_cell
END INTERFACE

!----------------------------------------------------------------------------
!                                                     getMeshQuality@Methods
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION md_quality( obj, Meshobj, Nodes, Measure ) RESULT( ans )
  CLASS( MeshData_ ), INTENT( INOUT ) :: obj
  CLASS( Mesh_ ), TARGET, INTENT( INOUT) :: Meshobj
  REAL( DFP ) , INTENT( IN ) :: Nodes(:,:)
  INTEGER( I4B ), INTENT( IN ) :: Measure
  REAL( DFP ), ALLOCATABLE :: ans(:)
END FUNCTION md_quality
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 FindElement
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION md_findelement( obj, Meshobj, coord, nodes ) &
  & RESULT( ans )
  CLASS( MeshData_ ), INTENT( IN ) :: obj
  CLASS( Mesh_ ), INTENT( IN ) :: Meshobj
  REAL( DFP ), INTENT( IN ) :: coord(:,:)
  REAL( DFP ), INTENT( IN ) :: nodes(:,:)
  REAL( DFP ) :: ans( SIZE( coord, 2 ) )
END FUNCTION md_findelement
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!                                                             meshdata_ptr_2
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! Function that returns pointer to the [[MeshData_]] object
!
!### Usage
!
! ```fortran
!	class( meshdata_ ), pointer :: obj
! obj => MeshData_Pointer( Meshobj )
! ```

FUNCTION meshdata_ptr_1( Meshobj ) RESULT( ans )
  CLASS( MeshData_ ), POINTER :: ans
    !! MeshData_ object
  CLASS( Mesh_ ), INTENT( INOUT ) :: Meshobj
    !! Mesh_ object
  ALLOCATE( MeshData_ :: ans )
  CALL ans % Initiate( Meshobj )
END FUNCTION meshdata_ptr_1

END MODULE MeshData_Class