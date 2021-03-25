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
    !! `.True.` if `MeshObj_` is initiated
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

    PROCEDURE, PUBLIC, PASS( Obj ) :: Finalize => Deallocate_meshdata
      !! Deallocate mesh data
    PROCEDURE, PUBLIC, PASS( Obj ) :: Initiate => init_meshdata_1
      !! Initiate mesh data
    PROCEDURE, PUBLIC, PASS( Obj ) :: TotalNodes => get_total_nodes
      !! Returns total number of nodes in mesh
    PROCEDURE, PUBLIC, PASS( Obj ) :: TotalBoundaryNodes => get_tbndy_nodes
      !! Returns total number of boundary nodes in mesh
    PROCEDURE, PUBLIC, PASS( Obj ) :: TotalInternalNodes => get_tint_nodes
      !! Return total number of internal nodes in mesh
    PROCEDURE, PUBLIC, PASS( Obj ) :: TotalBoundaryElements =>get_tbndy_elems
      !! Rertuns total boundary elements in mesh
    PROCEDURE, PUBLIC, PASS( Obj ) :: BoundingBox => get_bBox
      !! return boundingbox of mesh
    PROCEDURE, PUBLIC, PASS( Obj ) :: local_from_global
      !! return local node number of a given global node number
    PROCEDURE, PUBLIC, PASS( Obj ) :: local_from_global_scalar
      !! return local node number of a given global node number
    GENERIC, PUBLIC :: LocalNptrs =>  local_from_global, &
                                      & local_from_global_scalar
      !! return local node number of a given global node number
    PROCEDURE, PUBLIC, PASS( Obj ) :: global_from_local
      !! return global node nuber of a given local node number
    PROCEDURE, PUBLIC, PASS( Obj ) :: global_from_local_scalar
      !! return global node nuber of a given local node number
    GENERIC, PUBLIC :: GlobalNptrs => global_from_local, &
                                      & global_from_local_scalar
      !! return global node nuber of a given local node number
    PROCEDURE, PUBLIC, PASS( Obj ) :: isNodePresent => is_node_present
      !! returns `.true.` if a given node is present in the mesh
    PROCEDURE, PUBLIC, PASS( Obj ) :: isNodeToNodesInitiated => &
                                      & is_node_nodes_initiated
      !! returns `.true.` if `NToN` array is allocated
    PROCEDURE, PUBLIC, PASS( Obj ) :: isNodeToElementsInitiated => &
                                      & is_node_elements_initiated
      !! returns `.true.` if `NodeToElem` array is allocated
    PROCEDURE, PUBLIC, PASS( Obj ) :: isElementToElementsInitiated => &
                                      & is_element_elements_initiated
      !! returns `.true.` if `ElemToElem` array is allocated
    PROCEDURE, PUBLIC, PASS( Obj ) :: isElementToNodesInitiated => &
                                      & is_element_nodes_initiated
      !! returns `.true.` if `ElemToNode` array is allocated
    PROCEDURE, PUBLIC, PASS( Obj ) :: isBoundaryDataInitiated => &
                                      & is_boundarydata
      !! returns `.true.` if `BoundaryData` array is allocated
    PROCEDURE, PUBLIC, PASS( Obj ) :: isInternalNptrsInitiated => &
                                      & is_internalnptrs
      !! returns `.true.` if `InternalNptrs` array is allocated
    PROCEDURE, PUBLIC, PASS( Obj ) :: isBoundaryNptrsInitiated => &
                                      & is_bndy_nptrs
    PROCEDURE, PUBLIC, PASS( Obj ) :: isLocalNptrsInitiated => &
                                      & is_local_nptrs
      !! returns `.true.` if `Local_Nptrs` array is allocated
    PROCEDURE, PUBLIC, PASS( Obj ) :: isInternalBoundaryDataInitiated => &
                                      & is_int_bndy_data
      !! returns `.true.` if `InternalBoundaryData` array is allocated
    PROCEDURE, PUBLIC, PASS( Obj ) :: InitiateNodeToElements => &
                                      & init_node_elements
      !! construct node to element connectivity data
    PROCEDURE, PUBLIC, PASS( Obj ) :: InitiateNodeToNodes => &
                                      & init_node_nodes
      !! construct node to node connectivity data
    PROCEDURE, PUBLIC, PASS( Obj ) :: InitiateElementToElements => &
                                      & init_element_elements
      !! construct element to element connectivity data
    PROCEDURE, PUBLIC, PASS( Obj ) :: InitiateElementToNodes => &
                                      & init_elem_nodes
      !! construct element to node connectivity data
    PROCEDURE, PUBLIC, PASS( Obj ) :: InitiateBoundaryData => init_bndy_data
      !! construct boundary data information
    PROCEDURE, PUBLIC, PASS( Obj ) :: InitiateInternalNptrs => init_int_nptrs
      !! construct details about the internal boundary
    PROCEDURE, PUBLIC, PASS( Obj ) :: InitiateInternalBoundaryData => &
                                      & init_int_bndydata
      !! construct details about the internal boundary
    PROCEDURE, PUBLIC, PASS( Obj ) :: NodeToElements => node_elements
      !! return node to element connectivity data for `GlobalIndx`
    PROCEDURE, PUBLIC, PASS( Obj ) :: NodeToNodes => get_node_nodes
      !! return node to node connectivity data for `GlobalIndx`
    PROCEDURE, PUBLIC, PASS( Obj ) :: get_elem_elems_1
      !! return element to element connectivity data for `iel` element
    PROCEDURE, PUBLIC, PASS( Obj ) :: get_elem_elems_2
      !! return element to element connectivity data for `iel` element
    GENERIC, PUBLIC :: ElementToElements => get_elem_elems_1, get_elem_elems_2
      !! return element to element connectivity data for `iel` element
    PROCEDURE, PUBLIC, PASS( Obj ) :: ElementToNodes => get_elem_nodes
      !! return element to node connectivity data for `iel` element
    PROCEDURE, PUBLIC, PASS( Obj ) :: isBoundaryElement => is_bndy_elem
      !! return `.true.` if element `iel` is a boundary element
    PROCEDURE, PUBLIC, PASS( Obj ) :: BoundaryElementData => get_bndy_elem
      !! return boundary element data of a boundary element `iel` if it is
      !! boundary element
    PROCEDURE, PUBLIC, PASS( Obj ) :: setSparsity => setSparsity_1
      !! set sparsity in sparseMatrix_
    PROCEDURE, PUBLIC, PASS( mdObj ) :: getFacetElements => get_facet_elements
      !! return the boundary elements
    PROCEDURE, PUBLIC, PASS( CellMeshData ) :: connectFacetToCell => &
      & mc_connect_facet_cell
      !! connect facet to cell
    PROCEDURE, PUBLIC, PASS( Obj ) :: MeshQuality => md_quality
      !! return mesh quality
    PROCEDURE, PUBLIC, PASS( Obj ) :: FindElement => md_findElement

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

MODULE PURE SUBROUTINE init_meshdata_1( Obj, MeshObj )
  CLASS( MeshData_ ), INTENT( INOUT ) :: Obj
    !! mesh data container
  CLASS( Mesh_ ), INTENT( INOUT ) :: MeshObj
    !! mesh object
END SUBROUTINE init_meshdata_1
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE Initiate
  MODULE PROCEDURE init_meshdata_1
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
!	obj = meshData( MeshObj )
! ```

MODULE FUNCTION meshdata_1( MeshObj ) RESULT( Ans )
  TYPE( MeshData_ ) :: Ans
    !! Meshdata object
  CLASS( Mesh_ ), INTENT( INOUT ) :: MeshObj
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
!	call deallocateData( Obj )
! ```

MODULE SUBROUTINE Deallocate_meshdata( Obj )
  CLASS( MeshData_ ), INTENT( INOUT ) :: Obj
    !! mesh data object
END SUBROUTINE Deallocate_meshdata
END INTERFACE

!>
! Generic subroutine to deallocate data stored inside [[MeshData_]]
INTERFACE DeallocateData
  MODULE PROCEDURE Deallocate_meshdata
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
!	tnodes = Obj % TotalNodes( )
! ```

MODULE PURE FUNCTION get_total_nodes( Obj ) RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: Obj
    !! Mesh data object
  INTEGER( I4B ) :: Ans
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
!	tnodes = Obj % totalBoundaryNodes( )
! ```

MODULE PURE FUNCTION get_tbndy_nodes( Obj ) RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: Obj
    !! mesh data
  INTEGER( I4B ) :: Ans
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

MODULE PURE FUNCTION get_tint_nodes( Obj ) RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: Obj
    !! mesh data
  INTEGER( I4B ) :: Ans
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

MODULE PURE FUNCTION get_tbndy_elems( Obj ) RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ) :: Ans
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
!	bbox = BoundingBox( Obj, nodes )
! ```

MODULE PURE FUNCTION get_Bbox( Obj, nodes ) RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: Obj
    !! mesh data object
  REAL( DFP ), INTENT( IN ) :: nodes( :, : )
    !! nodal coordinates in xiJ format
  TYPE( BoundingBox_ ) :: Ans
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

MODULE PURE FUNCTION local_from_global( Obj, GlobalIndx ) &
  & RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: Obj
    !! mesh data
  INTEGER( I4B ), INTENT( IN ) :: GlobalIndx( : )
    !! vec of global node numbers
  INTEGER( I4B ), ALLOCATABLE :: Ans( : )
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
!	indx = Obj % localNptrs( GlobalIndx )
! ```

MODULE PURE FUNCTION local_from_global_scalar( Obj, GlobalIndx ) &
  & RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: Obj
    !! mesh data
  INTEGER( I4B ), INTENT( IN ) :: GlobalIndx
    !! global node number
  INTEGER( I4B ) :: Ans
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

MODULE PURE FUNCTION global_from_local( Obj, LocalIndx ) &
  & RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: Obj
    !! mesh data
  INTEGER( I4B ), INTENT( IN ) :: LocalIndx( : )
    !! vec of local node number
  INTEGER( I4B ), ALLOCATABLE :: Ans( : )
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

MODULE PURE FUNCTION global_from_local_scalar( Obj, LocalIndx ) RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: Obj
    !! mesh data
  INTEGER( I4B ), INTENT( IN ) :: LocalIndx
    !! local node number
  INTEGER( I4B ) :: Ans
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
!	is = Obj % isNodePresent( Nptrs )
! ```

MODULE PURE FUNCTION is_node_present( Obj, Nptrs ) RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: Obj
    !! mesh data
  INTEGER( I4B ), INTENT( IN ) :: Nptrs
    !! global node number
  LOGICAL( LGT ) :: Ans
    !! Returns true if present
END FUNCTION is_node_present
END INTERFACE

!----------------------------------------------------------------------------
!                                     isNodeToNodesInitiated@MeshDataMethods
!----------------------------------------------------------------------------

INTERFACE
!! Returns `.true.` if node to node connectivity data is initiated.

! Returns `.true.` if node to node connectivity data is initiated.

MODULE PURE FUNCTION is_node_nodes_initiated( Obj ) RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: Obj
  LOGICAL( LGT ) :: Ans
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

MODULE PURE FUNCTION is_node_elements_initiated( Obj ) RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: Obj
  LOGICAL( LGT ) :: Ans
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

MODULE PURE FUNCTION is_element_elements_initiated( Obj ) RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: Obj
  LOGICAL( LGT ) :: Ans
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

MODULE PURE FUNCTION is_element_nodes_initiated( Obj ) RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: Obj
  LOGICAL( LGT ) :: Ans
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

MODULE PURE FUNCTION is_boundarydata( Obj ) RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: Obj
  LOGICAL( LGT ) :: Ans
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

MODULE PURE FUNCTION is_internalnptrs( Obj ) RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: Obj
  LOGICAL( LGT ) :: Ans
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

MODULE PURE FUNCTION is_bndy_nptrs( Obj ) RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: Obj
  LOGICAL( LGT ) :: Ans
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

MODULE PURE FUNCTION is_local_nptrs( Obj  ) RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: Obj
  LOGICAL( LGT ) :: Ans
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

MODULE PURE FUNCTION is_int_bndy_data( Obj ) RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: Obj
  LOGICAL( LGT ) :: Ans
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
! - The size of `NodeToElem` array is same as `Obj % Nptrs`
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
!	call Obj % initiateNodeToElements( MeshObj )
! ```

MODULE PURE SUBROUTINE init_node_elements( Obj, MeshObj )
  CLASS( MeshData_ ), INTENT( INOUT ) :: Obj
    !! mesh data
  CLASS( Mesh_ ), INTENT( INOUT) :: MeshObj
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
! val = Obj % NodeToElements( GlobalPt )
! ```
MODULE PURE FUNCTION node_elements( Obj, GlobalPt ) RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: Obj
    !! mesh data
  INTEGER( I4B ), INTENT( IN ) :: GlobalPt
    !! global node number
  INTEGER( I4B ), ALLOCATABLE :: Ans( : )
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

MODULE PURE SUBROUTINE init_node_nodes( Obj, MeshObj )
  CLASS( MeshData_ ), INTENT( INOUT ) :: Obj
    !! mesh data
  CLASS( Mesh_ ),INTENT( INOUT) :: MeshObj
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

MODULE PURE FUNCTION get_node_nodes( Obj, GlobalNode, IncludeSelf )&
  & RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: Obj
    !! mesh data
  INTEGER( I4B ), INTENT( IN ) :: GlobalNode
    !! global node number
  LOGICAL( LGT ), INTENT( IN ) :: IncludeSelf
    !! logical variable
  INTEGER( I4B ), ALLOCATABLE :: Ans( : )
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
!	call obj % initiateElementToElement( MeshObj )
! ```

MODULE PURE SUBROUTINE init_element_elements( Obj, MeshObj )
  CLASS( MeshData_ ), INTENT( INOUT ) :: Obj
    !! mesh data
  CLASS( Mesh_ ), INTENT( INOUT) :: MeshObj
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
! - Rows of `Ans` denote the element to which `iel` is connected to
! - Column-1 of `Ans` denotes element number
! - Column-2 denotes the local face number of element `iel`, and
! - Column-3 denotes the local face number of element whose element number is
! given in the column-1
!
!### Usage
!
! ```fortran
!	val = Obj % ElementToElements( iel )
! ```

MODULE PURE FUNCTION get_elem_elems_1( Obj, iel ) RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: Obj
    !! mesh data
  INTEGER( I4B ), INTENT( IN ) :: iel
    !! element number
  INTEGER( I4B ), ALLOCATABLE :: Ans( :, : )
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
! - Rows of `Ans` denote the element to which `iel` is connected to
! - Column-1 of `Ans` denotes element number
! - Column-2 denotes the local face number of element `iel`, and
! - Column-3 denotes the local face number of element whose element number is
! given in the column-1
!
!### Usage
!
! ```fortran
!	val = Obj % ElementToElements( iel )
! ```

MODULE PURE FUNCTION get_elem_elems_2( Obj, iel ) RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: Obj
  INTEGER( I4B ), INTENT( IN ) :: iel( 2 )
  INTEGER( I4B ), ALLOCATABLE :: Ans( :, : )
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

MODULE PURE SUBROUTINE init_elem_nodes( Obj, MeshObj )
  CLASS( MeshData_ ), INTENT( INOUT ) :: Obj
    !! mesh data
  CLASS( Mesh_ ), INTENT( INOUT) :: MeshObj
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
!	val = Obj % elementToNodes( iel )
! ```

MODULE PURE FUNCTION get_elem_nodes( Obj, iel ) RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: Obj
    !! mesh data
  INTEGER( I4B ), INTENT( IN ) :: iel
    !! element number
  INTEGER( I4B ), ALLOCATABLE :: Ans( : )
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
!	call obj % initiateBoundaryData( MeshObj )
! ```

MODULE PURE SUBROUTINE init_bndy_data( Obj, MeshObj )
  CLASS( MeshData_ ), INTENT( INOUT) :: Obj
    !! mesh data
  CLASS( Mesh_ ), INTENT( INOUT) :: MeshObj
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
!	is = Obj % isBoundaryElement( iel )
! ```

MODULE PURE FUNCTION is_bndy_elem( Obj, iel ) RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: Obj
    !! mesh data
  INTEGER( I4B ), INTENT( IN ) :: iel
    !! element number
  LOGICAL( LGT ) :: Ans
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
!	val = Obj % boundaryElementData(iel)
! ```

MODULE PURE FUNCTION get_bndy_elem( Obj, iel ) RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: Obj
    !! mesh data
  INTEGER( I4B ), INTENT( IN ) :: iel
    !! element number
  INTEGER( I4B ), ALLOCATABLE :: Ans( : )
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
!	call obj % initiateInternalNptrs( MeshObj )
! ```

MODULE PURE SUBROUTINE init_int_nptrs( Obj, MeshObj )
  CLASS( MeshData_ ), INTENT( INOUT) :: Obj
    !! mesh data
  CLASS( Mesh_ ), INTENT( INOUT ) :: MeshObj
    !! mesh object
END SUBROUTINE init_int_nptrs
END INTERFACE

!----------------------------------------------------------------------------
!                                               InitiateInternalBoundaryData
!----------------------------------------------------------------------------

INTERFACE
!! Initiate internal boundary data

MODULE PURE SUBROUTINE init_int_bndydata( Obj, MeshObj )
  CLASS( MeshData_ ), INTENT( INOUT) :: Obj
  CLASS( Mesh_ ), INTENT( INOUT) :: MeshObj
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
!	call setSparsity( Obj, MeshObj, Mat )
! ```

MODULE PURE SUBROUTINE setSparsity_1( Obj, MeshObj, Mat, map )
  CLASS( MeshData_ ), INTENT( INOUT) :: Obj
    !! mesh data
  TYPE( Mesh_ ), INTENT( INOUT) :: MeshObj
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
! `Obj`

MODULE SUBROUTINE get_facet_elements( mdObj, Obj, facetmesh, feObj )
  CLASS( MeshData_ ), INTENT( INOUT) :: mdObj
    !! Mesh data of parent mesh
  CLASS( Mesh_ ), INTENT( INOUT) :: Obj
    !! Parent mesh
  CLASS( Mesh_ ), INTENT( INOUT) :: facetmesh
    !! facet mesh to be constructed
  CLASS( Element_ ), INTENT( IN ) :: feObj
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
MODULE FUNCTION md_quality( Obj, MeshObj, Nodes, Measure ) RESULT( Ans )
  CLASS( MeshData_ ), INTENT( INOUT ) :: Obj
  CLASS( Mesh_ ), TARGET, INTENT( INOUT) :: Meshobj
  REAL( DFP ) , INTENT( IN ) :: Nodes(:,:)
  INTEGER( I4B ), INTENT( IN ) :: Measure
  REAL( DFP ), ALLOCATABLE :: Ans(:)
END FUNCTION md_quality
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 FindElement
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION md_findelement( Obj, MeshObj, coord, nodes ) &
  & RESULT( Ans )
  CLASS( MeshData_ ), INTENT( IN ) :: Obj
  CLASS( Mesh_ ), INTENT( IN ) :: MeshObj
  REAL( DFP ), INTENT( IN ) :: coord(:,:)
  REAL( DFP ), INTENT( IN ) :: nodes(:,:)
  REAL( DFP ) :: Ans( SIZE( coord, 2 ) )
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
! obj => MeshData_Pointer( MeshObj )
! ```

FUNCTION meshdata_ptr_1( MeshObj ) RESULT( Ans )
  CLASS( MeshData_ ), POINTER :: Ans
    !! MeshData_ object
  CLASS( Mesh_ ), INTENT( INOUT ) :: MeshObj
    !! Mesh_ Object
  ALLOCATE( MeshData_ :: Ans )
  CALL Ans % Initiate( MeshObj )
END FUNCTION meshdata_ptr_1

END MODULE MeshData_Class