MODULE MeshConnectivity_Class
  !! This module defines a class for handling mesh connectivity
USE BaseType
USE GlobalData
USE FE
USE Mesh_Class
USE MeshData_Class
IMPLICIT NONE
PRIVATE

!----------------------------------------------------------------------------
!                                                          MeshConnectivity_
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! [[MeshConnectivity_]] contains data connectivity data between two meshobject

TYPE :: MeshConnectivity_
  INTEGER( I4B ), ALLOCATABLE :: CellFacet( :, : )
    !! Cell to facet connectivity data
  INTEGER( I4B ), ALLOCATABLE :: CellCell( :, : )
    !! Cell to cell connectivity data
  INTEGER( I4B ), ALLOCATABLE :: NodeToNodes( :, : )
    !! Node to nodes connectivity data

  CONTAINS
    PROCEDURE, PUBLIC, PASS( Obj ) :: Finalize => mc_deallocate_data
      !! Deallocate data stored in the object
    PROCEDURE, PUBLIC, PASS( Obj ) :: InitiateFacetToCellData => &
      & mc_init_cell_facet
      !! Initiate facet to cell connectivity data
    PROCEDURE, PUBLIC, PASS( Obj ) :: mc_cell_of_facet
      !! Return the cell number of a given facet
    PROCEDURE, PUBLIC, PASS( Obj ) :: mc_cells_of_facets
      !! Return the cell numbers of given facet elements
    GENERIC, PUBLIC :: CellNumber => mc_cell_of_facet, &
      & mc_cells_of_facets
      !! Return the cell numbers of given facet elements
    PROCEDURE, PUBLIC, PASS( Obj ) :: mc_facet_local_id_1
      !! Return the facet local id in cell element
    PROCEDURE, PUBLIC, PASS( Obj ) :: mc_facet_local_id_2
      !! Return the facet local id in cell element
    GENERIC, PUBLIC :: FacetLocalID => mc_facet_local_id_1, &
      & mc_facet_local_id_2
      !! Return the facet local id in cell element
    PROCEDURE, PUBLIC, PASS( Obj ) :: InitiateNodeToNodeData => &
      & mc_init_node_node
      !! Initiate the node to node connectivity between two meshes
END TYPE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PUBLIC :: MeshConnectivity_
TYPE( MeshConnectivity_ ), PARAMETER, PUBLIC :: &
  & TypeMeshConnectivity = MeshConnectivity_( &
  & CellFacet = NULL( ), &
  & CellCell = NULL( ), &
  & NodeToNodes = NULL( ) )

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!>
! This data type contains the pointer of [[MeshConnectivity_]]
TYPE :: MeshConnectivityPointer_
  CLASS( MeshConnectivity_ ), POINTER :: Ptr => NULL( )
END TYPE MeshConnectivityPointer_

PUBLIC :: MeshConnectivityPointer_

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

INTERFACE
!! This subroutine deallocate the data stored in [[MeshConnectivity_]]

!> authors: Dr. Vikas Sharma
!
! This subroutine deallocate the data stored in [[MeshConnectivity_]]
!
!### Usage
!
! ```fortran
!	call DeallocateData( Obj )
! ```

MODULE PURE SUBROUTINE mc_deallocate_data( Obj )
  CLASS( MeshConnectivity_ ), INTENT( INOUT) :: Obj
    !! Mesh connectivity object
END SUBROUTINE mc_deallocate_data
END INTERFACE

!>
! Generic subroutine to deallocate data stored inside [[MeshConnectivity_]]
INTERFACE DeallocateData
  MODULE PROCEDURE mc_deallocate_data
END INTERFACE DeallocateData

!----------------------------------------------------------------------------
!                                                    InitiateFacetToCellData
!----------------------------------------------------------------------------

INTERFACE
!! Generate the connectivity matrix between cell and facet mesh.

!> authors: Dr. Vikas Sharma
!
! This subroutine generate the connectivity matrix called Obj % CellFacet
! between cell and facet mesh.
!
!  - The output result will be an integer array with 2 rows
!  - First row contains the element number of `CellMesh`
!  - Second row contains the local facet number of cell element which
!  connects to the facet mesh element.
!  - Each column of `Obj % CellFacet` corresponds to an Element of
!  `FacetMesh`; total number of columns are same as total number of elem
!  in the `FacetMesh`
!  - if an element of `FacetMesh` is orphan then its corresponding entry
!  is set to zero in `Obj % CellFacet` matrix
!
!### Usage
!
! ```fortran
!	call Obj % initiateFacetToCellData( Obj, CellMesh, FacetMesh, CellMeshData)
! ```

MODULE PURE SUBROUTINE mc_init_cell_facet( Obj, CellMesh, FacetMesh, &
  & CellMeshData )
  CLASS( MeshConnectivity_ ), INTENT( INOUT) :: Obj
    !! Mesh connectivity data
  CLASS( Mesh_ ), INTENT( INOUT ) :: CellMesh
    !! Mesh of cell elements
  CLASS( Mesh_ ), INTENT( INOUT ) :: FacetMesh
    !! Mesh of facet mesh
  CLASS( MeshData_ ), INTENT( INOUT) :: CellMeshData
    !! Mesh data for `CellMesh`
END SUBROUTINE mc_init_cell_facet
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 CellNumber
!----------------------------------------------------------------------------

INTERFACE
!! Returns cell number of given facet number

!> authors: Dr. Vikas Sharma
!
! - Returns cell number of given facet number
! - if cell number is zero it means facet element is an orphan
!
!### Usage
!
! ```fortran
!	id = obj % CellNumber( facetNum )
! ```

MODULE PURE FUNCTION mc_cell_of_facet( Obj,  FacetNum ) RESULT( Ans )
  CLASS( MeshConnectivity_ ), INTENT( IN ) :: Obj
    !! Mesh connectivity data
  INTEGER( I4B ), INTENT( IN ) :: FacetNum
    !! Facet element number
  INTEGER( I4B ) :: Ans
    !! Cell number
END FUNCTION mc_cell_of_facet
END INTERFACE

INTERFACE
!! Returns cell number of given facet number

!> authors: Dr. Vikas Sharma
!
! - Returns cell number of given facet number
! - if cell number is zero it means facet element is an orphan
!
!### Usage
!
! ```fortran
!	id = obj % CellNumber( facetNum )
! ```

MODULE PURE FUNCTION mc_cells_of_facets( Obj, FacetNum ) RESULT( Ans )
  CLASS( MeshConnectivity_ ), INTENT( IN ) :: Obj
    !! Mesh connectivity data
  INTEGER( I4B ), INTENT( IN ) :: FacetNum( : )
    !! List of facet element numbers
  INTEGER( I4B ) :: Ans( SIZE( FacetNum ) )
    !! List of cell element numbers
END FUNCTION mc_cells_of_facets
END INTERFACE

!----------------------------------------------------------------------------
!                                                               FacetLocalID
!----------------------------------------------------------------------------

INTERFACE
!! Returns the local facet id of cell element

!> authors: Dr. Vikas Sharma
!
! Returns the local facet id of cell element which is in contact with
! facet element
!
!### Usage
!
! ```fortran
!	id = Obj % FacetLocalID( FacetNum )
! ```

MODULE PURE FUNCTION mc_facet_local_id_1( Obj, FacetNum ) RESULT( Ans )
  CLASS( MeshConnectivity_ ), INTENT( IN ) :: Obj
    !! Mesh connectivity object
  INTEGER( I4B ), INTENT( IN ) :: FacetNum
    !! Facet element number
  INTEGER( I4B ) :: Ans
    !! Local facet ID
END FUNCTION mc_facet_local_id_1
END INTERFACE

INTERFACE
!! Returns the local facet id of cell element

!> authors: Dr. Vikas Sharma
!
! Returns the local facet id of cell element which is in contact with
! facet element
!
!### Usage
!
! ```fortran
!	id = Obj % FacetLocalID( FacetNum )
! ```

MODULE PURE FUNCTION mc_facet_local_id_2( Obj, FacetNum ) RESULT( Ans )
  CLASS( MeshConnectivity_ ), INTENT( IN ) :: Obj
    !! Mesh connectivity data
  INTEGER( I4B ), INTENT( IN ) :: FacetNum( : )
    !! List of facet element numbers
  INTEGER( I4B ) :: Ans( SIZE( FacetNum ) )
    !! List of local facet IDs
END FUNCTION mc_facet_local_id_2
END INTERFACE

!----------------------------------------------------------------------------
!                                                          setNodeToNodeData
!----------------------------------------------------------------------------

INTERFACE
!! Generate the connectivity matrix between two meshes

!> authors: Dr. Vikas Sharma
!
!	 This subroutine generate the connectivity matrix between two meshes
!  The output result will be an integer array with 2 columns
!       - first column: contains the node number of Mesh1
!       - second column: contains the node number of Mesh2 which is
!       - directly connected to the node 1

MODULE PURE SUBROUTINE mc_init_node_node( Obj, Mesh1, Mesh2, &
  & Node1, Node2, MeshData1, MeshData2 )
  CLASS( MeshConnectivity_ ), INTENT( INOUT ) :: Obj
    !! mesh connectivity object
  CLASS( Mesh_ ), INTENT( IN ) :: Mesh1
    !! Mesh object
  CLASS( Mesh_ ), INTENT( IN ) :: Mesh2
    !! Mesh object
  REAL( DFP ), INTENT( IN ) :: Node1( :, : )
    !! Nodal coordinates in `Mesh1`
  REAL( DFP ), INTENT( IN ) :: Node2( :, : )
    !! Nodal coordinate in `Mesh2`
  CLASS( MeshData_ ), INTENT( INOUT ) :: MeshData1
    !! Mesh data for mesh 1
  CLASS( MeshData_ ), INTENT( INOUT ) :: MeshData2
    !! Mesh data for mesh 2
END SUBROUTINE mc_init_node_node
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE MeshConnectivity_Class