!> authors: Dr. Vikas Sharma
!
! `Domain_Class` module contains the collection of meshes which represent specific
! parts of domain
!

MODULE Domain_Class
  !! This module defines `Domain_` class
USE BaseType
USE GlobalData
USE Mesh_Class
USE MeshData_Class
USE MeshConnectivity_Class
USE FE
IMPLICIT NONE

PRIVATE

!----------------------------------------------------------------------------
!                                                                    Domain
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! `Domain_` contains the collection of meshes which represent specific
! parts of domain

TYPE :: Domain_
  TYPE( MeshPointer_ ), ALLOCATABLE :: Omega( : )
    !! Mesh of cell elements
  TYPE( MeshPointer_ ), ALLOCATABLE :: Boundary( : )
    !! Mesh of boundary elements
  TYPE( MeshPointer_ ), ALLOCATABLE :: Edge( : )
    !! Mesh of lines
  TYPE( MeshDataPointer_ ), ALLOCATABLE :: mdOmega( : )
    !! Mesh data for `Omega(:)`
  TYPE( MeshDataPointer_ ), ALLOCATABLE :: mdBoundary( : )
    !! Mesh data for `Boundary(:)`
  TYPE( MeshDataPointer_ ), ALLOCATABLE :: mdEdge( : )
    !! Mesh data for `Boundary(:)`
  REAL( DFP ), POINTER :: Nodes( :, : ) => NULL( )
  REAL( DFP ), ALLOCATABLE :: NodalVelocity( :, : )
  REAL( DFP ), ALLOCATABLE :: NodalAcceleration( :, : )
  LOGICAL( LGT ) :: allocateNodes = .FALSE.
    !! This indicates whether nodes are allocated inside the domain or outside
  TYPE( String ), ALLOCATABLE :: omega_name( : )
    !! Physical names of omega
  TYPE( String ), ALLOCATABLE :: boundary_name( : )
    !! physical names of boundary
  TYPE( String ), ALLOCATABLE :: edge_name( : )
    !! physical names of edge

  CONTAINS
    PROCEDURE, PUBLIC, PASS( obj ) :: Initiate => Initiate_obj
      !! Constructor for domain
    PROCEDURE, PUBLIC, PASS( obj ) :: Finalize => deallocate_obj
      !! Deallocate data store inside the domain
    PROCEDURE, PUBLIC, PASS( Dom ) :: ConnectFacetToCell => &
      & mc_connect_facet_cell
END TYPE Domain_

!----------------------------------------------------------------------------
!                                                                    Domain_
!----------------------------------------------------------------------------

PUBLIC :: Domain_
TYPE( Domain_ ), PARAMETER, PUBLIC :: &
  & TypeDomain = Domain_( &
    & Omega = NULL( ), &
    & Boundary = NULL( ), &
    & Edge = NULL( ), &
    & mdOmega = NULL( ), &
    & mdBoundary = NULL( ), &
    & mdEdge = NULL( ), &
    & omega_name = NULL( ), &
    & boundary_name = NULL( ), &
    & edge_name = NULL( ), &
    & NodalVelocity = NULL( ), &
    & NodalAcceleration = NULL( ) )

!----------------------------------------------------------------------------
!                                                             DomainPointer_
!----------------------------------------------------------------------------

!> authors: Dr. Vikas Sharma
!
! It contains pointer to [[Domain_]] object
TYPE :: DomainPointer_
  CLASS( Domain_ ), POINTER :: Ptr => NULL( )
END TYPE DomainPointer_

PUBLIC :: DomainPointer_

!----------------------------------------------------------------------------
!                                                           Initiate@Methods
!----------------------------------------------------------------------------

INTERFACE
!! Initiate [[Domain_]] object

!> authors: Dr. Vikas Sharma
!
! This routine allocate the memory for [[Domain_]] obj.
! - allocate size of `obj % Omega( 1:tOmega )`
! - allocate `obj % Boundary( 1:tBoundary )`,
! - allocate `obj % mdOmega( 1:tOmega )`
! - allocate `obj % mdBoundary( 1:tBoundary )`
!
!### Usage
!
! ```fortran
!	call obj % initiate( tOmega, tBoundary )
! ```

MODULE PURE SUBROUTINE Initiate_obj( obj, tOmega, tBoundary, tEdge )
  CLASS( Domain_ ), INTENT( INOUT ) :: obj
    !! Domain
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: tOmega
    !! total number of $\Omega$ domains
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: tBoundary
    !! total number of $\Gamma$ domains
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: tEdge
    !! total number of line domains
END SUBROUTINE Initiate_obj
END INTERFACE

!>
! Generic routine for initiating [[Domain_]] object
INTERFACE Initiate
  MODULE PROCEDURE Initiate_obj
END INTERFACE Initiate

PUBLIC :: Initiate

!----------------------------------------------------------------------------
!                                                  Deallocate@Constructor
!----------------------------------------------------------------------------

INTERFACE
!! Deallocate data in [[Domain_]] object

!> authors: Dr. Vikas Sharma
!
! Deallocate data stored in [[Domain_]] object
!
MODULE PURE SUBROUTINE deallocate_obj( obj )
  CLASS( Domain_ ), INTENT( INOUT ) :: obj
    !! Domain object
END SUBROUTINE deallocate_obj
END INTERFACE

!>
! generic interface to deallocate data in [[Domain_]]
INTERFACE Deallocate
  MODULE PROCEDURE deallocate_obj
END INTERFACE Deallocate

PUBLIC :: Deallocate

!----------------------------------------------------------------------------
!                                                 ConnectFacetToCell@Methods
!----------------------------------------------------------------------------

INTERFACE
!! Connect [[mesh_]] of [[facetelement_]] to [[mesh_]] of cell [[element_]]

!> authors: Dr. Vikas Sharma
!
! This subroutine connects the mesh of facet elements to the mesh of
! cell elements
!
! User does not require to access the mesh. One can simply provide the
! index of cell mesh (i.e., dom % omega( cellIndx ) ) and index of facet mesh
! (i.e., dom % boundary( faceIndx )). This subroutine runs several
! tests and reports error if there is any.
!
! @note
! This subroutine is a wrapper around [[mesh_:connectFacetToCell]]
!
!### Usage
!
! ```fortran
!	call ConnectFacetToCell( Dom, OmegaIndx, BoundaryIndx )
! ```

MODULE SUBROUTINE mc_connect_facet_cell( Dom, OmegaIndx, BoundaryIndx )
  CLASS( Domain_ ), INTENT( INOUT ), TARGET :: Dom
  INTEGER( I4B ), INTENT( IN ) :: OmegaIndx
  INTEGER( I4B ), INTENT( IN ) :: BoundaryIndx
END SUBROUTINE mc_connect_facet_cell
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE Domain_Class