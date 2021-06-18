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

!> authors: Vikas Sharma, Ph. D.
! date: 18 June 2021
! summary: This module contains methods for domain data type
!
!{!pages/Domain.md}


MODULE Domain_Class
USE BaseType
USE GlobalData
USE Mesh_Class
USE MeshPointerVector_Class
USE ElementFactory
USE ExceptionHandler_Class
IMPLICIT NONE
PRIVATE

CHARACTER( LEN = * ), PARAMETER :: modName = "DOMAIN_CLASS"
TYPE( ExceptionHandler_ ), PUBLIC :: eDomain

!----------------------------------------------------------------------------
!                                                                DomainData_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 June 2021
! summary: DomainData_ contains data of Domain_
!
!{!pages/Domain.md}

TYPE :: DomainData_
  PRIVATE
  TYPE( String ) :: engine
    !! Engine used for generating the meshes
  INTEGER( I4B ) :: majorVersion = 0
    !! Major version
  INTEGER( I4B ) :: minorVersion = 0
    !! Minor version
  INTEGER( I4B ), ALLOCATABLE :: NSD( : )
    !! Spatial dimension of each physical entities
  INTEGER( I4B ), ALLOCATABLE :: tag( : )
    !! Unique ID of each physical entities
  INTEGER( I4B ), ALLOCATABLE :: numElements( : )
    !! Number of elements in each physical entity
  INTEGER( I4B ), ALLOCATABLE :: numNodes( : )
    !! Number of nodes in each physical entity
  TYPE( IntVector_ ), ALLOCATABLE :: entities( : )
    !! Tags of Entities in each physical entities
  TYPE( String ), ALLOCATABLE :: physicalName( : )
    !! Physical name of each physical group
  INTEGER( I4B ) :: tElements( 0:3 ) = [0,0,0,0]
    !! total number of elements inside the domain
    !! tElements( 0 ) = total number of point elements
    !! tElements( 1 ) = total number of line elements
    !! tElements( 2 ) =  total number of surface elements
    !! tElements( 3 ) = total number of volume/cell elements
  INTEGER( I4B ) :: maxNptrs = 0
    !! Largest node number in the domain
  INTEGER( I4B ) :: minNptrs = 0
    !! Smallest node number in the domain
  LOGICAL( I4B ) :: isNodeNumberSparse = .FALSE.
    !! True if node numbers are not continuous
  INTEGER( I4B ) :: maxElemNum = 0
    !! Largest element number in the domain
  INTEGER( I4B ) :: minElemNum = 0
    !! Smallest element number in the domain
  LOGICAL( LGT ) :: isElemNumberSparse = .FALSE.
    !! True if element numbers are sparse
  INTEGER( I4B ) :: tEntities( 0:3 ) = [0,0,0,0]
    !! total number of entities inside the domain
    !! tEntities( 1 ) = total number of point entities, Points
    !! tEntities( 1 ) = total number of line entities, Edge
    !! tEntities( 2 ) = total number of surface entities, Boundary
    !! tEntities( 3 ) = total number of volume entities, Omega
  INTEGER( I4B ) :: tEntitiesForNodes = 0
    !! Total number of entities required for reading nodes
  INTEGER( I4B ) :: tEntitiesForElements = 0
    !! Total number of entities required for reading elements
  TYPE( String ), ALLOCATABLE :: omega_name( : )
    !! Physical names of cell mesh entity
  TYPE( String ), ALLOCATABLE :: boundary_name( : )
    !! Physical names of boundary mesh entity
  TYPE( String ), ALLOCATABLE :: edge_name( : )
    !! Physical names of edge mesh entity
  CONTAINS
    PRIVATE
    PROCEDURE, PUBLIC, PASS( Obj ) :: Initiate => DomainData_Initiate
    PROCEDURE, PUBLIC, PASS( Obj ) :: DeallocateData => DomainData_DeallocateData
    FINAL :: DomainData_Final
END TYPE DomainData_

!----------------------------------------------------------------------------
!                                                                   Domain_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	18 June 2021
! summary: 	`Domain_` contains the collection of meshes which represents specific parts of domain
!
!{!pages/Domain.md}

TYPE :: Domain_
  LOGICAL( LGT ) :: isInitiated = .FALSE.
  TYPE( DomainData_ ) :: domainData
    !! Domain data
  TYPE( MeshPointerVector_ ) :: meshList( 0:3 )
    !! meshList( 0 ) list of meshes of point entities
    !! meshList( 1 ) list of meshes of line entities
    !! meshList( 2 ) list of meshes of surface entities
    !! meshList( 3 ) list of meshes of volume entities
  CONTAINS
    PROCEDURE, PUBLIC, PASS( obj ) :: Initiate => Domain_Initiate
      !! Constructor for domain
    PROCEDURE, PUBLIC, PASS( obj ) :: DeallocateData => Domain_DeallocateData
      !! Deallocate data store inside the domain
    FINAL :: Domain_Final
END TYPE Domain_

PUBLIC :: Domain_

!----------------------------------------------------------------------------
!                                                             DomainPointer_
!----------------------------------------------------------------------------

TYPE :: DomainPointer_
  CLASS( Domain_ ), POINTER :: Ptr => NULL( )
END TYPE DomainPointer_

PUBLIC :: DomainPointer_

!----------------------------------------------------------------------------
!                                                   Initiate@DomainMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 June 2021
! summary: Initiate the instance of [[Domain_]] object
!
!### Introduction
!
! This routine allocate the memory for [[Domain_]] obj.
! tOmega = total number of cell type meshes
! tBoundary = total number of boundary type meshes
! tEdge = total number of edge type meshes

INTERFACE
MODULE SUBROUTINE Domain_Initiate( obj, tOmega, tBoundary, tEdge )
  CLASS( Domain_ ), INTENT( INOUT ) :: obj
    !! Domain object
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: tOmega
    !! total number of $\Omega$ domains
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: tBoundary
    !! total number of $\Gamma$ domains
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: tEdge
    !! total number of line domains
END SUBROUTINE Domain_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                             DeallocateData@DomainMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	18 June 2021
! summary: 	Deallocate data stored in Domain object

INTERFACE
MODULE SUBROUTINE Domain_DeallocateData( obj )
  CLASS( Domain_ ), INTENT( INOUT) :: obj
    !! Domain object
END SUBROUTINE Domain_DeallocateData
END INTERFACE

!>
! generic interface to deallocate data in [[Domain_]]
INTERFACE DeallocateData
  MODULE PROCEDURE Domain_DeallocateData
END INTERFACE DeallocateData

PUBLIC :: DeallocateData

!----------------------------------------------------------------------------
!                                                     Final@DomainMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 June 2021
! summary: Finalizer

INTERFACE
MODULE SUBROUTINE Domain_Final( obj )
  TYPE( Domain_ ), INTENT( INOUT ) :: obj
END SUBROUTINE Domain_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initiate@DomainDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 June 2021
! summary: Initiate the instance of [[DomainData_]] object

INTERFACE
MODULE SUBROUTINE DomainData_Initiate( obj )
  CLASS( DomainData_ ), INTENT( INOUT ) :: obj
    !! DomainData object
END SUBROUTINE DomainData_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                          DeallocateData@DomainDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	18 June 2021
! summary: 	Deallocate data stored in Domain object

INTERFACE
MODULE SUBROUTINE DomainData_DeallocateData( obj )
  CLASS( DomainData_ ), INTENT( INOUT) :: obj
    !! Domain object
END SUBROUTINE DomainData_DeallocateData
END INTERFACE

!>
! generic interface to deallocate data in [[Domain_]]
INTERFACE DeallocateData
  MODULE PROCEDURE DomainData_DeallocateData
END INTERFACE DeallocateData

!----------------------------------------------------------------------------
!                                                   Final@DomainDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 June 2021
! summary: Finalizer

INTERFACE
MODULE SUBROUTINE DomainData_Final( obj )
  TYPE( DomainData_ ), INTENT( INOUT ) :: obj
END SUBROUTINE DomainData_Final
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END MODULE Domain_Class