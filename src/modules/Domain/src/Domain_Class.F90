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

MODULE Domain_Class
USE BaseType
USE GlobalData
USE Mesh_Class
USE MeshPointerVector_Class
USE ElementFactory
USE ExceptionHandler_Class
USE HDF5File_Class
IMPLICIT NONE
PRIVATE
CHARACTER( LEN = * ), PARAMETER :: modName = "DOMAIN_CLASS"
TYPE( ExceptionHandler_ ) :: e

!----------------------------------------------------------------------------
!                                                                   Domain_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 June 2021
! summary: Domain_ contains data of Domain_

TYPE :: Domain_
  PRIVATE
  LOGICAL( LGT ) :: isInitiated = .FALSE.
    !! flag
  TYPE( String ) :: engine
    !! Engine used for generating the meshes
  INTEGER( I4B ) :: majorVersion = 0
    !! Major version
  INTEGER( I4B ) :: minorVersion = 0
    !! Minor version
  REAL( DFP ) :: Version = 0.0_DFP
    !! Version  MajorVersion.MinorVersion
  INTEGER( I4B ) :: nsd = 0_I4B
    !! number of spatial dimension
  INTEGER( I4B ) :: maxNptrs = 0
    !! Largest node number in the domain
  INTEGER( I4B ) :: minNptrs = 0
    !! Smallest node number in the domain
  INTEGER( I4B ) :: tNodes = 0
    !! Total number of nodes in the mesh
  LOGICAL( I4B ) :: isNodeNumberSparse = .FALSE.
    !! True if node numbers are not continuous
  INTEGER( I4B ) :: maxElemNum = 0
    !! Largest element number in the domain
  INTEGER( I4B ) :: minElemNum = 0
    !! Smallest element number in the domain
  LOGICAL( LGT ) :: isElemNumberSparse = .FALSE.
    !! True if element numbers are sparse
  INTEGER( I4B ) :: tEntitiesForNodes = 0
    !! Total number of entities required for reading nodes
  INTEGER( I4B ) :: tEntitiesForElements = 0
    !! Total number of entities required for reading elements
  INTEGER( I4B ) :: tElements( 0:3 ) = [0,0,0,0]
    !! Total number of elements inside the domain
    !! tElements( 0 ) = total number of point elements
    !! tElements( 1 ) = total number of line elements
    !! tElements( 2 ) =  total number of surface elements
    !! tElements( 3 ) = total number of volume/cell elements
  INTEGER( I4B ) :: tEntities( 0:3 ) = [0,0,0,0]
    !! Total number of entities inside the domain
    !! tEntities( 0 ) = total number of point mesh entities, mesh of Points
    !! tEntities( 1 ) = total number of line mesh entities, mesh of Edge
    !! tEntities( 2 ) = total number of surface mesh entities, mesh Boundary
    !! tEntities( 3 ) = total number of volume mesh entities, Omega
  REAL( DFP ), ALLOCATABLE, PUBLIC :: nodeCoord( :, : )
    !! Nodal coordinates in XiJ format
    !! Number of rows are 3, and number of columns is total nodes
  INTEGER( I4B ), ALLOCATABLE, PUBLIC :: local_nptrs( : )
    !! local_nptrs are required to access the nodeCoord
  TYPE( MeshPointerVector_ ), ALLOCATABLE :: meshList( : )
    !! meshList( 0 ) list of meshes of point entities
    !! meshList( 1 ) list of meshes of line entities
    !! meshList( 2 ) list of meshes of surface entities
    !! meshList( 3 ) list of meshes of volume entities
  CONTAINS
    PRIVATE
    ! @ConstructorMethods
    PROCEDURE, PUBLIC, PASS( obj ) :: addSurrogate => Domain_addSurrogate
      !! Add surrogate to the module error handler
    PROCEDURE, PUBLIC, PASS( Obj ) :: Initiate => Domain_Initiate
      !! Initiate an instance of domain
    PROCEDURE, PUBLIC, PASS( Obj ) :: DeallocateData => Domain_DeallocateData
      !! Deallocate data stored inside an instance of domain
    FINAL :: Domain_Final
      !! Finalizer for domain
    ! @IOMethods
    PROCEDURE, PASS( Obj ) :: Import => Domain_Import
      !! Initiates an instance of domain by importing data from meshfile
    ! @getMethods
    PROCEDURE, PUBLIC, PASS( obj ) :: isNodePresent => &
      & Domain_isNodePresent
    PROCEDURE, PUBLIC, PASS( obj ) :: getTotalNodes => Domain_getTotalNodes
      !! returns the total number of nodes in the mesh
    PROCEDURE, PASS( obj ) :: Domain_getLocalNodeNumber1
    PROCEDURE, PASS( obj ) :: Domain_getLocalNodeNumber2
    GENERIC, PUBLIC :: getLocalNodeNumber => Domain_getLocalNodeNumber1, &
      & Domain_getLocalNodeNumber2
    PROCEDURE, PUBLIC, PASS( obj ) :: getTotalMesh => Domain_getTotalMesh
      !! This routine returns total number of meshes of given dimension
    PROCEDURE, PUBLIC, PASS( obj ) :: getMeshPointer => Domain_getMeshPointer
      !! This routine a pointer to [[Mesh_]] object
    PROCEDURE, PASS( obj ) :: getNodeCoord => Domain_getNodeCoord
      !! This routine returns the nodal coordinate in rank2 array
    PROCEDURE, PUBLIC, PASS( obj ) :: getNodeCoordPointer => &
      & Domain_getNodeCoordPointer
      !! This routine returns the pointer to nodal coordinate
    PROCEDURE, PUBLIC, PASS( obj ) :: getNptrs => &
      & Domain_getNptrs
    PROCEDURE, PUBLIC, PASS( Obj ) :: getNSD => Domain_getNSD
      !! Returns the spatial dimension of each physical entities
    PROCEDURE, PUBLIC, PASS( obj ) :: setSparsity => Domain_setSparsity
END TYPE Domain_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PUBLIC :: Domain_

!----------------------------------------------------------------------------
!                                                             DomainPointer
!----------------------------------------------------------------------------

TYPE :: DomainPointer_
  CLASS( Domain_ ), POINTER :: ptr => NULL( )
END TYPE DomainPointer_

PUBLIC :: DomainPointer_

!----------------------------------------------------------------------------
!                                            addSurrogate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 1 Sept 2021
! summary: This routine addes surrogate to module exception handler

INTERFACE
MODULE SUBROUTINE Domain_addSurrogate( obj, userObj )
  CLASS( Domain_ ), INTENT( INOUT ) :: obj
  TYPE( ExceptionHandler_ ), INTENT( IN ) :: userObj
END SUBROUTINE Domain_addSurrogate
END INTERFACE

!----------------------------------------------------------------------------
!                                            addSurrogate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 1 Sept 2021
! summary: This routine addes surrogate to module exception handler

INTERFACE
MODULE SUBROUTINE addSurrogate_Domain( userObj )
  TYPE( ExceptionHandler_ ), INTENT( IN ) :: userObj
END SUBROUTINE addSurrogate_Domain
END INTERFACE

PUBLIC :: addSurrogate_Domain

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 June 2021
! summary: Initiate the instance of [[Domain_]] object

INTERFACE
MODULE SUBROUTINE Domain_Initiate( obj, hdf5, group )
  CLASS( Domain_ ), INTENT( INOUT ) :: obj
    !! DomainData object
  TYPE( HDF5File_ ), INTENT( INOUT ) :: hdf5
    !! HDF5 file
  CHARACTER( LEN = * ), INTENT( IN ) :: group
    !! Group name (directory name)
END SUBROUTINE Domain_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                          DeallocateData@ConstructorMethods
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

INTERFACE DeallocateData
  MODULE PROCEDURE Domain_DeallocateData
END INTERFACE DeallocateData

PUBLIC :: DeallocateData

!----------------------------------------------------------------------------
!                                                   Final@ConstructorMethods
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
!                                                  Domain@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 June 2021
! summary: Domain methods

INTERFACE
MODULE FUNCTION Domain_Constructor1( hdf5, group ) RESULT( Ans )
  TYPE( HDF5File_ ), INTENT( INOUT ) :: hdf5
  CHARACTER( LEN = * ), INTENT( IN ) :: group
  TYPE( Domain_ ) :: ans
END FUNCTION Domain_Constructor1
END INTERFACE

INTERFACE Domain
  MODULE PROCEDURE Domain_Constructor1
END INTERFACE Domain

PUBLIC :: Domain

!----------------------------------------------------------------------------
!                                          Domain_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 June 2021
! summary: This function returns pointer to a newly constructed Domain obj

INTERFACE
MODULE FUNCTION Domain_Constructor_1( hdf5, group ) RESULT( Ans )
  TYPE( HDF5File_ ), INTENT( INOUT ) :: hdf5
  CHARACTER( LEN = * ), INTENT( IN ) :: group
  CLASS( Domain_ ), POINTER :: ans
END FUNCTION Domain_Constructor_1
END INTERFACE

INTERFACE Domain_Pointer
  MODULE PROCEDURE Domain_Constructor_1
END INTERFACE Domain_Pointer

PUBLIC :: Domain_Pointer

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------


!> authors: Vikas Sharma, Ph. D.
! date: 	18 June 2021
! summary: Construct an instance of domain by importing data from mesh

INTERFACE
MODULE SUBROUTINE Domain_Import( obj, hdf5, group )
  CLASS( Domain_ ), INTENT( INOUT ) :: obj
  TYPE( HDF5File_ ), INTENT( INOUT ) :: hdf5
  CHARACTER( LEN = * ), INTENT( IN ) :: group
END SUBROUTINE Domain_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                   isNodePresent@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Sept 2021
! summary: Returns true if the global node number is present

INTERFACE
MODULE FUNCTION Domain_isNodePresent( obj, globalNode ) RESULT( Ans )
  CLASS( Domain_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: globalNode
  LOGICAL( LGT ) :: ans
END FUNCTION Domain_isNodePresent
END INTERFACE

!----------------------------------------------------------------------------
!                                                 getTotalNodes@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 June 2021
! summary: Returns the total number of nodes in the domain
!
!### Introduction
!
! This function returns the total number of nodes in a given mesh entity
! The mesh entity is given by its ID and its dimension.
!
! - `dim=0` denotes mesh of point entities
! - `dim=1` denotes mesh of curve entities
! - `dim=2` denotes mesh of surface entities
! - `dim=3` denotes mesh of volume entities
! - `entityNum` should not be out of bound

INTERFACE
MODULE FUNCTION Domain_getTotalNodes( obj, entityNum, dim ) RESULT( Ans )
  CLASS( Domain_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: entityNum
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: dim
  INTEGER( I4B ) :: ans
END FUNCTION Domain_getTotalNodes
END INTERFACE

!----------------------------------------------------------------------------
!                                             getLocalNodeNumber@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Sept 2021
! summary: Returns local node number of a global node number
!
!### Introduction
!
! This function returns the local node number of a global node number.


INTERFACE
MODULE FUNCTION Domain_getLocalNodeNumber1( obj, globalNode ) RESULT( Ans )
  CLASS( Domain_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: globalNode
  INTEGER( I4B ) :: ans
END FUNCTION Domain_getLocalNodeNumber1
END INTERFACE

!----------------------------------------------------------------------------
!                                              getLocalNodeNumber@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Sept 2021
! summary: Returns local node number of a global node number
!
!### Introduction
!
! This function returns the local node number of a global node number.

INTERFACE
MODULE FUNCTION Domain_getLocalNodeNumber2( obj, globalNode ) RESULT( Ans )
  CLASS( Domain_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: globalNode( : )
  INTEGER( I4B ) :: ans( SIZE( globalNode ) )
END FUNCTION Domain_getLocalNodeNumber2
END INTERFACE

!----------------------------------------------------------------------------
!                                                    getTotalMesh@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Sept 2021
! summary: This function returns the total number of mesh
!
!### Introduction
!
! This function returns the total number of mesh
!
! - `dim=0` returns the total number of mesh of point entities
! - `dim=1` returns the total number of mesh of curve entities
! - `dim=2` returns the total number of mesh of surface entities
! - `dim=3` returns the total number of mesh of volume entities

INTERFACE
MODULE FUNCTION Domain_getTotalMesh( obj, dim ) RESULT( Ans )
  CLASS( Domain_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: dim
  INTEGER( I4B ) :: ans
END FUNCTION Domain_getTotalMesh
END INTERFACE

!----------------------------------------------------------------------------
!                                                  getMeshPointer@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This rotuine returns mesh pointer
!
!### Introduction
!
! This returns the mesh Entity pointer.
! - dim is the dimension of the mesh; dim=0,1,2,3 corresponds to the point, curve, surface, volume meshes.
! - tag, is the number of mesh

INTERFACE
MODULE FUNCTION Domain_getMeshPointer( obj, dim, entityNum ) RESULT( Ans )
  CLASS( Domain_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: dim
  INTEGER( I4B ), INTENT( IN ) :: entityNum
  CLASS( Mesh_), POINTER :: ans
END FUNCTION Domain_getMeshPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                                     getNodeCoord@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine returns the nodal coordinates
!
!### Introduction
! - This routine returns the nodal coordinates in the form of rank2 array.
! - The nodal coordinates are in XiJ, the columns of XiJ denotes the node number, and the rows correspond to the component.
! - If `dim` and `tag` are absent then this routine returns the nodal coordinates of the entire domain
! - If `dim` and `tag` are present then the routine selects the mesh and returns its nodal coordinates

INTERFACE
MODULE SUBROUTINE Domain_getNodeCoord( obj, nodeCoord, dim, entityNum )
  CLASS( Domain_ ), INTENT( IN ) :: obj
  REAL( DFP ), ALLOCATABLE, INTENT( INOUT ) :: nodeCoord( :, : )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: dim
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: entityNum
END SUBROUTINE Domain_getNodeCoord
END INTERFACE

!----------------------------------------------------------------------------
!                                             getNodeCoordPointer@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine returns the pointer to nodal coordinates
!
!### Introduction
! - This routine returns the pointer to nodal coordinates in the form of rank2 array.
! - The nodal coordinates are in XiJ, the columns of XiJ denotes the node number, and the rows correspond to the component.

INTERFACE
MODULE FUNCTION Domain_getNodeCoordPointer( obj ) RESULT( ans )
  CLASS( Domain_ ), TARGET, INTENT( IN ) :: obj
  REAL( DFP ), POINTER :: ans( :, : )
END FUNCTION Domain_getNodeCoordPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                                         getNptrs@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2 Sept 2021
! summary: this routine returns the global node number
!
!### Introduction
! This routine returns the global node number
! xidim is the dimension of the mesh

INTERFACE
MODULE FUNCTION Domain_getNptrs( obj, meshID, xidim ) RESULT( Ans )
  CLASS( Domain_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: meshID( : )
  INTEGER( I4B ), INTENT( IN ) :: xidim
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
END FUNCTION Domain_getNptrs
END INTERFACE

!----------------------------------------------------------------------------
!                                                           getNSD@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Sept 2021
! summary: This routine returns the number of spatial dimensions

INTERFACE
MODULE PURE FUNCTION Domain_getNSD( obj ) RESULT( Ans )
  CLASS( Domain_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: ans
END FUNCTION Domain_getNSD
END INTERFACE

!----------------------------------------------------------------------------
!                                                     setSparsity@setMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE Domain_setSparsity( obj, mat )
  CLASS( Domain_ ), INTENT( IN ) :: obj
  TYPE( CSRMatrix_ ), INTENT( INOUT ) :: mat
END SUBROUTINE Domain_setSparsity
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END MODULE Domain_Class