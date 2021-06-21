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
USE HDF5File_Class
IMPLICIT NONE
PRIVATE

CHARACTER( LEN = * ), PARAMETER :: modName = "DOMAIN_CLASS"
TYPE( ExceptionHandler_ ) :: eDomain
INTEGER( I4B ), PARAMETER :: eUnitNo = 1001
CHARACTER( LEN = * ), PARAMETER :: eLogFile="DOMAIN_CLASS_EXCEPTION.txt"

!----------------------------------------------------------------------------
!                                                                   Domain_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 June 2021
! summary: Domain_ contains data of Domain_
!
!{!pages/Domain.md}

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
  INTEGER( I4B ), ALLOCATABLE :: NSDVec( : )
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
  INTEGER( I4B ) :: tEntities( 0:3 ) = [0,0,0,0]
    !! total number of entities inside the domain
    !! tEntities( 1 ) = total number of point entities, Points
    !! tEntities( 1 ) = total number of line entities, Edge
    !! tEntities( 2 ) = total number of surface entities, Boundary
    !! tEntities( 3 ) = total number of volume entities, Omega
  REAL( DFP ), ALLOCATABLE, PUBLIC :: nodeCoord( :, : )
    !! Nodal coordinates in XiJ format
  INTEGER( I4B ), ALLOCATABLE, PUBLIC :: local_nptrs( : )
    !! local_nptrs are required to access the nodeCoord
  TYPE( MeshPointerVector_ ), ALLOCATABLE :: meshList( : )
    !! meshList( 0 ) list of meshes of point entities
    !! meshList( 1 ) list of meshes of line entities
    !! meshList( 2 ) list of meshes of surface entities
    !! meshList( 3 ) list of meshes of volume entities
  CONTAINS
    PRIVATE
    PROCEDURE, PUBLIC, PASS( Obj ) :: Initiate => Domain_Initiate
      !! Initiate domain data
    PROCEDURE, PUBLIC, PASS( Obj ) :: DeallocateData => Domain_DeallocateData
      !! Deallocate data stored inside the domain data
    FINAL :: Domain_Final
    PROCEDURE, PASS( Obj ) :: Import => Domain_Import
      !! Import entire domain data

    PROCEDURE, PUBLIC, PASS( obj ) :: getTotalPhysicalEntities => &
      & Domain_getTotalPhysicalEntities
      !! Returns total number of physical entities, points, surface, volumes

    GENERIC, PUBLIC :: getIndex => &
      & Domain_getIndex_a, Domain_getIndex_b, &
      & Domain_getIndex_c, Domain_getIndex_d, &
      & Domain_getIndex_e
    PROCEDURE, PASS( obj ) :: Domain_getIndex_a
      !! Returns the index of a physical group
    PROCEDURE, PASS( obj ) :: Domain_getIndex_b
      !! Returns the index of a physical group
    PROCEDURE, PASS( obj ) :: Domain_getIndex_c
      !! Returns the index of a physical group
    PROCEDURE, PASS( obj ) :: Domain_getIndex_d
      !! Returns the index of a physical group
    PROCEDURE, PASS( Obj ) :: Domain_getIndex_e

    PROCEDURE, PUBLIC, PASS( Obj ) :: getPhysicalNames => &
      & Domain_getPhysicalNames
      !! Returns the physical names
    PROCEDURE, PUBLIC, PASS( Obj ) :: getPhysicalTags => &
      & Domain_getPhysicalTags
      !! Returns tags of physical entities
    PROCEDURE, PUBLIC, PASS( obj ) :: WhoAmI => Domain_WhoAmI
      !! Enquire about "volume, surface, curve, point'
    PROCEDURE, PUBLIC, PASS( Obj ) :: AppendEntities => &
      & Domain_AppendEntities
      !! Append entries to entities
    PROCEDURE, PUBLIC, PASS( Obj ) :: IncNumElements => &
      & Domain_IncNumElements
    PROCEDURE, PUBLIC, PASS( Obj ) :: IncNumNodes => Domain_IncNumNodes
    PROCEDURE, PUBLIC, PASS( Obj ) :: getEntities => Domain_getEntities
    PROCEDURE, PUBLIC, PASS( Obj ) :: getNSD => Domain_getNSD
    PROCEDURE, PUBLIC, PASS( Obj ) :: getNumElements => &
      & Domain_getNumElements
    PROCEDURE, PUBLIC, PASS( Obj ) :: getNumNodes => Domain_getNumNodes
    PROCEDURE, PUBLIC, PASS( Obj ) :: setNumNodes => Domain_setNumNodes
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
!                                                     Initiate@DomainMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 June 2021
! summary: Initiate the instance of [[Domain_]] object

INTERFACE
MODULE SUBROUTINE Domain_Initiate( obj, meshFile )
  CLASS( Domain_ ), INTENT( INOUT ) :: obj
    !! DomainData object
  TYPE( HDF5File_ ), INTENT( INOUT ) :: meshFile
END SUBROUTINE Domain_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                      Import@DomainMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	18 June 2021
! summary: Import domain data

INTERFACE
MODULE SUBROUTINE Domain_Import( obj, meshFile )
  CLASS( Domain_ ), INTENT( INOUT ) :: obj
  TYPE( HDF5File_ ), INTENT( INOUT ) :: meshFile
END SUBROUTINE Domain_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                               DeallocateData@DomainMethods
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

!----------------------------------------------------------------------------
!                                                        Final@DomainMethods
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
!                                                       Domain@DomainMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 June 2021
! summary: Domain methods

INTERFACE
MODULE FUNCTION Domain_Constructor1( meshFile ) RESULT( Ans )
  TYPE( HDF5File_ ), INTENT( INOUT ) :: meshFile
  TYPE( Domain_ ) :: ans
END FUNCTION Domain_Constructor1
END INTERFACE

INTERFACE Domain
  MODULE PROCEDURE Domain_Constructor1
END INTERFACE Domain

PUBLIC :: Domain

!----------------------------------------------------------------------------
!                                               Domain_Pointer@DomainMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 June 2021
! summary: Domain methods

INTERFACE
MODULE FUNCTION Domain_Constructor_1( meshFile ) RESULT( Ans )
  TYPE( HDF5File_ ), INTENT( INOUT ) :: meshFile
  CLASS( Domain_ ), POINTER :: ans
END FUNCTION Domain_Constructor_1
END INTERFACE

INTERFACE Domain_Pointer
  MODULE PROCEDURE Domain_Constructor_1
END INTERFACE Domain_Pointer

PUBLIC :: Domain_Pointer

!----------------------------------------------------------------------------
!                                                  getTotalPhysicalEntities
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	10 June 2021
! summary: This function returns total number of physical entities
!
!### Introduction
! 	Following usage
!
!### Usage
!
!```fortran
!	m = getTotalPhysicalEntities()
!	m = getTotalPhysicalEntities([0])
!	m = getTotalPhysicalEntities([1])
!	m = getTotalPhysicalEntities([2])
!	m = getTotalPhysicalEntities([3])
!	m = getTotalPhysicalEntities([0,1])
!	m = getTotalPhysicalEntities([0,1,2])
!	m = getTotalPhysicalEntities([0,1,3])
!
!```

INTERFACE
MODULE PURE FUNCTION Domain_getTotalPhysicalEntities( obj, dim ) RESULT( ans )
  CLASS( Domain_ ), INTENT( IN ):: obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: dim(:)
  INTEGER( I4B ) :: ans
END FUNCTION Domain_getTotalPhysicalEntities
END INTERFACE

!----------------------------------------------------------------------------
!                                                     getTotalPhysicalPoints
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	10 June 2021
! summary: This function returns total number of physical points

INTERFACE
MODULE PURE FUNCTION getTotalPhysicalPoints( obj ) RESULT( ans )
  CLASS( Domain_ ), INTENT( IN ):: obj
  INTEGER( I4B ) :: ans
END FUNCTION getTotalPhysicalPoints
END INTERFACE

!----------------------------------------------------------------------------
!                                                     getTotalPhysicalCurves
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	10 June 2021
! summary: 	This function returns total number of physical curves

INTERFACE
MODULE PURE FUNCTION getTotalPhysicalCurves( obj ) RESULT( ans )
  CLASS( Domain_ ), INTENT( IN ):: obj
  INTEGER( I4B ) :: ans
END FUNCTION getTotalPhysicalCurves
END INTERFACE

!----------------------------------------------------------------------------
!                                                  getTotalPhysicalSurfaces
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	10 June 2021
! summary: 	 This function returns total number of physical surfaces

INTERFACE
MODULE PURE FUNCTION getTotalPhysicalSurfaces( obj ) RESULT( ans )
  CLASS( Domain_ ), INTENT( IN ):: obj
  INTEGER( I4B ) :: ans
END FUNCTION getTotalPhysicalSurfaces
END INTERFACE

!----------------------------------------------------------------------------
!                                                   getTotalPhysicalVolumes
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	10 June 2021
! summary: 	This function returns total number of physical volumes

INTERFACE
MODULE PURE FUNCTION getTotalPhysicalVolumes( obj ) RESULT( ans )
  CLASS( Domain_ ), INTENT( IN ):: obj
  INTEGER( I4B ) :: ans
END FUNCTION getTotalPhysicalVolumes
END INTERFACE

!----------------------------------------------------------------------------
!                                                 getIndex@mshPhysicalNames
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	10 June 2021
! summary: 	 This function returns index of a given physical name

INTERFACE
MODULE PURE FUNCTION Domain_getIndex_a( obj, Name ) RESULT( ans )
  CLASS( Domain_ ), INTENT( IN ) :: obj
  TYPE( String ), INTENT( IN ) :: Name
  INTEGER( I4B ) :: ans
END FUNCTION Domain_getIndex_a
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 getIndex
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	10 June 2021
! summary: 	 This function returns index of a given physical name

INTERFACE
MODULE PURE FUNCTION Domain_getIndex_b( obj, Name ) RESULT( ans )
  CLASS( Domain_ ), INTENT( IN ) :: obj
  TYPE( String ), INTENT( IN ) :: Name( : )
  INTEGER( I4B ) :: ans( SIZE( Name ) )
END FUNCTION Domain_getIndex_b
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 getIndex
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	20 June 2021
! summary: 	 This function returns index of a given physical name
!
!### Introduction
! This function returns index of a given physical name
!
! - `dim` denotes the XiDImension of physical entity
!     - 0 => point
!     - 1 => line
!     - 2 => surface
!     - 3 => volume
! - `tag` denotes the Physical Tag of physical entity

INTERFACE
MODULE PURE FUNCTION Domain_getIndex_c( obj, dim, tag ) RESULT( ans )
  CLASS( Domain_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: dim, tag
  INTEGER( I4B ) :: ans
END FUNCTION Domain_getIndex_c
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 getIndex
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	20 June 2021
! summary: 	This function returns index of a given physical name
!
!### Introduction
!
! This function returns index of a given physical name
!
! - `dim` denotes the XiDImension of physical entity
! - `dim` = 0 => returns indices of points
! - `dim` = 1 => returns indices of lines
! - `dim` = 2 => returns indices of surfaces
! - `dim` = 3 => returns indices of volumes

INTERFACE
MODULE PURE FUNCTION Domain_getIndex_d( obj, dim ) RESULT( ans )
  CLASS( Domain_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: dim
  INTEGER( I4B ), ALLOCATABLE  :: ans( : )
END FUNCTION Domain_getIndex_d
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 getIndex
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	20 June 2021
! summary: 	 This function returns index of a given physical name
!
!### Introduction
! This function returns index of a given physical name
!
! - `dim` denotes the XiDImension of physical entity
!     - 0 => point
!     - 1 => line
!     - 2 => surface
!     - 3 => volume
! - `tag` denotes the Physical TagS of physical entity

INTERFACE
MODULE PURE FUNCTION Domain_getIndex_e( obj, dim, tag ) RESULT( ans )
  CLASS( Domain_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: dim, tag( : )
  INTEGER( I4B ) :: ans( SIZE( tag ) )
END FUNCTION Domain_getIndex_e
END INTERFACE

!----------------------------------------------------------------------------
!                                                          getPhysicalNames
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 10 June 2021
! summary: This function returns the names of physical entities

INTERFACE
MODULE PURE FUNCTION Domain_getPhysicalNames( obj, dim ) RESULT( ans )
  CLASS( Domain_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: dim
  TYPE( String ), ALLOCATABLE :: ans( : )
END FUNCTION Domain_getPhysicalNames
END INTERFACE

!----------------------------------------------------------------------------
!                                                      getPhysicalPointNames
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	10 June 2021
! summary: This function returns the names of physical poins

INTERFACE
MODULE PURE FUNCTION getPhysicalPointNames( obj ) RESULT( ans )
  CLASS( Domain_ ), INTENT( IN ) :: obj
  TYPE( String ), ALLOCATABLE :: ans( : )
END FUNCTION getPhysicalPointNames
END INTERFACE

!----------------------------------------------------------------------------
!                                                      getPhysicalCurveNames
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	10 June 2021
! summary: This function returns the names of physical names

INTERFACE
MODULE PURE FUNCTION getPhysicalCurveNames( obj ) RESULT( ans )
  CLASS( Domain_ ), INTENT( IN ) :: obj
  TYPE( String ), ALLOCATABLE :: ans( : )
END FUNCTION getPhysicalCurveNames
END INTERFACE

!----------------------------------------------------------------------------
!                                                    getPhysicalSurfaceNames
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	10 June 2021
! summary: This function returns the names of physical surface

INTERFACE
MODULE PURE FUNCTION getPhysicalSurfaceNames( obj ) RESULT( ans )
  CLASS( Domain_ ), INTENT( IN ) :: obj
  TYPE( String ), ALLOCATABLE :: ans( : )
END FUNCTION getPhysicalSurfaceNames
END INTERFACE

!----------------------------------------------------------------------------
!                                                     getPhysicalVolumeNames
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	10 June 2021
! summary: This function returns the names of physical volume

INTERFACE
MODULE PURE FUNCTION getPhysicalVolumeNames( obj ) RESULT( ans )
  CLASS( Domain_ ), INTENT( IN ) :: obj
  TYPE( String ), ALLOCATABLE :: ans( : )
END FUNCTION getPhysicalVolumeNames
END INTERFACE

!----------------------------------------------------------------------------
!                                                           getPhysicalTags
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	10 June 2021
! summary: 	This routine returns the physical tags of a physical entities

INTERFACE
MODULE PURE FUNCTION Domain_getPhysicalTags( obj, dim ) RESULT( Ans )
  CLASS( Domain_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: dim
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
END FUNCTION Domain_getPhysicalTags
END INTERFACE

!----------------------------------------------------------------------------
!                                                      getPhysicalPointTags
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	10 June 2021
! summary: 	 This function returns the physical tags of all physical points

INTERFACE
MODULE PURE FUNCTION getPhysicalPointTags( obj ) RESULT( ans )
  CLASS( Domain_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
END FUNCTION getPhysicalPointTags
END INTERFACE

!----------------------------------------------------------------------------
!                                                      getPhysicalCurveTags
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	10 June 2021
! summary: 	 This function returns the physical tags of all physical curves

INTERFACE
MODULE PURE FUNCTION getPhysicalCurveTags( obj ) RESULT( ans )
  CLASS( Domain_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
END FUNCTION getPhysicalCurveTags
END INTERFACE

!----------------------------------------------------------------------------
!                                                    getPhysicalSurfaceTags
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	10 June 2021
! summary: 	 This function returns the physical tags of all physical surfaces

INTERFACE
MODULE PURE FUNCTION getPhysicalSurfaceTags( obj ) RESULT( ans )
  CLASS( Domain_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
END FUNCTION getPhysicalSurfaceTags
END INTERFACE

!----------------------------------------------------------------------------
!                                                     getPhysicalVolumeTags
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	10 June 2021
! summary: 	 This function returns the physical tags of all physical volumes

INTERFACE
MODULE PURE FUNCTION getPhysicalVolumeTags( obj ) RESULT( ans )
  CLASS( Domain_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
END FUNCTION getPhysicalVolumeTags
END INTERFACE

!----------------------------------------------------------------------------
!                                                                    WhoAmI
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Domain_WhoAmI( obj, I ) RESULT( ans )
  CLASS( Domain_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: I
  TYPE( String ) :: ans
END FUNCTION Domain_WhoAmI
END INTERFACE

!----------------------------------------------------------------------------
!                                                           AppendEntities
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE Domain_AppendEntities( obj, indx, Entitytag )
  CLASS( Domain_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: EntityTag( : ), indx
END SUBROUTINE Domain_AppendEntities
END INTERFACE

!----------------------------------------------------------------------------
!                                                            IncNumElements
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Domain_IncNumElements( obj, indx, incr )
  CLASS( Domain_  ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: indx
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: incr
END SUBROUTINE Domain_IncNumElements
END INTERFACE

!----------------------------------------------------------------------------
!                                                                getEntities
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Domain_getEntities( obj, indx ) RESULT( Ans )
  CLASS( Domain_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: indx
  INTEGER( I4B ), ALLOCATABLE :: Ans( : )
END FUNCTION Domain_getEntities
END INTERFACE

!----------------------------------------------------------------------------
!                                                            IncNumNodes
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Domain_IncNumNodes( obj, indx, incr )
  CLASS( Domain_  ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: indx
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: incr
END SUBROUTINE Domain_IncNumNodes
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 getNSD
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Domain_getNSD( obj ) RESULT( Ans )
  CLASS( Domain_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
END FUNCTION Domain_getNSD
END INTERFACE

!----------------------------------------------------------------------------
!                                                            getNumElements
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Domain_getNumElements( obj ) RESULT( Ans )
  CLASS( Domain_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
END FUNCTION Domain_getNumElements
END INTERFACE

!----------------------------------------------------------------------------
!                                                               getNumNodes
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION Domain_getNumNodes( obj ) RESULT( Ans )
  CLASS( Domain_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
END FUNCTION Domain_getNumNodes
END INTERFACE

!----------------------------------------------------------------------------
!                                                               setNumNodes
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE SUBROUTINE Domain_setNumNodes( obj, indx, numNode )
  CLASS( Domain_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: indx
  INTEGER( I4B ), INTENT( IN ) :: numNode
END SUBROUTINE Domain_setNumNodes
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END MODULE Domain_Class