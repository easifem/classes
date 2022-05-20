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
! update:
!   - 12 Nov 2021
! summary: This module contains methods for domain data type

MODULE Domain_Class
USE BaseType
USE String_Class
USE GlobalData
USE Mesh_Class
USE MeshPointerVector_Class
USE ElementFactory
USE ExceptionHandler_Class
USE HDF5File_Class
IMPLICIT NONE
PRIVATE
CHARACTER(LEN=*), PARAMETER :: modName = "Domain_Class"
TYPE(ExceptionHandler_) :: e

!----------------------------------------------------------------------------
!                                                             MeshFacetData_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 May 2022
! summary: Data storage for mesh-facets
!
!# Introduction
!
! Mesh facet elements are located on mesh boundary which is connected to
! other mesh region.
! In this way, the slaveCell of a meshFacet is inside some other mesh.
! The information of slaveCell number will be accessed through the
! Halo of the mesh.
! The halo of the mesh will be stored inside the instance of Mesh_
!
! For each Halo (neighbouring mesh) we have an instance of MeshFacetData_.
! therefore, I have defined MeshFacetData_ as the collection of
! all meshfacets.

TYPE MeshFacetData_
  INTEGER( I4B ) :: masterMesh = 0
  INTEGER( I4B ) :: slaveMesh = 0
  INTEGER( I4B ), ALLOCATABLE :: masterCellNumber( : )
  INTEGER( I4B ), ALLOCATABLE :: slaveCellNumber( : )
  INTEGER( I4B ), ALLOCATABLE :: masterLocalFacetID( : )
  INTEGER( I4B ), ALLOCATABLE :: slaveLocalFacetID( : )
  ! CLASS( Halo_ ), POINTER :: halo => NULL()
  CONTAINS
  PROCEDURE, PUBLIC, PASS( obj ) :: Display => MeshFacetData_Display
  PROCEDURE, PUBLIC, PASS( obj ) :: Initiate => MeshFacetData_Initiate
  ! PROCEDURE, PUBLIC, PASS( obj ) :: Set => MeshFacet_Set
  ! PROCEDURE, PUBLIC, PASS( obj ) :: Size => MeshFacet_Size
  ! PROCEDURE, PUBLIC, PASS( obj ) :: SetSlaveCellNumber => &
  !   & MeshFacet_SetSlaveCellNumber
  ! PROCEDURE, PUBLIC, PASS( obj ) :: SetSlaveLocalFacetID => &
  !   & MeshFacet_SetSlaveLocalFacetID
  ! PROCEDURE, PUBLIC, PASS( obj ) :: SetSlaveData => &
  !   & MeshFacet_SetSlaveData
  ! !!
END TYPE MeshFacetData_

!----------------------------------------------------------------------------
!                                                                   Domain_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 June 2021
! summary: Domain_ contains finite element mesh data of a domain
!
!{!pages/Domain_.md!}

TYPE :: Domain_
  PRIVATE
  LOGICAL(LGT), PUBLIC :: isInitiated = .FALSE.
    !! flag
  TYPE(String) :: engine
    !! Engine used for generating the meshes
  INTEGER(I4B) :: majorVersion = 0
    !! Major version
  INTEGER(I4B) :: minorVersion = 0
    !! Minor version
  REAL(DFP) :: version = 0.0_DFP
    !! Version  MajorVersion.MinorVersion
  INTEGER(I4B) :: nsd = 0_I4B
    !! number of spatial dimension
  INTEGER(I4B), PUBLIC :: maxNptrs = 0
    !! Largest node number in the domain
  INTEGER(I4B), PUBLIC :: minNptrs = 0
    !! Smallest node number in the domain
  INTEGER(I4B) :: tNodes = 0
    !! Total number of nodes in the mesh
  LOGICAL(I4B) :: isNodeNumberSparse = .FALSE.
    !! True if node numbers are not continuous
  INTEGER(I4B), PUBLIC :: maxElemNum = 0
    !! Largest element number in the domain
  INTEGER(I4B), PUBLIC :: minElemNum = 0
    !! Smallest element number in the domain
  LOGICAL(LGT) :: isElemNumberSparse = .FALSE.
    !! True if element numbers are sparse
  INTEGER(I4B) :: tEntitiesForNodes = 0
    !! Total number of entities required for reading nodes
  INTEGER(I4B) :: tEntitiesForElements = 0
    !! Total number of entities required for reading elements
  INTEGER(I4B) :: tElements(0:3) = [0, 0, 0, 0]
    !! Total number of elements inside the domain
    !! tElements( 0 ) = total number of point elements
    !! tElements( 1 ) = total number of line elements
    !! tElements( 2 ) =  total number of surface elements
    !! tElements( 3 ) = total number of volume/cell elements
  INTEGER(I4B) :: tEntities(0:3) = [0, 0, 0, 0]
    !! Total number of entities inside the domain
    !! tEntities( 0 ) = total number of point mesh entities, mesh of Points
    !! tEntities( 1 ) = total number of line mesh entities, mesh of Edge
    !! tEntities( 2 ) = total number of surface mesh entities, mesh Boundary
    !! tEntities( 3 ) = total number of volume mesh entities, Omega
  REAL(DFP), ALLOCATABLE, PUBLIC :: nodeCoord(:, :)
    !! Nodal coordinates in XiJ format
    !! Number of rows are 3, and number of columns is total nodes
  INTEGER(I4B), ALLOCATABLE, PUBLIC :: local_nptrs(:)
    !! local_nptrs are required to access the nodeCoord
  TYPE(MeshPointerVector_), ALLOCATABLE :: meshList(:)
    !! meshList( 0 ) list of meshes of point entities
    !! meshList( 1 ) list of meshes of line entities
    !! meshList( 2 ) list of meshes of surface entities
    !! meshList( 3 ) list of meshes of volume entities
  TYPE(MeshFacetData_), ALLOCATABLE :: meshFacetData( : )
  TYPE(CSRSparsity_) :: meshMap
CONTAINS
  PRIVATE
  ! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: addSurrogate => Domain_addSurrogate
      !! Add surrogate to the module error handler
  PROCEDURE, PUBLIC, PASS(Obj) :: Initiate => Domain_Initiate
      !! Initiate an instance of domain
  PROCEDURE, PUBLIC, PASS(Obj) :: DEALLOCATE => Domain_Deallocate
      !! Deallocate data stored inside an instance of domain
      !! TODO Rename Deallocate to Deallocate
  FINAL :: Domain_Final
      !! Finalizer for domain
  ! @IOMethods
  PROCEDURE, PASS(Obj) :: IMPORT => Domain_Import
      !! Initiates an instance of domain by importing data from meshfile
      !! TODO Add an export method to [[Domain_]] class
  PROCEDURE, PUBLIC, PASS( obj ) :: Display => Domain_Display
    !! TODO Add a display method to [[Domain_]] class
  PROCEDURE, PUBLIC, PASS( obj ) :: DisplayMeshFacetData => &
    & Domain_DisplayMeshFacetData
    !! Display mesh facet data
  ! @getMethods
  PROCEDURE, PUBLIC, PASS(obj) :: &
    & isNodePresent => &
    & Domain_isNodePresent
  PROCEDURE, PUBLIC, PASS(obj) :: &
    & isElementPresent => &
    & Domain_isElementPresent
  PROCEDURE, PUBLIC, PASS(obj) :: &
    & getConnectivity => &
    & Domain_getConnectivity
  PROCEDURE, PASS(obj) :: Domain_getNodeToElements1
  PROCEDURE, PASS(obj) :: Domain_getNodeToElements2
  GENERIC, PUBLIC :: getNodeToElements => &
    & Domain_getNodeToElements1, &
    & Domain_getNodeToElements2
  PROCEDURE, PUBLIC, PASS(obj) :: getTotalNodes => Domain_getTotalNodes
    !! returns the total number of nodes in the domain, mesh, or part of mesh
  PROCEDURE, PASS(obj) :: Domain_tNodes1
    !! Returns the total nodes in domain
  PROCEDURE, PASS(obj) :: Domain_tNodes2
    !! Returns the total nodes in a dimension
  PROCEDURE, PASS(obj) :: Domain_tNodes3
    !! REturns the total nodes in a given mesh
  GENERIC, PUBLIC :: OPERATOR(.tNodes.) => &
    & Domain_tNodes1,  &
    & Domain_tNodes2,  &
    & Domain_tNodes3
  PROCEDURE, PUBLIC, PASS(obj) :: getTotalElements => Domain_getTotalElements
      !! returns the total number of Elements in domain, mesh, or part of mesh
  PROCEDURE, PASS(obj) :: &
    & Domain_tElements1,  &
    & Domain_tElements2,  &
    & Domain_tElements3
      !! returns total number of elements in domain, mesh, or part of domain
  GENERIC, PUBLIC :: OPERATOR(.tElements.) => &
    & Domain_tElements1,  &
    & Domain_tElements2,  &
    & Domain_tElements3
    !! return total number of elements in domain, mesh, or part of domain
  PROCEDURE, PASS(obj) :: Domain_getLocalNodeNumber1
  PROCEDURE, PASS(obj) :: Domain_getLocalNodeNumber2
  GENERIC, PUBLIC :: &
    & getLocalNodeNumber => &
    & Domain_getLocalNodeNumber1, &
    & Domain_getLocalNodeNumber2
  PROCEDURE, PASS(obj) :: domain_getGlobalNodeNumber1
      !! Returns the global node number of a local node number
  PROCEDURE, PASS(obj) :: domain_getGlobalNodeNumber2
      !! Returns the global node number of a local node number
  GENERIC, PUBLIC :: getGlobalNodeNumber => &
    & domain_getGlobalNodeNumber1, &
    & domain_getGlobalNodeNumber2
  PROCEDURE, PUBLIC, PASS(obj) :: getTotalMesh => Domain_getTotalMesh
      !! This routine returns total number of meshes of given dimension
  PROCEDURE, PASS(obj) :: Domain_getMeshPointer1
  PROCEDURE, PASS(obj) :: Domain_getMeshPointer2
  GENERIC, PUBLIC :: getMeshPointer => &
    & Domain_getMeshPointer1, &
    & Domain_getMeshPointer2
  !! This routine a pointer to [[Mesh_]] object
  PROCEDURE, PUBLIC, PASS(obj) :: getDimEntityNum => Domain_getDimEntityNum
  !! Returns a dim and entity number of mesh which contains the element number
  PROCEDURE, PUBLIC, PASS(obj) :: getNodeCoord => Domain_getNodeCoord
  !! This routine returns the nodal coordinate in rank2 array
  PROCEDURE, PUBLIC, PASS(obj) :: getNodeCoordPointer => &
    & Domain_getNodeCoordPointer
  !! This routine returns the pointer to nodal coordinate
  PROCEDURE, PUBLIC, PASS(obj) :: getGlobalToLocalNodeNumPointer => &
    & Domain_getGlobalToLocalNodeNumPointer
  PROCEDURE, PUBLIC, PASS(obj) :: getNptrs => &
    & Domain_getNptrs
  PROCEDURE, PUBLIC, PASS(obj) :: getInternalNptrs => &
    & Domain_getInternalNptrs
  PROCEDURE, PUBLIC, PASS(obj) :: getBoundingBox => Domain_getBoundingBox
  !! returns bounding box
  PROCEDURE, PUBLIC, PASS(Obj) :: getNSD => Domain_getNSD
  !! Returns the spatial dimension of each physical entities
  PROCEDURE, PUBLIC, PASS( obj ) :: getOrder => Domain_getOrder
  !! Get Order
  ! @setMethods
  PROCEDURE, PASS(obj) :: setSparsity1 => Domain_setSparsity1
  PROCEDURE, NOPASS :: setSparsity2 => Domain_setSparsity2
  GENERIC, PUBLIC :: setSparsity => setSparsity1, setSparsity2
  PROCEDURE, PUBLIC, PASS(obj) :: setTotalMaterial => Domain_setTotalMaterial
  !! set the total number of materials
  PROCEDURE, PUBLIC, PASS(obj) :: setMaterial => Domain_setMaterial
  !! set the material
  PROCEDURE, PUBLIC, PASS( obj ) :: setDomainFacetElement => &
    & Domain_setDomainFacetElement
  !! Set facet element of meshes
  PROCEDURE, PUBLIC, PASS( obj ) :: setFacetElementType => &
    & Domain_setFacetElementType
  !! Set facet element of meshes
  PROCEDURE, PUBLIC, PASS( obj ) :: setMeshmap => &
    & Domain_setMeshmap
  PROCEDURE, PUBLIC, PASS( obj ) :: setMeshFacetElement => &
    & Domain_setMeshFacetElement
  !! @ShapedataMethods
  PROCEDURE, PASS(obj) :: initiateElemSD1 => Domain_initiateElemSD1
  PROCEDURE, PASS(obj) :: initiateElemSD2 => Domain_initiateElemSD2
  PROCEDURE, PASS(obj) :: initiateElemSD3 => Domain_initiateElemSD3
  PROCEDURE, PASS(obj) :: initiateElemSD4 => Domain_initiateElemSD4
  GENERIC, PUBLIC :: initiateElemSD => &
    & initiateElemSD1, &
    & initiateElemSD2, &
    & initiateElemSD3, &
    & initiateElemSD4
  !! Initiating local shape data for mesh
END TYPE Domain_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PUBLIC :: Domain_

!----------------------------------------------------------------------------
!                                                             DomainPointer
!----------------------------------------------------------------------------

TYPE :: DomainPointer_
  CLASS(Domain_), POINTER :: ptr => NULL()
END TYPE DomainPointer_

PUBLIC :: DomainPointer_

!----------------------------------------------------------------------------
!                                            addSurrogate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 1 Sept 2021
! summary: This routine addes surrogate to module exception handler

INTERFACE
  MODULE SUBROUTINE Domain_addSurrogate(obj, userObj)
    CLASS(Domain_), INTENT(INOUT) :: obj
    TYPE(ExceptionHandler_), INTENT(IN) :: userObj
  END SUBROUTINE Domain_addSurrogate
END INTERFACE

!----------------------------------------------------------------------------
!                                            addSurrogate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 1 Sept 2021
! summary: This routine addes surrogate to module exception handler

INTERFACE
  MODULE SUBROUTINE addSurrogate_Domain(userObj)
    TYPE(ExceptionHandler_), INTENT(IN) :: userObj
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
  MODULE SUBROUTINE Domain_Initiate(obj, hdf5, group)
    CLASS(Domain_), INTENT(INOUT) :: obj
    !! DomainData object
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! HDF5 file
    CHARACTER(LEN=*), INTENT(IN) :: group
    !! Group name (directory name)
  END SUBROUTINE Domain_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initaite@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 May 2022
! summary: Initiate an instance of MeshFacetData

INTERFACE
MODULE PURE SUBROUTINE MeshFacetData_Initiate( obj, n )
  CLASS( MeshFacetData_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: n
END SUBROUTINE MeshFacetData_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                          Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 June 2021
! summary: Deallocate data stored in Domain object

INTERFACE
  MODULE SUBROUTINE Domain_Deallocate(obj)
    CLASS(Domain_), INTENT(INOUT) :: obj
    !! Domain object
  END SUBROUTINE Domain_Deallocate
END INTERFACE

INTERFACE DEALLOCATE
  MODULE PROCEDURE Domain_Deallocate
END INTERFACE DEALLOCATE

PUBLIC :: DEALLOCATE

!----------------------------------------------------------------------------
!                                                   Final@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 June 2021
! summary: Finalizer

INTERFACE
  MODULE SUBROUTINE Domain_Final(obj)
    TYPE(Domain_), INTENT(INOUT) :: obj
  END SUBROUTINE Domain_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                  Domain@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 June 2021
! summary: Domain methods

INTERFACE
  MODULE FUNCTION Domain_Constructor1(hdf5, group) RESULT(Ans)
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(LEN=*), INTENT(IN) :: group
    TYPE(Domain_) :: ans
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
  MODULE FUNCTION Domain_Constructor_1(hdf5, group) RESULT(Ans)
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(LEN=*), INTENT(IN) :: group
    CLASS(Domain_), POINTER :: ans
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
! date: 18 June 2021
! summary: Construct an instance of domain by importing data from mesh

INTERFACE
  MODULE SUBROUTINE Domain_Import(obj, hdf5, group)
    CLASS(Domain_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(LEN=*), INTENT(IN) :: group
  END SUBROUTINE Domain_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 May 2022
! summary: Display the domain

INTERFACE
MODULE SUBROUTINE Domain_Display( obj, msg, unitno )
  CLASS( Domain_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitno
END SUBROUTINE Domain_Display
END INTERFACE


!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 May 2022
! summary: Display mesh facet data

INTERFACE
MODULE SUBROUTINE Domain_DisplayMeshFacetData( obj, msg, unitno )
  CLASS( Domain_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitno
END SUBROUTINE Domain_DisplayMeshFacetData
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 May 2022
! summary: Display mesh facet data

INTERFACE
MODULE SUBROUTINE MeshFacetData_Display( obj, msg, unitno )
  CLASS( MeshFacetData_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitno
END SUBROUTINE MeshFacetData_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                   isNodePresent@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Sept 2021
! summary: Returns true if the global node number is present

INTERFACE
  MODULE FUNCTION Domain_isNodePresent(obj, globalNode) RESULT(Ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    LOGICAL(LGT) :: ans
  END FUNCTION Domain_isNodePresent
END INTERFACE

!----------------------------------------------------------------------------
!                                               isElementPresent@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-12
! update: 2021-11-12
! summary: Returns true if the element number is present inside the domain

INTERFACE
  MODULE FUNCTION Domain_isElementPresent(obj, globalElement) RESULT(Ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    LOGICAL(LGT) :: ans
  END FUNCTION Domain_isElementPresent
END INTERFACE

!----------------------------------------------------------------------------
!                                                 getConnectivity@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-12
! update: 2021-11-12
! summary: Returns the connectivity vector of a given element number

INTERFACE
  MODULE FUNCTION Domain_getConnectivity(obj, globalElement) RESULT(Ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION Domain_getConnectivity
END INTERFACE

!----------------------------------------------------------------------------
!                                               getNodeToElements@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-12
! update: 2021-11-12
! summary: returns the elements connected to a node

INTERFACE
  MODULE FUNCTION Domain_getNodeToElements1(obj, globalNode) RESULT(Ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION Domain_getNodeToElements1
END INTERFACE

!----------------------------------------------------------------------------
!                                               getNodeToElements@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-12
! update: 2021-11-12
! summary: returns the elements connected to a node

INTERFACE
  MODULE FUNCTION Domain_getNodeToElements2(obj, globalNode) RESULT(Ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION Domain_getNodeToElements2
END INTERFACE

!----------------------------------------------------------------------------
!                                                 getTotalNodes@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 June 2021
! summary: Returns the total number of nodes in the domain
!
!# Introduction
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
  MODULE FUNCTION Domain_getTotalNodes(obj, dim, entityNum) RESULT(Ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: entityNum
    INTEGER(I4B) :: ans
  END FUNCTION Domain_getTotalNodes
END INTERFACE

!----------------------------------------------------------------------------
!                                                         tNodes@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 June 2021
! summary: Returns the total number of nodes in the domain
!
!# Introduction
!
! This function returns the total number of nodes in a given mesh entity
! The mesh entity is given by its ID and its dimension.
! Here, opt = [dim, entityNum]
!
! This function is used for defining an operator [[.tNodes.]]
!
!
! - `dim=0` denotes mesh of point entities
! - `dim=1` denotes mesh of curve entities
! - `dim=2` denotes mesh of surface entities
! - `dim=3` denotes mesh of volume entities
! - `entityNum` should not be out of bound

INTERFACE
  MODULE FUNCTION Domain_tNodes1(obj, opt) RESULT(Ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: opt(2)
    INTEGER(I4B) :: ans
  END FUNCTION Domain_tNodes1
END INTERFACE

!----------------------------------------------------------------------------
!                                                         tNodes@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 June 2021
! summary: Returns the total number of nodes in the domain
!
!# Introduction
!
! This function returns the total number of nodes in a given mesh entity
! The mesh entity is given by its ID and its dimension.
!
! This function is used for defining an operator
! [[OPERATOR(.tNodes.)]]
!
!
! - `dim=0` denotes mesh of point entities
! - `dim=1` denotes mesh of curve entities
! - `dim=2` denotes mesh of surface entities
! - `dim=3` denotes mesh of volume entities

INTERFACE
  MODULE FUNCTION Domain_tNodes2(obj, dim) RESULT(Ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B) :: ans
  END FUNCTION Domain_tNodes2
END INTERFACE

!----------------------------------------------------------------------------
!                                                          tNodes@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 June 2021
! summary: Returns the total number of nodes in the domain

INTERFACE
  MODULE FUNCTION Domain_tNodes3(obj) RESULT(Ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION Domain_tNodes3
END INTERFACE

!----------------------------------------------------------------------------
!                                                getTotalElements@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 June 2021
! summary: Returns the total number of elements in the domain
!
!# Introduction
!
! This function returns the total number of elements in
!
! - entire Domain
! - selected region of domain
! - The mesh selection can be made by specifying the `dim` and `entityNum`
!
!@note
! - `dim=0` denotes mesh of point entities
! - `dim=1` denotes mesh of curve entities
! - `dim=2` denotes mesh of surface entities
! - `dim=3` denotes mesh of volume entities
!@endnote
!
!@warn
! `entityNum` should not be out of bound
!@endwarn

INTERFACE
  MODULE FUNCTION Domain_getTotalElements(obj, dim, entityNum) RESULT(Ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: entityNum
    INTEGER(I4B) :: ans
  END FUNCTION Domain_getTotalElements
END INTERFACE

!----------------------------------------------------------------------------
!                                                      tElements@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-13
! update: 2021-11-13
! summary: Returns total elements in domain

INTERFACE
  MODULE FUNCTION Domain_tElements1(obj) RESULT(Ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION Domain_tElements1
END INTERFACE

!----------------------------------------------------------------------------
!                                                      tElements@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-13
! update: 2021-11-13
! summary: Returns total elements in given dimension

INTERFACE
  MODULE FUNCTION Domain_tElements2(obj, dim) RESULT(Ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B) :: ans
  END FUNCTION Domain_tElements2
END INTERFACE

!----------------------------------------------------------------------------
!                                                      tElements@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-13
! update: 2021-11-13
! summary: Returns the total elements in a given mesh

INTERFACE
  MODULE FUNCTION Domain_tElements3(obj, opt) RESULT(Ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: opt(2)
    INTEGER(I4B) :: ans
  END FUNCTION Domain_tElements3
END INTERFACE

!----------------------------------------------------------------------------
!                                             getLocalNodeNumber@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Sept 2021
! summary: Returns local node number of a global node number

INTERFACE
  MODULE FUNCTION Domain_getLocalNodeNumber1(obj, globalNode) RESULT(Ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B) :: ans
  END FUNCTION Domain_getLocalNodeNumber1
END INTERFACE

!----------------------------------------------------------------------------
!                                              getLocalNodeNumber@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Sept 2021
! summary: Returns local node number of a global node number

INTERFACE
  MODULE FUNCTION Domain_getLocalNodeNumber2(obj, globalNode) RESULT(Ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B) :: ans(SIZE(globalNode))
  END FUNCTION Domain_getLocalNodeNumber2
END INTERFACE

!----------------------------------------------------------------------------
!                                             getGlobalNodeNumber@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Sept 2021
! summary: Returns local node number of a global node number

INTERFACE
  MODULE FUNCTION Domain_getGlobalNodeNumber1(obj, localNode) RESULT(Ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: localNode
    INTEGER(I4B) :: ans
  END FUNCTION Domain_getGlobalNodeNumber1
END INTERFACE

!----------------------------------------------------------------------------
!                                              getGlobalNodeNumber@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Sept 2021
! summary: Returns local node number of a global node number

INTERFACE
  MODULE FUNCTION Domain_getGlobalNodeNumber2(obj, localNode) RESULT(Ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: localNode(:)
    INTEGER(I4B) :: ans(SIZE(localNode))
  END FUNCTION Domain_getGlobalNodeNumber2
END INTERFACE

!----------------------------------------------------------------------------
!                                                    getTotalMesh@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Sept 2021
! summary: This function returns the total number of mesh
!
!# Introduction
!
! This function returns the total number of mesh
!
! - `dim=0` returns the total number of mesh of point entities
! - `dim=1` returns the total number of mesh of curve entities
! - `dim=2` returns the total number of mesh of surface entities
! - `dim=3` returns the total number of mesh of volume entities

INTERFACE
  MODULE FUNCTION Domain_getTotalMesh(obj, dim) RESULT(Ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B) :: ans
  END FUNCTION Domain_getTotalMesh
END INTERFACE

!----------------------------------------------------------------------------
!                                                  getMeshPointer@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This rotuine returns mesh pointer
!
!# Introduction
!
! This returns the mesh Entity pointer.
! - dim is the dimension of the mesh; dim=0,1,2,3 corresponds to the point,
! curve, surface, volume meshes.
! - tag, is the number of mesh

INTERFACE
  MODULE FUNCTION Domain_getMeshPointer1(obj, dim, entityNum) RESULT(Ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), INTENT(IN) :: entityNum
    CLASS(Mesh_), POINTER :: ans
  END FUNCTION Domain_getMeshPointer1
END INTERFACE

!----------------------------------------------------------------------------
!                                                  getMeshPointer@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-12
! update: 2021-11-12
! summary: Returns pointer to the mesh
!
!# Introduction
!
! This function returns the pointer to the mesh which contains the global
! element number

INTERFACE
  MODULE FUNCTION Domain_getMeshPointer2(obj, globalElement) RESULT(Ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    CLASS(Mesh_), POINTER :: ans
  END FUNCTION Domain_getMeshPointer2
END INTERFACE

!----------------------------------------------------------------------------
!                                                 getDimEntityNum@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-12
! update: 2021-11-12
! summary: Returns dim and entity number

INTERFACE
  MODULE FUNCTION Domain_getDimEntityNum(obj, globalElement) RESULT(ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    INTEGER(I4B) :: ans(2)
  END FUNCTION Domain_getDimEntityNum
END INTERFACE

!----------------------------------------------------------------------------
!                                                     getNodeCoord@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine returns the nodal coordinates
!
!# Introduction
! - This routine returns the nodal coordinates in the form of rank2 array.
! - The nodal coordinates are in XiJ, the columns of XiJ denotes the node
! number, and the rows correspond to the component.
! - If `dim` and `tag` are absent then this routine returns the nodal
! coordinates of the entire domain
! - If `dim` and `tag` are present then the routine selects the mesh and
! returns its nodal coordinates

INTERFACE
  MODULE SUBROUTINE Domain_getNodeCoord(obj, nodeCoord, dim, entityNum)
    CLASS(Domain_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: nodeCoord(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: entityNum
  END SUBROUTINE Domain_getNodeCoord
END INTERFACE

!----------------------------------------------------------------------------
!                                             getNodeCoordPointer@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine returns the pointer to nodal coordinates
!
!# Introduction
! - This routine returns the pointer to nodal coordinates in the form of
! rank2 array.
! - The nodal coordinates are in XiJ, the columns of XiJ denotes the node
! number, and the rows correspond to the component.

INTERFACE
  MODULE FUNCTION Domain_getNodeCoordPointer(obj) RESULT(ans)
    CLASS(Domain_), TARGET, INTENT(IN) :: obj
    REAL(DFP), POINTER :: ans(:, :)
  END FUNCTION Domain_getNodeCoordPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                  getGlobalToLocalNodeNumPointer@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine returns the pointer to nodal coordinates
!
!# Introduction
! - This routine returns the pointer to nodal coordinates in the form of
! rank2 array.
! - The nodal coordinates are in XiJ, the columns of XiJ denotes the node
! number, and the rows correspond to the component.

INTERFACE
  MODULE FUNCTION Domain_getGlobalToLocalNodeNumPointer(obj) RESULT(ans)
    CLASS(Domain_), TARGET, INTENT(IN) :: obj
    INTEGER(I4B), POINTER :: ans(:)
  END FUNCTION Domain_getGlobalToLocalNodeNumPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                                         getNptrs@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2 Sept 2021
! summary: this routine returns the global node number
!
!# Introduction
! This routine returns the global node number
! xidim is the dimension of the mesh

INTERFACE
  MODULE FUNCTION Domain_getNptrs(obj, entityNum, dim) RESULT(Ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: entityNum(:)
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION Domain_getNptrs
END INTERFACE

!----------------------------------------------------------------------------
!                                                         getNptrs@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2 Sept 2021
! summary: this routine returns the global node number
!
!# Introduction
! This routine returns the global node number
! xidim is the dimension of the mesh

INTERFACE
  MODULE FUNCTION Domain_getInternalNptrs(obj, entityNum, dim) RESULT(Ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: entityNum(:)
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION Domain_getInternalNptrs
END INTERFACE

!----------------------------------------------------------------------------
!                                                           getNSD@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Sept 2021
! summary: This routine returns the number of spatial dimensions

INTERFACE
  MODULE PURE FUNCTION Domain_getNSD(obj) RESULT(Ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION Domain_getNSD
END INTERFACE

!----------------------------------------------------------------------------
!                                                           getNSD@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Sept 2021
! summary: This routine returns the order of meshes of dimensions=dim

INTERFACE
  MODULE FUNCTION Domain_getOrder(obj, dim) RESULT(Ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER( I4B ), INTENT( IN ) :: dim
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION Domain_getOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                                  getBoundingBox@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 13 Oct 2021
! summary: Returns bounding box

INTERFACE
  MODULE PURE FUNCTION Domain_getBoundingBox(obj) RESULT(Ans)
    CLASS(Domain_), INTENT(IN) :: obj
    TYPE(BoundingBox_) :: ans
  END FUNCTION Domain_getBoundingBox
END INTERFACE

!----------------------------------------------------------------------------
!                                                     setSparsity@setMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Oct 2021
! summary: Set sparsity in [[CSRMatrix_]] from [[Domain_]]

INTERFACE
  MODULE SUBROUTINE Domain_setSparsity1(obj, mat)
    CLASS(Domain_), INTENT(IN) :: obj
    TYPE(CSRMatrix_), INTENT(INOUT) :: mat
  END SUBROUTINE Domain_setSparsity1
END INTERFACE

!----------------------------------------------------------------------------
!                                                     setSparsity@setMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Oct 2021
! summary: Set sparsity in [[CSRMatrix_]] from [[Domain_]]

INTERFACE
  MODULE SUBROUTINE Domain_setSparsity2(domains, mat)
    CLASS(DomainPointer_), INTENT(IN) :: domains(:)
    TYPE(CSRMatrix_), INTENT(INOUT) :: mat
  END SUBROUTINE Domain_setSparsity2
END INTERFACE

!----------------------------------------------------------------------------
!                                                     setMaterial@setMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-12-09
! update: 2021-12-09
! summary: Set the materials id of a given medium

INTERFACE
  MODULE SUBROUTINE Domain_setTotalMaterial(obj, dim, n)
    CLASS(Domain_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), INTENT(IN) :: n
  END SUBROUTINE Domain_setTotalMaterial
END INTERFACE

!----------------------------------------------------------------------------
!                                                     setMaterial@setMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-12-09
! update: 2021-12-09
! summary: Set the materials id of a given medium

INTERFACE
  MODULE SUBROUTINE Domain_setMaterial(obj, dim, entityNum, &
    & medium, material)
    CLASS(Domain_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), INTENT(IN) :: entityNum
    INTEGER(I4B), INTENT(IN) :: medium
    INTEGER(I4B), INTENT(IN) :: material
  END SUBROUTINE Domain_setMaterial
END INTERFACE

!----------------------------------------------------------------------------
!                                        setDomainBoundaryElement@setMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 April 2022
! summary: This routine sets the domain boundary element for cells and faces

INTERFACE
MODULE SUBROUTINE Domain_setFacetElementType( obj )
  CLASS( Domain_ ), INTENT( INOUT ) :: obj
END SUBROUTINE Domain_setFacetElementType
END INTERFACE

!----------------------------------------------------------------------------
!                                        setDomainBoundaryElement@setMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 April 2022
! summary: This routine sets the domain boundary element for cells and faces

INTERFACE
MODULE SUBROUTINE Domain_setDomainFacetElement( obj )
  CLASS( Domain_ ), INTENT( INOUT ) :: obj
END SUBROUTINE Domain_setDomainFacetElement
END INTERFACE

!----------------------------------------------------------------------------
!                                                      setMeshmap@setMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 May 2022
! summary: This routine sets meshMap

INTERFACE
MODULE SUBROUTINE Domain_setMeshmap( obj )
  CLASS( Domain_ ), INTENT( INOUT ) :: obj
END SUBROUTINE Domain_setMeshmap
END INTERFACE

!----------------------------------------------------------------------------
!                                             setMeshFacetElement@setMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 May 2022
! summary: This routine sets meshFacetData

INTERFACE
MODULE SUBROUTINE Domain_setMeshFacetElement( obj )
  CLASS( Domain_ ), INTENT( INOUT ) :: obj
END SUBROUTINE Domain_setMeshFacetElement
END INTERFACE

!----------------------------------------------------------------------------
!                                            InitiateElemSD@ShapeDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-12-09
! update: 2021-12-09
! summary: sets the local shape data for the mesh

INTERFACE
  MODULE SUBROUTINE Domain_initiateElemSD1(obj, &
    & dim, &
    & orderSpace, &
    & quadTypeForSpace, &
    & continuityTypeForSpace, &
    & interpolTypeForSpace)
    CLASS(Domain_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    !! dimension of the mesh
    INTEGER(I4B), INTENT(IN) :: orderSpace(:)
    !! order for each mesh
    !! the size of orderspace is same as obj%getTotalMesh(dim=dim)
    CHARACTER(LEN=*), INTENT(IN) :: quadTypeForSpace
    CHARACTER(LEN=*), INTENT(IN) :: continuityTypeForSpace
    CHARACTER(LEN=*), INTENT(IN) :: interpolTypeForSpace
  END SUBROUTINE Domain_initiateElemSD1
END INTERFACE

!----------------------------------------------------------------------------
!                                            InitiateElemSD@ShapeDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-12-09
! update: 2021-12-09
! summary: sets the local shape data for the mesh

INTERFACE
  MODULE SUBROUTINE Domain_initiateElemSD2(obj, &
    & dim, &
    & orderSpace, &
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
    !!
    CLASS(Domain_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    !! dimension of the mesh
    INTEGER(I4B), INTENT(IN) :: orderSpace(:)
    !! order for each mesh
    !! the size of orderspace is same as obj%getTotalMesh(dim=dim)
    CHARACTER(LEN=*), INTENT(IN) :: quadTypeForSpace
    CHARACTER(LEN=*), INTENT(IN) :: continuityTypeForSpace
    CHARACTER(LEN=*), INTENT(IN) :: interpolTypeForSpace
    INTEGER(I4B), INTENT(IN) :: orderTime
    TYPE(ReferenceLine_), INTENT(IN) :: linTimeElem
    TYPE(ReferenceLine_), INTENT(IN) :: timeElem
    CHARACTER(LEN=*), INTENT(IN) :: quadTypeForTime
    CHARACTER(LEN=*), INTENT(IN) :: continuityTypeForTime
    CHARACTER(LEN=*), INTENT(IN) :: interpolTypeForTime
    REAL(DFP), INTENT(IN) :: tvec(:)
  END SUBROUTINE Domain_initiateElemSD2
END INTERFACE

!----------------------------------------------------------------------------
!                                            InitiateElemSD@ShapeDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-12-09
! update: 2021-12-09
! summary: sets the local shape data for the mesh

INTERFACE
  MODULE SUBROUTINE Domain_initiateElemSD3(obj, &
    & dim, &
    & orderSpace, &
    & quadTypeForSpace, &
    & continuityTypeForSpace, &
    & interpolTypeForSpace, &
    & orderTime, &
    & linTimeElem, &
    & timeElem, &
    & quadTypeForTime, &
    & continuityTypeForTime, &
    & interpolTypeForTime)
    !!
    CLASS(Domain_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    !! dimension of the mesh
    INTEGER(I4B), INTENT(IN) :: orderSpace(:)
    !! order for each mesh
    !! the size of orderspace is same as obj%getTotalMesh(dim=dim)
    CHARACTER(LEN=*), INTENT(IN) :: quadTypeForSpace
    CHARACTER(LEN=*), INTENT(IN) :: continuityTypeForSpace
    CHARACTER(LEN=*), INTENT(IN) :: interpolTypeForSpace
    INTEGER(I4B), INTENT(IN) :: orderTime
    TYPE(ReferenceLine_), INTENT(IN) :: linTimeElem
    TYPE(ReferenceLine_), INTENT(IN) :: timeElem
    CHARACTER(LEN=*), INTENT(IN) :: quadTypeForTime
    CHARACTER(LEN=*), INTENT(IN) :: continuityTypeForTime
    CHARACTER(LEN=*), INTENT(IN) :: interpolTypeForTime
  END SUBROUTINE Domain_initiateElemSD3
END INTERFACE

!----------------------------------------------------------------------------
!                                            InitiateElemSD@ShapeDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-12-09
! update: 2021-12-09
! summary: sets the local shape data for the mesh

INTERFACE
  MODULE SUBROUTINE Domain_initiateElemSD4(obj, dim, tvec)
    CLASS(Domain_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    REAL( DFP ), INTENT( IN ) :: tvec(:)
  END SUBROUTINE Domain_initiateElemSD4
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE Domain_Class
