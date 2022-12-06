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
!   - 4 Nov 2022
! summary: This module contains methods for domain data type

MODULE Domain_Class
USE BaseType
USE String_Class
USE GlobalData
USE Mesh_Class
USE MeshPointerVector_Class
USE ElementFactory
USE ExceptionHandler_Class, ONLY: e
USE HDF5File_Class
IMPLICIT NONE
PRIVATE
CHARACTER(LEN=*), PARAMETER :: modName = "Domain_Class"

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
  INTEGER(I4B) :: masterMesh = 0
  INTEGER(I4B) :: slaveMesh = 0
  INTEGER(I4B), ALLOCATABLE :: masterCellNumber(:)
  INTEGER(I4B), ALLOCATABLE :: slaveCellNumber(:)
  INTEGER(I4B), ALLOCATABLE :: masterLocalFacetID(:)
  INTEGER(I4B), ALLOCATABLE :: slaveLocalFacetID(:)
  ! CLASS( Halo_ ), POINTER :: halo => NULL()
CONTAINS
  PROCEDURE, PUBLIC, PASS(obj) :: Display => MeshFacetData_Display
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => MeshFacetData_Initiate
  PROCEDURE, PUBLIC, PASS(obj) :: isInitiated => MeshFacetData_isInitiated
  PROCEDURE, PUBLIC, PASS(obj) :: Size => MeshFacetData_Size
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
  TYPE(MeshFacetData_), ALLOCATABLE, PUBLIC :: meshFacetData(:)
  TYPE(CSRSparsity_) :: meshMap
CONTAINS
  PRIVATE
  !
  ! @ConstructorMethods
  !
  PROCEDURE, PUBLIC, PASS(Obj) :: Initiate => Domain_Initiate
  !! Initiate an instance of domain
  PROCEDURE, PUBLIC, PASS(Obj) :: DEALLOCATE => Domain_Deallocate
  !! Deallocate data stored inside an instance of domain
  !! TODO Rename Deallocate to Deallocate
  FINAL :: Domain_Final
  !! Finalizer for domain
  !
  ! @IOMethods
  !
  PROCEDURE, PASS(Obj) :: IMPORT => Domain_Import
  !! Initiates an instance of domain by importing data from meshfile
  !! TODO Add an export method to [[Domain_]] class
  PROCEDURE, PUBLIC, PASS(obj) :: Display => Domain_Display
  !! TODO Add a display method to [[Domain_]] class
  PROCEDURE, PUBLIC, PASS(obj) :: DisplayMeshFacetData => &
    & Domain_DisplayMeshFacetData
  !! Display mesh facet data
  !
  ! @getMethods
  !
  PROCEDURE, PUBLIC, PASS(obj) :: &
    & IsNodePresent => &
    & Domain_IsNodePresent
  PROCEDURE, PUBLIC, PASS(obj) :: &
    & IsElementPresent => &
    & Domain_IsElementPresent
  PROCEDURE, PUBLIC, PASS(obj) :: &
    & GetConnectivity => &
    & Domain_GetConnectivity
  PROCEDURE, PASS(obj) :: Domain_GetNodeToElements1
  PROCEDURE, PASS(obj) :: Domain_GetNodeToElements2
  GENERIC, PUBLIC :: GetNodeToElements => &
    & Domain_GetNodeToElements1, &
    & Domain_GetNodeToElements2
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalNodes => Domain_GetTotalNodes
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
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalElements => Domain_GetTotalElements
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
  PROCEDURE, PASS(obj) :: Domain_GetLocalNodeNumber1
  PROCEDURE, PASS(obj) :: Domain_GetLocalNodeNumber2
  GENERIC, PUBLIC :: &
    & GetLocalNodeNumber => &
    & Domain_GetLocalNodeNumber1, &
    & Domain_GetLocalNodeNumber2
  PROCEDURE, PASS(obj) :: Domain_GetGlobalNodeNumber1
  !! Returns the global node number of a local node number
  PROCEDURE, PASS(obj) :: Domain_GetGlobalNodeNumber2
  !! Returns the global node number of a local node number
  GENERIC, PUBLIC :: GetGlobalNodeNumber => &
    & Domain_GetGlobalNodeNumber1, &
    & Domain_GetGlobalNodeNumber2
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalMesh => Domain_GetTotalMesh
  !! This routine returns total number of meshes of given dimension
  PROCEDURE, PASS(obj) :: Domain_GetMeshPointer1
  PROCEDURE, PASS(obj) :: Domain_GetMeshPointer2
  GENERIC, PUBLIC :: GetMeshPointer => &
    & Domain_GetMeshPointer1, &
    & Domain_GetMeshPointer2
  !! This routine a pointer to [[Mesh_]] object
  PROCEDURE, PUBLIC, PASS(obj) :: GetDimEntityNum => Domain_GetDimEntityNum
  !! Returns a dim and entity number of mesh which contains the element number
  PROCEDURE, PUBLIC, PASS(obj) :: GetNodeCoord => Domain_GetNodeCoord
  !! This routine returns the nodal coordinate in rank2 array
  PROCEDURE, PUBLIC, PASS(obj) :: GetNodeCoordPointer => &
    & Domain_GetNodeCoordPointer
  !! This routine returns the pointer to nodal coordinate
  PROCEDURE, PUBLIC, PASS(obj) :: GetGlobalToLocalNodeNumPointer => &
    & Domain_GetGlobalToLocalNodeNumPointer
  PROCEDURE, PUBLIC, PASS(obj) :: GetNptrs => &
    & Domain_GetNptrs
  PROCEDURE, PUBLIC, PASS(obj) :: GetInternalNptrs => &
    & Domain_GetInternalNptrs
  PROCEDURE, PUBLIC, PASS(obj) :: GetBoundingBox => Domain_GetBoundingBox
  !! returns bounding box
  PROCEDURE, PUBLIC, PASS(Obj) :: GetNSD => Domain_GetNSD
  !! Returns the spatial dimension of each physical entities
  PROCEDURE, PUBLIC, PASS(obj) :: GetOrder => Domain_GetOrder
  !! Get Order
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalMeshFacetData => &
    & Domain_GetTotalMeshFacetData
  !
  ! @setMethods
  !
  PROCEDURE, PASS(obj) :: SetSparsity1 => Domain_SetSparsity1
  PROCEDURE, NOPASS :: SetSparsity2 => Domain_SetSparsity2
  GENERIC, PUBLIC :: SetSparsity => SetSparsity1, SetSparsity2
  PROCEDURE, PUBLIC, PASS(obj) :: SetTotalMaterial => Domain_SetTotalMaterial
  !! set the total number of materials
  PROCEDURE, PUBLIC, PASS(obj) :: SetMaterial => Domain_SetMaterial
  !! set the material
  !
  ! @MeshDataMethods
  !
  PROCEDURE, PUBLIC, PASS(obj) :: InitiateNodeToElements => &
    & Domain_InitiateNodeToElements
  !! Initiate node to element data
  PROCEDURE, PUBLIC, PASS(obj) :: InitiateNodeToNodes => &
    & Domain_InitiateNodeToNodes
  !! Initiate node to node data
  PROCEDURE, PUBLIC, PASS(obj) :: InitiateElementToElements => &
      & Domain_InitiateElementToElements
  !! Initiate element to element data
  PROCEDURE, PUBLIC, PASS(obj) :: InitiateBoundaryData => &
      & Domain_InitiateBoundaryData
  !! Initiate element to element data
  PROCEDURE, PUBLIC, PASS(obj) :: InitiateFacetElements => &
      & Domain_InitiateFacetElements
  !! Initiate element to element data
  PROCEDURE, PUBLIC, PASS(obj) :: InitiateExtraNodeToNodes => &
      & Domain_InitiateExtraNodeToNodes
  !! Initiate extra node to nodes information for edge based methods
  PROCEDURE, PUBLIC, PASS(obj) :: SetFacetElementType => &
    & Domain_SetFacetElementType
  !! Set facet element of meshes
  PROCEDURE, PUBLIC, PASS(obj) :: SetDomainFacetElement => &
    & Domain_SetDomainFacetElement
  !! Set facet element of meshes
  PROCEDURE, PUBLIC, PASS(obj) :: SetMeshmap => &
    & Domain_SetMeshmap
  PROCEDURE, PUBLIC, PASS(obj) :: SetMeshFacetElement => &
    & Domain_SetMeshFacetElement
  !
  ! @ShapedataMethods
  !
  PROCEDURE, PASS(obj) :: InitiateElemSD1 => Domain_InitiateElemSD1
  PROCEDURE, PASS(obj) :: InitiateElemSD2 => Domain_InitiateElemSD2
  PROCEDURE, PASS(obj) :: InitiateElemSD3 => Domain_InitiateElemSD3
  PROCEDURE, PASS(obj) :: InitiateElemSD4 => Domain_InitiateElemSD4
  GENERIC, PUBLIC :: InitiateElemSD => &
    & InitiateElemSD1, &
    & InitiateElemSD2, &
    & InitiateElemSD3, &
    & InitiateElemSD4
  !! Initiating local shape data for mesh
  PROCEDURE, PASS(obj) :: InitiateFacetElemSD1 => Domain_InitiateFacetElemSD1
  PROCEDURE, PASS(obj) :: InitiateFacetElemSD2 => Domain_InitiateFacetElemSD2
  PROCEDURE, PASS(obj) :: InitiateFacetElemSD3 => Domain_InitiateFacetElemSD3
  GENERIC, PUBLIC :: InitiateFacetElemSD => &
    & InitiateFacetElemSD1, &
    & InitiateFacetElemSD2, &
    & InitiateFacetElemSD3
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
  MODULE PURE SUBROUTINE MeshFacetData_Initiate(obj, n)
    CLASS(MeshFacetData_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: n
  END SUBROUTINE MeshFacetData_Initiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initaite@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 May 2022
! summary: Returns true if MeshFacetData initiated

INTERFACE
  MODULE PURE FUNCTION MeshFacetData_isInitiated(obj) RESULT(ans)
    CLASS(MeshFacetData_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION MeshFacetData_isInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initaite@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 May 2022
! summary: Returns the size of MeshFacetData

INTERFACE
  MODULE PURE FUNCTION MeshFacetData_Size(obj) RESULT(ans)
    CLASS(MeshFacetData_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION MeshFacetData_Size
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
  MODULE SUBROUTINE Domain_Display(obj, msg, unitno)
    CLASS(Domain_), INTENT(IN) :: obj
    CHARACTER(LEN=*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE Domain_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 May 2022
! summary: Display mesh facet data

INTERFACE
  MODULE SUBROUTINE Domain_DisplayMeshFacetData(obj, msg, unitno)
    CLASS(Domain_), INTENT(IN) :: obj
    CHARACTER(LEN=*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE Domain_DisplayMeshFacetData
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 May 2022
! summary: Display mesh facet data

INTERFACE
  MODULE SUBROUTINE MeshFacetData_Display(obj, msg, unitno)
    CLASS(MeshFacetData_), INTENT(IN) :: obj
    CHARACTER(LEN=*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE MeshFacetData_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                   IsNodePresent@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Sept 2021
! summary: Returns true if the global node number is present

INTERFACE
  MODULE FUNCTION Domain_IsNodePresent(obj, globalNode) RESULT(Ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    LOGICAL(LGT) :: ans
  END FUNCTION Domain_IsNodePresent
END INTERFACE

!----------------------------------------------------------------------------
!                                               IsElementPresent@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-12
! update: 2021-11-12
! summary: Returns true if the element number is present inside the domain

INTERFACE
  MODULE FUNCTION Domain_IsElementPresent(obj, globalElement) RESULT(Ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    LOGICAL(LGT) :: ans
  END FUNCTION Domain_IsElementPresent
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetConnectivity@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-12
! update: 2021-11-12
! summary: Returns the connectivity vector of a given element number

INTERFACE
  MODULE FUNCTION Domain_GetConnectivity(obj, globalElement) RESULT(Ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION Domain_GetConnectivity
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetNodeToElements@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-12
! update: 2021-11-12
! summary: returns the elements connected to a node

INTERFACE
  MODULE FUNCTION Domain_GetNodeToElements1(obj, globalNode) RESULT(Ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION Domain_GetNodeToElements1
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetNodeToElements@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-12
! update: 2021-11-12
! summary: returns the elements connected to a node

INTERFACE
  MODULE FUNCTION Domain_GetNodeToElements2(obj, globalNode) RESULT(Ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION Domain_GetNodeToElements2
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetTotalNodes@getMethods
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
  MODULE FUNCTION Domain_GetTotalNodes(obj, dim, entityNum) RESULT(Ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: entityNum
    INTEGER(I4B) :: ans
  END FUNCTION Domain_GetTotalNodes
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
    INTEGER(I4B), INTENT(IN) :: dim
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
!                                          getTotalMeshFacetData@getMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 May 2022
! summary: returns size of meshFacetData

INTERFACE
  MODULE PURE FUNCTION Domain_getTotalMeshFacetData(obj, imeshFacetData) &
    & RESULT(ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: imeshFacetData
    INTEGER(I4B) :: ans
  END FUNCTION Domain_getTotalMeshFacetData
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetSparsity@setMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Oct 2021
! summary: Set sparsity in [[CSRMatrix_]] from [[Domain_]]

INTERFACE
  MODULE SUBROUTINE Domain_SetSparsity1(obj, mat)
    CLASS(Domain_), INTENT(IN) :: obj
    TYPE(CSRMatrix_), INTENT(INOUT) :: mat
  END SUBROUTINE Domain_SetSparsity1
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetSparsity@setMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Oct 2021
! summary: Set sparsity in [[CSRMatrix_]] from [[Domain_]]

INTERFACE
  MODULE SUBROUTINE Domain_SetSparsity2(domains, mat)
    CLASS(DomainPointer_), INTENT(IN) :: domains(:)
    TYPE(CSRMatrix_), INTENT(INOUT) :: mat
  END SUBROUTINE Domain_SetSparsity2
END INTERFACE

INTERFACE DomainSetSparsity
  MODULE PROCEDURE Domain_SetSparsity2
END INTERFACE DomainSetSparsity

PUBLIC :: DomainSetSparsity

!----------------------------------------------------------------------------
!                                               setTotalMaterial@setMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-12-09
! update: 2021-12-09
! summary:

INTERFACE
  MODULE SUBROUTINE Domain_SetTotalMaterial(obj, dim, n)
    CLASS(Domain_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), INTENT(IN) :: n
  END SUBROUTINE Domain_SetTotalMaterial
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetMaterial@setMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-12-09
! update: 2021-12-09
! summary: Set the materials id of a given medium

INTERFACE
  MODULE SUBROUTINE Domain_SetMaterial(obj, dim, entityNum, &
    & medium, material)
    CLASS(Domain_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), INTENT(IN) :: entityNum
    INTEGER(I4B), INTENT(IN) :: medium
    INTEGER(I4B), INTENT(IN) :: material
  END SUBROUTINE Domain_SetMaterial
END INTERFACE

!----------------------------------------------------------------------------
!                                            InitiateElemSD@ShapeDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-12-09
! update: 2021-12-09
! summary: sets the local shape data for the mesh

INTERFACE
  MODULE SUBROUTINE Domain_InitiateElemSD1(obj, &
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
  END SUBROUTINE Domain_InitiateElemSD1
END INTERFACE

!----------------------------------------------------------------------------
!                                            InitiateElemSD@ShapeDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-12-09
! update: 2021-12-09
! summary: sets the local shape data for the mesh

INTERFACE
  MODULE SUBROUTINE Domain_InitiateElemSD2(obj, &
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
  END SUBROUTINE Domain_InitiateElemSD2
END INTERFACE

!----------------------------------------------------------------------------
!                                            InitiateElemSD@ShapeDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-12-09
! update: 2021-12-09
! summary: sets the local shape data for the mesh

INTERFACE
  MODULE SUBROUTINE Domain_InitiateElemSD3(obj, &
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
  END SUBROUTINE Domain_InitiateElemSD3
END INTERFACE

!----------------------------------------------------------------------------
!                                            InitiateElemSD@ShapeDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-12-09
! update: 2021-12-09
! summary: sets the local shape data for the mesh

INTERFACE
  MODULE SUBROUTINE Domain_InitiateElemSD4(obj, dim, tvec)
    CLASS(Domain_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    REAL(DFP), INTENT(IN) :: tvec(:)
  END SUBROUTINE Domain_InitiateElemSD4
END INTERFACE

!----------------------------------------------------------------------------
!                                       InitiateFacetElemSD@ShapeDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-12-09
! update: 2021-12-09
! summary: sets the local shape data for the mesh

INTERFACE
  MODULE SUBROUTINE Domain_InitiateFacetElemSD1(obj, &
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
  END SUBROUTINE Domain_InitiateFacetElemSD1
END INTERFACE

!----------------------------------------------------------------------------
!                                            InitiateElemSD@ShapeDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 May 2022
! summary: sets the local shape data for the mesh

INTERFACE
  MODULE SUBROUTINE Domain_InitiateFacetElemSD2(obj, &
    & dim, &
    & orderSpace,  &
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
    CLASS(Domain_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    !! dimension of the mesh
    INTEGER(I4B), INTENT(IN) :: orderSpace(:)
      !! integrand order in space
    CHARACTER(LEN=*), INTENT(IN) :: quadTypeForSpace
      !! quadrature type for space
    CHARACTER(LEN=*), INTENT(IN) :: continuityTypeForSpace
      !! continuity type of base in space
    CHARACTER(LEN=*), INTENT(IN) :: interpolTypeForSpace
      !! interpol type of base in space
    INTEGER(I4B), INTENT(IN) :: orderTime
      !! integrand order in time
    TYPE(ReferenceLine_), INTENT(IN) :: linTimeElem
      !! linear time element
    TYPE(ReferenceLine_), INTENT(IN) :: timeElem
      !! time element
    CHARACTER(LEN=*), INTENT(IN) :: quadTypeForTime
      !! quadrature type of base in time
    CHARACTER(LEN=*), INTENT(IN) :: continuityTypeForTime
      !! continuity type of base in time
    CHARACTER(LEN=*), INTENT(IN) :: interpolTypeForTime
      !! interpol type of base in time
    REAL(DFP), INTENT(IN) :: tvec(:)
  END SUBROUTINE Domain_InitiateFacetElemSD2
END INTERFACE

!----------------------------------------------------------------------------
!                                            InitiateElemSD@ShapeDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-12-09
! update: 2021-12-09
! summary: sets the local shape data for the mesh

INTERFACE
  MODULE SUBROUTINE Domain_InitiateFacetElemSD3(obj, dim, tvec)
    CLASS(Domain_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    REAL(DFP), INTENT(IN) :: tvec(:)
  END SUBROUTINE Domain_InitiateFacetElemSD3
END INTERFACE

!----------------------------------------------------------------------------
!                                       InitiateNodeToElements@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 Nov 2022
! summary: This routine sets the node-to-elements data in mesh of domain

INTERFACE
  MODULE SUBROUTINE Domain_InitiateNodeToElements(obj)
    CLASS(Domain_), INTENT(INOUT) :: obj
  END SUBROUTINE Domain_InitiateNodeToElements
END INTERFACE

!----------------------------------------------------------------------------
!                                       InitiateNodeToNodes@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 Nov 2022
! summary: This routine sets the node-to-nodes data in mesh of domain

INTERFACE
  MODULE SUBROUTINE Domain_InitiateNodeToNodes(obj)
    CLASS(Domain_), INTENT(INOUT) :: obj
  END SUBROUTINE Domain_InitiateNodeToNodes
END INTERFACE

!----------------------------------------------------------------------------
!                                  InitiateElementToElements@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 Nov 2022
! summary: This routine sets the element-to-element data in mesh of domain

INTERFACE
  MODULE SUBROUTINE Domain_InitiateElementToElements(obj)
    CLASS(Domain_), INTENT(INOUT) :: obj
  END SUBROUTINE Domain_InitiateElementToElements
END INTERFACE

!----------------------------------------------------------------------------
!                                       InitiateBoundaryData@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 Nov 2022
! summary: This routine sets the boundarydata info in mesh of domain
!
!# Introduction
!
! This routine sets the boundary data info in mesh of domain.
! This routine calls `InitiateBoundarydata` on each mesh
! Then, it calls SetFacetElementType() on domain object.

INTERFACE
  MODULE SUBROUTINE Domain_InitiateBoundaryData(obj)
    CLASS(Domain_), INTENT(INOUT) :: obj
  END SUBROUTINE Domain_InitiateBoundaryData
END INTERFACE

!----------------------------------------------------------------------------
!                                      InitiateFacetElements@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 Nov 2022
! summary: This routine sets the facet elements data in mesh of domain

INTERFACE
  MODULE SUBROUTINE Domain_InitiateFacetElements(obj)
    CLASS(Domain_), INTENT(INOUT) :: obj
  END SUBROUTINE Domain_InitiateFacetElements
END INTERFACE

!----------------------------------------------------------------------------
!                                  InitiateExtraNodeToNodes@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 Nov 2022
! summary: This routine sets the node-to-nodes data in mesh of domain

INTERFACE
  MODULE SUBROUTINE Domain_InitiateExtraNodeToNodes(obj)
    CLASS(Domain_), INTENT(INOUT) :: obj
  END SUBROUTINE Domain_InitiateExtraNodeToNodes
END INTERFACE

!----------------------------------------------------------------------------
!                                       SetFacetElementType@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 April 2022
! summary: This routine sets the domain boundary element for cells and faces
!
!# Introduction
!
! The boudnary element of mesh may not be domain boundary element. This
! is because mesh does not have information of surrounding mesh. Therefore
! for mesh methods there is no distinction between boundary element
! and domain-boundary-element. And mesh-method set all of its boundary-elem
! to domain-elem.
!
! This methods correctly identifies the domain-boundary-element from
! mesh boundary-element.
! In this way the mesh-boundary-element, which are not domain-boundary-element
! can be treated as the interface element between two meshes.
!
! This methods needs following information:
!
!- boundary element data should be initiated for each mesh, this means
! a call to InitiateBoundaryElementData is necessary

INTERFACE
  MODULE SUBROUTINE Domain_SetFacetElementType(obj)
    CLASS(Domain_), INTENT(INOUT) :: obj
  END SUBROUTINE Domain_SetFacetElementType
END INTERFACE

!----------------------------------------------------------------------------
!                                      SetDomainFacetElement@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 14 April 2022
! summary: This routine sets the domain boundary element for cells and faces
!
!# Introduction
!
! When we call InitiateFacetData for mesh,  we can only identify
! boundary facet element. However, some of these boundary facet elements
! will be domain-boundary facet element and some will be at the interface
! of two mesh region.
!
! This method correctly identifies the boundary facet element which at
! the domain boundary or mesh boundary.
!
! The boundary facet element which are at the domain boundary are called
! `DOMAIN_BOUNDARY_ELEMENT`
!
! The boundary facet element which are not at the domain boundary are
! called `BOUNDARY_ELEMENT` (that is mesh boundary facet element)
!
! Following information are necessary before calling this method.
!
!- call InitiateFacetData for each mesh

INTERFACE
  MODULE SUBROUTINE Domain_SetDomainFacetElement(obj)
    CLASS(Domain_), INTENT(INOUT) :: obj
  END SUBROUTINE Domain_SetDomainFacetElement
END INTERFACE

!----------------------------------------------------------------------------
!                                                 SetMeshmap@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 May 2022
! summary: This routine sets meshMap

INTERFACE
  MODULE SUBROUTINE Domain_SetMeshmap(obj)
    CLASS(Domain_), INTENT(INOUT) :: obj
  END SUBROUTINE Domain_SetMeshmap
END INTERFACE

!----------------------------------------------------------------------------
!                                        SetMeshFacetElement@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 May 2022
! summary: This routine sets meshFacetData

INTERFACE
  MODULE SUBROUTINE Domain_SetMeshFacetElement(obj)
    CLASS(Domain_), INTENT(INOUT) :: obj
  END SUBROUTINE Domain_SetMeshFacetElement
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE Domain_Class
