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
USE tomlf, ONLY: toml_table
USE TxtFile_Class
IMPLICIT NONE
PRIVATE

PUBLIC :: Domain_
PUBLIC :: DomainPointer_
PUBLIC :: DomainDeallocate
PUBLIC :: Domain_Pointer
PUBLIC :: DomainSetSparsity

CHARACTER(*), PARAMETER :: modName = "Domain_Class"

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
!
! In this way, the `slaveCell` of a `meshFacet` is inside some other mesh.
! The information of `slaveCell` number will be accessed through the
! Halo of the mesh.
!
! The `halo` of the mesh will be stored inside the instance of `Mesh_`
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
!{!pages/docs-api/Domain/Domain_.md!}

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
  INTEGER(I4B), ALLOCATABLE, PUBLIC :: global_nptrs(:)
    !! global nptrs

  TYPE(MeshPointer_), ALLOCATABLE :: meshVolume(:)
    !! meshVolume list of meshes of volume entities
  TYPE(MeshPointer_), ALLOCATABLE :: meshSurface(:)
    !! meshSurface list of meshes of surface entities
  TYPE(MeshPointer_), ALLOCATABLE :: meshCurve(:)
    !! meshCurve list of meshes of curve entities
  TYPE(MeshPointer_), ALLOCATABLE :: meshPoint(:)
    !! meshPoint list of meshes of point entities

  TYPE(MeshFacetData_), ALLOCATABLE, PUBLIC :: meshFacetData(:)
  !! Mesh facet data
  TYPE(CSRSparsity_) :: meshMap
  !! Sparse mesh data in CSR format
CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(Obj) :: Initiate => Domain_Initiate
  !! Initiate an instance of domain
  PROCEDURE, PUBLIC, PASS(Obj) :: DEALLOCATE => Domain_Deallocate
  !! Deallocate data stored inside an instance of domain
  !! TODO Rename Deallocate to Deallocate
  FINAL :: Domain_Final
  !! Finalizer for domain

  ! IO:
  ! @IOMethods
  PROCEDURE, PASS(Obj) :: IMPORT => Domain_Import
  !! Initiates an instance of domain by importing data from meshfile
  !! TODO Add an export method to [[Domain_]] class
  PROCEDURE, PASS(obj) :: ImportFromToml1 => Domain_ImportFromToml1
  PROCEDURE, PASS(obj) :: ImportFromToml2 => Domain_ImportFromToml2
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1,  &
  & ImportFromToml2
  !! Initiates an instance of domain by importing meshfile name from
  !! Toml file
  PROCEDURE, PUBLIC, PASS(obj) :: Display => Domain_Display
  !! TODO Add a display method to [[Domain_]] class
  PROCEDURE, PUBLIC, PASS(obj) :: DisplayMeshFacetData => &
    & Domain_DisplayMeshFacetData
  !! Display mesh facet data

  ! GET:
  ! @GetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: IsNodePresent => Domain_IsNodePresent
  PROCEDURE, PUBLIC, PASS(obj) :: IsElementPresent => Domain_IsElementPresent
  PROCEDURE, PUBLIC, PASS(obj) :: GetConnectivity => Domain_GetConnectivity
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
  GENERIC, PUBLIC :: OPERATOR(.tNodes.) => &
    & Domain_tNodes1, Domain_tNodes2
  !! Generic method for getting total nodes
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
  !! Returns a dim entity-num of mesh which contains the element number
  PROCEDURE, PASS(obj) :: GetNodeCoord1 => Domain_GetNodeCoord
  !! This routine returns the nodal coordinate in rank2 array
  PROCEDURE, PASS(obj) :: GetNodeCoord2 => Domain_GetNodeCoord2
  !! This routine returns the nodal coordinate in rank2 array
  GENERIC, PUBLIC :: GetNodeCoord => GetNodeCoord1, GetNodeCoord2
  !! Generic method which returns the nodal coordinates
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
  PROCEDURE, PRIVATE, PASS(obj) :: Domain_GetTotalMaterial1, &
    & Domain_GetTotalMaterial2
  GENERIC, PUBLIC :: GetTotalMaterial => &
    & Domain_GetTotalMaterial1, &
    & Domain_GetTotalMaterial2
  !! Get total number of materials
  PROCEDURE, PUBLIC, PASS(obj) :: GetElemType => Domain_GetElemType
  !! Returns the element type of each mesh
  PROCEDURE, PUBLIC, PASS(obj) :: GetUniqueElemType =>  &
    & Domain_GetUniqueElemType
  !! Returns the unique element type in each mesh
  !! The size of returned integer vector can be different from
  !! the total number of meshes present in domain.

  ! SET:
  ! @setMethods
  PROCEDURE, PASS(obj) :: SetSparsity1 => Domain_SetSparsity1
  PROCEDURE, NOPASS :: SetSparsity2 => Domain_SetSparsity2
  GENERIC, PUBLIC :: SetSparsity => SetSparsity1, SetSparsity2
  PROCEDURE, PUBLIC, PASS(obj) :: SetTotalMaterial => Domain_SetTotalMaterial
  !! set the total number of materials
  PROCEDURE, PUBLIC, PASS(obj) :: SetMaterial => Domain_SetMaterial
  !! set the material
  PROCEDURE, PASS(obj) :: SetNodeCoord1 => Domain_SetNodeCoord1
  !! setNodeCoord
  GENERIC, PUBLIC :: SetNodeCoord => SetNodeCoord1
  PROCEDURE, PUBLIC, PASS(obj) :: SetQuality => Domain_SetQuality

  ! SET:
  ! @MeshDataMethods
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

END TYPE Domain_

!----------------------------------------------------------------------------
!                                                             DomainPointer
!----------------------------------------------------------------------------

TYPE :: DomainPointer_
  CLASS(Domain_), POINTER :: ptr => NULL()
END TYPE DomainPointer_

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
    CHARACTER(*), INTENT(IN) :: group
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
  MODULE SUBROUTINE MeshFacetData_Initiate(obj, n)
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
  MODULE FUNCTION MeshFacetData_isInitiated(obj) RESULT(ans)
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
  MODULE FUNCTION MeshFacetData_Size(obj) RESULT(ans)
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

INTERFACE DomainDeallocate
  MODULE SUBROUTINE Domain_Deallocate(obj)
    CLASS(Domain_), INTENT(INOUT) :: obj
    !! Domain object
  END SUBROUTINE Domain_Deallocate
END INTERFACE DomainDeallocate

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
!                                          Domain_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 19 June 2021
! summary: This function returns pointer to a newly constructed Domain obj

INTERFACE Domain_Pointer
  MODULE FUNCTION Domain_Constructor_1(hdf5, group) RESULT(Ans)
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
    CLASS(Domain_), POINTER :: ans
  END FUNCTION Domain_Constructor_1
END INTERFACE Domain_Pointer

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
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE Domain_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2023-12-20
! summary:  Initiate an instance of domain by importing meshfile name from
! Toml file
!
! NOTE: default meshfile name is "mesh.h5"
! and default group in hdf5 is ""
!
! NOTE: meshfile (hdf5) is internally initiated and is deallocated
! after initiation of domain

INTERFACE
  MODULE SUBROUTINE Domain_ImportFromToml1(obj, table)
    CLASS(Domain_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE Domain_ImportFromToml1
END INTERFACE

!----------------------------------------------------------------------------
!                                                   ImportFromToml1@IOMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:   2023-12-20
! summary:  Initiate an instance of domain by importing meshfile name from
! Toml file
!
! NOTE: default meshfile name is "mesh.h5"
! and default group in hdf5 is ""
!
! NOTE: meshfile (hdf5) is internally initiated and is deallocated
! after initiation of domain

INTERFACE
  MODULE SUBROUTINE Domain_ImportFromToml2(obj, tomlName, afile, filename,  &
  & printToml)
    CLASS(Domain_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: tomlName
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
  END SUBROUTINE Domain_ImportFromToml2
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
    CHARACTER(*), INTENT(IN) :: msg
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
    CHARACTER(*), INTENT(IN) :: msg
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
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE MeshFacetData_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                   IsNodePresent@GetMethods
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
!                                               IsElementPresent@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-12
! update: 2021-11-12
! summary: Returns true if the element number is present inside the domain

INTERFACE
  MODULE FUNCTION Domain_IsElementPresent(obj, globalElement, dim) RESULT(Ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! Element number
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim
    !! Dimension, if dim is present then
    !! if dim=0, then search is performed in meshPoint
    !! if dim=1, then search is performed in meshCurve
    !! if dim=2, then search is performed in meshSurface
    !! if dim=3, then search is performed in meshVolume
    !! If dim is not present, then search is performed in all meshes
    LOGICAL(LGT) :: ans
  END FUNCTION Domain_IsElementPresent
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetConnectivity@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-12
! update: 2021-11-12
! summary: Returns the connectivity vector of a given element number

INTERFACE
  MODULE FUNCTION Domain_GetConnectivity(obj, globalElement, dim) RESULT(Ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! Global element number
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim
    !! Dimension, if dim is present then
    !! if dim=0, then search is performed in meshPoint
    !! if dim=1, then search is performed in meshCurve
    !! if dim=2, then search is performed in meshSurface
    !! if dim=3, then search is performed in meshVolume
    !! If dim is not present, then search is performed in all meshes
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !! vertex connectivity
  END FUNCTION Domain_GetConnectivity
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetNodeToElements@GetMethods
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
!                                               GetNodeToElements@GetMethods
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
!                                                 GetTotalNodes@GetMethods
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
!
! Note: Both `dim` and `entityNum` should be present or absent.
! This is because two entities of same dimension can have common nodes
! which this routine cannot predict.

INTERFACE
  MODULE FUNCTION Domain_GetTotalNodes(obj, dim, entityNum) RESULT(Ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim
    !! dimension of the mesh entity
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: entityNum
    !! entity number
    INTEGER(I4B) :: ans
  END FUNCTION Domain_GetTotalNodes
END INTERFACE

!----------------------------------------------------------------------------
!                                                         tNodes@GetMethods
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
    !! opt(1) is dim
    !! opt(2) is entityNum
    INTEGER(I4B) :: ans
  END FUNCTION Domain_tNodes1
END INTERFACE

!----------------------------------------------------------------------------
!                                                          tNodes@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 June 2021
! summary: Returns the total number of nodes in the domain

INTERFACE
  MODULE FUNCTION Domain_tNodes2(obj) RESULT(Ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION Domain_tNodes2
END INTERFACE

!----------------------------------------------------------------------------
!                                                getTotalElements@GetMethods
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
  MODULE FUNCTION Domain_GetTotalElements(obj, dim, entityNum) RESULT(Ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim
    !! dimension of mesh entities
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: entityNum
    !! mesh entity number
    INTEGER(I4B) :: ans
  END FUNCTION Domain_GetTotalElements
END INTERFACE

!----------------------------------------------------------------------------
!                                                      tElements@GetMethods
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
!                                                      tElements@GetMethods
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
!                                                      tElements@GetMethods
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
!                                             getLocalNodeNumber@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Sept 2021
! summary: Returns local node number of a global node number

INTERFACE
  MODULE FUNCTION Domain_GetLocalNodeNumber1(obj, globalNode) RESULT(Ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B) :: ans
  END FUNCTION Domain_GetLocalNodeNumber1
END INTERFACE

!----------------------------------------------------------------------------
!                                              getLocalNodeNumber@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Sept 2021
! summary: Returns local node number of a global node number

INTERFACE
  MODULE FUNCTION Domain_GetLocalNodeNumber2(obj, globalNode) RESULT(Ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B) :: ans(SIZE(globalNode))
  END FUNCTION Domain_GetLocalNodeNumber2
END INTERFACE

!----------------------------------------------------------------------------
!                                             getGlobalNodeNumber@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Sept 2021
! summary: Returns local node number of a global node number

INTERFACE
  MODULE FUNCTION Domain_GetGlobalNodeNumber1(obj, localNode) RESULT(Ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: localNode
    INTEGER(I4B) :: ans
  END FUNCTION Domain_GetGlobalNodeNumber1
END INTERFACE

!----------------------------------------------------------------------------
!                                              getGlobalNodeNumber@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Sept 2021
! summary: Returns local node number of a global node number

INTERFACE
  MODULE FUNCTION Domain_GetGlobalNodeNumber2(obj, localNode) RESULT(Ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: localNode(:)
    INTEGER(I4B) :: ans(SIZE(localNode))
  END FUNCTION Domain_GetGlobalNodeNumber2
END INTERFACE

!----------------------------------------------------------------------------
!                                                    getTotalMesh@GetMethods
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
  MODULE FUNCTION Domain_GetTotalMesh(obj, dim) RESULT(Ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B) :: ans
  END FUNCTION Domain_GetTotalMesh
END INTERFACE

!----------------------------------------------------------------------------
!                                                  getMeshPointer@GetMethods
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
  MODULE FUNCTION Domain_GetMeshPointer1(obj, dim, entityNum) RESULT(Ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    !! dimension of mesh entity
    INTEGER(I4B), INTENT(IN) :: entityNum
    !! entity number
    CLASS(Mesh_), POINTER :: ans
  END FUNCTION Domain_GetMeshPointer1
END INTERFACE

!----------------------------------------------------------------------------
!                                                  getMeshPointer@GetMethods
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
  MODULE FUNCTION Domain_GetMeshPointer2(obj, globalElement) RESULT(Ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    CLASS(Mesh_), POINTER :: ans
  END FUNCTION Domain_GetMeshPointer2
END INTERFACE

!----------------------------------------------------------------------------
!                                                 getDimEntityNum@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-12
! update: 2021-11-12
! summary: Returns dim and entity number

INTERFACE
  MODULE FUNCTION Domain_GetDimEntityNum(obj, globalElement) RESULT(ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    INTEGER(I4B) :: ans(2)
  END FUNCTION Domain_GetDimEntityNum
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
  MODULE SUBROUTINE Domain_GetNodeCoord(obj, nodeCoord, dim, entityNum)
    CLASS(Domain_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: nodeCoord(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: entityNum
  END SUBROUTINE Domain_GetNodeCoord
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
  MODULE SUBROUTINE Domain_GetNodeCoord2(obj, nodeCoord, globalNode)
    CLASS(Domain_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: nodeCoord(:, :)
    !! It should be allocated by the user.
    !! SIZE(nodeCoord, 1) is equal to nsd
    !! Size(nodeCoord, 2) is equal to the size(globalNode)
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
  END SUBROUTINE Domain_GetNodeCoord2
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
  MODULE FUNCTION Domain_GetNodeCoordPointer(obj) RESULT(ans)
    CLASS(Domain_), TARGET, INTENT(IN) :: obj
    REAL(DFP), POINTER :: ans(:, :)
  END FUNCTION Domain_GetNodeCoordPointer
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
  MODULE FUNCTION Domain_GetGlobalToLocalNodeNumPointer(obj) RESULT(ans)
    CLASS(Domain_), TARGET, INTENT(IN) :: obj
    INTEGER(I4B), POINTER :: ans(:)
  END FUNCTION Domain_GetGlobalToLocalNodeNumPointer
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
  MODULE FUNCTION Domain_GetNptrs(obj, entityNum, dim) RESULT(Ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: entityNum(:)
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION Domain_GetNptrs
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
  MODULE FUNCTION Domain_GetInternalNptrs(obj, entityNum, dim) RESULT(Ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: entityNum(:)
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION Domain_GetInternalNptrs
END INTERFACE

!----------------------------------------------------------------------------
!                                                           getNSD@getMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Sept 2021
! summary: This routine returns the number of spatial dimensions

INTERFACE
  MODULE FUNCTION Domain_GetNSD(obj) RESULT(Ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION Domain_GetNSD
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetOrder@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Sept 2021
! summary: This routine returns the order of meshes of dimensions=dim

INTERFACE
  MODULE FUNCTION Domain_GetOrder(obj, dim) RESULT(Ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION Domain_GetOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                                  getBoundingBox@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 13 Oct 2021
! summary: Returns bounding box

INTERFACE
  MODULE FUNCTION Domain_GetBoundingBox(obj) RESULT(Ans)
    CLASS(Domain_), INTENT(IN) :: obj
    TYPE(BoundingBox_) :: ans
  END FUNCTION Domain_GetBoundingBox
END INTERFACE

!----------------------------------------------------------------------------
!                                          getTotalMeshFacetData@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 May 2022
! summary: returns size of meshFacetData

INTERFACE
  MODULE FUNCTION Domain_GetTotalMeshFacetData(obj, imeshFacetData) &
    & RESULT(ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: imeshFacetData
    INTEGER(I4B) :: ans
  END FUNCTION Domain_GetTotalMeshFacetData
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetTotalMaterial@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-12-09
! update: 2021-12-09
! summary: Returns the materials id of a given medium

INTERFACE
  MODULE FUNCTION Domain_GetTotalMaterial1(obj, dim) RESULT(ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION Domain_GetTotalMaterial1
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetTotalMaterial@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-12-09
! update: 2021-12-09
! summary: Set the materials id of a given medium

INTERFACE
  MODULE FUNCTION Domain_GetTotalMaterial2(obj, dim, entityNum) RESULT(ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), INTENT(IN) :: entityNum
    INTEGER(I4B) :: ans
  END FUNCTION Domain_GetTotalMaterial2
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetElemType@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-23
! summary:  Returns the element type of each mesh in domain

INTERFACE
  MODULE FUNCTION Domain_GetElemType(obj, dim) RESULT(ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION Domain_GetElemType
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetUniqueElemType@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-23
! summary: Returns only the unique elements in the meshes of domain

INTERFACE
  MODULE FUNCTION Domain_GetUniqueElemType(obj, dim) RESULT(ans)
    CLASS(Domain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION Domain_GetUniqueElemType
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

INTERFACE DomainSetSparsity
  MODULE SUBROUTINE Domain_SetSparsity2(domains, mat)
    CLASS(DomainPointer_), INTENT(IN) :: domains(:)
    TYPE(CSRMatrix_), INTENT(INOUT) :: mat
  END SUBROUTINE Domain_SetSparsity2
END INTERFACE DomainSetSparsity

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
!                                                   SetNodeCoord@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-02-24
! summary: SetNodeCoord

INTERFACE
  MODULE SUBROUTINE Domain_SetNodeCoord1(obj, nodeCoord, scale, &
    & addContribution)
    CLASS(Domain_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: nodeCoord(:, :)
    !! nodal coordinate in xij Format
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE Domain_SetNodeCoord1
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
! This routine sets the domain boundary element for cells and faces.
!
! When we call [InitiateFacetElement](../Mesh/InitiateFacetElement.md)
! for mesh,
! we can only identify boundary-facet-elements (i.e., boundary elements
! of the mesh).
! Moreover, when we call
! [InitiateFacetElement](../Mesh/InitiateFacetElement.md)
! from mesh or domain, all the facet elements are tagged
! as `DOMAIN_BOUNDARY_ELEMENT`.
!
! However, some of these boundary facet-elements will be located at the
! domain’s boundary. These facet elements are called `DOMAIN_BOUNDARY_ELEMENT`.
!
! Some of the facet elements will be at located at the interface of two
! mesh regions, these facet elements are called `BOUNDARY_ELEMENT`.
!
! This method correctly differentiates between `BOUNDARY_ELEMENT`  and
! `DOMAIN_BOUNDARY_ELEMENT`.

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
!                                                   SetQuality@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE Domain_SetQuality(obj, measures, max_measures, &
    & min_measures, dim, entityNum)
    CLASS(Domain_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: measures(:)
    REAL(DFP), INTENT(OUT) :: max_measures(:)
    REAL(DFP), INTENT(OUT) :: min_measures(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: entityNum
  END SUBROUTINE Domain_SetQuality
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE Domain_Class
