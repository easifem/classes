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

MODULE AbstractDomain_Class
USE GlobalData, ONLY: DFP, I4B, LGT
USE BaseType, ONLY: CSRSparsity_, CSRMatrix_, BoundingBox_
USE String_Class, ONLY: String
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE HDF5File_Class, ONLY: HDF5File_
USE tomlf, ONLY: toml_table
USE TxtFile_Class, ONLY: TxtFile_
USE ExceptionHandler_Class, ONLY: e
USE Kdtree2_Module, ONLY: Kdtree2_, Kdtree2Result_
USE ElemData_Class, ONLY: ElemData_

IMPLICIT NONE
PRIVATE

PUBLIC :: AbstractDomain_
PUBLIC :: AbstractDomainPointer_
PUBLIC :: AbstractDomainDeallocate
PUBLIC :: AbstractDomainSetSparsity
PUBLIC :: AbstractDomainInitiate
PUBLIC :: AbstractDomainImport
PUBLIC :: AbstractDomainDisplay
PUBLIC :: AbstractDomainDisplayDomainInfo

CHARACTER(*), PARAMETER :: modName = "AbstractDomain_Class"

!----------------------------------------------------------------------------
!                                                            AbstractDomain_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-03-31
! summary: AbstractDomain_ contains finite element mesh data of a domain
!
!{!pages/docs-api/AbstractDomain/AbstractDomain_.md!}

TYPE, ABSTRACT :: AbstractDomain_
  PRIVATE
  LOGICAL(LGT) :: showTime = .FALSE.
  !! set to true if you want to show time taken by various routines.
  LOGICAL(LGT) :: isInit = .FALSE.
  !! flag to check if the ddomain is initiated or not
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
  INTEGER(I4B) :: maxNptrs = 0
    !! Largest node number in the domain
  INTEGER(I4B) :: minNptrs = 0
    !! Smallest node number in the domain
  INTEGER(I4B) :: tNodes = 0
    !! Total number of nodes in the mesh
  LOGICAL(I4B) :: isNodeNumberSparse = .FALSE.
    !! True if node numbers are not continuous
  INTEGER(I4B) :: maxElemNum = 0
    !! Largest element number in the domain
  INTEGER(I4B) :: minElemNum = 0
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
  REAL(DFP), ALLOCATABLE :: nodeCoord(:, :)
    !! Nodal coordinates in XiJ format
    !! Number of rows are 3, and number of columns is total nodes
    !! How are these node coords arranged?

  TYPE(Kdtree2_), POINTER :: kdtree => NULL()
  TYPE(Kdtree2Result_), ALLOCATABLE :: kdresult(:)

CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods

  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  !! Initiate an instance of domain

  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Deallocate data stored inside an instance of domain

  PROCEDURE, PUBLIC, PASS(obj) :: DeallocateKdtree => obj_DeallocateKdtree

  ! IO:
  ! @IOMethods

  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  !! Initiates an instance of domain by importing data from meshfile
  !! TODO Add an export method to [[obj_]] class
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: ImportFromToml1 => &
    obj_ImportFromToml1
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: ImportFromToml2 => &
    obj_ImportFromToml2
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, &
    ImportFromToml2
  !! Initiates an instance of domain by importing meshfile name from
  !! Toml file
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! TODO Add a display method to [[obj_]] class
  PROCEDURE, PUBLIC, PASS(obj) :: DisplayDomainInfo => &
    obj_DisplayDomainInfo

  ! GET:
  ! @GetMethods

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: IsInitiated => &
    obj_IsInitiated
  !! Returns obj%isInit
  PROCEDURE, PUBLIC, PASS(obj) :: GetMeshPointer => obj_GetMeshPointer1
  !! Returns pointer to the mesh in the domain

  PROCEDURE, PUBLIC, PASS(obj) :: IsNodePresent => obj_IsNodePresent
  !! Check if a node is present or node in domain

  PROCEDURE, PUBLIC, PASS(obj) :: IsElementPresent => obj_IsElementPresent
  !! Check if an element is present or node in domain

  PROCEDURE, PUBLIC, PASS(obj) :: GetConnectivity => obj_GetConnectivity
  !! Get the vertex connectivity

  PROCEDURE, PUBLIC, PASS(obj) :: GetNNE => obj_GetNNE
  !! Get number of nodes(vertex)  in element, size of connectivity

  PROCEDURE, PASS(obj) :: GetNodeToElements1 => obj_GetNodeToElements1
  !! Get the list of elements connnected to a specified node
  PROCEDURE, PASS(obj) :: GetNodeToElements2 => obj_GetNodeToElements2
  !! Get the list of elements connnected to many specified nodes
  GENERIC, PUBLIC :: GetNodeToElements => &
    GetNodeToElements1, GetNodeToElements2
  !! Generic method to get node to element data

  PROCEDURE, PASS(obj) :: GetNodeToElements1_ => obj_GetNodeToElements1_
  !! Get the list of elements connnected to a specified node
  PROCEDURE, PASS(obj) :: GetNodeToElements2_ => obj_GetNodeToElements2_
  !! Get the list of elements connnected to many specified nodes
  GENERIC, PUBLIC :: GetNodeToElements_ => &
    & GetNodeToElements1_, &
    & GetNodeToElements2_
  !! Generic method to get node to element data

  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalNodes => obj_GetTotalNodes
    !! returns the total number of nodes in the domain, mesh, or part of mesh
  PROCEDURE, PASS(obj) :: obj_tNodes1
    !! Returns the total nodes in domain
  PROCEDURE, PASS(obj) :: obj_tNodes2
    !! Returns the total nodes in a dimension
  PROCEDURE, PASS(obj) :: obj_tNodes3
    !! Returns the total nodes in domain
  GENERIC, PUBLIC :: OPERATOR(.tNodes.) => &
    & obj_tNodes1, obj_tNodes2, obj_tNodes3
  !! Generic method for Getting total nodes

  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalElements => obj_GetTotalElements
  !! returns the total number of Elements in domain, mesh, or part of mesh
  PROCEDURE, PASS(obj) :: obj_tElements1
  PROCEDURE, PASS(obj) :: obj_tElements2
  PROCEDURE, PASS(obj) :: obj_tElements3
  !! returns total number of elements in domain, mesh, or part of domain
  GENERIC, PUBLIC :: OPERATOR(.tElements.) => obj_tElements1,  &
    & obj_tElements2, obj_tElements3
  !! return total number of elements in domain, mesh, or part of domain

  PROCEDURE, PASS(obj) :: GetLocalNodeNumber1 => obj_GetLocalNodeNumber1
  !! Get local node numbers stored in the domain
  PROCEDURE, PASS(obj) :: GetLocalNodeNumber2 => obj_GetLocalNodeNumber2
  !! Get local node numbers stored in the domain
  GENERIC, PUBLIC :: GetLocalNodeNumber => GetLocalNodeNumber1, &
    GetLocalNodeNumber2

  PROCEDURE, PASS(obj) :: GetLocalElemNumber1 => obj_GetLocalElemNumber1
  !! Get local element number from global element number
  PROCEDURE, PASS(obj) :: GetLocalElemNumber2 => obj_GetLocalElemNumber2
  !! Get local element number from global element number
  GENERIC, PUBLIC :: GetLocalElemNumber => GetLocalElemNumber1, &
    GetLocalElemNumber2
  !! Returns the local element number of a global element number

  PROCEDURE, PUBLIC, PASS(obj) :: GetElemDataPointer => &
    obj_GetElemDataPointer
  !! Get pointer to an element data

  PROCEDURE, PUBLIC, PASS(obj) :: GetElemData => obj_GetElemData
  !! Get pointer to an element data

  PROCEDURE, PASS(obj) :: GetGlobalNodeNumber1 => obj_GetGlobalNodeNumber1
  !! Returns the global node number of a local node number
  PROCEDURE, PASS(obj) :: GetGlobalNodeNumber2 => obj_GetGlobalNodeNumber2
  !! Returns the global node number of a local node number
  GENERIC, PUBLIC :: GetGlobalNodeNumber => &
    & GetGlobalNodeNumber1, &
    & GetGlobalNodeNumber2

  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalEntities => obj_GetTotalEntities
  !! Returns total number of mesh
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalEntitiesList => &
    obj_GetTotalEntitiesList
  !! Return total number of entities in an element
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalMesh => obj_GetTotalEntities
  !! GetTotalMesh will be removed in future, please use GetTotalEntities

  PROCEDURE, PUBLIC, PASS(obj) :: GetDimEntityNum => obj_GetDimEntityNum
  !! Returns a dim entity-num of mesh which contains the element number

  PROCEDURE, PASS(obj) :: GetNodeCoord1 => obj_GetNodeCoord1
  !! This routine returns the nodal coordinate in rank2 array
  PROCEDURE, PASS(obj) :: GetNodeCoord2 => obj_GetNodeCoord2
  !! This routine returns the nodal coordinate in rank2 array
  PROCEDURE, PASS(obj) :: GetNodeCoord3 => obj_GetNodeCoord3
  !! This routine returns the nodal coordinate in rank2 array
  GENERIC, PUBLIC :: GetNodeCoord => GetNodeCoord1, GetNodeCoord2, &
    GetNodeCoord3
  !! Generic method which returns the nodal coordinates

  PROCEDURE, PUBLIC, PASS(obj) :: GetNodeCoordPointer => &
    obj_GetNodeCoordPointer
  !! This routine returns the pointer to nodal coordinate

  PROCEDURE, PUBLIC, PASS(obj) :: GetNearestNode1 => obj_GetNearestNode1
  PROCEDURE, PUBLIC, PASS(obj) :: GetNearestNode2 => obj_GetNearestNode2
  GENERIC, PUBLIC :: GetNearestNode => GetNearestNode1, GetNearestNode2

  PROCEDURE, PUBLIC, PASS(obj) :: GetNptrs => obj_GetNptrs
  !! returns node number, this is a function

  PROCEDURE, PUBLIC, PASS(obj) :: GetNptrs_ => obj_GetNptrs_
  !! returns node number, this is subroutine

  PROCEDURE, PUBLIC, PASS(obj) :: GetInternalNptrs => &
    obj_GetInternalNptrs
  !! returns internal node number

  PROCEDURE, PUBLIC, PASS(obj) :: GetNptrsInBox => obj_GetNptrsInBox
  !! Get node numbers in the box

  PROCEDURE, PUBLIC, PASS(obj) :: GetNptrsInBox_ => obj_GetNptrsInBox_
  !! Get node numbers in box with allocation

  PROCEDURE, PUBLIC, PASS(obj) :: GetBoundingBox => obj_GetBoundingBox
  !! returns bounding box

  PROCEDURE, PUBLIC, PASS(obj) :: GetNSD => obj_GetNSD
  !! Returns the spatial dimension of each physical entities

  PROCEDURE, PUBLIC, PASS(obj) :: GetOrder => obj_GetOrder
  !! Get Order

  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalMeshFacetData => &
    obj_GetTotalMeshFacetData

  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalMaterial => obj_GetTotalMaterial
  !! Get total number of materials

  PROCEDURE, PUBLIC, PASS(obj) :: GetElemType => obj_GetElemType
  !! returns the element type of each mesh

  PROCEDURE, PUBLIC, PASS(obj) :: GetUniqueElemType => &
    obj_GetUniqueElemType
  !! Returns the unique element type in each mesh
  !! The size of returned integer vector can be different from
  !! the total number of meshes present in domain.

  PROCEDURE, PUBLIC, PASS(obj) :: GetMaxNodeNumber => obj_GetMaxNodeNumber
  !! Returns obj%maxNptrs

  PROCEDURE, PUBLIC, PASS(obj) :: GetMinNodeNumber => obj_GetMinNodeNumber
  !! Returns obj%minNptrs

  PROCEDURE, PUBLIC, PASS(obj) :: GetMaxElemNumber => obj_GetMaxElemNumber
  !! Returns obj%maxElemNum

  PROCEDURE, PUBLIC, PASS(obj) :: GetMinElemNumber => obj_GetMinElemNumber
  !! Returns obj%minElemNum

  PROCEDURE, PUBLIC, PASS(obj) :: GetParam => obj_GetParam
  !! Returns parameters

  PROCEDURE, PUBLIC, PASS(obj) :: GetGlobalEdgeNumber => &
    obj_GetGlobalEdgeNumber
  !! Get global Edge number from global element and localEdgenumber

  PROCEDURE, PUBLIC, PASS(obj) :: GetGlobalFaceNumber => &
    obj_GetGlobalFaceNumber
  !! Get global face number from global element and localFacenumber

  PROCEDURE, PASS(obj) :: GetConnectivity1_ => obj_GetConnectivity1_
  !! Get connectivity of an element in a single vector
  !! you can specify opt="A, V, E, F, C" for all, vertex, edge, face, cell

  PROCEDURE, PASS(obj) :: GetConnectivity2_ => obj_GetConnectivity2_
  !! Get connectivity of an element into separate vectors
  !! you can get cell, face, and edge connectivity

  GENERIC, PUBLIC :: GetConnectivity_ => GetConnectivity1_, &
    GetConnectivity2_
  !! Generic method for getting the connectivity of an element

  PROCEDURE, PASS(obj) :: GetTotalVertexNodes1 => obj_GetTotalVertexNodes1
  !! Get total number of vertex nodes in the mesh, global element
  PROCEDURE, PASS(obj) :: GetTotalVertexNodes2 => obj_GetTotalVertexNodes2
  !! Get total number of vertex nodes in the list of global elements
  GENERIC, PUBLIC :: GetTotalVertexNodes => GetTotalVertexNodes1, &
    GetTotalVertexNodes2

  PROCEDURE, PUBLIC, PASS(obj) :: GetOrientation => obj_GetOrientation
  !! Get the orientation of the element of the mesh

  PROCEDURE, PUBLIC, PASS(obj) :: GetElemTopologyIndx => &
    obj_GetElemTopologyIndx
  !! Get the topology index of the element of the mesh

  ! SET:
  ! @SetMethods

  PROCEDURE, PUBLIC, PASS(obj) :: SetShowTime => obj_SetShowTime
  !! Set showTime option

  PROCEDURE, PASS(obj) :: SetSparsity1 => obj_SetSparsity1
  PROCEDURE, NOPASS :: SetSparsity2 => obj_SetSparsity2
  GENERIC, PUBLIC :: SetSparsity => SetSparsity1, SetSparsity2

  PROCEDURE, PUBLIC, PASS(obj) :: SetTotalMaterial => obj_SetTotalMaterial
  !! set the total number of materials

  PROCEDURE, PUBLIC, PASS(obj) :: SetMaterial => obj_SetMaterial
  !! set the material

  PROCEDURE, PUBLIC, PASS(obj) :: SetNodeCoord => obj_SetNodeCoord
  !! setNodeCoord

  PROCEDURE, PUBLIC, PASS(obj) :: SetQuality => obj_SetQuality

  PROCEDURE, PUBLIC, PASS(obj) :: SetTotalElements => obj_SetTotalElements
  !! This method sets the value of tElements(indx) to value

  ! SET:
  ! @MeshDataMethods

  PROCEDURE, PUBLIC, PASS(obj) :: InitiateKdtree => obj_InitiateKdtree
  !! initiate the kdtree structure

  PROCEDURE, PUBLIC, PASS(obj) :: InitiateNodeToElements => &
    obj_InitiateNodeToElements
  !! Initiate node to element data

  PROCEDURE, PUBLIC, PASS(obj) :: InitiateNodeToNodes => &
    obj_InitiateNodeToNodes
  !! Initiate node to node data

  PROCEDURE, PUBLIC, PASS(obj) :: InitiateElementToElements => &
    obj_InitiateElementToElements
  !! Initiate element to element data

  PROCEDURE, PUBLIC, PASS(obj) :: InitiateBoundaryData => &
    obj_InitiateBoundaryData
  !! Initiate element to element data

  PROCEDURE, PUBLIC, PASS(obj) :: InitiateFacetElements => &
    obj_InitiateFacetElements
  !! Initiate element to element data

  PROCEDURE, PUBLIC, PASS(obj) :: InitiateExtraNodeToNodes => &
    obj_InitiateExtraNodeToNodes
  !! Initiate extra node to nodes information for edge based methods

  PROCEDURE, PUBLIC, PASS(obj) :: SetFacetElementType => &
    obj_SetFacetElementType
  !! Set facet element of meshes

  PROCEDURE, PUBLIC, PASS(obj) :: SetDomainFacetElement => &
    obj_SetDomainFacetElement
  !! Set facet element of meshes

  PROCEDURE, PUBLIC, PASS(obj) :: SetMeshmap => obj_SetMeshmap
  !! valid for old style domain only

  PROCEDURE, PUBLIC, PASS(obj) :: SetMeshFacetElement => &
    obj_SetMeshFacetElement

END TYPE AbstractDomain_

!----------------------------------------------------------------------------
!                                                      AbstractDomainPointer
!----------------------------------------------------------------------------

TYPE :: AbstractDomainPointer_
  CLASS(AbstractDomain_), POINTER :: ptr => NULL()
END TYPE AbstractDomainPointer_

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-03-28
! summary: Initiate the instance of [[AbstractDomain_]] object

INTERFACE AbstractDomainInitiate
  MODULE SUBROUTINE obj_Initiate(obj, hdf5, group)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
    !! AbstractDomainData object
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! HDF5 file
    CHARACTER(*), INTENT(IN) :: group
    !! Group name (directory name)
  END SUBROUTINE obj_Initiate
END INTERFACE AbstractDomainInitiate

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-03-28
! summary: Deallocate data stored in AbstractDomain object

INTERFACE AbstractDomainDeallocate
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
    !! AbstractDomain object
  END SUBROUTINE obj_Deallocate
END INTERFACE AbstractDomainDeallocate

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-10
! summary: Deallocate kdtree related data

INTERFACE
  MODULE SUBROUTINE obj_DeallocateKdtree(obj)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
    !! AbstractDomain object
  END SUBROUTINE obj_DeallocateKdtree
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-03-28
! summary: Construct an instance of domain by importing data from mesh

INTERFACE AbstractDomainImport
  MODULE SUBROUTINE obj_Import(obj, hdf5, group)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_Import
END INTERFACE AbstractDomainImport

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date:  2024-03-28
! summary:  Initiate an instance of domain by importing meshfile name from
! Toml file
!
! NOTE: default meshfile name is "mesh.h5"
! and default group in hdf5 is ""
!
! NOTE: meshfile (hdf5) is internally initiated and is deallocated
! after initiation of domain

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE obj_ImportFromToml1
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
  MODULE SUBROUTINE obj_ImportFromToml2(obj, tomlName, afile, filename, &
                                        printToml)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: tomlName
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 May 2022
! summary: Display the domain

INTERFACE AbstractDomainDisplay
  MODULE SUBROUTINE obj_Display(obj, msg, unitno)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE obj_Display
END INTERFACE AbstractDomainDisplay

!----------------------------------------------------------------------------
!                                               DisplayDomainInfo@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-15
! summary: Display the domain

INTERFACE AbstractDomainDisplayDomainInfo
  MODULE SUBROUTINE obj_DisplayDomainInfo(obj, msg, unitno)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE obj_DisplayDomainInfo
END INTERFACE AbstractDomainDisplayDomainInfo

!----------------------------------------------------------------------------
!                                                    IsInitiated@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-21
! summary:  Returns true if the domain is initiated

INTERFACE
  MODULE FUNCTION obj_IsInitiated(obj) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
  !! AbstractDomain_ object
    LOGICAL(LGT) :: ans
  END FUNCTION obj_IsInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                                                  GetMeshPointer@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This rotuine returns mesh pointer
!
!# Introduction
!
! This returns the pointer to the abtract mesh object
! - dim is the dimension of the mesh; dim=0,1,2,3 corresponds to the point,
! curve, surface, volume meshes.
! - the default value of dim is obj%nsd

INTERFACE
  MODULE FUNCTION obj_GetMeshPointer1(obj, dim, entityNum, &
                                      globalElement, isLocal) RESULT(Ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim
    !! dimension of mesh entity
    !! The default value of dim is obj%nsd
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: entityNum
    !! entity number
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: globalElement
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    CLASS(AbstractMesh_), POINTER :: ans
  END FUNCTION obj_GetMeshPointer1
END INTERFACE

!----------------------------------------------------------------------------
!                                                   IsNodePresent@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Sept 2021
! summary: Returns true if the global node number is present

INTERFACE
  MODULE FUNCTION obj_IsNodePresent(obj, globalNode, islocal) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    LOGICAL(LGT) :: ans
  END FUNCTION obj_IsNodePresent
END INTERFACE

!----------------------------------------------------------------------------
!                                               IsElementPresent@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-12
! update: 2021-11-12
! summary: Returns true if the element number is present inside the domain
!
!# Introduction
!
! globalElement, dim, entityNum
! globalElement, dim
! globalElement

INTERFACE
  MODULE FUNCTION obj_IsElementPresent(obj, globalElement, dim, entityNum, &
                                       islocal) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! Element number
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim
    !! Dimension, if dim is present then
    !! if dim=0, then search is performed in meshPoint
    !! if dim=1, then search is performed in meshCurve
    !! if dim=2, then search is performed in meshSurface
    !! if dim=3, then search is performed in meshVolume
    !! The default value of dim is obj%nsd
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: entityNum
    !! entity number
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    LOGICAL(LGT) :: ans
  END FUNCTION obj_IsElementPresent
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetConnectivity@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-12
! summary: Returns the connectivity vector of a given element number

INTERFACE
  MODULE FUNCTION obj_GetConnectivity(obj, globalElement, dim, entityNum, &
                                      islocal) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! Global element number
    !! Make sure globalElement is present
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim
    !! Dimension, if dim is present then
    !! if dim=0, then search is performed in meshPoint
    !! if dim=1, then search is performed in meshCurve
    !! if dim=2, then search is performed in meshSurface
    !! if dim=3, then search is performed in meshVolume
    !! The default value of dim is obj%nsd
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: entityNum
    !! entity number (for old style domain_)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !! vertex connectivity
  END FUNCTION obj_GetConnectivity
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetNNE@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-12
! summary: Returns the connectivity vector of a given element number

INTERFACE
  MODULE FUNCTION obj_GetNNE(obj, globalElement, dim, entityNum, islocal) &
    RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! Global element number
    !! Make sure globalElement is present
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim
    !! Dimension, if dim is present then
    !! if dim=0, then search is performed in meshPoint
    !! if dim=1, then search is performed in meshCurve
    !! if dim=2, then search is performed in meshSurface
    !! if dim=3, then search is performed in meshVolume
    !! The default value of dim is obj%nsd
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: entityNum
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    INTEGER(I4B) :: ans
    !! vertex connectivity
  END FUNCTION obj_GetNNE
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetNodeToElements@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-03-28
! summary: returns the elements connected to a node
!
!# Introduction
!
! For obj%nsd = 3, we use meshVolume
! For obj%nsd = 2, we use meshSurface
! For obj%nsd = 1, we use meshCurve
! for obj%nsd = 0, we use meshPoint

INTERFACE
  MODULE FUNCTION obj_GetNodeToElements1(obj, globalNode, islocal) &
    & RESULT(ans)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
      !! we can init the node to element data if necessary
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
  END FUNCTION obj_GetNodeToElements1
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetNodeToElements@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-03-28
! summary: returns the elements connected to a node
!
!# Introduction
!
! For obj%nsd = 3, we use meshVolume
! For obj%nsd = 2, we use meshSurface
! For obj%nsd = 1, we use meshCurve
! for obj%nsd = 0, we use meshPoint

INTERFACE
  MODULE FUNCTION obj_GetNodeToElements2(obj, globalNode, islocal) &
    & RESULT(ans)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
      !! we can init the node to element data if necessary
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
  END FUNCTION obj_GetNodeToElements2
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetNodeToElements@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-03-28
! summary: returns the elements connected to a node
!
!# Introduction
!
! For obj%nsd = 3, we use meshVolume
! For obj%nsd = 2, we use meshSurface
! For obj%nsd = 1, we use meshCurve
! for obj%nsd = 0, we use meshPoint

INTERFACE
  MODULE SUBROUTINE obj_GetNodeToElements1_(obj, ans, tsize, &
                                            globalNode, islocal)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
      !! We can init the node to element
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! node to elements, it should be atleast tsize long
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! actual size of ans, it is returned by this routine
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global node number
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! is true it means globalNode is actually local node
  END SUBROUTINE obj_GetNodeToElements1_
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetNodeToElements@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-03-28
! summary: returns the elements connected to a node
!
!# Introduction
!
! For obj%nsd = 3, we use meshVolume
! For obj%nsd = 2, we use meshSurface
! For obj%nsd = 1, we use meshCurve
! for obj%nsd = 0, we use meshPoint

INTERFACE
  MODULE SUBROUTINE obj_GetNodeToElements2_(obj, ans, tsize, &
                                            globalNode, islocal)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
      !! We can ionit the node to element data
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! node to elements, it should be atleast tsize long
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! actual size of ans, it is returned by this routine
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global node number
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! is true it means globalNode is actually local node
  END SUBROUTINE obj_GetNodeToElements2_
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetTotalNodes@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-03-28
! summary: Returns the total number of nodes in the domain
!
!# Introduction
!
! This function returns the total number of nodes in a given mesh entity
! The mesh entity is given by its ID and its dimension.
!
! - `entityNum` should not be out of bound
! - `entityNum` is currently not used
!
! Note: If both `dim` and `entityNum` is present then (in future) this
! routine will returns the total nodes in that entity only.

INTERFACE
  MODULE FUNCTION obj_GetTotalNodes(obj, dim, entityNum) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim
    !! dimension of the mesh entity
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: entityNum
    !! entity number
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalNodes
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
  MODULE FUNCTION obj_tNodes1(obj, dim) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B) :: ans
  END FUNCTION obj_tNodes1
END INTERFACE

!----------------------------------------------------------------------------
!                                                          tNodes@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 June 2021
! summary: Returns the total number of nodes in the domain

INTERFACE
  MODULE FUNCTION obj_tNodes2(obj) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_tNodes2
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
  MODULE FUNCTION obj_tNodes3(obj, opt) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: opt(2)
    !! opt(1) is dim
    !! opt(2) is entityNum
    INTEGER(I4B) :: ans
  END FUNCTION obj_tNodes3
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetTotalElements@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 28 June 2021
! summary: Returns the total number of elements in the domain
!
!# Introduction
!
! This function returns the total number of elements in mesh.
! It works as follow.
!
! - If `dim` and `entityNum` are present then it will select the
!   mesh of dim dimension and returns the total number of elements
!   in entityNum
!
! - If `dim` is not present then it will select the cellMesh and
!   returns the total number of elements in cellMesh with entityNum
!
! - If `entityNum` is not present then it will returns the total number
!   of elements in the mesh of `dim` dimension
!
! This routine does not returns the total number of elements in domain
! For that you need to call it for each dimension and sum the results.

INTERFACE
  MODULE FUNCTION obj_GetTotalElements(obj, dim, entityNum) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim
    !! dimension of mesh entities
    !!  `dim=0` denotes mesh of point entities
    !!  `dim=1` denotes mesh of curve entities
    !!  `dim=2` denotes mesh of surface entities
    !!  `dim=3` denotes mesh of volume entities
    !! If dim is not present then sum of obj%tElements is returned
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: entitynum
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalElements
END INTERFACE

!----------------------------------------------------------------------------
!                                                      tElements@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-13
! summary: Returns total elements in domain

INTERFACE
  MODULE FUNCTION obj_tElements1(obj) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_tElements1
END INTERFACE

!----------------------------------------------------------------------------
!                                                      tElements@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-13
! summary: Returns total elements in given dimension

INTERFACE
  MODULE FUNCTION obj_tElements2(obj, dim) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B) :: ans
  END FUNCTION obj_tElements2
END INTERFACE

!----------------------------------------------------------------------------
!                                                      tElements@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-13
! update: 2021-11-13
! summary: Returns the total elements in a given mesh

INTERFACE
  MODULE FUNCTION obj_tElements3(obj, opt) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: opt(2)
    INTEGER(I4B) :: ans
  END FUNCTION obj_tElements3
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetLocalNodeNumber@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Sept 2021
! summary: Returns local node number of a global node number

INTERFACE
  MODULE FUNCTION obj_GetLocalNodeNumber1(obj, globalNode, islocal) &
    RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! Global node number in mesh of obj%nsd dimension
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    INTEGER(I4B) :: ans
    !! Local node number in mesh of obj%nsd dimension
  END FUNCTION obj_GetLocalNodeNumber1
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetLocalNodeNumber@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Sept 2021
! summary: Returns local node number of a global node number

INTERFACE
  MODULE FUNCTION obj_GetLocalNodeNumber2(obj, globalNode, islocal) &
    RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    INTEGER(I4B) :: ans(SIZE(globalNode))
  END FUNCTION obj_GetLocalNodeNumber2
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetLocalElemNumber@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: This function returns the local element number

INTERFACE
  MODULE FUNCTION obj_GetLocalElemNumber1(obj, globalElement, islocal) &
    RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    INTEGER(I4B) :: ans(SIZE(globalElement))
  END FUNCTION obj_GetLocalElemNumber1
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetLocalElemNumber@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: This function returns the local element number

INTERFACE
  MODULE FUNCTION obj_GetLocalElemNumber2(obj, globalElement, islocal) &
    RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetLocalElemNumber2
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetElemDataPointer@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-04-18
! summary:  Get teh element data (hardcopoy)

INTERFACE
  MODULE SUBROUTINE obj_GetElemData(obj, elemdata, globalElement, islocal)
    CLASS(AbstractDomain_), INTENT(in) :: obj
    TYPE(ElemData_), INTENT(INOUT) :: elemdata
    INTEGER(I4B), INTENT(IN) :: globalElement
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
  END SUBROUTINE obj_GetElemData
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetElemDataPointer@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-04-18
! summary:  Get teh element data (hardcopoy)

INTERFACE
  MODULE FUNCTION obj_GetElemDataPointer(obj, globalElement, islocal) &
    RESULT(ans)
    CLASS(AbstractDomain_), TARGET, INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    TYPE(ElemData_), POINTER :: ans
  END FUNCTION obj_GetElemDataPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetGlobalNodeNumber@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Sept 2021
! summary: Returns local node number of a global node number
!
!# Introduction
!
! Note this function should be pure because we use it in doconcurrent

INTERFACE
  MODULE PURE FUNCTION obj_GetGlobalNodeNumber1(obj, localNode) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: localNode
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetGlobalNodeNumber1
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetGlobalNodeNumber@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Sept 2021
! summary: Returns local node number of a global node number

INTERFACE
  MODULE PURE FUNCTION obj_GetGlobalNodeNumber2(obj, localNode) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: localNode(:)
    INTEGER(I4B) :: ans(SIZE(localNode))
  END FUNCTION obj_GetGlobalNodeNumber2
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetTotalEntities@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Sept 2021
! summary: This function returns the total number of entities
!
!# Introduction
!
! This function returns the total number of mesh
!
! - `dim=0` returns the total number of mesh of point entities
! - `dim=1` returns the total number of mesh of curve entities
! - `dim=2` returns the total number of mesh of surface entities
! - `dim=3` returns the total number of mesh of volume entities
!
! If globalElement and islocal are present
!
! ans(1) =  total number of nodes in element
! ans(2) =  total number of edges in element
! ans(3) =  total number of faces in element
! ans(4) = 1

INTERFACE
  MODULE FUNCTION obj_GetTotalEntities(obj, dim) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim
    !! dimension of mesh entities
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalEntities
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetTotalEntitiesList@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-23
! summary: Get total number of entities in a given element

INTERFACE
  MODULE FUNCTION obj_GetTotalEntitiesList(obj, globalElement, islocal) &
    RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! Global element number
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! If islocal is true then globalElement is local element
    INTEGER(I4B) :: ans(4)
  END FUNCTION obj_GetTotalEntitiesList
END INTERFACE

!----------------------------------------------------------------------------
!                                                 getDimEntityNum@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-11-12
! update: 2021-11-12
! summary: Returns dim and entity number

INTERFACE
  MODULE FUNCTION obj_GetDimEntityNum(obj, globalElement, islocal) &
    RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    INTEGER(I4B) :: ans(2)
  END FUNCTION obj_GetDimEntityNum
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetNodeCoord@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine returns the nodal coordinates
!
!# Introduction
! - This routine returns the nodal coordinates in the form of rank2 array.
! - The nodal coordinates are in XiJ, the columns of XiJ denotes the node
! number, and the rows correspond to the component.

INTERFACE
  MODULE SUBROUTINE obj_GetNodeCoord1(obj, nodeCoord, dim, entityNum)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: nodeCoord(:, :)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: entityNum
  END SUBROUTINE obj_GetNodeCoord1
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetNodeCoord@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: This routine returns the nodal coordinates
!
!# Introduction
! - This routine returns the nodal coordinates in the form of rank2 array.
! - The nodal coordinates are in XiJ, the columns of XiJ denotes the node
! number, and the rows correspond to the component.

INTERFACE
 MODULE SUBROUTINE obj_GetNodeCoord2(obj, nodeCoord, nrow, ncol, globalNode, &
                                      islocal)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: nodeCoord(:, :)
    !! It should be allocated by the user.
    !! SIZE(nodeCoord, 1) should be atleast obj%nsd
    !! Size(nodeCoord, 2) is equal to the size(globalNode)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written in nodecoord
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global node numbers (pointer to nodeCoord)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! if islocal is true then we do not find local node nubmers
    !! in this case globalNode implies local node
  END SUBROUTINE obj_GetNodeCoord2
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetNodeCoord@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-11
! summary: This routine returns the nodal coordinates
!
!# Introduction
! - This routine returns the nodal coordinates
! - globalNode is global node (pointer to nodeCoord)
! - if islocal is true then globalNode is local node

INTERFACE
  MODULE SUBROUTINE obj_GetNodeCoord3(obj, nodeCoord, tsize, globalNode, &
                                      islocal)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: nodeCoord(:)
    !! It should be allocated by the user.
    !! SIZE(nodeCoord, 1) should be atleast nsd
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of data written in nodeCoord
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! globalNode number
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! if true then globalnode above is local node
  END SUBROUTINE obj_GetNodeCoord3
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetNodeCoordPointer@GetMethods
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
  MODULE FUNCTION obj_GetNodeCoordPointer(obj) RESULT(ans)
    CLASS(AbstractDomain_), TARGET, INTENT(IN) :: obj
    REAL(DFP), POINTER :: ans(:, :)
  END FUNCTION obj_GetNodeCoordPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                                  GetNearestNode@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-04-11
! summary:  Get nearest node

INTERFACE
  MODULE SUBROUTINE obj_GetNearestNode1(obj, qv, x, globalNode)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: qv(:)
    !! Query vector
    REAL(DFP), INTENT(INOUT) :: x(:)
    !! node coord of nearest node
    INTEGER(I4B), INTENT(OUT) :: globalNode
    !! globalNode number
  END SUBROUTINE obj_GetNearestNode1
END INTERFACE

!----------------------------------------------------------------------------
!                                                  GetNearestNode@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-04-11
! summary:  Get nearest node

INTERFACE
  MODULE SUBROUTINE obj_GetNearestNode2(obj, qv, x, globalNode, nn)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: qv(:)
    !! Query vector
    REAL(DFP), INTENT(INOUT) :: x(:, :)
    !! node coord of nearest node
    !! the size(x, 2) should be atleast nn
    INTEGER(I4B), INTENT(INOUT) :: globalNode(:)
    !! globalNode number, size of globalNode should be atleast nn
    INTEGER(I4B), INTENT(IN) :: nn
    !! number of nearest points
  END SUBROUTINE obj_GetNearestNode2
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
  MODULE FUNCTION obj_GetNptrs(obj, dim, entityNum) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: entityNum(:)
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_GetNptrs
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetNptrs@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-17
! summary: this routine returns the global node number
!
!# Introduction
! This routine returns the global node number
! xidim is the dimension of the mesh

INTERFACE
  MODULE SUBROUTINE obj_GetNptrs_(obj, nptrs, dim, entityNum, tsize)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(INOUT) :: nptrs(:)
    INTEGER(I4B), INTENT(IN) :: dim
    !! dim = 0 meshPoint is called
    !! dim=1 meshCurve is called
    !! dim=2, meshSurface is called
    !! dim=~3, meshVolume is called
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: entityNum(:)
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: tsize
    !! Returns the size of nptrs where data has been written
  END SUBROUTINE obj_GetNptrs_
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetInternalNptrs@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2 Sept 2021
! summary: this routine returns the global node number
!
!# Introduction
! This routine returns the global node number
! xidim is the dimension of the mesh

INTERFACE
  MODULE FUNCTION obj_GetInternalNptrs(obj, dim, entityNum) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: entityNum(:)
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_GetInternalNptrs
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetNptrs@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2 Sept 2021
! summary: this routine returns the global node number in a box

INTERFACE
  MODULE SUBROUTINE obj_GetNptrsInBox(obj, box, nptrs, isStrict)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
      !! If Kdtree is not init then we init it
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: nptrs(:)
    TYPE(BoundingBox_), INTENT(IN) :: box
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isStrict
    !! Default is true
    !! If it is true the returned points are strictly inside or on the
    !! box, but not outside of it
    !! This is because we use radius of bounding box to find the points
    !! this is over estimation.
  END SUBROUTINE obj_GetNptrsInBox
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetNptrs@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2 Sept 2021
! summary: this routine returns the global node number in a box

INTERFACE
  MODULE SUBROUTINE obj_GetNptrsInBox_(obj, box, nptrs, tnodes, isStrict)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
      !! If Kdtree is not init then we init it
    TYPE(BoundingBox_), INTENT(IN) :: box
    INTEGER(I4B), INTENT(INOUT) :: nptrs(:)
    !! it should allocated, size of nptrs should be .ge. tnodes
    INTEGER(I4B), INTENT(INOUT) :: tnodes
    !! total nodes found
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isStrict
    !! Default is true
    !! If it is true the returned points are strictly inside or on the
    !! box, but not outside of it
    !! This is because we use radius of bounding box to find the points
    !! this is over estimation.
  END SUBROUTINE obj_GetNptrsInBox_
END INTERFACE

!----------------------------------------------------------------------------
!                                                  GetBoundingBox@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-17
! summary: Returns bounding box

INTERFACE
  MODULE FUNCTION obj_GetBoundingBox(obj, dim, entityNum) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim
    !! dimension of the mesh
    !! if dim is not present then nodeCoord in domain is
    !! used for computing the bounding box
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: entityNum
    TYPE(BoundingBox_) :: ans
  END FUNCTION obj_GetBoundingBox
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetNSD@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Sept 2021
! summary: This routine returns the number of spatial dimensions

INTERFACE
  MODULE FUNCTION obj_GetNSD(obj) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetNSD
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetOrder@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 21 Sept 2021
! summary: This routine returns the order of meshes of dimensions=dim

INTERFACE
  MODULE FUNCTION obj_GetOrder(obj, dim) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_GetOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                          GetTotalMeshFacetData@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 May 2022
! summary: returns size of meshFacetData

INTERFACE
  MODULE FUNCTION obj_GetTotalMeshFacetData(obj, imeshFacetData) &
    RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: imeshFacetData
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalMeshFacetData
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetTotalMaterial@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-12-09
! update: 2021-12-09
! summary: Returns the materials id of a given medium

INTERFACE
  MODULE FUNCTION obj_GetTotalMaterial(obj, dim, globalElement, &
                                       islocal, entityNum) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    !! which dimension of the mesh we should search
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! is globalElement a local one
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: entityNum
    !! This is used for backward compatibility, default is 1
    INTEGER(I4B) :: ans
    !! returns the total materials in the element
  END FUNCTION obj_GetTotalMaterial
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetElemType@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-23
! summary:  Returns the element type of each mesh in domain

INTERFACE
  MODULE FUNCTION obj_GetElemType(obj, dim) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_GetElemType
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetUniqueElemType@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-23
! summary: Returns only the unique elements in the meshes of domain

INTERFACE
  MODULE FUNCTION obj_GetUniqueElemType(obj, dim) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_GetUniqueElemType
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetMaxNodeNumber@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-04-15
! summary:  Returns obj%isInit

INTERFACE
  MODULE FUNCTION obj_GetMaxNodeNumber(obj) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetMaxNodeNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetMinNodeNumber@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-04-15
! summary:  Returns obj%isInit

INTERFACE
  MODULE FUNCTION obj_GetMinNodeNumber(obj) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetMinNodeNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetMaxElemNumber@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-04-15
! summary:  Returns obj%isInit

INTERFACE
  MODULE FUNCTION obj_GetMaxElemNumber(obj) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetMaxElemNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetMinElemNumber@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-04-15
! summary:  Returns obj%isInit

INTERFACE
  MODULE FUNCTION obj_GetMinElemNumber(obj) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetMinElemNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetParam@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-04-08
! summary:  Get the PARAMETER

INTERFACE
  MODULE SUBROUTINE obj_GetParam(obj, isInitiated, engine, majorVersion, &
 minorVersion, version, nsd, maxNptrs, minNptrs, tNodes, isNodeNumberSparse, &
              maxElemNum, minElemNum, isElemNumberSparse, tEntitiesForNodes, &
                        tEntitiesForElements, tElements, tEntities, nodeCoord)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isInitiated
    CHARACTER(*), OPTIONAL, INTENT(INOUT) :: engine
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: majorVersion
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: minorVersion
    REAL(DFP), OPTIONAL, INTENT(OUT) :: version
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: nsd
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: maxNptrs
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: minNptrs
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: tNodes
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isNodeNumberSparse
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: maxElemNum
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: minElemNum
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isElemNumberSparse
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: tEntitiesForNodes
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: tEntitiesForElements
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: tElements(0:3)
    INTEGER(I4B), OPTIONAL, INTENT(INOUT) :: tEntities(0:3)
    REAL(DFP), OPTIONAL, INTENT(INOUT) :: nodeCoord(:, :)
  END SUBROUTINE obj_GetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetGlobalEdgeNumber@GetMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION obj_GetGlobalEdgeNumber(obj, globalElement, localEdgeNumber, &
                                          islocal) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! local or global element number
    INTEGER(I4B), INTENT(IN) :: localEdgeNumber
    !! local Edge number in global element
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! if true then global element is local element
    INTEGER(I4B) :: ans
    !! global Edge number
  END FUNCTION obj_GetGlobalEdgeNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetGlobalFaceNumber@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-21
! summary:  Get global face number from globalElement and local face number

INTERFACE
MODULE FUNCTION obj_GetGlobalFaceNumber(obj, globalElement, localFaceNumber, &
                                          islocal) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! local or global element number
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! local face number in global element
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! if true then global element is local element
    INTEGER(I4B) :: ans
    !! global face number
  END FUNCTION obj_GetGlobalFaceNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetConnectivity@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-12
! summary: Returns the connectivity vector of a given element number

INTERFACE
  MODULE SUBROUTINE obj_GetConnectivity1_(obj, globalElement, ans, tsize, &
                                          opt, dim, entityNum, islocal)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !!
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! Global element number
    !! Make sure globalElement is present
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! vertex connectivity
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! total size
    CHARACTER(*), OPTIONAL, INTENT(IN) :: opt
    !! Vertex, Edge, Face, Cell
    !! Default is Vertex
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim
    !! Dimension, if dim is present then
    !! if dim=0, then search is performed in meshPoint
    !! if dim=1, then search is performed in meshCurve
    !! if dim=2, then search is performed in meshSurface
    !! if dim=3, then search is performed in meshVolume
    !! The default value of dim is obj%nsd
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: entityNum
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
  END SUBROUTINE obj_GetConnectivity1_
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetConnectivity@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-14
! summary:  Get connectivity

INTERFACE
  MODULE SUBROUTINE obj_GetConnectivity2_(obj, cellCon, faceCon, edgeCon, &
                                          nodeCon, tCellCon, tFaceCon, &
                                          tEdgeCon, tNodeCon, globalElement, &
                                          dim, entityNum, islocal)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(INOUT) :: cellCon(:)
    !! cell connectivity of element
    INTEGER(I4B), INTENT(INOUT) :: faceCon(:)
    !! face connectivity of element
    INTEGER(I4B), INTENT(INOUT) :: edgeCon(:)
    !! edge connectivity of element
    INTEGER(I4B), INTENT(INOUT) :: nodeCon(:)
    !! node connectivity of element
    INTEGER(I4B), INTENT(OUT) :: tCellCon
    !! size of data written in cellCon
    INTEGER(I4B), INTENT(OUT) :: tFaceCon
    !! size of data written in faceCon
    INTEGER(I4B), INTENT(OUT) :: tEdgeCon
    !! size of data written in edgecon
    INTEGER(I4B), INTENT(OUT) :: tnodeCon
    !! size of data written in nodecon
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global or local element number
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim
    !! Dimension, if dim is present then
    !! if dim=0, then search is performed in meshPoint
    !! if dim=1, then search is performed in meshCurve
    !! if dim=2, then search is performed in meshSurface
    !! if dim=3, then search is performed in meshVolume
    !! The default value of dim is obj%nsd
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: entityNum
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! if true then global element is local element
  END SUBROUTINE obj_GetConnectivity2_
END INTERFACE

!----------------------------------------------------------------------------
!                                         GetTotalVertexNodes@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2025-06-05
! summary: Returns total number of vertex nodes in the mesh
!
!# Introduction
!
! This function can perform several tasks as described below:
!
! - If `globalElement` is present then it returns the total number of vertex
! nodes in the element. In this case if `islocal` is true then
! `globalElement` is a local element number.
!
! - If `globalElement` is not present then it returns the total number of
!   vertex nodes in the mesh of dimension `dim`.
!   In this case if `entityNum` is present then it returns the total number
!   of vertex nodes in the mesh of dimension `dim` and entity number
!  `entityNum`.

INTERFACE
  MODULE FUNCTION obj_GetTotalVertexNodes1(obj, globalElement, dim, &
                                           entityNum, islocal) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: globalElement
    !! Global element number
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim
    !! Dimension of the mesh
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: entityNum
    !! Entity number of the mesh
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! If true then global element is a local element
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalVertexNodes1
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetTotalVertexNodes@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-05
! summary:  Get total vertex nodes in a collection of elements
!
!# Introduction
!
! We use dim to get the mesh of dimension.
! Then we call GetTotalVertexNodes on that mesh

INTERFACE
  MODULE FUNCTION obj_GetTotalVertexNodes2(obj, globalElement, dim, &
                                           entityNum, islocal) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! abstrract mesh
    INTEGER(I4B), INTENT(IN) :: globalElement(:)
    !! global or local element number
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim
    !! Dimension of the mesh
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: entityNum
    !! Entity number of the mesh
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! if true then global element is local element
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalVertexNodes2
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetOrientation@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-07-14
! summary:  Get the orientation flags of an element

INTERFACE
  MODULE SUBROUTINE obj_GetOrientation(obj, cellOrient, faceOrient, &
                                       edgeOrient, tCellOrient, tFaceOrient, &
                                       tEdgeOrient, globalElement, dim, &
                                       entityNum, islocal)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(INOUT) :: cellOrient(:)
    !! cell connectivity of element
    INTEGER(I4B), INTENT(INOUT) :: faceOrient(:, :)
    !! face connectivity of element
    INTEGER(I4B), INTENT(INOUT) :: edgeOrient(:)
    !! edge connectivity of element
    INTEGER(I4B), INTENT(OUT) :: tCellOrient
    !! size of data written in cellCon
    INTEGER(I4B), INTENT(OUT) :: tFaceOrient(2)
    !! size of data written in faceCon
    !! tFaceOrient(1) is the number of rows in faceOrient
    !! tFaceOrient(2) is the number of columns in faceOrient
    INTEGER(I4B), INTENT(OUT) :: tEdgeOrient
    !! size of data written in edgecon
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global or local element number
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim
    !! Dimension of the mesh
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: entityNum
    !! Entity number of the mesh
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! if true then global element is local element
  END SUBROUTINE obj_GetOrientation
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetElemTopoIndx@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-23
! summary:  Get the element topology index

INTERFACE
  MODULE FUNCTION obj_GetElemTopologyIndx(obj, globalElement, dim, &
                                          entityNum, islocal) &
    RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global or local element number
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim
    !! Dimension of the mesh
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: entityNum
    !! Entity number of the mesh
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! if true then global element is local element
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetElemTopologyIndx
END INTERFACE

!----------------------------------------------------------------------------
!                                                   SetShowTime@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-10
! summary:  Set the showTime

INTERFACE
  MODULE SUBROUTINE obj_SetShowTime(obj, VALUE)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
    LOGICAL(LGT), INTENT(IN) :: VALUE
  END SUBROUTINE obj_SetShowTime
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetSparsity@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-03-31
! summary: Set sparsity in [[CSRMatrix_]] from [[AbstractDomain_]]

INTERFACE
  MODULE SUBROUTINE obj_SetSparsity1(obj, mat)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
    TYPE(CSRMatrix_), INTENT(INOUT) :: mat
  END SUBROUTINE obj_SetSparsity1
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetSparsity@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Oct 2021
! summary: Set sparsity in [[CSRMatrix_]] from [[AbstractDomain_]]

INTERFACE AbstractDomainSetSparsity
  MODULE SUBROUTINE obj_SetSparsity2(domains, mat)
    CLASS(AbstractDomainPointer_), INTENT(INOUT) :: domains(:)
    TYPE(CSRMatrix_), INTENT(INOUT) :: mat
  END SUBROUTINE obj_SetSparsity2
END INTERFACE AbstractDomainSetSparsity

!----------------------------------------------------------------------------
!                                               SetTotalMaterial@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:  2024-04-17
! summary:

INTERFACE
  MODULE SUBROUTINE obj_SetTotalMaterial(obj, dim, n, entityNum)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    !! dimension of the mesh to select from
    INTEGER(I4B), INTENT(IN) :: n
    !! Total number of materials
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: entityNum
    !! entity number of given dimension
  END SUBROUTINE obj_SetTotalMaterial
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetMaterial@setMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-12-09
! update: 2021-12-09
! summary: Set the materials id of a given medium

INTERFACE
  MODULE SUBROUTINE obj_SetMaterial(obj, dim, entityNum, &
    & medium, material)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: dim
    !! dimension of the mesh
    INTEGER(I4B), INTENT(IN) :: entityNum
    !! entity number
    INTEGER(I4B), INTENT(IN) :: medium
    !! medium number (like soil, water)
    INTEGER(I4B), INTENT(IN) :: material
    !! type of medium like clay, sand, water1, water2
  END SUBROUTINE obj_SetMaterial
END INTERFACE

!----------------------------------------------------------------------------
!                                                   SetNodeCoord@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-02-24
! summary: Set the node coordinate of the domain

INTERFACE
  MODULE SUBROUTINE obj_SetNodeCoord(obj, nodeCoord, scale, &
    & addContribution)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: nodeCoord(:, :)
    !! nodal coordinate in xij Format
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_SetNodeCoord
END INTERFACE

!----------------------------------------------------------------------------
!                                                   SetQuality@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_SetQuality(obj, measures, max_measures, &
                                   min_measures, dim, entityNum)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: measures(:)
    REAL(DFP), INTENT(OUT) :: max_measures(:)
    REAL(DFP), INTENT(OUT) :: min_measures(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: entityNum
  END SUBROUTINE obj_SetQuality
END INTERFACE

!----------------------------------------------------------------------------
!                                               SetTotalElements@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-11
! summary:  Set an entry in obj%tElements

INTERFACE
  MODULE SUBROUTINE obj_SetTotalElements(obj, indx, VALUE)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: indx
    !! indx is the index of tElements, it should be 0, 1, 2, or 3
    !! 0 for point
    !! 1 for curve
    !! 2 for surface
    !! 3 for volume
    INTEGER(I4B), INTENT(IN) :: VALUE
    !! value is the total number of elements in given dimension
  END SUBROUTINE obj_SetTotalElements
END INTERFACE

!----------------------------------------------------------------------------
!                                           InitiateKdtree@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-04-10
! summary: Initiate the kd tree

INTERFACE
  MODULE SUBROUTINE obj_InitiateKdtree(obj)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_InitiateKdtree
END INTERFACE

!----------------------------------------------------------------------------
!                                     InitiateNodeToElements@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 Nov 2022
! summary: This routine sets the node-to-elements data in mesh of domain

INTERFACE
  MODULE SUBROUTINE obj_InitiateNodeToElements(obj)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_InitiateNodeToElements
END INTERFACE

!----------------------------------------------------------------------------
!                                       InitiateNodeToNodes@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 Nov 2022
! summary: This routine sets the node-to-nodes data in mesh of domain

INTERFACE
  MODULE SUBROUTINE obj_InitiateNodeToNodes(obj)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_InitiateNodeToNodes
END INTERFACE

!----------------------------------------------------------------------------
!                                  InitiateElementToElements@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 Nov 2022
! summary: This routine sets the element-to-element data in mesh of domain

INTERFACE
  MODULE SUBROUTINE obj_InitiateElementToElements(obj)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_InitiateElementToElements
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
  MODULE SUBROUTINE obj_InitiateBoundaryData(obj)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_InitiateBoundaryData
END INTERFACE

!----------------------------------------------------------------------------
!                                      InitiateFacetElements@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 Nov 2022
! summary: This routine sets the facet elements data in mesh of domain

INTERFACE
  MODULE SUBROUTINE obj_InitiateFacetElements(obj)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_InitiateFacetElements
END INTERFACE

!----------------------------------------------------------------------------
!                                  InitiateExtraNodeToNodes@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 Nov 2022
! summary: This routine sets the node-to-nodes data in mesh of domain

INTERFACE
  MODULE SUBROUTINE obj_InitiateExtraNodeToNodes(obj)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_InitiateExtraNodeToNodes
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
! In this way mesh-boundary-element, which are not domain-boundary-element
! can be treated as the interface element between two meshes.
!
! This methods needs following information:
!
!- boundary element data should be initiated for each mesh, this means
! a call to InitiateBoundaryElementData is necessary

INTERFACE
  MODULE SUBROUTINE obj_SetFacetElementType(obj)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_SetFacetElementType
END INTERFACE

!----------------------------------------------------------------------------
!                              SetAbstractDomainFacetElement@MeshDataMethods
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
  MODULE SUBROUTINE obj_SetDomainFacetElement(obj)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_SetDomainFacetElement
END INTERFACE

!----------------------------------------------------------------------------
!                                                 SetMeshmap@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 May 2022
! summary: This routine sets meshMap

INTERFACE
  MODULE SUBROUTINE obj_SetMeshmap(obj)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_SetMeshmap
END INTERFACE

!----------------------------------------------------------------------------
!                                        SetMeshFacetElement@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 May 2022
! summary: This routine sets meshFacetData

INTERFACE
  MODULE SUBROUTINE obj_SetMeshFacetElement(obj)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_SetMeshFacetElement
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE AbstractDomain_Class
