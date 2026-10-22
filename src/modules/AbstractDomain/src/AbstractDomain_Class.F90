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

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-18
! summary: Abstract domain class
!
! This module defines AbstractDomain_, the abstract class for domains, and
! AbstractDomainPointer_.

MODULE AbstractDomain_Class
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE BaseType, ONLY: CSRSparsity_
USE BaseType, ONLY: CSRMatrix_
USE BaseType, ONLY: BoundingBox_
USE ElemData_Class, ONLY: ElemData_
USE ExceptionHandler_Class, ONLY: e
USE GlobalData, ONLY: DFP, I4B, LGT
USE HDF5File_Class, ONLY: HDF5File_
USE Kdtree2_Module, ONLY: Kdtree2_
USE Kdtree2_Module, ONLY: Kdtree2Result_
USE String_Class, ONLY: String
USE tomlf, ONLY: toml_table
USE TxtFile_Class, ONLY: TxtFile_
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

!----------------------------------------------------------------------------
!                                                            AbstractDomain_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-31
! summary: Abstract class for domains
!
! AbstractDomain_ holds the meta data and the node coordinates of a domain
! read from a HDF5 mesh file. The child class (FEDomain_) holds the meshes of
! the domain: one mesh for each dimension up to nsd (points, curves, surfaces,
! volumes).
!
! - nodeCoord(:, :) has nsd rows and one column per node.
! - tElements(0:3) and tEntities(0:3) are the number of elements and entities
!   of each dimension.
! - Most methods take dim (default nsd) to select a mesh, and work on that
!   mesh.
! - kdtree is a kdtree of the node coordinates, used by GetNearestNode and
!   GetNptrsInBox.
!
! See also: [[obj_Initiate]]

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
  !! Columns are in the order of the nodes in the mesh file

  TYPE(Kdtree2_), POINTER :: kdtree => NULL()
  !! kdtree of the node coordinates
  TYPE(Kdtree2Result_), ALLOCATABLE :: kdresult(:)
  !! results of kdtree searches

CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods

  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => obj_Initiate
  !! Initiate the domain from a HDF5 mesh file

  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Deallocate the data of the domain

  PROCEDURE, PUBLIC, PASS(obj) :: DeallocateKdtree => obj_DeallocateKdtree
  !! Deallocate the kdtree of the domain

  ! IO:
  ! @IOMethods

  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  !! Import the meta data of the domain
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: ImportFromToml1 => &
    obj_ImportFromToml1
  !! Import the domain from a toml table
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: ImportFromToml2 => &
    obj_ImportFromToml2
  !! Import the domain from a toml file
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, &
    ImportFromToml2
  !! Import the domain from a toml file or table
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the content of the domain
  PROCEDURE, PUBLIC, PASS(obj) :: DisplayDomainInfo => &
    obj_DisplayDomainInfo
  !! Display the meta data of the domain

  ! GET:
  ! @GetMethods

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: IsInitiated => &
    obj_IsInitiated
  !! Returns true if the domain is initiated
  PROCEDURE(obj_GetMeshPointer), DEFERRED, PUBLIC, PASS(obj) :: GetMeshPointer
  !! Get a pointer to a mesh of the domain

  PROCEDURE, PUBLIC, PASS(obj) :: IsNodePresent => obj_IsNodePresent
  !! Returns true if a node is present in the domain

  PROCEDURE, PUBLIC, PASS(obj) :: IsElementPresent => obj_IsElementPresent
  !! Returns true if an element is present

  PROCEDURE, PUBLIC, PASS(obj) :: GetConnectivity => obj_GetConnectivity
  !! Get the vertex connectivity of an element

  PROCEDURE, PUBLIC, PASS(obj) :: GetNNE => obj_GetNNE
  !! Get the number of nodes in an element

  PROCEDURE, PASS(obj) :: GetNodeToElements1 => obj_GetNodeToElements1
  !! Get the elements around a node
  PROCEDURE, PASS(obj) :: GetNodeToElements2 => obj_GetNodeToElements2
  !! Get the elements around a list of nodes
  GENERIC, PUBLIC :: GetNodeToElements => &
    GetNodeToElements1, GetNodeToElements2
  !! Get the elements around nodes

  PROCEDURE, PASS(obj) :: GetNodeToElements1_ => obj_GetNodeToElements1_
  !! Get the elements around a node without allocation
  PROCEDURE, PASS(obj) :: GetNodeToElements2_ => obj_GetNodeToElements2_
  !! Get the elements around nodes without allocation
  GENERIC, PUBLIC :: GetNodeToElements_ => &
    GetNodeToElements1_, &
    GetNodeToElements2_
  !! Get the elements around nodes without allocation

  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalNodes => obj_GetTotalNodes
  !! Get the total number of nodes
  PROCEDURE, PASS(obj) :: obj_tNodes1
  !! Number of nodes of a mesh
  PROCEDURE, PASS(obj) :: obj_tNodes2
  !! Number of nodes of the domain
  PROCEDURE, PASS(obj) :: obj_tNodes3
  !! Number of nodes of a mesh (dim, entityNum)
  GENERIC, PUBLIC :: OPERATOR(.tNodes.) => &
    obj_tNodes1, obj_tNodes2, obj_tNodes3
  !! Get the number of nodes

  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalElements => obj_GetTotalElements
  !! Get the total number of elements
  PROCEDURE, PASS(obj) :: obj_tElements1
  !! Number of elements of the nsd mesh
  PROCEDURE, PASS(obj) :: obj_tElements2
  !! Number of elements of a mesh
  PROCEDURE, PASS(obj) :: obj_tElements3
  !! Number of elements of an entity
  GENERIC, PUBLIC :: OPERATOR(.tElements.) => obj_tElements1, &
    obj_tElements2, obj_tElements3
  !! Get the number of elements

  PROCEDURE, PASS(obj) :: GetLocalNodeNumber1 => obj_GetLocalNodeNumber1
  !! Get the local number of a global node
  PROCEDURE, PASS(obj) :: GetLocalNodeNumber2 => obj_GetLocalNodeNumber2
  !! Get the local numbers of global nodes
  GENERIC, PUBLIC :: GetLocalNodeNumber => GetLocalNodeNumber1, &
    GetLocalNodeNumber2
  !! Get local node numbers

  PROCEDURE, PASS(obj) :: GetLocalElemNumber1 => obj_GetLocalElemNumber1
  !! Get the local numbers of global elements
  PROCEDURE, PASS(obj) :: GetLocalElemNumber2 => obj_GetLocalElemNumber2
  !! Get the local number of a global element
  GENERIC, PUBLIC :: GetLocalElemNumber => GetLocalElemNumber1, &
    GetLocalElemNumber2
  !! Get local element numbers

  PROCEDURE, PUBLIC, PASS(obj) :: GetElemDataPointer => &
    obj_GetElemDataPointer
  !! Get a pointer to the element data

  PROCEDURE, PUBLIC, PASS(obj) :: GetElemData => obj_GetElemData
  !! Get a copy of the element data

  PROCEDURE, PASS(obj) :: GetGlobalNodeNumber1 => obj_GetGlobalNodeNumber1
  !! Get the global number of a local node
  PROCEDURE, PASS(obj) :: GetGlobalNodeNumber2 => obj_GetGlobalNodeNumber2
  !! Get the global numbers of local nodes
  GENERIC, PUBLIC :: GetGlobalNodeNumber => &
    GetGlobalNodeNumber1, &
    GetGlobalNodeNumber2
  !! Get global node numbers

  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalEntities => obj_GetTotalEntities
  !! Get the number of entities of a dimension
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalEntitiesList => &
    obj_GetTotalEntitiesList
  !! Get the number of entities of an element
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalMesh => obj_GetTotalEntities
  !! Same as GetTotalEntities (will be removed)

  PROCEDURE, PUBLIC, PASS(obj) :: GetDimEntityNum => obj_GetDimEntityNum
  !! Get the dimension and entity of an element

  PROCEDURE, PASS(obj) :: GetNodeCoord1 => obj_GetNodeCoord1
  !! Get node coordinates
  PROCEDURE, PASS(obj) :: GetNodeCoord2 => obj_GetNodeCoord2
  !! Get the coordinates of a list of nodes
  PROCEDURE, PASS(obj) :: GetNodeCoord3 => obj_GetNodeCoord3
  !! Get the coordinates of a node
  GENERIC, PUBLIC :: GetNodeCoord => GetNodeCoord1, GetNodeCoord2, &
    GetNodeCoord3
  !! Get node coordinates

  PROCEDURE, PUBLIC, PASS(obj) :: GetNodeCoordPointer => &
    obj_GetNodeCoordPointer
  !! Get a pointer to the node coordinates

  PROCEDURE, PUBLIC, PASS(obj) :: GetNearestNode1 => obj_GetNearestNode1
  !! Get the node nearest to a point
  PROCEDURE, PUBLIC, PASS(obj) :: GetNearestNode2 => obj_GetNearestNode2
  !! Get the nn nodes nearest to a point
  GENERIC, PUBLIC :: GetNearestNode => GetNearestNode1, GetNearestNode2
  !! Get the nearest nodes to a point

  PROCEDURE, PUBLIC, PASS(obj) :: GetNptrs => obj_GetNptrs
  !! Get the global node numbers of a mesh

  PROCEDURE, PUBLIC, PASS(obj) :: GetNptrs_ => obj_GetNptrs_
  !! Get the global node numbers of a mesh

  PROCEDURE, PUBLIC, PASS(obj) :: GetInternalNptrs => &
    obj_GetInternalNptrs
  !! Get the internal nodes of a mesh

  PROCEDURE, PUBLIC, PASS(obj) :: GetNptrsInBox => obj_GetNptrsInBox
  !! Get the nodes of the domain in a box

  PROCEDURE, PUBLIC, PASS(obj) :: GetNptrsInBox_ => obj_GetNptrsInBox_
  !! Get the nodes in a box without allocation

  PROCEDURE, PUBLIC, PASS(obj) :: GetBoundingBox => obj_GetBoundingBox
  !! Get the bounding box

  PROCEDURE, PUBLIC, PASS(obj) :: GetNSD => obj_GetNSD
  !! Get the number of spatial dimensions

  PROCEDURE, PUBLIC, PASS(obj) :: GetOrder => obj_GetOrder
  !! Get the order of the meshes of a dimension

  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalMeshFacetData => &
    obj_GetTotalMeshFacetData
  !! Get the size of mesh facet data

  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalMaterial => obj_GetTotalMaterial
  !! Get the number of media of an element

  PROCEDURE, PUBLIC, PASS(obj) :: GetElemType => obj_GetElemType
  !! Get the element types of a mesh

  PROCEDURE, PUBLIC, PASS(obj) :: GetUniqueElemType => &
    obj_GetUniqueElemType
  !! Get the unique element types of a mesh

  PROCEDURE, PUBLIC, PASS(obj) :: GetMaxNodeNumber => obj_GetMaxNodeNumber
  !! Get the maximum node number

  PROCEDURE, PUBLIC, PASS(obj) :: GetMinNodeNumber => obj_GetMinNodeNumber
  !! Get the minimum node number

  PROCEDURE, PUBLIC, PASS(obj) :: GetMaxElemNumber => obj_GetMaxElemNumber
  !! Get the maximum element number

  PROCEDURE, PUBLIC, PASS(obj) :: GetMinElemNumber => obj_GetMinElemNumber
  !! Get the minimum element number

  PROCEDURE, PUBLIC, PASS(obj) :: GetParam => obj_GetParam
  !! Get the parameters of the domain

  PROCEDURE, PUBLIC, PASS(obj) :: GetGlobalEdgeNumber => &
    obj_GetGlobalEdgeNumber
  !! Get the global edge number of a local edge

  PROCEDURE, PUBLIC, PASS(obj) :: GetGlobalFaceNumber => &
    obj_GetGlobalFaceNumber
  !! Get the global face number of a local face

  PROCEDURE, PASS(obj) :: GetConnectivity1_ => obj_GetConnectivity1_
  !! Get the connectivity of an element

  PROCEDURE, PASS(obj) :: GetConnectivity2_ => obj_GetConnectivity2_
  !! Get all connectivities of an element

  GENERIC, PUBLIC :: GetConnectivity_ => GetConnectivity1_, &
    GetConnectivity2_
  !! Get the connectivity without allocation

  PROCEDURE, PASS(obj) :: GetTotalVertexNodes1 => obj_GetTotalVertexNodes1
  !! Get the number of vertex nodes
  PROCEDURE, PASS(obj) :: GetTotalVertexNodes2 => obj_GetTotalVertexNodes2
  !! Get the number of vertex nodes of a list of elements
  GENERIC, PUBLIC :: GetTotalVertexNodes => GetTotalVertexNodes1, &
    GetTotalVertexNodes2
  !! Get the number of vertex nodes

  PROCEDURE, PUBLIC, PASS(obj) :: GetOrientation => obj_GetOrientation
  !! Get the orientation flags of an element

  PROCEDURE, PUBLIC, PASS(obj) :: GetElemTopologyIndx => &
    obj_GetElemTopologyIndx
  !! Get the topology index of an element

  PROCEDURE, PUBLIC, PASS(obj) :: IsElementActive => &
    obj_isElementActive
  !! Returns true if an element is active

  ! SET:
  ! @SetMethods

  PROCEDURE, PUBLIC, PASS(obj) :: SetShowTime => obj_SetShowTime
  !! Set the showTime option

  PROCEDURE, PASS(obj) :: SetSparsity1 => obj_SetSparsity1
  !! Set the sparsity pattern of a matrix
  PROCEDURE, NOPASS :: SetSparsity2 => obj_SetSparsity2
  !! Set the sparsity pattern for several domains
  GENERIC, PUBLIC :: SetSparsity => SetSparsity1, SetSparsity2
  !! Set the sparsity pattern of a matrix

  PROCEDURE, PUBLIC, PASS(obj) :: SetTotalMedium => obj_SetTotalMedium
  !! Add space for media in the elements of a mesh

  PROCEDURE, PUBLIC, PASS(obj) :: SetMaterial => obj_SetMaterial
  !! Set the material of a medium in an entity

  PROCEDURE, PUBLIC, PASS(obj) :: SetNodeCoord => obj_SetNodeCoord
  !! Set the node coordinates of the domain

  PROCEDURE, PUBLIC, PASS(obj) :: SetQuality => obj_SetQuality
  !! Set the quality of the meshes

  PROCEDURE, PUBLIC, PASS(obj) :: SetTotalElements => obj_SetTotalElements
  !! Set the number of elements of a dimension

  ! SET:
  ! @MeshDataMethods

  PROCEDURE, PUBLIC, PASS(obj) :: InitiateKdtree => obj_InitiateKdtree
  !! Initiate the kdtree of the domain

  PROCEDURE, PUBLIC, PASS(obj) :: InitiateNodeToElements => &
    obj_InitiateNodeToElements
  !! Initiate the node to elements data of the meshes

  PROCEDURE, PUBLIC, PASS(obj) :: InitiateNodeToNodes => &
    obj_InitiateNodeToNodes
  !! Initiate the node to nodes data of the meshes

  PROCEDURE, PUBLIC, PASS(obj) :: InitiateElementToElements => &
    obj_InitiateElementToElements
  !! Initiate the element to elements data of the meshes

  PROCEDURE, PUBLIC, PASS(obj) :: InitiateFacetElements => &
    obj_InitiateFacetElements
  !! Initiate the facet elements of the meshes

  PROCEDURE, PUBLIC, PASS(obj) :: InitiateExtraNodeToNodes => &
    obj_InitiateExtraNodeToNodes
  !! Initiate the extra node to nodes data of the meshes

  PROCEDURE, PUBLIC, PASS(obj) :: SetDomainFacetElement => &
    obj_SetDomainFacetElement
  !! Set the domain boundary facet elements

  PROCEDURE, PUBLIC, PASS(obj) :: SetMeshmap => obj_SetMeshmap
  !! Set the mesh map

  PROCEDURE, PUBLIC, PASS(obj) :: SetMeshFacetElement => &
    obj_SetMeshFacetElement
  !! Set the mesh facet elements

END TYPE AbstractDomain_

!----------------------------------------------------------------------------
!                                                      AbstractDomainPointer
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-31
! summary: Pointer to a domain
!
! ptr points to an AbstractDomain_.
!
! See also: [[obj_SetSparsity2]]

TYPE :: AbstractDomainPointer_
  CLASS(AbstractDomain_), POINTER :: ptr => NULL()
END TYPE AbstractDomainPointer_

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-28
! summary: Initiate the domain from a HDF5 mesh file
!
! Deallocate obj and call Import. FEDomain_ reads the meta data and node
! coordinates of the domain and makes one mesh for each dimension up to nsd
! (points, curves, surfaces, volumes). The mesh of a dimension contains the
! elements of all entities of that dimension.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(inout) - domain object
!   hdf5 : TYPE(HDF5File_), intent(inout) - mesh file, open with read access
!   group : CHARACTER(*), intent(in) - group in the mesh file
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"
!   TYPE(HDF5File_) :: meshfile
!   TYPE(FEDomain_) :: obj
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, group="")
!
!   CALL OK(obj%IsInitiated(), "IsInitiated")
!   CALL OK(obj%GetNSD() .EQ. 2_I4B, "GetNSD")
!   CALL OK(obj%GetTotalNodes() .EQ. 9_I4B, "GetTotalNodes")
!   CALL OK(obj%GetTotalElements() .EQ. 4_I4B, "surface elements")
!   CALL OK(obj%GetTotalElements(dim=1_I4B) .EQ. 8_I4B, "curve elements")
!   CALL OK(obj%GetTotalElements(dim=0_I4B) .EQ. 4_I4B, "point elements")
!   CALL OK(obj%GetTotalEntities(dim=2_I4B) .EQ. 1_I4B, "surface entities")
!   CALL OK(obj%GetTotalEntities(dim=1_I4B) .EQ. 4_I4B, "curve entities")
!   CALL OK(obj%GetMinNodeNumber() .EQ. 1_I4B, "GetMinNodeNumber")
!   CALL OK(obj%GetMaxNodeNumber() .EQ. 9_I4B, "GetMaxNodeNumber")
!   CALL OK(obj%GetMinElemNumber() .EQ. 1_I4B, "GetMinElemNumber")
!   CALL OK(obj%GetMaxElemNumber() .EQ. 16_I4B, "GetMaxElemNumber")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "../../FEMesh/examples/meshdata/very_small_quad4_mesh_two_region.h5"
!   TYPE(HDF5File_) :: meshfile
!   TYPE(FEDomain_) :: obj
!   INTEGER(I4B), ALLOCATABLE :: con(:)
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, group="")
!
!   CALL OK(obj%GetTotalNodes() .EQ. 10_I4B, "GetTotalNodes")
!   CALL OK(obj%GetTotalElements() .EQ. 4_I4B, "GetTotalElements")
!   CALL OK(obj%GetTotalEntities(dim=2_I4B) .EQ. 2_I4B, &
!           "GetTotalEntities")
!   CALL OK(obj%GetTotalElements(dim=2_I4B, entityNum=2_I4B) &
!           .EQ. 2_I4B, "elements of entity 2")
!   con = obj%GetConnectivity(globalElement=16_I4B, dim=1_I4B)
!   CALL OK(ALL(con .EQ. [2, 5]), "interface element 16")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Import]], [[obj_Deallocate]]

INTERFACE AbstractDomainInitiate
  MODULE SUBROUTINE obj_Initiate(obj, hdf5, group)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
    !! domain object
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! mesh file, open with read access
    CHARACTER(*), INTENT(IN) :: group
    !! group in the mesh file
  END SUBROUTINE obj_Initiate
END INTERFACE AbstractDomainInitiate

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-28
! summary: Deallocate the data of the domain
!
! Reset the meta data, free the node coordinates and the kdtree.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(inout) - domain object
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"
!   TYPE(HDF5File_) :: meshfile
!   TYPE(FEDomain_) :: obj
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, group="")
!
!   CALL obj%DEALLOCATE()
!   CALL OK(.NOT. obj%IsInitiated(), "IsInitiated")
!   CALL obj%Initiate(hdf5=meshfile, group="")
!   CALL OK(obj%GetTotalNodes() .EQ. 9_I4B, "Initiate again")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate]]

INTERFACE AbstractDomainDeallocate
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
    !! domain object
  END SUBROUTINE obj_Deallocate
END INTERFACE AbstractDomainDeallocate

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-10
! summary: Deallocate the kdtree of the domain
!
! Methods which need the kdtree initiate it again.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(inout) - domain object
!
! See also: [[obj_InitiateKdtree]], [[obj_GetNearestNode1]]

INTERFACE
  MODULE SUBROUTINE obj_DeallocateKdtree(obj)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
    !! domain object
  END SUBROUTINE obj_DeallocateKdtree
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-28
! summary: Import the meta data of the domain
!
! Read the meta data (engine, version, nsd, node and element numbers, number
! of entities) and the node coordinates of the domain from the mesh file. The
! domain must not be initiated, and the file must be open with read access.
! FEDomain_ also reads the meshes.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(inout) - domain object
!   hdf5 : TYPE(HDF5File_), intent(inout) - mesh file, open with read access
!   group : CHARACTER(*), intent(in) - group in the mesh file
!
! See also: [[obj_Initiate]], [[FEDomain_:Import]]

INTERFACE AbstractDomainImport
  MODULE SUBROUTINE obj_Import(obj, hdf5, group)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
    !! domain object
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! mesh file, open with read access
    CHARACTER(*), INTENT(IN) :: group
    !! group in the mesh file
  END SUBROUTINE obj_Import
END INTERFACE AbstractDomainImport

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-28
! summary: Import the domain from a toml table
!
! ImportFromToml reads these keys of the table:
!
! - filename: mesh file (extension h5), default is mesh.h5;
! - group: group in the mesh file, default is the root;
! - totalMedium: number of media of the elements, default is 1.
!
! The mesh file is opened, the domain is imported, and the file is closed.
! Then SetTotalMedium is called for dim = 1 to nsd.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(inout) - domain object
!   table : TYPE(toml_table), intent(inout) - toml table of the domain
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: I4B
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   TYPE(FEDomain_) :: obj
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL obj%ImportFromToml(tomlName="domain", filename="FEDomain.toml")
!   CALL OK(obj%IsInitiated(), "IsInitiated")
!   CALL OK(obj%GetTotalNodes() .EQ. 9_I4B, "GetTotalNodes")
!   CALL OK(obj%GetTotalMaterial(dim=2_I4B, globalElement=13_I4B) &
!           .EQ. 2_I4B, "totalMedium")
!   CALL obj%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_ImportFromToml2]]

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
    !! domain object
    TYPE(toml_table), INTENT(INOUT) :: table
    !! toml table of the domain
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

!----------------------------------------------------------------------------
!                                                   ImportFromToml1@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-20
! summary: Import the domain from a toml file
!
! Read the table tomlName from the toml file (filename or afile), then call
! ImportFromToml with the table.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(inout) - domain object
!   tomlName : CHARACTER(*), intent(in) - name of the table in the toml file
!   afile : TYPE(TxtFile_), optional, intent(inout) - toml file
!   filename : CHARACTER(*), optional, intent(in) - name of the toml file
!   printToml : LOGICAL(LGT), optional, intent(in) - if true, the table is
!     printed
!
! See also: [[obj_ImportFromToml1]]

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml2( &
    obj, tomlName, afile, filename, printToml)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
    !! domain object
    CHARACTER(*), INTENT(IN) :: tomlName
    !! name of the table in the toml file
    TYPE(TxtFile_), INTENT(INOUT), OPTIONAL :: afile
    !! toml file
    CHARACTER(*), INTENT(IN), OPTIONAL :: filename
    !! name of the toml file
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: printToml
    !! if true, the table is printed
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-05-20
! summary: Display the content of the domain
!
! FEDomain_ also displays the mesh info of each mesh.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(inout) - domain object
!   msg : CHARACTER(*), intent(in) - message printed first
!   unitno : INTEGER(I4B), optional, intent(in) - unit number of the output;
!     default is stdout
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"
!   TYPE(HDF5File_) :: meshfile
!   TYPE(FEDomain_) :: obj
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, group="")
!
!   CALL obj%Display("domain:")
!   CALL obj%DisplayDomainInfo("domain info:")
!   CALL OK(obj%IsInitiated(), "domain is not changed")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_DisplayDomainInfo]]

INTERFACE AbstractDomainDisplay
  MODULE SUBROUTINE obj_Display(obj, msg, unitno)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
    !! domain object
    CHARACTER(*), INTENT(IN) :: msg
    !! message printed first
    INTEGER(I4B), INTENT(IN), OPTIONAL :: unitno
    !! unit number of the output; default is stdout
  END SUBROUTINE obj_Display
END INTERFACE AbstractDomainDisplay

!----------------------------------------------------------------------------
!                                               DisplayDomainInfo@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-15
! summary: Display the meta data of the domain
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(inout) - domain object
!   msg : CHARACTER(*), intent(in) - message printed first
!   unitno : INTEGER(I4B), optional, intent(in) - unit number of the output;
!     default is stdout
!
! See also: [[obj_Display]]

INTERFACE AbstractDomainDisplayDomainInfo
  MODULE SUBROUTINE obj_DisplayDomainInfo(obj, msg, unitno)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
    !! domain object
    CHARACTER(*), INTENT(IN) :: msg
    !! message printed first
    INTEGER(I4B), INTENT(IN), OPTIONAL :: unitno
    !! unit number of the output; default is stdout
  END SUBROUTINE obj_DisplayDomainInfo
END INTERFACE AbstractDomainDisplayDomainInfo

!----------------------------------------------------------------------------
!                                                    IsInitiated@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-21
! summary: Returns true if the domain is initiated
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!
! Returns:
!   ans : LOGICAL(LGT) - true if the domain is initiated
!
! See also: [[obj_Initiate]]

INTERFACE
  MODULE FUNCTION obj_IsInitiated(obj) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    LOGICAL(LGT) :: ans
    !! true if the domain is initiated
  END FUNCTION obj_IsInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                                                  GetMeshPointer@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-23
! summary: Get a pointer to a mesh of the domain
!
! Return the mesh of dimension dim, or the mesh of dimension nsd if dim is
! absent. In FEDomain_ the mesh of a dimension contains all entities of that
! dimension; entityNum, globalElement and islocal are not used. The pointer is
! not associated if the domain has no mesh of dimension dim.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!   dim : INTEGER(I4B), optional, intent(in) - dimension of the mesh: 0
!     points, 1 curves, 2 surfaces, 3 volumes; default is nsd
!   entityNum : INTEGER(I4B), optional, intent(in) - entity number; not used
!     by FEDomain_
!   globalElement : INTEGER(I4B), optional, intent(in) - global element
!     number; not used by FEDomain_
!   islocal : LOGICAL(LGT), optional, intent(in) - not used by FEDomain_
!
! Returns:
!   ans : CLASS(AbstractMesh_), pointer - pointer to the mesh
!
! Examples
!
!```fortran
! PROGRAM main
!   USE AbstractMesh_Class, ONLY: AbstractMesh_
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"
!   TYPE(HDF5File_) :: meshfile
!   TYPE(FEDomain_) :: obj
!   CLASS(AbstractMesh_), POINTER :: mesh
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, group="")
!
!   mesh => obj%GetMeshPointer()
!   CALL OK(mesh%GetXidimension() .EQ. 2_I4B, "default is nsd")
!   CALL OK(mesh%GetTotalElements() .EQ. 4_I4B, "surface mesh")
!   mesh => obj%GetMeshPointer(dim=1_I4B)
!   CALL OK(mesh%GetTotalElements() .EQ. 8_I4B, "curve mesh")
!   mesh => obj%GetMeshPointer(dim=0_I4B)
!   CALL OK(mesh%GetTotalElements() .EQ. 4_I4B, "point mesh")
!   mesh => obj%GetMeshPointer(dim=3_I4B)
!   CALL OK(.NOT. ASSOCIATED(mesh), "no volume mesh")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```

ABSTRACT INTERFACE
  FUNCTION obj_GetMeshPointer(obj, dim, entityNum, &
    globalElement, islocal) RESULT(ans)
    IMPORT :: AbstractDomain_, I4B, LGT, AbstractMesh_
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN), OPTIONAL :: dim
    !! dimension of the mesh: 0 points, 1 curves, 2 surfaces, 3 volumes;
    !! default is nsd
    INTEGER(I4B), INTENT(IN), OPTIONAL :: entityNum
    !! entity number; not used by FEDomain_
    INTEGER(I4B), INTENT(IN), OPTIONAL :: globalElement
    !! global element number; not used by FEDomain_
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! not used by FEDomain_
    CLASS(AbstractMesh_), POINTER :: ans
    !! pointer to the mesh
  END FUNCTION obj_GetMeshPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                                   IsNodePresent@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-09-21
! summary: Returns true if a node is present in the domain
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!   globalNode : INTEGER(I4B), intent(in) - global node number, or local node
!     number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans : LOGICAL(LGT) - true if the node is present
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"
!   TYPE(HDF5File_) :: meshfile
!   TYPE(FEDomain_) :: obj
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, group="")
!
!   CALL OK(obj%IsNodePresent(9_I4B), "node 9")
!   CALL OK(.NOT. obj%IsNodePresent(10_I4B), "node 10")
!   CALL OK(obj%IsElementPresent(13_I4B), "element 13")
!   CALL OK(.NOT. obj%IsElementPresent(5_I4B), "element 5 in nsd mesh")
!   CALL OK(obj%IsElementPresent(5_I4B, dim=1_I4B), "element 5, dim=1")
!   CALL OK(obj%IsElementActive(13_I4B), "IsElementActive")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_IsElementPresent]]

INTERFACE
  MODULE FUNCTION obj_IsNodePresent(obj, globalNode, islocal) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global node number, or local node number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    LOGICAL(LGT) :: ans
    !! true if the node is present
  END FUNCTION obj_IsNodePresent
END INTERFACE

!----------------------------------------------------------------------------
!                                               IsElementPresent@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-12
! summary: Returns true if an element is present
!
! The method works on the mesh of dimension dim (default nsd), given by
! GetMeshPointer.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   dim : INTEGER(I4B), optional, intent(in) - dimension of the mesh: 0
!     points, 1 curves, 2 surfaces, 3 volumes; default is nsd
!   entityNum : INTEGER(I4B), optional, intent(in) - entity number of the mesh
!     of dimension dim
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans : LOGICAL(LGT) - true if the element is present in the mesh
!
! See also: [[obj_IsNodePresent]], [[obj_isElementActive]]

INTERFACE
  MODULE FUNCTION obj_IsElementPresent(obj, globalElement, dim, entityNum, &
    islocal) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    INTEGER(I4B), INTENT(IN), OPTIONAL :: dim
    !! dimension of the mesh: 0 points, 1 curves, 2 surfaces, 3 volumes;
    !! default is nsd
    INTEGER(I4B), INTENT(IN), OPTIONAL :: entityNum
    !! entity number of the mesh of dimension dim
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    LOGICAL(LGT) :: ans
    !! true if the element is present in the mesh
  END FUNCTION obj_IsElementPresent
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetConnectivity@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-12
! summary: Get the vertex connectivity of an element
!
! The method works on the mesh of dimension dim (default nsd), given by
! GetMeshPointer. GetNNE returns the number of nodes of the element.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   dim : INTEGER(I4B), optional, intent(in) - dimension of the mesh: 0
!     points, 1 curves, 2 surfaces, 3 volumes; default is nsd
!   entityNum : INTEGER(I4B), optional, intent(in) - entity number of the mesh
!     of dimension dim
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans(:) : INTEGER(I4B), allocatable - global node numbers of the element
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"
!   TYPE(HDF5File_) :: meshfile
!   TYPE(FEDomain_) :: obj
!   INTEGER(I4B), ALLOCATABLE :: con(:)
!   INTEGER(I4B) :: buf(20), tsize
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, group="")
!
!   con = obj%GetConnectivity(globalElement=14_I4B)
!   CALL OK(ALL(con .EQ. [7, 9, 6, 3]), "element 14")
!   con = obj%GetConnectivity(globalElement=6_I4B, dim=1_I4B)
!   CALL OK(ALL(con .EQ. [5, 2]), "line element 6")
!   CALL OK(obj%GetNNE(13_I4B) .EQ. 4_I4B, "GetNNE")
!   CALL OK(obj%GetNNE(6_I4B, dim=1_I4B) .EQ. 2_I4B, "GetNNE dim=1")
!   CALL obj%GetConnectivity_(globalElement=13_I4B, ans=buf, &
!                             tsize=tsize, opt="V")
!   CALL OK(ALL(buf(1:tsize) .EQ. [1, 5, 9, 7]), "GetConnectivity_")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetNNE]], [[obj_GetConnectivity1_]]

INTERFACE
  MODULE FUNCTION obj_GetConnectivity(obj, globalElement, dim, entityNum, &
    islocal) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    INTEGER(I4B), INTENT(IN), OPTIONAL :: dim
    !! dimension of the mesh: 0 points, 1 curves, 2 surfaces, 3 volumes;
    !! default is nsd
    INTEGER(I4B), INTENT(IN), OPTIONAL :: entityNum
    !! entity number of the mesh of dimension dim
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !! global node numbers of the element
  END FUNCTION obj_GetConnectivity
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetNNE@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-12
! summary: Get the number of nodes in an element
!
! The method works on the mesh of dimension dim (default nsd), given by
! GetMeshPointer.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   dim : INTEGER(I4B), optional, intent(in) - dimension of the mesh: 0
!     points, 1 curves, 2 surfaces, 3 volumes; default is nsd
!   entityNum : INTEGER(I4B), optional, intent(in) - entity number of the mesh
!     of dimension dim
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans : INTEGER(I4B) - number of nodes of the element
!
! See also: [[obj_GetConnectivity]]

INTERFACE
  MODULE FUNCTION obj_GetNNE(obj, globalElement, dim, entityNum, islocal) &
    RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    INTEGER(I4B), INTENT(IN), OPTIONAL :: dim
    !! dimension of the mesh: 0 points, 1 curves, 2 surfaces, 3 volumes;
    !! default is nsd
    INTEGER(I4B), INTENT(IN), OPTIONAL :: entityNum
    !! entity number of the mesh of dimension dim
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    INTEGER(I4B) :: ans
    !! number of nodes of the element
  END FUNCTION obj_GetNNE
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetNodeToElements@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-28
! summary: Get the elements around a node
!
! Return the global elements around a node in the mesh of dimension nsd. For a
! list of nodes the result is sorted and has no duplicates. GetNodeToElements_
! works without allocation. The node to elements data of the mesh is initiated
! if needed.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(inout) - domain object
!   globalNode : INTEGER(I4B), intent(in) - global node number, or local node
!     number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans(:) : INTEGER(I4B), allocatable - global element numbers
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"
!   TYPE(HDF5File_) :: meshfile
!   TYPE(FEDomain_) :: obj
!   INTEGER(I4B), ALLOCATABLE :: elems(:)
!   INTEGER(I4B) :: buf(10), tsize
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, group="")
!
!   elems = obj%GetNodeToElements(globalNode=9_I4B)
!   CALL OK(ALL(elems .EQ. [13, 14, 15, 16]), "node 9")
!   elems = obj%GetNodeToElements(globalNode=[1_I4B, 2_I4B])
!   CALL OK(ALL(elems .EQ. [13, 15]), "nodes 1 and 2")
!   CALL obj%GetNodeToElements_(ans=buf, tsize=tsize, globalNode=5_I4B)
!   CALL OK(ALL(buf(1:tsize) .EQ. [13, 15]), "GetNodeToElements_")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetNodeToElements2]], [[obj_GetNodeToElements1_]]

INTERFACE
  MODULE FUNCTION obj_GetNodeToElements1(obj, globalNode, islocal) &
    RESULT(ans)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global node number, or local node number if islocal is true
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !! global element numbers
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
  END FUNCTION obj_GetNodeToElements1
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetNodeToElements@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-28
! summary: Get the elements around a list of nodes
!
! See GetNodeToElements.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(inout) - domain object
!   globalNode(:) : INTEGER(I4B), intent(in) - global node numbers (local if
!     islocal is true)
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans(:) : INTEGER(I4B), allocatable - global element numbers, sorted
!
! See also: [[obj_GetNodeToElements1]]

INTERFACE
  MODULE FUNCTION obj_GetNodeToElements2(obj, globalNode, islocal) &
    RESULT(ans)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global node numbers (local if islocal is true)
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !! global element numbers, sorted
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
  END FUNCTION obj_GetNodeToElements2
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetNodeToElements@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-28
! summary: Get the elements around a node without allocation
!
! See GetNodeToElements.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(inout) - domain object
!   ans(:) : INTEGER(I4B), intent(inout) - global element numbers, size must
!     be at least tsize
!   tsize : INTEGER(I4B), intent(out) - number of entries written
!   globalNode : INTEGER(I4B), intent(in) - global node number, or local node
!     number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! See also: [[obj_GetNodeToElements1]]

INTERFACE
  MODULE SUBROUTINE obj_GetNodeToElements1_(obj, ans, tsize, &
    globalNode, islocal)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
    !! domain object
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! global element numbers, size must be at least tsize
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global node number, or local node number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
  END SUBROUTINE obj_GetNodeToElements1_
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetNodeToElements@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-28
! summary: Get the elements around nodes without allocation
!
! See GetNodeToElements.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(inout) - domain object
!   ans(:) : INTEGER(I4B), intent(inout) - global element numbers, size must
!     be at least tsize
!   tsize : INTEGER(I4B), intent(out) - number of entries written
!   globalNode(:) : INTEGER(I4B), intent(in) - global node numbers (local if
!     islocal is true)
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! See also: [[obj_GetNodeToElements1]]

INTERFACE
  MODULE SUBROUTINE obj_GetNodeToElements2_(obj, ans, tsize, &
    globalNode, islocal)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
    !! domain object
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! global element numbers, size must be at least tsize
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global node numbers (local if islocal is true)
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
  END SUBROUTINE obj_GetNodeToElements2_
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetTotalNodes@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-28
! summary: Get the total number of nodes
!
! Without dim and entityNum, return the number of nodes of the domain. Else
! return the number of nodes of the mesh of dimension dim. The operator
! .tNodes. calls this method.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!   dim : INTEGER(I4B), optional, intent(in) - dimension of the mesh
!   entityNum : INTEGER(I4B), optional, intent(in) - entity number of the mesh
!     of dimension dim
!
! Returns:
!   ans : INTEGER(I4B) - number of nodes
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"
!   TYPE(HDF5File_) :: meshfile
!   TYPE(FEDomain_) :: obj
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, group="")
!
!   CALL OK(obj%GetTotalNodes() .EQ. 9_I4B, "domain")
!   CALL OK(obj%GetTotalNodes(dim=1_I4B) .EQ. 8_I4B, "curve mesh")
!   CALL OK(obj%GetTotalNodes(dim=0_I4B) .EQ. 4_I4B, "point mesh")
!   CALL OK((.tNodes.obj) .EQ. 9_I4B, ".tNodes. obj")
!   CALL OK((obj.tNodes.1_I4B) .EQ. 8_I4B, "obj .tNodes. dim")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_tNodes1]], [[obj_tNodes2]], [[obj_tNodes3]]

INTERFACE
  MODULE FUNCTION obj_GetTotalNodes(obj, dim, entityNum) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN), OPTIONAL :: dim
    !! dimension of the mesh
    INTEGER(I4B), INTENT(IN), OPTIONAL :: entityNum
    !! entity number of the mesh of dimension dim
    INTEGER(I4B) :: ans
    !! number of nodes
  END FUNCTION obj_GetTotalNodes
END INTERFACE

!----------------------------------------------------------------------------
!                                                         tNodes@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-28
! summary: Get the number of nodes of a mesh (operator)
!
! obj .tNodes. dim calls GetTotalNodes(dim=dim).
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!   dim : INTEGER(I4B), intent(in) - dimension of the mesh
!
! Returns:
!   ans : INTEGER(I4B) - number of nodes of the mesh
!
! See also: [[obj_GetTotalNodes]]

INTERFACE
  MODULE FUNCTION obj_tNodes1(obj, dim) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN) :: dim
    !! dimension of the mesh
    INTEGER(I4B) :: ans
    !! number of nodes of the mesh
  END FUNCTION obj_tNodes1
END INTERFACE

!----------------------------------------------------------------------------
!                                                          tNodes@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-28
! summary: Get the number of nodes of the domain (operator)
!
! .tNodes. obj calls GetTotalNodes().
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!
! Returns:
!   ans : INTEGER(I4B) - number of nodes
!
! See also: [[obj_GetTotalNodes]]

INTERFACE
  MODULE FUNCTION obj_tNodes2(obj) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B) :: ans
    !! number of nodes
  END FUNCTION obj_tNodes2
END INTERFACE

!----------------------------------------------------------------------------
!                                                         tNodes@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-28
! summary: Get the number of nodes of a mesh (operator)
!
! obj .tNodes. [dim, entityNum] calls GetTotalNodes(dim, entityNum).
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!   opt(2) : INTEGER(I4B), intent(in) - opt(1) is dim, opt(2) is entityNum
!
! Returns:
!   ans : INTEGER(I4B) - number of nodes
!
! See also: [[obj_GetTotalNodes]]

INTERFACE
  MODULE FUNCTION obj_tNodes3(obj, opt) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN) :: opt(2)
    !! opt(1) is dim, opt(2) is entityNum
    INTEGER(I4B) :: ans
    !! number of nodes
  END FUNCTION obj_tNodes3
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetTotalElements@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-06-28
! summary: Get the total number of elements
!
! Return the number of elements of the mesh of dimension dim (default nsd).
! With entityNum, only the elements of that entity are counted. The operator
! .tElements. calls this method.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!   dim : INTEGER(I4B), optional, intent(in) - dimension of the mesh: 0
!     points, 1 curves, 2 surfaces, 3 volumes; default is nsd
!   entitynum : INTEGER(I4B), optional, intent(in) - entity number of the mesh
!     of dimension dim
!
! Returns:
!   ans : INTEGER(I4B) - number of elements
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"
!   TYPE(HDF5File_) :: meshfile
!   TYPE(FEDomain_) :: obj
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, group="")
!
!   CALL OK(obj%GetTotalElements() .EQ. 4_I4B, "nsd mesh")
!   CALL OK(obj%GetTotalElements(dim=1_I4B) .EQ. 8_I4B, "curve mesh")
!   CALL OK(obj%GetTotalElements(dim=2_I4B, entityNum=1_I4B) .EQ. &
!           4_I4B, "entity 1")
!   CALL OK((.tElements.obj) .EQ. 4_I4B, ".tElements. obj")
!   CALL OK((obj.tElements.1_I4B) .EQ. 8_I4B, "obj .tElements. dim")
!   CALL OK((obj.tElements.[2_I4B, 1_I4B]) .EQ. 4_I4B, &
!           "obj .tElements. [dim, entityNum]")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_tElements1]], [[obj_tElements2]], [[obj_tElements3]]

INTERFACE
  MODULE FUNCTION obj_GetTotalElements(obj, dim, entitynum) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN), OPTIONAL :: dim
    !! dimension of the mesh: 0 points, 1 curves, 2 surfaces, 3 volumes;
    !! default is nsd
    INTEGER(I4B), INTENT(IN), OPTIONAL :: entitynum
    !! entity number of the mesh of dimension dim
    INTEGER(I4B) :: ans
    !! number of elements
  END FUNCTION obj_GetTotalElements
END INTERFACE

!----------------------------------------------------------------------------
!                                                      tElements@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-13
! summary: Get the number of elements (operator)
!
! .tElements. obj calls GetTotalElements().
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!
! Returns:
!   ans : INTEGER(I4B) - number of elements of the mesh of dimension nsd
!
! See also: [[obj_GetTotalElements]]

INTERFACE
  MODULE FUNCTION obj_tElements1(obj) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B) :: ans
    !! number of elements of the mesh of dimension nsd
  END FUNCTION obj_tElements1
END INTERFACE

!----------------------------------------------------------------------------
!                                                      tElements@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-13
! summary: Get the number of elements of a mesh (operator)
!
! obj .tElements. dim calls GetTotalElements(dim=dim).
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!   dim : INTEGER(I4B), intent(in) - dimension of the mesh
!
! Returns:
!   ans : INTEGER(I4B) - number of elements of the mesh
!
! See also: [[obj_GetTotalElements]]

INTERFACE
  MODULE FUNCTION obj_tElements2(obj, dim) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN) :: dim
    !! dimension of the mesh
    INTEGER(I4B) :: ans
    !! number of elements of the mesh
  END FUNCTION obj_tElements2
END INTERFACE

!----------------------------------------------------------------------------
!                                                      tElements@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-13
! summary: Get the number of elements of an entity (operator)
!
! obj .tElements. [dim, entityNum] calls GetTotalElements(dim, entityNum).
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!   opt(2) : INTEGER(I4B), intent(in) - opt(1) is dim, opt(2) is entityNum
!
! Returns:
!   ans : INTEGER(I4B) - number of elements of the entity
!
! See also: [[obj_GetTotalElements]]

INTERFACE
  MODULE FUNCTION obj_tElements3(obj, opt) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN) :: opt(2)
    !! opt(1) is dim, opt(2) is entityNum
    INTEGER(I4B) :: ans
    !! number of elements of the entity
  END FUNCTION obj_tElements3
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetLocalNodeNumber@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-09-21
! summary: Get the local number of a global node
!
! Local numbers are the local numbers of the mesh of dimension nsd (in
! FEDomain_). AbstractDomain_ returns the given numbers.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!   globalNode : INTEGER(I4B), intent(in) - global node number, or local node
!     number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans : INTEGER(I4B) - local node number
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"
!   TYPE(HDF5File_) :: meshfile
!   TYPE(FEDomain_) :: obj
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, group="")
!
!   CALL OK(obj%GetLocalNodeNumber(7_I4B) .EQ. 7_I4B, &
!           "GetLocalNodeNumber")
!   CALL OK(ALL(obj%GetLocalNodeNumber([2_I4B, 3_I4B]) .EQ. [2, 3]), &
!           "GetLocalNodeNumber(:)")
!   CALL OK(obj%GetGlobalNodeNumber(7_I4B) .EQ. 7_I4B, &
!           "GetGlobalNodeNumber")
!   CALL OK(ALL(obj%GetGlobalNodeNumber([2_I4B, 3_I4B]) .EQ. [2, 3]), &
!           "GetGlobalNodeNumber(:)")
!   CALL OK(obj%GetLocalElemNumber(15_I4B) .EQ. 3_I4B, &
!           "GetLocalElemNumber")
!   CALL OK(ALL(obj%GetLocalElemNumber([13_I4B, 16_I4B]) .EQ. [1, 4]), &
!           "GetLocalElemNumber(:)")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetLocalNodeNumber2]], [[obj_GetGlobalNodeNumber1]],
!   [[obj_GetLocalElemNumber1]]

INTERFACE
  MODULE FUNCTION obj_GetLocalNodeNumber1(obj, globalNode, islocal) &
    RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global node number, or local node number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    INTEGER(I4B) :: ans
    !! local node number
  END FUNCTION obj_GetLocalNodeNumber1
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetLocalNodeNumber@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-09-21
! summary: Get the local numbers of global nodes
!
! Local numbers are the local numbers of the mesh of dimension nsd (in
! FEDomain_). AbstractDomain_ returns the given numbers.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!   globalNode(:) : INTEGER(I4B), intent(in) - global node numbers (local if
!     islocal is true)
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans(SIZE(globalNode)) : INTEGER(I4B) - local node numbers
!
! See also: [[obj_GetLocalNodeNumber1]]

INTERFACE
  MODULE FUNCTION obj_GetLocalNodeNumber2(obj, globalNode, islocal) &
    RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global node numbers (local if islocal is true)
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    INTEGER(I4B) :: ans(SIZE(globalNode))
    !! local node numbers
  END FUNCTION obj_GetLocalNodeNumber2
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetLocalElemNumber@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Get the local numbers of global elements
!
! This method is implemented by the child class (see FEDomain_).
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!   globalElement(:) : INTEGER(I4B), intent(in) - global element numbers
!     (local if islocal is true)
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans(SIZE(globalElement)) : INTEGER(I4B) - local element numbers
!
! See also: [[obj_GetLocalNodeNumber1]]

INTERFACE
  MODULE FUNCTION obj_GetLocalElemNumber1(obj, globalElement, islocal) &
    RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN) :: globalElement(:)
    !! global element numbers (local if islocal is true)
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    INTEGER(I4B) :: ans(SIZE(globalElement))
    !! local element numbers
  END FUNCTION obj_GetLocalElemNumber1
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetLocalElemNumber@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Get the local number of a global element
!
! This method is implemented by the child class (see FEDomain_).
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans : INTEGER(I4B) - local element number
!
! See also: [[obj_GetLocalNodeNumber1]]

INTERFACE
  MODULE FUNCTION obj_GetLocalElemNumber2(obj, globalElement, islocal) &
    RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    INTEGER(I4B) :: ans
    !! local element number
  END FUNCTION obj_GetLocalElemNumber2
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetElemDataPointer@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-18
! summary: Get a copy of the element data
!
! This method is implemented by the child class (see FEDomain_). FEDomain_
! uses the mesh of dimension nsd.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!   elemdata : TYPE(ElemData_), intent(inout) - copy of the element data
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ElemData_Class, ONLY: ElemData_
!   USE ElemData_Class, ONLY: Elemdata_globalElemNum
!   USE ElemData_Class, ONLY: Elemdata_localElemNum
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"
!   TYPE(HDF5File_) :: meshfile
!   TYPE(FEDomain_) :: obj
!   TYPE(ElemData_) :: elemdata
!   TYPE(ElemData_), POINTER :: ptr
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, group="")
!
!   CALL obj%GetElemData(elemdata=elemdata, globalElement=15_I4B)
!   CALL OK(Elemdata_localElemNum(elemdata) .EQ. 3_I4B, "GetElemData")
!   ptr => obj%GetElemDataPointer(globalElement=16_I4B)
!   CALL OK(Elemdata_globalElemNum(ptr) .EQ. 16_I4B, &
!           "GetElemDataPointer")
!   ptr => NULL()
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetElemDataPointer]]

INTERFACE
  MODULE SUBROUTINE obj_GetElemData(obj, elemdata, globalElement, islocal)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    TYPE(ElemData_), INTENT(INOUT) :: elemdata
    !! copy of the element data
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
  END SUBROUTINE obj_GetElemData
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetElemDataPointer@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-18
! summary: Get a pointer to the element data
!
! This method is implemented by the child class (see FEDomain_).
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans : TYPE(ElemData_), pointer - pointer to the element data
!
! See also: [[obj_GetElemData]]

INTERFACE
  MODULE FUNCTION obj_GetElemDataPointer(obj, globalElement, islocal) &
    RESULT(ans)
    CLASS(AbstractDomain_), TARGET, INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    TYPE(ElemData_), POINTER :: ans
    !! pointer to the element data
  END FUNCTION obj_GetElemDataPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetGlobalNodeNumber@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-09-21
! summary: Get the global number of a local node
!
! AbstractDomain_ returns localNode.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!   localNode : INTEGER(I4B), intent(in) - local node number
!
! Returns:
!   ans : INTEGER(I4B) - global node number
!
! See also: [[obj_GetLocalNodeNumber1]]

INTERFACE
  MODULE PURE FUNCTION obj_GetGlobalNodeNumber1(obj, localNode) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN) :: localNode
    !! local node number
    INTEGER(I4B) :: ans
    !! global node number
  END FUNCTION obj_GetGlobalNodeNumber1
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetGlobalNodeNumber@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-09-21
! summary: Get the global numbers of local nodes
!
! AbstractDomain_ returns localNode.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!   localNode(:) : INTEGER(I4B), intent(in) - local node numbers
!
! Returns:
!   ans(SIZE(localNode)) : INTEGER(I4B) - global node numbers
!
! See also: [[obj_GetLocalNodeNumber1]]

INTERFACE
  MODULE PURE FUNCTION obj_GetGlobalNodeNumber2(obj, localNode) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN) :: localNode(:)
    !! local node numbers
    INTEGER(I4B) :: ans(SIZE(localNode))
    !! global node numbers
  END FUNCTION obj_GetGlobalNodeNumber2
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetTotalEntities@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-09-21
! summary: Get the number of entities of a dimension
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!   dim : INTEGER(I4B), optional, intent(in) - dimension: 0 points, 1 curves,
!     2 surfaces, 3 volumes
!
! Returns:
!   ans : INTEGER(I4B) - number of entities of dimension dim
!
! See also: [[obj_GetParam]]

INTERFACE
  MODULE FUNCTION obj_GetTotalEntities(obj, dim) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN), OPTIONAL :: dim
    !! dimension: 0 points, 1 curves, 2 surfaces, 3 volumes
    INTEGER(I4B) :: ans
    !! number of entities of dimension dim
  END FUNCTION obj_GetTotalEntities
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetTotalEntitiesList@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-23
! summary: Get the number of entities of an element
!
! This method is implemented by the child class (see FEDomain_). FEDomain_
! returns the number of nodes, edges, faces and 1 (the cell) of an element of
! the mesh of dimension nsd.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans(4) : INTEGER(I4B) - nodes, edges, faces, 1
!
! See also: [[obj_GetTotalVertexNodes1]]

INTERFACE
  MODULE FUNCTION obj_GetTotalEntitiesList(obj, globalElement, islocal) &
    RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    INTEGER(I4B) :: ans(4)
    !! nodes, edges, faces, 1
  END FUNCTION obj_GetTotalEntitiesList
END INTERFACE

!----------------------------------------------------------------------------
!                                                 getDimEntityNum@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-11-12
! summary: Get the dimension and entity of an element
!
! Return [dim, entityNum] of the first mesh which contains the element,
! searching dim = 0 to nsd and entityNum = 1 to the number of entities. In
! FEDomain_ the mesh of a dimension contains all its entities, so the returned
! entityNum is 1. Zero is returned if the element is not found.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans(2) : INTEGER(I4B) - dimension and entity number
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"
!   TYPE(HDF5File_) :: meshfile
!   TYPE(FEDomain_) :: obj
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, group="")
!
!   CALL OK(ALL(obj%GetDimEntityNum(15_I4B) .EQ. [2, 1]), "element 15")
!   CALL OK(ALL(obj%GetDimEntityNum(6_I4B) .EQ. [1, 1]), "element 6")
!   CALL OK(ALL(obj%GetDimEntityNum(2_I4B) .EQ. [0, 1]), "element 2")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```

INTERFACE
  MODULE FUNCTION obj_GetDimEntityNum(obj, globalElement, islocal) &
    RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    INTEGER(I4B) :: ans(2)
    !! dimension and entity number
  END FUNCTION obj_GetDimEntityNum
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetNodeCoord@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-23
! summary: Get node coordinates
!
! GetNodeCoord is a generic method:
!
! - without dim, return the coordinates of all nodes of the domain (nsd rows);
! - with dim, return the coordinates of the nodes of the mesh of dimension dim
!   (three rows, columns in local node order of the mesh);
! - with globalNode(:), return the coordinates of a list of nodes;
! - with globalNode and tsize, return the coordinates of one node.
!
! GetNodeCoordPointer returns a pointer to the node coordinates.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!   nodeCoord(:,:) : REAL(DFP), allocatable, intent(inout) - node coordinates
!     in xiJ format, it is reallocated
!   dim : INTEGER(I4B), optional, intent(in) - dimension of the mesh; if
!     absent, all nodes of the domain
!   entityNum : INTEGER(I4B), optional, intent(in) - entity number of the mesh
!     of dimension dim
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: DFP
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"
!   TYPE(HDF5File_) :: meshfile
!   TYPE(FEDomain_) :: obj
!   REAL(DFP), PARAMETER :: tol = 1.0E-8_DFP
!   REAL(DFP), ALLOCATABLE :: xij(:, :)
!   REAL(DFP), POINTER :: ptr(:, :)
!   REAL(DFP) :: xx(3, 10), x(3)
!   INTEGER(I4B) :: nrow, ncol, tsize
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, group="")
!
!   CALL obj%GetNodeCoord(nodeCoord=xij)
!   CALL OK(SIZE(xij, 2) .EQ. 9_I4B, "all nodes")
!   CALL OK(ALL(ABS(xij(1:2, 9) - 0.5_DFP) .LT. tol), "node 9")
!   CALL obj%GetNodeCoord(nodeCoord=xij, dim=1_I4B)
!   CALL OK(SIZE(xij, 2) .EQ. 8_I4B, "nodes of the curve mesh")
!   CALL obj%GetNodeCoord(nodeCoord=xx, nrow=nrow, ncol=ncol, &
!                         globalNode=[4_I4B, 9_I4B])
!   CALL OK(nrow .EQ. 2_I4B .AND. ncol .EQ. 2_I4B, "nrow and ncol")
!   CALL OK(ALL(ABS(xx(1:2, 1) - 1.0_DFP) .LT. tol), "node 4")
!   CALL obj%GetNodeCoord(nodeCoord=x, tsize=tsize, globalNode=6_I4B)
!   CALL OK(ALL(ABS(x(1:2) - [0.5_DFP, 1.0_DFP]) .LT. tol), "node 6")
!   ptr => obj%GetNodeCoordPointer()
!   CALL OK(SIZE(ptr, 2) .EQ. 9_I4B, "GetNodeCoordPointer")
!   ptr => NULL()
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetNodeCoord2]], [[obj_GetNodeCoord3]],
!   [[obj_GetNodeCoordPointer]]

INTERFACE
  MODULE SUBROUTINE obj_GetNodeCoord1(obj, nodeCoord, dim, entityNum)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: nodeCoord(:, :)
    !! node coordinates in xiJ format, it is reallocated
    INTEGER(I4B), INTENT(IN), OPTIONAL :: dim
    !! dimension of the mesh; if absent, all nodes of the domain
    INTEGER(I4B), INTENT(IN), OPTIONAL :: entityNum
    !! entity number of the mesh of dimension dim
  END SUBROUTINE obj_GetNodeCoord1
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetNodeCoord@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-23
! summary: Get the coordinates of a list of nodes
!
! See GetNodeCoord.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!   nodeCoord(:,:) : REAL(DFP), intent(inout) - node coordinates,
!     SIZE(nodeCoord, 1) >= nsd and SIZE(nodeCoord, 2) >= SIZE(globalNode)
!   nrow : INTEGER(I4B), intent(out) - number of rows written
!   ncol : INTEGER(I4B), intent(out) - number of columns written
!   globalNode(:) : INTEGER(I4B), intent(in) - global node numbers (local if
!     islocal is true)
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! See also: [[obj_GetNodeCoord1]]

INTERFACE
  MODULE SUBROUTINE obj_GetNodeCoord2(obj, nodeCoord, nrow, ncol, &
    globalNode, islocal)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    REAL(DFP), INTENT(INOUT) :: nodeCoord(:, :)
    !! node coordinates, SIZE(nodeCoord, 1) >= nsd and SIZE(nodeCoord, 2) >=
    !! SIZE(globalNode)
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! nrow: number of rows written
    !! ncol: number of columns written
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global node numbers (local if islocal is true)
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
  END SUBROUTINE obj_GetNodeCoord2
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetNodeCoord@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-11
! summary: Get the coordinates of a node
!
! See GetNodeCoord.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!   nodeCoord(:) : REAL(DFP), intent(inout) - coordinates of the node,
!     SIZE(nodeCoord) >= nsd
!   tsize : INTEGER(I4B), intent(out) - number of entries written
!   globalNode : INTEGER(I4B), intent(in) - global node number, or local node
!     number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! See also: [[obj_GetNodeCoord1]]

INTERFACE
  MODULE SUBROUTINE obj_GetNodeCoord3(obj, nodeCoord, tsize, globalNode, &
    islocal)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    REAL(DFP), INTENT(INOUT) :: nodeCoord(:)
    !! coordinates of the node, SIZE(nodeCoord) >= nsd
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global node number, or local node number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
  END SUBROUTINE obj_GetNodeCoord3
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetNodeCoordPointer@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-23
! summary: Get a pointer to the node coordinates
!
! See GetNodeCoord.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!
! Returns:
!   ans(:,:) : REAL(DFP), pointer - pointer to the node coordinates of the
!     domain (xiJ format)
!
! See also: [[obj_GetNodeCoord1]]

INTERFACE
  MODULE FUNCTION obj_GetNodeCoordPointer(obj) RESULT(ans)
    CLASS(AbstractDomain_), TARGET, INTENT(IN) :: obj
    !! domain object
    REAL(DFP), POINTER :: ans(:, :)
    !! pointer to the node coordinates of the domain (xiJ format)
  END FUNCTION obj_GetNodeCoordPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                                  GetNearestNode@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-11
! summary: Get the node nearest to a point
!
! Use the kdtree of the node coordinates of the domain; it is initiated if
! needed. With nn, the nn nearest nodes are returned, in no fixed order.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(inout) - domain object
!   qv(:) : REAL(DFP), intent(in) - coordinates of the query point, SIZE(qv)
!     >= nsd
!   x(:) : REAL(DFP), intent(inout) - coordinates of the nearest node
!   globalNode : INTEGER(I4B), intent(out) - global node number of the nearest
!     node
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: DFP
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"
!   TYPE(HDF5File_) :: meshfile
!   TYPE(FEDomain_) :: obj
!   REAL(DFP) :: x(3), xn(3, 1)
!   INTEGER(I4B) :: node, nodes(1)
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, group="")
!
!   CALL obj%GetNearestNode(qv=[0.1_DFP, 0.9_DFP], x=x, globalNode=node)
!   CALL OK(node .EQ. 3_I4B, "nearest node to (0.1, 0.9)")
!   CALL obj%DeallocateKdtree()
!   CALL obj%InitiateKdtree()
!   CALL obj%GetNearestNode(qv=[0.9_DFP, 0.1_DFP], x=xn, &
!                           globalNode=nodes, nn=1_I4B)
!   CALL OK(nodes(1) .EQ. 2_I4B, "nearest node to (0.9, 0.1)")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetNearestNode2]], [[obj_InitiateKdtree]]

INTERFACE
  MODULE SUBROUTINE obj_GetNearestNode1(obj, qv, x, globalNode)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
    !! domain object
    REAL(DFP), INTENT(IN) :: qv(:)
    !! coordinates of the query point, SIZE(qv) >= nsd
    REAL(DFP), INTENT(INOUT) :: x(:)
    !! coordinates of the nearest node
    INTEGER(I4B), INTENT(OUT) :: globalNode
    !! global node number of the nearest node
  END SUBROUTINE obj_GetNearestNode1
END INTERFACE

!----------------------------------------------------------------------------
!                                                  GetNearestNode@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-11
! summary: Get the nn nodes nearest to a point
!
! See GetNearestNode.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(inout) - domain object
!   qv(:) : REAL(DFP), intent(in) - coordinates of the query point, SIZE(qv)
!     >= nsd
!   x(:,:) : REAL(DFP), intent(inout) - coordinates of the nearest nodes,
!     SIZE(x, 2) >= nn
!   globalNode(:) : INTEGER(I4B), intent(inout) - global node numbers,
!     SIZE(globalNode) >= nn
!   nn : INTEGER(I4B), intent(in) - number of nearest nodes
!
! See also: [[obj_GetNearestNode1]]

INTERFACE
  MODULE SUBROUTINE obj_GetNearestNode2(obj, qv, x, globalNode, nn)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
    !! domain object
    REAL(DFP), INTENT(IN) :: qv(:)
    !! coordinates of the query point, SIZE(qv) >= nsd
    REAL(DFP), INTENT(INOUT) :: x(:, :)
    !! coordinates of the nearest nodes, SIZE(x, 2) >= nn
    INTEGER(I4B), INTENT(INOUT) :: globalNode(:)
    !! global node numbers, SIZE(globalNode) >= nn
    INTEGER(I4B), INTENT(IN) :: nn
    !! number of nearest nodes
  END SUBROUTINE obj_GetNearestNode2
END INTERFACE

!----------------------------------------------------------------------------
!                                                         getNptrs@getMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-09-02
! summary: Get the global node numbers of a mesh
!
! Return the global node numbers of the mesh of dimension dim. GetNptrs_ works
! without allocation, and GetInternalNptrs returns the internal nodes of the
! mesh. entityNum is not used.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!   dim : INTEGER(I4B), intent(in) - dimension of the mesh
!   entityNum(:) : INTEGER(I4B), optional, intent(in) - entity numbers; not
!     used
!
! Returns:
!   ans(:) : INTEGER(I4B), allocatable - global node numbers
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"
!   TYPE(HDF5File_) :: meshfile
!   TYPE(FEDomain_) :: obj
!   INTEGER(I4B), ALLOCATABLE :: nodes(:)
!   INTEGER(I4B) :: buf(20), tsize
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, group="")
!
!   nodes = obj%GetNptrs(dim=2_I4B)
!   CALL OK(ALL(nodes .EQ. [1, 2, 3, 4, 5, 6, 7, 8, 9]), "surface mesh")
!   nodes = obj%GetNptrs(dim=1_I4B)
!   CALL OK(ALL(nodes .EQ. [1, 2, 3, 4, 5, 6, 7, 8]), "curve mesh")
!   CALL obj%GetNptrs_(nptrs=buf, dim=1_I4B, tsize=tsize)
!   CALL OK(tsize .EQ. 8_I4B, "GetNptrs_")
!   nodes = obj%GetInternalNptrs(dim=2_I4B)
!   CALL OK(ALL(nodes .EQ. [9]), "GetInternalNptrs")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetNptrs_]], [[obj_GetInternalNptrs]]

INTERFACE
  MODULE FUNCTION obj_GetNptrs(obj, dim, entityNum) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN) :: dim
    !! dimension of the mesh
    INTEGER(I4B), INTENT(IN), OPTIONAL :: entityNum(:)
    !! entity numbers; not used
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !! global node numbers
  END FUNCTION obj_GetNptrs
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetNptrs@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-17
! summary: Get the global node numbers of a mesh
!
! See GetNptrs.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!   nptrs(:) : INTEGER(I4B), intent(inout) - global node numbers, size must be
!     at least the number of nodes of the mesh
!   dim : INTEGER(I4B), intent(in) - dimension of the mesh
!   entityNum(:) : INTEGER(I4B), optional, intent(in) - entity numbers; not
!     used
!   tsize : INTEGER(I4B), optional, intent(out) - number of entries written in
!     nptrs
!
! See also: [[obj_GetNptrs]]

INTERFACE
  MODULE SUBROUTINE obj_GetNptrs_(obj, nptrs, dim, entityNum, tsize)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(INOUT) :: nptrs(:)
    !! global node numbers, size must be at least the number of nodes of the
    !! mesh
    INTEGER(I4B), INTENT(IN) :: dim
    !! dimension of the mesh
    INTEGER(I4B), INTENT(IN), OPTIONAL :: entityNum(:)
    !! entity numbers; not used
    INTEGER(I4B), INTENT(OUT), OPTIONAL :: tsize
    !! number of entries written in nptrs
  END SUBROUTINE obj_GetNptrs_
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetInternalNptrs@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-09-02
! summary: Get the internal nodes of a mesh
!
! See GetNptrs.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!   dim : INTEGER(I4B), intent(in) - dimension of the mesh
!   entityNum(:) : INTEGER(I4B), optional, intent(in) - entity numbers; not
!     used
!
! Returns:
!   ans(:) : INTEGER(I4B), allocatable - global numbers of the internal nodes
!
! See also: [[obj_GetNptrs]]

INTERFACE
  MODULE FUNCTION obj_GetInternalNptrs(obj, dim, entityNum) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN) :: dim
    !! dimension of the mesh
    INTEGER(I4B), INTENT(IN), OPTIONAL :: entityNum(:)
    !! entity numbers; not used
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !! global numbers of the internal nodes
  END FUNCTION obj_GetInternalNptrs
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetNptrs@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-09-02
! summary: Get the nodes of the domain in a box
!
! The kdtree is initiated if needed. The nodes within the radius of the box
! are found; if isStrict is true (default) only the nodes inside or on the box
! are kept. GetNptrsInBox_ works without allocation.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(inout) - domain object
!   box : TYPE(BoundingBox_), intent(in) - bounding box
!   nptrs(:) : INTEGER(I4B), allocatable, intent(inout) - global node numbers
!     in the box, it is reallocated
!   isStrict : LOGICAL(LGT), optional, intent(in) - if true (default), only
!     nodes inside or on the box are returned
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: BoundingBox_
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE BoundingBox_Method, ONLY: BoundingBoxInitiate => Initiate
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: DFP
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE SortUtility, ONLY: Sort
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"
!   TYPE(HDF5File_) :: meshfile
!   TYPE(FEDomain_) :: obj
!   TYPE(BoundingBox_) :: box
!   INTEGER(I4B), ALLOCATABLE :: nodes(:)
!   INTEGER(I4B) :: buf(20), tnodes
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, group="")
!
!   CALL BoundingBoxInitiate(obj=box, nsd=3_I4B, &
!                            lim=[-0.1_DFP, 0.6_DFP, -0.1_DFP, &
!                                 0.6_DFP, -0.1_DFP, 0.1_DFP])
!   CALL obj%GetNptrsInBox(box=box, nptrs=nodes)
!   CALL OK(ALL(Sort(nodes) .EQ. [1, 5, 7, 9]), "GetNptrsInBox")
!   CALL obj%GetNptrsInBox_(box=box, nptrs=buf, tnodes=tnodes)
!   CALL OK(tnodes .EQ. 4_I4B, "GetNptrsInBox_")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetNptrsInBox_]]

INTERFACE
  MODULE SUBROUTINE obj_GetNptrsInBox(obj, box, nptrs, isStrict)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
    !! domain object
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: nptrs(:)
    !! global node numbers in the box, it is reallocated
    TYPE(BoundingBox_), INTENT(IN) :: box
    !! bounding box
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: isStrict
    !! if true (default), only nodes inside or on the box are returned
  END SUBROUTINE obj_GetNptrsInBox
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetNptrs@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-09-02
! summary: Get the nodes in a box without allocation
!
! See GetNptrsInBox. In debug mode an error is raised if nptrs is too small.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(inout) - domain object
!   box : TYPE(BoundingBox_), intent(in) - bounding box
!   nptrs(:) : INTEGER(I4B), intent(inout) - global node numbers in the box,
!     size must be at least the number of nodes found
!   tnodes : INTEGER(I4B), intent(inout) - number of nodes found
!   isStrict : LOGICAL(LGT), optional, intent(in) - if true (default), only
!     nodes inside or on the box are returned
!
! See also: [[obj_GetNptrsInBox]]

INTERFACE
  MODULE SUBROUTINE obj_GetNptrsInBox_(obj, box, nptrs, tnodes, isStrict)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
    !! domain object
    TYPE(BoundingBox_), INTENT(IN) :: box
    !! bounding box
    INTEGER(I4B), INTENT(INOUT) :: nptrs(:)
    !! global node numbers in the box, size must be at least the number of
    !! nodes found
    INTEGER(I4B), INTENT(INOUT) :: tnodes
    !! number of nodes found
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: isStrict
    !! if true (default), only nodes inside or on the box are returned
  END SUBROUTINE obj_GetNptrsInBox_
END INTERFACE

!----------------------------------------------------------------------------
!                                                  GetBoundingBox@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-17
! summary: Get the bounding box
!
! Without dim and entityNum, return the box of all nodes of the domain. Else
! return the box of the nodes of the mesh of dimension dim.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!   dim : INTEGER(I4B), optional, intent(in) - dimension of the mesh: 0
!     points, 1 curves, 2 surfaces, 3 volumes; default is nsd
!   entityNum : INTEGER(I4B), optional, intent(in) - entity number of the mesh
!     of dimension dim
!
! Returns:
!   ans : TYPE(BoundingBox_) - bounding box
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: BoundingBox_
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE BoundingBox_Method, ONLY: OPERATOR(.Xmax.)
!   USE BoundingBox_Method, ONLY: OPERATOR(.Ymax.)
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: DFP
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"
!   TYPE(HDF5File_) :: meshfile
!   TYPE(FEDomain_) :: obj
!   REAL(DFP), PARAMETER :: tol = 1.0E-8_DFP
!   TYPE(BoundingBox_) :: box
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, group="")
!
!   box = obj%GetBoundingBox()
!   CALL OK(ABS((.Xmax.box) - 1.0_DFP) .LT. tol, "Xmax")
!   CALL OK(ABS((.Ymax.box) - 1.0_DFP) .LT. tol, "Ymax")
!   box = obj%GetBoundingBox(dim=1_I4B)
!   CALL OK(ABS((.Xmax.box) - 1.0_DFP) .LT. tol, "Xmax of curve mesh")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```

INTERFACE
  MODULE FUNCTION obj_GetBoundingBox(obj, dim, entityNum) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN), OPTIONAL :: dim
    !! dimension of the mesh: 0 points, 1 curves, 2 surfaces, 3 volumes;
    !! default is nsd
    INTEGER(I4B), INTENT(IN), OPTIONAL :: entityNum
    !! entity number of the mesh of dimension dim
    TYPE(BoundingBox_) :: ans
    !! bounding box
  END FUNCTION obj_GetBoundingBox
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetNSD@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-09-21
! summary: Get the number of spatial dimensions
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!
! Returns:
!   ans : INTEGER(I4B) - number of spatial dimensions
!
! See also: [[obj_Initiate]]

INTERFACE
  MODULE FUNCTION obj_GetNSD(obj) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B) :: ans
    !! number of spatial dimensions
  END FUNCTION obj_GetNSD
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetOrder@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-09-21
! summary: Get the order of the meshes of a dimension
!
! This routine is not implemented; it raises an error.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!   dim : INTEGER(I4B), intent(in) - dimension of the mesh
!
! Returns:
!   ans(:) : INTEGER(I4B), allocatable - order of the meshes

INTERFACE
  MODULE FUNCTION obj_GetOrder(obj, dim) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN) :: dim
    !! dimension of the mesh
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !! order of the meshes
  END FUNCTION obj_GetOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                          GetTotalMeshFacetData@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-05-24
! summary: Get the size of mesh facet data
!
! This method is deprecated; it raises an error.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!   imeshFacetData : INTEGER(I4B), optional, intent(in) - index of the mesh
!     facet data
!
! Returns:
!   ans : INTEGER(I4B) - zero

INTERFACE
  MODULE FUNCTION obj_GetTotalMeshFacetData(obj, imeshFacetData) &
    RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN), OPTIONAL :: imeshFacetData
    !! index of the mesh facet data
    INTEGER(I4B) :: ans
    !! zero
  END FUNCTION obj_GetTotalMeshFacetData
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetTotalMaterial@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-09
! summary: Get the number of media of an element
!
! The method works on the mesh of dimension dim (default nsd), given by
! GetMeshPointer.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!   dim : INTEGER(I4B), intent(in) - dimension of the mesh
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!   entityNum : INTEGER(I4B), optional, intent(in) - entity number; not used
!     by FEDomain_
!
! Returns:
!   ans : INTEGER(I4B) - number of media of the element
!
! See also: [[obj_SetTotalMedium]]

INTERFACE
  MODULE FUNCTION obj_GetTotalMaterial(obj, dim, globalElement, &
    islocal, entityNum) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN) :: dim
    !! dimension of the mesh
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    INTEGER(I4B), INTENT(IN), OPTIONAL :: entityNum
    !! entity number; not used by FEDomain_
    INTEGER(I4B) :: ans
    !! number of media of the element
  END FUNCTION obj_GetTotalMaterial
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetElemType@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-23
! summary: Get the element types of a mesh
!
! This routine is not implemented; it raises an error.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!   dim : INTEGER(I4B), intent(in) - dimension of the mesh
!
! Returns:
!   ans(:) : INTEGER(I4B), allocatable - element types
!
! See also: [[obj_GetUniqueElemType]]

INTERFACE
  MODULE FUNCTION obj_GetElemType(obj, dim) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN) :: dim
    !! dimension of the mesh
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !! element types
  END FUNCTION obj_GetElemType
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetUniqueElemType@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-23
! summary: Get the unique element types of a mesh
!
! This routine calls GetElemType, which is not implemented.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!   dim : INTEGER(I4B), intent(in) - dimension of the mesh
!
! Returns:
!   ans(:) : INTEGER(I4B), allocatable - element types without duplicates
!
! See also: [[obj_GetElemType]]

INTERFACE
  MODULE FUNCTION obj_GetUniqueElemType(obj, dim) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN) :: dim
    !! dimension of the mesh
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !! element types without duplicates
  END FUNCTION obj_GetUniqueElemType
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetMaxNodeNumber@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-15
! summary: Get the maximum node number
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!
! Returns:
!   ans : INTEGER(I4B) - largest global node number of the domain
!
! See also: [[obj_Initiate]]

INTERFACE
  MODULE FUNCTION obj_GetMaxNodeNumber(obj) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B) :: ans
    !! largest global node number of the domain
  END FUNCTION obj_GetMaxNodeNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetMinNodeNumber@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-15
! summary: Get the minimum node number
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!
! Returns:
!   ans : INTEGER(I4B) - smallest global node number of the domain
!
! See also: [[obj_Initiate]]

INTERFACE
  MODULE FUNCTION obj_GetMinNodeNumber(obj) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B) :: ans
    !! smallest global node number of the domain
  END FUNCTION obj_GetMinNodeNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetMaxElemNumber@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-15
! summary: Get the maximum element number
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!
! Returns:
!   ans : INTEGER(I4B) - largest global element number of the domain
!
! See also: [[obj_Initiate]]

INTERFACE
  MODULE FUNCTION obj_GetMaxElemNumber(obj) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B) :: ans
    !! largest global element number of the domain
  END FUNCTION obj_GetMaxElemNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetMinElemNumber@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-15
! summary: Get the minimum element number
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!
! Returns:
!   ans : INTEGER(I4B) - smallest global element number of the domain
!
! See also: [[obj_Initiate]]

INTERFACE
  MODULE FUNCTION obj_GetMinElemNumber(obj) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B) :: ans
    !! smallest global element number of the domain
  END FUNCTION obj_GetMinElemNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetParam@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-08
! summary: Get the parameters of the domain
!
! Each present argument returns the component of the same name. tElements(0:3)
! and tEntities(0:3) are the number of elements and entities of each
! dimension.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!   isInitiated : LOGICAL(LGT), optional, intent(out) - true if the domain is
!     initiated
!   engine : CHARACTER(*), optional, intent(inout) - engine of the mesh file
!   majorVersion : INTEGER(I4B), optional, intent(out) - major version of the
!     mesh file
!   minorVersion : INTEGER(I4B), optional, intent(out) - minor version of the
!     mesh file
!   version : REAL(DFP), optional, intent(out) - version of the mesh file
!   nsd : INTEGER(I4B), optional, intent(out) - number of spatial dimensions
!   maxNptrs : INTEGER(I4B), optional, intent(out) - largest node number
!   minNptrs : INTEGER(I4B), optional, intent(out) - smallest node number
!   tNodes : INTEGER(I4B), optional, intent(out) - number of nodes
!   isNodeNumberSparse : LOGICAL(LGT), optional, intent(out) - true if node
!     numbers are not contiguous
!   maxElemNum : INTEGER(I4B), optional, intent(out) - largest element number
!   minElemNum : INTEGER(I4B), optional, intent(out) - smallest element number
!   isElemNumberSparse : LOGICAL(LGT), optional, intent(out) - true if element
!     numbers are not contiguous
!   tEntitiesForNodes : INTEGER(I4B), optional, intent(out) - number of
!     entities used for nodes
!   tEntitiesForElements : INTEGER(I4B), optional, intent(out) - number of
!     entities used for elements
!   tElements(0:3) : INTEGER(I4B), optional, intent(inout) - number of
!     elements of each dimension
!   tEntities(0:3) : INTEGER(I4B), optional, intent(inout) - number of
!     entities of each dimension
!   nodeCoord(:,:) : REAL(DFP), optional, intent(inout) - node coordinates,
!     SHAPE must be that of the domain
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"
!   TYPE(HDF5File_) :: meshfile
!   TYPE(FEDomain_) :: obj
!   INTEGER(I4B) :: nsd, tNodes, tElements(0:3), tEntities(0:3)
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, group="")
!
!   CALL obj%GetParam(nsd=nsd, tNodes=tNodes, tElements=tElements, &
!                     tEntities=tEntities)
!   CALL OK(nsd .EQ. 2_I4B .AND. tNodes .EQ. 9_I4B, "nsd and tNodes")
!   CALL OK(ALL(tElements .EQ. [4, 8, 4, 0]), "tElements")
!   CALL OK(ALL(tEntities .EQ. [4, 4, 1, 0]), "tEntities")
!
!   CALL obj%SetTotalElements(indx=0_I4B, VALUE=5_I4B)
!   CALL obj%GetParam(tElements=tElements)
!   CALL OK(tElements(0) .EQ. 5_I4B, "SetTotalElements")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_SetTotalElements]]

INTERFACE
  MODULE SUBROUTINE obj_GetParam(obj, isInitiated, engine, majorVersion, &
    minorVersion, version, nsd, maxNptrs, minNptrs, tNodes, &
    isNodeNumberSparse, maxElemNum, minElemNum, isElemNumberSparse, &
    tEntitiesForNodes, tEntitiesForElements, tElements, tEntities, nodeCoord)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    LOGICAL(LGT), INTENT(OUT), OPTIONAL :: isInitiated
    !! true if the domain is initiated
    CHARACTER(*), INTENT(INOUT), OPTIONAL :: engine
    !! engine of the mesh file
    INTEGER(I4B), INTENT(OUT), OPTIONAL :: majorVersion
    !! major version of the mesh file
    INTEGER(I4B), INTENT(OUT), OPTIONAL :: minorVersion
    !! minor version of the mesh file
    REAL(DFP), INTENT(OUT), OPTIONAL :: version
    !! version of the mesh file
    INTEGER(I4B), INTENT(OUT), OPTIONAL :: nsd
    !! number of spatial dimensions
    INTEGER(I4B), INTENT(OUT), OPTIONAL :: maxNptrs
    !! largest node number
    INTEGER(I4B), INTENT(OUT), OPTIONAL :: minNptrs
    !! smallest node number
    INTEGER(I4B), INTENT(OUT), OPTIONAL :: tNodes
    !! number of nodes
    LOGICAL(LGT), INTENT(OUT), OPTIONAL :: isNodeNumberSparse
    !! true if node numbers are not contiguous
    INTEGER(I4B), INTENT(OUT), OPTIONAL :: maxElemNum
    !! largest element number
    INTEGER(I4B), INTENT(OUT), OPTIONAL :: minElemNum
    !! smallest element number
    LOGICAL(LGT), INTENT(OUT), OPTIONAL :: isElemNumberSparse
    !! true if element numbers are not contiguous
    INTEGER(I4B), INTENT(OUT), OPTIONAL :: tEntitiesForNodes
    !! number of entities used for nodes
    INTEGER(I4B), INTENT(OUT), OPTIONAL :: tEntitiesForElements
    !! number of entities used for elements
    INTEGER(I4B), INTENT(INOUT), OPTIONAL :: tElements(0:3)
    !! number of elements of each dimension
    INTEGER(I4B), INTENT(INOUT), OPTIONAL :: tEntities(0:3)
    !! number of entities of each dimension
    REAL(DFP), INTENT(INOUT), OPTIONAL :: nodeCoord(:, :)
    !! node coordinates, SHAPE must be that of the domain
  END SUBROUTINE obj_GetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetGlobalEdgeNumber@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Get the global edge number of a local edge
!
! This method is implemented by the child class (see FEDomain_). FEDomain_
! uses the mesh of dimension nsd; its edges are numbered by
! InitiateEdgeConnectivity of the mesh.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   localEdgeNumber : INTEGER(I4B), intent(in) - local edge number in the
!     element
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans : INTEGER(I4B) - global edge number
!
! Examples
!
!```fortran
! PROGRAM main
!   USE AbstractMesh_Class, ONLY: AbstractMesh_
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"
!   TYPE(HDF5File_) :: meshfile
!   TYPE(FEDomain_) :: obj
!   CLASS(AbstractMesh_), POINTER :: mesh
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, group="")
!
!   CALL OK(obj%GetGlobalFaceNumber(globalElement=13_I4B, &
!                                   localFaceNumber=1_I4B) .EQ. 1_I4B, &
!           "GetGlobalFaceNumber")
!   mesh => obj%GetMeshPointer()
!   CALL mesh%InitiateEdgeConnectivity()
!   CALL OK(obj%GetGlobalEdgeNumber(globalElement=13_I4B, &
!                                   localEdgeNumber=2_I4B) .EQ. &
!           obj%GetGlobalEdgeNumber(globalElement=14_I4B, &
!                                   localEdgeNumber=1_I4B), &
!           "GetGlobalEdgeNumber")
!   mesh => NULL()
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetGlobalFaceNumber]]

INTERFACE
  MODULE FUNCTION obj_GetGlobalEdgeNumber(obj, globalElement, &
    localEdgeNumber, islocal) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    INTEGER(I4B), INTENT(IN) :: localEdgeNumber
    !! local edge number in the element
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    INTEGER(I4B) :: ans
    !! global edge number
  END FUNCTION obj_GetGlobalEdgeNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetGlobalFaceNumber@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-21
! summary: Get the global face number of a local face
!
! This method is implemented by the child class (see FEDomain_). FEDomain_
! uses the mesh of dimension nsd.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   localFaceNumber : INTEGER(I4B), intent(in) - local face number in the
!     element
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans : INTEGER(I4B) - global face number
!
! See also: [[obj_GetGlobalEdgeNumber]]

INTERFACE
  MODULE FUNCTION obj_GetGlobalFaceNumber(obj, globalElement, &
    localFaceNumber, islocal) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    INTEGER(I4B), INTENT(IN) :: localFaceNumber
    !! local face number in the element
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    INTEGER(I4B) :: ans
    !! global face number
  END FUNCTION obj_GetGlobalFaceNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetConnectivity@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-12
! summary: Get the connectivity of an element
!
! This method is implemented by the child class (see FEDomain_).
! GetConnectivity_ is a generic method; see the mesh method GetConnectivity_.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   ans(:) : INTEGER(I4B), intent(inout) - connectivity, size must be at least
!     tsize
!   tsize : INTEGER(I4B), intent(out) - number of entries written
!   opt : CHARACTER(*), optional, intent(in) - connectivity: V vertex
!     (default), E edges, F faces, C cell, A all
!   dim : INTEGER(I4B), optional, intent(in) - dimension of the mesh: 0
!     points, 1 curves, 2 surfaces, 3 volumes; default is nsd
!   entityNum : INTEGER(I4B), optional, intent(in) - entity number of the mesh
!     of dimension dim
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! See also: [[obj_GetConnectivity]], [[obj_GetConnectivity2_]]

INTERFACE
  MODULE SUBROUTINE obj_GetConnectivity1_(obj, globalElement, ans, tsize, &
    opt, dim, entityNum, islocal)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! connectivity, size must be at least tsize
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written
    CHARACTER(*), INTENT(IN), OPTIONAL :: opt
    !! connectivity: V vertex (default), E edges, F faces, C cell, A all
    INTEGER(I4B), INTENT(IN), OPTIONAL :: dim
    !! dimension of the mesh: 0 points, 1 curves, 2 surfaces, 3 volumes;
    !! default is nsd
    INTEGER(I4B), INTENT(IN), OPTIONAL :: entityNum
    !! entity number of the mesh of dimension dim
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
  END SUBROUTINE obj_GetConnectivity1_
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetConnectivity@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-14
! summary: Get all connectivities of an element
!
! This method is implemented by the child class (see FEDomain_).
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!   cellCon(:) : INTEGER(I4B), intent(inout) - cell number of the element
!   faceCon(:) : INTEGER(I4B), intent(inout) - global face numbers of the
!     element
!   edgeCon(:) : INTEGER(I4B), intent(inout) - global edge numbers of the
!     element
!   nodeCon(:) : INTEGER(I4B), intent(inout) - global node numbers of the
!     element
!   tCellCon : INTEGER(I4B), intent(out) - number of entries written in
!     cellCon
!   tFaceCon : INTEGER(I4B), intent(out) - number of entries written in
!     faceCon
!   tEdgeCon : INTEGER(I4B), intent(out) - number of entries written in
!     edgeCon
!   tnodeCon : INTEGER(I4B), intent(out) - number of entries written in
!     nodeCon
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   dim : INTEGER(I4B), optional, intent(in) - dimension of the mesh: 0
!     points, 1 curves, 2 surfaces, 3 volumes; default is nsd
!   entityNum : INTEGER(I4B), optional, intent(in) - entity number of the mesh
!     of dimension dim
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! See also: [[obj_GetConnectivity1_]]

INTERFACE
  MODULE SUBROUTINE obj_GetConnectivity2_(obj, cellCon, faceCon, edgeCon, &
    nodeCon, tCellCon, tFaceCon, &
    tEdgeCon, tnodeCon, globalElement, &
    dim, entityNum, islocal)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(INOUT) :: cellCon(:)
    !! cell number of the element
    INTEGER(I4B), INTENT(INOUT) :: faceCon(:)
    !! global face numbers of the element
    INTEGER(I4B), INTENT(INOUT) :: edgeCon(:)
    !! global edge numbers of the element
    INTEGER(I4B), INTENT(INOUT) :: nodeCon(:)
    !! global node numbers of the element
    INTEGER(I4B), INTENT(OUT) :: tCellCon
    !! number of entries written in cellCon
    INTEGER(I4B), INTENT(OUT) :: tFaceCon
    !! number of entries written in faceCon
    INTEGER(I4B), INTENT(OUT) :: tEdgeCon
    !! number of entries written in edgeCon
    INTEGER(I4B), INTENT(OUT) :: tnodeCon
    !! number of entries written in nodeCon
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    INTEGER(I4B), INTENT(IN), OPTIONAL :: dim
    !! dimension of the mesh: 0 points, 1 curves, 2 surfaces, 3 volumes;
    !! default is nsd
    INTEGER(I4B), INTENT(IN), OPTIONAL :: entityNum
    !! entity number of the mesh of dimension dim
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
  END SUBROUTINE obj_GetConnectivity2_
END INTERFACE

!----------------------------------------------------------------------------
!                                         GetTotalVertexNodes@MeshDataMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-05
! summary: Get the number of vertex nodes
!
! This method is implemented by the child class (see FEDomain_). FEDomain_
! uses the mesh of dimension dim: with globalElement it returns the vertex
! nodes of the element, with entityNum those of the entity, else those of the
! mesh.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!   globalElement : INTEGER(I4B), optional, intent(in) - global element number
!     (local if islocal is true)
!   dim : INTEGER(I4B), optional, intent(in) - dimension of the mesh: 0
!     points, 1 curves, 2 surfaces, 3 volumes; default is nsd
!   entityNum : INTEGER(I4B), optional, intent(in) - entity number of the mesh
!     of dimension dim
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans : INTEGER(I4B) - number of vertex nodes
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"
!   TYPE(HDF5File_) :: meshfile
!   TYPE(FEDomain_) :: obj
!   INTEGER(I4B) :: ent(4)
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, group="")
!
!   CALL OK(obj%GetTotalVertexNodes() .EQ. 9_I4B, "mesh")
!   CALL OK(obj%GetTotalVertexNodes(globalElement=13_I4B) .EQ. 4_I4B, &
!           "element 13")
!   CALL OK(obj%GetTotalVertexNodes(globalElement=[13_I4B, 14_I4B]) &
!           .EQ. 6_I4B, "elements 13 and 14")
!   ent = obj%GetTotalEntitiesList(globalElement=13_I4B)
!   CALL OK(ent(1) .EQ. 4_I4B .AND. ent(4) .EQ. 1_I4B, &
!           "GetTotalEntitiesList")
!   CALL OK(obj%GetElemTopologyIndx(13_I4B) .EQ. 4_I4B, &
!           "GetElemTopologyIndx")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetTotalVertexNodes2]], [[obj_GetTotalEntitiesList]],
!   [[obj_GetElemTopologyIndx]]

INTERFACE
  MODULE FUNCTION obj_GetTotalVertexNodes1(obj, globalElement, dim, &
    entityNum, islocal) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN), OPTIONAL :: globalElement
    !! global element number (local if islocal is true)
    INTEGER(I4B), INTENT(IN), OPTIONAL :: dim
    !! dimension of the mesh: 0 points, 1 curves, 2 surfaces, 3 volumes;
    !! default is nsd
    INTEGER(I4B), INTENT(IN), OPTIONAL :: entityNum
    !! entity number of the mesh of dimension dim
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    INTEGER(I4B) :: ans
    !! number of vertex nodes
  END FUNCTION obj_GetTotalVertexNodes1
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetTotalVertexNodes@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-05
! summary: Get the number of vertex nodes of a list of elements
!
! This method is implemented by the child class (see FEDomain_).
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!   globalElement(:) : INTEGER(I4B), intent(in) - global element numbers
!     (local if islocal is true)
!   dim : INTEGER(I4B), optional, intent(in) - dimension of the mesh: 0
!     points, 1 curves, 2 surfaces, 3 volumes; default is nsd
!   entityNum : INTEGER(I4B), optional, intent(in) - entity number of the mesh
!     of dimension dim
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans : INTEGER(I4B) - number of vertex nodes of the elements
!
! See also: [[obj_GetTotalVertexNodes1]]

INTERFACE
  MODULE FUNCTION obj_GetTotalVertexNodes2(obj, globalElement, dim, &
    entityNum, islocal) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN) :: globalElement(:)
    !! global element numbers (local if islocal is true)
    INTEGER(I4B), INTENT(IN), OPTIONAL :: dim
    !! dimension of the mesh: 0 points, 1 curves, 2 surfaces, 3 volumes;
    !! default is nsd
    INTEGER(I4B), INTENT(IN), OPTIONAL :: entityNum
    !! entity number of the mesh of dimension dim
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    INTEGER(I4B) :: ans
    !! number of vertex nodes of the elements
  END FUNCTION obj_GetTotalVertexNodes2
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetOrientation@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-14
! summary: Get the orientation flags of an element
!
! The method works on the mesh of dimension dim (default nsd), given by
! GetMeshPointer. See the mesh method GetOrientation.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!   cellOrient(:) : INTEGER(I4B), intent(inout) - cell orientation
!   faceOrient(:,:) : INTEGER(I4B), intent(inout) - face orientation, one
!     column per face
!   edgeOrient(:) : INTEGER(I4B), intent(inout) - edge orientation
!   tCellOrient : INTEGER(I4B), intent(out) - number of entries written in
!     cellOrient
!   tFaceOrient(2) : INTEGER(I4B), intent(out) - number of rows and columns
!     written in faceOrient
!   tEdgeOrient : INTEGER(I4B), intent(out) - number of entries written in
!     edgeOrient
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   dim : INTEGER(I4B), optional, intent(in) - dimension of the mesh: 0
!     points, 1 curves, 2 surfaces, 3 volumes; default is nsd
!   entityNum : INTEGER(I4B), optional, intent(in) - entity number of the mesh
!     of dimension dim
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Examples
!
!```fortran
! PROGRAM main
!   USE AbstractMesh_Class, ONLY: AbstractMesh_
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"
!   TYPE(HDF5File_) :: meshfile
!   TYPE(FEDomain_) :: obj
!   CLASS(AbstractMesh_), POINTER :: mesh
!   INTEGER(I4B) :: cell(5), face(5, 10), edge(20), tface(2)
!   INTEGER(I4B) :: tcell, tedge
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, group="")
!
!   mesh => obj%GetMeshPointer()
!   CALL mesh%InitiateEdgeConnectivity()
!   CALL obj%GetOrientation(cellOrient=cell, faceOrient=face, &
!                           edgeOrient=edge, tCellOrient=tcell, &
!                           tFaceOrient=tface, tEdgeOrient=tedge, &
!                           globalElement=16_I4B)
!   CALL OK(tedge .EQ. 4_I4B, "number of edges")
!   CALL OK(ALL(edge(1:tedge) .EQ. -1), "edge orientation")
!   mesh => NULL()
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE obj_GetOrientation(obj, cellOrient, faceOrient, &
    edgeOrient, tCellOrient, tFaceOrient, &
    tEdgeOrient, globalElement, dim, &
    entityNum, islocal)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(INOUT) :: cellOrient(:)
    !! cell orientation
    INTEGER(I4B), INTENT(INOUT) :: faceOrient(:, :)
    !! face orientation, one column per face
    INTEGER(I4B), INTENT(INOUT) :: edgeOrient(:)
    !! edge orientation
    INTEGER(I4B), INTENT(OUT) :: tCellOrient
    !! number of entries written in cellOrient
    INTEGER(I4B), INTENT(OUT) :: tFaceOrient(2)
    !! number of rows and columns written in faceOrient
    INTEGER(I4B), INTENT(OUT) :: tEdgeOrient
    !! number of entries written in edgeOrient
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    INTEGER(I4B), INTENT(IN), OPTIONAL :: dim
    !! dimension of the mesh: 0 points, 1 curves, 2 surfaces, 3 volumes;
    !! default is nsd
    INTEGER(I4B), INTENT(IN), OPTIONAL :: entityNum
    !! entity number of the mesh of dimension dim
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
  END SUBROUTINE obj_GetOrientation
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetElemTopoIndx@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-23
! summary: Get the topology index of an element
!
! The method works on the mesh of dimension dim (default nsd), given by
! GetMeshPointer. The index is the position of the topology in: Point, Line,
! Triangle, Quadrangle, Tetrahedron, Hexahedron, Prism, Pyramid.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   dim : INTEGER(I4B), optional, intent(in) - dimension of the mesh: 0
!     points, 1 curves, 2 surfaces, 3 volumes; default is nsd
!   entityNum : INTEGER(I4B), optional, intent(in) - entity number of the mesh
!     of dimension dim
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans : INTEGER(I4B) - topology index
!
! See also: [[obj_GetTotalVertexNodes1]]

INTERFACE
  MODULE FUNCTION obj_GetElemTopologyIndx(obj, globalElement, dim, &
    entityNum, islocal) &
    RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    INTEGER(I4B), INTENT(IN), OPTIONAL :: dim
    !! dimension of the mesh: 0 points, 1 curves, 2 surfaces, 3 volumes;
    !! default is nsd
    INTEGER(I4B), INTENT(IN), OPTIONAL :: entityNum
    !! entity number of the mesh of dimension dim
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    INTEGER(I4B) :: ans
    !! topology index
  END FUNCTION obj_GetElemTopologyIndx
END INTERFACE

!----------------------------------------------------------------------------
!                                                 isElementActive@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Returns true if an element is active
!
! The method works on the mesh of dimension dim (default nsd), given by
! GetMeshPointer.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(in) - domain object
!   globalElement : INTEGER(I4B), intent(in) - global element number, or local
!     element number if islocal is true
!   dim : INTEGER(I4B), optional, intent(in) - dimension of the mesh: 0
!     points, 1 curves, 2 surfaces, 3 volumes; default is nsd
!   entityNum : INTEGER(I4B), optional, intent(in) - entity number of the mesh
!     of dimension dim
!   islocal : LOGICAL(LGT), optional, intent(in) - if true, the given element
!     or node number is a local number; default is false
!
! Returns:
!   ans : LOGICAL(LGT) - true if the element is active
!
! See also: [[obj_IsElementPresent]]

INTERFACE
  MODULE FUNCTION obj_isElementActive(obj, globalElement, dim, entityNum, &
    islocal) RESULT(ans)
    CLASS(AbstractDomain_), INTENT(IN) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number, or local element number if islocal is true
    INTEGER(I4B), INTENT(IN), OPTIONAL :: dim
    !! dimension of the mesh: 0 points, 1 curves, 2 surfaces, 3 volumes;
    !! default is nsd
    INTEGER(I4B), INTENT(IN), OPTIONAL :: entityNum
    !! entity number of the mesh of dimension dim
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: islocal
    !! if true, the given element or node number is a local number; default is
    !! false
    LOGICAL(LGT) :: ans
    !! true if the element is active
  END FUNCTION obj_isElementActive
END INTERFACE

!----------------------------------------------------------------------------
!                                                   SetShowTime@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-10
! summary: Set the showTime option
!
! If showTime is true, the time of some domain operations is printed.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(inout) - domain object
!   VALUE : LOGICAL(LGT), intent(in) - value of showTime
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"
!   TYPE(HDF5File_) :: meshfile
!   TYPE(FEDomain_) :: obj
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, group="")
!
!   CALL obj%SetShowTime(.TRUE.)
!   CALL obj%InitiateKdtree()
!   CALL obj%SetShowTime(.FALSE.)
!   CALL OK(obj%IsInitiated(), "domain is not changed")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE obj_SetShowTime(obj, VALUE)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
    !! domain object
    LOGICAL(LGT), INTENT(IN) :: VALUE
    !! value of showTime
  END SUBROUTINE obj_SetShowTime
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetSparsity@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-31
! summary: Set the sparsity pattern of a matrix
!
! Set the sparsity pattern of mat from the mesh of dimension nsd (see the mesh
! method SetSparsity), then finish the pattern.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(inout) - domain object
!   mat : TYPE(CSRMatrix_), intent(inout) - sparse matrix
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: CSRMatrix_
!   USE BaseType, ONLY: DOF_
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE CSRMatrix_Method, ONLY: CSRMatrixInitiate => Initiate
!   USE CSRMatrix_Method, ONLY: GetNNZ
!   USE DOF_Method, ONLY: DOFInitiate => Initiate
!   USE DOF_Method, ONLY: DOFSize => Size
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: I4B
!   USE GlobalData, ONLY: NODES_FMT
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"
!   TYPE(HDF5File_) :: meshfile
!   TYPE(FEDomain_) :: obj
!   TYPE(DOF_) :: dofobj
!   TYPE(CSRMatrix_) :: mat
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, group="")
!
!   CALL DOFInitiate(obj=dofobj, tNodes=[obj%GetTotalNodes()], &
!                    names=["K"], spacecompo=[1_I4B], &
!                    timecompo=[1_I4B], storagefmt=NODES_FMT)
!   CALL CSRMatrixInitiate(obj=mat, ncol=DOFSize(dofobj), &
!                          nrow=DOFSize(dofobj), idof=dofobj, &
!                          jdof=dofobj)
!   CALL obj%SetSparsity(mat=mat)
!   CALL OK(GetNNZ(mat) .EQ. 49_I4B, "number of nonzeros")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_SetSparsity2]]

INTERFACE
  MODULE SUBROUTINE obj_SetSparsity1(obj, mat)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
    !! domain object
    TYPE(CSRMatrix_), INTENT(INOUT) :: mat
    !! sparse matrix
  END SUBROUTINE obj_SetSparsity1
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetSparsity@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-10-12
! summary: Set the sparsity pattern for several domains
!
! Each domain is a physical variable: the block (ivar, jvar) of mat gets the
! pattern between the nodes of domain ivar and domain jvar, found by
! FEDomainConnectivity_. For a RECTANGLE matrix two domains are used (rows and
! columns). All domains must have the same nsd.
!
! Arguments:
!   domains(:) : CLASS(AbstractDomainPointer_), intent(inout) - domains, one
!     per physical variable
!   mat : TYPE(CSRMatrix_), intent(inout) - sparse matrix
!
! See also: [[obj_SetSparsity1]]

INTERFACE AbstractDomainSetSparsity
  MODULE SUBROUTINE obj_SetSparsity2(domains, mat)
    CLASS(AbstractDomainPointer_), INTENT(INOUT) :: domains(:)
    !! domains, one per physical variable
    TYPE(CSRMatrix_), INTENT(INOUT) :: mat
    !! sparse matrix
  END SUBROUTINE obj_SetSparsity2
END INTERFACE AbstractDomainSetSparsity

!----------------------------------------------------------------------------
!                                               SetTotalMedium@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-17
! summary: Add space for media in the elements of a mesh
!
! Call SetTotalMedium of the mesh of dimension dim; n media are added to every
! element.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(inout) - domain object
!   dim : INTEGER(I4B), intent(in) - dimension of the mesh
!   n : INTEGER(I4B), intent(in) - number of media to add
!   entityNum : INTEGER(I4B), optional, intent(in) - entity number; not used
!     by FEDomain_
!
! Examples
!
!```fortran
! PROGRAM main
!   USE AbstractMesh_Class, ONLY: AbstractMesh_
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "../../FEMesh/examples/meshdata/very_small_quad4_mesh_two_region.h5"
!   TYPE(HDF5File_) :: meshfile
!   TYPE(FEDomain_) :: obj
!   CLASS(AbstractMesh_), POINTER :: mesh
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, group="")
!
!   CALL obj%SetTotalMedium(dim=2_I4B, n=1_I4B)
!   CALL OK(obj%GetTotalMaterial(dim=2_I4B, globalElement=18_I4B) &
!           .EQ. 1_I4B, "GetTotalMaterial")
!   CALL obj%SetMaterial(dim=2_I4B, entityNum=1_I4B, medium=1_I4B, &
!                        material=8_I4B)
!   CALL obj%SetMaterial(dim=2_I4B, entityNum=2_I4B, medium=1_I4B, &
!                        material=9_I4B)
!   mesh => obj%GetMeshPointer()
!   CALL OK(mesh%GetMaterial(medium=1_I4B, globalElement=18_I4B) &
!           .EQ. 8_I4B, "entity 1")
!   CALL OK(mesh%GetMaterial(medium=1_I4B, globalElement=21_I4B) &
!           .EQ. 9_I4B, "entity 2")
!   mesh => NULL()
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_SetMaterial]], [[obj_GetTotalMaterial]]

INTERFACE
  MODULE SUBROUTINE obj_SetTotalMedium(obj, dim, n, entityNum)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN) :: dim
    !! dimension of the mesh
    INTEGER(I4B), INTENT(IN) :: n
    !! number of media to add
    INTEGER(I4B), INTENT(IN), OPTIONAL :: entityNum
    !! entity number; not used by FEDomain_
  END SUBROUTINE obj_SetTotalMedium
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetMaterial@setMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-12-09
! summary: Set the material of a medium in an entity
!
! Set the material id of a medium in the elements of entity entityNum of the
! mesh of dimension dim.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(inout) - domain object
!   dim : INTEGER(I4B), intent(in) - dimension of the mesh
!   entityNum : INTEGER(I4B), intent(in) - entity number of the elements
!   medium : INTEGER(I4B), intent(in) - medium number (for example soil,
!     water)
!   material : INTEGER(I4B), intent(in) - material id of the medium
!
! See also: [[obj_SetTotalMedium]]

INTERFACE
  MODULE SUBROUTINE obj_SetMaterial(obj, dim, entityNum, &
    medium, material)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN) :: dim
    !! dimension of the mesh
    INTEGER(I4B), INTENT(IN) :: entityNum
    !! entity number of the elements
    INTEGER(I4B), INTENT(IN) :: medium
    !! medium number (for example soil, water)
    INTEGER(I4B), INTENT(IN) :: material
    !! material id of the medium
  END SUBROUTINE obj_SetMaterial
END INTERFACE

!----------------------------------------------------------------------------
!                                                   SetNodeCoord@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-02-24
! summary: Set the node coordinates of the domain
!
! Set obj = nodeCoord, or with addContribution obj = obj + scale * nodeCoord.
! The shape of nodeCoord must be that of the node coordinates of the domain.
! The node coordinates of the meshes are not changed.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(inout) - domain object
!   nodeCoord(:,:) : REAL(DFP), intent(in) - node coordinates in xiJ format
!   scale : REAL(DFP), optional, intent(in) - scale; default is 1
!   addContribution : LOGICAL(LGT), optional, intent(in) - if true, add scale
!     * nodeCoord; default is false
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: DFP
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"
!   TYPE(HDF5File_) :: meshfile
!   TYPE(FEDomain_) :: obj
!   REAL(DFP), PARAMETER :: tol = 1.0E-8_DFP
!   REAL(DFP), ALLOCATABLE :: xij(:, :)
!   REAL(DFP) :: x(3)
!   INTEGER(I4B) :: tsize
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, group="")
!
!   CALL obj%GetNodeCoord(nodeCoord=xij)
!   CALL obj%SetNodeCoord(nodeCoord=xij, scale=1.0_DFP, &
!                         addContribution=.TRUE.)
!   CALL obj%GetNodeCoord(nodeCoord=x, tsize=tsize, globalNode=4_I4B)
!   CALL OK(ALL(ABS(x(1:2) - 2.0_DFP) .LT. tol), "add")
!   CALL obj%SetNodeCoord(nodeCoord=xij)
!   CALL obj%GetNodeCoord(nodeCoord=x, tsize=tsize, globalNode=4_I4B)
!   CALL OK(ALL(ABS(x(1:2) - 1.0_DFP) .LT. tol), "set")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE obj_SetNodeCoord(obj, nodeCoord, scale, &
    addContribution)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
    !! domain object
    REAL(DFP), INTENT(IN) :: nodeCoord(:, :)
    !! node coordinates in xiJ format
    REAL(DFP), INTENT(IN), OPTIONAL :: scale
    !! scale; default is 1
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: addContribution
    !! if true, add scale * nodeCoord; default is false
  END SUBROUTINE obj_SetNodeCoord
END INTERFACE

!----------------------------------------------------------------------------
!                                                   SetQuality@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Set the quality of the meshes
!
! This routine is not implemented; it raises an error.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(inout) - domain object
!   measures(:) : INTEGER(I4B), intent(in) - quality measures
!   max_measures(:) : REAL(DFP), intent(out) - maximum value of each measure
!   min_measures(:) : REAL(DFP), intent(out) - minimum value of each measure
!   dim : INTEGER(I4B), optional, intent(in) - dimension of the mesh: 0
!     points, 1 curves, 2 surfaces, 3 volumes; default is nsd
!   entityNum : INTEGER(I4B), optional, intent(in) - entity number

INTERFACE
  MODULE SUBROUTINE obj_SetQuality(obj, measures, max_measures, &
    min_measures, dim, entityNum)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN) :: measures(:)
    !! quality measures
    REAL(DFP), INTENT(OUT) :: max_measures(:)
    !! maximum value of each measure
    REAL(DFP), INTENT(OUT) :: min_measures(:)
    !! minimum value of each measure
    INTEGER(I4B), INTENT(IN), OPTIONAL :: dim
    !! dimension of the mesh: 0 points, 1 curves, 2 surfaces, 3 volumes;
    !! default is nsd
    INTEGER(I4B), INTENT(IN), OPTIONAL :: entityNum
    !! entity number
  END SUBROUTINE obj_SetQuality
END INTERFACE

!----------------------------------------------------------------------------
!                                               SetTotalElements@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-11
! summary: Set the number of elements of a dimension
!
! Set tElements(indx) = VALUE. It does not change the meshes.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(inout) - domain object
!   indx : INTEGER(I4B), intent(in) - dimension: 0 points, 1 curves, 2
!     surfaces, 3 volumes
!   VALUE : INTEGER(I4B), intent(in) - number of elements
!
! See also: [[obj_GetParam]]

INTERFACE
  MODULE SUBROUTINE obj_SetTotalElements(obj, indx, VALUE)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
    !! domain object
    INTEGER(I4B), INTENT(IN) :: indx
    !! dimension: 0 points, 1 curves, 2 surfaces, 3 volumes
    INTEGER(I4B), INTENT(IN) :: VALUE
    !! number of elements
  END SUBROUTINE obj_SetTotalElements
END INTERFACE

!----------------------------------------------------------------------------
!                                           InitiateKdtree@MeshDataMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-04-10
! summary: Initiate the kdtree of the domain
!
! Make a kdtree of the node coordinates of the domain (not sorted).
! GetNearestNode and GetNptrsInBox initiate it when needed.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(inout) - domain object
!
! Examples
!
!```fortran
! PROGRAM main
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEDomain_Class, ONLY: FEDomain_
!   USE GlobalData, ONLY: DFP
!   USE GlobalData, ONLY: I4B
!   USE HDF5File_Class, ONLY: HDF5File_
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CHARACTER(*), PARAMETER :: filename = &
!     "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"
!   TYPE(HDF5File_) :: meshfile
!   TYPE(FEDomain_) :: obj
!   REAL(DFP) :: x(3), xn(3, 1)
!   INTEGER(I4B) :: node, nodes(1)
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   CALL meshfile%Initiate(filename=filename, mode="READ")
!   CALL meshfile%OPEN()
!   CALL obj%Initiate(hdf5=meshfile, group="")
!
!   CALL obj%GetNearestNode(qv=[0.1_DFP, 0.9_DFP], x=x, globalNode=node)
!   CALL OK(node .EQ. 3_I4B, "nearest node to (0.1, 0.9)")
!   CALL obj%DeallocateKdtree()
!   CALL obj%InitiateKdtree()
!   CALL obj%GetNearestNode(qv=[0.9_DFP, 0.1_DFP], x=xn, &
!                           globalNode=nodes, nn=1_I4B)
!   CALL OK(nodes(1) .EQ. 2_I4B, "nearest node to (0.9, 0.1)")
!
!   CALL obj%DEALLOCATE()
!   CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_DeallocateKdtree]], [[obj_GetNearestNode1]]

INTERFACE
  MODULE SUBROUTINE obj_InitiateKdtree(obj)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
    !! domain object
  END SUBROUTINE obj_InitiateKdtree
END INTERFACE

!----------------------------------------------------------------------------
!                                     InitiateNodeToElements@MeshDataMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-11-04
! summary: Initiate the node to elements data of the meshes
!
! This method is implemented by the child class (see FEDomain_).
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(inout) - domain object
!
! See also: [[FEDomain_:InitiateNodeToElements]]

INTERFACE
  MODULE SUBROUTINE obj_InitiateNodeToElements(obj)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
    !! domain object
  END SUBROUTINE obj_InitiateNodeToElements
END INTERFACE

!----------------------------------------------------------------------------
!                                       InitiateNodeToNodes@MeshDataMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-11-04
! summary: Initiate the node to nodes data of the meshes
!
! This method is implemented by the child class (see FEDomain_).
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(inout) - domain object
!
! See also: [[FEDomain_:InitiateNodeToNodes]]

INTERFACE
  MODULE SUBROUTINE obj_InitiateNodeToNodes(obj)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
    !! domain object
  END SUBROUTINE obj_InitiateNodeToNodes
END INTERFACE

!----------------------------------------------------------------------------
!                                  InitiateElementToElements@MeshDataMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-11-04
! summary: Initiate the element to elements data of the meshes
!
! This method is implemented by the child class (see FEDomain_).
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(inout) - domain object
!
! See also: [[FEDomain_:InitiateElementToElements]]

INTERFACE
  MODULE SUBROUTINE obj_InitiateElementToElements(obj)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
    !! domain object
  END SUBROUTINE obj_InitiateElementToElements
END INTERFACE

!----------------------------------------------------------------------------
!                                      InitiateFacetElements@MeshDataMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-11-04
! summary: Initiate the facet elements of the meshes
!
! This method is implemented by the child class (see FEDomain_).
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(inout) - domain object
!
! See also: [[FEDomain_:InitiateFacetElements]]

INTERFACE
  MODULE SUBROUTINE obj_InitiateFacetElements(obj)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
    !! domain object
  END SUBROUTINE obj_InitiateFacetElements
END INTERFACE

!----------------------------------------------------------------------------
!                                  InitiateExtraNodeToNodes@MeshDataMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-11-04
! summary: Initiate the extra node to nodes data of the meshes
!
! This method is implemented by the child class (see FEDomain_).
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(inout) - domain object
!
! See also: [[FEDomain_:InitiateExtraNodeToNodes]]

INTERFACE
  MODULE SUBROUTINE obj_InitiateExtraNodeToNodes(obj)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
    !! domain object
  END SUBROUTINE obj_InitiateExtraNodeToNodes
END INTERFACE

!----------------------------------------------------------------------------
!                              SetAbstractDomainFacetElement@MeshDataMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-04-14
! summary: Set the domain boundary facet elements
!
! This routine is not implemented; it does nothing.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(inout) - domain object

INTERFACE
  MODULE SUBROUTINE obj_SetDomainFacetElement(obj)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
    !! domain object
  END SUBROUTINE obj_SetDomainFacetElement
END INTERFACE

!----------------------------------------------------------------------------
!                                                 SetMeshmap@MeshDataMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-05-20
! summary: Set the mesh map
!
! This routine is not implemented; it does nothing.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(inout) - domain object

INTERFACE
  MODULE SUBROUTINE obj_SetMeshmap(obj)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
    !! domain object
  END SUBROUTINE obj_SetMeshmap
END INTERFACE

!----------------------------------------------------------------------------
!                                        SetMeshFacetElement@MeshDataMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-05-20
! summary: Set the mesh facet elements
!
! This routine is not implemented; it does nothing.
!
! Arguments:
!   obj : CLASS(AbstractDomain_), intent(inout) - domain object

INTERFACE
  MODULE SUBROUTINE obj_SetMeshFacetElement(obj)
    CLASS(AbstractDomain_), INTENT(INOUT) :: obj
    !! domain object
  END SUBROUTINE obj_SetMeshFacetElement
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE AbstractDomain_Class
