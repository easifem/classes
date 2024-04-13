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

!> authors: Vikas Sharma, Ph. D.
! date: 25 March 2021
! summary: [[Mesh_Class]] module contains data type for handling the mesh.

MODULE Mesh_Class
USE GlobalData
USE Basetype
USE String_Class, ONLY: String
USE ExceptionHandler_Class, ONLY: e
USE FPL, ONLY: ParameterList_
USE HDF5File_Class
USE VTKFile_Class
USE NodeData_Class, ONLY: NodeData_, INTERNAL_NODE, BOUNDARY_NODE,  &
  & DOMAIN_BOUNDARY_NODE, GHOST_NODE, TypeNode
USE ElemData_Class, ONLY: ElemData_, INTERNAL_ELEMENT, BOUNDARY_ELEMENT,  &
  & DOMAIN_BOUNDARY_ELEMENT, GHOST_ELEMENT, TypeElem
USE FacetData_Class, ONLY: InternalFacetData_, BoundaryFacetData_
USE AbstractMesh_Class

IMPLICIT NONE
PRIVATE

PUBLIC :: INTERNAL_NODE, BOUNDARY_NODE, DOMAIN_BOUNDARY_NODE,  &
  & GHOST_NODE, TypeNode

PUBLIC :: INTERNAL_ELEMENT, BOUNDARY_ELEMENT, DOMAIN_BOUNDARY_ELEMENT,  &
  & GHOST_ELEMENT, TypeElem

PUBLIC :: Mesh_
PUBLIC :: MeshPointer_
PUBLIC :: Mesh_Pointer
PUBLIC :: DEALLOCATE
PUBLIC :: meshPointerDeallocate
PUBLIC :: MeshDisplay
PUBLIC :: MeshGetFacetConnectivity

CHARACTER(*), PARAMETER :: modName = "Mesh_Class"

!----------------------------------------------------------------------------
!                                                                     Mesh_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 13 June 2021
! summary: This datatype contains the meta data of a mesh
!
!{!pages/docs-api/Mesh/Mesh_.md!}

TYPE, EXTENDS(AbstractMesh_) :: Mesh_
  INTEGER(I4B) :: elemType = 0
    !! type of element present inside the mesh

  TYPE(ReferenceElement_), PUBLIC, ALLOCATABLE :: facetElements(:)
    !! Facet Elements in the reference element

  CLASS(ReferenceElement_), PUBLIC, POINTER :: refelem => NULL()
    !! Reference element of the mesh (spatial)
    !! TODO: Change refelem to Type(ReferenceElement_)

CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods
  FINAL :: obj_Final
    !! mesh finalizer
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
    !! Deallocate memory occupied by the mesh instance

  ! IO:
  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
    !! Read mesh from hdf5 file
  PROCEDURE, PUBLIC, PASS(obj) :: ExportToVTK => obj_ExportToVTK
    !! Export mesh to a VTKfile
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
    !! Display the mesh
  PROCEDURE, PUBLIC, PASS(obj) :: DisplayFacetElements => &
    & obj_DisplayFacetElements

  ! SET:
  ! @NodeDataMethods
  PROCEDURE, PUBLIC, PASS(obj) :: InitiateExtraNodeToNodes => &
    & obj_InitiateExtraNodetoNodes
  !! Initiate Node to nodes mapping

  ! SET:
  ! @ElementDataMethods
  PROCEDURE, PUBLIC, PASS(obj) :: InitiateElementToElements => &
    & obj_InitiateElementToElements
  !! Initiate element to elements mapping

  ! SET:
  ! @BoundaryDataMethods
  PROCEDURE, PUBLIC, PASS(obj) :: InitiateBoundaryData => &
    & obj_InitiateBoundaryData

  ! SET:
  ! @FacetDataMethods
  PROCEDURE, PUBLIC, PASS(obj) :: InitiateFacetElements => &
    & obj_InitiateFacetElements
  !! Initiate boundary data

  !  GET:
  ! @GetMethods

  PROCEDURE, PUBLIC, PASS(obj) :: GetRefElemPointer =>  &
    & obj_GetRefElemPointer
  !! Returns pointer to the reference element

  PROCEDURE, PUBLIC, PASS(obj) :: GetOrder => &
    & obj_GetOrder
  !! Returns the order ofthe element of mesh

  PROCEDURE, PUBLIC, PASS(obj) :: GetFacetConnectivity =>  &
    & obj_GetFacetConnectivity
  !! Return the node nubmers in the facet element of a cellElement

  PROCEDURE, PUBLIC, PASS(obj) :: GetParam => obj_GetParam
  !! Get parameter of mesh

  ! SET:
  ! @SetMethods
  PROCEDURE, PASS(obj) :: SetSparsity1 => obj_setSparsity1
  PROCEDURE, PASS(obj) :: SetSparsity2 => obj_setSparsity2
  PROCEDURE, PASS(obj) :: SetSparsity3 => obj_setSparsity3
  PROCEDURE, PASS(obj) :: SetSparsity4 => obj_setSparsity4

  PROCEDURE, PUBLIC, PASS(obj) :: SetQuality => obj_setQuality
  !! Set mesh quality

END TYPE Mesh_

!----------------------------------------------------------------------------
!                                                                     Mesh_
!----------------------------------------------------------------------------

! TYPE(Mesh_), PARAMETER, PUBLIC :: TypeMesh = Mesh_( )

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 Sept 2021
! summary: Userdefine datatype which contains the pointer to a mesh

TYPE :: MeshPointer_
  TYPE(Mesh_), POINTER :: ptr => NULL()
END TYPE MeshPointer_

!----------------------------------------------------------------------------
!                                           Mesh_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 June 2021
! summary: This function returns a pointer to an instance of obj_ object

INTERFACE Mesh_Pointer
  MODULE FUNCTION obj_Constructor_1(hdf5, group) RESULT(ans)
    CLASS(Mesh_), POINTER :: ans
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END FUNCTION obj_Constructor_1
END INTERFACE Mesh_Pointer

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(Mesh_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 March 2021
! update: 2024-01-27
! summary: Free up the memory stored in [[obj_]]

INTERFACE DEALLOCATE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(Mesh_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE DEALLOCATE

!----------------------------------------------------------------------------
!                                               Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 March 2021
! update: 2024-01-27
! summary: Free up the memory stored in [[obj_]]

INTERFACE DEALLOCATE
  MODULE SUBROUTINE meshPointerDeallocate(obj)
    TYPE(MeshPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
  END SUBROUTINE meshPointerDeallocate
END INTERFACE DEALLOCATE

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 June 2021
! summary: This routine reads the mesh from a meshFile which is an hdf5
! file
!
!# Introduction
!
! This routine reads the following
!
! meshdata%uid,  meshdata%xidim, meshData%elemType, meshData%minX,
! meshData%minY, meshData%minZ, meshData%maxX, meshData%maxY, meshData%maxZ,
! meshData%X,meshData%Y, meshData%Z, meshData%tElements, meshData%tIntNodes,
! meshData%physicalTag, meshData%InternalNptrs, meshData%elemNumber,
! meshData%connectivity, meshData%boundingEntity
!
! This routine Initiate the local_nptrs data in mesh.
! This routine also Sets the number of nodes in the mesh (tNodes)
! This routine allocate obj%nodeData
! This routine Set localNodeNum and globalNodeNum data inside the
! nodeData

INTERFACE
  MODULE SUBROUTINE obj_Import(obj, hdf5, group, dim, entities)
    CLASS(Mesh_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), OPTIONAL, INTENT(IN) :: group
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: entities(:)
  END SUBROUTINE obj_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Export@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 June 2021
! summary: This routine exports the mesh to a hdf5 file
!
!# Introduction

INTERFACE
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(Mesh_), INTENT(IN) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ExportToVTK@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 Sept 2021
! summary: Export mesh to a VTK file
!
!# Introduction
!
! - If `filename` is present then call [[VTKFile_:InitiateVTKFile]] method
! - If `nodeCoord` is present then write Points by calling
! [[VTKFile_:WritePoints]] method
! - If `content` is present then write cell data by calling
! [[VTKFile_:WriteCells]] methods
! - If openTag is true then write piece info
! - If cloSetag is true then close the piece

INTERFACE
  MODULE SUBROUTINE obj_ExportToVTK(obj, vtkFile, nodeCoord, filename, &
    & OpenTag, Content, CloSetag)
    CLASS(Mesh_), INTENT(IN) :: obj
    TYPE(VTKFile_), INTENT(INOUT) :: vtkFile
    REAL(DFP), OPTIONAL, INTENT(IN) :: nodeCoord(:, :)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: OpenTag
    !! Default is true
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: CloSetag
    !! Default is true
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: Content
    !! Default is true
  END SUBROUTINE obj_ExportToVTK
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 June 2021
! summary: Displays the content of [[obj_]] datatype
!
!# Introduction
!
! This routine displays the content of [[obj_]] datatype
!
!### Usage
!
!```fortran
! call display( obj, 'mesh', stdout )
! call obj%display( 'mesh', stdout )
!```

INTERFACE MeshDisplay
  MODULE SUBROUTINE obj_Display(obj, msg, unitno)
    CLASS(Mesh_), INTENT(INOUT) :: obj
    !! mesh object
    CHARACTER(*), INTENT(IN) :: msg
    !! message on screen
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
    !! unit number of ouput file
  END SUBROUTINE obj_Display
END INTERFACE MeshDisplay

!----------------------------------------------------------------------------
!                                              DisplayFacetElements@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 May 2022
! summary: Display the facet elements

INTERFACE
  MODULE SUBROUTINE obj_DisplayFacetElements(obj, msg, unitno)
    CLASS(Mesh_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE obj_DisplayFacetElements
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetRefElemPointer@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: Returns the pointer to the reference element

INTERFACE
  MODULE FUNCTION obj_GetRefElemPointer(obj) RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    CLASS(ReferenceElement_), POINTER :: ans
  END FUNCTION obj_GetRefElemPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetOrder@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2021-12-08
! update: 2021-12-08
! summary: Returns the order of reference element

INTERFACE
  MODULE FUNCTION obj_GetOrder(obj) RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                           GetFacetConnectivity@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns the connectivity of a facet element
!
!# Introduction
!
! - Returns the connectivity of a given facet element
! - facetElement is local facet element number

INTERFACE
  MODULE FUNCTION MeshGetFacetConnectivity(obj, facetElement, &
    & elementType, isMaster) RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: facetElement
    INTEGER(I4B), INTENT(IN) :: elementType
    LOGICAL(LGT), INTENT(IN) :: isMaster
      !! if isMaster is true then connectivity of facet in master-cell
      !! is returned, otherwise connectivity of facet in slave-cell
      !! is returned. This is only applicable for internal facet element
      !! because for domain facet we do not have slave-cell.
      !! Currently, we do not support slave-cell for meshFacet because
      !! the slave of meshFacet lives in different instance of obj_
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION MeshGetFacetConnectivity
END INTERFACE

!----------------------------------------------------------------------------
!                                           GetFacetConnectivity@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns the connectivity of a facet element of a cellElement
!
!# Introduction
!
! - Returns the connectivity of a given facet element of a cellElement
! - globalElement is global element number of cell number
! - iface is the local face number in globalElement

INTERFACE
  MODULE FUNCTION obj_GetFacetConnectivity(obj, globalElement, &
    & iface, islocal) RESULT(ans)
    CLASS(Mesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    INTEGER(I4B), INTENT(IN) :: iface
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_GetFacetConnectivity
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetQuery@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_GetParam(obj, &
    & isInitiated, isNodeToElementsInitiated, isNodeToNodesInitiated, &
    & isExtraNodeToNodesInitiated, isElementToElementsInitiated, &
    & isBoundaryDataInitiated, isFacetDataInitiated, uid, &
    & xidim, elemType, nsd, maxNptrs, minNptrs, &
    & maxElemNum, minElemNum, tNodes, tElements, &
    & minX, minY, minZ, maxX, maxY, maxZ, &
    & x, y, z, tElements_topology_wise, tElemTopologies, elemTopologies)
    CLASS(Mesh_), INTENT(IN) :: obj
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isInitiated, &
      & isNodeToElementsInitiated, isNodeToNodesInitiated, &
      & isExtraNodeToNodesInitiated, isElementToElementsInitiated, &
      & isBoundaryDataInitiated, isFacetDataInitiated

    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: uid, &
      & xidim, elemType, nsd, maxNptrs, minNptrs, &
      & maxElemNum, minElemNum, tNodes, &
      & tElements, tElements_topology_wise(8), tElemTopologies,  &
      & elemTopologies(8)

    REAL(DFP), OPTIONAL, INTENT(OUT) :: minX, &
      & minY, minZ, maxX, maxY, maxZ, &
      & x, y, z
  END SUBROUTINE obj_GetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                   InitiateExtraNodeToNode@NodeDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 June 2021
! summary: Initiate node to node connectivity data
!
!# Introduction
!
!- This routine generate the node to nodes mapping
!- This mapping is stored inside `obj%nodeData%extraGlobalNodeNum`
!- For a local node number i, `obj%nodeData(i)%ExtraGlobalNodeNum` denotes
! global node data surrounding the local node number used for edge-based
!  stabilization. This list does not include self node.
!
!- This methods needs information about `nodeToNodes`, `nodeToElements`,
! and `elementToElements`. Therefore,
!- If `nodeToNodes` is not Initiated, then this method initiates it.
!- If `nodeToElements` is not Initiated, then this method initiates it.
!- If `elementToElements` is not Initiated, then this method initiates it.

INTERFACE
  MODULE SUBROUTINE obj_InitiateExtraNodetoNodes(obj)
    CLASS(Mesh_), INTENT(INOUT) :: obj
    !! mesh data
  END SUBROUTINE obj_InitiateExtraNodetoNodes
END INTERFACE

!----------------------------------------------------------------------------
!                               InitiateElementToElements@ElementDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:  15 June 2021
! summary: Initiate element to element data
!
!# Introduction
!
!- This routine creates the element surrounding a given element data
!- Before calling this routine make sure the `refelem` in mesh is allocated.
!- By using `refelem`, this routine forms the FacetElements.
!- This routine needs `nodeToElements` information, therefore, if
! `nodeToElements` is not Initiated then it calls `initiateNodeToelements`
! method
!- This method forms the following data:
!- `obj%elementData(ielem)%globalElements`
!- It also identifies those elements which are boundary element of mesh, and
!- Set `obj%elementData(ielem)%elementType=BOUNDARY_ELEMENT` for those element
!- Note that at this point boundary element are those which has at least
! one orphan face.
!- Note that at this point these boundary element can be interface element
! between two mesh, or domain-boundary element.

INTERFACE
  MODULE SUBROUTINE obj_InitiateElementToElements(obj)
    CLASS(Mesh_), INTENT(INOUT) :: obj
    !! mesh data
  END SUBROUTINE obj_InitiateElementToElements
END INTERFACE

!----------------------------------------------------------------------------
!                                  InitiateBoundaryData@BoundaryDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 15 June 2021
! summary: Initiate boundary data
!
!# Introduction
!
! This method construct the boundary element data.
! It marks elements of mesh as BOUNDARY_ELEMENT and INTERNAL_ELEMENT
! In this case boundary elements are those which contains the boundary node.
! Boundary node information is available by reading the mesh file, see
! mesh import method.
! It also forms `obj%elementData(ii)%boundaryData`
!
! This methods needs following information:
!
!- `ElementToElements`
!- `refelem` to construct the FacetElements

INTERFACE
  MODULE SUBROUTINE obj_InitiateBoundaryData(obj)
    CLASS(Mesh_), INTENT(INOUT) :: obj
    !! mesh data
  END SUBROUTINE obj_InitiateBoundaryData
END INTERFACE

!----------------------------------------------------------------------------
!                                     InitiateFacetElements@FacetDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 11 April 2022
! summary: Compute the total number of facet elements in the mesh
!
!# Introduction
!
! This routine needs following information:
!
!- `ElementToElements`
!- `BoundaryData`
!
! It makes following data
!
!- `InternalFacetData`
!- `BoundaryFacetData`
!- `FacetElementType`
!
! Note that at this point all boundaryFacet element are of type
! `DOMAIN_BOUNDARY_ELEMENT`. This information can be corrected only when
! we call SetFacetElementType from Domain_ class. This is because,
! at this point we only know that a boundary facet is a domain boundary
! element, as we have no information about the neighbouring mesh.

INTERFACE
  MODULE SUBROUTINE obj_InitiateFacetElements(obj)
    CLASS(Mesh_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_InitiateFacetElements
END INTERFACE

!----------------------------------------------------------------------------
!                                                      SetSparsity@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine Set the sparsity pattern in [[CSRMatrix_]] object
!
!# Introduction
!
! This routine Sets the sparsity pattern in [[CSRMatrix_]] object.

INTERFACE
  MODULE SUBROUTINE obj_SetSparsity1(obj, mat, localNodeNumber, lbound, &
    & ubound)
    CLASS(Mesh_), INTENT(INOUT) :: obj
    !! [[Mesh_]] class
    TYPE(CSRMatrix_), INTENT(INOUT) :: mat
    !! [[CSRMatrix_]] object
    INTEGER(I4B), INTENT(IN) :: lbound
    INTEGER(I4B), INTENT(IN) :: ubound
    INTEGER(I4B), INTENT(IN) :: localNodeNumber(lbound:ubound)
    !! Global to local node number map
  END SUBROUTINE obj_SetSparsity1
END INTERFACE

!----------------------------------------------------------------------------
!                                                SetSparsity@MeshDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 Oct 2021
! summary: This routine Set the sparsity pattern in [[CSRMatrix_]] object

INTERFACE
  MODULE SUBROUTINE obj_SetSparsity2(obj, mat)
    CLASS(Mesh_), INTENT(INOUT) :: obj
    !! Mesh_ class
    TYPE(CSRMatrix_), INTENT(INOUT) :: mat
    !! CSRMatrix object
  END SUBROUTINE obj_SetSparsity2
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetSparsity@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Oct 2021
! summary: This routine Set the sparsity pattern in [[CSRMatrix_]] object

INTERFACE
  MODULE SUBROUTINE obj_SetSparsity3(obj, colMesh, nodeToNode, mat, &
    & ivar, jvar)
    CLASS(Mesh_), INTENT(INOUT) :: obj
    !! [[Mesh_]] class
    CLASS(AbstractMesh_), INTENT(INOUT) :: colMesh
    !! [[Mesh_]] class
    INTEGER(I4B), INTENT(IN) :: nodeToNode(:)
    !! Node to node connectivity between obj and colMesh
    TYPE(CSRMatrix_), INTENT(INOUT) :: mat
    !! [[CSRMatrix_]] object
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
  END SUBROUTINE obj_SetSparsity3
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetSparsity@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 12 Oct 2021
! summary: This routine Set the sparsity pattern in [[CSRMatrix_]] object

INTERFACE
  MODULE SUBROUTINE obj_SetSparsity4(obj, colMesh, nodeToNode, mat, &
  & rowGlobalToLocalNodeNum, rowLBOUND, rowUBOUND, colGlobalToLocalNodeNum, &
  & colLBOUND, colUBOUND, ivar, jvar)
    CLASS(Mesh_), INTENT(INOUT) :: obj
    !! [[Mesh_]] class
    CLASS(AbstractMesh_), INTENT(INOUT) :: colMesh
    !! [[Mesh_]] class
    INTEGER(I4B), INTENT(IN) :: nodeToNode(:)
    !! node to node connectivity between obj and colMesh
    TYPE(CSRMatrix_), INTENT(INOUT) :: mat
    !! [[CSRMatrix_]] object
    INTEGER(I4B), INTENT(IN) :: rowLBOUND
    INTEGER(I4B), INTENT(IN) :: rowUBOUND
    INTEGER(I4B), INTENT(IN) :: rowGlobalToLocalNodeNum( &
      & rowLBOUND:rowUBOUND)
    !! Global to local node number map
    INTEGER(I4B), INTENT(IN) :: colLBOUND
    INTEGER(I4B), INTENT(IN) :: colUBOUND
    INTEGER(I4B), INTENT(IN) :: colGlobalToLocalNodeNum( &
      & colLBOUND:colUBOUND)
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: jvar
  END SUBROUTINE obj_SetSparsity4
END INTERFACE

!----------------------------------------------------------------------------
!                                                   SetQuality@setMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-02-27
! summary:  Set mesh quality

INTERFACE
  MODULE SUBROUTINE obj_SetQuality(obj, measures, max_measures, &
    & min_measures, nodeCoord, local_nptrs)
    CLASS(Mesh_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: measures(:)
    REAL(DFP), INTENT(OUT) :: max_measures(:)
    REAL(DFP), INTENT(OUT) :: min_measures(:)
    REAL(DFP), INTENT(IN) :: nodeCoord(:, :)
    INTEGER(I4B), INTENT(IN) :: local_nptrs(:)
  END SUBROUTINE obj_SetQuality
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE Mesh_Class
