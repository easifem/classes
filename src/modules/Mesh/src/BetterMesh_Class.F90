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
! date: 2024-03-17
! summary: `Mesh_Class` module contains data type for handling the mesh.

MODULE BetterMesh_Class
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
USE AbstractMesh_Class, ONLY: AbstractMesh_, AbstractMeshDeallocate, &
  & AbstractMeshDisplay, AbstractMeshGetQuery, AbstractMeshImport

IMPLICIT NONE
PRIVATE

PUBLIC :: INTERNAL_NODE, BOUNDARY_NODE, DOMAIN_BOUNDARY_NODE,  &
  & GHOST_NODE, TypeNode

PUBLIC :: INTERNAL_ELEMENT, BOUNDARY_ELEMENT, DOMAIN_BOUNDARY_ELEMENT,  &
  & GHOST_ELEMENT, TypeElem

PUBLIC :: BetterMesh_
PUBLIC :: BetterMeshPointer_
PUBLIC :: BetterMesh_Pointer
PUBLIC :: DEALLOCATE
PUBLIC :: BetterMeshPointerDeallocate

CHARACTER(*), PARAMETER :: modName = "BetterMesh_Class"

!----------------------------------------------------------------------------
!                                                              BetterMesh_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-03-17
! summary: This datatype contains the meta data of a mesh
!
!{!pages/docs-api/BetterMesh/BetterMesh_.md!}

TYPE, EXTENDS(AbstractMesh_) :: BetterMesh_
CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods
  FINAL :: obj_Final
    !! mesh finalizer

  ! IO:
  ! @IOMethods
  ! NA

  !  GET:
  ! @GetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: GetNNE => obj_GetNNE
    !! Get number of nodes in an element

  PROCEDURE, PUBLIC, PASS(obj) :: GetMaxNNE => obj_GetMaxNNE
    !! Get maximum number of nodes in an element

  PROCEDURE, PUBLIC, PASS(obj) :: GetOrder => obj_GetOrder
    !! Returns the order ofthe element of mesh

  PROCEDURE, PUBLIC, PASS(obj) :: GetXidimension => obj_GetXidimension
    !! Return the Xidimension of an element

  PROCEDURE, PASS(obj) :: obj_GetFacetConnectivity1
    !! Return the node nubmers in the facet element

  PROCEDURE, PASS(obj) :: obj_GetFacetConnectivity2
    !! Return the node nubmers in the facet element of a cellElement

  ! SET:
  ! @NodeDataMethods
  ! NA

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

  ! SET:
  ! @SetMethods
  PROCEDURE, PASS(obj) :: SetSparsity1 => obj_setSparsity1
  PROCEDURE, PASS(obj) :: SetSparsity2 => obj_setSparsity2
  PROCEDURE, PASS(obj) :: SetSparsity3 => obj_setSparsity3
  PROCEDURE, PASS(obj) :: SetSparsity4 => obj_setSparsity4

  PROCEDURE, PUBLIC, PASS(obj) :: SetQuality => obj_setQuality
    !! Set mesh quality

END TYPE BetterMesh_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-03-17
! summary: Userdefine datatype which contains the pointer to a mesh

TYPE :: BetterMeshPointer_
  TYPE(BetterMesh_), POINTER :: ptr => NULL()
END TYPE BetterMeshPointer_

!----------------------------------------------------------------------------
!                                     BetterMesh_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-03-17
! summary: This function returns a pointer to an instance of obj_ object

INTERFACE BetterMesh_Pointer
  MODULE FUNCTION obj_Constructor_1() RESULT(ans)
    CLASS(BetterMesh_), POINTER :: ans
  END FUNCTION obj_Constructor_1
END INTERFACE BetterMesh_Pointer

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-03-17
! summary:  finalizer

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(BetterMesh_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                               Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-03-17
! summary: Free up the memory stored in [[obj_]]

INTERFACE DEALLOCATE
  MODULE SUBROUTINE BetterMeshPointerDeallocate(obj)
    TYPE(BetterMeshPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
  END SUBROUTINE BetterMeshPointerDeallocate
END INTERFACE DEALLOCATE

!----------------------------------------------------------------------------
!                                                         GetNNE@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-25
! summary:  Get number of nodes in element

INTERFACE
  MODULE FUNCTION obj_GetNNE(obj, globalElement) RESULT(ans)
    CLASS(BetterMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetNNE
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetNNE@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-25
! summary:  Get number of nodes in element

INTERFACE
  MODULE FUNCTION obj_GetMaxNNE(obj) RESULT(ans)
    CLASS(BetterMesh_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetMaxNNE
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetRefElemPointer@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: Returns the pointer to the reference element

INTERFACE
  MODULE FUNCTION obj_GetRefElemPointer(obj) RESULT(ans)
    CLASS(BetterMesh_), INTENT(IN) :: obj
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
    CLASS(BetterMesh_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetOrder
END INTERFACE

!----------------------------------------------------------------------------
!                                                  GetXidimension@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-01-27
! summary: Returns the xidimension of the mesh

INTERFACE
  MODULE FUNCTION obj_GetXidimension(obj) RESULT(ans)
    CLASS(BetterMesh_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetXidimension
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
  MODULE FUNCTION obj_GetFacetConnectivity1(obj, facetElement, &
    & elementType, isMaster) RESULT(ans)
    CLASS(BetterMesh_), INTENT(IN) :: obj
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
  END FUNCTION obj_GetFacetConnectivity1
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
  MODULE FUNCTION obj_GetFacetConnectivity2(obj, globalElement, &
    & iface) RESULT(ans)
    CLASS(BetterMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    INTEGER(I4B), INTENT(IN) :: iface
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_GetFacetConnectivity2
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetNodeConnectivity@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-03-17
! summary: This routine returns global node numbers in a given global elem

INTERFACE
  MODULE SUBROUTINE obj_GetNodeConnectivity(obj, VALUE)
    CLASS(BetterMesh_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(INOUT) :: VALUE(:, :)
    !! The number of columns are equal to the total number of elements
    !! in the mesh, the number of rows equal to the maximum number of
    !! nodes in the elements of mesh
  END SUBROUTINE obj_GetNodeConnectivity
END INTERFACE

!----------------------------------------------------------------------------
!                               InitiateElementToElements@ElementDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-03-17
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
!- Set `obj%elementData(ielem)%elementType=BOUNDARY_ELEMENT` for those elem
!- Note that at this point boundary element are those which has at least
! one orphan face.
!- Note that at this point these boundary element can be interface element
! between two mesh, or domain-boundary element.

INTERFACE
  MODULE SUBROUTINE obj_InitiateElementToElements(obj)
    CLASS(BetterMesh_), INTENT(INOUT) :: obj
    !! mesh data
  END SUBROUTINE obj_InitiateElementToElements
END INTERFACE

!----------------------------------------------------------------------------
!                                  InitiateBoundaryData@BoundaryDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-03-17
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
    CLASS(BetterMesh_), INTENT(INOUT) :: obj
    !! mesh data
  END SUBROUTINE obj_InitiateBoundaryData
END INTERFACE

!----------------------------------------------------------------------------
!                                     InitiateFacetElements@FacetDataMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-03-17
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
    CLASS(BetterMesh_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_InitiateFacetElements
END INTERFACE

!----------------------------------------------------------------------------
!                                                      SetSparsity@SetMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-03-17
! summary: This routine Set the sparsity pattern in [[CSRMatrix_]] object
!
!# Introduction
!
! This routine Sets the sparsity pattern in [[CSRMatrix_]] object.

INTERFACE
  MODULE SUBROUTINE obj_SetSparsity1(obj, mat, localNodeNumber, lbound, &
    & ubound)
    CLASS(BetterMesh_), INTENT(INOUT) :: obj
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
!                                                    SetSparsity@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-03-17
! summary: This routine Set the sparsity pattern in [[CSRMatrix_]] object

INTERFACE
  MODULE SUBROUTINE obj_SetSparsity2(obj, mat)
    CLASS(BetterMesh_), INTENT(INOUT) :: obj
    !! Mesh_ class
    TYPE(CSRMatrix_), INTENT(INOUT) :: mat
    !! CSRMatrix object
  END SUBROUTINE obj_SetSparsity2
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetSparsity@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-03-17
! summary: This routine Set the sparsity pattern in [[CSRMatrix_]] object

INTERFACE
  MODULE SUBROUTINE obj_SetSparsity3(obj, colMesh, nodeToNode, mat, &
    & ivar, jvar)
    CLASS(BetterMesh_), INTENT(INOUT) :: obj
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
! date: 2024-03-17
! summary: This routine Set the sparsity pattern in [[CSRMatrix_]] object

INTERFACE
  MODULE SUBROUTINE obj_SetSparsity4(obj, colMesh, nodeToNode, mat, &
  & rowGlobalToLocalNodeNum, rowLBOUND, rowUBOUND, colGlobalToLocalNodeNum, &
  & colLBOUND, colUBOUND, ivar, jvar)
    CLASS(BetterMesh_), INTENT(INOUT) :: obj
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
!                                                     SetQuality@setMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-03-17
! summary:  Set mesh quality

INTERFACE
  MODULE SUBROUTINE obj_SetQuality(obj, measures, max_measures, &
    & min_measures, nodeCoord, local_nptrs)
    CLASS(BetterMesh_), INTENT(INOUT) :: obj
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

END MODULE BetterMesh_Class
