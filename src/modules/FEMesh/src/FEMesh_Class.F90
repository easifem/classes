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

MODULE FEMesh_Class
USE GlobalData
USE Basetype
USE String_Class, ONLY: String
USE ExceptionHandler_Class, ONLY: e
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

PUBLIC :: FEMesh_
PUBLIC :: FEMeshPointer_
PUBLIC :: FEMesh_Pointer
PUBLIC :: DEALLOCATE
PUBLIC :: FEMeshPointerDeallocate

CHARACTER(*), PARAMETER :: modName = "FEMesh_Class"

!----------------------------------------------------------------------------
!                                                              FEMesh_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-03-17
! summary: This datatype contains the meta data of a mesh
!
!{!pages/docs-api/FEMesh/FEMesh_.md!}

TYPE, EXTENDS(AbstractMesh_) :: FEMesh_
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
  ! NA

  ! SET:
  ! @NodeDataMethods
  ! NA

  ! SET:
  ! @ElementDataMethods
  ! NA

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

END TYPE FEMesh_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-03-17
! summary: Userdefine datatype which contains the pointer to a mesh

TYPE :: FEMeshPointer_
  TYPE(FEMesh_), POINTER :: ptr => NULL()
END TYPE FEMeshPointer_

!----------------------------------------------------------------------------
!                                     FEMesh_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-03-17
! summary: This function returns a pointer to an instance of obj_ object

INTERFACE FEMesh_Pointer
  MODULE FUNCTION obj_Constructor_1() RESULT(ans)
    CLASS(FEMesh_), POINTER :: ans
  END FUNCTION obj_Constructor_1
END INTERFACE FEMesh_Pointer

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-03-17
! summary:  finalizer

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(FEMesh_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                               Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-03-17
! summary: Free up the memory stored in [[obj_]]

INTERFACE DEALLOCATE
  MODULE SUBROUTINE FEMeshPointerDeallocate(obj)
    TYPE(FEMeshPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
  END SUBROUTINE FEMeshPointerDeallocate
END INTERFACE DEALLOCATE

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
    CLASS(FEMesh_), INTENT(INOUT) :: obj
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
    CLASS(FEMesh_), INTENT(INOUT) :: obj
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
    CLASS(FEMesh_), INTENT(INOUT) :: obj
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
    CLASS(FEMesh_), INTENT(INOUT) :: obj
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
    CLASS(FEMesh_), INTENT(INOUT) :: obj
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
    CLASS(FEMesh_), INTENT(INOUT) :: obj
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
    CLASS(FEMesh_), INTENT(INOUT) :: obj
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

END MODULE FEMesh_Class
