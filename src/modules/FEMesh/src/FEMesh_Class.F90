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
USE GlobalData, ONLY: DFP, I4B, LGT, stdout, stderr
USE String_Class, ONLY: String
USE ExceptionHandler_Class, ONLY: e
USE AbstractMesh_Class, ONLY: AbstractMesh_

IMPLICIT NONE
PRIVATE

! PUBLIC :: INTERNAL_NODE, BOUNDARY_NODE, DOMAIN_BOUNDARY_NODE,  &
!   & GHOST_NODE, TypeNode

! PUBLIC :: INTERNAL_ELEMENT, BOUNDARY_ELEMENT, DOMAIN_BOUNDARY_ELEMENT,  &
!   & GHOST_ELEMENT, TypeElem

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
  FINAL :: obj_Final
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
!
!----------------------------------------------------------------------------

END MODULE FEMesh_Class
