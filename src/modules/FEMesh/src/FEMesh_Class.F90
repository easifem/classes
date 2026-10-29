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

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-17
! summary: Finite element mesh class
!
! This module defines FEMesh_, the finite element mesh, and FEMeshPointer_.

MODULE FEMesh_Class
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE ExceptionHandler_Class, ONLY: e
USE GlobalData, ONLY: DFP, I4B, LGT, stderr, stdout
USE String_Class, ONLY: String

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

!----------------------------------------------------------------------------
!                                                              FEMesh_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-17
! summary: Finite element mesh
!
! FEMesh_ is the finite element mesh. It extends AbstractMesh_ and has no
! extra components; all methods are those of AbstractMesh_. The examples of
! AbstractMesh_ use FEMesh_.
!
! See also: [[obj_Constructor_1]]

TYPE, EXTENDS(AbstractMesh_) :: FEMesh_
CONTAINS
  FINAL :: obj_Final
  !! Finalizer, calls Deallocate
END TYPE FEMesh_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-17
! summary: Pointer to a FEMesh_
!
! ptr points to a FEMesh_.
!
! See also: [[FEMeshPointerDeallocate]]

TYPE :: FEMeshPointer_
  TYPE(FEMesh_), POINTER :: ptr => NULL()
END TYPE FEMeshPointer_

!----------------------------------------------------------------------------
!                                     FEMesh_Pointer@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-17
! summary: Get a pointer to a new FEMesh_
!
! Allocate a new empty FEMesh_ and return a pointer to it. Initiate it with
! Initiate.
!
! Returns:
!   ans : CLASS(FEMesh_), pointer - pointer to the new mesh
!
! Examples
!
!```fortran
! PROGRAM main
!   USE AbstractMesh_Class, ONLY: AbstractMeshPointer_
!   USE AbstractMesh_Class, ONLY: AbstractMeshPointerDeallocate
!   USE BaseType, ONLY: math => TypeMathOpt
!   USE ExceptionHandler_Class, ONLY: e
!   USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
!   USE FEMesh_Class, ONLY: FEMesh_
!   USE FEMesh_Class, ONLY: FEMesh_Pointer
!   USE FEMesh_Class, ONLY: FEMeshPointer_
!   USE FEMesh_Class, ONLY: FEMeshPointerDeallocate
!   USE Test_Method, ONLY: OK
!
!   IMPLICIT NONE
!
!   CLASS(FEMesh_), POINTER :: ptr
!   TYPE(FEMeshPointer_), ALLOCATABLE :: fptrs(:)
!   TYPE(AbstractMeshPointer_), ALLOCATABLE :: aptrs(:)
!
!   CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
!   ptr => FEMesh_Pointer()
!   CALL OK(ASSOCIATED(ptr), "FEMesh_Pointer")
!   CALL OK(ptr%IsEmpty(), "new mesh is empty")
!
!   ALLOCATE (fptrs(2))
!   fptrs(1)%ptr => FEMesh_Pointer()
!   CALL FEMeshPointerDeallocate(fptrs)
!   CALL OK(.NOT. ALLOCATED(fptrs), "FEMeshPointerDeallocate")
!
!   ALLOCATE (aptrs(1))
!   aptrs(1)%ptr => ptr
!   ! frees the mesh of ptr; ptr must not be used after this
!   CALL AbstractMeshPointerDeallocate(aptrs)
!   CALL OK(.NOT. ALLOCATED(aptrs), "AbstractMeshPointerDeallocate")
! END PROGRAM main
!```
!
! See also: [[FEMeshPointerDeallocate]]

INTERFACE FEMesh_Pointer
  MODULE FUNCTION obj_Constructor_1() RESULT(ans)
    CLASS(FEMesh_), POINTER :: ans
    !! pointer to the new mesh
  END FUNCTION obj_Constructor_1
END INTERFACE FEMesh_Pointer

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-17
! summary: Finalizer of FEMesh_
!
! Call Deallocate.
!
! Arguments:
!   obj : TYPE(FEMesh_), intent(inout) - mesh object
!
! See also: [[AbstractMesh_:Deallocate]]

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(FEMesh_), INTENT(INOUT) :: obj
    !! mesh object
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                               Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-03-17
! update:
!   - 2026-10-23: The mesh objects are freed (they leaked before)
! summary: Deallocate an array of FEMesh_ pointers
!
! Call Deallocate on every associated mesh of obj, free the mesh object
! (DEALLOCATE(obj(ii)%ptr)), then deallocate obj.
!
! obj must own its meshes: every associated obj(ii)%ptr must have been
! created by ALLOCATE (for example by FEMesh_Pointer), must not point to a
! TARGET variable, and no two entries may point to the same mesh. Other
! pointers to these meshes are dangling after the call.
!
! Arguments:
!   obj(:) : TYPE(FEMeshPointer_), allocatable, intent(inout) - array of mesh
!     pointers
!
! See also: [[obj_Constructor_1]]

INTERFACE DEALLOCATE
  MODULE SUBROUTINE FEMeshPointerDeallocate(obj)
    TYPE(FEMeshPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    !! array of mesh pointers
  END SUBROUTINE FEMeshPointerDeallocate
END INTERFACE DEALLOCATE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE FEMesh_Class
