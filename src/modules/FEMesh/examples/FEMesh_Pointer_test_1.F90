!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Create and free pointers to FEMesh_
!
! FEMesh_Pointer returns a pointer to a new empty FEMesh_.
! FEMeshPointerDeallocate deallocates the meshes in an array of
! FEMeshPointer_, and AbstractMeshPointerDeallocate does the same for
! AbstractMeshPointer_. Both deallocate the array.

PROGRAM main
  USE AbstractMesh_Class, ONLY: AbstractMeshPointer_
  USE AbstractMesh_Class, ONLY: AbstractMeshPointerDeallocate
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FEMesh_Class, ONLY: FEMesh_
  USE FEMesh_Class, ONLY: FEMesh_Pointer
  USE FEMesh_Class, ONLY: FEMeshPointer_
  USE FEMesh_Class, ONLY: FEMeshPointerDeallocate
  USE Test_Method, ONLY: OK

  IMPLICIT NONE

  CLASS(FEMesh_), POINTER :: ptr
  TYPE(FEMeshPointer_), ALLOCATABLE :: fptrs(:)
  TYPE(AbstractMeshPointer_), ALLOCATABLE :: aptrs(:)

  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  ptr => FEMesh_Pointer()
  CALL OK(ASSOCIATED(ptr), "FEMesh_Pointer")
  CALL OK(ptr%IsEmpty(), "new mesh is empty")

  ALLOCATE(fptrs(2))
  fptrs(1)%ptr => FEMesh_Pointer()
  CALL FEMeshPointerDeallocate(fptrs)
  CALL OK(.NOT. ALLOCATED(fptrs), "FEMeshPointerDeallocate")

  ALLOCATE(aptrs(1))
  aptrs(1)%ptr => ptr
  CALL AbstractMeshPointerDeallocate(aptrs)
  CALL OK(.NOT. ALLOCATED(aptrs), "AbstractMeshPointerDeallocate")
END PROGRAM main
