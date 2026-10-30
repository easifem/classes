!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise OneDimHierarchicalFE_EmptyConstructor

PROGRAM main
USE OneDimHierarchicalFE_Class, ONLY: FE_ => OneDimHierarchicalFE_
USE OneDimHierarchicalFE_Class, ONLY: NewFE => OneDimHierarchicalFEPointer
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(FE_), POINTER :: ptr

ptr => NewFE()
CALL OK(ASSOCIATED(ptr), "Empty constructor allocates a target")
CALL OK(.NOT. ptr%isInit, "Empty target starts uninitialized")
CALL ptr%DEALLOCATE()
DEALLOCATE (ptr)
END PROGRAM main
