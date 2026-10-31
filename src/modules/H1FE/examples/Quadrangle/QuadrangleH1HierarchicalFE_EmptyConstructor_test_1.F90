!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise QuadrangleH1HierarchicalFE_EmptyConstructor

PROGRAM main
USE QuadrangleH1HierarchicalFE_Class, ONLY: &
  FE_ => QuadrangleH1HierarchicalFE_
USE QuadrangleH1HierarchicalFE_Class, ONLY: &
  NewFE => QuadrangleH1HierarchicalFEPointer
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(FE_), POINTER :: ptr

ptr => NewFE()
CALL OK(ASSOCIATED(ptr), "Empty constructor allocates a target")
CALL OK(.NOT. ptr%isInit, "Empty target starts uninitialized")
CALL ptr%DEALLOCATE()
DEALLOCATE (ptr)
END PROGRAM main
