!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise PrismH1HierarchicalFE_EmptyConstructor

PROGRAM main
USE PrismH1HierarchicalFE_Class, ONLY: FE_ => PrismH1HierarchicalFE_
USE PrismH1HierarchicalFE_Class, ONLY: NewFE => PrismH1HierarchicalFEPointer
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(FE_), POINTER :: ptr

ptr => NewFE()
CALL OK(ASSOCIATED(ptr), "Empty constructor allocates a target")
CALL OK(.NOT. ptr%isInit, "Empty target starts uninitialized")
CALL ptr%DEALLOCATE()
DEALLOCATE (ptr)
END PROGRAM main
