!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise HexahedronH1HierarchicalFE_EmptyConstructor

PROGRAM main
USE HexahedronH1HierarchicalFE_Class, ONLY: &
  FE_ => HexahedronH1HierarchicalFE_
USE HexahedronH1HierarchicalFE_Class, ONLY: &
  NewFE => HexahedronH1HierarchicalFEPointer
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(FE_), POINTER :: ptr

ptr => NewFE()
CALL OK(ASSOCIATED(ptr), "Empty constructor allocates a target")
CALL OK(.NOT. ptr%isInit, "Empty target starts uninitialized")
CALL ptr%DEALLOCATE()
DEALLOCATE (ptr)
END PROGRAM main
