!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise PrismH1HierarchicalFE_Constructor

PROGRAM main
USE GlobalData, ONLY: I4B
USE PrismH1HierarchicalFE_Class, ONLY: FE_ => PrismH1HierarchicalFE_
USE PrismH1HierarchicalFE_Class, ONLY: NewFE => PrismH1HierarchicalFEPointer
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(FE_), POINTER :: ptr

ptr => NewFE(order=2_I4B, nsd=3_I4B, quadratureOrder=[6_I4B])
CALL OK(ASSOCIATED(ptr), "Configured constructor allocates a target")
CALL OK(ptr%isInit, "Target is initialized")
CALL OK(ptr%opt%GetTotalDOF() .EQ. 9_I4B, &
        "Constructor establishes quadratic DOFs")
CALL ptr%DEALLOCATE()
DEALLOCATE (ptr)
END PROGRAM main
