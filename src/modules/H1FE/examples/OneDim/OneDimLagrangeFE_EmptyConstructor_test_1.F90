!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise OneDimLagrangeFE_EmptyConstructor

PROGRAM main
USE OneDimLagrangeFE_Class, ONLY: FE_ => OneDimLagrangeFE_
USE OneDimLagrangeFE_Class, ONLY: NewFE => OneDimLagrangeFEPointer
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(FE_), POINTER :: ptr

ptr => NewFE()
CALL OK(ASSOCIATED(ptr), "Empty constructor allocates a target")
CALL OK(.NOT. ptr%isInit, "Empty target starts uninitialized")
CALL ptr%DEALLOCATE()
DEALLOCATE (ptr)
END PROGRAM main
