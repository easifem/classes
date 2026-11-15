!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise PrismH1LagrangeFE_EmptyConstructor

PROGRAM main
USE PrismH1LagrangeFE_Class, ONLY: FE_ => PrismH1LagrangeFE_
USE PrismH1LagrangeFE_Class, ONLY: NewFE => PrismH1LagrangeFEPointer
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(FE_), POINTER :: ptr

ptr => NewFE()
CALL OK(ASSOCIATED(ptr), "Empty constructor allocates a target")
CALL OK(.NOT. ptr%isInit, "Empty target starts uninitialized")
CALL ptr%DEALLOCATE()
DEALLOCATE (ptr)
END PROGRAM main
