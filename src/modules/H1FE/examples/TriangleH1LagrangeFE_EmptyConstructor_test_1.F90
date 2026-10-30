!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise TriangleH1LagrangeFE_EmptyConstructor

PROGRAM main
USE Test_Method, ONLY: OK
USE TriangleH1LagrangeFE_Class, ONLY: FE_ => TriangleH1LagrangeFE_
USE TriangleH1LagrangeFE_Class, ONLY: NewFE => TriangleH1LagrangeFEPointer
IMPLICIT NONE
TYPE(FE_), POINTER :: ptr

ptr => NewFE()
CALL OK(ASSOCIATED(ptr), "Empty constructor allocates a target")
CALL OK(.NOT. ptr%isInit, "Empty target starts uninitialized")
CALL ptr%DEALLOCATE()
DEALLOCATE (ptr)
END PROGRAM main
