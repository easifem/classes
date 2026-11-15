!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise PyramidH1LagrangeFE_Deallocate

PROGRAM main
USE PyramidH1LagrangeFE_Class, ONLY: FE_ => PyramidH1LagrangeFE_
USE PyramidH1LagrangeFE_Class, ONLY: Wrapper_ => PyramidH1LagrangeFEPointer_
USE PyramidH1LagrangeFE_Class, ONLY: ReleaseFE => FiniteElementDeallocate
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(FE_), ALLOCATABLE :: values(:)
TYPE(Wrapper_), ALLOCATABLE :: refs(:)
TYPE(FE_), TARGET :: TARGET

ALLOCATE (values(2))
CALL ReleaseFE(values)
CALL OK(.NOT. ALLOCATED(values), "Value vector released")
ALLOCATE (refs(2))
refs(1)%ptr => TARGET
CALL ReleaseFE(refs)
CALL OK(.NOT. ALLOCATED(refs), "Pointer wrappers released")
CALL OK(.NOT. TARGET%isInit, "Target reset and still accessible")
END PROGRAM main
