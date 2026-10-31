!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise OneDimOrthogonalFE_Constructor

PROGRAM main
USE GlobalData, ONLY: I4B
USE OneDimOrthogonalFE_Class, ONLY: FE_ => OneDimOrthogonalFE_
USE OneDimOrthogonalFE_Class, ONLY: NewFE => OneDimOrthogonalFEPointer
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(FE_), POINTER :: ptr

ptr => NewFE(baseContinuity="H1", order=2_I4B)
CALL OK(ASSOCIATED(ptr), "Configured constructor allocates a target")
CALL OK(ptr%isInit, "Target is initialized")
CALL OK(ptr%GetOrder() .EQ. 2_I4B, "Constructor order")
CALL ptr%DEALLOCATE()
DEALLOCATE (ptr)
END PROGRAM main
