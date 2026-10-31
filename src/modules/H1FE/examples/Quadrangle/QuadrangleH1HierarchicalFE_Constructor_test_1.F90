!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise QuadrangleH1HierarchicalFE_Constructor

PROGRAM main
USE GlobalData, ONLY: I4B
USE QuadrangleH1HierarchicalFE_Class, ONLY: &
  FE_ => QuadrangleH1HierarchicalFE_
USE QuadrangleH1HierarchicalFE_Class, ONLY: &
  NewFE => QuadrangleH1HierarchicalFEPointer
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(FE_), POINTER :: ptr

ptr => NewFE(cellOrder=[2_I4B], &
             faceOrder=RESHAPE([2_I4B], [1, 1]), nsd=2_I4B, &
             cellOrient=[1_I4B, 2_I4B, 3_I4B], &
             faceOrient=RESHAPE([1_I4B], [1, 1]), quadratureOrder=[6_I4B], &
             quadratureIsHomogeneous=.TRUE.)
CALL OK(ASSOCIATED(ptr), "Configured constructor allocates a target")
CALL OK(ptr%isInit, "Target is initialized")
CALL OK(ptr%opt%GetTotalDOF() .EQ. 9_I4B, &
        "Constructor establishes quadratic DOFs")
CALL ptr%DEALLOCATE()
DEALLOCATE (ptr)
END PROGRAM main
