!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise PyramidH1HierarchicalFE_SetOrder

PROGRAM main
USE GlobalData, ONLY: I4B
USE PyramidH1HierarchicalFE_Class, ONLY: FE_ => PyramidH1HierarchicalFE_
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(FE_) :: obj

! This operation is currently a WIP placeholder.
CALL obj%SetOrder(order=1_I4B)
CALL OK(obj%opt%GetTotalDOF() .GT. 0_I4B, "SetOrder establishes DOFs")
CALL obj%DEALLOCATE()
END PROGRAM main
