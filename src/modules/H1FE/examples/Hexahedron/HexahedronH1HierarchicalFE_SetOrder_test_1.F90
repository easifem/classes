!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise HexahedronH1HierarchicalFE_SetOrder

PROGRAM main
USE GlobalData, ONLY: I4B
USE HexahedronH1HierarchicalFE_Class, ONLY: &
  FE_ => HexahedronH1HierarchicalFE_
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(FE_) :: obj

! This operation is currently a WIP placeholder.
CALL obj%SetOrder(order=1_I4B)
CALL OK(obj%opt%GetTotalDOF() .GT. 0_I4B, "SetOrder establishes DOFs")
CALL obj%DEALLOCATE()
END PROGRAM main
