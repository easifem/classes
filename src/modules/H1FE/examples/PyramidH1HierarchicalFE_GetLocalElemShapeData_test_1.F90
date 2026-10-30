!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise PyramidH1HierarchicalFE_GetLocalElemShapeData

PROGRAM main
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: ElemShapeData_
USE GlobalData, ONLY: I4B
USE PyramidH1HierarchicalFE_Class, ONLY: FE_ => PyramidH1HierarchicalFE_
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(FE_) :: obj
TYPE(ElemShapeData_) :: shapes
TYPE(QuadraturePoint_) :: quad

! This operation is currently a WIP placeholder.
CALL obj%GetLocalElemShapeData(elemsd=shapes, quad=quad)
CALL OK(shapes%nips .GT. 0_I4B, "Local shapes populated")
CALL obj%DEALLOCATE()
END PROGRAM main
