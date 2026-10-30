!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise TetrahedronH1LagrangeFE_GetLocalElemShapeData

PROGRAM main
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: ElemShapeData_
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
USE TetrahedronH1LagrangeFE_Class, ONLY: FE_ => TetrahedronH1LagrangeFE_
IMPLICIT NONE
TYPE(FE_) :: obj
TYPE(ElemShapeData_) :: shapes
TYPE(QuadraturePoint_) :: quad

! This operation is currently a WIP placeholder.
CALL obj%GetLocalElemShapeData(elemsd=shapes, quad=quad)
CALL OK(shapes%nips .GT. 0_I4B, "Local shapes populated")
CALL obj%DEALLOCATE()
END PROGRAM main
