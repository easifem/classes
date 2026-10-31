!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise OneDimLagrangeFE_Shapes

PROGRAM main
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: ElemShapeData_
USE GlobalData, ONLY: I4B
USE OneDimLagrangeFE_Class, ONLY: FE_ => OneDimLagrangeFE_
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(FE_) :: obj
TYPE(ElemShapeData_) :: shapes
TYPE(QuadraturePoint_) :: quad

CALL obj%Initiate(baseContinuity="H1", &
                  baseInterpolation="Lagrange", order=2_I4B, &
                  quadratureIsOrder=.TRUE., quadratureOrder=6_I4B)
CALL obj%GetQuadraturePoints(quad)
CALL obj%GetLocalElemShapeData(elemsd=shapes, quad=quad)
CALL OK(shapes%nns .EQ. 3_I4B, "Quadratic basis size")
CALL OK(shapes%nips .GT. 0_I4B, "Nonempty quadrature")
CALL obj%DEALLOCATE()
END PROGRAM main
