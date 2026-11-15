!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise TetrahedronH1LagrangeFE_BaseGetQuadraturePoints

PROGRAM main
USE BaseType, ONLY: QuadraturePoint_
USE GlobalData, ONLY: I4B
USE QuadraturePoint_Method, ONLY: GetTotalQuadraturePoints
USE Test_Method, ONLY: OK
USE TetrahedronH1LagrangeFE_Class, ONLY: FE_ => TetrahedronH1LagrangeFE_
IMPLICIT NONE
TYPE(FE_) :: obj
TYPE(QuadraturePoint_) :: quad

! Topology operation is currently a WIP placeholder.
CALL obj%GetQuadraturePoints(quad)
CALL OK(obj%GetTotalQuadraturePoints() .GT. 0_I4B, "Quadrature populated")
END PROGRAM main
