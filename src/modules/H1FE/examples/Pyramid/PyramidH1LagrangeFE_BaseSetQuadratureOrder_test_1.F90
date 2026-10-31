!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise PyramidH1LagrangeFE_BaseSetQuadratureOrder

PROGRAM main
USE GlobalData, ONLY: I4B
USE PyramidH1LagrangeFE_Class, ONLY: FE_ => PyramidH1LagrangeFE_
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(FE_) :: obj
INTEGER(I4B) :: orders(3)

! Topology operation is currently a WIP placeholder.
CALL obj%SetQuadratureOrder(order1=4_I4B)
CALL obj%GetParam(quadratureOrder=orders)
CALL OK(orders(1) .EQ. 4_I4B, "Quadrature order set")
END PROGRAM main
