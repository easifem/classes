!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise PrismH1LagrangeFE_BaseSetQuadratureType

PROGRAM main
USE GlobalData, ONLY: I4B
USE PrismH1LagrangeFE_Class, ONLY: FE_ => PrismH1LagrangeFE_
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(FE_) :: obj
INTEGER(I4B) :: types(3)

! Topology operation is currently a WIP placeholder.
CALL obj%SetQuadratureType(quadratureType1=1_I4B)
CALL obj%GetParam(quadratureType=types)
CALL OK(types(1) .EQ. 1_I4B, "Quadrature type set")
END PROGRAM main
