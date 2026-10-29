!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise AbstractFE_ SetQuadratureOrder operations
!
! Uses a concrete triangle finite element to exercise the public API.
!
PROGRAM main
USE BaseType, ONLY: TypeElemNameOpt
USE BaseType, ONLY: TypeInterpolationOpt
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
IMPLICIT NONE
TYPE(TriangleH1LagrangeFE_) :: obj
INTEGER(I4B) :: orders(3), types(3)

CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
                  baseContinuity="H1", baseInterpolation="Lagrange", &
                  order=1_I4B, &
                  ipType=TypeInterpolationOpt%Equidistance, &
                  quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
                  quadratureOrder=[4_I4B])

CALL obj%SetQuadratureOrder(order1=6_I4B)
CALL obj%GetParam(quadratureOrder=orders, quadratureType=types)
CALL OK(orders(1) .EQ. 6_I4B, "Quadrature accuracy updated")
CALL obj%SetQuadratureType(quadratureType1=types(1))
CALL obj%GetParam(quadratureType=orders)
CALL OK(orders(1) .EQ. types(1), "Quadrature type retained")
CALL obj%DEALLOCATE()
END PROGRAM main
