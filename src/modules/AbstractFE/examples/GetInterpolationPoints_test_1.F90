!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise AbstractFE_ GetInterpolationPoints operations
!
! Uses a concrete triangle finite element to exercise the public API.
!
PROGRAM main
USE BaseType, ONLY: TypeElemNameOpt
USE BaseType, ONLY: TypeInterpolationOpt
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE Test_Method, ONLY: OK
USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
IMPLICIT NONE
TYPE(TriangleH1LagrangeFE_) :: obj
REAL(DFP) :: xij(2, 3), points(3, 6)
INTEGER(I4B) :: nrow, ncol, count, order

CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
                  baseContinuity="H1", baseInterpolation="Lagrange", &
                  order=1_I4B, &
                  ipType=TypeInterpolationOpt%Equidistance, &
                  quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
                  quadratureOrder=[4_I4B])

xij(:, 1) = [0.0_DFP, 0.0_DFP]
xij(:, 2) = [1.0_DFP, 0.0_DFP]
xij(:, 3) = [0.0_DFP, 1.0_DFP]
CALL obj%SetOrder(order=2_I4B)
CALL obj%GetParam(order=order)
CALL OK(order .EQ. 2_I4B, "Child order setter")
count = obj%GetTotalInterpolationPoints(order=[2_I4B], &
                                        ipType=[TypeInterpolationOpt%Equidistance])
CALL OK(count .EQ. 6_I4B, "Quadratic triangle point count")
CALL obj%GetInterpolationPoints(xij=xij, ans=points, &
                                nrow=nrow, ncol=ncol, order=[2_I4B], &
                                ipType=[TypeInterpolationOpt%Equidistance])
CALL OK(ncol .EQ. count, "Interpolation output size")
CALL OK(ALL(points(1:2, 1:ncol) .GE. -1.0E-12_DFP), "Points in triangle")
CALL obj%DEALLOCATE()
END PROGRAM main
