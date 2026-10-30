!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise LineH1LagrangeFE_Interpolation

PROGRAM main
USE BaseType, ONLY: TypeElemNameOpt
USE BaseType, ONLY: TypeInterpolationOpt
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE LineH1LagrangeFE_Class, ONLY: FE_ => LineH1LagrangeFE_
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(FE_) :: obj
REAL(DFP) :: xij(3, 8), points(3, 27)
INTEGER(I4B) :: count, nrow, ncol

CALL obj%Initiate(elemType=TypeElemNameOpt%Line, nsd=1_I4B, &
                  baseContinuity="H1", baseInterpolation="Lagrange", &
                  order=2_I4B, &
                  ipType=TypeInterpolationOpt%Equidistance, &
                  quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
                  quadratureOrder=[6_I4B])
CALL obj%GetRefElemCoord(ans=xij, nrow=nrow, ncol=ncol)
count = obj%GetTotalInterpolationPoints(order=[2_I4B], &
                                        ipType=[TypeInterpolationOpt%Equidistance])
CALL obj%GetInterpolationPoints(xij=xij(1:1, 1:2), ans=points, &
                                nrow=nrow, ncol=ncol, order=[2_I4B], &
                                ipType=[TypeInterpolationOpt%Equidistance])
CALL OK(ncol .EQ. count, "Interpolation output count")
CALL OK(count .EQ. 3_I4B, "Quadratic point count")
CALL obj%DEALLOCATE()
END PROGRAM main
