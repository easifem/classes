!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise QuadrangleH1LagrangeFE_Interpolation

PROGRAM main
USE BaseType, ONLY: TypeElemNameOpt
USE BaseType, ONLY: TypeInterpolationOpt
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE QuadrangleH1LagrangeFE_Class, ONLY: FE_ => QuadrangleH1LagrangeFE_
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(FE_) :: obj
REAL(DFP) :: xij(3, 8), points(3, 27)
INTEGER(I4B) :: count, nrow, ncol

CALL obj%Initiate(elemType=TypeElemNameOpt%Quadrangle, nsd=2_I4B, &
                  baseContinuity="H1", baseInterpolation="Lagrange", &
                  order=2_I4B, &
                  ipType=TypeInterpolationOpt%Equidistance, &
                  quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
                  quadratureOrder=[6_I4B])
CALL obj%GetRefElemCoord(ans=xij, nrow=nrow, ncol=ncol)
count = obj%GetTotalInterpolationPoints(order=[2_I4B], &
                                        ipType=[TypeInterpolationOpt%Equidistance])
CALL obj%GetInterpolationPoints(xij=xij(1:2, 1:4), ans=points, &
                                nrow=nrow, ncol=ncol, order=[2_I4B], &
                                ipType=[TypeInterpolationOpt%Equidistance])
CALL OK(ncol .EQ. count, "Interpolation output count")
CALL OK(count .EQ. 9_I4B, "Quadratic point count")
CALL obj%DEALLOCATE()
END PROGRAM main
