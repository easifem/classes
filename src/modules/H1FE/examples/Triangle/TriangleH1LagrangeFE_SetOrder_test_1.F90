!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise TriangleH1LagrangeFE_SetOrder

PROGRAM main
USE BaseType, ONLY: TypeElemNameOpt
USE BaseType, ONLY: TypeInterpolationOpt
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
USE TriangleH1LagrangeFE_Class, ONLY: FE_ => TriangleH1LagrangeFE_
IMPLICIT NONE
TYPE(FE_) :: obj

CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
                  baseContinuity="H1", baseInterpolation="Lagrange", &
                  order=2_I4B, &
                  ipType=TypeInterpolationOpt%Equidistance, &
                  quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
                  quadratureOrder=[6_I4B])
CALL obj%SetOrder(order=3_I4B)
CALL OK(obj%opt%GetTotalDOF() .EQ. 10_I4B, "Cubic basis DOF count")
CALL obj%DEALLOCATE()
END PROGRAM main
