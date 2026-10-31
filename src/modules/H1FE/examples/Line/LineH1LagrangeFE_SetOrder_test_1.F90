!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise LineH1LagrangeFE_SetOrder

PROGRAM main
USE BaseType, ONLY: TypeElemNameOpt
USE BaseType, ONLY: TypeInterpolationOpt
USE GlobalData, ONLY: I4B
USE LineH1LagrangeFE_Class, ONLY: FE_ => LineH1LagrangeFE_
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(FE_) :: obj

CALL obj%Initiate(elemType=TypeElemNameOpt%Line, nsd=1_I4B, &
                  baseContinuity="H1", baseInterpolation="Lagrange", &
                  order=2_I4B, &
                  ipType=TypeInterpolationOpt%Equidistance, &
                  quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
                  quadratureOrder=[6_I4B])
CALL obj%SetOrder(order=3_I4B)
CALL OK(obj%opt%GetTotalDOF() .EQ. 4_I4B, "Cubic basis DOF count")
CALL obj%DEALLOCATE()
END PROGRAM main
