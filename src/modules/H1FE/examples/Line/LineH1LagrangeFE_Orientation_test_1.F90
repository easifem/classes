!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise LineH1LagrangeFE_Orientation

PROGRAM main
USE BaseType, ONLY: TypeElemNameOpt
USE BaseType, ONLY: TypeInterpolationOpt
USE GlobalData, ONLY: I4B
USE LineH1LagrangeFE_Class, ONLY: FE_ => LineH1LagrangeFE_
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(FE_) :: obj
INTEGER(I4B) :: cell(3), tsize

CALL obj%Initiate(elemType=TypeElemNameOpt%Line, nsd=1_I4B, &
                  baseContinuity="H1", baseInterpolation="Lagrange", &
                  order=2_I4B, &
                  ipType=TypeInterpolationOpt%Equidistance, &
                  quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
                  quadratureOrder=[6_I4B])
CALL obj%SetOrientation(cellOrient=[1_I4B], tcell=1_I4B)
CALL obj%opt%GetCellOrder(ans=cell, tsize=tsize)
CALL OK(tsize .EQ. 1_I4B, "Cell orientation active size")
CALL obj%DEALLOCATE()
END PROGRAM main
