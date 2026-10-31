!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise TriangleH1HierarchicalFE_SetOrder

PROGRAM main
USE BaseType, ONLY: TypeElemNameOpt
USE BaseType, ONLY: TypeInterpolationOpt
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
USE TriangleH1HierarchicalFE_Class, ONLY: FE_ => TriangleH1HierarchicalFE_
IMPLICIT NONE
TYPE(FE_) :: obj
INTEGER(I4B) :: faceOrders(3, 6), nrow, ncol

CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
                  baseContinuity="H1", baseInterpolation="Hierarchical", &
                  cellOrder=[2_I4B], tcell=2_I4B, &
                  cellOrient=[1_I4B, 2_I4B, 3_I4B], &
                  faceOrder=RESHAPE([2_I4B], [1, 1]), &
                  faceOrient=RESHAPE([1_I4B], [1, 1]), tface=3_I4B, &
                  ipType=TypeInterpolationOpt%Equidistance, &
                  quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
                  quadratureOrder=[6_I4B])
CALL obj%SetOrder(order=3_I4B)
CALL OK(obj%opt%GetTotalDOF() .EQ. 10_I4B, "Cubic basis DOF count")
CALL obj%opt%GetFaceOrder(ans=faceOrders, nrow=nrow, ncol=ncol)
CALL OK(ncol .EQ. 3_I4B, "Triangle has three face-order columns")
CALL obj%DEALLOCATE()
END PROGRAM main
