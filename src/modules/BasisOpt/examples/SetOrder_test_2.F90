!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Update hierarchical cell and face orders

PROGRAM main
USE BaseType, ONLY: TypeElemNameOpt
USE BaseType, ONLY: TypeInterpolationOpt
USE BasisOpt_Class, ONLY: BasisOpt_
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(BasisOpt_) :: obj
INTEGER(I4B) :: cell(3), faces(3, 3), tsize, nrow, ncol

CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
                  baseContinuity="H1", baseInterpolation="Hierarchy", &
                  cellOrder=[2_I4B], tcell=2_I4B, &
                  cellOrient=[1_I4B, 2_I4B, 3_I4B], &
                  faceOrder=RESHAPE([2_I4B], [1, 1]), &
                  faceOrient=RESHAPE([1_I4B], [1, 1]), tface=3_I4B, &
                  ipType=TypeInterpolationOpt%Equidistance, &
                  quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
                  quadratureOrder=[6_I4B])

CALL obj%SetOrder(cellOrder=[3_I4B], tcell=2_I4B, &
                  faceOrder=RESHAPE([3_I4B], [1, 1]), tface=3_I4B, &
                  cellOrient=[1_I4B, 2_I4B, 3_I4B], &
                  faceOrient=RESHAPE([1_I4B], [1, 1]))
CALL obj%GetCellOrder(ans=cell, tsize=tsize)
CALL obj%GetFaceOrder(ans=faces, nrow=nrow, ncol=ncol)
CALL OK(tsize .EQ. 2_I4B .AND. ALL(cell(1:tsize) .EQ. 3_I4B), "Cell order")
CALL OK(ncol .EQ. 3_I4B .AND. ALL(faces .EQ. 3_I4B), "Facet orders")
CALL OK(obj%GetTotalDOF() .EQ. 10_I4B, "Cubic hierarchical triangle DOFs")
CALL obj%DEALLOCATE()
END PROGRAM main
