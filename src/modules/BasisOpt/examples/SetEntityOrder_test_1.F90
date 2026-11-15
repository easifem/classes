!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise SetEntityOrder

PROGRAM main
USE BaseType, ONLY: TypeElemNameOpt
USE BaseType, ONLY: TypeInterpolationOpt
USE BasisOpt_Class, ONLY: BasisOpt_
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(BasisOpt_) :: obj
INTEGER(I4B) :: cell(3), face(3, 4), edge(12), nrow, ncol, tsize
LOGICAL(LGT) :: active

CALL obj%Initiate(elemType=TypeElemNameOpt%Quadrangle, nsd=2_I4B, &
                  baseContinuity="H1", baseInterpolation="Lagrange", &
                  order=2_I4B, &
                  ipType=TypeInterpolationOpt%Equidistance, &
                  quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
                  quadratureOrder=[6_I4B])

CALL obj%SetCellOrder(cellOrder=[3_I4B, 2_I4B], tCell=2_I4B)
CALL obj%GetCellOrder(ans=cell, tsize=tsize)
CALL OK(tsize .EQ. 2_I4B, "Active cell orders")
CALL OK(ALL(cell(1:tsize) .EQ. [3_I4B, 2_I4B]), "Cell-order values")
face = 2_I4B
CALL obj%SetFaceOrder(faceOrder=face, tFace=4_I4B)
CALL obj%GetFaceOrder(ans=face, nrow=nrow, ncol=ncol)
CALL OK(nrow .EQ. 3_I4B .AND. ncol .EQ. 4_I4B, "Face-order dimensions")
CALL OK(ALL(face .EQ. 2_I4B), "Face-order values")
edge = 3_I4B
CALL obj%SetEdgeOrder(edgeOrder=edge, tEdge=4_I4B)
CALL obj%GetEdgeOrder(ans=edge, tsize=tsize)
CALL OK(tsize .EQ. 4_I4B .AND. ALL(edge(1:tsize) .EQ. 3_I4B), "Edge orders")
CALL obj%SetEdgeOrder()
CALL obj%GetParam(isEdgeOrder=active)
CALL OK(.NOT. active, "Omitted edgeOrder clears the active flag")
CALL obj%DEALLOCATE()
END PROGRAM main
