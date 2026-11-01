!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise Quadrangle_FixOrientOfNptrs

PROGRAM main
USE BaseType, ONLY: TypeElemNameOpt
USE BaseType, ONLY: TypeInterpolationOpt
USE BasisOpt_Class, ONLY: BasisOpt_
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(BasisOpt_) :: obj
INTEGER(I4B) :: nptrs(16), orient(3, 4), ii

CALL obj%Initiate(elemType=TypeElemNameOpt%Quadrangle, nsd=2_I4B, &
                  baseContinuity="H1", baseInterpolation="Lagrange", &
                  order=2_I4B, &
                  ipType=TypeInterpolationOpt%Equidistance, &
                  quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
                  quadratureOrder=[6_I4B])

CALL obj%SetOrder(order=3_I4B)
nptrs = [(ii, ii=1, 16)]
orient = 1_I4B
orient(1, 1) = -1_I4B
CALL obj%SetCellOrientation(cellOrient=[-1_I4B], tCell=1_I4B)
CALL obj%SetEdgeOrientation(edgeOrient=[1_I4B], tEdge=1_I4B)
CALL obj%SetFaceOrientation(faceOrient=orient, tFace=4_I4B)
CALL obj%Quadrangle_FixOrientOfNptrs(nptrs=nptrs, tsize=16_I4B)
CALL OK(ALL(nptrs(1:4) .EQ. [(ii, ii=1, 4)]), "Vertices retained")
CALL OK(ALL(nptrs(5:6) .EQ. [6_I4B, 5_I4B]), &
        "Reversed first edge")
CALL obj%DEALLOCATE()
END PROGRAM main
