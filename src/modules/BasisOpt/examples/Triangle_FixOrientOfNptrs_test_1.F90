!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise Triangle_FixOrientOfNptrs

PROGRAM main
USE GlobalData, ONLY: I4B
USE BasisOpt_Class, ONLY: BasisOpt_
USE BaseType, ONLY: TypeElemNameOpt
USE BaseType, ONLY: TypeInterpolationOpt
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(BasisOpt_) :: obj
INTEGER(I4B) :: nptrs(10), orient(3, 3), ii

CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
  baseContinuity="H1", baseInterpolation="Lagrange", &
  order=2_I4B, &
  ipType=TypeInterpolationOpt%Equidistance, &
  quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
  quadratureOrder=[6_I4B])

CALL obj%SetOrder(order=3_I4B)
nptrs = [(ii, ii=1,10)]
orient = 1_I4B
orient(1, 1) = -1_I4B
CALL obj%SetCellOrientation(cellOrient=[-1_I4B], tCell=1_I4B)
CALL obj%SetEdgeOrientation(edgeOrient=[1_I4B], tEdge=1_I4B)
CALL obj%SetFaceOrientation(faceOrient=orient, tFace=3_I4B)
CALL obj%Triangle_FixOrientOfNptrs(nptrs=nptrs, tsize=10_I4B)
CALL OK(ALL(nptrs(1:3) == [(ii, ii=1,3)]), "Vertices retained")
CALL OK(ALL(nptrs(4:5) == [5_I4B, 4_I4B]), &
  "Reversed first edge")
CALL obj%DEALLOCATE()
END PROGRAM main
