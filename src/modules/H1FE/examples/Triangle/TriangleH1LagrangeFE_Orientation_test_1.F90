!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise TriangleH1LagrangeFE_Orientation

PROGRAM main
USE BaseType, ONLY: TypeElemNameOpt
USE BaseType, ONLY: TypeInterpolationOpt
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
USE TriangleH1LagrangeFE_Class, ONLY: FE_ => TriangleH1LagrangeFE_
IMPLICIT NONE
TYPE(FE_) :: obj
INTEGER(I4B) :: nptrs(10), orient(3, 3), ii

CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
                  baseContinuity="H1", baseInterpolation="Lagrange", &
                  order=2_I4B, &
                  ipType=TypeInterpolationOpt%Equidistance, &
                  quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
                  quadratureOrder=[6_I4B])

CALL obj%SetOrder(order=3_I4B)
nptrs = [(ii, ii=1, 10)]
orient = 1_I4B
orient(1, 1) = -1_I4B
CALL obj%SetOrientation(faceOrient=orient, tFace=3_I4B)
CALL obj%FixOrientOfNptrs(nptrs=nptrs, tsize=10_I4B)
CALL OK(ALL(nptrs(1:3) .EQ. [(ii, ii=1, 3)]), "Vertices retained")
CALL OK(ALL(nptrs(4:5) .EQ. [5_I4B, 4_I4B]), &
        "Reversed first edge")
CALL obj%DEALLOCATE()
END PROGRAM main
