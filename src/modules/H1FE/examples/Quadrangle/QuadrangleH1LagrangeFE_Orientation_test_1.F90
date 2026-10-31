!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise QuadrangleH1LagrangeFE_Orientation

PROGRAM main
USE BaseType, ONLY: TypeElemNameOpt
USE BaseType, ONLY: TypeInterpolationOpt
USE GlobalData, ONLY: I4B
USE QuadrangleH1LagrangeFE_Class, ONLY: FE_ => QuadrangleH1LagrangeFE_
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(FE_) :: obj
INTEGER(I4B) :: nptrs(16), orient(3, 4), ii

CALL obj%Initiate(elemType=TypeElemNameOpt%Quadrangle, nsd=2_I4B, &
                  baseContinuity="H1", baseInterpolation="Lagrange", &
                  order=2_I4B, &
                  ipType=TypeInterpolationOpt%Equidistance, &
                  quadratureIsHomogeneous=.TRUE., &
                  quadratureIsOrder=.TRUE., &
                  quadratureOrder=[6_I4B])

CALL obj%SetOrder(order=3_I4B)
nptrs = [(ii, ii=1, 16)]
orient = 1_I4B
orient(1, 1) = -1_I4B
CALL obj%SetOrientation(faceOrient=orient, tFace=4_I4B)
CALL obj%FixOrientOfNptrs(nptrs=nptrs, tsize=16_I4B)
CALL OK(ALL(nptrs(1:4) .EQ. [(ii, ii=1, 4)]), "Vertices retained")
CALL OK(ALL(nptrs(5:6) .EQ. [6_I4B, 5_I4B]), &
        "Reversed first edge")
CALL obj%DEALLOCATE()
END PROGRAM main
