!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise AbstractFE_ FixOrientOfNptrs operations
!
! Uses a concrete triangle finite element to exercise the public API.
!
PROGRAM main
USE BaseType, ONLY: TypeElemNameOpt
USE BaseType, ONLY: TypeInterpolationOpt
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
IMPLICIT NONE
TYPE(TriangleH1LagrangeFE_) :: obj
INTEGER(I4B) :: nptrs(10), faceOrient(3, 3), ii

CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
                  baseContinuity="H1", baseInterpolation="Lagrange", &
                  order=1_I4B, &
                  ipType=TypeInterpolationOpt%Equidistance, &
                  quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
                  quadratureOrder=[4_I4B])

CALL obj%SetOrder(order=3_I4B)
nptrs = [(ii, ii=1, 10)]
faceOrient = 1_I4B
faceOrient(1, 1) = -1_I4B
CALL obj%SetOrientation(faceOrient=faceOrient, tface=3_I4B)
CALL obj%FixOrientOfNptrs(nptrs=nptrs, tsize=10_I4B)
CALL OK(ALL(nptrs(1:3) .EQ. [1_I4B, 2_I4B, 3_I4B]), "Vertices retained")
CALL OK(ALL(nptrs(4:5) .EQ. [5_I4B, 4_I4B]), "First edge DOFs reversed")
CALL OK(ALL(nptrs(6:10) .EQ. [6_I4B, 7_I4B, 8_I4B, 9_I4B, 10_I4B]), &
        "Other DOFs retained")
CALL obj%DEALLOCATE()
END PROGRAM main
