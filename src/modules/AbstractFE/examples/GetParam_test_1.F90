!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise AbstractFE_ GetParam operations
!
! Uses a concrete triangle finite element to exercise the public API.
!
PROGRAM main
USE BaseType, ONLY: TypeElemNameOpt
USE BaseType, ONLY: TypeInterpolationOpt
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE Test_Method, ONLY: OK
USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
IMPLICIT NONE
TYPE(TriangleH1LagrangeFE_) :: obj
REAL(DFP) :: coords(3, 3)
INTEGER(I4B) :: nrow, ncol

CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
                  baseContinuity="H1", baseInterpolation="Lagrange", &
                  order=1_I4B, &
                  ipType=TypeInterpolationOpt%Equidistance, &
                  quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
                  quadratureOrder=[4_I4B])

CALL OK(obj%GetTopologyType() .EQ. TypeElemNameOpt%Triangle, "Topology")
CALL OK(obj%GetBaseContinuity() .EQ. "H1", "Continuity code")
CALL OK(LEN_TRIM(obj%GetBaseInterpolation()) .GT. 0, "Interpolation code")
CALL OK(obj%GetIpType() .EQ. TypeInterpolationOpt%Equidistance, "Point type")
CALL obj%GetRefElemCoord(ans=coords, nrow=nrow, ncol=ncol)
CALL OK(nrow .EQ. 3_I4B .AND. ncol .EQ. 3_I4B, "Reference vertex layout")
CALL OK(ALL(ABS(coords(3, :)) .LT. 1.0E-12_DFP), "Planar reference vertices")
CALL obj%DEALLOCATE()
END PROGRAM main
