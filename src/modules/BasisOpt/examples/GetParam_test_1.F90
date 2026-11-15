!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise GetParam

PROGRAM main
USE BaseType, ONLY: TypeElemNameOpt
USE BaseType, ONLY: TypeInterpolationOpt
USE BasisOpt_Class, ONLY: BasisOpt_
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(BasisOpt_) :: obj
INTEGER(I4B) :: nrow, ncol, basis(3), orders(3), ipType
REAL(DFP) :: coords(3, 8), alpha(3)
CHARACTER(4) :: interpolation
CHARACTER(2) :: continuity

CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
                  baseContinuity="H1", baseInterpolation="Lagrange", &
                  order=2_I4B, &
                  ipType=TypeInterpolationOpt%Equidistance, &
                  quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
                  quadratureOrder=[6_I4B])

CALL obj%GetParam(basisType=basis, anisoOrder=orders, alpha=alpha, &
                  ipType=ipType, baseInterpolation=interpolation, &
                  baseContinuity=continuity)
CALL OK(obj%GetTotalVertex() .EQ. 3_I4B, "Triangle vertices")
CALL OK(obj%GetTotalFace() .EQ. 3_I4B, "Triangle facets")
CALL OK(obj%GetTopologyType() .EQ. TypeElemNameOpt%Triangle, "Topology")
CALL OK(obj%GetBaseInterpolation() .EQ. "LAGR", "Interpolation code")
CALL OK(obj%GetBaseContinuity() .EQ. "H1", "Continuity code")
CALL OK(obj%GetIpType() .EQ. TypeInterpolationOpt%Equidistance, "Point type")
CALL obj%GetRefElemCoord(ans=coords, nrow=nrow, ncol=ncol)
CALL OK(nrow .EQ. 3_I4B .AND. ncol .EQ. 3_I4B, "Reference coordinate shape")
CALL OK(ALL(ABS(coords(3, 1:3)) .LT. 1.0E-12_DFP), "Reference plane")
CALL obj%DEALLOCATE()
END PROGRAM main
