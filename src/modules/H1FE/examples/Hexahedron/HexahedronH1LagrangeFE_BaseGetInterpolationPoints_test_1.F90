!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise HexahedronH1LagrangeFE_BaseGetInterpolationPoints

PROGRAM main
USE BaseType, ONLY: TypeInterpolationOpt
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE HexahedronH1LagrangeFE_Class, ONLY: FE_ => HexahedronH1LagrangeFE_
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(FE_) :: obj
REAL(DFP) :: xij(3, 8), points(3, 27)
INTEGER(I4B) :: nrow, ncol

! Topology operation is currently a WIP placeholder.
xij = 0.0_DFP
CALL obj%GetInterpolationPoints(xij=xij, ans=points, &
                                nrow=nrow, ncol=ncol, order=[1_I4B], &
                                ipType=[TypeInterpolationOpt%Equidistance])
CALL OK(ncol .GT. 0_I4B, "Interpolation coordinates populated")
END PROGRAM main
