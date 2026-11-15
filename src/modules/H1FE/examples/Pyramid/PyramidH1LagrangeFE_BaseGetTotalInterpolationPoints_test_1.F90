!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise PyramidH1LagrangeFE_BaseGetTotalInterpolationPoints

PROGRAM main
USE BaseType, ONLY: TypeInterpolationOpt
USE GlobalData, ONLY: I4B
USE PyramidH1LagrangeFE_Class, ONLY: FE_ => PyramidH1LagrangeFE_
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(FE_) :: obj
INTEGER(I4B) :: count

! Topology operation is currently a WIP placeholder.
count = obj%GetTotalInterpolationPoints( &
        order=[1_I4B], ipType=[TypeInterpolationOpt%Equidistance])
CALL OK(count .GT. 0_I4B, "Interpolation point count")
END PROGRAM main
