!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise PrismH1LagrangeFE_BaseGetFacetQuadraturePoints

PROGRAM main
USE BaseType, ONLY: QuadraturePoint_
USE GlobalData, ONLY: I4B
USE PrismH1LagrangeFE_Class, ONLY: FE_ => PrismH1LagrangeFE_
USE QuadraturePoint_Method, ONLY: GetTotalQuadraturePoints
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(FE_) :: obj
TYPE(QuadraturePoint_) :: quad, facetQuad

! Topology operation is currently a WIP placeholder.
CALL obj%GetFacetQuadraturePoints( &
  quad=quad, facetQuad=facetQuad, localFaceNumber=1_I4B)
CALL OK(GetTotalQuadraturePoints(facetQuad) .GT. 0_I4B, &
        "Facet quadrature populated")
END PROGRAM main
