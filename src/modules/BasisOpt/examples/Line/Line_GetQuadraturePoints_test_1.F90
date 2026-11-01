!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise Line_GetQuadraturePoints

PROGRAM main
USE BaseType, ONLY: TypeElemNameOpt
USE BaseType, ONLY: TypeInterpolationOpt
USE BaseType, ONLY: QuadraturePoint_
USE BasisOpt_Class, ONLY: BasisOpt_
USE GlobalData, ONLY: I4B
USE QuadraturePoint_Method, ONLY: GetTotalQuadraturePoints
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(BasisOpt_) :: obj
TYPE(QuadraturePoint_) :: quad, traceQuad, facetQuad
INTEGER(I4B) :: orders(3), types(3), count

CALL obj%Initiate(elemType=TypeElemNameOpt%Line, nsd=1_I4B, &
                  baseContinuity="H1", baseInterpolation="Lagrange", &
                  order=2_I4B, &
                  ipType=TypeInterpolationOpt%Equidistance, &
                  quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
                  quadratureOrder=[6_I4B])

CALL obj%Line_SetQuadratureOrder(order=8_I4B)
CALL obj%GetParam(quadratureOrder=orders, quadratureType=types)
CALL OK(orders(1) .EQ. 8_I4B, "Quadrature accuracy")
CALL obj%Line_SetQuadratureType(quadratureType=types(1))
CALL obj%Line_GetQuadraturePoints(quad)
count = GetTotalQuadraturePoints(quad)
CALL OK(count .EQ. obj%GetTotalQuadraturePoints(), "Quadrature count")
CALL OK(count .GT. 0_I4B, "Nonempty quadrature")
CALL obj%Line_GetFacetQuadraturePoints( &
  quad=traceQuad, facetQuad=facetQuad, localFaceNumber=1_I4B)
CALL OK(GetTotalQuadraturePoints(facetQuad) .GT. 0_I4B, "Facet quadrature")
CALL obj%DEALLOCATE()
END PROGRAM main
