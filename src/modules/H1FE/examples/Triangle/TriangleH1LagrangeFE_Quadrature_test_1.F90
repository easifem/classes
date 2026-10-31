!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise TriangleH1LagrangeFE_Quadrature

PROGRAM main
USE BaseType, ONLY: TypeElemNameOpt
USE BaseType, ONLY: TypeInterpolationOpt
USE BaseType, ONLY: QuadraturePoint_
USE GlobalData, ONLY: I4B
USE QuadraturePoint_Method, ONLY: GetTotalQuadraturePoints
USE Test_Method, ONLY: OK
USE TriangleH1LagrangeFE_Class, ONLY: FE_ => TriangleH1LagrangeFE_
IMPLICIT NONE
TYPE(FE_) :: obj
TYPE(QuadraturePoint_) :: quad, traceQuad, facetQuad
INTEGER(I4B) :: orders(3), types(3), count

CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
                  baseContinuity="H1", baseInterpolation="Lagrange", &
                  order=2_I4B, &
                  ipType=TypeInterpolationOpt%Equidistance, &
                  quadratureIsHomogeneous=.TRUE., &
                  quadratureIsOrder=.TRUE., &
                  quadratureOrder=[6_I4B])

CALL obj%SetQuadratureOrder(order1=8_I4B)
CALL obj%GetParam(quadratureOrder=orders, quadratureType=types)
CALL OK(orders(1) .EQ. 8_I4B, "Quadrature accuracy")
CALL obj%SetQuadratureType(quadratureType1=types(1))
CALL obj%GetQuadraturePoints(quad)
count = GetTotalQuadraturePoints(quad)
CALL OK(count .EQ. obj%GetTotalQuadraturePoints(), "Quadrature count")
CALL OK(count .GT. 0_I4B, "Nonempty quadrature")
CALL obj%GetFacetQuadraturePoints( &
  quad=traceQuad, facetQuad=facetQuad, localFaceNumber=1_I4B)
CALL OK(GetTotalQuadraturePoints(facetQuad) .GT. 0_I4B, "Facet quadrature")
CALL obj%DEALLOCATE()
END PROGRAM main
