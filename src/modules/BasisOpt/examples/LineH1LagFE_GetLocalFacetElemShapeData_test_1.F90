!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise LineH1LagFE_GetLocalFacetElemShapeData

PROGRAM main
USE GlobalData, ONLY: I4B
USE BasisOpt_Class, ONLY: BasisOpt_
USE BaseType, ONLY: TypeElemNameOpt
USE BaseType, ONLY: TypeInterpolationOpt
USE Test_Method, ONLY: OK
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: QuadraturePoint_
IMPLICIT NONE
TYPE(BasisOpt_) :: obj
TYPE(ElemShapeData_) :: shapes, facets
TYPE(QuadraturePoint_) :: quad, facetQuad

CALL obj%Initiate(elemType=TypeElemNameOpt%Line, nsd=1_I4B, &
  baseContinuity="H1", baseInterpolation="Lagrange", &
  order=2_I4B, &
  ipType=TypeInterpolationOpt%Equidistance, &
  quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
  quadratureOrder=[6_I4B])

CALL obj%LineH1LagFE_GetLocalFacetElemShapeData( &
  elemsd=shapes, facetElemsd=facets, quad=quad, facetQuad=facetQuad, &
  localFaceNumber=1_I4B)
CALL OK(facets%nips > 0_I4B, "Point-facet shape data populated")
CALL obj%DEALLOCATE()
END PROGRAM main
