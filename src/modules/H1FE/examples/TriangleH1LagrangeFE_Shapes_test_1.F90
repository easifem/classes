!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise TriangleH1LagrangeFE_Shapes

PROGRAM main
USE BaseType, ONLY: TypeElemNameOpt
USE BaseType, ONLY: TypeInterpolationOpt
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: QuadraturePoint_
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE Test_Method, ONLY: OK
USE TriangleH1LagrangeFE_Class, ONLY: FE_ => TriangleH1LagrangeFE_
IMPLICIT NONE
TYPE(FE_) :: obj
TYPE(FE_) :: geometry
TYPE(ElemShapeData_) :: shapes, geoShapes, facets, geoFacets
TYPE(QuadraturePoint_) :: quad, facetQuad
REAL(DFP) :: xij(2, 3)

CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
                  baseContinuity="H1", baseInterpolation="Lagrange", &
                  order=2_I4B, &
                  ipType=TypeInterpolationOpt%Equidistance, &
                  quadratureIsHomogeneous=.TRUE., &
                  quadratureIsOrder=.TRUE., &
                  quadratureOrder=[6_I4B])

CALL geometry%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
                       baseContinuity="H1", baseInterpolation="Lagrange", &
                       order=1_I4B, &
                       ipType=TypeInterpolationOpt%Equidistance, &
                       quadratureIsHomogeneous=.TRUE., &
                       quadratureIsOrder=.TRUE., &
                       quadratureOrder=[6_I4B])
xij(:, 1) = [0.0_DFP, 0.0_DFP]
xij(:, 2) = [1.0_DFP, 0.0_DFP]
xij(:, 3) = [0.0_DFP, 1.0_DFP]
CALL obj%GetQuadraturePoints(quad)
CALL obj%GetLocalElemShapeData(elemsd=shapes, quad=quad)
CALL geometry%GetLocalElemShapeData( &
  elemsd=geoShapes, quad=quad)
CALL OK(shapes%nns .EQ. obj%opt%GetTotalDOF(), "Local basis size")
CALL OK(shapes%nips .GT. 0_I4B, "Local quadrature size")
CALL OK(ALL(ABS(SUM(shapes%N(1:shapes%nns, 1:shapes%nips), &
                    DIM=1) - 1.0_DFP) .LT. 1.0E-10_DFP), &
        "Partition of unity")
CALL obj%GetGlobalElemShapeData( &
  elemsd=shapes, xij=xij, geoelemsd=geoShapes)
CALL OK(ALL(shapes%coord(1:2, 1:shapes%nips) .GE. -1.0E-10_DFP), &
        "Physical coordinates inside the unit cell")
CALL obj%GetFacetQuadraturePoints( &
  quad=quad, facetQuad=facetQuad, localFaceNumber=1_I4B)
CALL obj%GetLocalFacetElemShapeData( &
  elemsd=shapes, facetElemsd=facets, quad=quad, facetQuad=facetQuad, &
  localFaceNumber=1_I4B)
CALL geometry%GetLocalFacetElemShapeData( &
  elemsd=geoShapes, facetElemsd=geoFacets, quad=quad, &
  facetQuad=facetQuad, localFaceNumber=1_I4B)
CALL OK(facets%nns .EQ. 3_I4B, "Quadratic facet basis size")
CALL obj%GetGlobalFacetElemShapeData( &
  elemsd=shapes, facetElemsd=facets, geoElemsd=geoShapes, &
  geoFacetElemsd=geoFacets, xij=xij, localFaceNumber=1_I4B)
CALL OK(facets%nips .GT. 0_I4B, "Mapped facet data")
CALL obj%DEALLOCATE()
CALL geometry%DEALLOCATE()
END PROGRAM main
