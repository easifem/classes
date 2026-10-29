!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise TriangleH1HieFE_GetLocalElemShapeData

PROGRAM main
USE BaseType, ONLY: TypeElemNameOpt
USE BaseType, ONLY: TypeInterpolationOpt
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: QuadraturePoint_
USE BasisOpt_Class, ONLY: BasisOpt_
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(BasisOpt_) :: obj
TYPE(BasisOpt_) :: geometry
TYPE(ElemShapeData_) :: shapes, geoShapes, facets, geoFacets
TYPE(QuadraturePoint_) :: quad, facetQuad
REAL(DFP) :: xij(2, 3)

CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
                  baseContinuity="H1", baseInterpolation="Hierarchy", &
                  cellOrder=[2_I4B], tcell=2_I4B, &
                  cellOrient=[1_I4B, 2_I4B, 3_I4B], &
                  faceOrder=RESHAPE([2_I4B], [1, 1]), &
                  faceOrient=RESHAPE([1_I4B], [1, 1]), tface=3_I4B, &
                  ipType=TypeInterpolationOpt%Equidistance, &
                  quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
                  quadratureOrder=[6_I4B])

CALL geometry%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
                       baseContinuity="H1", baseInterpolation="Hierarchy", &
                       cellOrder=[1_I4B], tcell=2_I4B, &
                       cellOrient=[1_I4B, 2_I4B, 3_I4B], &
                       faceOrder=RESHAPE([1_I4B], [1, 1]), &
                       faceOrient=RESHAPE([1_I4B], [1, 1]), tface=3_I4B, &
                       ipType=TypeInterpolationOpt%Equidistance, &
                       quadratureIsHomogeneous=.TRUE., &
                       quadratureIsOrder=.TRUE., &
                       quadratureOrder=[6_I4B])
xij(:, 1) = [0.0_DFP, 0.0_DFP]
xij(:, 2) = [1.0_DFP, 0.0_DFP]
xij(:, 3) = [0.0_DFP, 1.0_DFP]
CALL obj%Triangle_GetQuadraturePoints(quad)
CALL obj%TriangleH1HieFE_GetLocalElemShapeData(elemsd=shapes, quad=quad)
CALL geometry%TriangleH1HieFE_GetLocalElemShapeData( &
  elemsd=geoShapes, quad=quad)
CALL OK(shapes%nns .EQ. obj%GetTotalDOF(), "Local basis size")
CALL OK(shapes%nips .GT. 0_I4B, "Local quadrature size")
CALL OK(ALL(ABS(SUM(shapes%N(1:3, 1:shapes%nips), &
                    DIM=1) - 1.0_DFP) .LT. 1.0E-10_DFP), &
                    "Vertex partition of unity")
CALL obj%TriangleH1HieFE_GetGlobalElemShapeData( &
  elemsd=shapes, xij=xij, geoelemsd=geoShapes)
CALL OK(ALL(shapes%coord(1:2, 1:shapes%nips) .GE. -1.0E-10_DFP), &
        "Physical coordinates inside the unit cell")
CALL obj%Triangle_GetFacetQuadraturePoints( &
  quad=quad, facetQuad=facetQuad, localFaceNumber=1_I4B)
CALL obj%TriangleH1HieFE_GetLocalFacetElemShapeData( &
  elemsd=shapes, facetElemsd=facets, quad=quad, facetQuad=facetQuad, &
  localFaceNumber=1_I4B)
CALL geometry%TriangleH1HieFE_GetLocalFacetElemShapeData( &
  elemsd=geoShapes, facetElemsd=geoFacets, quad=quad, &
  facetQuad=facetQuad, localFaceNumber=1_I4B)
CALL OK(facets%nns .EQ. 3_I4B, "Quadratic facet basis size")
CALL obj%TriangleH1HieFE_GetGlobalFacetElemShapeData( &
  elemsd=shapes, facetElemsd=facets, geoElemsd=geoShapes, &
  geoFacetElemsd=geoFacets, xij=xij, localFaceNumber=1_I4B)
CALL OK(facets%nips .GT. 0_I4B, "Mapped facet data")
CALL obj%DEALLOCATE()
CALL geometry%DEALLOCATE()
END PROGRAM main
