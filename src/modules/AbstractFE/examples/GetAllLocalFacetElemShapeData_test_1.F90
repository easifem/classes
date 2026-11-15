!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise AbstractFE_ GetAllLocalFacetElemShapeData operations
!
! Uses a concrete triangle finite element to exercise the public API.
!
PROGRAM main
USE BaseType, ONLY: TypeElemNameOpt
USE BaseType, ONLY: TypeInterpolationOpt
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: QuadraturePoint_
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE Test_Method, ONLY: OK
USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
IMPLICIT NONE
TYPE(TriangleH1LagrangeFE_) :: obj
TYPE(TriangleH1LagrangeFE_) :: geometry
TYPE(ElemShapeData_) :: shapes(3), facets(3), geoShapes(3), geoFacets(3)
TYPE(QuadraturePoint_) :: quad(3), facetQuad(3)
REAL(DFP) :: xij(2, 3)
INTEGER(I4B) :: tsize

CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
                  baseContinuity="H1", baseInterpolation="Lagrange", &
                  order=1_I4B, &
                  ipType=TypeInterpolationOpt%Equidistance, &
                  quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
                  quadratureOrder=[4_I4B])

CALL geometry%Copy(obj)
xij(:, 1) = [0.0_DFP, 0.0_DFP]
xij(:, 2) = [1.0_DFP, 0.0_DFP]
xij(:, 3) = [0.0_DFP, 1.0_DFP]
CALL obj%GetFacetQuadraturePoints( &
  quad=quad(1), facetQuad=facetQuad(1), localFaceNumber=1_I4B)
CALL obj%GetAllFacetQuadraturePoints( &
  quad=quad, facetQuad=facetQuad, tsize=tsize)
CALL OK(tsize .EQ. 3_I4B, "Triangle has three facets")
CALL obj%GetLocalFacetElemShapeData(elemsd=shapes(1), &
                                    facetElemsd=facets(1), quad=quad(1), &
                                    facetQuad=facetQuad(1), &
                                    localFaceNumber=1_I4B)
CALL obj%GetAllLocalFacetElemShapeData(elemsd=shapes, &
                                       facetElemsd=facets, quad=quad, &
                                       facetQuad=facetQuad, tsize=tsize)
CALL geometry%GetAllLocalFacetElemShapeData(elemsd=geoShapes, &
                                            facetElemsd=geoFacets, &
                                            quad=quad, facetQuad=facetQuad, &
                                            tsize=tsize)
CALL OK(ALL(facets%nns .EQ. 2_I4B), "Linear facet basis sizes")
CALL obj%GetGlobalFacetElemShapeData(elemsd=shapes(1), &
                                     facetElemsd=facets(1), &
                                     geoElemsd=geoShapes(1), &
                                     geoFacetElemsd=geoFacets(1), xij=xij, &
                                     localFaceNumber=1_I4B)
! Restore local data before mapping every face.
CALL obj%GetAllLocalFacetElemShapeData(elemsd=shapes, &
                                       facetElemsd=facets, quad=quad, &
                                       facetQuad=facetQuad, tsize=tsize)
CALL obj%GetAllGlobalFacetElemShapeData(elemsd=shapes, &
                                        facetElemsd=facets, &
                                        geoElemsd=geoShapes, &
                                        geoFacetElemsd=geoFacets, xij=xij, &
                                        tsize=tsize)
CALL OK(tsize .EQ. 3_I4B, "All facets mapped")
CALL obj%GetGlobalFacetElemShapeData2(geofeptr=geometry, &
                                      elemsd=shapes(1), &
                                      facetElemsd=facets(1), &
                                      geoElemsd=geoShapes(1), &
                                      geoFacetElemsd=geoFacets(1), &
                                      quad=quad(1), &
                                      facetQuad=facetQuad(1), &
                                      xij=xij, localFaceNumber=1_I4B)
CALL OK(facets(1)%nips .GT. 0_I4B, "High-level facet preparation")
CALL obj%DEALLOCATE()
CALL geometry%DEALLOCATE()
END PROGRAM main
