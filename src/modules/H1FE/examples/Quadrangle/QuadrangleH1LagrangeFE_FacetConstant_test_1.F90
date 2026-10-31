!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise QuadrangleH1LagrangeFE_FacetConstant

PROGRAM main
USE BaseType, ONLY: TypeElemNameOpt
USE BaseType, ONLY: TypeInterpolationOpt
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: QuadraturePoint_
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE QuadrangleH1LagrangeFE_Class, ONLY: QuadrangleH1LagrangeFE_
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(QuadrangleH1LagrangeFE_) :: obj
TYPE(QuadrangleH1LagrangeFE_) :: geometry
TYPE(ElemShapeData_) :: shapes, facets, geoShapes, geoFacets
TYPE(QuadraturePoint_) :: quad, facetQuad
REAL(DFP) :: xij(2, 4), ans(2), massMat(2, 2), nodal(2)
REAL(DFP), ALLOCATABLE :: samples(:)
INTEGER(I4B) :: ipiv(2), tsize

CALL obj%Initiate(elemType=TypeElemNameOpt%Quadrangle, nsd=2_I4B, &
                  baseContinuity="H1", baseInterpolation="Lagrange", &
                  order=1_I4B, &
                  ipType=TypeInterpolationOpt%Equidistance, &
                  quadratureIsHomogeneous=.TRUE., &
                  quadratureIsOrder=.TRUE., &
                  quadratureOrder=[4_I4B])

CALL geometry%Copy(obj)
xij(:, 1) = [0.0_DFP, 0.0_DFP]
xij(:, 2) = [1.0_DFP, 0.0_DFP]
xij(:, 3) = [1.0_DFP, 1.0_DFP]
xij(:, 4) = [0.0_DFP, 1.0_DFP]
CALL obj%GetGlobalFacetElemShapeData2(geofeptr=geometry, &
                                      elemsd=shapes, facetElemsd=facets, &
                                      geoElemsd=geoShapes, &
                                      geoFacetElemsd=geoFacets, quad=quad, &
                                      facetQuad=facetQuad, &
                                      xij=xij, localFaceNumber=1_I4B)
ALLOCATE (samples(facets%nips))
nodal = 2.0_DFP
CALL obj%GetFacetDOFValue(elemsd=shapes, facetElemsd=facets, &
                          geoElemsd=geoShapes, &
                          geoFacetElemsd=geoFacets, xij=xij, &
                          localFaceNumber=1_I4B, func=nodal, ans=ans, &
                          tsize=tsize, &
                          massMat=massMat, ipiv=ipiv, funcValue=samples, &
                          onlyFaceBubble=.FALSE., tVertices=2_I4B)
CALL OK(tsize .EQ. 2_I4B, "Facet projection size")
CALL OK(ALL(ABS(ans - 2.0_DFP) .LT. 1.0E-10_DFP), &
        "Nodal constant projected")
CALL obj%GetFacetDOFValue(elemsd=shapes, facetElemsd=facets, &
                          xij=xij, localFaceNumber=1_I4B, ans=ans, &
                          tsize=tsize, &
                          massMat=massMat, ipiv=ipiv, funcValue=samples, &
                          onlyFaceBubble=.FALSE.)
CALL OK(ALL(ABS(ans - 1.0_DFP) .LT. 1.0E-10_DFP), "Unit constant projected")
CALL obj%DEALLOCATE()
CALL geometry%DEALLOCATE()
END PROGRAM main
