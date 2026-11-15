!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise AbstractFE_ GetDOFValue operations
!
! Uses a concrete triangle finite element to exercise the public API.
!
PROGRAM main
USE BaseType, ONLY: TypeElemNameOpt
USE BaseType, ONLY: TypeInterpolationOpt
USE BaseType, ONLY: TypeFEVariableOpt
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: QuadraturePoint_
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE Test_Method, ONLY: OK
USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
USE UserFunction_Class, ONLY: UserFunction_
IMPLICIT NONE
TYPE(TriangleH1LagrangeFE_) :: obj
TYPE(UserFunction_) :: func
REAL(DFP) :: xij(2, 3)
INTEGER(I4B) :: tsize
TYPE(TriangleH1LagrangeFE_) :: geometry
TYPE(ElemShapeData_) :: shapes(3), geoShapes(3), facets(3), geoFacets(3)
TYPE(ElemShapeData_) :: cellShapes, geoCellShapes
TYPE(QuadraturePoint_) :: quad(3), facetQuad(3), cellQuad
REAL(DFP) :: ans(10), massMat(10, 10), temp(10)
REAL(DFP) :: facetAns(10)
REAL(DFP), ALLOCATABLE :: samples(:)
INTEGER(I4B) :: ipiv(10), count

CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
                  baseContinuity="H1", baseInterpolation="Lagrange", &
                  order=1_I4B, &
                  ipType=TypeInterpolationOpt%Equidistance, &
                  quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
                  quadratureOrder=[4_I4B])

CALL func%Initiate(NAME="constant", returnType=TypeFEVariableOpt%scalar, &
                   argType=TypeFEVariableOpt%space, numArgs=4_I4B, &
                   numReturns=1_I4B)
CALL func%SetScalarConstantVal(val=2.0_DFP)
xij(:, 1) = [0.0_DFP, 0.0_DFP]
xij(:, 2) = [1.0_DFP, 0.0_DFP]
xij(:, 3) = [0.0_DFP, 1.0_DFP]
! Cubic triangle: three vertices, six edge DOFs and one cell DOF.
CALL geometry%Copy(obj)
CALL obj%SetOrder(order=3_I4B)
CALL obj%SetQuadratureOrder(order1=8_I4B)
CALL obj%GetQuadraturePoints(quad=cellQuad)
CALL obj%GetAllFacetQuadraturePoints( &
  quad=quad, facetQuad=facetQuad, tsize=count)
CALL obj%GetAllLocalFacetElemShapeData(elemsd=shapes, facetElemsd=facets, &
                                       quad=quad, facetQuad=facetQuad, &
                                       tsize=count)
CALL geometry%GetAllLocalFacetElemShapeData( &
  elemsd=geoShapes, facetElemsd=geoFacets, &
  quad=quad, facetQuad=facetQuad, tsize=count)
CALL obj%GetAllGlobalFacetElemShapeData(elemsd=shapes, facetElemsd=facets, &
                                        geoElemsd=geoShapes, &
                                        geoFacetElemsd=geoFacets, xij=xij, &
                                        tsize=count)
CALL obj%GetGlobalElemShapeData2(geofeptr=geometry, elemsd=cellShapes, &
                                 geoelemsd=geoCellShapes, xij=xij, &
                                 quad=cellQuad)
ALLOCATE (samples(MAX(cellShapes%nips, MAXVAL(facets%nips))))
CALL obj%GetFacetDOFValue(elemsd=shapes(1), facetElemsd=facets(1), &
                          xij=xij, times=0.0_DFP, localFaceNumber=1_I4B, &
                          func=func, &
                          ans=facetAns, tsize=tsize, massMat=massMat, &
                          ipiv=ipiv, &
                          funcValue=samples, onlyFaceBubble=.FALSE.)
CALL OK(tsize .EQ. 4_I4B, "Cubic facet projection size")
CALL OK(ALL(ABS(facetAns(1:tsize) - 2.0_DFP) .LT. 1.0E-9_DFP), &
        "User function projected on facet")
CALL obj%GetDOFValue(elemsd=shapes, facetElemsd=facets, &
                     cellElemsd=cellShapes, xij=xij, times=0.0_DFP, &
                     func=func, ans=ans, &
                     tsize=tsize, massMat=massMat, ipiv=ipiv, &
                     funcValue=samples, temp=temp)
CALL OK(tsize .EQ. 10_I4B, "Cubic triangle coefficient count")
! Exercise the interior residual step with the boundary values already set.
CALL obj%GetInCellDOFValue(cellElemsd=cellShapes, func=func, &
                           times=0.0_DFP, ans=ans, temp=temp, tsize=tsize, &
                           massMat=massMat, &
                           ipiv=ipiv, funcValue=samples, offset=9_I4B)
CALL OK(tsize .EQ. 1_I4B, "One interior coefficient")
CALL obj%GetDOFValue(geofeptr=geometry, elemsd=shapes, &
                     geoelemsd=geoShapes, facetElemsd=facets, &
                     geoFacetElemsd=geoFacets, &
                     cellElemsd=cellShapes, geoCellElemsd=geoCellShapes, &
                     quad=quad, &
                     facetQuad=facetQuad, cellQuad=cellQuad, xij=xij, &
                     times=0.0_DFP, &
                     func=func, ans=ans, tsize=tsize, massMat=massMat, &
                     ipiv=ipiv, &
                     funcValue=samples, temp=temp)
CALL OK(tsize .EQ. 10_I4B, "High-level user-function projection")
CALL OK(ALL(ABS(MATMUL(TRANSPOSE( &
                       cellShapes%N(1:10, 1:cellShapes%nips)), ans) &
            - 2.0_DFP) &
            .LT. 1.0E-9_DFP), &
            "Projected constant reproduced at quadrature points")
CALL OK(ALL(ABS(ans(1:3) - 2.0_DFP) .LT. 1.0E-10_DFP), "Vertex values")
CALL func%DEALLOCATE()
CALL obj%DEALLOCATE()
CALL geometry%DEALLOCATE()
END PROGRAM main
