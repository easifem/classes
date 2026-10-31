!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise QuadrangleH1LagrangeFE_FacetFunction

PROGRAM main
USE BaseType, ONLY: TypeElemNameOpt
USE BaseType, ONLY: TypeInterpolationOpt
USE BaseType, ONLY: TypeFEVariableOpt
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: QuadraturePoint_
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE QuadrangleH1LagrangeFE_Class, ONLY: QuadrangleH1LagrangeFE_
USE Test_Method, ONLY: OK
USE UserFunction_Class, ONLY: UserFunction_
IMPLICIT NONE
TYPE(QuadrangleH1LagrangeFE_) :: obj
TYPE(UserFunction_) :: func
REAL(DFP) :: xij(2, 4)
INTEGER(I4B) :: tsize
TYPE(QuadrangleH1LagrangeFE_) :: geometry
TYPE(ElemShapeData_) :: shapes(4), geoShapes(4), facets(4), geoFacets(4)
TYPE(ElemShapeData_) :: cellShapes, geoCellShapes
TYPE(QuadraturePoint_) :: quad(4), facetQuad(4), cellQuad
REAL(DFP) :: ans(10), massMat(10, 10), temp(10)
REAL(DFP) :: facetAns(10)
REAL(DFP), ALLOCATABLE :: samples(:)
INTEGER(I4B) :: ipiv(10), count

CALL obj%Initiate(elemType=TypeElemNameOpt%Quadrangle, nsd=2_I4B, &
                  baseContinuity="H1", baseInterpolation="Lagrange", &
                  order=1_I4B, &
                  ipType=TypeInterpolationOpt%Equidistance, &
                  quadratureIsHomogeneous=.TRUE., &
                  quadratureIsOrder=.TRUE., &
                  quadratureOrder=[4_I4B])

CALL func%Initiate(NAME="constant", returnType=TypeFEVariableOpt%scalar, &
                   argType=TypeFEVariableOpt%space, numArgs=4_I4B, &
                   numReturns=1_I4B)
CALL func%SetScalarConstantVal(val=2.0_DFP)
xij(:, 1) = [0.0_DFP, 0.0_DFP]
xij(:, 2) = [1.0_DFP, 0.0_DFP]
xij(:, 3) = [1.0_DFP, 1.0_DFP]
xij(:, 4) = [0.0_DFP, 1.0_DFP]
! Use cubic facet basis functions.
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
CALL func%DEALLOCATE()
CALL obj%DEALLOCATE()
CALL geometry%DEALLOCATE()
END PROGRAM main
