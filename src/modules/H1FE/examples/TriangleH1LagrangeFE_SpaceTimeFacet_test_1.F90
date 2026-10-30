!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise TriangleH1LagrangeFE_SpaceTimeFacet

PROGRAM main
USE BaseType, ONLY: TypeElemNameOpt
USE BaseType, ONLY: TypeInterpolationOpt
USE BaseType, ONLY: TypeFEVariableOpt
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: QuadraturePoint_
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE LineH1LagrangeFE_Class, ONLY: LineH1LagrangeFE_
USE Test_Method, ONLY: OK
USE TriangleH1LagrangeFE_Class, ONLY: TriangleH1LagrangeFE_
USE UserFunction_Class, ONLY: UserFunction_
IMPLICIT NONE
TYPE(TriangleH1LagrangeFE_) :: obj
TYPE(UserFunction_) :: func
REAL(DFP) :: xij(2, 3)
TYPE(TriangleH1LagrangeFE_) :: geometry
TYPE(LineH1LagrangeFE_) :: timeFE, timeGeometry
TYPE(ElemShapeData_) :: shapes, facets, geoShapes, geoFacets
TYPE(ElemShapeData_) :: timeShapes, geoTimeShapes
TYPE(QuadraturePoint_) :: quad, facetQuad, timeQuad
REAL(DFP) :: ans(4, 4), massMat(16, 16), samples(100, 100), temp(16)
REAL(DFP) :: timeCoords(1, 2)
INTEGER(I4B) :: ipiv(16), r1, r2, c1, c2

CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
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
xij(:, 3) = [0.0_DFP, 1.0_DFP]
CALL geometry%Copy(obj)
CALL obj%SetOrder(order=3_I4B)
CALL obj%SetQuadratureOrder(order1=8_I4B)
CALL obj%GetGlobalFacetElemShapeData2(geofeptr=geometry, &
                                      elemsd=shapes, facetElemsd=facets, &
                                      geoElemsd=geoShapes, &
                                      geoFacetElemsd=geoFacets, quad=quad, &
                                      facetQuad=facetQuad, &
                                      xij=xij, localFaceNumber=1_I4B)
CALL timeFE%Initiate(elemType=TypeElemNameOpt%Line, nsd=1_I4B, &
                     baseContinuity="H1", baseInterpolation="Lagrange", &
                     order=1_I4B, &
                     ipType=TypeInterpolationOpt%Equidistance, &
                     quadratureIsHomogeneous=.TRUE., &
                     quadratureIsOrder=.TRUE., &
                     quadratureOrder=[8_I4B])
CALL timeGeometry%Copy(timeFE)
CALL timeFE%SetOrder(order=3_I4B)
timeCoords(1, :) = [0.0_DFP, 1.0_DFP]
CALL timeFE%GetQuadraturePoints(quad=timeQuad)
CALL timeFE%GetLocalElemShapeData(elemsd=timeShapes, quad=timeQuad)
CALL timeGeometry%GetLocalElemShapeData( &
  elemsd=geoTimeShapes, quad=timeQuad)
CALL timeFE%GetGlobalElemShapeData( &
  elemsd=timeShapes, geoelemsd=geoTimeShapes, xij=timeCoords)
CALL obj%GetFacetDOFValue(elemsd=shapes, facetElemsd=facets, &
                          timeElemsd=timeShapes, xij=xij, times=[0.0_DFP, &
                                                                 1.0_DFP], &
                          localFaceNumber=1_I4B, ans=ans, nrowStart=r1, &
                          nrowEnd=r2, &
                          ncolStart=c1, ncolEnd=c2, massMat=massMat, &
                          ipiv=ipiv, &
                          funcValue=samples, temp=temp, &
                          onlyFaceBubble=.TRUE.)
CALL OK(r1 .EQ. 3_I4B .AND. r2 .EQ. 4_I4B, "Spatial bubble rows")
CALL OK(c1 .EQ. 1_I4B .AND. c2 .EQ. 4_I4B, "All temporal columns")
CALL obj%GetFacetDOFValue(elemsd=shapes, facetElemsd=facets, &
                          timeElemsd=timeShapes, xij=xij, times=[0.0_DFP, &
                                                                 1.0_DFP], &
                          localFaceNumber=1_I4B, func=func, ans=ans, &
                          nrowStart=r1, nrowEnd=r2, ncolStart=c1, &
                          ncolEnd=c2, &
                          massMat=massMat, ipiv=ipiv, funcValue=samples, &
                          temp=temp, &
                          onlyFaceBubble=.TRUE.)
CALL OK(r1 .EQ. 3_I4B .AND. c2 .EQ. 4_I4B, "User-function output range")
CALL func%DEALLOCATE()
CALL obj%DEALLOCATE()
CALL geometry%DEALLOCATE()
CALL timeFE%DEALLOCATE()
CALL timeGeometry%DEALLOCATE()
END PROGRAM main
