!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise LineH1LagFE_GetLocalElemShapeData

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
REAL(DFP) :: xij(1, 2)

CALL obj%Initiate(elemType=TypeElemNameOpt%Line, nsd=1_I4B, &
                  baseContinuity="H1", baseInterpolation="Lagrange", &
                  order=2_I4B, &
                  ipType=TypeInterpolationOpt%Equidistance, &
                  quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
                  quadratureOrder=[6_I4B])

CALL geometry%Initiate(elemType=TypeElemNameOpt%Line, nsd=1_I4B, &
                       baseContinuity="H1", baseInterpolation="Lagrange", &
                       order=1_I4B, &
                       ipType=TypeInterpolationOpt%Equidistance, &
                       quadratureIsHomogeneous=.TRUE., &
                       quadratureIsOrder=.TRUE., &
                       quadratureOrder=[6_I4B])
xij(1, :) = [0.0_DFP, 1.0_DFP]
CALL obj%Line_GetQuadraturePoints(quad)
CALL obj%LineH1LagFE_GetLocalElemShapeData(elemsd=shapes, quad=quad)
CALL geometry%LineH1LagFE_GetLocalElemShapeData( &
  elemsd=geoShapes, quad=quad)
CALL OK(shapes%nns .EQ. obj%GetTotalDOF(), "Local basis size")
CALL OK(shapes%nips .GT. 0_I4B, "Local quadrature size")
CALL OK(ALL(ABS(SUM(shapes%N(1:shapes%nns, 1:shapes%nips), &
                    DIM=1) - 1.0_DFP) .LT. 1.0E-10_DFP), "Partition of unity")
CALL obj%LineH1LagFE_GetGlobalElemShapeData( &
  elemsd=shapes, xij=xij, geoelemsd=geoShapes)
CALL OK(ALL(shapes%coord(1:1, 1:shapes%nips) .GE. -1.0E-10_DFP), &
        "Physical coordinates inside the unit cell")
CALL obj%DEALLOCATE()
CALL geometry%DEALLOCATE()
END PROGRAM main
