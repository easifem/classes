!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise AbstractFE_ GetGlobalElemShapeData operations
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
TYPE(ElemShapeData_) :: shapes, geoShapes
TYPE(QuadraturePoint_) :: quad
REAL(DFP) :: xij(2, 3)
INTEGER(I4B) :: count

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
CALL obj%GetQuadraturePoints(quad=quad)
count = obj%GetTotalQuadraturePoints()
CALL obj%GetLocalElemShapeData(elemsd=shapes, quad=quad)
CALL geometry%GetLocalElemShapeData(elemsd=geoShapes, quad=quad)
CALL OK(shapes%nips .EQ. count, "Configured quadrature count")
CALL OK(shapes%nns .EQ. 3_I4B, "Linear triangle basis size")
CALL obj%GetGlobalElemShapeData( &
  elemsd=shapes, geoelemsd=geoShapes, xij=xij)
CALL OK(ALL(ABS(SUM(shapes%N(1:3, 1:count), DIM=1) - &
                1.0_DFP) .LT. 1.0E-10_DFP), "Partition of unity")
CALL obj%GetGlobalElemShapeData2(geofeptr=geometry, &
                                 elemsd=shapes, geoelemsd=geoShapes, &
                                 xij=xij, quad=quad)
CALL OK(shapes%nips .EQ. count, "High-level shape preparation")
CALL obj%DEALLOCATE()
CALL geometry%DEALLOCATE()
END PROGRAM main
