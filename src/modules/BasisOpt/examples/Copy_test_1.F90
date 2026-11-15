!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise Copy

PROGRAM main
USE BaseType, ONLY: TypeElemNameOpt
USE BaseType, ONLY: TypeInterpolationOpt
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: QuadraturePoint_
USE BasisOpt_Class, ONLY: BasisOpt_
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(BasisOpt_) :: obj
TYPE(BasisOpt_) :: other
TYPE(ElemShapeData_) :: shapes
TYPE(QuadraturePoint_) :: quad
INTEGER(I4B) :: order

CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
                  baseContinuity="H1", baseInterpolation="Lagrange", &
                  order=2_I4B, &
                  ipType=TypeInterpolationOpt%Equidistance, &
                  quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
                  quadratureOrder=[6_I4B])

CALL obj%Triangle_GetQuadraturePoints(quad)
CALL obj%TriangleH1LagFE_GetLocalElemShapeData(shapes, quad)
CALL other%Copy(obj)
CALL obj%DEALLOCATE()
CALL other%GetParam(order=order)
CALL OK(order .EQ. 2_I4B, "Copied order survives source cleanup")
CALL other%TriangleH1LagFE_GetLocalElemShapeData(shapes, quad)
CALL OK(shapes%nns .EQ. 6_I4B, "Copied shape workspace remains usable")
obj = other
CALL other%SetIsotropicOrder(order=1_I4B)
CALL OK(obj%GetTotalDOF() .EQ. 6_I4B, "Assignment is independent")
CALL obj%DEALLOCATE()
CALL other%DEALLOCATE()
END PROGRAM main
