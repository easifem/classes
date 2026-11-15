!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise Initiate

PROGRAM main
USE BaseType, ONLY: TypeElemNameOpt
USE BaseType, ONLY: TypeInterpolationOpt
USE BasisOpt_Class, ONLY: BasisOpt_
USE GlobalData, ONLY: I4B
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(BasisOpt_) :: obj
INTEGER(I4B) :: dofs(4), transform

CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
                  baseContinuity="H1", baseInterpolation="Lagrange", &
                  dofType=[2_I4B, 2_I4B, 2_I4B, 2_I4B], transformType=2_I4B, &
                  order=2_I4B, &
                  ipType=TypeInterpolationOpt%Equidistance, &
                  quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
                  quadratureOrder=[6_I4B])
CALL obj%GetParam(dofType=dofs, transformType=transform)
CALL OK(ALL(dofs .EQ. 2_I4B), "Initiate preserves requested DOF types")
CALL OK(transform .EQ. 2_I4B, "Initiate preserves requested transform")
CALL obj%DEALLOCATE()
END PROGRAM main
