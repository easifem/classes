!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise SetParam

PROGRAM main
USE BaseType, ONLY: TypeElemNameOpt
USE BaseType, ONLY: TypeInterpolationOpt
USE BasisOpt_Class, ONLY: BasisOpt_
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(BasisOpt_) :: obj
INTEGER(I4B) :: basis(3), dofs(4), transform
REAL(DFP) :: alpha(3), beta(3), lambda(3)

CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
                  baseContinuity="H1", baseInterpolation="Lagrange", &
                  order=2_I4B, &
                  ipType=TypeInterpolationOpt%Equidistance, &
                  quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
                  quadratureOrder=[6_I4B])

CALL obj%SetParam(alpha=[0.1_DFP, 0.2_DFP, 0.3_DFP], &
                  beta=[0.4_DFP, 0.5_DFP, 0.6_DFP], &
                  lambda=[0.7_DFP, 0.8_DFP, 0.9_DFP], &
                  dofType=[2_I4B, 2_I4B, 2_I4B, 2_I4B], transformType=2_I4B)
CALL obj%GetParam(alpha=alpha, beta=beta, lambda=lambda, &
                  dofType=dofs, transformType=transform)
CALL OK(ABS(alpha(2) - 0.2_DFP) .LT. 1.0E-12_DFP, "Alpha updated")
CALL OK(ABS(beta(2) - 0.5_DFP) .LT. 1.0E-12_DFP, "Beta updated")
CALL OK(ABS(lambda(2) - 0.8_DFP) .LT. 1.0E-12_DFP, "Lambda updated")
CALL OK(ALL(dofs .EQ. 2_I4B), "DOF types updated")
CALL OK(transform .EQ. 2_I4B, "Transform type updated")
CALL obj%DEALLOCATE()
END PROGRAM main
