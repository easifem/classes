!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise Initiate

PROGRAM main
USE BaseType, ONLY: TypeElemNameOpt
USE BaseType, ONLY: TypeInterpolationOpt
USE BasisOpt_Class, ONLY: BasisOpt_
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(BasisOpt_) :: obj
INTEGER(I4B) :: order, basis(3), dofs(4), transform
REAL(DFP) :: alpha(3), beta(3), lambda(3)

CALL obj%Initiate(elemType=TypeElemNameOpt%Triangle, nsd=2_I4B, &
                  baseContinuity="H1", baseInterpolation="Lagrange", &
                  order=2_I4B, &
                  ipType=TypeInterpolationOpt%Equidistance, &
                  quadratureIsHomogeneous=.TRUE., quadratureIsOrder=.TRUE., &
                  quadratureOrder=[6_I4B])

CALL obj%GetParam(order=order, basisType=basis, dofType=dofs, &
                  transformType=transform, alpha=alpha, beta=beta, &
                  lambda=lambda)
CALL OK(order .EQ. 2_I4B, "Initial order")
CALL OK(obj%GetNSD() .EQ. 2_I4B, "Spatial dimension")
CALL OK(obj%GetTotalDOF() .EQ. 6_I4B, "Quadratic triangle DOF count")
CALL OK(ALL(dofs .EQ. 1_I4B), "Default DOF types")
CALL OK(ALL(ABS(alpha) .LT. 1.0E-12_DFP), "Default Jacobi alpha")
CALL OK(ALL(ABS(beta) .LT. 1.0E-12_DFP), "Default Jacobi beta")
CALL OK(ALL(ABS(lambda - 0.5_DFP) .LT. 1.0E-12_DFP), "Default lambda")
CALL obj%DEALLOCATE()
CALL OK(obj%GetNSD() .EQ. 0_I4B, "Deallocate restores default dimension")
CALL OK(obj%GetTotalDOF() .EQ. 0_I4B, "Deallocate resets DOF count")
END PROGRAM main
