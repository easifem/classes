!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Constructing BasisOpt using Initiate method
!
PROGRAM main
USE BaseType, ONLY: TypeElemNameOpt
USE BaseType, ONLY: TypeInterpolationOpt
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: math => TypeMathOpt
USE BasisOpt_Class, ONLY: BasisOpt_
USE ExceptionHandler_Class, ONLY: e
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: LGT
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(BasisOpt_) :: obj
INTEGER(I4B) :: order, basis(3), dofs(4), transform
REAL(DFP) :: alpha(3), beta(3), lambda(3)
LOGICAL(LGT) :: isBrokenSpace

CALL e%SetQuietMode(math%yes)

CALL obj%Initiate(elemType=TypeElemNameOpt%Line, nsd=1_I4B, &
                  baseContinuity="H1", baseInterpolation="Lagrange", &
                  order=4_I4B, isBrokenSpace=math%no, &
                  ipType=TypeInterpolationOpt%GaussLegendreLobatto, &
                  quadratureIsHomogeneous=math%yes, &
                  quadratureIsOrder=math%yes, &
                  quadratureOrder=[8_I4B])

CALL obj%GetParam(order=order, basisType=basis, dofType=dofs, &
                  transformType=transform, alpha=alpha, beta=beta, &
                  lambda=lambda, isBrokenSpace=isBrokenSpace)
CALL OK(order .EQ. 4_I4B, "order")
CALL OK(obj%GetNSD() .EQ. 1_I4B, "nsd")
CALL OK(obj%GetTotalDOF() .EQ. 5_I4B, "tdof ")
CALL OK(ALL(dofs .EQ. 1_I4B), "all dofs")
CALL OK(ALL(ABS(alpha) .LT. 1.0E-12_DFP), "Default Jacobi alpha")
CALL OK(ALL(ABS(beta) .LT. 1.0E-12_DFP), "Default Jacobi beta")
CALL OK(ALL(ABS(lambda - 0.5_DFP) .LT. 1.0E-12_DFP), "Default lambda")
CALL OK(.NOT. isBrokenSpace, "isBrokenSpace: ")
CALL obj%DEALLOCATE()
CALL OK(obj%GetNSD() .EQ. 0_I4B, "Deallocate restores default dimension")
CALL OK(obj%GetTotalDOF() .EQ. 0_I4B, "Deallocate resets DOF count")

CALL obj%Initiate(elemType=TypeElemNameOpt%Line, nsd=1_I4B, &
                  baseContinuity="H1", baseInterpolation="Lagrange", &
                  order=4_I4B, isBrokenSpace=math%yes, &
                  ipType=TypeInterpolationOpt%GaussLegendreLobatto, &
                  quadratureIsHomogeneous=math%yes, &
                  quadratureIsOrder=math%yes, &
                  quadratureOrder=[8_I4B])
CALL OK(obj%GetIsBrokenSpace(), "isBrokenSpace: ")
END PROGRAM main
