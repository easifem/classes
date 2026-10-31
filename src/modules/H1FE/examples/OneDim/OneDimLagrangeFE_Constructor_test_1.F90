!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise OneDimLagrangeFE_Constructor

PROGRAM main
USE BaseType, ONLY: TypeInterpolationOpt
USE BaseType, ONLY: TypePolynomialOpt
USE GlobalData, ONLY: I4B
USE OneDimLagrangeFE_Class, ONLY: FE_ => OneDimLagrangeFE_
USE OneDimLagrangeFE_Class, ONLY: NewFE => OneDimLagrangeFEPointer
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(FE_), POINTER :: ptr

ptr => NewFE(baseContinuity="H1", order=2_I4B, &
             ipType=TypeInterpolationOpt%Equidistance, &
             basisType=TypePolynomialOpt%Monomial)
CALL OK(ASSOCIATED(ptr), "Configured constructor allocates a target")
CALL OK(ptr%isInit, "Target is initialized")
CALL OK(ptr%GetOrder() .EQ. 2_I4B, "Constructor order")
CALL ptr%DEALLOCATE()
DEALLOCATE (ptr)
END PROGRAM main
