!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Exercise OneDimLagrangeFE_ValueConstructor

PROGRAM main
USE BaseType, ONLY: TypeInterpolationOpt
USE BaseType, ONLY: TypePolynomialOpt
USE GlobalData, ONLY: I4B
USE OneDimLagrangeFE_Class, ONLY: FE_ => OneDimLagrangeFE_
USE OneDimLagrangeFE_Class, ONLY: NewValue => OneDimLagrangeFE
USE Test_Method, ONLY: OK
IMPLICIT NONE
TYPE(FE_) :: obj

obj = NewValue(baseContinuity="H1", order=2_I4B, &
               ipType=TypeInterpolationOpt%Equidistance, &
               basisType=TypePolynomialOpt%Monomial)
CALL OK(obj%isInit, "Value constructor initializes the object")
CALL OK(obj%GetOrder() .EQ. 2_I4B, "Value constructor order")
CALL obj%DEALLOCATE()
END PROGRAM main
