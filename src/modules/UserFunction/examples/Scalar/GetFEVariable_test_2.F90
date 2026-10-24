!> author: Vikas Sharma, Ph. D.
! date: 2026-01-16
! summary: we test scalar user function here, function is specified
! by equation parser.

PROGRAM main
  USE GlobalData, ONLY: DFP, I4B, LGT
  USE UserFunction_Class, ONLY: UserFunction_
  USE BaseType, ONLY: varopt => TypeFEVariableOpt
  USE BaseType, ONLY: FEVariable_
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ApproxUtility, ONLY: OPERATOR(.APPROXEQ.)
  USE Test_Method, ONLY: OK, ISNEAR
  USE Display_Method, ONLY: Display
  USE ReallocateUtility, ONLY: Reallocate
  USE FEVariable_Method, ONLY: FEVariableDisplay => Display

  IMPLICIT NONE

  TYPE(UserFunction_) :: obj
  CHARACTER(*), PARAMETER :: testname = "Scalar func: use equation parser", &
    var(1) = ["x"], &
    funcStr = "x"
  INTEGER(I4B), PARAMETER :: numArgs = 1, numReturns = 1
  REAL(DFP), ALLOCATABLE :: xij(:, :)
  TYPE(FEVariable_) :: fevar

  CALL e%setQuietMode(math%yes)

  CALL obj%Initiate(NAME="func", &
                    returnType=varopt%scalar, &
                    argType=varopt%space, &
                    numArgs=numArgs, &
                    numReturns=numReturns)

  CALL obj%SetScalarEquationParser(var=var, funcStr=funcStr)
  CALL Reallocate(xij, 2, 3)

  CALL RANDOM_NUMBER(xij)
  CALL Display(xij, "xij: ")

  CALL obj%Get_(fevar=fevar, xij=xij)
  CALL FEVariableDisplay(fevar, "fevar: ")
END PROGRAM main
