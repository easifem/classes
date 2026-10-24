!> author: Vikas Sharma, Ph. D.
! date: 2026-01-16
! summary: we test scalar user function here, function is specified
! by equation parser.

PROGRAM main
  USE GlobalData, ONLY: DFP, I4B, LGT
  USE UserFunction_Class, ONLY: UserFunction_
  USE BaseType, ONLY: varopt => TypeFEVariableOpt
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ApproxUtility, ONLY: OPERATOR(.APPROXEQ.)
  USE Test_Method, ONLY: OK, ISNEAR

  IMPLICIT NONE

  TYPE(UserFunction_) :: obj
  CHARACTER(*), PARAMETER :: testname = "Scalar func: use equation parser", &
    var(1) = ["x"], &
    funcStr = "2.0*x"
  INTEGER(I4B), PARAMETER :: numArgs = 1, numReturns = 1
  REAL(DFP), PARAMETER :: val = 4.0, args(1) = 2.0
  REAL(DFP) :: want, got

  CALL e%setQuietMode(math%yes)

  CALL obj%Initiate(NAME="func", &
                    returnType=varopt%scalar, &
                    argType=varopt%space, &
                    numArgs=numArgs, &
                    numReturns=numReturns)

  CALL obj%SetScalarEquationParser(var=var, funcStr=funcStr)

  CALL obj%Get_(val=got, args=args)
  want = val
  CALL ISNEAR(got, want, 1.0E-10_DFP, testname)

END PROGRAM main
! ok 1 - Scalar func: use equation parser
