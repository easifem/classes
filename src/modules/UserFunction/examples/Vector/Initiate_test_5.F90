!> author: Vikas Sharma, Ph. D.
! date: 2026-01-16
! summary: This example set the value of vector function usign expression

PROGRAM main
  USE GlobalData, ONLY: DFP, I4B, LGT
  USE UserFunction_Class, ONLY: UserFunction_
  USE BaseType, ONLY: varopt => TypeFEVariableOpt
  USE BaseType, ONLY: math => TypeMathOpt
  USE String_Class, ONLY: String
  USE ExceptionHandler_Class, ONLY: e
  USE ApproxUtility, ONLY: OPERATOR(.APPROXEQ.)
  USE Test_Method, ONLY: OK, ISNEAR

  IMPLICIT NONE

  TYPE(UserFunction_) :: obj
  CHARACTER(*), PARAMETER :: testname = &
    "Vector function: use equation parser", var(1) = ["x"]
  INTEGER(I4B), PARAMETER :: numArgs = 1, numReturns = 3
  TYPE(String) :: funcStr(numReturns)
  REAL(DFP), PARAMETER :: val = 4.0, args(1) = 2.0
  REAL(DFP) :: want(numReturns), got(numReturns)
  INTEGER(I4B) :: ii, tsize

  CALL e%setQuietMode(math%yes)

  CALL obj%Initiate(NAME="func", &
                    returnType=varopt%vector, &
                    argType=varopt%space, &
                    numArgs=numArgs, &
                    numReturns=numReturns)

  DO ii = 1, numReturns
    funcStr(ii) = "2.0*x"
  END DO

  CALL obj%SetVectorEquationParser(var=var, funcStr=funcStr)

  CALL obj%Get_(val=got, args=args, tsize=tsize)
  want = val
  CALL OK(ALL(want .EQ. val), testname)

END PROGRAM main
! ok 1 - Vector function: use equation parser
