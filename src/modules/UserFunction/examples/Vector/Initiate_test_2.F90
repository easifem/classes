!> author: Vikas Sharma, Ph. D.
! date: 2026-01-16
! summary: This test sets the constant value.

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
    "Vector function setting constant value"
  REAL(DFP), PARAMETER :: val = 1.0, args(1) = 2.0
  INTEGER(I4B), PARAMETER :: numArgs = 1, numReturns = 3
  REAL(DFP) :: want(numReturns), got(numReturns)
  INTEGER(I4B) :: tsize

  CALL e%setQuietMode(math%yes)

  CALL obj%Initiate(NAME="func", &
                    returnType=varopt%vector, &
                    argType=varopt%space, &
                    numArgs=numArgs, &
                    numReturns=numReturns)

  want = val

  CALL obj%SetVectorConstantVal(val=want)

  CALL obj%Get_(val=got, tsize=tsize, args=args)

  CALL OK(ALL(got.APPROXEQ.want), testname)

END PROGRAM main
! ok 1 - Vector function setting constant value
