!> author: Vikas Sharma, Ph. D.
! date: 2026-01-16
! summary: Scalar function, constant value

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
  CHARACTER(*), PARAMETER :: testname = "Scalar func: setting constant value"
  REAL(DFP), PARAMETER :: val = 1.0, args(1) = 2.0
  REAL(DFP) :: want, got

  CALL e%setQuietMode(math%yes)

  CALL obj%Initiate(NAME="func", &
                    returnType=varopt%scalar, &
                    argType=varopt%space, &
                    numArgs=1, &
                    numReturns=1)

  CALL obj%SetScalarConstantVal(val=val)

  CALL obj%Get_(val=got, args=args)
  want = val

  CALL OK(got.APPROXEQ.want, testname)
END PROGRAM main
! ok 1 - Scalar func: setting constant value
