!> author: Vikas Sharma, Ph. D.
! date: 2026-01-16
! summary: Initiate Scalar user function

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
  CHARACTER(*), PARAMETER :: testname = "Initiate method for ScalarFunction"

  CALL e%SetQuietMode(math%yes)

  CALL obj%Initiate(NAME="func", &
                    returnType=varopt%scalar, &
                    argType=varopt%space, &
                    numArgs=1, &
                    numReturns=1)

  CALL obj%Display(testname)

END PROGRAM main
! Initiate method for ScalarFunction
! isInit: TRUE
! name: func
! number of arguments: 1
! number of returns: 1
! returnType:Scalar
! argType:Space
! engineID: NOT FOUND
