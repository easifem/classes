!> author: Vikas Sharma, Ph. D.
! date: 2026-01-16
! summary: This is a simple example to show how to use initiate method

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
  CHARACTER(*), PARAMETER :: testname = "Vector function: display test"
  INTEGER(I4B), PARAMETER :: numArgs = 1, numReturns = 3

  CALL e%setQuietMode(math%yes)

  CALL obj%Initiate(NAME="func", &
                    returnType=varopt%vector, &
                    argType=varopt%space, &
                    numArgs=numArgs, &
                    numReturns=numReturns)

  CALL obj%Display(testname)
END PROGRAM main
! Vector function: display test
! isInit: TRUE
! name: func
! isExternalFunc: FALSE
! isLuaScript: FALSE
! returnType:Vector
! argType:Space
! number of arguments: 1
! number of returns: 3
! scalarFunction ASSOCIATED: FALSE
! vectorFunction ASSOCIATED: FALSE
! matrixFunction ASSOCIATED: FALSE
