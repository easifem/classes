!> author: Vikas Sharma, Ph. D.
! date: 2026-01-16
! summary: Initiate vector valued user function.

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

  CALL e%SetQuietMode(math%yes)

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
! number of arguments: 1
! number of returns: 3
! returnType:Vector
! argType:Space
! engineID: NOT FOUND
