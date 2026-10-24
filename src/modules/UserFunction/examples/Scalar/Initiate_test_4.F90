!> author: Vikas Sharma, Ph. D.
! date: 2026-01-16
! summary:  we test scalar user function here

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
  CHARACTER(*), PARAMETER :: testname = "Scalar func: lua script test", &
    luaScript = "./scalarfuncs.lua", &
    luaFunctionName = "Func1"
  INTEGER(I4B), PARAMETER :: numArgs = 1, numReturns = 1
  REAL(DFP), PARAMETER :: val = 4.0, args(1) = 2.0
  REAL(DFP) :: want, got

  CALL e%setQuietMode(math%yes)

  CALL obj%Initiate(NAME="func", &
                    returnType=varopt%scalar, &
                    argType=varopt%space, &
                    numArgs=numArgs, &
                    numReturns=numReturns)

  CALL obj%SetLuaScript(luaScript=luaScript, luaFunctionName=luaFunctionName)

  CALL obj%Get_(val=got, args=args)
  want = val
  CALL OK(got.APPROXEQ.want, testname)

END PROGRAM main
! ok 1 - Scalar func: lua script test
