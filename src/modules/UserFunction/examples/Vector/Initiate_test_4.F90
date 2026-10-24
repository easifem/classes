!> author: Vikas Sharma, Ph. D.
! date: 2026-01-16
! summary: This function sets the value using lua script

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
  CHARACTER(*), PARAMETER :: testname = "test4: lua script test", &
    luaScript = "./vectorfuncs.lua", &
    luaFunctionName = "Func1"
  INTEGER(I4B), PARAMETER :: numArgs = 1, numReturns = 3
  INTEGER(I4B) :: tsize
  REAL(DFP), PARAMETER :: val = 4.0, args(1) = 2.0
  REAL(DFP) :: want(numReturns), got(numReturns)

  CALL e%setQuietMode(math%yes)

  CALL obj%Initiate(NAME="func", &
                    returnType=varopt%vector, &
                    argType=varopt%space, &
                    numArgs=numArgs, &
                    numReturns=numReturns)

  CALL obj%SetLuaScript(luaScript=luaScript, luaFunctionName=luaFunctionName)

  CALL obj%Get_(val=got, args=args, tsize=tsize)
  want = val
  CALL OK(ALL(got.APPROXEQ.want), testname)

END PROGRAM main
! ok 1 - test4: lua script test
