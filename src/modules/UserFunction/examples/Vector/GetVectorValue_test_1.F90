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
  USE Display_Method, ONLY: Display

  IMPLICIT NONE

  TYPE(UserFunction_) :: obj
  CHARACTER(*), PARAMETER :: &
    testname = "testing: GetVectorValue_Lua1", &
    luaScript = "./toml/vectorfuncs.lua", &
    luaFunctionName = "Func1"
  INTEGER(I4B), PARAMETER :: numArgs = 1, numReturns = 3
  INTEGER(I4B) :: tsize
  REAL(DFP) :: args(numArgs)
  REAL(DFP) :: want(numReturns), found(numReturns)

  CALL e%setQuietMode(math%yes)

  CALL obj%Initiate(NAME="func", &
                    returnType=varopt%vector, &
                    argType=varopt%space, &
                    numArgs=numArgs, &
                    numReturns=numReturns)

  CALL obj%SetLuaScript(luaScript=luaScript, luaFunctionName=luaFunctionName)

  CALL RANDOM_NUMBER(args)

  want = math%two * args(1)

  CALL obj%Get_(val=found, args=args, tsize=tsize)

  CALL OK(ALL(found.APPROXEQ.want), testname)
END PROGRAM main
! ok 1 - testing: GetVectorValue_Lua1
