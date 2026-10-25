!> author: Vikas Sharma, Ph. D.
! date: 2026-01-16
!
! Vector valued, GetVectorValue_Lua1, lua script

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
  USE Display_Method, ONLY: ToString

  IMPLICIT NONE

  TYPE(UserFunction_) :: obj
  CHARACTER(*), PARAMETER :: &
    testname = "GetVectorValue_Lua2", &
    luaScript = "./toml/vectorfuncs.lua", &
    luaFunctionName = "Func2"
  INTEGER(I4B), PARAMETER :: numArgs = 3, numReturns = 3
  INTEGER(I4B) :: nrow, ncol, ii
  REAL(DFP) :: args(numArgs, 2)
  REAL(DFP) :: want(numReturns, 2), found(numReturns, 2)

  CALL e%setQuietMode(math%yes)

  CALL obj%Initiate(NAME="func", &
                    returnType=varopt%vector, &
                    argType=varopt%space, &
                    numArgs=numArgs, &
                    numReturns=numReturns)

  CALL obj%SetLuaScript(luaScript=luaScript, luaFunctionName=luaFunctionName)

  DO ii = 1, 4
    CALL RANDOM_NUMBER(args)
    want(1, :) = math%two * args(1, :)
    want(2, :) = math%two * args(2, :)
    want(3, :) = math%two * args(3, :)
    CALL obj%GetVectorValue_(val=found, nrow=nrow, ncol=ncol, args=args)
    CALL OK(ALL(found.APPROXEQ.want), testname // " ii=" // ToString(ii))
  END DO

END PROGRAM main
! ok 1 - GetVectorValue_Lua2 ii=1
! ok 2 - GetVectorValue_Lua2 ii=2
! ok 3 - GetVectorValue_Lua2 ii=3
! ok 4 - GetVectorValue_Lua2 ii=4
