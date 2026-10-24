!> author: Vikas Sharma, Ph. D.
! date: 2026-01-16
! summary:  we test scalar user function here using ImportFromToml
!
! [test3]
! name = "func"
! returnType = "Scalar"
! argType = "Space"
! numArgs = 3
! luaScript = "./_ScalarFunction_test_2.lua"
! luaFunction = "Func1"

PROGRAM main
  USE GlobalData, ONLY: DFP, I4B, LGT
  USE UserFunction_Class, ONLY: UserFunction_
  USE BaseType, ONLY: varopt => TypeFEVariableOpt
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_ERROR
  USE ApproxUtility, ONLY: OPERATOR(.APPROXEQ.)
  USE Test_Method, ONLY: OK, ISNEAR
  USE Display_Method, ONLY: EqualLine

  IMPLICIT NONE
  CHARACTER(*), PARAMETER :: filename = "./scalarfuncs.toml"
  CHARACTER(*), PARAMETER :: testname = "test3: lua script test"
  CHARACTER(*), PARAMETER :: tomlName = "test3"
  TYPE(UserFunction_) :: obj
  REAL(DFP), PARAMETER :: val = 4.0, args(1) = 2.0
  REAL(DFP) :: want, got
  LOGICAL(LGT) :: isok

  CALL e%setQuietMode(math%yes)
  CALL e%setQuietMode(EXCEPTION_ERROR, math%no)

  CALL obj%ImportFromToml(tomlName=tomlName, filename=filename)
  CALL obj%Get_(val=got, args=args)

  want = val
  isok = got.approxeq.want
  CALL OK(isok, testname)
END PROGRAM main
! ok 1 - test3: lua script test
