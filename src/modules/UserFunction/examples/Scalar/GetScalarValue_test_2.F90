!> author: Vikas Sharma, Ph. D.
! date: 2026-01-16
! summary: Scalar function, Lua script, GetScalarValue_2
!
! [test5]
! name = "func"
! returnType = "Scalar"
! argType = "Space"
! numArgs = 2
! luaScript = "./scalarfuncs.lua"
! luaFunctionName = "Func2"

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
  USE ReallocateUtility, ONLY: Reallocate
  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "./toml/scalarfuncs.toml"
  CHARACTER(*), PARAMETER :: testname = "Test name: GetScalarValue_test_1.F90"
  CHARACTER(*), PARAMETER :: tomlName = "test5"

  TYPE(UserFunction_) :: obj
  INTEGER(I4B) :: tsize
  REAL(DFP), ALLOCATABLE :: args(:, :), found(:), want(:)

  CALL e%setQuietMode(math%yes)
  CALL e%setQuietMode(EXCEPTION_ERROR, math%no)

  CALL obj%ImportFromToml(tomlName=tomlName, filename=filename)

  CALL Reallocate(args, 3, 2)
  CALL RANDOM_NUMBER(args)

  CALL Reallocate(want, 2)
  want = math%two * (args(1, :) + args(2, :))

  CALL Reallocate(found, 2)
  CALL obj%GetScalarValue_(val=found, tsize=tsize, args=args)

  CALL OK(ALL(want .approxeq. found), testname)
END PROGRAM main
