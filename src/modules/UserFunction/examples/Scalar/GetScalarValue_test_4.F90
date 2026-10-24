!> author: Vikas Sharma, Ph. D.
! date: 2026-01-16
! summary: Lazy Lua state reuse across scalar batches
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
  USE Display_Method, ONLY: Display
  USE Display_Method, ONLY: EqualLine
  USE ReallocateUtility, ONLY: Reallocate
  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "./toml/scalarfuncs.toml"
  CHARACTER(*), PARAMETER :: testname = "Test name: GetScalarValue_test_3.F90"
  CHARACTER(*), PARAMETER :: tomlName = "test5"

  TYPE(UserFunction_) :: obj
  INTEGER(I4B) :: tsize
  REAL(DFP), ALLOCATABLE :: args(:, :), found(:), want(:)
  REAL(DFP) :: scalarFound
  INTEGER(I4B) :: ibatch

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
  CALL OK(tsize == SIZE(want), "First batch size")

  DO ibatch = 1, 10
    CALL Display(ibatch, "ibatch")
    args = REAL(ibatch, KIND=DFP)
    args(2, :) = args(2, :) + math%one
    want = math%two * (args(1, :) + args(2, :))

    CALL obj%GetScalarValue_(val=scalarFound, args=args(:, 1))
    CALL OK(scalarFound .approxeq. want(1), "Interleaved scalar call")

    CALL obj%GetScalarValue_(val=found, tsize=tsize, args=args)
    CALL OK(ALL(want .approxeq. found), "Repeated batch")
    CALL OK(tsize == SIZE(want), "Repeated batch size")
  END DO

  CALL obj%DEALLOCATE()
  CALL obj%ImportFromToml(tomlName=tomlName, filename=filename)
  CALL obj%GetScalarValue_(val=found, tsize=tsize, args=args)
  CALL OK(ALL(want .approxeq. found), "Batch after reinitialization")
  CALL obj%DEALLOCATE()
END PROGRAM main
