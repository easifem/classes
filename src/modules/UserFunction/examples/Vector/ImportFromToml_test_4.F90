!> author: Vikas Sharma, Ph. D.
! date: 2026-01-16
! summary:  We test vector user function here
!
! [test4]
! name = "func"
! returnType = "Vector"
! argType = "Space"
! numArgs = 1
! numReturns = 2
! equationParser = true
! vars= ["x"]
! expression = ["2.0*x", "2.0*x"]

PROGRAM main
  USE GlobalData, ONLY: DFP, I4B, LGT
  USE UserFunction_Class, ONLY: UserFunction_
  USE BaseType, ONLY: varopt => TypeFEVariableOpt
  USE BaseType, ONLY: math => TypeMathOpt
  USE String_Class, ONLY: String
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_ERROR
  USE ApproxUtility, ONLY: OPERATOR(.APPROXEQ.)
  USE Test_Method, ONLY: OK
  USE Display_Method, ONLY: Display
  USE Display_Method, ONLY: EqualLine
  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: filename = "vectorfuncs.toml"
  CHARACTER(*), PARAMETER :: testname = "test4: use equation parser"
  CHARACTER(*), PARAMETER :: tomlName = "test4"
  TYPE(UserFunction_) :: obj
  REAL(DFP), PARAMETER :: val = 4.0, args(1) = 2.0
  REAL(DFP) :: want(3), got(3)
  INTEGER(I4B) :: tsize
  LOGICAL(LGT) :: isok

  CALL e%setQuietMode(math%yes)
  CALL e%setQuietMode(EXCEPTION_ERROR, math%no)

  CALL obj%ImportFromToml(tomlName=tomlName, filename=filename)
  CALL obj%Get_(val=got, args=args, tsize=tsize)

  want = val

  isok = ALL(got(1:tsize) .approxeq.want(1:tsize))
  CALL OK(isok, testname)

END PROGRAM main
! ok 1 - test4: use equation parser
