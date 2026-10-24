!> author: Vikas Sharma, Ph. D.
! date: 2026-01-16
! summary:  We test vector user function here

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

  CHARACTER(*), PARAMETER :: filename = "./vectorfuncs.toml"
  CHARACTER(*), PARAMETER :: testname = "Vector function lua script test"
  CHARACTER(*), PARAMETER :: tomlName = "test3"
  TYPE(UserFunction_) :: obj
  REAL(DFP), PARAMETER :: val = 4.0, args(1) = 2.0
  REAL(DFP) :: want(3), got(3)
  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: tsize

  CALL e%setQuietMode(math%yes)
  CALL e%setQuietMode(EXCEPTION_ERROR, math%no)

  CALL obj%ImportFromToml(tomlName=tomlName, filename=filename)
  CALL obj%Get_(val=got, args=args, tsize=tsize)

  want = val
  isok = ALL(got(1:tsize) .approxeq.want(1:tsize))
  CALL OK(isok, testname)
END PROGRAM main
! ok 1 - Vector function lua script test
