!> author: Vikas Sharma, Ph. D.
! date: 2026-01-16
! summary:  We test vector user function here
!
! [test1]
! name = "func"
! returnType = "Vector"
! argType = "Constant"
! numArgs = 0
! numReturns = 3
! value = [1.0, 1.0, 1.0]

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
  TYPE(UserFunction_) :: obj
  CHARACTER(*), PARAMETER :: testname = "Vector functions constant function"
  CHARACTER(*), PARAMETER :: tomlName = "test1"
  REAL(DFP), PARAMETER :: val = 1.0_DFP
  REAL(DFP), PARAMETER :: args(1) = 2.0_DFP
  REAL(DFP) :: want(3), got(3)
  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: tsize

  CALL e%setQuietMode(math%yes)
  CALL e%setQuietMode(EXCEPTION_ERROR, math%no)

  CALL obj%ImportFromToml(tomlName=tomlName, filename=filename)
  CALL obj%Get_(val=got, args=args, tsize=tsize)
  want = val

  isok = ALL(got(1:tsize) .approxeq.want(1:3))
  CALL OK(isok, testname)

END PROGRAM main
! ok 1 - Vector functions constant function
