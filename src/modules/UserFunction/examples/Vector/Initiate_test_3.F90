!> author: Vikas Sharma, Ph. D.
! date: 2026-01-16
! summary: This function set the external function

PROGRAM main
  USE GlobalData, ONLY: DFP, I4B, LGT
  USE UserFunction_Class, ONLY: UserFunction_
  USE BaseType, ONLY: varopt => TypeFEVariableOpt
  USE BaseType, ONLY: math => TypeMathOpt
  USE String_Class, ONLY: String
  USE ExceptionHandler_Class, ONLY: e
  USE ApproxUtility, ONLY: OPERATOR(.APPROXEQ.)
  USE Test_Method, ONLY: OK, ISNEAR
  USE BaseType, ONLY: InterfaceVectorSubroutine
  IMPLICIT NONE

  TYPE(UserFunction_) :: obj
  CHARACTER(*), PARAMETER :: testname = &
    "Vector function, external function"
  REAL(DFP), PARAMETER :: val = 4.0, args(1) = 2.0
  INTEGER(I4B), PARAMETER :: numArgs = 1, numReturns = 3
  REAL(DFP) :: want(numReturns), got(numReturns)
  PROCEDURE(InterfaceVectorSubroutine), POINTER :: func
  INTEGER(I4B) :: tsize

  CALL e%SetQuietMode(math%yes)

  CALL obj%Initiate(NAME="func", &
                    returnType=varopt%vector, &
                    argType=varopt%space, &
                    numArgs=numArgs, &
                    numReturns=numReturns)

  func => vectorFunc1
  CALL obj%SetVectorFunctionPointer(func=func)

  CALL obj%Get_(val=got, args=args, tsize=tsize)
  want = val

  CALL OK(ALL(got.APPROXEQ.want), testname)

CONTAINS

  SUBROUTINE vectorFunc1(args, nargs, ans, tsize)
    REAL(DFP), INTENT(IN) :: args(:)
    INTEGER(I4B), INTENT(IN) :: nargs
    REAL(DFP), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    tsize = 3
    ans(1:tsize) = args(1) * 2.0_DFP
  END SUBROUTINE vectorFunc1

END PROGRAM main
! ok 1 - Vector function, external function
