!> author: Vikas Sharma, Ph. D.
! date: 2026-01-16
! summary: Scalar function, external fortran function.

PROGRAM main
  USE GlobalData, ONLY: DFP, I4B, LGT
  USE UserFunction_Class, ONLY: UserFunction_
  USE BaseType, ONLY: varopt => TypeFEVariableOpt
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ApproxUtility, ONLY: OPERATOR(.APPROXEQ.)
  USE Test_Method, ONLY: OK, ISNEAR
  USE BaseType, ONLY: InterfaceScalarSubroutine
  IMPLICIT NONE

  TYPE(UserFunction_) :: obj
  CHARACTER(*), PARAMETER :: testname = &
    "Scalar func: setting external function"
  REAL(DFP), PARAMETER :: val = 4.0, args(1) = 2.0
  REAL(DFP) :: want, got
  PROCEDURE(InterfaceScalarSubroutine), POINTER :: func

  CALL e%setQuietMode(math%yes)
  CALL obj%Initiate(NAME="func", &
                    returnType=varopt%scalar, &
                    argType=varopt%space, &
                    numArgs=1, &
                    numReturns=1)
  func => scalarFunc1
  CALL obj%SetScalarFunctionPointer(func=func)
  CALL obj%Get_(val=got, args=args)
  want = val
  CALL OK(got.APPROXEQ.want, testname)

CONTAINS
  SUBROUTINE scalarFunc1(args, nargs, ans)
    REAL(DFP), INTENT(IN) :: args(:)
    INTEGER(I4B), INTENT(IN) :: nargs
    REAL(DFP), INTENT(INOUT) :: ans
    ans = args(1) * 2.0_DFP
  END SUBROUTINE scalarFunc1
END PROGRAM main
