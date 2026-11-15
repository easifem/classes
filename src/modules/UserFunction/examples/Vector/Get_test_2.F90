!> author: Vikas Sharma, Ph. D.
! date: 2026-01-16
! summary: This example shows the use of Get and Get_ method for vector
! function.

PROGRAM main
  USE GlobalData, ONLY: DFP, I4B, LGT
  USE UserFunction_Class, ONLY: UserFunction_
  USE BaseType, ONLY: varopt => TypeFEVariableOpt
  USE BaseType, ONLY: math => TypeMathOpt
  USE String_Class, ONLY: String
  USE ExceptionHandler_Class, ONLY: e
  USE ApproxUtility, ONLY: OPERATOR(.APPROXEQ.)
  USE Test_Method, ONLY: OK, ISNEAR

  IMPLICIT NONE

  TYPE(UserFunction_) :: obj
  CHARACTER(*), PARAMETER :: testname = "Vector function Get"
  REAL(DFP), PARAMETER :: val = 1.0, args(1) = 2.0
  INTEGER(I4B), PARAMETER :: numArgs = 1, numReturns = 3
  REAL(DFP), ALLOCATABLE :: want(:), got(:)

  CALL e%setQuietMode(math%yes)

  CALL obj%Initiate(NAME="func", &
                    returnType=varopt%vector, &
                    argType=varopt%space, &
                    numArgs=numArgs, &
                    numReturns=numReturns)

  ALLOCATE(want(numReturns))
  want = val

  CALL obj%SetVectorConstantVal(val=want)

  CALL obj%Get(val=got, args=args)

  CALL OK(ALL(got.APPROXEQ.want), testname)

END PROGRAM main
! ok 1 - Vector function Get
