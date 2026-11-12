!> author: Vikas Sharma, Ph. D.
! date: 2026-11-12
! summary: Set a space-time scalar field by a user function
!
! The field u (order 1 Lagrange in space and time, timeCompo = 2) is
! set by f(x, y, t) = 1 + x + 2 y + 3 t at the nodes of the mesh.
!
!- SetByFunction(func, times) evaluates f at time times(a) for time
!  component a, SIZE(times) = timeCompo,
!- SetByFunction(func) evaluates f at t = 0 and uses this value for
!  all time components.
!
! The nodes are 1 (0,0), 2 (10,0), 3 (10,10), 4 (0,10), 5 (5,0),
! 6 (5,10). The routine works for Lagrange interpolation, where the
! space DOF number is the node number.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: TypeFEVariableOpt
USE BaseType, ONLY: InterfaceScalarSubroutine
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE STScalarField_Class, ONLY: STScalarField_
USE Test_Method, ONLY: OK
USE TimeFEDOF_Class, ONLY: TimeFEDOF_
USE UserFunction_Class, ONLY: UserFunction_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "STScalarField_test.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
REAL(DFP), PARAMETER :: f0(6) = [1.0_DFP, 11.0_DFP, 31.0_DFP, &
                                 21.0_DFP, 6.0_DFP, 26.0_DFP]
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(TimeFEDOF_), TARGET :: timefedof
TYPE(STScalarField_) :: u
TYPE(UserFunction_) :: func
PROCEDURE(InterfaceScalarSubroutine), POINTER :: procptr
REAL(DFP) :: found(12)
INTEGER(I4B) :: tsize

CALL e%SetQuietMode(math%yes)
CALL func%Initiate(NAME="func", returnType=TypeFEVariableOpt%scalar, &
                   argType=TypeFEVariableOpt%space, numArgs=4, &
                   numReturns=1)
procptr => linearFunc
CALL func%SetScalarFunctionPointer(func=procptr)

CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
                      fedof=fedof, geofedof=geofedof, &
                      timefedof=timefedof, dom=dom)

! Test 1: one time for each time component
BLOCK
  CALL u%Set(VALUE=0.0_DFP)
  CALL u%SetByFunction(func=func, times=[0.0_DFP, 1.0_DFP])
  CALL u%Get(VALUE=found, tsize=tsize, timeCompo=1)
  CALL OK(ALL(ABS(found(1:6) - f0) .LT. tol), "times: t = 0")
  CALL u%Get(VALUE=found, tsize=tsize, timeCompo=2)
  CALL OK(ALL(ABS(found(1:6) - f0 - 3.0_DFP) .LT. tol), "times: t = 1")
END BLOCK

! Test 2: without times, f at t = 0 for all time components
BLOCK
  CALL u%Set(VALUE=0.0_DFP)
  CALL u%SetByFunction(func=func)
  CALL u%Get(VALUE=found, tsize=tsize, timeCompo=2)
  CALL OK(ALL(ABS(found(1:6) - f0) .LT. tol), "no times")
END BLOCK

CALL u%DEALLOCATE()
CALL timefedof%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()

CONTAINS

SUBROUTINE linearFunc(args, nargs, ans)
  REAL(DFP), INTENT(IN) :: args(:)
  INTEGER(I4B), INTENT(IN) :: nargs
  REAL(DFP), INTENT(INOUT) :: ans
  ans = 1.0_DFP + args(1) + 2.0_DFP * args(2) + 3.0_DFP * args(4)
END SUBROUTINE linearFunc

END PROGRAM main
