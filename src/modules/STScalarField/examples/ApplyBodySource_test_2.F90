!> author: Vikas Sharma, Ph. D.
! date: 2026-11-14
! summary: Add the body source at one time to one time component
!
! The field u is order 1 Lagrange in space on square_3x2.h5 (two
! Quadrangle4 elements [0, 5] x [0, 10] and [5, 10] x [0, 10]) and
! order 1 Lagrange in time (timeCompo = 2).
! ApplyBodySource(bodySource, scale, times, timeCompo) adds
!
!   scale * integral over space of N_I f(x, y, times)
!
! to the entry of node I and time component timeCompo only; the other
! time component is not changed.
!
! In space, integral(N_I) = 12.5 for nodes 1 to 4 and 25 for the
! shared nodes 5, 6.

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
REAL(DFP), PARAMETER :: space(6) = [12.5_DFP, 12.5_DFP, 12.5_DFP, &
                                    12.5_DFP, 25.0_DFP, 25.0_DFP]
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(TimeFEDOF_), TARGET :: timefedof
TYPE(STScalarField_) :: u
TYPE(UserFunction_) :: func
PROCEDURE(InterfaceScalarSubroutine), POINTER :: procptr
REAL(DFP) :: found(12)
INTEGER(I4B) :: tsize

CALL e%SetQuietMode(math%yes)
CALL func%Initiate(NAME="f", returnType=TypeFEVariableOpt%scalar, &
                   argType=TypeFEVariableOpt%space, numArgs=4, &
                   numReturns=1)
procptr => onePlusT
CALL func%SetScalarFunctionPointer(func=procptr)

CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
                      fedof=fedof, geofedof=geofedof, &
                      timefedof=timefedof, dom=dom)

! Test 1: f = 1 + t at t = 2, scale 2, added to time component 2
BLOCK
  CALL u%Set(VALUE=1.0_DFP)
  CALL u%ApplyBodySource(bodySource=func, scale=2.0_DFP, times=2.0_DFP, &
                         timeCompo=2)
  CALL u%Get(VALUE=found, tsize=tsize, timeCompo=1)
  CALL OK(ALL(ABS(found(1:6) - 1.0_DFP) .LT. tol), &
          "timeCompo 2: time 1 not changed")
  CALL u%Get(VALUE=found, tsize=tsize, timeCompo=2)
  CALL OK(ALL(ABS(found(1:6) - (1.0_DFP + 6.0_DFP * space)) .LT. tol), &
          "timeCompo 2: time 2")
END BLOCK

! Test 2: f = 1 + t at t = 0, scale 1, added to time component 1
BLOCK
  CALL u%Set(VALUE=0.0_DFP)
  CALL u%ApplyBodySource(bodySource=func, scale=1.0_DFP, times=0.0_DFP, &
                         timeCompo=1)
  CALL u%Get(VALUE=found, tsize=tsize, timeCompo=1)
  CALL OK(ALL(ABS(found(1:6) - space) .LT. tol), "timeCompo 1: time 1")
  CALL u%Get(VALUE=found, tsize=tsize, timeCompo=2)
  CALL OK(ALL(ABS(found(1:6)) .LT. tol), "timeCompo 1: time 2 not changed")
END BLOCK

CALL u%DEALLOCATE()
CALL timefedof%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()

CONTAINS

SUBROUTINE onePlusT(args, nargs, ans)
  REAL(DFP), INTENT(IN) :: args(:)
  INTEGER(I4B), INTENT(IN) :: nargs
  REAL(DFP), INTENT(INOUT) :: ans
  ans = 1.0_DFP + args(4)
END SUBROUTINE onePlusT

END PROGRAM main
