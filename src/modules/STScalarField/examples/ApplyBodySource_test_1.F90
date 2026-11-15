!> author: Vikas Sharma, Ph. D.
! date: 2026-11-12
! summary: Add the body source to a space-time scalar field
!
! The field u is order 1 Lagrange in space on square_3x2.h5 (two
! Quadrangle4 elements [0, 5] x [0, 10] and [5, 10] x [0, 10]) and
! order 1 Lagrange in time on [0, 1] (timeCompo = 2).
! ApplyBodySource(bodySource, scale, times) adds
!
!   scale * integral over space and time of N_I T_a f(x, y, t)
!
! to the entry of node I and time component a.
!
! In space, integral(N_I) = 12.5 for nodes 1 to 4 and 25 for the
! shared nodes 5, 6. In time, integral(T_a) = 1/2,
! integral(T_1 t) = 1/6 and integral(T_2 t) = 1/3.

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
REAL(DFP), PARAMETER :: times(2) = [0.0_DFP, 1.0_DFP]
REAL(DFP), PARAMETER :: space(6) = [12.5_DFP, 12.5_DFP, 12.5_DFP, &
                                    12.5_DFP, 25.0_DFP, 25.0_DFP]
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(TimeFEDOF_), TARGET :: timefedof
TYPE(STScalarField_) :: u
TYPE(UserFunction_) :: funcOne, funcT
PROCEDURE(InterfaceScalarSubroutine), POINTER :: procptr
REAL(DFP) :: found(12)
INTEGER(I4B) :: tsize

CALL e%SetQuietMode(math%yes)
CALL funcOne%Initiate(NAME="one", returnType=TypeFEVariableOpt%scalar, &
                      argType=TypeFEVariableOpt%space, numArgs=4, &
                      numReturns=1)
procptr => oneFunc
CALL funcOne%SetScalarFunctionPointer(func=procptr)
CALL funcT%Initiate(NAME="t", returnType=TypeFEVariableOpt%scalar, &
                    argType=TypeFEVariableOpt%space, numArgs=4, &
                    numReturns=1)
procptr => tFunc
CALL funcT%SetScalarFunctionPointer(func=procptr)

CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
                      fedof=fedof, geofedof=geofedof, &
                      timefedof=timefedof, dom=dom)

! Test 1: f = 1
BLOCK
  CALL u%Set(VALUE=0.0_DFP)
  CALL u%ApplyBodySource(bodySource=funcOne, scale=1.0_DFP, times=times)
  CALL u%Get(VALUE=found, tsize=tsize, timeCompo=1)
  CALL OK(ALL(ABS(found(1:6) - 0.5_DFP * space) .LT. tol), &
          "f = 1: time 1")
  CALL u%Get(VALUE=found, tsize=tsize, timeCompo=2)
  CALL OK(ALL(ABS(found(1:6) - 0.5_DFP * space) .LT. tol), &
          "f = 1: time 2")
END BLOCK

! Test 2: f = t, added with scale 6
BLOCK
  CALL u%Set(VALUE=1.0_DFP)
  CALL u%ApplyBodySource(bodySource=funcT, scale=6.0_DFP, times=times)
  CALL u%Get(VALUE=found, tsize=tsize, timeCompo=1)
  CALL OK(ALL(ABS(found(1:6) - (1.0_DFP + space)) .LT. tol), &
          "f = t: time 1")
  CALL u%Get(VALUE=found, tsize=tsize, timeCompo=2)
  CALL OK(ALL(ABS(found(1:6) - (1.0_DFP + 2.0_DFP * space)) .LT. tol), &
          "f = t: time 2")
END BLOCK

CALL u%DEALLOCATE()
CALL timefedof%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()

CONTAINS

SUBROUTINE oneFunc(args, nargs, ans)
  REAL(DFP), INTENT(IN) :: args(:)
  INTEGER(I4B), INTENT(IN) :: nargs
  REAL(DFP), INTENT(INOUT) :: ans
  ans = 1.0_DFP
END SUBROUTINE oneFunc

SUBROUTINE tFunc(args, nargs, ans)
  REAL(DFP), INTENT(IN) :: args(:)
  INTEGER(I4B), INTENT(IN) :: nargs
  REAL(DFP), INTENT(INOUT) :: ans
  ans = args(4)
END SUBROUTINE tFunc

END PROGRAM main
