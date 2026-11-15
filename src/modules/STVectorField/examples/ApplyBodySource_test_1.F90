!> author: Vikas Sharma, Ph. D.
! date: 2026-11-12
! summary: Add the body source to a space-time vector field
!
! The field u is order 1 Lagrange in space with two components on
! square_3x2.h5 (two Quadrangle4 elements [0, 5] x [0, 10] and
! [5, 10] x [0, 10]) and order 1 Lagrange in time on [0, 1]
! (timeCompo = 2). ApplyBodySource(bodySource, scale, times) adds
!
!   scale * integral over space and time of N_I T_a f_c(x, y, t)
!
! to space component c, time component a of node I.
!
! In space, integral(N_I) is 12.5 for nodes 1 to 4 and 25 for the
! shared nodes 5, 6; integral(N_I x) is 125/6 for nodes 1, 4, 625/6
! for nodes 2, 3 and 125 for nodes 5, 6. In time, integral(T_a) = 1/2,
! integral(T_1 t) = 1/6 and integral(T_2 t) = 1/3.
!
! The last test uses a field w with three components on the same
! (two-dimensional) mesh.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: TypeFEVariableOpt
USE BaseType, ONLY: InterfaceVectorSubroutine
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE STVectorField_Class, ONLY: STVectorField_
USE Test_Method, ONLY: OK
USE TimeFEDOF_Class, ONLY: TimeFEDOF_
USE UserFunction_Class, ONLY: UserFunction_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "STVectorField_test.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-8_DFP
REAL(DFP), PARAMETER :: times(2) = [0.0_DFP, 1.0_DFP]
REAL(DFP), PARAMETER :: one(6) = [12.5_DFP, 12.5_DFP, 12.5_DFP, &
                                  12.5_DFP, 25.0_DFP, 25.0_DFP]
REAL(DFP), PARAMETER :: xint(6) = [125.0_DFP / 6.0_DFP, &
                                   625.0_DFP / 6.0_DFP, &
                                   625.0_DFP / 6.0_DFP, &
                                   125.0_DFP / 6.0_DFP, &
                                   125.0_DFP, 125.0_DFP]
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(TimeFEDOF_), TARGET :: timefedof
TYPE(STVectorField_) :: u, w
TYPE(UserFunction_) :: funcOneX, funcT, func3
PROCEDURE(InterfaceVectorSubroutine), POINTER :: procptr
REAL(DFP) :: found(24)
INTEGER(I4B) :: tsize

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(tomlName="u", filename=tomlFileName, &
                      fedof=fedof, geofedof=geofedof, &
                      timefedof=timefedof, dom=dom)

CALL funcOneX%Initiate(NAME="f", returnType=TypeFEVariableOpt%vector, &
                       argType=TypeFEVariableOpt%space, numArgs=4, &
                       numReturns=2)
procptr => oneXFunc
CALL funcOneX%SetVectorFunctionPointer(func=procptr)

CALL funcT%Initiate(NAME="g", returnType=TypeFEVariableOpt%vector, &
                    argType=TypeFEVariableOpt%space, numArgs=4, &
                    numReturns=2)
procptr => tFunc
CALL funcT%SetVectorFunctionPointer(func=procptr)

CALL func3%Initiate(NAME="h", returnType=TypeFEVariableOpt%vector, &
                    argType=TypeFEVariableOpt%space, numArgs=4, &
                    numReturns=3)
procptr => threeFunc
CALL func3%SetVectorFunctionPointer(func=procptr)

! Test 1: f = [1, x]
BLOCK
  CALL u%Set(VALUE=0.0_DFP)
  CALL u%ApplyBodySource(bodySource=funcOneX, scale=1.0_DFP, &
                         times=times)
  CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=1, timeCompo=1)
  CALL OK(ALL(ABS(found(1:6) - 0.5_DFP * one) .LT. tol), &
          "f = [1, x]: x, time 1")
  CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=2, timeCompo=2)
  CALL OK(ALL(ABS(found(1:6) - 0.5_DFP * xint) .LT. tol), &
          "f = [1, x]: y, time 2")
END BLOCK

! Test 2: g = [t, 1], added with scale 6
BLOCK
  CALL u%Set(VALUE=1.0_DFP)
  CALL u%ApplyBodySource(bodySource=funcT, scale=6.0_DFP, times=times)
  CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=1, timeCompo=1)
  CALL OK(ALL(ABS(found(1:6) - (1.0_DFP + one)) .LT. tol), &
          "g = [t, 1]: x, time 1")
  CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=1, timeCompo=2)
  CALL OK(ALL(ABS(found(1:6) - (1.0_DFP + 2.0_DFP * one)) .LT. tol), &
          "g = [t, 1]: x, time 2")
  CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=2, timeCompo=1)
  CALL OK(ALL(ABS(found(1:6) - (1.0_DFP + 3.0_DFP * one)) .LT. tol), &
          "g = [t, 1]: y, time 1")
END BLOCK

! Test 3: three components on a 2D mesh, h = [1, x, 2]
BLOCK
  CALL w%Initiate(NAME="w", engine="NATIVE_SERIAL", spaceCompo=[3], &
                  fedof=fedof, geofedof=geofedof, timefedof=timefedof)
  CALL w%Set(VALUE=0.0_DFP)
  CALL w%ApplyBodySource(bodySource=func3, scale=1.0_DFP, times=times)
  CALL w%Get(VALUE=found, tsize=tsize, spaceCompo=2, timeCompo=1)
  CALL OK(ALL(ABS(found(1:6) - 0.5_DFP * xint) .LT. tol), &
          "spaceCompo = 3: y")
  CALL w%Get(VALUE=found, tsize=tsize, spaceCompo=3, timeCompo=2)
  CALL OK(ALL(ABS(found(1:6) - one) .LT. tol), "spaceCompo = 3: z")
END BLOCK

CALL w%DEALLOCATE()
CALL u%DEALLOCATE()
CALL timefedof%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()

CONTAINS

SUBROUTINE oneXFunc(args, nargs, ans, tsize)
  REAL(DFP), INTENT(IN) :: args(:)
  INTEGER(I4B), INTENT(IN) :: nargs
  REAL(DFP), INTENT(INOUT) :: ans(:)
  INTEGER(I4B), INTENT(OUT) :: tsize
  tsize = 2
  ans(1) = 1.0_DFP
  ans(2) = args(1)
END SUBROUTINE oneXFunc

SUBROUTINE tFunc(args, nargs, ans, tsize)
  REAL(DFP), INTENT(IN) :: args(:)
  INTEGER(I4B), INTENT(IN) :: nargs
  REAL(DFP), INTENT(INOUT) :: ans(:)
  INTEGER(I4B), INTENT(OUT) :: tsize
  tsize = 2
  ans(1) = args(4)
  ans(2) = 1.0_DFP
END SUBROUTINE tFunc

SUBROUTINE threeFunc(args, nargs, ans, tsize)
  REAL(DFP), INTENT(IN) :: args(:)
  INTEGER(I4B), INTENT(IN) :: nargs
  REAL(DFP), INTENT(INOUT) :: ans(:)
  INTEGER(I4B), INTENT(OUT) :: tsize
  tsize = 3
  ans(1) = 1.0_DFP
  ans(2) = args(1)
  ans(3) = 2.0_DFP
END SUBROUTINE threeFunc

END PROGRAM main
