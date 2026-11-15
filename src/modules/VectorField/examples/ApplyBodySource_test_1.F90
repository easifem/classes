!> author: Vikas Sharma, Ph. D.
! date: 2026-11-12
! summary: Add the body source to a vector field
!
! The field u is order 1 Lagrange with two components on square_3x2.h5:
! [0, 10] x [0, 10] with two Quadrangle4 elements [0, 5] x [0, 10] and
! [5, 10] x [0, 10]. ApplyBodySource adds scale * integral(N_I f_c) to
! component c of node I.
!
! integral(N_I) is 12.5 for nodes 1 to 4 and 25 for the shared nodes
! 5, 6. integral(N_I x) is 125/6 for nodes 1, 4, 625/6 for nodes 2, 3
! and 125 for nodes 5, 6.
!
!- ApplyBodySource(bodySource=func) uses a vector user function; the
!  time times is passed to it as the fourth argument.
!- ApplyBodySource(bodySource=v) uses a vector field; with v = [1, x]
!  at the nodes (exact for order 1) it gives the same result.
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
USE Test_Method, ONLY: OK
USE UserFunction_Class, ONLY: UserFunction_
USE VectorField_Class, ONLY: VectorField_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "VectorField_test.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-8_DFP
REAL(DFP), PARAMETER :: one(6) = [12.5_DFP, 12.5_DFP, 12.5_DFP, &
                                  12.5_DFP, 25.0_DFP, 25.0_DFP]
REAL(DFP), PARAMETER :: xint(6) = [125.0_DFP / 6.0_DFP, &
                                   625.0_DFP / 6.0_DFP, &
                                   625.0_DFP / 6.0_DFP, &
                                   125.0_DFP / 6.0_DFP, &
                                   125.0_DFP, 125.0_DFP]
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(VectorField_) :: u, v, w
TYPE(UserFunction_) :: funcOneX, funcT, func3
PROCEDURE(InterfaceVectorSubroutine), POINTER :: procptr
REAL(DFP) :: found(6)
INTEGER(I4B) :: tsize

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")
CALL v%Initiate(NAME="v", engine="NATIVE_SERIAL", spaceCompo=[2], &
                fedof=fedof, geofedof=geofedof)

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

! Test 1: user function f = [1, x]
BLOCK
  CALL u%Set(VALUE=0.0_DFP)
  CALL u%ApplyBodySource(bodySource=funcOneX, scale=1.0_DFP)
  CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=1)
  CALL OK(ALL(ABS(found - one) .LT. tol), "f = [1, x]: x")
  CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=2)
  CALL OK(ALL(ABS(found - xint) .LT. tol), "f = [1, x]: y")
END BLOCK

! Test 2: the source is added, with scale
BLOCK
  CALL u%Set(VALUE=1.0_DFP)
  CALL u%ApplyBodySource(bodySource=funcOneX, scale=2.0_DFP)
  CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=1)
  CALL OK(ALL(ABS(found - (1.0_DFP + 2.0_DFP * one)) .LT. tol), &
          "add with scale 2: x")
  CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=2)
  CALL OK(ALL(ABS(found - (1.0_DFP + 2.0_DFP * xint)) .LT. tol), &
          "add with scale 2: y")
END BLOCK

! Test 3: time dependent source g = [t, 1] at times = 2
BLOCK
  CALL u%Set(VALUE=0.0_DFP)
  CALL u%ApplyBodySource(bodySource=funcT, scale=1.0_DFP, &
                         times=2.0_DFP)
  CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=1)
  CALL OK(ALL(ABS(found - 2.0_DFP * one) .LT. tol), "g = [t, 1]: x")
  CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=2)
  CALL OK(ALL(ABS(found - one) .LT. tol), "g = [t, 1]: y")
END BLOCK

! Test 4: vector field v = [1, x]
BLOCK
  CALL v%Set(VALUE=1.0_DFP, spaceCompo=1)
  CALL v%Set(VALUE=[0.0_DFP, 10.0_DFP, 10.0_DFP, 0.0_DFP, 5.0_DFP, &
                    5.0_DFP], spaceCompo=2)
  CALL u%Set(VALUE=0.0_DFP)
  CALL u%ApplyBodySource(bodySource=v, scale=1.0_DFP)
  CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=1)
  CALL OK(ALL(ABS(found - one) .LT. tol), "field v = [1, x]: x")
  CALL u%Get(VALUE=found, tsize=tsize, spaceCompo=2)
  CALL OK(ALL(ABS(found - xint) .LT. tol), "field v = [1, x]: y")
END BLOCK

! Test 5: three components on a 2D mesh, h = [1, x, 2]
BLOCK
  CALL w%Initiate(NAME="w", engine="NATIVE_SERIAL", spaceCompo=[3], &
                  fedof=fedof, geofedof=geofedof)
  CALL w%Set(VALUE=0.0_DFP)
  CALL w%ApplyBodySource(bodySource=func3, scale=1.0_DFP)
  CALL w%Get(VALUE=found, tsize=tsize, spaceCompo=2)
  CALL OK(ALL(ABS(found - xint) .LT. tol), "spaceCompo = 3: y")
  CALL w%Get(VALUE=found, tsize=tsize, spaceCompo=3)
  CALL OK(ALL(ABS(found - 2.0_DFP * one) .LT. tol), "spaceCompo = 3: z")
END BLOCK

CALL w%DEALLOCATE()
CALL v%DEALLOCATE()
CALL u%DEALLOCATE()
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
