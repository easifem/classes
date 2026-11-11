!> author: Vikas Sharma, Ph. D.
! date: 2026-11-12
! summary: Add the body source to a scalar field
!
! The field u is order 1 Lagrange on square_3x2.h5: [0, 10] x [0, 10]
! with two Quadrangle4 elements [0, 5] x [0, 10] and [5, 10] x [0, 10].
! ApplyBodySource adds scale * integral(N_I f) to entry I.
!
! For f = 1 each node of an element gets area / 4 = 12.5, so nodes
! 1 to 4 get 12.5 and the shared nodes 5, 6 get 25.
!
! For f = x the exact values are
!   nodes 1, 4: 125/6, nodes 2, 3: 625/6, nodes 5, 6: 125.
!
! ApplyBodySource(bodySource=func) uses a user function, and
! ApplyBodySource(bodySource=v) a scalar field; with v = x at the nodes
! (exact for order 1) both give the same result.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: TypeFEVariableOpt
USE BaseType, ONLY: InterfaceScalarSubroutine
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE ScalarField_Class, ONLY: ScalarField_
USE Test_Method, ONLY: OK
USE UserFunction_Class, ONLY: UserFunction_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = "ScalarField_test.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(ScalarField_) :: u, v
TYPE(UserFunction_) :: funcOne, funcX
PROCEDURE(InterfaceScalarSubroutine), POINTER :: procptr
REAL(DFP) :: found(6), want(6)
INTEGER(I4B) :: tsize

CALL e%SetQuietMode(math%yes)
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")
CALL v%Initiate(NAME="v", engine="NATIVE_SERIAL", fedof=fedof, &
                geofedof=geofedof)

CALL funcOne%Initiate(NAME="one", returnType=TypeFEVariableOpt%scalar, &
                      argType=TypeFEVariableOpt%space, numArgs=4, &
                      numReturns=1)
procptr => oneFunc
CALL funcOne%SetScalarFunctionPointer(func=procptr)

CALL funcX%Initiate(NAME="x", returnType=TypeFEVariableOpt%scalar, &
                    argType=TypeFEVariableOpt%space, numArgs=4, &
                    numReturns=1)
procptr => xFunc
CALL funcX%SetScalarFunctionPointer(func=procptr)

! Test 1: user function f = 1
BLOCK
  CALL u%Set(VALUE=0.0_DFP)
  CALL u%ApplyBodySource(bodySource=funcOne, scale=1.0_DFP)
  CALL u%Get(VALUE=found, tsize=tsize)
  want = [12.5_DFP, 12.5_DFP, 12.5_DFP, 12.5_DFP, 25.0_DFP, 25.0_DFP]
  CALL OK(ALL(ABS(found - want) .LT. tol), "f = 1")
  CALL OK(ABS(SUM(found) - 100.0_DFP) .LT. tol, "f = 1: sum = area")
END BLOCK

! Test 2: the source is added, with scale
BLOCK
  CALL u%Set(VALUE=1.0_DFP)
  CALL u%ApplyBodySource(bodySource=funcOne, scale=2.0_DFP, &
                         times=0.0_DFP)
  CALL u%Get(VALUE=found, tsize=tsize)
  want = [26.0_DFP, 26.0_DFP, 26.0_DFP, 26.0_DFP, 51.0_DFP, 51.0_DFP]
  CALL OK(ALL(ABS(found - want) .LT. tol), "f = 1: add with scale 2")
END BLOCK

! Test 3: user function f = x
BLOCK
  CALL u%Set(VALUE=0.0_DFP)
  CALL u%ApplyBodySource(bodySource=funcX, scale=1.0_DFP)
  CALL u%Get(VALUE=found, tsize=tsize)
  want = [125.0_DFP / 6.0_DFP, 625.0_DFP / 6.0_DFP, &
          625.0_DFP / 6.0_DFP, 125.0_DFP / 6.0_DFP, &
          125.0_DFP, 125.0_DFP]
  CALL OK(ALL(ABS(found - want) .LT. tol), "f = x")
END BLOCK

! Test 4: scalar field v = x
BLOCK
  CALL v%Set(VALUE=[0.0_DFP, 10.0_DFP, 10.0_DFP, 0.0_DFP, 5.0_DFP, &
                    5.0_DFP])
  CALL u%Set(VALUE=0.0_DFP)
  CALL u%ApplyBodySource(bodySource=v, scale=1.0_DFP)
  CALL u%Get(VALUE=found, tsize=tsize)
  CALL OK(ALL(ABS(found - want) .LT. tol), "field v = x")
END BLOCK

CALL v%DEALLOCATE()
CALL u%DEALLOCATE()
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

SUBROUTINE xFunc(args, nargs, ans)
  REAL(DFP), INTENT(IN) :: args(:)
  INTEGER(I4B), INTENT(IN) :: nargs
  REAL(DFP), INTENT(INOUT) :: ans
  ans = args(1)
END SUBROUTINE xFunc

END PROGRAM main
