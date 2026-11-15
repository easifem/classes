!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Set a vector field by a user function on triangles of order 3
!
! The field u (order 3 Lagrange, two components) on small_tri3_mesh.h5
! (Triangle3 elements with different orientations) is set by the vector
! function f(x, y) = [x**2 y + 1, x - y**2], a polynomial of degree 3.
! On an edge whose orientation is reversed, the two edge DOF must not be
! swapped. Since f is exact in the space of u, the values at the VTK
! points (GetVtkUnstructuredValue) are equal to f at the points.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: TypeFEVariableOpt
USE BaseType, ONLY: InterfaceVectorSubroutine
USE ExceptionHandler_Class, ONLY: e
USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: INT8
USE Test_Method, ONLY: OK
USE UserFunction_Class, ONLY: UserFunction_
USE VectorField_Class, ONLY: VectorField_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = &
                           "GetVtkUnstructuredValue_test_2.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-8_DFP
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(VectorField_) :: u
INTEGER(I4B), ALLOCATABLE :: connectivity(:), offsets(:)
INTEGER(INT8), ALLOCATABLE :: types(:)
REAL(DFP), ALLOCATABLE :: points(:, :), VALUE(:, :), fvals(:, :)
TYPE(UserFunction_) :: func
PROCEDURE(InterfaceVectorSubroutine), POINTER :: procptr
INTEGER(I4B) :: ii, tsize

CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

CALL func%Initiate(NAME="f", returnType=TypeFEVariableOpt%vector, &
                   argType=TypeFEVariableOpt%space, numArgs=4, &
                   numReturns=2)
procptr => vectorFunc
CALL func%SetVectorFunctionPointer(func=procptr)

CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")
CALL u%Set(VALUE=math%zero)
CALL u%SetByFunction(func=func)

CALL u%GetVtkUnstructuredGridData(points=points, &
                                  connectivity=connectivity, &
                                  offsets=offsets, types=types)
CALL u%GetVtkUnstructuredValue(VALUE=VALUE)

ALLOCATE (fvals(2, SIZE(VALUE, 2)))
DO ii = 1, SIZE(VALUE, 2)
  CALL vectorFunc(points(:, ii), 3, fvals(:, ii), tsize)
END DO
CALL OK(MAXVAL(ABS(VALUE(1:2, :) - fvals)) .LT. tol, &
        "u = f at the VTK points")

CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()

CONTAINS

SUBROUTINE vectorFunc(args, nargs, ans, tsize)
  REAL(DFP), INTENT(IN) :: args(:)
  INTEGER(I4B), INTENT(IN) :: nargs
  REAL(DFP), INTENT(INOUT) :: ans(:)
  INTEGER(I4B), INTENT(OUT) :: tsize
  tsize = 2
  ans(1) = args(1)**2 * args(2) + 1.0_DFP
  ans(2) = args(1) - args(2)**2
END SUBROUTINE vectorFunc

END PROGRAM main
