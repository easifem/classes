!> author: Vikas Sharma, Ph. D.
! date: 2026-10-29
! summary: Testing VectorField GetVtkUnstructuredValue
!
! The field u of order 3 (H1 Lagrange, equidistance points, two space
! components) on square_3x2.h5 (two Quadrangle4 elements) is set by the
! function f(x, y) = [x (x - 10) y (y - 10), x + 2 y].
! GetVtkUnstructuredGridData gives 16 points in each cell (32 points),
! and GetVtkUnstructuredValue gives the vector of u at each point, in the
! same order, as VALUE(3, 32); the third row is zero. Since the points
! are the interpolation points of u, the values are equal to f at the
! points.

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
                           "GetVtkUnstructuredValue_test_1.toml"
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

! user function
CALL func%Initiate(NAME="f", returnType=TypeFEVariableOpt%vector, &
                   argType=TypeFEVariableOpt%space, numArgs=4, &
                   numReturns=2)
procptr => vectorFunc
CALL func%SetVectorFunctionPointer(func=procptr)

! domain and u = f
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")
CALL u%Set(VALUE=math%zero)
CALL u%SetByFunction(func=func)

! grid and values
CALL u%GetVtkUnstructuredGridData(points=points, &
                                  connectivity=connectivity, &
                                  offsets=offsets, types=types)
CALL u%GetVtkUnstructuredValue(VALUE=VALUE)

CALL OK(SIZE(VALUE, 1) .EQ. 3, "three rows")
CALL OK(SIZE(VALUE, 2) .EQ. 32, "number of columns")
CALL OK(SIZE(VALUE, 2) .EQ. SIZE(points, 2), "one vector for each point")
CALL OK(ALL(VALUE(3, :) .EQ. math%zero), "third row is zero")

ALLOCATE (fvals(2, SIZE(VALUE, 2)))
DO ii = 1, SIZE(VALUE, 2)
  CALL vectorFunc(points(:, ii), 3, fvals(:, ii), tsize)
END DO
CALL OK(MAXVAL(ABS(VALUE(1:2, :) - fvals)) .LT. tol, &
        "value = f at points")

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
  ans(1) = args(1) * (args(1) - 10.0_DFP) * args(2) * &
           (args(2) - 10.0_DFP)
  ans(2) = args(1) + 2.0_DFP * args(2)
END SUBROUTINE vectorFunc

END PROGRAM main
