!> author: Vikas Sharma, Ph. D.
! date: 2026-10-28
! summary: Testing ScalarField GetVtkUnstructuredValue
!
! The field u of order 3 (H1 Lagrange, equidistance points) on
! square_3x2.h5 (two Quadrangle4 elements) is set by the function
! f(x, y) = x (x - 10) y (y - 10). GetVtkUnstructuredGridData gives
! 16 points in each cell (32 points), and GetVtkUnstructuredValue
! gives the value of u at each point, in the same order. Since the
! points are the interpolation points of u, the values are equal to f
! at the points.
!
! Changes:
!   - 2026-10-28: values checked against the user function

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: TypeFEVariableOpt
USE BaseType, ONLY: InterfaceScalarSubroutine
USE ExceptionHandler_Class, ONLY: e
USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: INT8
USE ScalarField_Class, ONLY: ScalarField_
USE Test_Method, ONLY: OK
USE UserFunction_Class, ONLY: UserFunction_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: tomlFileName = &
                           "GetVtkUnstructuredGridData_test_1.toml"
REAL(DFP), PARAMETER :: tol = 1.0E-8_DFP
TYPE(FEDomain_) :: dom
TYPE(FEDOF_), TARGET :: fedof, geofedof
TYPE(ScalarField_) :: u
INTEGER(I4B), ALLOCATABLE :: connectivity(:), offsets(:)
INTEGER(INT8), ALLOCATABLE :: types(:)
REAL(DFP), ALLOCATABLE :: points(:, :), VALUE(:), fvals(:)
TYPE(UserFunction_) :: func
PROCEDURE(InterfaceScalarSubroutine), POINTER :: procptr
INTEGER(I4B) :: ii

CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

! user function
CALL func%Initiate(NAME="func1", returnType=TypeFEVariableOpt%scalar, &
                   argType=TypeFEVariableOpt%space, numArgs=4, &
                   numReturns=1)
procptr => scalarFunc
CALL func%SetScalarFunctionPointer(func=procptr)

! domain and u = f
CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      filename=tomlFileName, tomlName="u")
CALL u%Set(math%zero)
CALL u%SetByFunction(func=func)

! grid and values
CALL u%GetVtkUnstructuredGridData(points=points, &
                                  connectivity=connectivity, &
                                  offsets=offsets, types=types)
CALL u%GetVtkUnstructuredValue(VALUE=VALUE)

CALL OK(SIZE(VALUE) .EQ. 32, "size of value")
CALL OK(SIZE(VALUE) .EQ. SIZE(points, 2), "one value for each point")
CALL OK(SIZE(VALUE) .EQ. fedof%GetSumElemTotalDOF(), &
        "size = GetSumElemTotalDOF")

ALLOCATE (fvals(SIZE(VALUE)))
DO ii = 1, SIZE(VALUE)
  CALL scalarFunc(points(:, ii), 3, fvals(ii))
END DO
CALL OK(MAXVAL(ABS(VALUE - fvals)) .LT. tol, "value = f at points")

CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()

CONTAINS

SUBROUTINE scalarFunc(args, nargs, ans)
  REAL(DFP), INTENT(IN) :: args(:)
  INTEGER(I4B), INTENT(IN) :: nargs
  REAL(DFP), INTENT(INOUT) :: ans
  ans = args(1) * (args(1) - 10.0_DFP) * args(2) * (args(2) - 10.0_DFP)
END SUBROUTINE scalarFunc

END PROGRAM main
