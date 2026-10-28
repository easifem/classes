!> author: Shion Shimizu
! date: 2026-10-24
! update:
!   - 2026-10-28: checks of the grid, the values and the written file
! summary: Write a Lagrange grid and a scalar field
!
! The mesh square_3x2.h5 is the square [0, 10] x [0, 10] with two
! Quadrangle4 elements. The scalar field u of order 3 (H1 Lagrange,
! equidistance points) is set by the function
!
!   f(x, y) = x (x - 10) y (y - 10)
!
! GetVtkUnstructuredGridData of u gives a discontinuous grid of two
! VTK_LAGRANGE_QUADRILATERAL (70) cells with 16 points each, and
! GetVtkUnstructuredValue gives the value of u at each point. Since
! the points are the interpolation points of u, the values are equal
! to f at the points (zero on the boundary). The grid and the values
! are written, and the file is read back as an HDF5File_.

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE BaseType, ONLY: TypeFileOpt
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
  USE GlobalData, ONLY: LGT
  USE HDF5File_Class, ONLY: HDF5File_
  USE ScalarField_Class, ONLY: ScalarField_
  USE Test_Method, ONLY: OK
  USE UserFunction_Class, ONLY: UserFunction_
  USE VtkHDF5File_Class, ONLY: VtkHDF5File_
  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: tomlFileName = &
    "WriteUnstructuredGrid_test_3.toml", &
    filename = "WriteUnstructuredGrid_test_3.vtkhdf"
  TYPE(FEDomain_) :: dom
  TYPE(FEDOF_) :: fedof
  TYPE(FEDOF_) :: geofedof
  TYPE(ScalarField_) :: u
  TYPE(VtkHDF5File_) :: obj
  TYPE(UserFunction_) :: func
  PROCEDURE(InterfaceScalarSubroutine), POINTER :: procptr
  REAL(DFP), ALLOCATABLE :: points(:, :), VALUE(:), fvals(:), rvals1(:)
  INTEGER(I4B), ALLOCATABLE :: connectivity(:), offsets(:), ivals(:)
  INTEGER(INT8), ALLOCATABLE :: types(:), tvals(:)
  LOGICAL(LGT), ALLOCATABLE :: onBoundary(:)
  INTEGER(I4B) :: ii

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

! user function f(x, y)
  CALL func%Initiate(NAME="func1", returnType=TypeFEVariableOpt%scalar, &
                     argType=TypeFEVariableOpt%space, numArgs=4, &
                     numReturns=1)
  procptr => scalarFunc
  CALL func%SetScalarFunctionPointer(func=procptr)

! domain and scalar field u = f
  CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
  CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                        filename=tomlFileName, tomlName="u")
  CALL u%Set(math%zero)
  CALL u%SetByFunction(func=func)

! Lagrange grid and values of u
  CALL u%GetVtkUnstructuredGridData(points=points, &
                                    connectivity=connectivity, &
                                    offsets=offsets, types=types)
  CALL u%GetVtkUnstructuredValue(VALUE=VALUE)
! write the grid and u
  CALL obj%InitiateVTKHDF5(filename=filename, mode=TypeFileOpt%new, &
                           vtkType="UnstructuredGrid")
  CALL obj%WriteUnstructuredGrid(points, connectivity, offsets, types)
  CALL obj%WritePointData(NAME="u", VALUE=VALUE)

  CALL obj%DEALLOCATE()

! clean up
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
