!> author: Vikas Sharma, Ph. D.
! date: 2026-10-28
! summary: Write a transient scalar field
!
! The mesh very_small_quad4_mesh.h5 is the unit square with four
! Quadrangle4 elements. The scalar field u of order 3 (H1 Lagrange,
! equidistance points) depends on time:
!
!   u(x, y, t) = (1 + t) x (x - 1) y (y - 1)
!
! The Lagrange grid of u (four VTK_LAGRANGE_QUADRILATERAL cells with 16
! points each) is written once before the first step. Then four time
! steps t = 0.25, 0.5, 0.75, 1 are run: in each step u is set by the
! user function at time t (SetByFunction with times), and its values at
! the points of the grid are written as point data. The time is also
! written as field data. The file is read back as an HDF5File_: the
! values of step k are the slab k of PointData/u.

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

  CHARACTER(*), PARAMETER :: tomlFileName = "BeginStep_test_2.toml", &
    filename = "BeginStep_test_2.vtkhdf"
  INTEGER(I4B), PARAMETER :: nsteps = 4
  REAL(DFP), PARAMETER :: dt = 0.25_DFP, tol = 1.0E-10_DFP
  TYPE(FEDomain_) :: dom
  TYPE(FEDOF_), TARGET :: fedof, geofedof
  TYPE(ScalarField_) :: u
  TYPE(VtkHDF5File_) :: obj
  TYPE(HDF5File_) :: h5
  TYPE(UserFunction_) :: func
  PROCEDURE(InterfaceScalarSubroutine), POINTER :: procptr
  REAL(DFP), ALLOCATABLE :: points(:, :), VALUE(:), fvals(:), &
    rvals1(:), rvals2(:, :)
  INTEGER(I4B), ALLOCATABLE :: connectivity(:), offsets(:), ivals(:)
  INTEGER(INT8), ALLOCATABLE :: types(:)
  REAL(DFP) :: t, args(4)
  INTEGER(I4B) :: istep, nPoints, ii, nsteps_found
  LOGICAL(LGT) :: isok

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

! user function u(x, y, t)
  CALL func%Initiate(NAME="u", returnType=TypeFEVariableOpt%scalar, &
                     argType=TypeFEVariableOpt%space, numArgs=4, &
                     numReturns=1)
  procptr => scalarFunc
  CALL func%SetScalarFunctionPointer(func=procptr)

! domain and scalar field
  CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
  CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                        filename=tomlFileName, tomlName="u")

! Lagrange grid, written once
  CALL u%GetVtkUnstructuredGridData(points=points, &
                                    connectivity=connectivity, &
                                    offsets=offsets, types=types)
  nPoints = SIZE(points, 2)
  CALL OK(nPoints .EQ. 64, "number of points")

  CALL obj%InitiateVTKHDF5(filename=filename, mode=TypeFileOpt%new, &
                           vtkType="UnstructuredGrid", &
                           isTransient=math%yes)
  CALL obj%WriteUnstructuredGrid(points, connectivity, offsets, types)

! time steps
  DO istep = 1, nsteps
    t = dt * istep
    CALL obj%BeginStep(time=t)
    IF (obj%IsOutputStep()) THEN
      CALL u%Set(math%zero)
      CALL u%SetByFunction(func=func, times=t)
      CALL u%GetVtkUnstructuredValue(VALUE=VALUE)
      CALL obj%WritePointData(NAME="u", VALUE=VALUE)
      CALL obj%WriteFieldData(NAME="time", vals=[t])
    END IF
    CALL obj%EndStep()
  END DO
  CALL obj%DEALLOCATE()

! read the file back
  CALL h5%Initiate(filename=filename, mode="READ")
  CALL h5%OPEN()

  CALL h5%ReadAttribute("/VTKHDF/Steps", "NSteps", nsteps_found)
  CALL OK(nsteps_found .EQ. nsteps, "NSteps")

  CALL h5%ReadSlab("/VTKHDF/Steps/Values", 0, nsteps, rvals1)
  CALL OK(MAXVAL(ABS(rvals1 - dt * [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP])) &
          .LT. tol, "Steps/Values")

! the grid is written once, all steps refer to it
  CALL h5%ReadSlab("/VTKHDF/NumberOfPoints", 0, 1, ivals)
  CALL OK(SIZE(ivals) .EQ. 1 .AND. ivals(1) .EQ. nPoints, "one part")
  CALL h5%ReadSlab("/VTKHDF/Steps/PointOffsets", 0, nsteps, ivals)
  CALL OK(ALL(ivals .EQ. 0), "Steps/PointOffsets")

! u: one slab of nPoints values in each step
  CALL h5%ReadSlab("/VTKHDF/Steps/PointDataOffsets/u", 0, nsteps, ivals)
  CALL OK(ALL(ivals .EQ. nPoints * [0, 1, 2, 3]), &
          "Steps/PointDataOffsets/u")

  ALLOCATE(fvals(nPoints))
  isok = .TRUE.
  DO istep = 1, nsteps
    t = dt * istep
    CALL h5%ReadSlab("/VTKHDF/PointData/u", (istep - 1) * nPoints, &
                     nPoints, rvals1)
    DO ii = 1, nPoints
      args(1:3) = points(:, ii)
      args(4) = t
      CALL scalarFunc(args, 4, fvals(ii))
    END DO
    isok = isok .AND. MAXVAL(ABS(rvals1 - fvals)) .LT. tol
  END DO
  CALL OK(isok, "PointData/u = u(x, y, t) in each step")

! time as field data
  CALL h5%ReadSlab("/VTKHDF/Steps/FieldDataOffsets/time", 0, nsteps, &
                   ivals)
  CALL OK(ALL(ivals .EQ. [0, 1, 2, 3]), "Steps/FieldDataOffsets/time")
  CALL h5%ReadSlab("/VTKHDF/FieldData/time", 0, nsteps, rvals2)
  CALL OK(ALL(SHAPE(rvals2) .EQ. [1, nsteps]), "FieldData/time: shape")
  CALL OK(MAXVAL(ABS(rvals2(1, :) - dt * [1.0_DFP, 2.0_DFP, 3.0_DFP, &
                                          4.0_DFP])) .LT. tol, &
          "FieldData/time: values")

  CALL h5%DEALLOCATE()

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
    ans = (1.0_DFP + args(4)) * args(1) * (args(1) - 1.0_DFP) * &
      args(2) * (args(2) - 1.0_DFP)
  END SUBROUTINE scalarFunc

END PROGRAM main
