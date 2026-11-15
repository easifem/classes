!> author: Vikas Sharma, Ph. D.
! date: 2026-10-28
! summary: Read point data of each step of a transient file
!
! The grid (one quadrangle, four points) is written once. Three steps
! are written: the scalar u = k * [1, 2, 3, 4] is written in each step
! k, and the vector v (two components) only in the first step, so v is
! static. ReadPointData gives vals of shape (1, 4) for u and (2, 4) for
! v. The data offset of u (step k starts at 4 * (k - 1)) differs from
! the geometry offset (always 0). Without step, the first data is read.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: TypeFileOpt
USE ExceptionHandler_Class, ONLY: e
USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: INT8
USE Test_Method, ONLY: OK
USE VtkHDF5File_Class, ONLY: VtkHDF5File_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: filename = "ReadPointData_test_1.vtkhdf"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(VtkHDF5File_) :: obj
REAL(DFP) :: points(3, 4), u0(4), v0(2, 4)
REAL(DFP), ALLOCATABLE :: vals(:, :)
INTEGER(I4B) :: istep
LOGICAL :: isok

CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

points = RESHAPE([0.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP, &
                  1.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP], &
                 [3, 4])
u0 = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP]
v0(1, :) = u0
v0(2, :) = -u0

CALL obj%InitiateVTKHDF5(filename=filename, mode=TypeFileOpt%new, &
                         vtkType="UnstructuredGrid", &
                         isTransient=math%yes)
CALL obj%WriteUnstructuredGrid(points, [0, 1, 2, 3], [0, 4], [9_INT8])
DO istep = 1, 3
  CALL obj%BeginTimeStep(time=REAL(istep, DFP))
  CALL obj%WritePointData(NAME="u", VALUE=istep * u0)
  IF (istep .EQ. 1) CALL obj%WritePointData(NAME="v", VALUE=v0)
  CALL obj%EndTimeStep()
END DO
CALL obj%DEALLOCATE()

CALL obj%InitiateVTKHDF5(filename=filename, mode=TypeFileOpt%READ, &
                         vtkType="UnstructuredGrid")

! u in each step
isok = .TRUE.
DO istep = 1, 3
  CALL obj%ReadPointData(NAME="u", vals=vals, step=istep)
  isok = isok .AND. ALL(SHAPE(vals) .EQ. [1, 4])
  IF (isok) isok = MAXVAL(ABS(vals(1, :) - istep * u0)) .LT. tol
END DO
CALL OK(isok, "ReadPointData: u in each step")

! u without step: first data
CALL obj%ReadPointData(NAME="u", vals=vals)
isok = ALL(SHAPE(vals) .EQ. [1, 4])
IF (isok) isok = MAXVAL(ABS(vals(1, :) - u0)) .LT. tol
CALL OK(isok, "ReadPointData: u without step")

! v is static: the same in all steps
isok = .TRUE.
DO istep = 1, 3
  CALL obj%ReadPointData(NAME="v", vals=vals, step=istep)
  isok = isok .AND. ALL(SHAPE(vals) .EQ. [2, 4])
  IF (isok) isok = MAXVAL(ABS(vals - v0)) .LT. tol
END DO
CALL OK(isok, "ReadPointData: static v")

CALL obj%DEALLOCATE()
END PROGRAM main
