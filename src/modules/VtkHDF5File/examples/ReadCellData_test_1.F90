!> author: Vikas Sharma, Ph. D.
! date: 2026-10-28
! summary: Read cell data of each step of a transient file
!
! The grid A (one quadrangle) is written before the first step, and a
! new grid B (two triangles) in the second step. The cell data c is
! [10] in step 1 and [20, 21] in step 2. ReadCellData gives one tuple
! for step 1 and two tuples for step 2, as many as the cells of the
! grid of the step. Without step, the first data is read.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: TypeFileOpt
USE ExceptionHandler_Class, ONLY: e
USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: INT8
USE Test_Method, ONLY: OK
USE VtkHDF5File_Class, ONLY: VtkHDF5File_
IMPLICIT NONE

CHARACTER(*), PARAMETER :: filename = "ReadCellData_test_1.vtkhdf"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(VtkHDF5File_) :: obj
REAL(DFP) :: points(3, 4)
REAL(DFP), ALLOCATABLE :: vals(:, :)
LOGICAL :: isok

CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

points = RESHAPE([0.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP, &
                  1.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP], &
                 [3, 4])

CALL obj%InitiateVTKHDF5(filename=filename, mode=TypeFileOpt%new, &
                         vtkType="UnstructuredGrid", &
                         isTransient=math%yes)
! step 1: grid A
CALL obj%WriteUnstructuredGrid(points, [0, 1, 2, 3], [0, 4], [9_INT8])
CALL obj%BeginTimeStep(time=1.0_DFP)
CALL obj%WriteCellData(NAME="c", vals=[10.0_DFP])
CALL obj%EndTimeStep()
! step 2: grid B
CALL obj%BeginTimeStep(time=2.0_DFP)
CALL obj%WriteUnstructuredGrid(points, [0, 1, 2, 0, 2, 3], [0, 3, 6], &
                               [5_INT8, 5_INT8])
CALL obj%WriteCellData(NAME="c", vals=[20.0_DFP, 21.0_DFP])
CALL obj%EndTimeStep()
CALL obj%DEALLOCATE()

CALL obj%InitiateVTKHDF5(filename=filename, mode=TypeFileOpt%READ, &
                         vtkType="UnstructuredGrid")

CALL obj%ReadCellData(NAME="c", vals=vals, step=1)
isok = ALL(SHAPE(vals) .EQ. [1, 1])
IF (isok) isok = ABS(vals(1, 1) - 10.0_DFP) .LT. tol
CALL OK(isok, "ReadCellData: step 1")

CALL obj%ReadCellData(NAME="c", vals=vals, step=2)
isok = ALL(SHAPE(vals) .EQ. [1, 2])
IF (isok) isok = MAXVAL(ABS(vals(1, :) - [20.0_DFP, 21.0_DFP])) .LT. tol
CALL OK(isok, "ReadCellData: step 2")

CALL obj%ReadCellData(NAME="c", vals=vals)
isok = ALL(SHAPE(vals) .EQ. [1, 1])
IF (isok) isok = ABS(vals(1, 1) - 10.0_DFP) .LT. tol
CALL OK(isok, "ReadCellData: without step")

CALL obj%DEALLOCATE()
END PROGRAM main
