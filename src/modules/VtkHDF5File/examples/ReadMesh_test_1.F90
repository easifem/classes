!> author: Vikas Sharma, Ph. D.
! date: 2026-10-28
! summary: Read the grid of each step of a transient file
!
! Three steps are written. The grid A (one quadrangle) is written before
! the first step. In the second step a new grid B (two triangles with
! moved points) is written, and the third step uses B again. ReadMesh
! gives A for step 1, B for steps 2 and 3, and A (the first part) when
! step is not present.

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

CHARACTER(*), PARAMETER :: filename = "ReadMesh_test_1.vtkhdf"
TYPE(VtkHDF5File_) :: obj
REAL(DFP) :: pointsA(3, 4), pointsB(3, 4)
INTEGER(I4B), PARAMETER :: connA(4) = [0, 1, 2, 3], &
                           offsetsA(2) = [0, 4], &
                           connB(6) = [0, 1, 2, 0, 2, 3], &
                           offsetsB(3) = [0, 3, 6]
INTEGER(INT8), PARAMETER :: typesA(1) = [9_INT8], &
                            typesB(2) = [5_INT8, 5_INT8]
INTEGER(I4B) :: istep

CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

pointsA = RESHAPE([0.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, &
                   0.0_DFP, 1.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP, &
                   1.0_DFP, 0.0_DFP], [3, 4])
pointsB = 2.0_DFP * pointsA

CALL obj%InitiateVTKHDF5(filename=filename, mode=TypeFileOpt%new, &
                         vtkType="UnstructuredGrid", &
                         isTransient=math%yes)
CALL obj%WriteUnstructuredGrid(pointsA, connA, offsetsA, typesA)
DO istep = 1, 3
  CALL obj%BeginTimeStep(time=REAL(istep, DFP))
  IF (istep .EQ. 2) THEN
    CALL obj%WriteUnstructuredGrid(pointsB, connB, offsetsB, typesB)
  END IF
  CALL obj%EndTimeStep()
END DO
CALL obj%DEALLOCATE()

CALL obj%InitiateVTKHDF5(filename=filename, mode=TypeFileOpt%READ, &
                         vtkType="UnstructuredGrid")
CALL CheckMesh(obj, pointsA, connA, offsetsA, typesA, "no step")
CALL CheckMesh(obj, pointsA, connA, offsetsA, typesA, "step 1", 1)
CALL CheckMesh(obj, pointsB, connB, offsetsB, typesB, "step 2", 2)
CALL CheckMesh(obj, pointsB, connB, offsetsB, typesB, "step 3", 3)
CALL obj%DEALLOCATE()

CONTAINS

SUBROUTINE CheckMesh(obj, points0, conn0, offsets0, types0, msg, step)
  TYPE(VtkHDF5File_), INTENT(INOUT) :: obj
  REAL(DFP), INTENT(IN) :: points0(:, :)
  INTEGER(I4B), INTENT(IN) :: conn0(:), offsets0(:)
  INTEGER(INT8), INTENT(IN) :: types0(:)
  CHARACTER(*), INTENT(IN) :: msg
  INTEGER(I4B), INTENT(IN), OPTIONAL :: step

  REAL(DFP), ALLOCATABLE :: points(:, :)
  INTEGER(I4B), ALLOCATABLE :: conn(:), offsets(:)
  INTEGER(INT8), ALLOCATABLE :: types(:)
  LOGICAL :: isok

  CALL obj%ReadMesh(points, conn, offsets, types, step)

  isok = ALL(SHAPE(points) .EQ. SHAPE(points0))
  IF (isok) isok = MAXVAL(ABS(points - points0)) .LT. 1.0E-12_DFP
  CALL OK(isok, "ReadMesh: points, "//msg)

  isok = SIZE(conn) .EQ. SIZE(conn0)
  IF (isok) isok = ALL(conn .EQ. conn0)
  CALL OK(isok, "ReadMesh: connectivity, "//msg)

  isok = SIZE(offsets) .EQ. SIZE(offsets0)
  IF (isok) isok = ALL(offsets .EQ. offsets0)
  CALL OK(isok, "ReadMesh: offsets, "//msg)

  isok = SIZE(types) .EQ. SIZE(types0)
  IF (isok) isok = ALL(types .EQ. types0)
  CALL OK(isok, "ReadMesh: types, "//msg)
END SUBROUTINE CheckMesh

END PROGRAM main
