!> author: Vikas Sharma, Ph. D.
! date: 2026-10-28
! summary: Number of steps and time values of a file
!
! A file which is not transient has no steps: GetTotalSteps returns 0
! and GetTimeValues returns an empty array. A transient file with three
! steps t = 0.5, 1.0, 1.5 is then written and read back. While the file
! is written, GetTotalSteps is called after each step: the read cache is
! reset by EndTimeStep, so the new number of steps is returned.

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

CHARACTER(*), PARAMETER :: file1 = "GetTimeValues_test_1a.vtkhdf", &
                           file2 = "GetTimeValues_test_1b.vtkhdf"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(VtkHDF5File_) :: obj
REAL(DFP), ALLOCATABLE :: times(:)
REAL(DFP) :: points(3, 4)
INTEGER(I4B) :: istep
LOGICAL :: isok

CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

! one quadrangle
points = RESHAPE([0.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP, &
                  1.0_DFP, 1.0_DFP, 0.0_DFP, 0.0_DFP, 1.0_DFP, 0.0_DFP], &
                 [3, 4])

! file which is not transient
CALL obj%InitiateVTKHDF5(filename=file1, mode=TypeFileOpt%new, &
                         vtkType="UnstructuredGrid")
CALL obj%WriteUnstructuredGrid(points, [0, 1, 2, 3], [0, 4], [9_INT8])
CALL obj%DEALLOCATE()

CALL obj%InitiateVTKHDF5(filename=file1, mode=TypeFileOpt%READ, &
                         vtkType="UnstructuredGrid")
CALL OK(obj%GetTotalSteps() .EQ. 0, "GetTotalSteps: not transient")
CALL obj%GetTimeValues(times)
CALL OK(SIZE(times) .EQ. 0, "GetTimeValues: not transient")
CALL obj%DEALLOCATE()

! transient file with three steps
CALL obj%InitiateVTKHDF5(filename=file2, mode=TypeFileOpt%new, &
                         vtkType="UnstructuredGrid", &
                         isTransient=math%yes)
CALL obj%WriteUnstructuredGrid(points, [0, 1, 2, 3], [0, 4], [9_INT8])
isok = .TRUE.
DO istep = 1, 3
  CALL obj%BeginTimeStep(time=0.5_DFP * istep)
  CALL obj%EndTimeStep()
  isok = isok .AND. (obj%GetTotalSteps() .EQ. istep)
END DO
CALL OK(isok, "GetTotalSteps: while writing")
CALL obj%DEALLOCATE()

CALL obj%InitiateVTKHDF5(filename=file2, mode=TypeFileOpt%READ, &
                         vtkType="UnstructuredGrid")
CALL OK(obj%GetTotalSteps() .EQ. 3, "GetTotalSteps: transient")
CALL obj%GetTimeValues(times)
CALL OK(SIZE(times) .EQ. 3, "GetTimeValues: size")
CALL OK(MAXVAL(ABS(times - [0.5_DFP, 1.0_DFP, 1.5_DFP])) .LT. tol, &
        "GetTimeValues: values")
CALL obj%DEALLOCATE()
END PROGRAM main
