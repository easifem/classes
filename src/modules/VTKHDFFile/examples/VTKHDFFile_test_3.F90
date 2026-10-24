!> author: Shion Shimizu
! date: 2026-10-24
! summary: Write and read time steps

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
USE FEMesh_Class, ONLY: FEMesh_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE HDF5File_Class, ONLY: HDF5File_
USE Test_Method, ONLY: OK
USE VTKHDFFile_Class, ONLY: VTKHDFFile_
IMPLICIT NONE
CHARACTER(*), PARAMETER :: filename = "VTKHDFFile_test_3.vtkhdf"
TYPE(HDF5File_) :: meshfile
TYPE(FEMesh_) :: amesh
TYPE(VTKHDFFile_) :: obj
REAL(DFP), ALLOCATABLE :: u(:), vals(:, :), times(:)
INTEGER(I4B) :: istep, nPoints
CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
CALL meshfile%Initiate(fileName="./meshdata/small_tri3_mesh.h5", &
                       MODE="READ")
CALL meshfile%OPEN()
CALL amesh%Initiate(hdf5=meshfile, dim=2)
nPoints = amesh%GetTotalNodes()
ALLOCATE (u(nPoints))
! 4 steps, every 2 steps are written (steps 1 and 3)
CALL obj%InitiateVTKHDF(filename=filename, mode="NEW", frequency=2)
CALL obj%WriteMesh(amesh)
DO istep = 1, 4
  CALL obj%BeginStep(time=0.1_DFP * istep)
  IF (obj%IsOutputStep()) THEN
    u = REAL(istep, DFP)
    CALL obj%WritePointData(name="u", vals=u)
  END IF
  CALL obj%EndStep()
END DO
CALL obj%DEALLOCATE()
CALL obj%InitiateVTKHDF(filename=filename, mode="READ")
CALL OK(obj%GetTotalSteps() .EQ. 2, "GetTotalSteps")
CALL obj%GetTimeValues(times)
CALL OK(ABS(times(2) - 0.3_DFP) .LT. 1.0E-12_DFP, "GetTimeValues")
CALL obj%ReadPointData(name="u", vals=vals, step=2)
CALL OK(ALL(ABS(vals - 3.0_DFP) .LT. 1.0E-12_DFP), "ReadPointData step")
CALL obj%DEALLOCATE()
CALL amesh%DEALLOCATE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
