!> author: Shion Shimizu
! date: 2026-10-24
! summary: Write time steps of a transient file
!
! Four steps are run with frequency 2, so only the steps 1 and 3 are
! written. The grid of small_tri3_mesh.h5 is written once, and the point
! data u = step number is written in each output step. The file is then
! opened as an HDF5File_ and the Steps group is read back.
!
! Changes:
!   - 2026-10-28: grid from GetVtkUnstructuredGridData; the Steps group
!     is checked with the methods of HDF5File_

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: TypeFileOpt
USE ExceptionHandler_Class, ONLY: e
USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
USE FEMesh_Class, ONLY: FEMesh_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: INT8
USE HDF5File_Class, ONLY: HDF5File_
USE Test_Method, ONLY: OK
USE VtkHDF5File_Class, ONLY: VtkHDF5File_

IMPLICIT NONE

CHARACTER(*), PARAMETER :: meshname = "./meshdata/small_tri3_mesh.h5"
CHARACTER(*), PARAMETER :: filename = "BeginTimeStep_test_1.vtkhdf"
REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
TYPE(HDF5File_) :: meshfile, h5
TYPE(FEMesh_) :: amesh
TYPE(VtkHDF5File_) :: obj
REAL(DFP), ALLOCATABLE :: points(:, :), u(:), rvals1(:)
INTEGER(I4B), ALLOCATABLE :: conn(:), offsets(:), ivals(:), &
                             ivals2(:, :)
INTEGER(INT8), ALLOCATABLE :: types(:)
INTEGER(I4B) :: istep, nPoints, nsteps

CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

CALL meshfile%Initiate(filename=meshname, mode=TypeFileOpt%READ)
CALL meshfile%OPEN()
CALL amesh%Initiate(hdf5=meshfile, DIM=2)

CALL amesh%GetVtkUnstructuredGridData(points=points, &
                                      connectivity=conn, &
                                      offsets=offsets, types=types)
nPoints = SIZE(points, 2)
ALLOCATE (u(nPoints))

! 4 steps, every 2 steps are written (steps 1 and 3)
CALL obj%InitiateVTKHDF5(filename=filename, mode=TypeFileOpt%new, &
                         vtkType="UnstructuredGrid", &
                         isTransient=math%yes, frequency=2)
CALL obj%WriteUnstructuredGrid(points=points, connectivity=conn, &
                               offsets=offsets, types=types)
DO istep = 1, 4
  CALL obj%BeginTimeStep(time=0.1_DFP * istep)
  CALL OK(obj%IsOutputStep() .EQV. (MOD(istep, 2) .EQ. 1), &
          "IsOutputStep: step "//CHAR(48 + istep))
  IF (obj%IsOutputStep()) THEN
    u = REAL(istep, DFP)
    CALL obj%WritePointData(NAME="u", VALUE=u)
  END IF
  CALL obj%EndTimeStep()
END DO
CALL obj%DEALLOCATE()

! read the Steps group back
CALL h5%Initiate(filename=filename, mode="READ")
CALL h5%OPEN()

CALL h5%ReadAttribute("/VTKHDF/Steps", "NSteps", nsteps)
CALL OK(nsteps .EQ. 2, "NSteps")

CALL h5%ReadSlab("/VTKHDF/Steps/Values", 0, 2, rvals1)
CALL OK(MAXVAL(ABS(rvals1 - [0.1_DFP, 0.3_DFP])) .LT. tol, &
        "Steps/Values")

! the grid is written once, all steps refer to it
CALL h5%ReadSlab("/VTKHDF/NumberOfPoints", 0, 1, ivals)
CALL OK(SIZE(ivals) .EQ. 1 .AND. ivals(1) .EQ. nPoints, &
        "one part")
CALL h5%ReadSlab("/VTKHDF/Steps/PartOffsets", 0, 2, ivals)
CALL OK(ALL(ivals .EQ. 0), "Steps/PartOffsets")
CALL h5%ReadSlab("/VTKHDF/Steps/NumberOfParts", 0, 2, ivals)
CALL OK(ALL(ivals .EQ. 1), "Steps/NumberOfParts")
CALL h5%ReadSlab("/VTKHDF/Steps/PointOffsets", 0, 2, ivals)
CALL OK(ALL(ivals .EQ. 0), "Steps/PointOffsets")
CALL h5%ReadSlab("/VTKHDF/Steps/CellOffsets", 0, 2, ivals2)
CALL OK(ALL(SHAPE(ivals2) .EQ. [1, 2]) .AND. ALL(ivals2 .EQ. 0), &
        "Steps/CellOffsets")

! u is written in two steps, one after the other
CALL h5%ReadSlab("/VTKHDF/Steps/PointDataOffsets/u", 0, 2, ivals)
CALL OK(ALL(ivals .EQ. [0, nPoints]), "Steps/PointDataOffsets/u")
CALL h5%ReadSlab("/VTKHDF/PointData/u", 0, 2 * nPoints, rvals1)
CALL OK(MAXVAL(ABS(rvals1(1:nPoints) - 1.0_DFP)) .LT. tol, &
        "PointData/u: step 1")
CALL OK(MAXVAL(ABS(rvals1(nPoints + 1:) - 3.0_DFP)) .LT. tol, &
        "PointData/u: step 3")

CALL h5%DEALLOCATE()
CALL amesh%DEALLOCATE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
