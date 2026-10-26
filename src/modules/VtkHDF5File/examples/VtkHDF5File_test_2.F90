!> author: Shion Shimizu
! date: 2026-10-24
! summary: Write and read a static linear mesh with data

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FEMesh_Class, ONLY: FEMesh_
  USE GlobalData, ONLY: DFP
  USE GlobalData, ONLY: I4B
  USE GlobalData, ONLY: INT8
  USE HDF5File_Class, ONLY: HDF5File_
  USE Test_Method, ONLY: OK
  USE VtkHDF5File_Class, ONLY: VtkHDF5File_
  IMPLICIT NONE
  CHARACTER(*), PARAMETER :: filename = "VtkHDF5File_test_2.vtkhdf"
  TYPE(HDF5File_) :: meshfile
  TYPE(FEMesh_) :: amesh
  TYPE(VtkHDF5File_) :: obj
  REAL(DFP), ALLOCATABLE :: xij(:, :), points(:, :), vals(:, :), &
    cellIds(:)
  INTEGER(I4B), ALLOCATABLE :: conn(:), offsets(:)
  INTEGER(INT8), ALLOCATABLE :: types(:)
  INTEGER(I4B) :: nPoints, nCells, nrow, ncol, ii
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
  CALL meshfile%Initiate(filename="./meshdata/small_tri3_mesh.h5", &
                         MODE="READ")
  CALL meshfile%OPEN()
  CALL amesh%Initiate(hdf5=meshfile, DIM=2)
  nPoints = amesh%GetTotalNodes()
  ALLOCATE(xij(3, nPoints))
  CALL amesh%GetNodeCoord(nodeCoord=xij, nrow=nrow, ncol=ncol)
! write a static mesh with point and cell data
  CALL obj%InitiateVTKHDF(filename=filename, mode="NEW", &
                          isTransient=math%no)
  CALL obj%WriteMesh(amesh)
  nCells = obj%GetTotalCells()
  cellIds = [(REAL(ii, DFP), ii=1, nCells)]
  CALL obj%WritePointData(NAME="x", vals=xij)
  CALL obj%WriteCellData(NAME="id", vals=cellIds)
  CALL obj%DEALLOCATE()
! read back
  CALL obj%InitiateVTKHDF(filename=filename, mode="READ")
  CALL obj%ReadMesh(points=points, connectivity=conn, offsets=offsets, &
                    types=types)
  CALL OK(SIZE(points, 2) .EQ. nPoints, "ReadMesh: nPoints")
  CALL OK(ALL(types .EQ. 5_INT8), "ReadMesh: VTK_TRIANGLE")
  CALL obj%ReadPointData(NAME="x", vals=vals)
  CALL OK(MAXVAL(ABS(vals - points)) .LT. 1.0E-12_DFP, "ReadPointData")
  CALL obj%ReadCellData(NAME="id", vals=vals)
  CALL OK(MAXVAL(ABS(vals(1, :) - cellIds)) .LT. 1.0E-12_DFP, &
          "ReadCellData")
  CALL obj%DEALLOCATE()
  CALL amesh%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
