!> author: Shion Shimizu
! date: 2026-10-24
! update:
!   - 2026-10-27: grid from GetVtkUnstructuredGridData; the datasets are
!     checked with the methods of HDF5File_
! summary: Write a static grid with point, cell and field data
!
! The grid of the mesh very_small_quad4_mesh.h5 (unit square, four
! Quadrangle4 elements, nine nodes) is written in a non-transient file
! together with point data (coordinates), cell data (cell number) and
! field data. The file is then opened as an HDF5File_ and the datasets
! of the VTKHDF layout are read back with ReadSlab.

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

  CHARACTER(*), PARAMETER :: meshname = &
    "./meshdata/very_small_quad4_mesh.h5"
  CHARACTER(*), PARAMETER :: filename = &
    "WriteUnstructuredGrid_test_1.vtkhdf"
  REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
  TYPE(HDF5File_) :: meshfile, h5
  TYPE(FEMesh_) :: amesh
  TYPE(VtkHDF5File_) :: obj
  REAL(DFP), ALLOCATABLE :: points(:, :), cellIds(:), rvals1(:), &
    rvals2(:, :)
  INTEGER(I4B), ALLOCATABLE :: conn(:), offsets(:), ivals(:)
  INTEGER(INT8), ALLOCATABLE :: types(:), tvals(:)
  INTEGER(I4B) :: nPoints, nCells, nIds, ii

  CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)

  CALL meshfile%Initiate(filename=meshname, mode=TypeFileOpt%READ)
  CALL meshfile%OPEN()
  CALL amesh%Initiate(hdf5=meshfile, DIM=2)

! grid of the mesh
  CALL amesh%GetVtkUnstructuredGridData(points=points, &
                                        connectivity=conn, &
                                        offsets=offsets, types=types)
  nPoints = SIZE(points, 2)
  nCells = SIZE(types)
  nIds = SIZE(conn)
  cellIds = [(REAL(ii, DFP), ii=1, nCells)]

! write a static grid with point, cell and field data
  CALL obj%InitiateVTKHDF5(filename=filename, mode=TypeFileOpt%new, &
                           vtkType="UnstructuredGrid")
  CALL obj%WriteUnstructuredGrid(points=points, connectivity=conn, &
                                 offsets=offsets, types=types)
  CALL OK(obj%GetTotalPoints() .EQ. nPoints, "GetTotalPoints")
  CALL OK(obj%GetTotalCells() .EQ. nCells, "GetTotalCells")
  CALL OK(obj%IsOutputStep(), "IsOutputStep: not transient")
  CALL obj%WritePointData(NAME="x", VALUE=points)
  CALL obj%WritePointData(NAME="xcoord", VALUE=points(1, :))
  CALL obj%WriteCellData(NAME="id", vals=cellIds)
  CALL obj%WriteFieldData(NAME="energy", vals=[1.5_DFP, 2.5_DFP])
  CALL obj%DEALLOCATE()

! read the datasets back
  CALL h5%Initiate(filename=filename, mode="READ")
  CALL h5%OPEN()

  CALL OK(.NOT. h5%PathExists("/VTKHDF/Steps"), "no Steps group")

  CALL h5%ReadSlab("/VTKHDF/NumberOfPoints", 0, 1, ivals)
  CALL OK(ivals(1) .EQ. nPoints, "NumberOfPoints")
  CALL h5%ReadSlab("/VTKHDF/NumberOfCells", 0, 1, ivals)
  CALL OK(ivals(1) .EQ. nCells, "NumberOfCells")
  CALL h5%ReadSlab("/VTKHDF/NumberOfConnectivityIds", 0, 1, ivals)
  CALL OK(ivals(1) .EQ. nIds, "NumberOfConnectivityIds")

  CALL h5%ReadSlab("/VTKHDF/Points", 0, nPoints, rvals2)
  CALL OK(ALL(SHAPE(rvals2) .EQ. [3, nPoints]), "Points: shape")
  CALL OK(MAXVAL(ABS(rvals2 - points)) .LT. tol, "Points: values")
  CALL h5%ReadSlab("/VTKHDF/Connectivity", 0, nIds, ivals)
  CALL OK(ALL(ivals .EQ. conn), "Connectivity")
  CALL h5%ReadSlab("/VTKHDF/Offsets", 0, nCells + 1, ivals)
  CALL OK(ALL(ivals .EQ. offsets), "Offsets")
  CALL h5%ReadSlab("/VTKHDF/Types", 0, nCells, tvals)
  CALL OK(ALL(tvals .EQ. 9_INT8), "Types: VTK_QUAD")

  CALL h5%ReadSlab("/VTKHDF/PointData/x", 0, nPoints, rvals2)
  CALL OK(MAXVAL(ABS(rvals2 - points)) .LT. tol, "PointData rank-2")
  CALL h5%ReadSlab("/VTKHDF/PointData/xcoord", 0, nPoints, rvals1)
  CALL OK(MAXVAL(ABS(rvals1 - points(1, :))) .LT. tol, &
          "PointData rank-1")
  CALL h5%ReadSlab("/VTKHDF/CellData/id", 0, nCells, rvals1)
  CALL OK(MAXVAL(ABS(rvals1 - cellIds)) .LT. tol, "CellData")
  CALL h5%ReadSlab("/VTKHDF/FieldData/energy", 0, 1, rvals2)
  CALL OK(ALL(SHAPE(rvals2) .EQ. [2, 1]), "FieldData: shape")
  CALL OK(MAXVAL(ABS(rvals2(:, 1) - [1.5_DFP, 2.5_DFP])) .LT. tol, &
          "FieldData: values")

  CALL h5%DEALLOCATE()
  CALL amesh%DEALLOCATE()
  CALL meshfile%DEALLOCATE()
END PROGRAM main
