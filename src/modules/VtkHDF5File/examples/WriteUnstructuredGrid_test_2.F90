!> author: Shion Shimizu
! date: 2026-10-24
! summary: Write a Lagrange grid and a field

PROGRAM main
  USE BaseType, ONLY: math => TypeMathOpt
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE FEDOF_Class, ONLY: FEDOF_
  USE FEDomain_Class, ONLY: FEDomain_
  USE GlobalData, ONLY: DFP
  USE GlobalData, ONLY: I4B
  USE GlobalData, ONLY: INT8
  USE ScalarField_Class, ONLY: ScalarField_
  USE Test_Method, ONLY: OK
  USE VtkHDF5File_Class, ONLY: VtkHDF5File_
  IMPLICIT NONE
  CHARACTER(*), PARAMETER :: tomlFileName = "VtkHDF5File_test.toml", &
    filename = "WriteUnstructuredGrid_test_2.vtkhdf"
  TYPE(FEDomain_) :: dom
  TYPE(FEDOF_) :: fedof, geofedof
  TYPE(ScalarField_) :: u
  TYPE(VtkHDF5File_) :: obj
  REAL(DFP), ALLOCATABLE :: vals(:, :)
  INTEGER(I4B), ALLOCATABLE :: conn(:), offsets(:)
  INTEGER(INT8), ALLOCATABLE :: types(:)
  REAL(DFP), ALLOCATABLE :: points(:, :)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
  CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
  CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                        filename=tomlFileName, tomlName="u")
  CALL u%Set(VALUE=2.0_DFP)
  CALL obj%InitiateVTKHDF5(filename=filename, mode="NEW", &
                           vtkType="UnstructuredGrid")
  CALL obj%WriteMesh(fedof=fedof, geofedof=geofedof)
  CALL obj%WritePointData(NAME="u", field=u)
  CALL obj%DEALLOCATE()
  CALL obj%InitiateVTKHDF5(filename=filename, mode="READ", &
                           vtkType="UnstructuredGrid")
  CALL obj%ReadMesh(points=points, connectivity=conn, offsets=offsets, &
                    types=types)
  CALL OK(ALL(types .EQ. 22_INT8), "WriteMesh2: VTK_QUADRATIC_TRIANGLE")
  CALL obj%ReadPointData(NAME="u", vals=vals)
  CALL OK(MAXVAL(ABS(vals - 2.0_DFP)) .LT. 1.0E-10_DFP, "WriteMesh2: u")
  CALL obj%DEALLOCATE()
  CALL u%DEALLOCATE()
  CALL fedof%DEALLOCATE()
  CALL geofedof%DEALLOCATE()
  CALL dom%DEALLOCATE()
END PROGRAM main
