!> author: Shion Shimizu
! date: 2026-10-24
! summary: VTK Lagrange grid from a FEDOF

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
  USE VTKMeshUtility, ONLY: VTKGetLagrangeGrid
  IMPLICIT NONE
  CHARACTER(*), PARAMETER :: tomlFileName = "VtkHDF5File_test.toml"
  TYPE(FEDomain_) :: dom
  TYPE(FEDOF_) :: fedof, geofedof
  TYPE(ScalarField_) :: u
  REAL(DFP), ALLOCATABLE :: points(:, :)
  INTEGER(I4B), ALLOCATABLE :: connectivity(:), offsets(:)
  INTEGER(INT8), ALLOCATABLE :: types(:)
  CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
  CALL dom%ImportFromToml(fileName=tomlFileName, tomlName="domain")
  CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                        fileName=tomlFileName, tomlName="u")
  CALL VTKGetLagrangeGrid(fedof=fedof, geofedof=geofedof, &
                          points=points, connectivity=connectivity, &
                          offsets=offsets, types=types)
! order 2 triangle: VTK_QUADRATIC_TRIANGLE with 6 points
  CALL OK(ALL(types .EQ. 22_INT8), "VTK_QUADRATIC_TRIANGLE")
  CALL OK(SIZE(points, 2) .EQ. 6 * SIZE(types), "nPoints")
  CALL u%DEALLOCATE()
  CALL fedof%DEALLOCATE()
  CALL geofedof%DEALLOCATE()
  CALL dom%DEALLOCATE()
END PROGRAM main
