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
USE VTKHDFFile_Class, ONLY: VTKHDFFile_
IMPLICIT NONE
CHARACTER(*), PARAMETER :: tomlFileName = "VTKHDFFile_test.toml", &
                           filename = "VTKHDFFile_test_4.vtkhdf"
TYPE(FEDomain_) :: dom
TYPE(FEDOF_) :: fedof, geofedof
TYPE(ScalarField_) :: u
TYPE(VTKHDFFile_) :: obj
REAL(DFP), ALLOCATABLE :: vals(:, :)
INTEGER(I4B), ALLOCATABLE :: conn(:), offsets(:)
INTEGER(INT8), ALLOCATABLE :: types(:)
REAL(DFP), ALLOCATABLE :: points(:, :)
CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
CALL dom%ImportFromToml(fileName=tomlFileName, tomlName="domain")
CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
                      fileName=tomlFileName, tomlName="u")
CALL u%Set(VALUE=2.0_DFP)
CALL obj%InitiateVTKHDF(filename=filename, mode="NEW", &
                        isTransient=math%no)
CALL obj%WriteMesh(fedof=fedof, geofedof=geofedof)
CALL obj%WritePointData(name="u", field=u)
CALL obj%DEALLOCATE()
CALL obj%InitiateVTKHDF(filename=filename, mode="READ")
CALL obj%ReadMesh(points=points, connectivity=conn, offsets=offsets, &
                  types=types)
CALL OK(ALL(types .EQ. 22_INT8), "WriteMesh2: VTK_QUADRATIC_TRIANGLE")
CALL obj%ReadPointData(name="u", vals=vals)
CALL OK(MAXVAL(ABS(vals - 2.0_DFP)) .LT. 1.0E-10_DFP, "WriteMesh2: u")
CALL obj%DEALLOCATE()
CALL u%DEALLOCATE()
CALL fedof%DEALLOCATE()
CALL geofedof%DEALLOCATE()
CALL dom%DEALLOCATE()
END PROGRAM main
