!> author: Vikas Sharma, Ph. D.
! date: 2025-06-01
! summary:  Initiate FEDOF object by import from toml

PROGRAM main
USE FEDOF_Class
USE FEDomain_Class
USE AbstractMesh_Class
USE HDF5File_Class
USE Display_Method
USE GlobalData
USE Test_Method
USE ExceptionHandler_Class, ONLY: e, EXCEPTION_INFORMATION

IMPLICIT NONE

TYPE(FEDOF_) :: obj
TYPE(FEDomain_) :: dom
CLASS(AbstractMesh_), POINTER :: meshptr => NULL()
CHARACTER(*), PARAMETER :: filename = &
                     "../../FEMesh/examples/meshdata/very_small_quad4_mesh.h5"
TYPE(HDF5File_) :: meshfile
INTEGER(I4B) :: found, want

CALL e%setQuietMode(EXCEPTION_INFORMATION, .TRUE.)
CALL meshfile%Initiate(filename, mode="READ")
CALL meshfile%OPEN()
CALL dom%Initiate(meshfile, '')

meshptr => dom%GetMeshPointer()

CALL obj%ImportFromToml(tomlName="test1", &
                  filename="./toml/_ImportFromToml_test_1.toml", mesh=meshptr)
!CALL fedof%Display("FEDOF:")
found = obj%GetTotalDOF()
want = meshptr%GetTotalNodes()
CALL IS(found, want, "Total DOF (order=1): ")

CALL obj%ImportFromToml(tomlName="test2", &
                  filename="./toml/_ImportFromToml_test_1.toml", mesh=meshptr)
found = obj%GetTotalDOF()
want = meshptr%GetTotalNodes() + meshptr%GetTotalFaces() + 0*meshptr%GetTotalCells()
CALL IS(found, want, "Total DOF (order=2): ")

CALL obj%ImportFromToml(tomlName="test3", &
                  filename="./toml/_ImportFromToml_test_1.toml", mesh=meshptr)
found = obj%GetTotalDOF()
want = meshptr%GetTotalNodes() + 2*meshptr%GetTotalFaces() + 1*meshptr%GetTotalCells()
CALL IS(found, want, "Total DOF (order=3): ")

CALL obj%ImportFromToml(tomlName="test4", &
                  filename="./toml/_ImportFromToml_test_1.toml", mesh=meshptr)
found = obj%GetTotalDOF()
want = meshptr%GetTotalNodes() + 3*meshptr%GetTotalFaces() + 3*meshptr%GetTotalCells()
CALL OK(found == want, "Total DOF (order=4): ")

CALL obj%ImportFromToml(tomlName="test5", &
                  filename="./toml/_ImportFromToml_test_1.toml", mesh=meshptr)
!CALL fedof%Display("FEDOF:")
found = obj%GetTotalDOF()
want = meshptr%GetTotalNodes()
CALL IS(found, want, "Total DOF (order=1): ")

CALL obj%ImportFromToml(tomlName="test6", &
                  filename="./toml/_ImportFromToml_test_1.toml", mesh=meshptr)
found = obj%GetTotalDOF()
want = meshptr%GetTotalNodes() + meshptr%GetTotalFaces() + 0*meshptr%GetTotalCells()
CALL IS(found, want, "Total DOF (order=2): ")

CALL obj%ImportFromToml(tomlName="test7", &
                  filename="./toml/_ImportFromToml_test_1.toml", mesh=meshptr)
found = obj%GetTotalDOF()
want = meshptr%GetTotalNodes() + 2*meshptr%GetTotalFaces() + 1*meshptr%GetTotalCells()
CALL IS(found, want, "Total DOF (order=3): ")

CALL obj%ImportFromToml(tomlName="test8", &
                  filename="./toml/_ImportFromToml_test_1.toml", mesh=meshptr)
found = obj%GetTotalDOF()
want = meshptr%GetTotalNodes() + 3*meshptr%GetTotalFaces() + 3*meshptr%GetTotalCells()
CALL OK(found == want, "Total DOF (order=4): ")

!CALL dom%Display("domain:")
CALL dom%DEALLOCATE()
CALL meshfile%DEALLOCATE()

END PROGRAM main
