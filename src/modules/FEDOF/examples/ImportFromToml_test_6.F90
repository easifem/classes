!> author: Vikas Sharma, Ph. D.
! date: 2025-06-01
! summary:  Initiate FEDOF object by import from toml

PROGRAM main
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: I4B
USE HDF5File_Class, ONLY: HDF5File_
USE Test_Method, ONLY: is
USE Test_Method, ONLY: ok
IMPLICIT NONE

TYPE(FEDOF_) :: obj
TYPE(FEDomain_) :: dom
CLASS(AbstractMesh_), POINTER :: mesh => NULL()
CHARACTER(*), PARAMETER :: filename = &
                           "./meshdata/very_small_quad4_mesh.h5", &
                           toml_filename = "./toml/ImportFromToml_test_6.toml"
TYPE(HDF5File_) :: meshfile
INTEGER(I4B) :: found, want

CALL e%setQuietMode(math%yes)
CALL meshfile%Initiate(filename, mode="READ")
CALL meshfile%OPEN()
CALL dom%Initiate(meshfile, '')
mesh => dom%GetMeshPointer()

CALL obj%ImportFromToml(tomlName="test1", filename=toml_filename, dom=dom)
found = obj%GetTotalDOF()
want = 4 * mesh%GetTotalElements()
CALL OK(found .EQ. want, "Total DOF (order=1): ")

CALL obj%ImportFromToml(tomlName="test2", filename=toml_filename, dom=dom)
found = obj%GetTotalDOF()
want = 9 * mesh%GetTotalElements()
CALL OK(found .EQ. want, "Total DOF (order=2): ")

CALL obj%ImportFromToml(tomlName="test3", filename=toml_filename, dom=dom)
found = obj%GetTotalDOF()
want = 16 * mesh%GetTotalElements()
CALL OK(found .EQ. want, "Total DOF (order=3): ")

CALL dom%DEALLOCATE()
CALL meshfile%DEALLOCATE()
CALL obj%DEALLOCATE()
END PROGRAM main
