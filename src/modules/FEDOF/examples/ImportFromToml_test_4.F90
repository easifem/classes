!> author: Vikas Sharma, Ph. D.
! date: 2025-06-01
! summary:  Initiate Broken FEDOF object by import from toml

PROGRAM main
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE BaseType, ONLY: math => TypeMathOpt
USE Display_Method, ONLY: Display
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE HDF5File_Class, ONLY: HDF5File_
USE Test_Method, ONLY: OK
USE Test_Method, ONLY: IS

IMPLICIT NONE

TYPE(FEDOF_) :: obj
TYPE(FEDomain_) :: dom
CLASS(AbstractMesh_), POINTER :: meshptr => NULL()
CHARACTER(*), PARAMETER :: filename = "./meshdata/very_small_quad4_mesh.h5", &
                           tomlFileName = "./toml/ImportFromToml_test_4.toml"
TYPE(HDF5File_) :: meshfile
INTEGER(I4B) :: found, want

CALL e%SetQuietMode(math%yes)
CALL meshfile%Initiate(filename, mode="READ")
CALL meshfile%OPEN()
CALL dom%Initiate(meshfile, '')
meshptr => dom%GetMeshPointer()

CALL obj%ImportFromToml(tomlName= "test1", filename=tomlFileName, dom=dom)
found = obj%GetTotalDOF()
want = 4 * meshptr%GetTotalElements()
CALL OK(found .EQ. want, "Total DOF (order=1): ")

CALL obj%ImportFromToml(tomlName="test2", filename=tomlFileName, dom=dom)
found = obj%GetTotalDOF()
want = 9 * meshptr%GetTotalElements()
CALL OK(found .EQ. want, "Total DOF (order=2): ")

CALL obj%ImportFromToml(tomlName="test3", filename=tomlFileName, dom=dom)
found = obj%GetTotalDOF()
want = 16 * meshptr%GetTotalElements()
CALL OK(found .EQ. want, "Total DOF (order=3): ")

CALL obj%ImportFromToml(tomlName="test4", filename=tomlFileName, dom=dom)
found = obj%GetTotalDOF()
want = 25 * meshptr%GetTotalElements()
CALL OK(found .EQ. want, "Total DOF (order=4): ")

CALL obj%ImportFromToml(tomlName= "test5", filename=tomlFileName, dom=dom)
found = obj%GetTotalDOF()
want = 4 * meshptr%GetTotalElements()
CALL OK(found .EQ. want, "Total DOF (order=1): ")

CALL obj%ImportFromToml(tomlName="test6", filename=tomlFileName, dom=dom)
found = obj%GetTotalDOF()
want = 9 * meshptr%GetTotalElements()
CALL OK(found .EQ. want, "Total DOF (order=2): ")

CALL obj%ImportFromToml(tomlName="test7", filename=tomlFileName, dom=dom)
found = obj%GetTotalDOF()
want = 16 * meshptr%GetTotalElements()
CALL OK(found .EQ. want, "Total DOF (order=3): ")

CALL obj%ImportFromToml(tomlName="test8", filename=tomlFileName, dom=dom)
found = obj%GetTotalDOF()
want = 25 * meshptr%GetTotalElements()
CALL OK(found .EQ. want, "Total DOF (order=4): ")

CALL dom%DEALLOCATE()
CALL meshfile%DEALLOCATE()
CALL obj%DEALLOCATE()
END PROGRAM main
