!> author: Vikas Sharma, Ph. D.
! date: 2026-11-02
! summary: ImportFromToml, Variable order, Broken FEDOF
!
PROGRAM main
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE HDF5File_Class, ONLY: HDF5File_
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(FEDOF_) :: obj
TYPE(FEDomain_) :: dom
CLASS(AbstractMesh_), POINTER :: meshptr => NULL()
CHARACTER(*), PARAMETER :: filename = "./meshdata/very_small_quad4_mesh.h5", &
                           toml_filename = &
                           "./toml/ImportFromToml_test_5.toml", &
                           tomlName = "test1"
TYPE(HDF5File_) :: meshfile
LOGICAL(LGT) :: isok
INTEGER(I4B) :: found, want

CALL e%SetQuietMode(math%yes)
CALL meshfile%Initiate(filename, mode="READ")
CALL meshfile%OPEN()
CALL dom%Initiate(meshfile, '')

meshptr => dom%GetMeshPointer()
CALL obj%ImportFromToml(tomlName=tomlName, filename=toml_filename, dom=dom)

found = obj%GetTotalDOF()
want = 2 * 2 + 3 * 3 + 4 * 4 + 5 * 5
isok = found .EQ. want
CALL OK(found .EQ. want, "Total DOF ")

CALL dom%DEALLOCATE()
CALL meshfile%DEALLOCATE()
CALL obj%DEALLOCATE()
END PROGRAM main
