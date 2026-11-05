!> author: Vikas Sharma, Ph. D.
! date: 2026-11-02
! summary: ImportFromToml, Variable order
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
USE Test_Method, ONLY: is
IMPLICIT NONE

TYPE(FEDOF_) :: fedof
TYPE(FEDomain_) :: dom
CLASS(AbstractMesh_), POINTER :: meshptr => NULL()
CHARACTER(*), PARAMETER :: filename = "./meshdata/small_tri3_mesh.h5", &
                           toml_filename = &
                           "./toml/ImportFromToml_test_2.toml", &
                           tomlName = "test1"
TYPE(HDF5File_) :: meshfile
LOGICAL(LGT) :: isok
INTEGER(I4B) :: found, want

CALL e%setQuietMode(math%yes)
CALL meshfile%Initiate(filename, mode="READ")
CALL meshfile%OPEN()
CALL dom%Initiate(meshfile, '')

meshptr => dom%GetMeshPointer()
CALL fedof%ImportFromToml(tomlName=tomlName, filename=toml_filename, dom=dom)

found = fedof%GetTotalDOF()
want = 39
isok = found .EQ. want
CALL IS(found, want, "Total DOF ")

CALL dom%DEALLOCATE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
