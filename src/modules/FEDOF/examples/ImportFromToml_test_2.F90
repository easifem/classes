!> author: Vikas Sharma, Ph. D.
! date:
! summary: Initiate fedof with H1 and Hierarchical bases, order is a
!   vector.

PROGRAM main
  USE FEDOF_Class, ONLY: FEDOF_
  USE FEDomain_Class, ONLY: FEDomain_
  USE AbstractMesh_Class, ONLY: AbstractMesh_
  USE HDF5File_Class, ONLY: HDF5File_

  USE GlobalData, ONLY: I4B
  USE GlobalData, ONLY: LGT
  USE Test_Method, ONLY: is
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  IMPLICIT NONE

  TYPE(FEDOF_) :: fedof
  TYPE(FEDomain_) :: dom
  CLASS(AbstractMesh_), POINTER :: meshptr => NULL()
  CHARACTER(*), PARAMETER :: filename = &
    "./meshdata/small_tri3_mesh.h5", &
    toml_filename = "./toml/ImportFromToml_test_2.toml"
  TYPE(HDF5File_) :: meshfile
  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: found, want

  CALL e%setQuietMode(EXCEPTION_INFORMATION, .TRUE.)
  CALL meshfile%Initiate(filename, mode="READ")
  CALL meshfile%OPEN()
  CALL dom%Initiate(meshfile, '')

  meshptr => dom%GetMeshPointer()

  CALL fedof%ImportFromToml(tomlName="test1", &
                            filename=toml_filename, dom=dom)

  found = fedof%GetTotalDOF()
  want = 39
  isok = found == want
  CALL IS(found, want, "Total DOF ")

  CALL dom%DEALLOCATE()
  CALL meshfile%DEALLOCATE()

END PROGRAM main
