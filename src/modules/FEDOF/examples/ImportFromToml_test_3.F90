!> author: Vikas Sharma, Ph. D.
! date: 2025-06-01
! summary:  Initiate FEDOF object by import from toml

PROGRAM main
  USE FEDOF_Class, ONLY: FEDOF_
  USE FEDomain_Class, ONLY: FEDomain_
  USE AbstractMesh_Class, ONLY: AbstractMesh_
  USE HDF5File_Class, ONLY: HDF5File_

  USE GlobalData, ONLY: I4B
  USE Test_Method, ONLY: is
  USE Test_Method, ONLY: ok
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
  IMPLICIT NONE

  TYPE(FEDOF_) :: obj
  TYPE(FEDomain_) :: dom
  CLASS(AbstractMesh_), POINTER :: meshptr => NULL()
  CHARACTER(*), PARAMETER :: filename = &
    "./meshdata/very_small_quad4_mesh.h5", &
    toml_filename = "./toml/ImportFromToml_test_1.toml"
  TYPE(HDF5File_) :: meshfile
  INTEGER(I4B) :: found, want

  CALL e%setQuietMode(EXCEPTION_INFORMATION, .TRUE.)
  CALL e%setQuietMode(EXCEPTION_DEBUG, .TRUE.)
  CALL meshfile%Initiate(filename, mode="READ")
  CALL meshfile%OPEN()
  CALL dom%Initiate(meshfile, '')

  meshptr => dom%GetMeshPointer()

  CALL obj%ImportFromToml(tomlName="test1", filename=toml_filename, dom=dom)
!CALL fedof%Display("FEDOF:")
  found = obj%GetTotalDOF()
  want = meshptr%GetTotalNodes()
  CALL IS(found, want, "Total DOF (order=1): ")

  CALL obj%ImportFromToml(tomlName="test2", filename=toml_filename, dom=dom)
  found = obj%GetTotalDOF()
  want = meshptr%GetTotalNodes() + meshptr%GetTotalFaces() + &
    1 * meshptr%GetTotalCells()
  CALL IS(found, want, "Total DOF (order=2): ")

  CALL obj%ImportFromToml(tomlName="test3", filename=toml_filename, dom=dom)
  found = obj%GetTotalDOF()
  want = meshptr%GetTotalNodes() + 2 * meshptr%GetTotalFaces() + &
    4 * meshptr%GetTotalCells()
  CALL IS(found, want, "Total DOF (order=3): ")

  CALL obj%ImportFromToml(tomlName="test4", filename=toml_filename, dom=dom)
  found = obj%GetTotalDOF()
  want = meshptr%GetTotalNodes() + 3 * meshptr%GetTotalFaces() + &
    9 * meshptr%GetTotalCells()
  CALL OK(found == want, "Total DOF (order=4): ")

  CALL obj%ImportFromToml(tomlName="test5", filename=toml_filename, dom=dom)
  !CALL fedof%Display("FEDOF:")
  found = obj%GetTotalDOF()
  want = meshptr%GetTotalNodes()
  CALL IS(found, want, "Total DOF (order=1): ")

  CALL obj%ImportFromToml(tomlName="test6", filename=toml_filename, dom=dom)
  found = obj%GetTotalDOF()
  want = meshptr%GetTotalNodes() + meshptr%GetTotalFaces() + &
    1 * meshptr%GetTotalCells()
  CALL IS(found, want, "Total DOF (order=2): ")

  CALL obj%ImportFromToml(tomlName="test7", filename=toml_filename, dom=dom)
  found = obj%GetTotalDOF()
  want = meshptr%GetTotalNodes() + 2 * meshptr%GetTotalFaces() + &
    4 * meshptr%GetTotalCells()
  CALL IS(found, want, "Total DOF (order=3): ")

  CALL obj%ImportFromToml(tomlName="test8", filename=toml_filename, dom=dom)
  found = obj%GetTotalDOF()
  want = meshptr%GetTotalNodes() + 3 * meshptr%GetTotalFaces() + &
    9 * meshptr%GetTotalCells()
  CALL OK(found == want, "Total DOF (order=4): ")

!CALL dom%Display("domain:")
  CALL dom%DEALLOCATE()
  CALL meshfile%DEALLOCATE()

END PROGRAM main
