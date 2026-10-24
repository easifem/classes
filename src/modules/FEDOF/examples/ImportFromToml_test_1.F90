!> author: Vikas Sharma, Ph. D.
! date: 2025-06-01
! summary:  Initiate FEDOF object by import from toml

PROGRAM main
  USE FEDOF_Class, ONLY: FEDOF_
  USE FEDomain_Class, ONLY: FEDomain_
  USE AbstractMesh_Class, ONLY: AbstractMesh_
  USE HDF5File_Class, ONLY: HDF5File_
  USE Display_Method, ONLY: Display
  USE GlobalData, ONLY: DFP
  USE GlobalData, ONLY: I4B
  USE GlobalData, ONLY: LGT
  USE Test_Method, ONLY: OK
  USE Test_Method, ONLY: IS
  USE ExceptionHandler_Class, ONLY: e
  USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
  USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG

  IMPLICIT NONE

  TYPE(FEDOF_) :: obj
  TYPE(FEDomain_) :: dom
  CLASS(AbstractMesh_), POINTER :: meshptr => NULL()
  CHARACTER(*), PARAMETER :: filename = "./meshdata/small_tri3_mesh.h5", &
    tomlFileName = "./toml/ImportFromToml_test_1.toml"
  TYPE(HDF5File_) :: meshfile
  INTEGER(I4B) :: found, want

  CALL e%setQuietMode(EXCEPTION_INFORMATION, .TRUE.)
  CALL e%setQuietMode(EXCEPTION_DEBUG, .TRUE.)

  CALL meshfile%Initiate(filename, mode="READ")
  CALL meshfile%OPEN()
  CALL dom%Initiate(meshfile, '')

  meshptr => dom%GetMeshPointer()

  CALL obj%ImportFromToml( &
                           tomlName="test1", filename=tomlFileName, dom=dom)

!CALL fedof%Display("FEDOF:")
  found = obj%GetTotalDOF()
  want = meshptr%GetTotalNodes()
  CALL IS(found, want, "Total DOF (order=1): ")

  CALL obj%ImportFromToml( &
                           tomlName="test2", filename=tomlFileName, dom=dom)
  found = obj%GetTotalDOF()
  want = meshptr%GetTotalNodes() + meshptr%GetTotalFaces() + &
    0 * meshptr%GetTotalCells()
  CALL IS(found, want, "Total DOF (order=2): ")

  CALL obj%ImportFromToml( &
                           tomlName="test3", filename=tomlFileName, dom=dom)

  found = obj%GetTotalDOF()
  want = meshptr%GetTotalNodes() + 2 * meshptr%GetTotalFaces() + &
    1 * meshptr%GetTotalCells()
  CALL IS(found, want, "Total DOF (order=3): ")

  CALL obj%ImportFromToml( &
                           tomlName="test4", filename=tomlFileName, dom=dom)

  found = obj%GetTotalDOF()
  want = meshptr%GetTotalNodes() + 3 * meshptr%GetTotalFaces() + &
    3 * meshptr%GetTotalCells()
  CALL OK(found == want, "Total DOF (order=4): ")

  CALL obj%ImportFromToml(tomlName="test5", filename=tomlFileName, &
                          dom=dom)

  found = obj%GetTotalDOF()
  want = meshptr%GetTotalNodes()
  CALL IS(found, want, "Total DOF (order=1): ")

  CALL obj%ImportFromToml(tomlName="test6", filename=tomlFileName, &
                          dom=dom)
  found = obj%GetTotalDOF()
  want = meshptr%GetTotalNodes() + meshptr%GetTotalFaces() + &
    0 * meshptr%GetTotalCells()
  CALL IS(found, want, "Total DOF (order=2): ")

  CALL obj%ImportFromToml(tomlName="test7", filename=tomlFileName, &
                          dom=dom)
  found = obj%GetTotalDOF()
  want = meshptr%GetTotalNodes() + 2 * meshptr%GetTotalFaces() + &
    1 * meshptr%GetTotalCells()
  CALL IS(found, want, "Total DOF (order=3): ")

  CALL obj%ImportFromToml(tomlName="test8", filename=tomlFileName, &
                          dom=dom)
  found = obj%GetTotalDOF()
  want = meshptr%GetTotalNodes() + 3 * meshptr%GetTotalFaces() + &
    3 * meshptr%GetTotalCells()
  CALL OK(found == want, "Total DOF (order=4): ")

!CALL dom%Display("domain:")
  CALL dom%DEALLOCATE()
  CALL meshfile%DEALLOCATE()

END PROGRAM main
