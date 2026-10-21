!> author: Vikas Sharma, Ph. D.
! date: 2025-06-03
! summary: This example demonstrates the use of the `InitiateFaceConnectivity` method in the `FEMesh_` class to set up face connectivity for a 3D mesh.
!
! In this example we show the usage of `InitiateFaceConnectivity` method.

PROGRAM main
  USE FEMesh_Class
  USE HDF5File_Class
  USE GlobalData
  USE Display_Method
  USE ExceptionHandler_Class, only: e

  IMPLICIT NONE
  TYPE(FEMesh_) :: obj
  TYPE(HDF5File_) :: meshfile
  CHARACTER(*), PARAMETER :: filename="./meshdata/small_tri3_mesh.h5"
  CHARACTER(*), PARAMETER :: testname="InitiateFaceConnectivity_test_1"

  CALL e%SetQuietMode(.TRUE.)

  CALL meshfile%Initiate(FileName=filename, MODE="READ")

  !Open the mesh file
  CALL meshfile%Open()
  CALL obj%SetShowTime(.TRUE.)

  !Initiate an instance of `FEMesh_`
  CALL obj%Initiate(hdf5=meshfile, dim=2)

  !Initiate an edge connectivity
  CALL obj%InitiateFaceConnectivity()

  !Display the content of mesh.
  CALL obj%DisplayMeshInfo(filename)

  CALL obj%DisplayElementData("ElementData:")

  CALL obj%Deallocate()
  CALL meshfile%Deallocate()

  CALL Display("End of test: " //testname)
END PROGRAM main
