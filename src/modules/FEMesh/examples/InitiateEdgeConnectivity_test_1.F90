!> author: Vikas Sharma, Ph. D.
! date:  2025-06-03
! summary:  This example demonstrates how to initiate edge connectivity for a mesh object in EasiFEM.
!
! In this example we show the usage of `InitiateEdgeConnectivity` method.

PROGRAM main
  USE FEMesh_Class
  USE HDF5File_Class
  USE GlobalData
  use Display_Method
  USE ExceptionHandler_Class, only: e

  IMPLICIT NONE

  TYPE(FEMesh_) :: obj
  TYPE(HDF5File_) :: meshfile
  CHARACTER(*), PARAMETER :: filename="./meshdata/small_tri3_mesh.h5"
  CHARACTER(*), PARAMETER :: testname="InitiateEdgeConnectivity_test_1"

  CALL e%SetQuietMode(.TRUE.)
  CALL meshfile%Initiate(FileName=filename, MODE="READ")
  !Open the mesh file
  CALL meshfile%Open()
  CALL obj%SetShowTime(.TRUE.)
  !Initiate an instance of `Mesh_`
  CALL obj%Initiate(hdf5=meshfile, dim=2)
  !Initiate an edge connectivity
  CALL obj%InitiateEdgeConnectivity()
  !Display the content of mesh.
  CALL obj%DisplayMeshInfo(filename)
  CALL obj%DisplayElementData("ElementData:")
  CALL obj%Deallocate()
  CALL meshfile%Deallocate()
  CALL Display("End of test: " // trim(testname))
END PROGRAM main
