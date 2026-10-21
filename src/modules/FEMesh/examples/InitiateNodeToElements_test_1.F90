!> author: Vikas Sharma, Ph. D.
! date: 2025-06-03
! summary:  Testing of InitiateNodeToElements
!

PROGRAM main
  USE FEMesh_Class
  USE HDF5File_Class
  USE GlobalData
  use ExceptionHandler_Class, only: e

  IMPLICIT NONE

  TYPE(FEMesh_) :: obj
  TYPE(HDF5File_) :: meshfile
  CHARACTER(*), PARAMETER :: filename="./meshdata/small_tri3_mesh.h5"

  CALL e%SetQuietMode(.TRUE.)
  CALL meshfile%Initiate(FileName=filename, MODE="READ")

  !Open the mesh file
  CALL meshfile%Open()
  CALL obj%SetShowTime(.true.)

  !Initiate an instance of `Mesh_`
  CALL obj%Initiate(hdf5=meshfile, dim=2)

  ! Initiate dynamic data structure
  CALL obj%InitiateNodeToElements()

  !Display the content of mesh.
  CALL obj%DisplayMeshInfo(filename)

  ! Display the mesh data
  CALL obj%DisplayNodeData("Node data of " // filename)

  CALL obj%Deallocate()
  CALL meshfile%Deallocate()
END PROGRAM main
