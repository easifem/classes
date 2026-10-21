!> author: Vikas Sharma, Ph. D.
! summary: InitiateNodeToElements_test_4

PROGRAM main
  USE easifemBase
  USE easifemClasses
  IMPLICIT NONE
  TYPE(FEMesh_) :: obj
  TYPE(HDF5File_) :: meshfile
  CHARACTER(*), PARAMETER :: filename="../../Mesh/examples/meshdata/small_mesh_two_region.h5"
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
