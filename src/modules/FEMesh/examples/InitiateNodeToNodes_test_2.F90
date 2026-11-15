!> author: Vikas Sharma, Ph. D.
! date: 2025-06-03
! summary:  Testing of InitiateNodeToNodes
!

PROGRAM main
USE FEMesh_Class
USE HDF5File_Class
USE GlobalData
USE ExceptionHandler_Class, ONLY: e

IMPLICIT NONE

TYPE(FEMesh_) :: obj
TYPE(HDF5File_) :: meshfile
CHARACTER(*), PARAMETER :: filename = "./meshdata/big_tri3_mesh.h5"

CALL e%SetQuietMode(.TRUE.)

CALL meshfile%Initiate(FileName=filename, MODE="READ")

!Open the mesh file
CALL meshfile%OPEN()
CALL obj%SetShowTime(.TRUE.)

!Initiate an instance of `Mesh_`
CALL obj%Initiate(hdf5=meshfile, dim=2)

! Initiate node to nodes
CALL obj%InitiateNodeToNodes()

!Display the content of mesh.
CALL obj%DisplayMeshInfo(filename)

! Display the mesh data
CALL obj%DisplayNodeData("Node data of "//filename)

CALL obj%DEALLOCATE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
