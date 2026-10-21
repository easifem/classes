!> author: Vikas Sharma, Ph. D.
! summary: This example shows how to initiate an instance of `Mesh_` by
!          reading data from mesh file, which is in `HDF5File_` format.

PROGRAM main
USE easifemBase
USE easifemClasses
IMPLICIT NONE
TYPE(Mesh_) :: obj
TYPE(HDF5File_) :: meshfile
CHARACTER(LEN=*), PARAMETER :: filename = "./mesh.h5"
INTEGER(I4B), ALLOCATABLE :: edge_connectivity(:, :)
! Initiate and open the mesh file which is in `HDF5File_` format.
CALL meshfile%Initiate(FileName=filename, MODE="READ")
! Open the mesh file
CALL meshfile%OPEN()
! Initiate an instance of `Mesh_`
CALL obj%Initiate(hdf5=meshfile, group="/surfaceEntities_1")
CALL Reallocate(edge_connectivity, obj%GetMaxNNE(), obj%GetTotalElements())
CALL obj%GetNodeConnectivity(edge_connectivity)
CALL Display(edge_connectivity, "edge_connectivity")
! Display the content of mesh.
CALL obj%DEALLOCATE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
