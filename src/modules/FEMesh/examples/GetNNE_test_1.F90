!> author: Vikas Sharma, Ph. D.
! summary: GetNNE_test_1

PROGRAM main
USE easifemBase
USE easifemClasses
IMPLICIT NONE
TYPE(FEMesh_) :: obj
TYPE(HDF5File_) :: meshfile
CHARACTER(LEN=*), PARAMETER :: filename = &
  & "../../Mesh/examples/meshdata/small_mesh_two_region.h5"
CALL meshfile%Initiate(FileName=filename, MODE="READ")
CALL meshfile%OPEN()
CALL obj%Initiate(hdf5=meshfile, dim=2)
CALL OK(obj%GetNNE(obj%GetGlobalElemNumber(1)) .EQ. 4_I4B, "NNE: ")
CALL obj%DEALLOCATE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
