!> author: Vikas Sharma, Ph. D.
! summary: Testing `GetMaxNNE` for old style mesh.

PROGRAM main
  USE easifemBase
  USE easifemClasses
  IMPLICIT NONE
  TYPE(FEMesh_) :: obj
  TYPE(HDF5File_) :: meshfile
  CHARACTER(LEN = *), PARAMETER :: filename= &
    & "../../Mesh/examples/meshdata/small_mesh.h5"
  CALL meshfile%Initiate(FileName=filename, MODE="READ")
  CALL meshfile%Open()
  CALL obj%Initiate(hdf5=meshfile, group="/surfaceEntities_1")
  CALL OK(obj%GetMaxNNE() .EQ. 3_I4B, "GetMaxNNE: ")
  CALL obj%Deallocate()
  CALL meshfile%Deallocate()
END PROGRAM main
