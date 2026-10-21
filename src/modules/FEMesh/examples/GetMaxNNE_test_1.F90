!> author: Vikas Sharma, Ph. D.
! summary: This example tests the `GetMaxNNE` method, which returns the total
!          number of nodes in an element.
!
! The element number should be global element number.

PROGRAM main
  USE easifemBase
  USE easifemClasses
  IMPLICIT NONE
  TYPE(FEMesh_) :: obj
  TYPE(HDF5File_) :: meshfile
  CHARACTER(LEN = *), PARAMETER :: filename= &
    & "../../Mesh/examples/meshdata/small_mesh_two_region.h5"
  CALL meshfile%Initiate(FileName=filename, MODE="READ")
  CALL meshfile%Open()
  CALL obj%Initiate(hdf5=meshfile, dim=2)
  CALL OK(obj%GetMaxNNE() .EQ. 4_I4B, "NNE: ")
  CALL obj%Deallocate()
  CALL meshfile%Deallocate()
END PROGRAM main
