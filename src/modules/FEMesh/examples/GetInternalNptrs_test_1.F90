!> author: Vikas Sharma, Ph. D.
! summary: GetInternalNptrs_test_1

PROGRAM main
USE easifemBase
USE easifemClasses
IMPLICIT NONE
TYPE(FEMesh_) :: obj
TYPE(HDF5File_) :: meshfile
CHARACTER(LEN=*), PARAMETER :: filename = &
  & "../../Mesh/examples/meshdata/small_tri3_mesh_two_region.h5"
CALL meshfile%Initiate(FileName=filename, MODE="READ")
CALL meshfile%OPEN()
CALL obj%Initiate(hdf5=meshfile, dim=2)
CALL Display("flag 1")
BLOCK
  INTEGER(I4B), ALLOCATABLE :: nptrs(:)
  nptrs = obj%GetInternalNptrs()
  CALL Display(nptrs, "Internal Nptrs: ", full=.TRUE.)
END BLOCK
CALL Display("flag 2")
CALL obj%DEALLOCATE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
