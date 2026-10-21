!> author: Vikas Sharma, Ph. D.
! summary: GetNodeToElements_test_2

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
CALL meshfile%OPEN()
CALL obj%SetShowTime(.TRUE.)
!Initiate an instance of `Mesh_`
CALL obj%Initiate(hdf5=meshfile, dim=2)
! Initiate dynamic data structure
CALL obj%InitiateNodeToElements()
!Display the content of mesh.
CALL obj%DisplayMeshInfo(filename)
! Display the mesh data
BLOCK
  INTEGER(I4B), ALLOCATABLE :: ans(:), exact_ans(:)
  INTEGER(I4B), PARAMETER :: globalNode = 2
  ans = obj%GetNodeToElements(globalNode)
  ans = sort(ans)
  exact_ans = [19, 20]
  CALL OK(ALL(exact_ans .EQ. ans), "GetNodeToElements: ")
END BLOCK
BLOCK
  INTEGER(I4B), ALLOCATABLE :: ans(:), exact_ans(:)
  INTEGER(I4B), PARAMETER :: globalNode = 1
  ans = obj%GetNodeToElements(globalNode)
  ans = sort(ans)
  exact_ans = [18]
  CALL OK(ALL(exact_ans .EQ. ans), "GetNodeToElements: ")
END BLOCK
BLOCK
  INTEGER(I4B), ALLOCATABLE :: ans(:), exact_ans(:)
  INTEGER(I4B), PARAMETER :: globalNode(2) = [1, 2]
  ans = obj%GetNodeToElements(globalNode)
  ans = sort(ans)
  exact_ans = [18, 19, 20]
  CALL OK(ALL(exact_ans .EQ. ans), "GetNodeToElements: ")
END BLOCK
CALL obj%DEALLOCATE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
