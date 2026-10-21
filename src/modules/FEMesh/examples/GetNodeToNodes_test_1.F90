!> author: Vikas Sharma, Ph. D.
! summary: This example shows how to get the node to node connectivity data in
!          the mesh.

PROGRAM main
USE easifemBase
USE easifemClasses

IMPLICIT NONE

TYPE(FEMesh_) :: obj
TYPE(HDF5File_) :: meshfile
  CHARACTER(*), PARAMETER :: filename="../../Mesh/examples/meshdata/small_mesh.h5"

CALL e%SetQuietMode(.TRUE.)
CALL meshfile%Initiate(FileName=filename, MODE="READ")

!Open the mesh file
CALL meshfile%OPEN()
CALL obj%SetShowTime(.TRUE.)
!Initiate an instance of `Mesh_`
CALL obj%Initiate(hdf5=meshfile, group="/surfaceEntities_1")
! Initiate node to nodes
CALL obj%InitiateNodeToNodes()
!Display the content of mesh.
CALL obj%DisplayMeshInfo(filename)

BLOCK
  INTEGER(I4B), ALLOCATABLE :: ans(:), exact_ans(:)
  ans = obj%GetNodeToNodes(globalNode=10, includeSelf=.TRUE.)
  ans = SORT(ans)
  exact_ans = [1, 5, 8, 9, 10, 11, 12]
  CALL OK(ALL(ans .EQ. exact_ans), "GetNodeToNodes: ")
END BLOCK

BLOCK
  INTEGER(I4B), ALLOCATABLE :: ans(:), exact_ans(:)
  ans = obj%GetNodeToNodes(globalNode=10, includeSelf=.FALSE.)
  ans = SORT(ans)
  exact_ans = [1, 5, 8, 9, 11, 12]
  CALL OK(ALL(ans .EQ. exact_ans), "GetNodeToNodes: ")
END BLOCK

BLOCK
  INTEGER(I4B), ALLOCATABLE :: ans(:), exact_ans(:)
  ans = obj%GetNodeToNodes(globalNode=5, includeSelf=.TRUE.)
  ans = SORT(ans)
  exact_ans = [1, 2, 5, 10, 12]
  CALL OK(ALL(ans .EQ. exact_ans), "GetNodeToNodes: ")
END BLOCK

BLOCK
  INTEGER(I4B), ALLOCATABLE :: ans(:), exact_ans(:)
  ans = obj%GetNodeToNodes(globalNode=5, includeSelf=.FALSE.)
  ans = SORT(ans)
  exact_ans = [1, 2, 10, 12]
  CALL OK(ALL(ans .EQ. exact_ans), "GetNodeToNodes: ")
END BLOCK

BLOCK
  INTEGER(I4B), ALLOCATABLE :: ans(:), exact_ans(:)
  ans = obj%GetNodeToNodes(globalNode=5, includeSelf=.FALSE., islocal=.TRUE.)
  ans = SORT(ans)
  exact_ans = [1, 2, 10, 12]
  CALL OK(ALL(ans .EQ. exact_ans), "GetNodeToNodes: ")
END BLOCK

CALL obj%DEALLOCATE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
