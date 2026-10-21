!> author: Vikas Sharma, Ph. D.
! summary: GetNodeToNodes_test_4

PROGRAM main
USE easifemBase
USE easifemClasses
IMPLICIT NONE
TYPE(FEMesh_) :: obj
TYPE(HDF5File_) :: meshfile
CHARACTER(*), PARAMETER :: filename = &
                       "../../Mesh/examples/meshdata/small_mesh_two_region.h5"
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

BLOCK
  INTEGER(I4B), ALLOCATABLE :: ans(:), exact_ans(:)
  INTEGER(I4B), PARAMETER :: globalNode(1) = [2]
  INTEGER(I4B) :: tsize

  CALL Reallocate(ans, 100)
  CALL obj%GetNodeToNodes_(globalNode=globalNode, includeSelf=.TRUE., &
                           ans=ans, tsize=tsize)
  ans(1:tsize) = SORT(ans(1:tsize))
  exact_ans = [2, 5, 7, 8, 9, 10]

  CALL OK(tsize .EQ. SIZE(exact_ans), "GetNodeToNodes: ")
  CALL OK(ALL(ans(1:tsize) .EQ. exact_ans), "GetNodeToNodes: ")

END BLOCK

BLOCK
  INTEGER(I4B), ALLOCATABLE :: ans(:), exact_ans(:)
  INTEGER(I4B), PARAMETER :: globalNode(1) = [2]
  INTEGER(I4B) :: tsize

  CALL Reallocate(ans, 100)
  CALL obj%GetNodeToNodes_(globalNode=globalNode, includeSelf=.FALSE., &
                           ans=ans, tsize=tsize)
  ans(1:tsize) = SORT(ans(1:tsize))
  exact_ans = [5, 7, 8, 9, 10]

  CALL OK(tsize .EQ. SIZE(exact_ans), "GetNodeToNodes: ")
  CALL OK(ALL(ans(1:tsize) .EQ. exact_ans), "GetNodeToNodes: ")
END BLOCK

!! this test gets nodetonodes from mesh for multiple nodes
!! includeself is TRUE.
BLOCK
  INTEGER(I4B), ALLOCATABLE :: ans(:), exact_ans(:)
  INTEGER(I4B), PARAMETER :: globalNode(2) = [1, 2]
  INTEGER(I4B) :: tsize

  CALL Reallocate(ans, 100)
  CALL obj%GetNodeToNodes_(globalNode=globalNode, includeSelf=.TRUE., &
                           ans=ans, tsize=tsize)
  ans(1:tsize) = SORT(ans(1:tsize))
  exact_ans = [1, 2, 4, 5, 7, 8, 9, 10]

  CALL OK(tsize .EQ. SIZE(exact_ans), "GetNodeToNodes: ")
  CALL OK(ALL(ans(1:tsize) .EQ. exact_ans), "GetNodeToNodes: ")

END BLOCK

!! this test gets nodetonodes from mesh for multiple nodes
!! includeself is false.
BLOCK
  INTEGER(I4B), ALLOCATABLE :: ans(:), exact_ans(:)
  INTEGER(I4B), PARAMETER :: globalNode(2) = [1, 2]
  INTEGER(I4B) :: tsize

  CALL Reallocate(ans, 100)
  CALL obj%GetNodeToNodes_(globalNode=globalNode, includeSelf=.FALSE., &
                           ans=ans, tsize=tsize)
  ans(1:tsize) = SORT(ans(1:tsize))
  exact_ans = [4, 5, 7, 8, 9, 10]

  CALL OK(tsize .EQ. SIZE(exact_ans), "GetNodeToNodes: ")
  CALL OK(ALL(ans(1:tsize) .EQ. exact_ans), "GetNodeToNodes: ")

END BLOCK

CALL obj%DEALLOCATE()
CALL meshfile%DEALLOCATE()
END PROGRAM main
