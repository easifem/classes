!> author: Vikas Sharma, Ph. D.
! summary: This example shows how to get the node to node connectivity data in
!          the mesh.

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
  ! Initiate node to nodes
  CALL obj%InitiateNodeToNodes()
  !Display the content of mesh.
  CALL obj%DisplayMeshInfo(filename)
  ! Display the mesh data
  block
    INTEGER(I4B), ALlOCATABLE :: ans(:), exact_ans(:)
    INTEGER(I4B), PARAMETER :: globalNode=2
    ans = obj%GetNodeToNodes(globalNode=globalNode, includeSelf=.TRUE.)
    ans = SORT(ans)
    exact_ans = [2, 5, 7, 8, 9, 10]
    CALL OK(ALL(ans .eq. exact_ans), "GetNodeToNodes: ")
  end block
  block
    INTEGER(I4B), ALlOCATABLE :: ans(:), exact_ans(:)
    INTEGER(I4B), PARAMETER :: globalNode=2
    ans = obj%GetNodeToNodes(globalNode=globalNode, includeSelf=.FALSE.)
    ans = SORT(ans)
    exact_ans = [5, 7, 8, 9, 10]
    CALL OK(ALL(ans .eq. exact_ans), "GetNodeToNodes: ")
  end block
  block
    INTEGER(I4B), ALlOCATABLE :: ans(:), exact_ans(:)
    INTEGER(I4B), PARAMETER :: globalNode=1
    ans = obj%GetNodeToNodes(globalNode=globalNode, includeSelf=.TRUE.)
    ans = SORT(ans)
    exact_ans = [1, 4, 7, 9]
    CALL OK(ALL(ans .eq. exact_ans), "GetNodeToNodes: ")
  end block
  block
    INTEGER(I4B), ALlOCATABLE :: ans(:), exact_ans(:)
    INTEGER(I4B), PARAMETER :: globalNode=1
    ans = obj%GetNodeToNodes(globalNode=globalNode, includeSelf=.FALSE.)
    ans = SORT(ans)
    exact_ans = [4, 7, 9]
    CALL OK(ALL(ans .eq. exact_ans), "GetNodeToNodes: ")
  end block
  CALL obj%Deallocate()
  CALL meshfile%Deallocate()
END PROGRAM main
